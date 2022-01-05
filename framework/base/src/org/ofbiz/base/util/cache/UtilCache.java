/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.base.util.cache;

import java.io.NotSerializableException;
import java.io.Serializable;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.ilscipio.scipio.ce.util.collections.ResourceBundleMap;
import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.util.*;

import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap;
import com.googlecode.concurrentlinkedhashmap.EvictionListener;

/**
 * Generalized caching utility, heavily modified for Scipio.
 * <p>
 * FIXME: Although cache operations are thread-safe, UtilCache instances themselves are not automatically thread-safe
 *  and may need to be safe-published in static variables and thread-safe containers.
 * </p>
 * <ul>Caching features:
 *   <li>Limited or unlimited element capacity</li>
 *   <li>If limited, removes elements with the LRU (Least Recently Used) algorithm</li>
 *   <li>Keeps track of when each element was loaded into the cache</li>
 *   <li>Using the expireTime can report whether a given element has expired</li>
 *   <li>Counts misses and hits</li>
 * </ul>
 * <p>SCIPIO: 2.1.0: Now provides {@link Builder} through {@link #builder}.</p>
 * <p>SCIPIO: 2.1.0: Synchronized blocks and methods for {@link #removeInternal} and bulk remove calls other than
 * {@link #erase()} and {@link Index} operations have been completely removed to eliminate contention. They were largely
 * unnecessary for {@link ConcurrentHashMap} in the absence of listeners and stats accuracy, and use of listeners is
 * highly discouraged and considered deprecated.</p>
 * <p>SCIPIO: 2.1.0: Execution pulses are no longer removed; because cache lines are removed by object identity rather
 * than by key, it is possible to let cache lines expire as this lessens the removal processing cost at the expense
 * of temporarily higher memory usage from the extra cache lines pending expiry. Problems can be prevented by using
 * {@link #putIfAbsent(Object, Object)} rather than {@link #put(Object, Object)} in client code and in corresponding
 * UtilCache calls, which avoids cross-thread publishing of temporary instances in general and so that fewer extra
 * cache line instances and pulses are created. Please use {@link #putIfAbsent(Object, Object)} over
 * {@link #put(Object, Object)} wherever possible.</p>
 * <p>SCIPIO: 2.1.0: Added experimental index support using {@link Index}, which currently forces the use of
 * non-concurrent synchronization and must be programmed via cache.properties index key definitions and is not yet
 * recommended for use.</p>
 */
@SuppressWarnings("serial")
public class UtilCache<K, V> implements Serializable, EvictionListener<Object, CacheLine<V>> {

    public static final String SEPARATOR = "::";    // cache key separator
    public static final Pattern SEPARATOR_PAT = Pattern.compile(SEPARATOR); // SCIPIO: 2.1.0: Added

    /**
     * Default system cache configuration properties resource name, normally cache.properties with stripped extension.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static final String CFG_RES = "cache";

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** A static Map to keep track of all of the UtilCache instances. */
    private static final ConcurrentHashMap<String, UtilCache<?, ?>> utilCacheTable = new ConcurrentHashMap<>();

    /** An index number appended to utilCacheTable names when there are conflicts. */
    private static final ConcurrentHashMap<String, AtomicInteger> defaultIndices = new ConcurrentHashMap<>();

    /** The name of the UtilCache instance, is also the key for the instance in utilCacheTable. */
    protected final String name;
    protected final String baseName;

    /**
     * True if cache is enabled or false if disabled, in which case {@link #put} or {@link #putIfAbsent} have no effect
     * and {@link #get(Object)} operations always return null.
     * <p>Can be set false using explicit "enabled=false" cache property or {@link #setEnabled(boolean)}, and supports
     * real-time toggling only insofar as this flag is not <code>volatile</code>, meaning the jvm publishes the toggling
     * lazily to other threads with no time guarantee.</p>
     * <p>SCIPIO: 2.0.0: Added.</p>
     */
    protected boolean enabled;

    /**
     * The maximum number of elements in the cache.
     * <p>If set to 0, there will be no limit on the number of elements in the cache.</p>
     */
    protected int sizeLimit;
    protected int maxInMemory;

    /**
     * Specifies the amount of time since initial loading before an element will be reported as expired.
     * <p>If set to 0, elements will never expire.</p>
     */
    protected long expireDelayNanos;

    /** Specifies whether or not to use soft references for this cache, defaults to false */
    protected boolean useSoftReference;

    /**
     * Main key format for keys in the {@link #memoryTable}, in the form: <code>delegator::productId::...</code>.
     * <p>SCIPIO: 2.1.0: Added for {@link UtilCache.Index} support: added keyFormat definition for use in defining index keys from main cache keys.</p>
     */
    protected String keyFormat;

    /**
     * Static index definitions, from property or builder.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected Map<String, Object> indexDefs;

    /**
     * The set of listeners to receive notifications when items are modified (either deliberately or because they were expired).
     * <p>SCIPIO: NOTE: Should now have volatile, but never used in serious code and typically can be safe-published
     * by holding class (static variable), and often publishing time will be arbitrary to begin with.</p>
     * <p>SCIPIO: 2.1.0: Optimized with empty.</p>
     */
    protected Collection<CacheListener<K, V>> listeners = Collections.emptySet();

    protected ConcurrentMap<Object, CacheLine<V>> memoryTable;

    /**
     * Indexes by name whose index keys map to main keys in the {@link #memoryTable}.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected final Map<String, Index> indexNameMap;

    /**
     * Indexes whose index keys map to main keys in the {@link #memoryTable}.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected final List<Index> indexList;

    /**
     * Used to exclude {@link #erase()} and {@link #clear()} operations for performance reasons (not essential).
     */
    protected final Object eraseLockObj = new Serializable(){};

    /** A count of the number of cache hits */
    protected final AtomicLong hitCount = new AtomicLong(0);

    /** A count of the number of cache misses because it is not found in the cache */
    protected final AtomicLong missCountNotFound = new AtomicLong(0);
    /** A count of the number of cache misses because it expired */
    protected final AtomicLong missCountExpired = new AtomicLong(0);
    /** A count of the number of cache misses because it was cleared from the Soft Reference (ie garbage collection, etc) */
    protected final AtomicLong missCountSoftRef = new AtomicLong(0);

    /** A count of the number of cache hits on removes */
    protected final AtomicLong removeHitCount = new AtomicLong(0);
    /** A count of the number of cache misses on removes */
    protected final AtomicLong removeMissCount = new AtomicLong(0);

    /**
     * Main builder constructor.
     * <p>SCIPIO: 2.1.0: Created Builder-based constructor for {@link Index} support: added keyFormat definition for use
     * in defining index keys from main cache keys.</p>
     */
    protected UtilCache(Builder<K, V> builder) {
        this.baseName = builder.baseNameOrDefault();
        this.name = builder.nameOrDefault();
        this.enabled = (builder.enabled() != null) ? builder.enabled() : true;
        this.sizeLimit = (builder.sizeLimit() != null) ? builder.sizeLimit() : 0;
        this.maxInMemory = (builder.maxInMemory() != null) ? builder.maxInMemory() : 0;
        this.expireDelayNanos = (builder.expireDelay() != null) ? TimeUnit.NANOSECONDS.convert(builder.expireDelay(), TimeUnit.MILLISECONDS) : 0L;
        this.useSoftReference = (builder.useSoftReference() != null) ? builder.useSoftReference() : false;
        this.keyFormat = UtilValidate.nullIfEmpty(builder.keyFormat());
        this.indexDefs = UtilValidate.isNotEmpty(builder.indexDefs()) ? builder.indexDefs() : Collections.emptyMap();
        int maxMemSize = this.maxInMemory;
        if (maxMemSize == 0) {
            maxMemSize = sizeLimit;
        }
        if (maxMemSize == 0) {
            this.memoryTable = new ConcurrentHashMap<>();
        } else {
            this.memoryTable = new ConcurrentLinkedHashMap.Builder<Object, CacheLine<V>>()
                .maximumWeightedCapacity(maxMemSize)
                .listener(this)
                .build();
        }
        Map<String, Index> indexNameMap = Collections.emptyMap();
        List<Index> indexList = Collections.emptyList();
        if (UtilValidate.isNotEmpty(this.keyFormat) && UtilValidate.isNotEmpty(this.indexDefs)) {
            indexNameMap = makeIndexes(this.indexDefs);
            if (!indexNameMap.isEmpty()) {
                indexList = new ArrayList<>(indexNameMap.values());
            } else {
                indexNameMap = Collections.emptyMap();
            }
        }
        this.indexNameMap = indexNameMap;
        this.indexList = indexList;
    }

    /**
     * If the cache already exists by name, return it; otherwise create using supplier and store; if supplier is
     * {@link Builder} then {@link Builder#copy()}, {@link Builder#baseNameIfSet(String)} and {@link Builder#build()} are called.
     * <p>NOTE: Builder instances are copied and left untouched so they can be safely published and reused in static variables.</p>
     * <p>NOTE: For this method, the global cache table key is the simple cache name, without sequence index number appended.
     * The cache supplier is only consulted if the cache's simple name is not already in the global cache table.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support with enhanced Builder which can be returned by supplier.</p>
     * @param baseName The logical cache name
     * @param supplier A UtilCache or {@link Builder} instance that will have {@link Builder#baseNameIfSet(String)} and {@link Builder#build()} called
     */
    public static <K, V> UtilCache<K, V> getOrCreate(String baseName, Supplier<?> supplier) {
        return getOrCreateAuto(baseName, supplier);
    }

    /**
     * If the cache already exists by name, return it; otherwise creates with builder on which {@link Builder#copy()},
     * {@link Builder#baseNameIfSet(String)} and {@link Builder#build()} are called.
     * <p>NOTE: Builder instances are copied and left untouched so they can be safely published and reused in static variables.</p>
     * <p>NOTE: For this method, the global cache table key is the simple cache name, without sequence index number appended.
     * The cache supplier is only consulted if the cache's simple name is not already in the global cache table.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support with enhanced Builder which can be returned by supplier.</p>
     * @param baseName The logical cache name
     * @param builder A {@link Builder} instance that will have {@link Builder#baseName(String)} and {@link Builder#build()} called
     */
    public static <K, V> UtilCache<K, V> getOrCreate(String baseName, Builder<K, V> builder) {
        return getOrCreateAuto(baseName, builder);
    }

    protected static <K, V> UtilCache<K, V> getOrCreateAuto(String baseName, Object cacheBuilderSupplier) {
        UtilCache<K, V> cache = UtilGenerics.cast(utilCacheTable.get(baseName));
        if (cache != null) {
            return cache;
        }
        cache = build(baseName, cacheBuilderSupplier);
        UtilCache<K, V> prevCache = UtilGenerics.cast(utilCacheTable.putIfAbsent(baseName, cache));
        return (prevCache != null) ? prevCache : cache;
    }

    /**
     * Creates a UtilCache using given supplier and stores; if supplier returns a {@link Builder} then {@link Builder#baseNameIfSet(String)}
     * and {@link Builder#build()} are called.
     * <p>NOTE: Builder instances are copied and left untouched so they can be safely published and reused in static variables.</p>
     * <p>NOTE: For this method, the global cache table key is the full cache name, with sequence index number appended.
     * The cache supplier is always called and the cache is always added to the global table.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support with enhanced Builder which can be returned by supplier.</p>
     * @param baseName The logical cache name
     * @param supplier A supplier of a {@link Builder} or UtilCache
     */
    public static <K, V> UtilCache<K, V> create(String baseName, Supplier<?> supplier) {
        return createAuto(baseName, supplier);
    }

    /**
     * Creates a UtilCache using given supplier and stores; {@link Builder#baseNameIfSet(String)} and {@link Builder#build()} are called.
     * <p>NOTE: Builder instances are copied and left untouched so they can be safely published and reused in static variables.</p>
     * <p>NOTE: For this method, the global cache table key is the full cache name, with sequence index number appended.
     * The cache supplier is always called and the cache is always added to the global table.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support with enhanced Builder which can be returned by supplier.</p>
     * @param baseName The logical cache name
     * @param builder A {@link Builder} instance
     */
    public static <K, V> UtilCache<K, V> create(String baseName, Builder<K, V> builder) {
        return createAuto(baseName, builder);
    }

    protected static <K, V> UtilCache<K, V> createAuto(String name, Object cacheBuilderSupplier) {
        UtilCache<K, V> cache = build(name, cacheBuilderSupplier);
        utilCacheTable.put(cache.getName(), cache); // NOTE: Here uses full cache name (not simple name)
        return cache;
    }

    /**
     * Builds a UtilCache using given supplier without storing; if supplier provides a {@link Builder} then
     * {@link Builder#baseNameIfSet(String)} and {@link Builder#build()} are called.
     * <p>NOTE: Builder instances are copied and left untouched so they can be safely published and reused in static variables.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support with enhanced Builder which can be returned by supplier.</p>
     * @param baseName The cache's simple name
     * @param cacheBuilderSupplier A {@link Builder} or {@link UtilCache} instance, or a {@link Supplier} for one of these
     */
    protected static <K, V> UtilCache<K, V> build(String baseName, Object cacheBuilderSupplier) {
        if (cacheBuilderSupplier instanceof Supplier) {
            cacheBuilderSupplier = ((Supplier<?>) cacheBuilderSupplier).get();
        }
        if (cacheBuilderSupplier instanceof Builder) {
            // NOTE: This makes a copy of the builder to leave the original intact, which is in most cases
            // great because it allows the caller to reuse Builder definitions without the overhead at every getOrCreate() call
            // and rather only at creation time. Even though it slows down the call, it allows safe scoping and reuse
            // and UtilCache instances are rarely created in practice.
            return UtilGenerics.<Builder<K, V>>cast(cacheBuilderSupplier).copy().baseNameIfSet(baseName).build();
        } else if (cacheBuilderSupplier instanceof UtilCache) {
            return UtilGenerics.cast(cacheBuilderSupplier);
        } else {
            throw new IllegalArgumentException("Invalid cache, builder or supplier; must be one of: " + Builder.class.getSimpleName() +
                    ", " + Supplier.class.getSimpleName() + ", " + UtilCache.class.getSimpleName());
        }
    }

    /**
     * Returns a cache builder that automatically creates and registers a new cache in the system when the
     * {@link Builder#build()} method is called to create the cache.
     * <p>NOTE: It is recommended to pass this Builder as result of the supplier in a call to
     * {@link #getOrCreate(String, Supplier)} or {@link #create(String, Supplier)} as they automatically set the name
     * and avoid creating a builder or cache unless necessary using common patterns; note that
     * {@link #getOrCreate(String, Supplier)} is recommended over {@link #create(String, Supplier)} because
     * <code>putIfAbsent</code> is better than <code>put</code> to avoid unnecessary lingering references (since any new
     * instances temporarily created are not published to other threads at all, instead of being published temporarily
     * into the map and replaced).</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <K, V> Builder<K, V> builder() {
        return new Builder<>();
    }

    /**
     * Returns a cache builder that automatically creates and registers a new cache in the system when the
     * {@link Builder#build()} method is called to create the cache.
     * <p>NOTE: It is recommended to pass this Builder as result of the supplier in a call to
     * {@link #getOrCreate(String, Supplier)} or {@link #create(String, Supplier)} as they automatically set the name
     * and avoid creating a builder or cache unless necessary using common patterns; note that
     * {@link #getOrCreate(String, Supplier)} is recommended over {@link #create(String, Supplier)} because
     * <code>putIfAbsent</code> is better than <code>put</code> to avoid unnecessary lingering references (since any new
     * instances temporarily created are not published to other threads at all, instead of being published temporarily
     * into the map and replaced).</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <K, V> Builder<K, V> builder(String name) {
        return new Builder<>(name);
    }

    /**
     * Builder for UtilCache with easy defaults.
     * <p>NOTE: Instances are thread-safe and reusable, but only if safely published in static variable and thread-safe
     * collections</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static class Builder<K, V> {
        String baseName;
        String resource;
        Boolean autoScope;
        List<String> propNames;
        Boolean autoProps;
        Boolean enabled;
        Integer sizeLimit;
        Integer maxInMemory;
        Long expireDelay;
        Boolean useSoftReference;
        String keyFormat;
        Map<String, Object> indexDefs;

        // mutable/transient
        String name;
        Integer seqNum;
        ResourceBundle resourceBundle;

        public Builder() {}

        public Builder(String baseName) {
            baseName(baseName);
        }

        public Builder(Builder<K, V> other) {
            this.baseName = other.baseName;
            this.resource = other.resource;
            this.autoScope = other.autoScope;
            this.propNames = other.propNames;
            this.autoProps = other.autoProps;
            this.enabled = other.enabled;
            this.sizeLimit = other.sizeLimit;
            this.maxInMemory = other.maxInMemory;
            this.expireDelay = other.expireDelay;
            this.useSoftReference = other.useSoftReference;
            this.keyFormat = other.keyFormat;
            this.indexDefs = other.indexDefs;

            // mutable/transient
            this.name = other.name; // may also be manually set so must be preserved
            this.seqNum = null;
            this.resourceBundle = null;
        }

        /**
         * Creates a copy of this Builder, with mutable/transient fields not or only partially preserved as appropriate.
         * <p>NOTE: Despite the extra copies, it is recommended to use this method as a scoping mechanism, as this
         * easily to easily reuse static Builder definitions without modifying them, especially before calling any of
         * the methods that modify any of the mutable/transient fields. This can normally be done before any
         * name-resolution ({@link #nameOrDefault()}), build ({@link #build()}) or constructor (UtilCache(Builder)) calls.</p>
         */
        public Builder<K, V> copy() {
            return new Builder<>(this);
        }

        /**
         * Creates a cache instance without registering in the system.
         * <p>Does not store the cache; intended for use with {@link #getOrCreate(String, Supplier)}.</p>
         */
        public UtilCache<K, V> build() {
            if (!Boolean.FALSE.equals(autoProps())) {
                fromProperties();
            }
            return new UtilCache<>(this);
        }

        public String baseName() {
            return baseName;
        }

        public Builder<K, V> baseName(String baseName) {
            this.baseName = baseName;
            return this;
        }

        public Builder<K, V> baseNameIfSet(String baseName) {
            return UtilValidate.isNotEmpty(baseName) ? baseName(baseName) : this;
        }

        public String baseNameOrDefault() {
            String name = this.baseName();
            return (name != null) ? name : "default";
        }

        public String name() {
            return name;
        }

        /**
         * Overrides the full name, which is normally the name concatenated with a generated number.
         */
        public Builder<K, V> name(String name) {
            this.name = name;
            return this;
        }

        public String nameOrDefault() {
            String fullName = this.name();
            if (fullName == null) {
                fullName = baseNameOrDefault();
                int seqNum = nextSeqNum();
                if (seqNum != 0) {
                    fullName += seqNum + "";
                }
                this.name = fullName;
            }
            return fullName;
        }

        public String resource() {
            return resource;
        }

        public Builder<K, V> resource(String resource) {
            this.resource = resource;
            return this;
        }

        public Boolean autoScope() {
            return autoScope;
        }

        /**
         * If true (default), factory methods like {@link #getOrCreate}, {@link #create), {@link #build} automatically
         * scope their changes using {@link #copy()} calls to prevent modifying the builders they receive as parameters;
         * if false, no scoping is automatically applied.
         * <p>NOTE: Usually this is a small performance concern so it's better to have on.</p>
         * <p>SCIPIO: 2.1.0: Added.</p>
         */
        public Builder<K, V> autoScope(Boolean autoScope) {
            this.autoScope = autoScope;
            return this;
        }

        public String resourceOrDefault() {
            return (this.resource() != null) ? this.resource() : CFG_RES;
        }

        public ResourceBundle resourceBundle() {
            ResourceBundle resourceBundle = this.resourceBundle;
            if (resourceBundle == null) {
                resourceBundle = ResourceBundle.getBundle(resourceOrDefault());
                this.resourceBundle = resourceBundle;
            }
            return resourceBundle;
        }

        /**
         * The cache.properties or specified {@link #resource()} property names to consult for values, with priority
         * lookup to first in list.
         */
        public List<String> propNames() {
            return propNames;
        }

        public Builder<K, V> propNames(Collection<String> propNames) {
            this.propNames = (propNames != null) ? (!propNames.isEmpty() ? new ArrayList<>(propNames) : Collections.emptyList()) : null;
            return this;
        }

        public Builder<K, V> propNames(String... propNames) {
            this.propNames = (propNames != null && propNames.length > 0) ? new ArrayList<>(Arrays.asList(propNames)) : null;
            return this;
        }

        public Boolean autoProps() {
            return autoProps;
        }

        public Builder<K, V> autoProps(Boolean autoProps) {
            this.autoProps = autoProps;
            return this;
        }

        public Boolean enabled() {
            return enabled;
        }

        public Builder<K, V> enabled(Boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        public Integer sizeLimit() {
            return sizeLimit;
        }

        public Builder<K, V> sizeLimit(Integer sizeLimit) {
            this.sizeLimit = sizeLimit;
            return this;
        }

        public Integer maxInMemory() {
            return maxInMemory;
        }

        public Builder<K, V> maxInMemory(Integer maxInMemory) {
            this.maxInMemory = maxInMemory;
            return this;
        }

        public Long expireDelay() {
            return expireDelay;
        }

        public Builder<K, V> expireDelay(Long expireDelay) {
            this.expireDelay = expireDelay;
            return this;
        }

        public Boolean useSoftReference() {
            return useSoftReference;
        }

        public Builder<K, V> useSoftReference(Boolean useSoftReference) {
            this.useSoftReference = useSoftReference;
            return this;
        }

        public String keyFormat() {
            return keyFormat;
        }

        public Builder<K, V> keyFormat(String keyFormat) {
            this.keyFormat = keyFormat;
            return this;
        }

        public Map<String, Object> indexDefs() {
            return indexDefs;
        }

        public Builder<K, V> indexDefs(Map<String, ?> indexDefs) {
            this.indexDefs = UtilGenerics.cast(indexDefs);
            return this;
        }

        public Builder<K, V> indexDefs(Object... indexDefs) {
            return indexDefs(UtilMisc.toOrderedMap(indexDefs));
        }

        public Integer seqNum() {
            return this.seqNum;
        }

        public int nextSeqNum() {
            Integer seqNum = this.seqNum();
            if (seqNum == null) {
                seqNum = getNextDefaultIndexNumber(baseNameOrDefault());
                this.seqNum = seqNum;
            }
            return seqNum;
        }

        /**
         * Reads extra cache properties from cache.properties or other resource and replaces any explicitly specied
         * or empty values as appropriate.
         * <p>SCIPIO: 2.1.0: Replaces setPropertiesParams; automatically called by {@link #build()}.</p>
         */
        public Builder<K, V> fromProperties() {
            Collection<String> propNames = this.propNames();
            return fromProperties((propNames != null) ? propNames : List.of(baseNameOrDefault()));
        }

        /**
         * Reads extra cache properties from cache.properties or other resource and replaces any explicitly specied
         * or empty values as appropriate.
         * <p>SCIPIO: 2.1.0: Replaces setPropertiesParams; automatically called by {@link #build()}.</p>
         */
        public Builder<K, V> fromProperties(String... propNames) {
            return fromProperties(resourceBundle(), propNames);
        }

        /**
         * Reads extra cache properties from cache.properties or other resource and replaces any explicitly specied
         * or empty values as appropriate.
         * <p>SCIPIO: 2.1.0: Replaces setPropertiesParams; automatically called by {@link #build()}.</p>
         */
        public Builder<K, V> fromProperties(Collection<String> propNames) {
            return fromProperties(resourceBundle(), propNames);
        }

        /**
         * Reads extra cache properties from cache.properties or other resource and replaces any explicitly specied
         * or empty values as appropriate.
         * <p>SCIPIO: 2.1.0: Replaces setPropertiesParams; automatically called by {@link #build()}.</p>
         */
        public Builder<K, V> fromProperties(ResourceBundle res, String... propNames) {
            return fromProperties(res, Arrays.asList(propNames));
        }

        /**
         * Reads extra cache properties from cache.properties or other resource and replaces any explicitly specied
         * or empty values as appropriate.
         * <p>SCIPIO: 2.1.0: Replaces setPropertiesParams; automatically called by {@link #build()}.</p>
         */
        public Builder<K, V> fromProperties(ResourceBundle res, Collection<String> propNames) {
            if (UtilValidate.isEmpty(propNames)) {
                return this;
            }
            String value;
            value = getPropertyParam(res, propNames, "enabled");
            if (value != null) {
                this.enabled(UtilMisc.booleanValue(value));
            }
            // SCIPIO: 2.1.0: Now supports "sizeLimit", but "maxSize" remains supported
            value = getPropertyParam(res, propNames, "sizeLimit");
            if (UtilValidate.isEmpty(value)) {
                value = getPropertyParam(res, propNames, "maxSize");
            }
            if (UtilValidate.isNotEmpty(value)) {
                this.sizeLimit(Integer.parseInt(value));
            }
            value = getPropertyParam(res, propNames, "maxInMemory");
            if (UtilValidate.isNotEmpty(value)) {
                this.maxInMemory(Integer.parseInt(value));
            }
            // SCIPIO: 2.1.0: Now prefers "expireDelay", but "expireTime" remains supported
            value = getPropertyParam(res, propNames, "expireDelay");
            if (UtilValidate.isEmpty(value)) {
                value = getPropertyParam(res, propNames, "expireTime");
            }
            if (UtilValidate.isNotEmpty(value)) {
                this.expireDelay(Long.parseLong(value));
            }
            value = getPropertyParam(res, propNames, "useSoftReference");
            if (value != null) {
                this.useSoftReference(UtilMisc.booleanValue(value));
            }
            // SCIPIO: 2.1.0: keyFormat and indexDefs
            value = getPropertyParam(res, propNames, "keyFormat");
            if (UtilValidate.isNotEmpty(value)) {
                this.keyFormat(value);
                Map<String, Object> ikf = makeSubPropertyParamsWithId(res, propNames, "index");
                if (UtilValidate.isNotEmpty(ikf)) {
                    Map<String, Object> indexDefs = UtilGenerics.cast(this.indexDefs());
                    if (indexDefs != null) {
                        Map<String, Object> ikfNew = new LinkedHashMap<>(indexDefs);
                        for (Map.Entry<String, Object> entry : ikf.entrySet()) {
                            Object ikfValueOld = ikfNew.get(entry.getKey());
                            if (ikfValueOld == null) {
                                ikfNew.put(entry.getKey(), entry.getValue());
                            } else if (ikfValueOld instanceof Map) {
                                Map<String, Object> newMap = new LinkedHashMap<>(UtilGenerics.cast(ikfValueOld));
                                newMap.putAll(UtilGenerics.cast(entry.getValue()));
                                ikfNew.put(entry.getKey(), newMap);
                            } else if (ikfValueOld instanceof String) {
                                Map<String, Object> newMap = UtilMisc.toMap("keyFormat", ikfValueOld);
                                newMap.putAll(UtilGenerics.cast(entry.getValue()));
                                ikfNew.put(entry.getKey(), newMap);
                            } else {
                                Debug.logError("Invalid index defs for key [" + entry.getKey() + "]: " + indexDefs, module);
                            }
                        }
                        this.indexDefs(ikfNew);
                    } else {
                        this.indexDefs(ikf);
                    }
                }
            }
            return this;
        }
    }

    private static String getNextDefaultIndex(String cacheName) {
        int i = getNextDefaultIndexNumber(cacheName);
        return i == 0 ? "" : Integer.toString(i);
    }

    private static int getNextDefaultIndexNumber(String cacheName) { // SCIPIO: 2.1.0: Refactored
        AtomicInteger curInd = defaultIndices.get(cacheName);
        if (curInd == null) {
            defaultIndices.putIfAbsent(cacheName, new AtomicInteger(0));
            curInd = defaultIndices.get(cacheName);
        }
        return curInd.getAndIncrement();
    }

    /**
     * Gets the named property from the resource bundle using the given propNames as prefix, where the last entry
     * has priority over the first.
     * <p>SCIPIO: 2.1.0: Refactored and inverted main loop: in most cases only up to 2 propNames total were normally
     * specified (["default", baseName]) and because the first propName was separate, these produced 2 calls to
     * getPropertyParam so effectively the second overrode the first correctly; however this method itself was written
     * backward such that any extra propNames after the first 2 (including the original dedicated propName method
     * parameter) do not get priority, which was highly confusing from the point of view of client code. We have fixed
     * this situation by having only one propNames list (no separate propName) that preserves backward-compatibility for
     * the most cases (total propNames length 1 or 2).</p>
     */
    protected static String getPropertyParam(ResourceBundle res, Collection<String> propNames, String parameter) {
        try {
            // SCIPIO: 2.1.0: Reverted loop
            List<String> propNamesList = UtilMisc.toList(propNames);
            ListIterator<String> it = propNamesList.listIterator(propNames.size());
            while(it.hasPrevious()) {
                String propName = it.previous();
                if (UtilValidate.isEmpty(propName)) {
                    continue; // SCIPIO: 2.1.0: More friendly
                }
                String key = propName.concat(".").concat(parameter);
                if (res.containsKey(key)) {
                    try {
                        return res.getString(key);
                    } catch (MissingResourceException e) {
                    }
                }
            }
        } catch (Exception e) {
            Debug.logWarning(e, "Error getting " + parameter + " value from ResourceBundle for propNames: " + propNames, module);
        }
        return null;
    }

    protected static Map<String, Object> makeSubPropertyParamsWithId(ResourceBundle res, Collection<String> propNames, String parameter) { // SCIPIO: 2.1.0: Added
        try {
            Map<String, Map<String, Object>> paramMap = new LinkedHashMap<>();
            Map<String, Object> resMap = new ResourceBundleMap(res);
            // SCIPIO: 2.1.0: NOTE: This is the opposite order of getPropertyParam() because it always replaces entries
            // instead of finding the first non-null.
            for (String propName : propNames) {
                if (UtilValidate.isEmpty(propName)) {
                    continue;
                }
                try {
                    UtilProperties.extractPropertiesWithPrefixAndId(paramMap, resMap, propName + "." + parameter + ".");
                } catch (MissingResourceException e) {
                }
            }
            return UtilGenerics.cast(paramMap);
        } catch (Exception e) {
            Debug.logWarning(e, "Error getting " + parameter + " value from ResourceBundle for propNames: " + propNames, module);
        }
        return null;
    }

    private Object fromKey(Object key) {
        return key == null ? ObjectType.NULL : key;
    }

    private K toKey(Object key) {
        return key == ObjectType.NULL ? null : UtilGenerics.cast(key);
    }

    public Object getCacheLineTable() {
        throw new UnsupportedOperationException();
    }

    public boolean isEmpty() {
        return memoryTable.isEmpty();
    }

    /**
     * Puts or loads the passed element into the cache.
     * @param key The key for the element, used to reference it in the hashtables and LRU linked list
     * @param value The value of the element
     */
    public V put(K key, V value) {
        return putInternal(key, value, expireDelayNanos);
    }

    public V putIfAbsent(K key, V value) {
        return putIfAbsentInternal(key, value, expireDelayNanos);
    }

    public V putIfAbsentAndGet(K key, V value) {
        V cachedValue = putIfAbsent(key, value);
        return (cachedValue != null) ? cachedValue : value;
    }

    /**
     * Creates cache line.
     * <p>SCIPIO: 2.1.0: To avoid pulses expiring before the cache line is added to memoryTable, this has been split
     * so that the initial cache line creation defers the {@link #registerExpirePulse(CacheLine)} call to after the memoryTable
     * put calls; this also avoids putIfAbsent unnecessarily adding execution pulses that are never actually added to
     * memoryTable.</p>
     */
    protected CacheLine<V> createCacheLine(K key, V value, long expireDelayNanos) {
        long loadTimeNanos = expireDelayNanos > 0 ? System.nanoTime() : 0;
        if (useSoftReference) {
            return createSoftRefCacheLine(key, value, loadTimeNanos, expireDelayNanos);
        }
        return createHardRefCacheLine(key, value, loadTimeNanos, expireDelayNanos);
    }

    /**
     * Common UtilCache cache line with <code>UtilCache.this</code> reference.
     * <p>SCIPIO: 2.1.0: Refactored and added Serializable.</p>
     */
    protected abstract class Line extends CacheLine<V> implements Serializable {
        protected final Object key;

        public Line(Object key, V value, long loadTimeNanos, long expireDelayNanos) {
            super(loadTimeNanos, expireDelayNanos);
            this.key = key;
        }

        @Override
        public <R> R getKey() { // SCIPIO: 2.1.0: Added for UtilCache.Index support
            return UtilGenerics.cast(key);
        }

        @Override
        protected void remove() {
            removeInternal(key, this, false);
        }
    }

    /**
     * Creates soft-ref cache line.
     * <p>SCIPIO: 2.1.0: Now omits registering pulses, which is left to callers to do after adding to memoryTable
     * via {@link #registerExpirePulse(CacheLine)}.</p>
     */
    protected CacheLine<V> createSoftRefCacheLine(Object key, V value, long loadTimeNanos, long expireDelayNanos) {
        return new SoftLine(key, value, loadTimeNanos, expireDelayNanos);
    }

    /**
     * Soft reference cache line.
     * <p>SCIPIO: 2.1.0: Moved from SoftRefCacheLine abstract class plus parts extracted from former createSoftRefCacheLine method.</p>
     */
    protected class SoftLine extends Line {
        protected final CacheSoftReference<V> ref;

        public SoftLine(Object key, V value, long loadTimeNanos, long delayNanos) {
            super(key, value, loadTimeNanos, delayNanos);
            this.ref = new CacheSoftReference<V>(value) {
                @Override
                public void remove() {
                    Debug.logInfo("Cache [" + UtilCache.this.name + "]: Removing soft ref [" + key + "]", module);
                    SoftLine.this.remove();
                }
            };
        }

        @Override
        public V getValue() {
            return ref.get();
        }

        @Override
        protected void cancel() {
            ref.clear();
        }

        @Override
        protected CacheLine<V> changeLine(boolean useSoftReference, long expireDelayNanos) {
            if (useSoftReference) {
                if (!differentExpireTime(expireDelayNanos)) { // SCIPIO: 2.1.0: Inverted this logic: should return same if same expire time
                    return this;
                }
                return createSoftRefCacheLine(getKey(), getValue(), getLoadTimeNanos(), expireDelayNanos);
            }
            return createHardRefCacheLine(getKey(), getValue(), getLoadTimeNanos(), expireDelayNanos);
        }
    }

    /**
     * Creates hard-ref cache line.
     * <p>SCIPIO: 2.1.0: Now omits registering pulses, which is left to callers to do after adding to memoryTable
     * via {@link #registerExpirePulse(CacheLine)}.</p>
     */
    protected CacheLine<V> createHardRefCacheLine(Object key, V value, long loadTimeNanos, long expireDelayNanos) {
        return new HardLine(key, value, loadTimeNanos, expireDelayNanos);
    }

    /**
     * Hard reference cache line.
     * <p>SCIPIO: 2.1.0: Moved from HardRefCacheLine abstract class plus parts extracted from former createHardRefCacheLine method.</p>
     */
    protected class HardLine extends Line {
        protected final V value;

        public HardLine(Object key, V value, long loadTimeNanos, long expireDelayNanos) {
            super(key, value, loadTimeNanos, expireDelayNanos);
            this.value = value;
        }

        @Override
        public V getValue() {
            return value;
        }

        @Override
        protected CacheLine<V> changeLine(boolean useSoftReference, long expireDelayNanos) {
            if (useSoftReference) {
                return createSoftRefCacheLine(getKey(), getValue(), getLoadTimeNanos(), expireDelayNanos);
            }
            if (!differentExpireTime(expireDelayNanos)) { // SCIPIO: 2.1.0: Inverted this logic: should return same if same expire time
                return this;
            }
            return createHardRefCacheLine(getKey(), getValue(), getLoadTimeNanos(), expireDelayNanos);
        }
    }

    /**
     * Registers the cache line as execution pulse for delayed removal if enabled.
     * <p>SCIPIO: 2.1.0: Renamed from <code>tryRegister</code> and removed loadTimeNanos parameter since already recorded in cache line.</p>
     */
    private CacheLine<V> registerExpirePulse(CacheLine<V> line) {
        if (line.getLoadTimeNanos() > 0) {
            ExecutionPool.addPulse(line);
        }
        return line;
    }

    private V cancel(CacheLine<V> line) {
        // FIXME: this is a race condition, the item could expire
        // between the time it is replaced, and it is cancelled
        V oldValue = line.getValue();
        // SCIPIO: 2.1.0: Becomes expensive and is not strictly needed because deletions are by CacheLine not by key and putIfAbsent() helps
        //ExecutionPool.removePulse(line);
        line.cancel();
        return oldValue;
    }

    /**
     * Puts or loads the passed element into the cache.
     * @param key The key for the element, used to reference it in the hashtables and LRU linked list
     * @param value The value of the element
     * @param expireDelayMillis how long to keep this key in the cache
     */
    public V put(K key, V value, long expireDelayMillis) {
        return putInternal(key, value, TimeUnit.NANOSECONDS.convert(expireDelayMillis, TimeUnit.MILLISECONDS));
    }

    public V putIfAbsent(K key, V value, long expireDelayMillis) {
        return putIfAbsentInternal(key, value, TimeUnit.NANOSECONDS.convert(expireDelayMillis, TimeUnit.MILLISECONDS));
    }

    V putInternal(K key, V value, long expireDelayNanos) {
        if (!enabled) {
            return null; // SCIPIO: 2018-03: no-op
        }
        // SCIPIO: 2.1.0: Now defer the initial pulse register to after memoryTable.put (see createCacheLine)
        CacheLine<V> newCacheLine = createCacheLine(key, value, expireDelayNanos);
        if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
            // NOTE: Cache lines must be added to index before adding to memoryTable to guarantee lifetime visibility
            // due to lack of synchronization
            addCacheLineToIndexes(newCacheLine);
        }
        CacheLine<V> oldCacheLine = memoryTable.put(fromKey(key), newCacheLine);
        registerExpirePulse(newCacheLine);
        V oldValue = oldCacheLine == null ? null : cancel(oldCacheLine);
        if (oldValue == null) {
            noteAddition(key, value);
            return null;
        }
        noteUpdate(key, value, oldValue);
        return oldValue;
    }

    V putIfAbsentInternal(K key, V value, long expireDelayNanos) {
        if (!enabled) {
            return null; // SCIPIO: 2018-03: no-op
        }
        V oldValue;
        // SCIPIO: 2.1.0: Now defer the initial pulse register to after memoryTable.put (see createCacheLine)
        CacheLine<V> newCacheLine = createCacheLine(key, value, expireDelayNanos);
        if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
            // NOTE: Cache lines must be added to index before adding to memoryTable to guarantee lifetime visibility
            // due to lack of synchronization, and for putIfAbsent must be done even if the cache line is not
            // actually added to memoryTable otherwise lifetime is not fully covered.
            addCacheLineToIndexes(newCacheLine);
        }
        CacheLine<V> oldCacheLine = memoryTable.putIfAbsent(fromKey(key), newCacheLine);
        if (oldCacheLine == null) {
            oldValue = null;
            // SCIPIO: 2.1.0: As above, now simply run tryRegister() here after the line is actually added
            registerExpirePulse(newCacheLine);
        } else {
            if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
                // NOTE: In this case the cache line was not added, so remove it immediately
                removeCacheLineFromIndexes(newCacheLine);
            }
            oldValue = oldCacheLine.getValue();
            // SCIPIO: 2.1.0: As above, there is no longer need to cancel the cache line since it was never registered.
            //cancel(newCacheLine);
        }
        if (oldValue == null) {
            noteAddition(key, value);
            return null;
        }
        return oldValue;
    }

    /**
     * Gets an element from the cache according to the specified key.
     * @param key The key for the element, used to reference it in the hashtables and LRU linked list
     * @return The value of the element specified by the key
     */
    public V get(Object key) {
        if (!enabled) {
            return null; // SCIPIO: 2018-03: no-op
        }
        boolean countGet = true;
        CacheLine<V> line = memoryTable.get(fromKey(key));
        if (line == null) {
            missCountNotFound.incrementAndGet();
        } else {
            if (countGet) {
                hitCount.incrementAndGet();
            }
        }
        return line != null ? line.getValue() : null;
    }

    public Collection<V> values() {
        List<V> valuesList = new LinkedList<>();
        for (CacheLine<V> line: memoryTable.values()) {
            valuesList.add(line.getValue());
        }
        return valuesList;
    }

    private long findSizeInBytes(Object o, Object key) { // SCIPIO: Added key and improved logging
        if (o == null) {
            if (Debug.verboseOn()) {
                Debug.logVerbose("Found null object in cache: " + getName(), module);
            }
            return 0;
        }
        try {
            if (o instanceof Serializable) {
                return UtilObject.getByteCount(o);
            }
            if (Debug.verboseOn()) {
                Debug.logVerbose("Unable to compute memory size for non serializable object; returning 0 byte size for object of " + o.getClass()
                        + " for key '" + key + "' in cache: " + getName(), module);
            }
            return 0;
        } catch (NotSerializableException e) {
            // this happens when we try to get the byte count for an object which itself is
            // serializable, but fails to be serialized, such as a map holding unserializable objects
            if (Debug.warningOn()) {
                Debug.logWarning("NotSerializableException while computing memory size; returning 0 byte size for object of " + e.getMessage()
                        + " for key '" + key + "' in cache: " + getName(), module);
            }
            return 0;
        } catch (Exception e) {
            Debug.logWarning(e, "Unable to compute memory size for object of " + o.getClass()
                    + " for key '" + key + "' in cache: " + getName(), module);
            return 0;
        }
    }

    public long getSizeInBytes() {
        long totalSize = 0;
        // SCIPIO: Include key for debugging
        //for (CacheLine<V> line: memoryTable.values()) {
        //    totalSize += findSizeInBytes(line.getValue());
        //}
        for (Map.Entry<Object, CacheLine<V>> lineEntry : memoryTable.entrySet()) {
            totalSize += findSizeInBytes(lineEntry.getValue().getValue(), lineEntry.getKey());
        }
        return totalSize;
    }

    /**
     * Counts all element from the cache matching the given filter.
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.0.0: Added.</p>
     * <p>SCIPIO: 2.1.0: Removed synchronized because only used on remove calls and ConcurrentHashMap can usually
     * handle bulk removal iterations without it.</p>
     * @param entryFilter The entry filter - return true to remove key
     */
    public int countByFilter(CacheEntryFilter<K, V> entryFilter) { // SCIPIO: 2.1.0: Removed synchronized
        int count = 0;
        for (Map.Entry<Object, CacheLine<V>> entry : memoryTable.entrySet()) {
            if (entryFilter.filter(UtilGenerics.cast(toKey(entry.getKey())), UtilGenerics.cast(entry.getValue().getValue()))) {
                count++;
            }
        }
        return count;
    }

    /**
     * Counts all element from the cache matching the given key regex pattern (uses {@link java.util.regex.Matcher#matches()} on each key).
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int countByKeyPat(Pattern keyPat) {
        return countByFilter(new KeyPatternCacheEntryFilter<>(keyPat));
    }

    /**
     * Counts all element from the cache matching the given key regex pattern (uses {@link java.util.regex.Matcher#matches()} on each key).
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int countByKeyPat(String keyPat) {
        return countByFilter(new KeyPatternCacheEntryFilter<>(keyPat));
    }

    /**
     * Counts all element from the cache matching the given key prefix.
     * <p>NOTE: Typically one should suffix prefixes with {@link #SEPARATOR} (not added).</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int countByKeyPrefix(String keyPrefix) {
        return countByFilter(new KeyPrefixCacheEntryFilter<>(keyPrefix));
    }

    /**
     * Removes an element from the cache according to the specified key
     * @param key The key for the element, used to reference it in the hashtables and LRU linked list
     * @return The value of the removed element specified by the key
     */
    public V remove(Object key) {
        return this.removeInternal(key, true);
    }

    /**
     * Remove operation; only use if existing cache line not available (see {@link #removeInternal(Object, CacheLine, boolean)}).
     * <p>SCIPIO: 2.1.0: Removed synchronized because only used on remove calls and ConcurrentHashMap can usually handle bulk removal iterations without it.</p>
     * <p>SCIPIO: 2.1.0: Added removeRelated for shortKey removal.</p>
     * @see #removeInternal(Object, CacheLine, boolean)
     */
    protected V removeInternal(Object key, boolean explicit) { // SCIPIO: 2.1.0: Removed synchronized
        CacheLine<V> oldCacheLine = memoryTable.remove(fromKey(key));
        V oldValue = (oldCacheLine != null) ? oldCacheLine.getValue() : null;
        if (oldCacheLine != null) {
            if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
                // SCIPIO: NOTE: We only remove if the memoryTable.remove() hit succeeded to guarantee we only do
                // a single remove call for this cache line per index. Because putInternal() adds cache lines to indexes
                // before the main memoryTable, cache lines are visible early in the indexes to operations such as
                // removeByIndexKey(). So by removing from indexes only in the case the memoryTable.remove() actually
                // succeeds, it ensures 1) at most one remove call per index and 2) no index removes can happen before and
                // until the cache line is added to the main memoryTable. Note that we must also cover the case where
                // removal happens some other way, such as ConcurrentLinkedHashMap maxInMemory #onEviction() callback.
                removeCacheLineFromIndexes(oldCacheLine);
            }
            cancel(oldCacheLine);
        }
        if (oldValue != null) {
            noteRemoval(UtilGenerics.cast(key), oldValue);
            if (explicit) {
                removeHitCount.incrementAndGet();
            }
            return oldValue;
        }
        if (explicit) {
            removeMissCount.incrementAndGet();
        }
        return null;
    }

    /**
     * Remove operation; if existingCacheLine non-null, only removes key if matching existing cache line.
     * <p>SCIPIO: 2.1.0: Removed synchronized because only used on remove calls and ConcurrentHashMap can usually handle bulk removal iterations without it.</p>
     * <p>SCIPIO: 2.1.0: cancel() call now occurs after memoryTable remove for consistency between overloads and double-cancel prevention.</p>
     */
    protected boolean removeInternal(Object key, CacheLine<V> existingCacheLine, boolean explicit) { // SCIPIO: 2.1.0: Removed synchronized, added explicit
        //cancel(existingCacheLine); // SCIPIO: 2.1.0: Moved below so that only called once actually removed
        if (memoryTable.remove(fromKey(key), existingCacheLine)) {
            if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
                // SCIPIO: NOTE: We only remove if the memoryTable.remove() hit succeeded to guarantee we only do
                // a single remove call for this cache line per index. Because putInternal() adds cache lines to indexes
                // before the main memoryTable, cache lines are visible early in the indexes to operations such as
                // removeByIndexKey(). So by removing from indexes only in the case the memoryTable.remove() actually
                // succeeds, it ensures 1) at most one remove call per index and 2) no index removes can happen before and
                // until the cache line is added to the main memoryTable. Note that we must also cover the case where
                // removal happens some other way, such as ConcurrentLinkedHashMap maxInMemory #onEviction() callback.
                removeCacheLineFromIndexes(existingCacheLine);
            }
            V oldValue = existingCacheLine.getValue();
            cancel(existingCacheLine);
            if (oldValue != null) {
                noteRemoval(UtilGenerics.cast(key), oldValue);
                if (explicit) {
                    removeHitCount.incrementAndGet();
                }
                return true;
            }
        }
        if (explicit) {
            removeMissCount.incrementAndGet();
        }
        return false;
    }

    /**
     * Removes all elements from this cache.
     * <p>SCIPIO: NOTE: This method will retain synchronized(UtilCache.this) </p>
     * <p>SCIPIO: 2.1.0: Replaced implementation because <code>it.remove()</code> is key-based without specific value check.</p>
     * <p>SCIPIO: 2.1.0: Now synchronizes on dedicated eraseLockObj for performance reasons; only {@link #erase()} should lock on it.</p>
     */
    public int erase() {
        int removed = 0;
        synchronized(eraseLockObj) { // SCIPIO: 2.1.0: Added eraseLockObj
            for (Map.Entry<Object, CacheLine<V>> entry : memoryTable.entrySet()) {
                if (removeInternal(entry.getKey(), entry.getValue(), true)) {
                    removed++;
                }
            }
        }
        return removed;
    }

    /**
     * Removes all element from the cache matching the given filter.
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.0.0: Added.</p>
     * <p>SCIPIO: 2.1.0: Removed synchronized because only used on remove calls and ConcurrentHashMap can usually
     * handle bulk removal iterations without it.</p>
     * @param entryFilter The entry filter - return true to remove key
     */
    public int removeByFilter(CacheEntryFilter<K, V> entryFilter) { // SCIPIO: 2.1.0: Removed synchronized
        int removed = 0;
        for (Map.Entry<Object, CacheLine<V>> entry : memoryTable.entrySet()) {
            if (entryFilter.filter(UtilGenerics.cast(toKey(entry.getKey())), UtilGenerics.cast(entry.getValue().getValue()))) {
                if (removeInternal(entry.getKey(), entry.getValue(), true)) {
                    removed++;
                }
            }
        }
        return removed;
    }

    /**
     * Removes all cache entries linked to the given index key.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void removeByIndexKey(String indexName, Object indexKey) {
        Index index = indexNameMap.get(indexName);
        if (index == null) {
            if (indexDefs.get(indexName) == null) {
                throw new IllegalArgumentException("Invalid index name [" + indexName + "] for cache [" + getName() + "]");
            } else {
                return;
            }
        }
        index.removeIndexLineMainKeysFromCache(indexKey);
    }

    /**
     * Removes all element from the cache matching the given key regex pattern (uses {@link java.util.regex.Matcher#matches()} on each key).
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyPat(Pattern keyPat) {
        return removeByFilter(new KeyPatternCacheEntryFilter<>(keyPat));
    }

    /**
     * Removes all element from the cache matching the given key regex pattern (uses {@link java.util.regex.Matcher#matches()} on each key).
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyPat(String keyPat) {
        return removeByFilter(new KeyPatternCacheEntryFilter<>(keyPat));
    }

    /**
     * Removes all element from the cache matching the given key prefix.
     * <p>NOTE: Typically one should suffix prefixes with {@link #SEPARATOR} (not added).</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyPrefix(String keyPrefix) {
        return removeByFilter(new KeyPrefixCacheEntryFilter<>(keyPrefix));
    }

    /**
     * Removes all element from the cache having the given key substring.
     * <p>NOTE: Typically one should suffix prefixes with {@link #SEPARATOR} (not added).</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeySubstring(String keySub) {
        return removeByFilter(new KeySubstringCacheEntryFilter<>(keySub));
    }

    /**
     * Removes all element from the cache matching the given key parts (pre-separated).
     * <p>null and empty string key parts are treated as wildcards matching any value in the key at that position.</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyParts(Collection<String> keyParts) {
        return removeByFilter(new KeyPartsCacheEntryFilter<>(keyParts));
    }

    /**
     * Removes all element from the cache matching the given key parts (pre-separated).
     * <p>null and empty string key parts are treated as wildcards matching any value in the key at that position.</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyParts(String... keyParts) {
        return removeByFilter(new KeyPartsCacheEntryFilter<>(keyParts));
    }

    /**
     * Removes all element from the cache matching the given key part string delimited by {@link #SEPARATOR}.
     * <p>null and empty string key parts are treated as wildcards matching any value in the key at that position.</p>
     * <p>e.g.: <code>"default::*::PH-1000"</code> (<code>delegator::productStoreId::productId::featureId</code>)</p>
     * <p>WARN: Slow on large caches.</p>
     * <p>SCIPIO: 2.1.0: Added.
     */
    public int removeByKeyPartString(String key) {
        return removeByFilter(new KeyPartsCacheEntryFilter<>(splitKey(key)));
    }

    /**
     * Cache entry filter.
     * <p>SCIPIO: 2.0.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)
     */
    public interface CacheEntryFilter<K, V> {
        boolean filter(K key, V value);
    }

    /**
     * Negating cache entry filter.
     * <p>SCIPIO: 2.0.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)
     */
    public static class NotCacheEntryFilter<K, V> implements CacheEntryFilter<K, V> {
        private final CacheEntryFilter<K, V> negatedFilter;

        public NotCacheEntryFilter(CacheEntryFilter<K, V> negatedFilter) {
            this.negatedFilter = negatedFilter;
        }

        public CacheEntryFilter<K, V> getNegatedFilter() {
            return negatedFilter;
        }

        @Override
        public boolean filter(K key, V value) {
            return !getNegatedFilter().filter(key, value);
        }
    }

    /**
     * Key pattern regex filter.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)}
     */
    public static class KeyPatternCacheEntryFilter<K, V> implements CacheEntryFilter<K, V> {
        private final Pattern keyPat;

        public KeyPatternCacheEntryFilter(Pattern keyPat) {
            this.keyPat = keyPat;
        }

        public KeyPatternCacheEntryFilter(String keyPat) {
            this(Pattern.compile(keyPat));
        }

        protected Pattern getKeyPat() {
            return keyPat;
        }

        @Override
        public boolean filter(K key, V value) {
            return (key != null && key != ObjectType.NULL && getKeyPat().matcher(key.toString()).matches());
        }
    }

    /**
     * Key prefix filter.
     * <p>NOTE: Typically one should suffix prefixes with {@link #SEPARATOR} (not added).</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)}
     * @see String#startsWith(String)
     */
    public static class KeyPrefixCacheEntryFilter<K, V> implements CacheEntryFilter<K, V> {
        private final String keyPrefix;

        public KeyPrefixCacheEntryFilter(String keyPrefix) {
            this.keyPrefix = keyPrefix;
        }

        protected String getKeyPrefix() {
            return keyPrefix;
        }

        @Override
        public boolean filter(K key, V value) {
            return (key != null && key != ObjectType.NULL && key.toString().startsWith(getKeyPrefix()));
        }
    }

    /**
     * Key substring filter.
     * <p>NOTE: Typically one should prefix and/or suffix prefixes with {@link #SEPARATOR} (not added).</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)}
     * @see String#startsWith(String)
     */
    public static class KeySubstringCacheEntryFilter<K, V> implements CacheEntryFilter<K, V> {
        private final String keySub;

        public KeySubstringCacheEntryFilter(String keySub) {
            this.keySub = keySub;
        }

        protected String getKeySub() {
            return keySub;
        }

        @Override
        public boolean filter(K key, V value) {
            return (key != null && key != ObjectType.NULL && key.toString().contains(getKeySub()));
        }
    }

    /**
     * Key parts filter, where each string in the key parts array corresponds to a delimited part of the target key,
     * or accepts any value if null.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @see #removeByFilter(CacheEntryFilter)}
     */
    public static class KeyPartsCacheEntryFilter<K, V> implements CacheEntryFilter<K, V> {
        private final List<String> keyParts;

        public KeyPartsCacheEntryFilter(Collection<String> keyParts) {
            this.keyParts = keyParts.stream()
                    .map(p -> (p != null && !p.isEmpty()) ? p : null)
                    .collect(Collectors.toCollection(ArrayList::new));
        }

        public KeyPartsCacheEntryFilter(String... keyParts) {
            this(Arrays.asList(keyParts));
        }

        protected List<String> getKeyParts() {
            return keyParts;
        }

        @Override
        public boolean filter(K key, V value) {
            List<String> keyParts = getKeyParts();
            if (keyParts.isEmpty()) {
                return true; // Accept all (wilcard by default)
            }
            if (key == null || key == ObjectType.NULL) {
                return false;
            }

            List<String> splitParts = StringUtil.split(null, key.toString(), SEPARATOR);
            if (splitParts == null) {
                return keyParts.isEmpty();
            }
            for(int i = 0; i < keyParts.size(); i++) {
                String keyPart = keyParts.get(i);
                if (keyPart != null && (i >= splitParts.size() || !keyPart.equals(splitParts.get(i)))) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * Removes all elements from this cache and clears counters.
     */
    public int clear() {
        int removed = erase();
        clearCounters();
        return removed;
    }

    public static void clearAllCaches() {
        clearAllCaches(null, null);
    }

    /**
     * Clears all caches with support for excludes.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static void clearAllCaches(Collection<String> excludeNames, Collection<Pattern> excludePatterns) {
        // We make a copy since clear may take time
        for (UtilCache<?,?> cache : utilCacheTable.values()) {
            if (excludeNames != null && excludeNames.contains(cache.getName())) {
                continue;
            } else if (excludePatterns != null) {
                boolean excluded = false;
                for(Pattern pat : excludePatterns) {
                    if (pat.matcher(cache.getName()).matches()) {
                        excluded = true;
                        break;
                    }
                }
                if (excluded) {
                    continue;
                }
            }
            cache.clear();
        }
    }

    public static Set<String> getUtilCacheTableKeySet() {
        Set<String> set = new HashSet<>(utilCacheTable.size());
        set.addAll(utilCacheTable.keySet());
        return set;
    }

    /**
     * Getter for the name of the UtilCache instance.
     * @return The name of the instance
     */
    public String getName() {
        return this.name;
    }

    /**
     * Getter for the simple name (without sequence index number) of the UtilCache instance.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @return The simple name of the instance
     */
    public String getBaseName() {
        return this.baseName;
    }

    /**
     * True if cache is enabled or false if disabled, in which case {@link #put} or {@link #putIfAbsent} have no effect
     * and {@link #get(Object)} operations always return null.
     * <p>Can be set false using explicit "enabled=false" cache property or {@link #setEnabled(boolean)}, and supports
     * real-time toggling only insofar as this flag is not <code>volatile</code>, meaning the jvm publishes the toggling
     * lazily to other threads with no time guarantee.</p>
     * <p>SCIPIO: 2.0.0: Added.</p>
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets cache enabled or disabled, in which case {@link #put} or {@link #putIfAbsent} have no effect
     * and {@link #get(Object)} operations always return null.
     * <p>Can be set false using explicit "enabled=false" cache property or {@link #setEnabled(boolean)}, and supports
     * real-time toggling only insofar as this flag is not <code>volatile</code>, meaning the jvm publishes the toggling
     * lazily to other threads with no time guarantee.</p>
     * <p>SCIPIO: 2.0.0: Added.</p>
     */
    public UtilCache<K, V> setEnabled(boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /**
     * Returns the number of successful hits on the cache.
     * @return The number of successful cache hits
     */
    public long getHitCount() {
        return this.hitCount.get();
    }

    /**
     * Returns the number of cache misses from entries that are not found in the cache.
     * @return The number of cache misses
     */
    public long getMissCountNotFound() {
        return this.missCountNotFound.get();
    }

    /**
     * Returns the number of cache misses from entries that are expired.
     * @return The number of cache misses
     */
    public long getMissCountExpired() {
        return this.missCountExpired.get();
    }

    /**
     * Returns the number of cache misses from entries that are have had the soft reference cleared out (by garbage collector and such).
     * @return The number of cache misses
     */
    public long getMissCountSoftRef() {
        return this.missCountSoftRef.get();
    }

    /**
     * Returns the number of cache misses caused by any reason.
     * @return The number of cache misses
     */
    public long getMissCountTotal() {
        return getMissCountSoftRef() + getMissCountNotFound() + getMissCountExpired();
    }

    public long getRemoveHitCount() {
        return this.removeHitCount.get();
    }

    public long getRemoveMissCount() {
        return this.removeMissCount.get();
    }

    /** Clears the hit and miss counters. */
    public void clearCounters() {
        this.hitCount.set(0);
        this.missCountNotFound.set(0);
        this.missCountExpired.set(0);
        this.missCountSoftRef.set(0);
        this.removeHitCount.set(0);
        this.removeMissCount.set(0);
    }

    public void setMaxInMemory(int newInMemory) {
        this.maxInMemory = newInMemory;
        Map<Object, CacheLine<V>> oldmap = this.memoryTable;
        if (newInMemory > 0) {
            if (this.memoryTable instanceof ConcurrentLinkedHashMap<?, ?>) {
                ((ConcurrentLinkedHashMap<?, ?>) this.memoryTable).setCapacity(newInMemory);
                return;
            }
            this.memoryTable = new ConcurrentLinkedHashMap.Builder<Object, CacheLine<V>>()
                    .maximumWeightedCapacity(newInMemory)
                    .build();
        } else {
            this.memoryTable = new ConcurrentHashMap<>();
        }
        this.memoryTable.putAll(oldmap);
    }

    public int getMaxInMemory() {
        return maxInMemory;
    }

    public void setSizeLimit(int newSizeLimit) {
        this.sizeLimit = newSizeLimit;
    }

    public int getSizeLimit() {
        return sizeLimit;
    }

    /**
     * Sets the expire delay for the cache elements, in milliseconds.
     * <p>If 0, elements never expire.</p>
     * <p>SCIPIO: 2.1.0: Corrected time -> delay, but neither of these methods are recommended anymore since
     * {@link Builder} is available.</p>
     * @param expireDelayMillis The expire time for the cache elements, in milliseconds
     */
    public void setExpireDelay(long expireDelayMillis) {
        // if expire time was <= 0 and is now greater, fill expire table now
        if (expireDelayMillis > 0) {
            long expireDelayNanos = TimeUnit.NANOSECONDS.convert(expireDelayMillis, TimeUnit.MILLISECONDS);
            this.expireDelayNanos = expireDelayNanos;
            for (Map.Entry<?, CacheLine<V>> entry: memoryTable.entrySet()) {
                // SCIPIO: 2.1.0: Defer pulse registration following memoryTable update
                CacheLine<V> newCacheLine = entry.getValue().changeLine(useSoftReference, expireDelayNanos);
                entry.setValue(newCacheLine);
                registerExpirePulse(newCacheLine);
            }
        } else {
            this.expireDelayNanos = 0;
            // if expire time was > 0 and is now <=, do nothing, just leave the load times in place, won't hurt anything...
        }
    }

    /**
     * Return the current expire time for the cache elements, in nanoseconds (approximated).
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @return The expire time for the cache elements, in nanoseconds.
     */
    public long getExpireDelayNanos() {
        return expireDelayNanos;
    }

    /**
     * Return the current expire time for the cache elements, in milliseconds.
     * @return The expire time for the cache elements, in milliseconds.
     */
    public long getExpireDelay() {
        return TimeUnit.MILLISECONDS.convert(expireDelayNanos, TimeUnit.NANOSECONDS);
    }

    /**
     * Sets the expire delay for the cache elements, in milliseconds.
     * <p>If 0, elements never expire.</p>
     * <p>SCIPIO: 2.1.0: The correct method to use is {@link #setExpireDelay(long)}, but neither of these methods
     * are recommended anymore since {@link Builder} is available.</p>
     * @param expireDelayMillis The expire time for the cache elements, in milliseconds
     * @see #setExpireDelay(long)
     */
    public void setExpireTime(long expireDelayMillis) {
        setExpireDelay(expireDelayMillis);
    }

    /**
     * Return the current expire time for the cache elements.
     * @return The expire time for the cache elements
     * @see #getExpireDelay()
     */
    public long getExpireTime() {
        return getExpireDelay();
    }

    /** Set whether or not the cache lines should use a soft reference to the data */
    public void setUseSoftReference(boolean useSoftReference) {
        if (this.useSoftReference != useSoftReference) {
            this.useSoftReference = useSoftReference;
            for (Map.Entry<?, CacheLine<V>> entry: memoryTable.entrySet()) {
                // SCIPIO: 2.1.0: Defer pulse registration following memoryTable update
                CacheLine<V> newCacheLine = entry.getValue().changeLine(useSoftReference, expireDelayNanos);
                entry.setValue(newCacheLine);
                registerExpirePulse(newCacheLine);
            }
        }
    }

    /** Return whether or not the cache lines should use a soft reference to the data */
    public boolean getUseSoftReference() {
        return this.useSoftReference;
    }

    /**
     * Returns the number of elements currently in the cache
     * @return The number of elements currently in the cache
     */
    public int size() {
        return memoryTable.size();
    }

    /**
     * Returns a boolean specifying whether or not an element with the specified key is in the cache.
     * @param key The key for the element, used to reference it in the hashtables and LRU linked list
     * @return True is the cache contains an element corresponding to the specified key, otherwise false
     */
    public boolean containsKey(Object key) {
        return (memoryTable.get(fromKey(key)) != null);
    }

    /**
     * Gets cache line keys.
     * <p>NOTE: this returns an unmodifiable copy of the keySet, so removing from here won't have an effect,
     * and calling a remove while iterating through the set will not cause a concurrent modification exception.
     * This behavior is necessary for now for the persisted cache feature.</p>
     */
    public Set<? extends K> getCacheLineKeys() {
        // note that this must be a HashSet and not a FastSet in order to have a null value
        Set<Object> keys;
        if (memoryTable.containsKey(ObjectType.NULL)) {
            keys = new HashSet<>(memoryTable.keySet());
            keys.remove(ObjectType.NULL);
            keys.add(null);
        } else {
            keys = memoryTable.keySet();
        }
        return Collections.unmodifiableSet(UtilGenerics.<Set<? extends K>>cast(keys));
    }

    public Collection<? extends CacheLine<V>> getCacheLineValues() {
        throw new UnsupportedOperationException();
    }

    private Map<String, Object> createLineInfo(int keyNum, K key, CacheLine<V> line) {
        Map<String, Object> lineInfo = new LinkedHashMap<>();
        lineInfo.put("elementKey", key);
        if (line.getLoadTimeNanos() > 0) {
            lineInfo.put("expireTimeMillis", TimeUnit.MILLISECONDS.convert(line.getExpireTimeNanos() - System.nanoTime(), TimeUnit.NANOSECONDS));
        }
        lineInfo.put("lineSize", findSizeInBytes(line.getValue(), key)); // SCIPIO: pass key
        lineInfo.put("keyNum", keyNum);
        return lineInfo;
    }

    public Collection<? extends Map<String, Object>> getLineInfos() {
        Set<? extends K> cacheLineKeys = getCacheLineKeys();
        List<Map<String, Object>> lineInfos = new ArrayList<>(cacheLineKeys.size());
        int keyIndex = 0;
        for (K key : cacheLineKeys) {
            CacheLine<V> line = memoryTable.get(fromKey(key));
            if (line != null) {
                lineInfos.add(createLineInfo(keyIndex, key, line));
            }
            keyIndex++;
        }
        return lineInfos;
    }

    /** Send a key addition event to all registered listeners */
    protected void noteAddition(K key, V newValue) {
        Collection<CacheListener<K, V>> listeners = this.listeners;
        for (CacheListener<K, V> listener : listeners) {
            listener.noteKeyAddition(this, key, newValue);
        }
    }

    /** Send a key removal event to all registered listeners */
    protected void noteRemoval(K key, V oldValue) {
        Collection<CacheListener<K, V>> listeners = this.listeners;
        for (CacheListener<K, V> listener : listeners) {
            listener.noteKeyRemoval(this, key, oldValue);
        }
    }

    /** Send a key update event to all registered listeners */
    protected void noteUpdate(K key, V newValue, V oldValue) {
        Collection<CacheListener<K, V>> listeners = this.listeners;
        for (CacheListener<K, V> listener : listeners) {
            listener.noteKeyUpdate(this, key, newValue, oldValue);
        }
    }

    /**
     * Adds an event listener for key removals.
     * <p>SCIPIO: 2.1.0: NOTE: Do not use cache listeners except best-effort stats; cache modifications not synchronized.</p>
     * <p>SCIPIO: 2.1.0: Added synchronization and optimized.</p>
     */
    public synchronized UtilCache<K, V> addListener(CacheListener<K, V> listener) {
        Collection<CacheListener<K, V>> listeners = this.listeners;
        if (listeners.isEmpty()) {
            listeners = new CopyOnWriteArraySet<>();
            listeners.add(listener);
            this.listeners = listeners;
        } else {
            listeners.add(listener);
        }
        return this;
    }

    /**
     * Removes an event listener for key removals.
     * <p>SCIPIO: 2.1.0: NOTE: Do not use cache listeners except best-effort stats; cache modifications not synchronized.</p>
     * <p>SCIPIO: 2.1.0: Added synchronization and optimized.</p>
     */
    public synchronized UtilCache<K, V>  removeListener(CacheListener<K, V> listener) {
        Collection<CacheListener<K, V>> listeners = this.listeners;
        if (!listeners.isEmpty()) {
            listeners.remove(listener);
            if (listeners.isEmpty()) {
                this.listeners = Collections.emptySet();
            }
        }
        return this;
    }

    /** Checks for a non-expired key in a specific cache */
    public static boolean validKey(String cacheName, Object key) {
        UtilCache<?, ?> cache = findCache(cacheName);
        if (cache != null) {
            if (cache.containsKey(key)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Splits key parts from a separator-delimited key.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<String>> C splitKey(C out, String key) {
        return StringUtil.split(out, key, SEPARATOR);
    }

    /**
     * Splits key parts from a separator-delimited key.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static List<String> splitKey(String key) {
        return splitKey(new ArrayList<>(), key);
    }

    public static Object getCacheElement(String cacheName, String key) {
        try{
            UtilCache<?, ?> cacheTable = utilCacheTable.get(cacheName);
            if(UtilValidate.isNotEmpty(cacheTable)){
                return cacheTable.get(key);
            }
        }catch(Exception e){
        }
        return null;
    }

    public static void clearCachesThatStartWith(String cacheNamePrefix) {
        for (Map.Entry<String, UtilCache<?, ?>> entry : utilCacheTable.entrySet()) {
            String name = entry.getKey();
            if (name.startsWith(cacheNamePrefix)) {
                entry.getValue().clear();
            }
        }
    }

    public static void clearKeysThatStartWithFromCache(String cacheName, String keyPrefix) {
        try {
            UtilCache<?, ?> cacheObj = utilCacheTable.get(cacheName);
            if (cacheObj != null) {
                cacheObj.removeByKeyPrefix(keyPrefix);
            }
        } catch(Exception e) {
            Debug.logWarning("Could not find or clear caches from cache [" + cacheName + "]", module);
        }
    }

    public static void clearKeysThatContainFromCache(String cacheName, String keySub) {
        try{
            UtilCache<?, ?> cacheObj = utilCacheTable.get(cacheName);
            if (cacheObj != null) {
                cacheObj.removeByKeySubstring(keySub);
            }
        } catch(Exception e) {
            Debug.logWarning("Could not find or clear caches from cache "+cacheName,module);
        }
    }

    public static void clearCache(String cacheName) {
        UtilCache<?, ?> cache = findCache(cacheName);
        if (cache == null) {
            return;
        }
        cache.clear();
    }

    /**
     * Removal of individual cache objects by key
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void clearCacheValue(String cacheName,String key) {
        UtilCache<?, ?> cache = findCache(cacheName);
        if (cache == null) {
            return;
        }
        try {
            cache.remove(key);
        } catch(Exception e) {
            // TODO: review
        }
    }

    /**
     * Outputs descriptors to a map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <M extends Map<String, Map<String, Object>>> M getDescriptors(M out, Collection<String> types) {
        for(Map.Entry<String, UtilCache<?, ?>> entry : (out instanceof TreeMap ? utilCacheTable.entrySet() : new TreeSet<>(utilCacheTable.entrySet()))) {
            out.put(entry.getKey(), entry.getValue().getDescriptor(types));
        }
        return out;
    }

    /**
     * Outputs descriptors to a sorted map copy.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static Map<String, Map<String, Object>> getDescriptors(Collection<String> types) {
        return getDescriptors(new TreeMap<>(), types);
    }

    /**
     * Outputs descriptor fields to the given map
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <M extends Map<String, Object>> M getDescriptor(M out, Collection<String> types) { // SCIPIO: 2.1.0: Added for convenience/conversion
        if (types == null || types.contains("id")) {
            //out.put("baseName", this.getBaseName());
            out.put("name", this.getName());
            //out.put("cacheName", this.getName());
        }
        if (types == null || types.contains("options")) {
            out.put("enabled", this.isEnabled());
            out.put("sizeLimit", this.getSizeLimit());
            out.put("maxInMemory", this.getMaxInMemory());
            out.put("expireDelay", this.getExpireDelay());
            out.put("expireDelayNanos", this.expireDelayNanos);
            out.put("useSoftReference", this.getUseSoftReference());
            //out.put("keyFormat", this.keyFormat);
            //out.put("indexDefs", this.indexDefs);
        }
        if (types == null || types.contains("stats-basic")) {
            out.put("size", this.size());
            //out.put("cacheSize", this.size());
        }
        if (types == null || types.contains("stats-hit")) {
            out.put("hitCount", this.getHitCount());
            out.put("missCountTotal", this.getMissCountTotal());
            out.put("missCountNotFound", this.getMissCountNotFound());
            out.put("missCountExpired", this.getMissCountExpired());
            out.put("missCountSoftRef", this.getMissCountSoftRef());
            out.put("removeHitCount", this.getRemoveHitCount());
            out.put("removeMissCount", this.getRemoveMissCount());
        }
        if (types == null || types.contains("stats-mem")) {
            out.put("cacheMemory", this.getSizeInBytes());
        }
        return out;
    }

    /**
     * Outputs descriptor fields to a map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public Map<String, Object> getDescriptor(Collection<String> types) {
        return getDescriptor(new LinkedHashMap<>(), types);
    }

    /**
     * Makes a map of indexes by index name describing for each index a keyFormat derived from the supplied memoryTable
     * entry full keyFormat.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected Map<String, Index> makeIndexes(Map<String, ?> indexDefs) {
        Map<String, Index> indexTables = new LinkedHashMap<>();
        for(Map.Entry<String, ?> entry : indexDefs.entrySet()) {
            String indexName = entry.getKey();
            String indexKeyFormat = null;
            if (entry.getValue() instanceof String) {
                indexKeyFormat = (String) entry.getValue();
            } else if (entry.getValue() instanceof Map) {
                Map<String, ?> values = UtilGenerics.cast(entry.getValue());
                indexKeyFormat = (String) values.get("keyFormat");
                boolean enabled = UtilMisc.booleanValue(values.get("enabled"), true);
                if (!enabled) {
                    Debug.logInfo("Cache [" + getName() + "] index [" + indexName + "] disabled; skipping registration", module);
                    continue;
                }
            }
            if (UtilValidate.isEmpty(indexKeyFormat)) {
                Debug.logWarning("Cache [" + getName() + "] index [" + indexName + "] does not define a keyFormat; treating as disabled and skipping registration", module);
                continue;
            }
            Index index = makeIndex(indexName, indexKeyFormat, indexDefs);
            indexTables.put(indexName, index);
        }
        return indexTables;
    }

    protected Index getIndex(String indexName) {
        return indexNameMap.get(indexName);
    }

    protected void addCacheLineToIndexes(CacheLine<V> cacheLine) { // SCIPIO: 2.1.0: Added
        for(Index index : indexList) {
            index.addCacheLine(cacheLine);
        }
    }

    protected void removeCacheLineFromIndexes(CacheLine<V> cacheLine) { // SCIPIO: 2.1.0: Added
        for(Index index : indexList) {
            index.removeCacheLine(cacheLine);
        }
    }

    protected Index makeIndex(String indexName, String indexKeyFormat, Map<String, ?> indexDefs) {
        return new Index(indexName, indexKeyFormat, indexDefs);
    }

    /**
     * Maps index keys to sets of cache lines.
     * <p>Configured via cache.properties or via {@link Builder}.</p>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected final class Index implements Serializable {
        protected final String indexName;
        protected final String indexKeyFormat;
        protected final List<Integer> indexKeyOffsets;

        /**
         * Map of index keys to index lines of cache lines.
         * <p>NOTE: As soon as an index line goes empty, it is considered expired and expected to be removed
         * (non-trivial for fast synchronization).</p>
         */
        protected final ConcurrentMap<Object, IndexLine> indexLines;

        /**
         * Map of index keys to index lines of cache lines.
         * <p>NOTE: As soon as an index line goes empty, it is considered expired and expected to be removed
         * (non-trivial for fast synchronization).</p>
         */
        protected final ConcurrentMap<CacheLine<V>, IndexNode> indexNodes;

        protected Index(String indexName, String indexKeyFormat, Map<String, ?> indexDefs) {
            if (UtilValidate.isEmpty(name)) {
                throw new IllegalArgumentException("missing index name");
            }
            if (UtilValidate.isEmpty(indexKeyFormat)) {
                throw new IllegalArgumentException("missing index keyFormat");
            }
            this.indexName = indexName;
            this.indexKeyFormat = indexKeyFormat;

            if (indexKeyFormat.startsWith("[") && indexKeyFormat.endsWith("]")) {
                List<Integer> newIndexKeyOffsets = Arrays.stream(indexKeyFormat.substring(1, indexKeyFormat.length() - 1).split(","))
                        .map(Integer::parseInt).collect(Collectors.toList());
                this.indexKeyOffsets = newIndexKeyOffsets;
            } else {
                String[] splitKeyFormat = UtilCache.this.keyFormat.split(SEPARATOR);
                String[] splitIndexKeyFormat = this.indexKeyFormat.split(SEPARATOR);
                Collection<?> indexKeyOffsets = extractIndexKeyOffsets(splitIndexKeyFormat, splitKeyFormat);
                if (UtilValidate.isEmpty(indexKeyOffsets)) {
                    throw new IllegalArgumentException("invalid index keyFormat");
                }
                List<Integer> newIndexKeyOffsets = new ArrayList<>(indexKeyOffsets.size());
                for (Object indexKeyOffsetObj : indexKeyOffsets) {
                    Integer indexKeyOffset = UtilMisc.toInteger(indexKeyOffsetObj, null);
                    if (indexKeyOffset == null || indexKeyOffset < 0 || indexKeyOffset >= splitKeyFormat.length) {
                        throw new IllegalArgumentException("invalid index keyFormat");
                    }
                    newIndexKeyOffsets.add(indexKeyOffset);
                }
                UtilGenerics.<ArrayList<?>>cast(newIndexKeyOffsets).trimToSize();
                this.indexKeyOffsets = newIndexKeyOffsets;
            }
            this.indexLines = new ConcurrentHashMap<>();
            this.indexNodes = new ConcurrentHashMap<>();
        }

        public String getIndexName() {
            return indexName;
        }

        public String getIndexKeyFormat() {
            return indexKeyFormat;
        }

        public Object getIndexKey(CacheLine<?> cacheLine) {
            return extractIndexKey(cacheLine.getKey());
        }

        public Object extractIndexKey(Object fullKey) {
            return UtilCache.this.extractIndexKey(fullKey.toString(), indexKeyOffsets);
        }

        /**
         * Adds a cache line to the index (only).
         * <p>Contract: At the end of this call, the cache is guaranteed to be in the index line and the index line
         * is checked to ensure it is still the active one before the call finishes. This is only correct and valid
         * as this method must only be called <i>before</i> the cacheline is added to the memoryTable; no other
         * use of this method is valid.</p>
         */
        protected void addCacheLine(CacheLine<V> cacheLine) {
            Object indexKey = getIndexKey(cacheLine);
            if (indexKey == null) {
                return;
            }
            while (true) {
                IndexLine indexLine = indexLines.get(indexKey);
                if (indexLine == null) {
                    indexLine = new IndexLine(new IndexNode(cacheLine));
                    IndexLine prevIndexLine = indexLines.putIfAbsent(indexKey, indexLine);
                    if (prevIndexLine == null) {
                        return; // Successfully added new index line
                    } else {
                        indexLine = prevIndexLine;
                    }
                }
                // TODO: Optimize: synchronized can probably be more fine-grained, but difficult due to linked list
                synchronized (indexLine) {
                    // Add as first instead of last so that iterators/loops that start at an index node won't go over newly-added nodes
                    IndexNode firstIndexNode = indexLine.first;
                    if (firstIndexNode == null) {
                        // Don't add to empty index line, always remove and create new line, by convention
                        indexLines.remove(indexKey, indexLine);
                        continue;
                    }
                    IndexNode indexNode = new IndexNode(cacheLine, null, firstIndexNode);
                    firstIndexNode.previous = indexNode;
                    indexLine.first = indexNode;
                    indexNodes.put(cacheLine, indexNode);
                    return;
                }
            }
        }

        /**
         * Removes cache line from the index (only).
         */
        protected void removeCacheLine(CacheLine<V> cacheLine) {
            Object indexKey = getIndexKey(cacheLine);
            if (indexKey == null) {
                return;
            }
            IndexLine indexLine = indexLines.get(indexKey);
            if (indexLine == null) {
                return;
            }
            // TODO: Optimize: synchronized can probably be more fine-grained, but difficult due to linked list
            synchronized (indexLine) {
                IndexNode indexNode = indexNodes.remove(cacheLine);
                if (indexNode != null) {
                    IndexNode prevIndexNode = indexNode.previous;
                    IndexNode nextIndexNode = indexNode.next;
                    if (prevIndexNode != null) {
                        prevIndexNode.next = indexNode.next;
                    }
                    if (nextIndexNode != null) {
                        nextIndexNode.previous = indexNode.previous;
                    }
                    if (indexLine.first == indexNode) {
                        indexLine.first = indexNode.next;
                    }
                    //if (indexLine.last == indexNode) {
                    //    indexLine.last = indexNode.previous;
                    //}
                }
                if (indexLine.first == null) {
                    indexLines.remove(indexKey, indexLine);
                }
            }
        }

        /**
         * Performs a general cache remove on all keys recorded in this index line.
         * <p>NOTE: Not fully synchronized for speed reasons.</p>
         */
        protected void removeIndexLineMainKeysFromCache(Object indexKey) {
            IndexLine indexLine = this.indexLines.remove(indexKey);
            if (indexLine == null) {
                return;
            }
            IndexNode indexNode;
            synchronized (indexLine) {
                indexNode = indexLine.first;
            }
            // NOTE: Not using synchronized block because it should be unnecessary as long as the node next references are never nulled,
            // the removals are guarded against multiple removals by ConcurrentMap.remove(Object, Object) in removeInternal(),
            // and the addCacheLine() calls add at head/first instead of at tail/last so they don't affect this iteration.
            while(indexNode != null) {
                CacheLine<V> cacheLine = UtilGenerics.cast(indexNode.cacheLine);
                removeInternal(cacheLine.getKey(), cacheLine, true);
                indexNode = indexNode.next;
            }
        }
    }

    /**
     * An index line, one per index key, container cache lines.
     * <p>Considered expired and removed from the index table as soon as empty.</p>
     * <p>NOTE: This was originally an encapsulated inner class of Index with synchronized(this), but to lower memory
     * usage and overhead a trivial wrapper is preferable; Index synchronizes on IndexLine instances and implements
     * all methods itself to retain access to UtilCache and Index members.</o>
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    private final static class IndexLine implements Serializable {
        protected volatile IndexNode first;
        //protected volatile IndexNode last; // not currently needed
        // Not needed
        //protected volatile int size;
        protected final ReentrantLock removeLock = new ReentrantLock();

        IndexLine(IndexNode initialNode) {
            this.first = initialNode;
            //this.last = initialNode;
        }
    }

    private final static class IndexNode implements Serializable {
        private final CacheLine<?> cacheLine;
        private volatile IndexNode previous;
        private volatile IndexNode next;

        IndexNode(CacheLine<?> cacheLine, IndexNode previous, IndexNode next) {
            this.cacheLine = cacheLine;
            this.previous = previous;
            this.next = next;
        }

        IndexNode(CacheLine<?> cacheLine) {
            this(cacheLine, null, null);
        }
    }

    /**
     * Extracts an index key using an array of key part positions in a full key for this cache.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected String extractIndexKey(String fullKey, List<Integer> indexKeyOffsets) {
        return extractIndexKey(fullKey, indexKeyOffsets, SEPARATOR);
    }

    /**
     * Extracts an index key using an array of key part positions in a full key for this cache.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected static String extractIndexKey(String fullKey, List<Integer> indexKeyOffsets, String delim) {
        StringBuilder indexKey = new StringBuilder();
        for(int indexKeyOffset : indexKeyOffsets) {
            int fullKeyIndex = 0;
            int partBeginStrIndex = 0;
            boolean fullKeyIndexFound = false;
            // NOTE: because indexKeyOffsets may be out of order, we must re-loop for each offset
            while(!fullKeyIndexFound && partBeginStrIndex < fullKey.length()) {
                int partEndStrIndex = fullKey.indexOf(delim, partBeginStrIndex);
                if (fullKeyIndex == indexKeyOffset) {
                    if (indexKey.length() > 0) {
                        indexKey.append(delim);
                    }
                    indexKey.append(fullKey.substring(partBeginStrIndex, partEndStrIndex));
                    fullKeyIndexFound = true;
                } else {
                    partBeginStrIndex = partEndStrIndex + delim.length();
                    fullKeyIndex++;
                }
            }
        }
        return (indexKey.length() > 0) ? indexKey.toString() : null;
    }

    /**
     * Extracts an index key using an array of key part positions in a main key.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected static List<Integer> extractIndexKeyOffsets(String[] splitIndexKeyFormat, String[] splitKeyFormat) {
        return extractIndexKeyOffsets(new ArrayList<>(splitKeyFormat.length),
                splitIndexKeyFormat, splitKeyFormat);
    }

    /**
     * Extracts an index key using an array of key part positions in a main key.
     * <p>SCIPIO: 2.1.0: Added for UtilCache.Index support.</p>
     */
    protected static <C extends Collection<V>, V> C extractIndexKeyOffsets(C out, String[] splitIndexKeyFormat, String[] splitKeyFormat) {
        if (UtilValidate.isEmpty(splitIndexKeyFormat)) {
            throw new IllegalArgumentException("splitIndexKeyFormat empty");
        }
        if (UtilValidate.isEmpty(splitKeyFormat)) {
            throw new IllegalArgumentException("splitKeyFormat empty");
        }
        for (int i = 0; i < splitIndexKeyFormat.length; i++) {
            // If it's a number, use as index; otherwise map to index by name
            int k;
            try {
                k = Integer.parseInt(splitIndexKeyFormat[i]);
                out.add(UtilGenerics.cast(k));
                continue;
            } catch(NumberFormatException e) {
            }
            boolean found = false;
            k = 0;
            for(; k < splitKeyFormat.length; k++) {
                if (splitIndexKeyFormat[i].equals(splitKeyFormat[k])) {
                    found = true;
                    break;
                }
            }
            if (found) {
                out.add(UtilGenerics.cast(k));
            } else {
                Debug.logError("Invalid index key format - invalid key part name [" + splitIndexKeyFormat[i] +
                        "] from splitIndexKeyFormat " + Arrays.toString(splitIndexKeyFormat) + " splitKeyFormat " +
                        Arrays.toString(splitKeyFormat) + "; ignoring", module);
            }
        }
        return out;
    }

    // SCIPIO: 2.1.0: Extended overload.
    public static <K, V> UtilCache<K, V> getOrCreateUtilCache(String name, int sizeLimit, int maxInMemory, long expireDelay, boolean useSoftReference, String keyFormat, Map<String, ?> indexDefs, String... propNames) {
        // SCIPIO: 2.1.0: Refactored
        return getOrCreate(name, () -> new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .maxInMemory(maxInMemory)
                .expireDelay(expireDelay)
                .useSoftReference(useSoftReference)
                .keyFormat(keyFormat)
                .indexDefs(indexDefs)
                .propNames(propNames)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //UtilCache<K, V> cache = new UtilCache<>(cacheName, sizeLimit, maxInMemory, expireDelay, useSoftReference, keyFormat, indexDefs, name, names);
    }

    public static <K, V> UtilCache<K, V> getOrCreateUtilCache(String name, int sizeLimit, int maxInMemory, long expireDelay, boolean useSoftReference, String... propNames) {
        return getOrCreateUtilCache(name, sizeLimit, maxInMemory, expireDelay, useSoftReference, (String) null, (Map<String, ?>) null, propNames);
    }

    public static <K, V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, int maxInMemory, long expireDelay, boolean useSoftReference, String... propNames) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .maxInMemory(maxInMemory)
                .expireDelay(expireDelay)
                .useSoftReference(useSoftReference)
                .propNames(propNames)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, sizeLimit, maxInMemory, expireTime, useSoftReference, null, null, name, names));
    }

    public static <K, V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, int maxInMemory, long expireDelay, boolean useSoftReference) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .maxInMemory(maxInMemory)
                .expireDelay(expireDelay)
                .useSoftReference(useSoftReference)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, sizeLimit, maxInMemory, expireDelay, useSoftReference, null, null, name));
    }

    public static <K,V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, long expireDelay, boolean useSoftReference) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .expireDelay(expireDelay)
                .useSoftReference(useSoftReference)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, sizeLimit, sizeLimit, expireDelay, useSoftReference, null, null, name));
    }

    public static <K,V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, long expireDelay) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .expireDelay(expireDelay)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, sizeLimit, sizeLimit, expireDelay, false, null, null, name));
    }

    public static <K,V> UtilCache<K, V> createUtilCache(int sizeLimit, long expireDelay) {
        // SCIPIO: 2.1.0: Refactored
        return create("specified", new Builder<K, V>()
                .sizeLimit(sizeLimit)
                .expireDelay(expireDelay)
        );
        //String cacheName = "specified" + getNextDefaultIndex("specified");
        //return storeCache(new UtilCache<>(cacheName, sizeLimit, sizeLimit, expireDelay, false, null, null, "specified"));
    }

    /**
     * Creates a util cache with the given simple name.
     * <p>SCIPIO: NOTE: Due to legacy behavior, this method falls backs on <code>default.*=</code> settings
     * in cache.properties when specific cache settings are not present.</p>
     * <p>SCIPIO: 2.1.0: This method was fixed to no longer accidentally prioritize <code>default.*=</code> properties over
     * specific cache name properties.</p>
     */
    public static <K,V> UtilCache<K, V> createUtilCache(String name, boolean useSoftReference) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .useSoftReference(useSoftReference)
                .propNames("default", name)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, 0, 0, 0, useSoftReference, null, null, "default", name));
    }

    /**
     * Creates a util cache with the given simple name.
     * <p>SCIPIO: NOTE: Due to legacy behavior, this method falls backs on <code>default.*=</code> settings
     * in cache.properties when specific cache settings are not present.</p>
     * <p>SCIPIO: 2.1.0: This method was fixed to no longer accidentally prioritize <code>default.*=</code> properties over
     * specific cache name properties.</p>
     */
    public static <K,V> UtilCache<K, V> createUtilCache(String name) {
        // SCIPIO: 2.1.0: Refactored
        return create(name, new Builder<K, V>()
                .propNames("default", name)
        );
        //String cacheName = name + getNextDefaultIndex(name);
        //return storeCache(new UtilCache<>(cacheName, 0, 0, 0, false, null, null, "default", name));
    }

    public static <K,V> UtilCache<K, V> createUtilCache() {
        // SCIPIO: 2.1.0: Refactored
        return create(null, new Builder<K, V>());
        //String cacheName = "default" + getNextDefaultIndex("default");
        //return storeCache(new UtilCache<>(cacheName, 0, 0, 0, false, null, null, "default"));
    }

    /**
     * SCIPIO: Creates UtilCache without storing. FOR TESTING ONLY.
     * @deprecated SCIPIO: 2.1.0: Use {@link Builder} instead.
     * <p>
     * Added 2018-09-14.
     */
    @Deprecated
    public static <K, V> UtilCache<K, V> createOnlyUtilCache(String name, int sizeLimit, int maxInMemory, long expireDelay, boolean useSoftReference, String... propNames) {
        // SCIPIO: 2.1.0: Refactored
        return new Builder<K, V>(propNames.length > 0 ? propNames[propNames.length - 1] : name)
                .name(name)
                .sizeLimit(sizeLimit)
                .maxInMemory(maxInMemory)
                .expireDelay(expireDelay)
                .useSoftReference(useSoftReference)
                .propNames(propNames)
                .build();
        //return new UtilCache<>(cacheName, sizeLimit, maxInMemory, expireDelay, useSoftReference, null, null, propName, propNames);
    }

    @SuppressWarnings("unchecked")
    public static <K, V> UtilCache<K, V> findCache(String cacheName) {
        return (UtilCache<K, V>) UtilCache.utilCacheTable.get(cacheName);
    }

    @Override
    public void onEviction(Object key, CacheLine<V> cacheLine) {
        if (!indexList.isEmpty()) { // SCIPIO: 2.1.0: Added for UtilCache.Index support
            removeCacheLineFromIndexes(cacheLine);
        }
        // SCIPIO: 2.1.0: Added cancel for consistency.
        // TODO: REVIEW: It's unclear this is needed but may help guarantee soft refs clear
        cancel(cacheLine);
        // SCIPIO: removePulse becomes expensive and is not strictly needed because deletions are by CacheLine not by key and putIfAbsent() helps
        //ExecutionPool.removePulse(cacheLine);
    }

    /**
     * @deprecated SCIPIO: use overload without useFileSystemStore (flag ignored - 2018-08-20).
     */
    @Deprecated
    public static <K, V> UtilCache<K, V> getOrCreateUtilCache(String name, int sizeLimit, int maxInMemory, long expireTime, boolean useSoftReference, boolean useFileSystemStore, String... names) {
        Debug.logWarning("Deprecated method called: getOrCreateUtilCache with useFileSystemStore", module);
        return getOrCreateUtilCache(name, sizeLimit, maxInMemory, expireTime, useSoftReference, names);
    }

    /**
     * @deprecated SCIPIO: use overload without useFileSystemStore (flag ignored - 2018-08-20).
     */
    @Deprecated
    public static <K, V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, int maxInMemory, long expireTime, boolean useSoftReference, boolean useFileSystemStore, String... propNames) {
        Debug.logWarning("Deprecated method called: createUtilCache with useFileSystemStore", module);
        return createUtilCache(name, sizeLimit, maxInMemory, expireTime, useSoftReference, propNames);
    }

    /**
     * @deprecated SCIPIO: use overload without useFileSystemStore (flag ignored - 2018-08-20).
     */
    @Deprecated
    public static <K, V> UtilCache<K, V> createUtilCache(String name, int sizeLimit, int maxInMemory, long expireTime, boolean useSoftReference, boolean useFileSystemStore) {
        Debug.logWarning("Deprecated method called: createUtilCache with useFileSystemStore", module);
        return createUtilCache(name, sizeLimit, maxInMemory, expireTime, useSoftReference);
    }

    /**
     * @deprecated SCIPIO: no longer implemented (always returns false - 2018-08-20).
     */
    @Deprecated
    public boolean getUseFileSystemStore() {
        Debug.logWarning("Deprecated method called: getUseFileSystemStore", module);
        return false;
    }
}
