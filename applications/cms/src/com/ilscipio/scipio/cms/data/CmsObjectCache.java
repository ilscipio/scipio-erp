package com.ilscipio.scipio.cms.data;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

import com.ilscipio.scipio.cms.data.Preloadable.PreloadWorker;

/**
 * Generic Cms object cache.
 * <p>
 * The CmsObject must be thread-safe OR its instance never modified after a put.
 * <p>
 * NOTE: See CmsObject for caching-related flags.
 * <p>
 * NOTE: 2016: The put method of the cache will achieve thread safety for its object by calling
 * {@link CmsObject#preloadContent}.
 * ALL OBJECTS MUST DILIGENTLY IMPLEMENT THE {@link CmsObject#preloadContent} METHOD!
 * <p>
 * WARN: OBJECTS STORED IN CACHE MUST NOT BE MODIFIED BY CALLER AFTERWARD! Otherwise thread safety is lost.
 */
public abstract class CmsObjectCache<T extends CmsObject> {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final int EXPIRATION_TIME_DEFAULT = UtilProperties.getPropertyAsInteger("cms", "cache.data.expiration", 10000);

    /**
     * Debug-like flag: If true, the cached preloaded objects are kept mutable
     * instead of being made explicitly immutable. 
     * NOTE: in all cases, cached live render objects should NEVER be modified;
     * this switch is for debugging and optimization only.
     * <p>
     * 2016-12: currently leaving this false for debug and helping correctness.
     * The only reason to set this true is performance but might be negligeable.
     */
    public static final boolean CACHE_PRELOAD_MUTABLE = false;
    
    private static final PreloadWorker cachePreloadWorker = PreloadWorker.getPreloadWorker(CACHE_PRELOAD_MUTABLE);
        
    protected CmsObjectCache() {
    }
    
    // factories allow disabling the caches more easily
    // NOTE: global should always have a name, while 
    // local should not (because would have to generate tons of unique names - let cache do that)
    
    public static <T extends CmsObject> CmsObjectCache<T> getGlobalCache(String cacheName, int expireTime) {
        if (CmsObject.ALLOW_GLOBAL_OBJ_CACHE) {
            return new UtilCmsObjectCache<>(cacheName, expireTime);
        } else {
            return new DummyCmsObjectCache<>();
        }
    }
    
    public static <T extends CmsObject> CmsObjectCache<T> getGlobalCache(String cacheName) {
        if (CmsObject.ALLOW_GLOBAL_OBJ_CACHE) {
            return new UtilCmsObjectCache<>(cacheName);
        } else {
            return new DummyCmsObjectCache<>();
        }
    }
    
    public static <T extends CmsObject> CmsObjectCache<T> getLocalCache(int expireTime) {
        if (CmsObject.ALLOW_LOCAL_OBJ_CACHE) {
            return new UtilCmsObjectCache<>(expireTime);
        } else {
            return new DummyCmsObjectCache<>();
        }
    }
    
    public static <T extends CmsObject> CmsObjectCache<T> getLocalCache() {
        if (CmsObject.ALLOW_LOCAL_OBJ_CACHE) {
            return new UtilCmsObjectCache<>();
        } else {
            return new DummyCmsObjectCache<>();
        }
    }
    
    public static <K, V> UtilCache<K, V> getGenericGlobalCache(String cacheName) {
        if (CmsObject.ALLOW_GLOBAL_OBJ_CACHE) {
            return createCache(cacheName, readExpireTime(cacheName));
        } else {
            return null;
        }
    }
    
    /**
     * Gets cache entry, or null if no attempt was made to store in cache yet.
     */
    public abstract CacheEntry<T> getEntry(String key);
    
    /**
     * NOTE: use {@link #getEntry} instead; the caller SHOULD
     * handle the already-looked-up-and-nothing-was-found case, 
     * otherwise cache is only half-effective.
     */
    public T get(String key) {
        CacheEntry<T> entry = getEntry(key);
        if (entry != null && entry.hasValue()) {
            return entry.getValue(); 
        } else {
            return null;
        }
    }

    public abstract void clear();

    public abstract void put(String key, T value);
    
    public abstract void remove(String key);

    public static int getDefaultExpiration() {
        return EXPIRATION_TIME_DEFAULT;
    }

    public static Integer readExpireTime(String cacheName) {
        Integer expireTime = null;
        // FIRST, lookup the cache name in cache.properties, so
        // consistent with rest of system
        String expireTimeStr = UtilProperties.getPropertyValue("cache", cacheName);
        if (UtilValidate.isNotEmpty(expireTimeStr)) {
            try {
                expireTime = (Integer) ObjectType.simpleTypeConvert(expireTimeStr, "Integer", null, null);
            } catch (GeneralException e) {
                Debug.logWarning("Invalid cache integer value in cache.properties for " + cacheName + ": " + expireTimeStr + "; ignoring", module);
                expireTime = null;
            }
        } 
        
        // THEN, if not there or invalid, use the value from cms.properties
        if (expireTime == null) {
            expireTime = getDefaultExpiration();
        }

        return expireTime;
    }
    
    public static <K, V> UtilCache<K, V> createCache(String cacheName, Integer expireTime) {
        // only log this one (with name) because sure to be a global cache; local caches would get too verbose
        Debug.logInfo("Cms: Creating cache '" + cacheName + "' with expireTime '" + expireTime + "'", module);
        return UtilCache.createUtilCache(cacheName, 0, expireTime);
    }
    
    /**
     * This is used to prevent lookups for NON-matches.
     */
    public static abstract class CacheEntry<T extends CmsObject> {
        protected CacheEntry() {
        }
        
        public abstract boolean hasValue();
        
        public abstract T getValue();
    }
    
    public static class SimpleCacheEntry<T extends CmsObject> extends CacheEntry<T> {
        private final T value;
        
        public SimpleCacheEntry(T value) {
            this.value = value;
        }
        
        public boolean hasValue() {
            return value != null;
        }
        
        public T getValue() {
            return value;
        }
    }
    
    public static class UtilCmsObjectCache<T extends CmsObject> extends CmsObjectCache<T> {
        private UtilCache<String, SimpleCacheEntry<T>> cache;

        public UtilCmsObjectCache(String cacheName, Integer expireTime) {
            if (expireTime == null) expireTime = readExpireTime(cacheName);
            cache = createCache(cacheName, expireTime);
        }
        
        public UtilCmsObjectCache(int expireTime) {
            cache = UtilCache.<String, SimpleCacheEntry<T>> createUtilCache(0, expireTime);
        }

        public UtilCmsObjectCache(String cacheName) {
            this(cacheName, null);
        }
        
        public UtilCmsObjectCache() {
            this(getDefaultExpiration());
        }

        
        /**
         * Gets cache entry, or null if no attempt was made to store in cache yet.
         */
        public CacheEntry<T> getEntry(String key) {
            return cache.get(key);
        }
        
        /**
         * NOTE: use {@link #getEntry} instead; the caller SHOULD
         * handle the already-looked-up-and-nothing-was-found case, 
         * otherwise cache is only half-effective.
         */
        public T get(String key) {
            CacheEntry<T> entry = getEntry(key);
            if (entry != null && entry.hasValue()) {
                return entry.getValue(); 
            } else {
                return null;
            }
        }

        public void clear() {
            cache.clear();
        }

        public void put(String key, T value) {
            // 2016: THREAD SAFETY handled through forced preload call
            // after the object is preloaded, the cache should provide consistent safe published view
            // to other threads.
            cachePreloadWorker.preload(value);
            
            cache.put(key, new SimpleCacheEntry<T>(value));
        }
        
        public void remove(String key) {
            cache.remove(key);
        }

    }
    
    
    /**
     * Doesn't cache at all - use this to simplify the implementing methods' logic.
     */
    public static class DummyCmsObjectCache<T extends CmsObject> extends CmsObjectCache<T> {

        public DummyCmsObjectCache() {
            super();
        }

        @Override
        public CacheEntry<T> getEntry(String key) {
            return null;
        }

        @Override
        public void clear() {
        }

        @Override
        public void put(String key, T value) {
        }

        @Override
        public void remove(String key) {
        }
        
    }
    
}
