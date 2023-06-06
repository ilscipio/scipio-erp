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
package org.ofbiz.base.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections4.iterators.EnumerationIterator;
import org.ofbiz.base.util.collections.MapComparator;

/**
 * General and most commonly used language and utility functions for common types such as collections, maps, booleans,
 * locales, generics and casts ({@link #cast(Object)}).
 *
 * <p>SCIPIO: NOTE: This is a misnomer for a general language and utility function namespace class and would have been
 * more accurate as UtilLang. Although split classes for utilities (UtilMap, UtilList, UtilBoolean, etc.) are sometimes
 * clearer, this is a longstanding legacy class conveniently shortly named that is already automatically and manually
 * imported throughout the codebase; not all methods are helpful or appropriate for all contexts, and many come from
 * different development histories, so individual descriptions should be consulted. Currently, some generic language
 * and type methods are also found in {@link UtilNumber}, {@link UtilValidate} and others - some may be moved or
 * duplicated into UtilMisc in the future as facades; meanwhile alternative enhanced type-specific classes already exist
 * (such as ScipioMap and others) and may be used for more advanced purposes and additional helpers.</p>
 *
 * <p>SCIPIO: 3.0.0: Added several new utils such as {@link #constMap(Object...)} and deprecated code in transition;
 *  moving toward super-generic helpers such as {@link #put(Map, Object...)} (eliminates need for tons of overloads).</p>
 * <p>SCIPIO: 2.1.0: Now contains {@link #cast(Object)} as more succinct and UtilGenerics is headed toward deprecation.</p>
 */
public class UtilMisc {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilMisc INSTANCE = new UtilMisc(); // SCIPIO: This is for FreeMarkerWorker (only!)

    @Deprecated
    public static final BigDecimal ZERO_BD = BigDecimal.ZERO;

    /**
     * Casts object to given type.
     *
     * <p>SCIPIO: 3.0.0: Added as more succinct than {@link UtilGenerics#cast}.</p>
     */
    @SuppressWarnings("unchecked")
    public static <V> V cast(Object object) {
        return (V) object;
    }

    public static <T extends Throwable> T initCause(T throwable, Throwable cause) {
        throwable.initCause(cause);
        return throwable;
    }

    public static <T> int compare(Comparable<T> obj1, T obj2) {
        if (obj1 == null) {
            if (obj2 == null) {
                return 0;
            }
            return 1;
        }
        return obj1.compareTo(obj2);
    }

    public static <E> int compare(List<E> obj1, List<E> obj2) {
        if (obj1 == obj2) {
            return 0;
        }
        try {
            if (obj1.size() == obj2.size() && obj1.containsAll(obj2) && obj2.containsAll(obj1)) {
                return 0;
            }
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            Debug.log(e, module);
        }
        return 1;
    }

    /**
     * Get an iterator from a collection, returning null if collection is null
     * @param col The collection to be turned in to an iterator
     * @return The resulting Iterator
     */
    public static <T> Iterator<T> toIterator(Collection<T> col) {
        if (col == null) {
            return null;
        }
        return col.iterator();
    }

    /**
     * Gets (casts) a map from the given value.
     *
     * <p>NOTE: This is for consistency but asList, asSet have serious uses.</p>
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> asMap(Object map) {
        if (map instanceof Map) {
            return  (Map<K, V>) map;
        } else if (map == null) {
            return null;
        } else {
            throw new IllegalArgumentException("asList: Not a map-compatible type: " + map.getClass().getName());
        }
    }

    /**
     * Create a map from passed nameX, valueX parameters
     *
     * <p>SCIPIO: TODO: This should ideally return insert-order-preserving maps (LinkedHashMap), which was the FastMap behavior.</p>
     *
     * @return The resulting Map
     */
    public static <K, V> Map<K, V> toMap(Object... keyValuePairs) {
        if (keyValuePairs.length == 1 && keyValuePairs[0] instanceof Map) {
            return cast(keyValuePairs[0]);
        }
        return put(new HashMap<>(), keyValuePairs);
    }

    /**
     * Create a map from passed nameX, valueX parameters, as ordered map (currently LinkedHashMap).
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> orderedMap(Object... keyValuePairs) {
        if (keyValuePairs.length == 1 && keyValuePairs[0] instanceof Map) {
            return cast(keyValuePairs[0]);
        }
        return put(new LinkedHashMap<>(), keyValuePairs);
    }

    /**
     * Create a map from passed nameX, valueX parameters, as ordered map (currently LinkedHashMap).
     * @deprecated SCIPIO: 3.0.0: Use {@link #orderedMap(Object...)}.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> toOrderedMap(Object... keyValuePairs) {
        return orderedMap(keyValuePairs);
    }

    /**
     * Create a map from passed nameX, valueX parameters, as fast random order map (currently HashMap).
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> randomMap(Object... keyValuePairs) {
        if (keyValuePairs.length == 1 && keyValuePairs[0] instanceof Map) {
            return cast(keyValuePairs[0]);
        }
        return put(new HashMap<>(), keyValuePairs);
    }

    /**
     * Create a map from passed nameX, valueX parameters, as umodifiable/read-only ordered map (currently LinkedHashMap).
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> constMap(Object... keyValuePairs) {
        return Collections.unmodifiableMap(orderedMap(keyValuePairs));
        //return Map.of(keyValuePairs); // Unordered
    }

    /**
     * Create a map from passed nameX, valueX parameters, as umodifiable/read-only ordered map (currently LinkedHashMap).
     *
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> constMapCopy(Map<? extends K, ? extends V> map) {
        return Collections.unmodifiableMap(new LinkedHashMap<>(map));
        //return Map.copyOf(map); // Unordered
    }

    /**
     * Puts the given key-value pairs into the map and returns the map, for chaining.
     *
     * <p>SCIPIO: 3.0.0: Made this method prominent because it's extremely short, convenient and works with all map types.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     *
     * @return The same map (for chaining)
     */
    public static <M extends Map<K, V>, K, V> M put(M map, Object... keyValuePairs) {
        if ((keyValuePairs.length % 2) != 0) {
            throw new IllegalArgumentException("Uneven number of key-value pair arguments");
        }
        for (int i = 0; i < keyValuePairs.length; i += 2) {
            @SuppressWarnings("unchecked")
            K key = (K) keyValuePairs[i];
            @SuppressWarnings("unchecked")
            V value = (V) keyValuePairs[i + 1];
            map.put(key, value);
        }
        return map;
    }

    /**
     * Create a map from passed nameX, valueX parameters, into provided map (currently LinkedHashMap).
     *
     * <p>TODO: Deprecate in favor of superior shorthand {@link #put(Map, Object...)} as this hasn't been in use for long.</p>
     *
     * <p>SCIPIO: 2.1.0: Added.</p>
     *
     * @return The same map (for chaining)
     * @see #put(Map, Object...)
     */
    @SuppressWarnings("unchecked")
    public static <K, V, M extends Map<K, V>> M putAll(M map, Object... keyValuePairs) {
        return put(map, keyValuePairs);
    }

    /**
     * SCIPIO: For an inMap with generics Map&lt;K, V&gt;, populates and returns the opposite mapping outMap, Map&lt;V, K&gt;
     * Added 2017-07-12.
     */
    public static <K, V> Map<V, K> putAllReverseMapping(Map<V, K> map, Map<? extends K, ? extends V> inMap) {
        for (Map.Entry<? extends K, ? extends V> entry : inMap.entrySet()) {
            map.put(entry.getValue(), entry.getKey());
        }
        return map;
    }

    /**
     * Transfers specified keys from in to out map.
     *
     * <p>SCIPIO: 2017-12-04: Added.</p>
     */
    public static <M extends Map<K, V>, K, V> M putKeys(M map, Map<? extends K, ? extends V> inMap, Collection<? extends K> keys) {
        for (K key : keys) {
            map.put(key, inMap.get(key));
        }
        return map;
    }

    /**
     * Transfers all or part of the specified keys from in to out (this) map, with null-treatment options.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param preserveNull If null, transfer all keys even if missing from source; if false, transfer only non-null values from source;
     *                     if true, transfer only non-null and keys present in source ({@link Map#containsKey}
     */
    public static <K, V, M extends Map<K, V>> M putKeys(M map, Map<? extends K, ? extends V> inMap, Collection<? extends K> keys, Boolean preserveNull) {
        if (keys == null) {
            keys = inMap.keySet();
        }
        if (preserveNull == null) {
            for (K key : keys) {
                map.put(key, inMap.get(key));
            }
        } else if (preserveNull) {
            for (K key : keys) {
                V value = inMap.get(key);
                if (value != null || inMap.containsKey(key)) { // NOTE: Optimized to leave containsKey() last because most values are non-null
                    map.put(key, value);
                }
            }
        } else {
            for (K key : keys) {
                V value = inMap.get(key);
                if (value != null) {
                    map.put(key, value);
                }
            }
        }
        return map;
    }

    /**
     * Transfers specified keys from in to out map.
     *
     * <p>SCIPIO: 3.0.0: Changed unused overload signature to fix compilation ambiguity.</p>
     *
     * @param preserveNull If null, transfer all keys even if missing from source; if false, transfer only non-null values from source;
     *                     if true, transfer only non-null and keys present in source ({@link Map#containsKey}
     */
    @SafeVarargs
    public static <M extends Map<K, V>, K, V> M putKeys(M map, Map<? extends K, ? extends V> inMap, Boolean preserveNull, K... keys) {
        return putKeys(map, inMap, Arrays.asList(keys), preserveNull);
    }

    public static <K, V> String printMap(Map<? extends K, ? extends V> theMap) {
        StringBuilder theBuf = new StringBuilder();
        for (Map.Entry<? extends K, ? extends V> entry: theMap.entrySet()) {
            theBuf.append(entry.getKey());
            theBuf.append(" --> ");
            theBuf.append(entry.getValue());
            theBuf.append(System.getProperty("line.separator"));
        }
        return theBuf.toString();
    }

    public static <T> List<T> makeListWritable(Collection<? extends T> col) {
        return (col != null) ? new ArrayList<>(col) : new ArrayList<>();
    }

    public static <K, V> Map<K, V> makeMapWritable(Map<K, ? extends V> map) {
        return (map != null) ? new HashMap<>(map) : new HashMap<>();
    }

    public static <T> Set<T> makeSetWritable(Collection<? extends T> col) {
        return (col != null) ? new LinkedHashSet<>(col) : new LinkedHashSet<>();
    }

    /**
     * This change a Map to be Serializable by removing all entries with values that are not Serializable.
     *
     * @param <V>
     * @param map
     */
    public static <V> void makeMapSerializable(Map<String, V> map) {
        // now filter out all non-serializable values
        Set<String> keysToRemove = new LinkedHashSet<>();
        for (Map.Entry<String, V> mapEntry: map.entrySet()) {
            Object entryValue = mapEntry.getValue();
            if (entryValue != null && !(entryValue instanceof Serializable)) {
                keysToRemove.add(mapEntry.getKey());
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Found Map value that is not Serializable: " + mapEntry.getKey() + "=" + mapEntry.getValue(), module);
                }

            }
        }
        for (String keyToRemove: keysToRemove) { map.remove(keyToRemove); }
    }

    /**
     * Sort a List of Maps by specified consistent keys.
     * @param listOfMaps List of Map objects to sort.
     * @param sortKeys List of Map keys to sort by.
     * @return a new List of sorted Maps.
     */
    public static List<Map<Object, Object>> sortMaps(List<Map<Object, Object>> listOfMaps, List<? extends String> sortKeys) {
        if (listOfMaps == null || sortKeys == null) {
            return null;
        }
        List<Map<Object, Object>> toSort = new ArrayList<>(listOfMaps.size());
        toSort.addAll(listOfMaps);
        try {
            MapComparator mc = new MapComparator(sortKeys);
            toSort.sort(mc);
        } catch (Exception e) {
            Debug.logError(e, "Problems sorting list of maps; returning null.", module);
            return null;
        }
        return toSort;
    }

    /**
     * Assuming outerMap not null; if null will throw a NullPointerException
     */
    public static <K, IK, V> Map<IK, V> getMapFromMap(Map<K, Object> outerMap, K key) {
        Map<IK, V> innerMap = UtilGenerics.<IK, V>checkMap(outerMap.get(key));
        if (innerMap == null) {
            innerMap = new HashMap<>();
            outerMap.put(key, innerMap);
        }
        return innerMap;
    }

    /**
     * Assuming outerMap not null; if null will throw a NullPointerException
     */
    public static <K, V> List<V> getListFromMap(Map<K, Object> outerMap, K key) {
        List<V> innerList = UtilGenerics.<V>checkList(outerMap.get(key));
        if (innerList == null) {
            innerList = new ArrayList<>(); // SCIPIO: switched to ArrayList
            outerMap.put(key, innerList);
        }
        return innerList;
    }

    /**
     * Assuming theMap not null; if null will throw a NullPointerException
     */
    public static <K> BigDecimal addToBigDecimalInMap(Map<K, Object> theMap, K mapKey, BigDecimal addNumber) {
        Object currentNumberObj = theMap.get(mapKey);
        BigDecimal currentNumber = null;
        if (currentNumberObj == null) {
            currentNumber = BigDecimal.ZERO;
        } else if (currentNumberObj instanceof BigDecimal) {
            currentNumber = (BigDecimal) currentNumberObj;
        } else if (currentNumberObj instanceof Double) {
            currentNumber = new BigDecimal((Double) currentNumberObj);
        } else if (currentNumberObj instanceof Long) {
            currentNumber = new BigDecimal((Long) currentNumberObj);
        } else {
            throw new IllegalArgumentException("In addToBigDecimalInMap found a Map value of a type not supported: " + currentNumberObj.getClass().getName());
        }

        if (addNumber == null || BigDecimal.ZERO.compareTo(addNumber) == 0) {
            return currentNumber;
        }
        currentNumber = currentNumber.add(addNumber);
        theMap.put(mapKey, currentNumber);
        return currentNumber;
    }

    public static <T> T removeFirst(List<T> lst) {
        return lst.remove(0);
    }

    public static <T> Set<T> collectionToSet(Collection<T> c) {
        if (c == null) {
            return null;
        }
        Set<T> theSet;
        if (c instanceof Set<?>) {
            theSet = (Set<T>) c;
        } else {
            // SCIPIO: 2.1.0: Fixed erroneous modification of input collection
            theSet = new LinkedHashSet<>();
            //c.remove(null);
            //theSet.addAll(c);
            for(T val : c) {
                if (val != null) {
                    theSet.add(val);
                }
            }
        }
        return theSet;
    }

    /**
     * Generates a String from given values delimited by delimiter.
     *
     * @param values
     * @param delimiter
     * @return String
     */
    public static String collectionToString(Collection<? extends Object> values, String delimiter) {
        if (UtilValidate.isEmpty(values)) {
            return null;
        }
        if (delimiter == null) {
            delimiter = "";
        }
        StringBuilder out = new StringBuilder();

        for (Object val : values) {
            out.append(UtilFormatOut.safeToString(val)).append(delimiter);
        }
        return out.toString();
    }

    /**
     * Gets set or creates an ordered set copy from the passed collection.
     *
     * <p>NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static <T> Set<T> asSet(Object collection) {
        if (collection instanceof Set) {
            return cast(collection);
        } else if (collection instanceof Collection) {
            return new LinkedHashSet<>(UtilMisc.<Collection<T>>cast(collection));
        } else if (collection == null) {
            return null;
            // TODO: arrays
        } else {
            throw new IllegalArgumentException("asSet: Not a collection-compatible type: " + collection.getClass().getName());
        }
    }

    /**
     * Create an ordered set copy from the passed objects.
     *
     * <p>SCIPIO: 3.0.0: NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * @param elems
     * @return theSet
     */
    @SafeVarargs
    public static <T> Set<T> toSet(T... elems) {
        return new LinkedHashSet<>(Arrays.asList(elems));
    }

    /**
     * Gets or creates an ordered set copy from the passed collection.
     *
     * <p>NOTE: This can now always be expected to return an insert-order-preserving set when one is created, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * <p>TODO: Should be deprecated in favor of {@link #asSet} because much clearer and this one conflicts with {@link #toSet(Object...)}.</p>
     *
     * <p>SCIPIO: 3.0.0: Made order-preserving and now always creates a copy for safety and caller modification.</p>
     *
     * @param collection
     * @return theSet
     */
    public static <T> Set<T> toSet(Collection<T> collection) {
        return (collection != null) ? (collection instanceof Set ? (Set<T>) collection : new LinkedHashSet<>(collection)) : null;
    }

    /**
     * Create a fast random set copy from the passed objects.
     *
     * <p>SCIPIO: 3.0.0: Added because {@link #toSet} is no longer random and sometimes this is explicitly desired.
     * You can also use {@link Set#of} and {@link Set#copyOf} for immutable versions.</p>
     *
     * @see Set#of
     */
    @SafeVarargs
    public static <T> Set<T> randomSet(T... elems) {
        return (elems != null) ? new HashSet<>(Arrays.asList(elems)) : null;
    }

    /**
     * Gets from or creates an unmodifiable ordered set copy from the passed elements.
     *
     * <p>NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> constSet(T... elems) {
        return Collections.unmodifiableSet(toSet(elems));
    }

    /**
     * Gets from or creates an unmodifiable ordered set copy from the passed collection.
     *
     * <p>NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static <T> Set<T> constSetCopy(Collection<? extends T> coll) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(coll));
    }

    /**
     * Creates a set from the passed array.
     * <p>SCIPIO: 3.0.0: NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     */
    public static <T> Set<T> toSetArray(T[] data) {
        if (data == null) {
            return null;
        }
        return new LinkedHashSet<>(Arrays.asList(data));
    }

    /**
     * SCIPIO: Create a HashSet from passed objX parameters.
     *
     * <p>TODO: Deprecate in favor of {@link #randomSet}.</p>
     *
     * @return The resulting HashSet
     */
    @SafeVarargs
    public static <T> Set<T> toHashSet(T... obj) {
        return new HashSet<T>(Arrays.asList(obj));
    }

    /**
     * SCIPIO: Create a HashSet from passed objX parameters.
     *
     * <p>TODO: Deprecate in favor of {@link #randomSet}.</p>
     *
     * @return The resulting HashSet
     */
    @Deprecated
    public static <T> Set<T> toHashSet(Collection<? extends T> collection) {
        return new HashSet<T>(collection);
    }

    /**
     * Gets list or creates a list copy from the passed collection.
     *
     * <p>NOTE: This can now always be expected to return an insert-order-preserving set, because this
     * generally increases the predictibility and reliability of various operations (security, stability, queries,
     * logging, etc.).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param collection
     * @return The list
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> asList(Object collection) {
        if (collection instanceof List) {
            return (List<T>) collection;
        } else if (collection instanceof Collection) {
            return new ArrayList<>((Collection<T>) collection);
            // TODO: arrays
        } else if (collection == null) {
            return null;
        } else {
            throw new IllegalArgumentException("asList: Not a collection-compatible type: " + collection.getClass().getName());
        }
    }

    /**
     * Creates a list from passed objects.
     * @param data
     * @return list
     */
    @SafeVarargs
    public static <T> List<T> toList(T... data) {
        return new ArrayList<>(Arrays.asList(data));
    }

    /**
     * Gets or creates a list copy from the passed collection.
     *
     * <p>TODO: Should be deprecated in favor of {@link #asList} because much clearer and this one conflicts with {@link #toList(Object...)}.</p>
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> toList(Collection<? extends T> collection) {
        return asList(collection);
    }

    /**
     * Creates an unmodifiable ordered list copy from the passed elements.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param elems
     * @return theSet
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> constList(T... elems) {
        return List.copyOf(Arrays.asList(elems));
    }

    /**
     * Creates an unmodifiable ordered list copy from the passed collection.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param collection
     * @return theSet
     */
    public static <T> List<T> constListCopy(Collection<? extends T> collection) {
        return List.copyOf(collection);
    }

    public static <T> List<T> toListArray(T[] data) {
        return (data != null) ? new ArrayList<T>(Arrays.asList(data)) : null;
    }

    public static <K, V> void addToListInMap(V element, Map<K, ?> theMap, K listKey) { // SCIPIO: Generalized this: Map<K, Object>
        List<V> theList = UtilGenerics.checkList(theMap.get(listKey));
        if (theList == null) {
            theList = new ArrayList<>(); // SCIPIO: switched to ArrayList
            UtilGenerics.<Map<K, List<V>>>cast(theMap).put(listKey, theList); // SCIPIO: cast
        }
        theList.add(element);
    }

    /**
     * SCIPIO: Adds element to the list in the in map having given key; if no set yet, listSupplier provides a new one.
     */
    public static <K, V> void addToListInMap(V element, Map<K, ?> theMap, K listKey, Supplier<List<V>> listSupplier) { // SCIPIO: Generalized this: Map<K, Object>
        List<V> theList = UtilGenerics.checkList(theMap.get(listKey));
        if (theList == null) {
            theList = listSupplier.get();
            UtilGenerics.<Map<K, List<V>>>cast(theMap).put(listKey, theList); // SCIPIO: cast
        }
        theList.add(element);
    }

    public static <K, V> void addToSetInMap(V element, Map<K, Set<V>> theMap, K setKey) {
        Set<V> theSet = UtilGenerics.checkSet(theMap.get(setKey));
        if (theSet == null) {
            theSet = new LinkedHashSet<>();
            theMap.put(setKey, theSet);
        }
        theSet.add(element);
    }

    public static <K, V> void addToSortedSetInMap(V element, Map<K, Set<V>> theMap, K setKey) {
        Set<V> theSet = UtilGenerics.checkSet(theMap.get(setKey));
        if (theSet == null) {
            theSet = new TreeSet<>();
            theMap.put(setKey, theSet);
        }
        theSet.add(element);
    }

    /**
     * SCIPIO: Adds element to the set in the in map having given key; if no set yet, setSupplier provides a new one.
     */
    public static <K, V> void addToSetInMap(V element, Map<K, Set<V>> theMap, K setKey, Supplier<Set<V>> setSupplier) {
        Set<V> theSet = UtilGenerics.checkSet(theMap.get(setKey));
        if (theSet == null) {
            theSet = setSupplier.get();
            theMap.put(setKey, theSet);
        }
        theSet.add(element);
    }

    /** Converts an <code>Object</code> to a <code>double</code>. Returns
     * zero if conversion is not possible.
     * @param obj Object to convert
     * @return double value
     */
    public static double toDouble(Object obj) {
        Double result = toDoubleObject(obj);
        return result == null ? 0.0 : result;
    }

    /** Converts an <code>Object</code> to a <code>Double</code>. Returns
     * defaultValue if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @param defaultValue default value
     * @return Double
     */
    public static Double toDouble(Object obj, Double defaultValue) {
        Double result = toDoubleObject(obj);
        return result == null ? defaultValue : result;
    }

    /** Converts an <code>Object</code> to a <code>Double</code>. Returns
     * <code>null</code> if conversion is not possible.
     * @param obj Object to convert
     * @return Double
     */
    public static Double toDoubleObject(Object obj) {
        if (obj == null) {
            return null;
        }
        if (obj instanceof Double) {
            return (Double) obj;
        }
        if (obj instanceof Number) {
            return ((Number) obj).doubleValue();
        }
        Double result = null;
        try {
            result = Double.parseDouble(obj.toString());
        } catch (Exception e) {}
        return result;
    }

    /** Converts an <code>Object</code> to a <code>float</code>. Returns
     * zero if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @return float value
     */
    public static float toFloat(Object obj) {
        Float result = toFloatObject(obj);
        return result == null ? 0.0f : result;
    }

    /** Converts an <code>Object</code> to a <code>Float</code>. Returns
     * defaultValue if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @param defaultValue default value
     * @return Float
     */
    public static Float toFloat(Object obj, Float defaultValue) {
        Float result = toFloatObject(obj);
        return result == null ? defaultValue : result;
    }

    /** Converts an <code>Object</code> to a <code>Float</code>. Returns
     * <code>null</code> if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @return Float
     */
    public static Float toFloatObject(Object obj) {
        if (obj == null) {
            return null;
        }
        if (obj instanceof Float) {
            return (Float) obj;
        }
        if (obj instanceof Number) {
            return ((Number) obj).floatValue();
        }
        Float result = null;
        try {
            result = Float.parseFloat(obj.toString());
        } catch (Exception e) {}
        return result;
    }

    /** Converts an <code>Object</code> to an <code>int</code>. Returns
     * zero if conversion is not possible.
     * @param obj Object to convert
     * @return int value
     */
    public static int toInteger(Object obj) {
        Integer result = toIntegerObject(obj);
        return result == null ? 0 : result;
    }

    /** Converts an <code>Object</code> to an <code>Integer</code>. Returns
     * default value if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @return Integer
     */
    public static Integer toInteger(Object obj, Integer defaultValue) {
        Integer result = toIntegerObject(obj);
        return result == null ? defaultValue : result;
    }

    /** Converts an <code>Object</code> to an <code>Integer</code>. Returns
     * <code>null</code> if conversion is not possible.
     * @param obj Object to convert
     * @return Integer
     */
    public static Integer toIntegerObject(Object obj) {
        if (obj == null) {
            return null;
        }
        if (obj instanceof Integer) {
            return (Integer) obj;
        }
        if (obj instanceof Number) {
            return ((Number)obj).intValue();
        }
        Integer result = null;
        try {
            result = Integer.parseInt(obj.toString());
        } catch (Exception e) {}
        return result;
    }

    /** Converts an <code>Object</code> to a <code>long</code>. Returns
     * zero if conversion is not possible.
     * @param obj Object to convert
     * @return long value
     */
    public static long toLong(Object obj) {
        Long result = toLongObject(obj);
        return result == null ? 0 : result;
    }

    /** Converts an <code>Object</code> to a <code>Long</code>. Returns
     * default value if conversion is not possible (SCIPIO).
     * @param obj Object to convert
     * @return Long
     */
    public static Long toLong(Object obj, Long defaultValue) {
        Long result = toLongObject(obj);
        return result == null ? defaultValue : result;
    }

    /** Returns true if the number is null or zero.
     * Not reliable for all floating-point values.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static boolean nullOrZero(Number number) { // SCIPIO
        return (number == null || number.longValue() == 0L);
    }

    /** Returns true if the number is null or zero.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static boolean nullOrZero(Integer number) { // SCIPIO
        return (number == null || number == 0);
    }

    /** Returns true if the number is null or zero.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static boolean nullOrZero(Long number) { // SCIPIO
        return (number == null || number == 0);
    }

    /** Converts an <code>Object</code> to a <code>Long</code>. Returns
     * <code>null</code> if conversion is not possible.
     * @param obj Object to convert
     * @return Long
     */
    public static Long toLongObject(Object obj) {
        if (obj == null) {
            return null;
        }
        if (obj instanceof Long) {
            return (Long) obj;
        }
        if (obj instanceof Number) {
            return ((Number) obj).longValue();
        }
        Long result = null;
        try {
            result = Long.parseLong(obj.toString());
        } catch (Exception e) {}
        return result;
    }

    /**
     * Adds value to the key entry in theMap, or creates a new one if not already there
     * @param theMap
     * @param key
     * @param value
     */
    public static <K> void addToDoubleInMap(Map<K, Object> theMap, K key, Double value) {
        Double curValue = (Double) theMap.get(key);
        if (curValue != null) {
            theMap.put(key, curValue + value);
        } else {
            theMap.put(key, value);
        }
    }

    /**
     * Parse a locale string Locale object
     * @param localeString The locale string (en_US)
     * @return Locale The new Locale object or null if no valid locale can be interpreted
     */
    public static Locale parseLocale(String localeString) {
        if (UtilValidate.isEmpty(localeString)) {
            return null;
        }

        Locale locale = null;
        if (localeString.length() == 2) {
            // two letter language code
            locale = new Locale.Builder().setLanguage(localeString).build();
        } else if (localeString.length() == 5) {
            // positions 0-1 language, 3-4 are country
            String language = localeString.substring(0, 2);
            String country = localeString.substring(3, 5);
            locale = new Locale.Builder().setLanguage(language).setRegion(country).build();
        } else if (localeString.length() > 6) {
            // positions 0-1 language, 3-4 are country, 6 and on are special extensions
            String language = localeString.substring(0, 2);
            String country = localeString.substring(3, 5);
            String extension = localeString.substring(6);
            locale = new Locale(language, country, extension);
        } else {
            Debug.logWarning("Do not know what to do with the localeString [" + localeString + "], should be length 2, 5, or greater than 6, returning null", module);
        }

        return locale;
    }

    /**
     * Parse a locale string Locale objects (SCIPIO).
     * @param localeStrings The locale strings (en_US)
     * @return The locales
     */
    public static List<Locale> parseLocales(Collection<String> localeStrings) {
        List<Locale> locales = new ArrayList<>(localeStrings.size());
        for(String localeString : localeStrings) {
            Locale locale = parseLocale(localeString);
            if (locale != null) {
                locales.add(locale);
            }
        }
        return locales;
    }

    /**
     * Parse a locale string Locale objects (SCIPIO).
     * @param localeString The comma-separated locales string (en_US)
     * @return The locales
     */
    public static List<Locale> parseLocales(String localeString) {
        if (UtilValidate.isEmpty(localeString)) {
            return Collections.emptyList();
        }
        return parseLocales(Arrays.asList(localeString.split("\\s*,\\s*")));
    }

    /**
     * Return the object converted to a Locale per framework conversion rules.
     *
     * <p>The input can be a String, Locale, or even null and a valid Locale will always be returned; if nothing else works, returns the default locale.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param localeObject An Object representing the locale
     */
    public static Locale asLocale(Object localeObject) {
        if (localeObject instanceof Locale) {
            return (Locale) localeObject;
        } else if (localeObject instanceof String) {
            Locale locale = parseLocale((String) localeObject);
            if (locale != null)  {
                return locale;
            } else {
                return null;
            }
        } else if (localeObject == null) {
            return null;
        } else {
            throw new IllegalArgumentException("Invalid locale type: " + localeObject.getClass().getName());
        }
    }

    /**
     * Return the object converted to a Locale per framework conversion rules.
     *
     * <p>The input can be a String, Locale, or even null and a valid Locale will always be returned; if nothing else works, returns the default locale.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param localeObject An Object representing the locale
     * @param defaultLocaleObject Object representing the default locale to use
     */
    public static Locale asLocale(Object localeObject, Object defaultLocaleObject) {
        Locale locale = asLocale(localeObject);
        return (locale != null) ? locale : asLocale(defaultLocaleObject);
    }

    /** The input can be a String, Locale, or even null and a valid Locale will always be returned; if nothing else works, returns the default locale.
     * @param localeObject An Object representing the locale
     */
    public static Locale ensureLocale(Object localeObject) {
        try {
            Locale locale = asLocale(localeObject);
            return (locale != null) ? locale : Locale.getDefault();
        } catch (IllegalArgumentException e) {
            Debug.logWarning("ensureLocale: " + e.toString(), module);
            return null;
        }
    }

    /**
     * Returns matching locale candidate to available locales filter or null if no
     * candidate matching criteria; if availableLocales is empty itself, the (first) locale from localeObject is returned.
     *
     * <p>availableAllString may be set to true as optimization.</p>
     *
     * <p>SCIPIO: 3.0.0: Added to filter UserLogin.lastLocale and client request accept headers.</p>
     */
    public static Locale getCandidateLocale(Object localeObject, List<?> availableLocales, Boolean availableAllString, Boolean exact) {
        Collection<?> locales = null;
        if (localeObject instanceof Collection) {
            locales = (Collection<?>) localeObject;
            if (locales.isEmpty()) {
                return null;
            }
        }
        Locale locale = asLocale(locales != null ? firstSafe(locales) : localeObject);
        if (locale == null) {
            return null;
        }
        if (locales != null && locales.size() == 1) {
            locales = null;
        }
        if (UtilValidate.isEmpty(availableLocales)) {
            return locale;
        }

        if (!Boolean.TRUE.equals(availableAllString)) {
            availableLocales = availableLocales.stream().map(Object::toString).collect(Collectors.toList());
        }

        // Here check either a single locale with or without its candidates, or the full passed list without candidates (as requested)
        if (locales == null) {
            if (exact) {
                if (availableLocales.contains(locale.toString())) {
                    return locale;
                }
            } else {
                for (Locale candidateLocale : UtilProperties.localeToCandidateList(locale)) {
                    if (availableLocales.contains(candidateLocale.toString())) {
                        return candidateLocale;
                    }
                }
            }
        } else {
            for (Object currentLocaleObject : locales) {
                if (currentLocaleObject instanceof String) {
                    if (availableLocales.contains(currentLocaleObject)) {
                        return asLocale(currentLocaleObject);
                    }
                } else {
                    Locale candidateLocale = asLocale(currentLocaleObject);
                    if (availableLocales.contains(candidateLocale.toString())) {
                        return candidateLocale;
                    }
                }
            }
        }

        // If the client passed a list as localeObject, do a second pass with candidate locales of each (excluding the first of each, already checked)
        if (!exact && locales != null) {
            for (Object candidateLocaleObject : locales) {
                Locale candidateLocale = asLocale(candidateLocaleObject);
                List<Locale> secondCandidateLocales = UtilProperties.localeToCandidateList(candidateLocale);
                for (Locale secondCandidateLocale : secondCandidateLocales.subList(1, secondCandidateLocales.size())) {
                    if (availableLocales.contains(candidateLocale.toString())) {
                        return secondCandidateLocale;
                    }
                }
            }
        }

        return null;
    }

    // Private lazy-initializer class
    private static class LocaleHolder {
        private static final List<Locale> availableLocaleList = getAvailableLocaleList();

        /** SCIPIO: SPECIAL: Available locales automatically expanded to include countries (country required) (added 2017-10-11) */
        private static final List<Locale> availableLocaleExpandedCountryRequiredList = getAvailableLocaleExpandedCountryRequiredList();

        /** SCIPIO: SPECIAL: Available locales automatically expanded to include countries, but will also show locales not having countries (added 2017-10-11) */
        private static final List<Locale> availableLocaleExpandedCountryOptionalList = getAvailableLocaleExpandedCountryOptionalList();

        private static List<Locale> getAvailableLocaleList() {
            TreeMap<String, Locale> localeMap = new TreeMap<>();
            String localesString = UtilProperties.getPropertyValue("general", "locales.available");
            if (UtilValidate.isNotEmpty(localesString)) {
                List<String> idList = StringUtil.split(localesString, ",");
                for (String id : idList) {
                    Locale curLocale = parseLocale(id);
                    localeMap.put(curLocale.getDisplayName(), curLocale);
                }
            } else {
                Locale[] locales = Locale.getAvailableLocales();
                for (int i = 0; i < locales.length && locales[i] != null; i++) {
                    String displayName = locales[i].getDisplayName();
                    if (!displayName.isEmpty()) {
                        localeMap.put(displayName, locales[i]);
                    }
                }
            }
            return Collections.unmodifiableList(new ArrayList<>(localeMap.values()));
        }

        /** SCIPIO: SPECIAL: Returns a List of available locales sorted by display name expanded to include country codes (added 2017-10-11) */
        private static List<Locale> getAvailableLocaleExpandedCountryRequiredList() {
            List<Locale> list = getAvailableLocaleExpandedCountryOptionalList();
            ArrayList<Locale> filtered = new ArrayList<>();
            for(Locale locale : list) {
                if (UtilValidate.isNotEmpty(locale.getCountry())) {
                    filtered.add(locale);
                }
            }
            filtered.trimToSize();
            return Collections.unmodifiableList(filtered);
        }

        /** SCIPIO: SPECIAL: Returns a List of available locales sorted by display name expanded to include country codes and also without country codes (added 2017-10-11) */
        private static List<Locale> getAvailableLocaleExpandedCountryOptionalList() {
            TreeMap<String, Locale> localeMap = new TreeMap<>();
            String localesString = UtilProperties.getPropertyValue("general", "locales.available");
            if (UtilValidate.isNotEmpty(localesString)) {
                List<String> idList = StringUtil.split(localesString, ",");
                Set<String> genericLangs = new HashSet<>();
                for (String id : idList) {
                    Locale curLocale = parseLocale(id);
                    localeMap.put(curLocale.getDisplayName(), curLocale);
                    //if (UtilValidate.isEmpty(curLocale.getCountry())) { // TODO: REVIEW: don't restrict the countries with this list for now...
                    genericLangs.add(curLocale.getLanguage());
                    //}
                }
                Locale[] locales = Locale.getAvailableLocales();
                for (int i = 0; i < locales.length && locales[i] != null; i++) {
                    if (genericLangs.contains(locales[i].getLanguage())) {
                        String displayName = locales[i].getDisplayName();
                        if (!displayName.isEmpty()) {
                            localeMap.put(displayName, locales[i]);
                        }
                    }
                }
            } else {
                Locale[] locales = Locale.getAvailableLocales();
                for (int i = 0; i < locales.length && locales[i] != null; i++) {
                    String displayName = locales[i].getDisplayName();
                    if (!displayName.isEmpty()) {
                        localeMap.put(displayName, locales[i]);
                    }
                }
            }
            return Collections.unmodifiableList(new ArrayList<>(localeMap.values()));
        }
    }

    /** Returns a List of available locales sorted by display name */
    public static List<Locale> availableLocales() {
        return LocaleHolder.availableLocaleList;
    }

    /** SCIPIO: SPECIAL: Returns a List of available locales sorted by display name expanded to include country codes (added 2017-10-11)
     * NOTE: This list may be subject to restrictions by user configuration (now or in the future) - do not rely on this to get a list of all existing countries. */
    public static List<Locale> availableLocalesExpandedCountryRequired() {
        return LocaleHolder.availableLocaleExpandedCountryRequiredList;
    }

    /** SCIPIO: SPECIAL: Returns a List of available locales sorted by display name expanded to include country codes and also without country codes (added 2017-10-11).
     * NOTE: This list may be subject to restrictions by user configuration (now or in the future) - do not rely on this to get a list of all existing countries. */
    public static List<Locale> availableLocalesExpandedCountryOptional() {
        return LocaleHolder.availableLocaleExpandedCountryOptionalList;
    }

    /** @deprecated use Thread.sleep() */
    @Deprecated
    public static void staticWait(long timeout) throws InterruptedException {
        Thread.sleep(timeout);
    }

    public static void copyFile(File sourceLocation , File targetLocation) throws IOException {
        if (sourceLocation.isDirectory()) {
            throw new IOException("File is a directory, not a file, cannot copy") ;
        }
        try (
                InputStream in = new FileInputStream(sourceLocation);
                OutputStream out = new FileOutputStream(targetLocation);
        ) {
            // Copy the bits from instream to outstream
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
        }
    }

    public static int getViewLastIndex(int listSize, int viewSize) {
        return (int)Math.ceil(listSize / (float) viewSize) - 1;
    }

    // SCIPIO: 2018-08-30: these methods cannot exist here because Delegator
    // is not available from base component; they are moved to:
    // org.ofbiz.common.address.AddressUtil
//    public static Map<String, String> splitPhoneNumber(String phoneNumber, Delegator delegator) {
//        Map<String, String> result = new HashMap<>();
//        try {
//            PhoneNumberUtil phoneUtil = PhoneNumberUtil.getInstance();
//            String defaultCountry = EntityUtilProperties.getPropertyValue("general", "country.geo.id.default", delegator);
//            GenericValue defaultGeo = EntityQuery.use(delegator).from("Geo").where("geoId", defaultCountry).cache().queryOne();
//            String defaultGeoCode = defaultGeo != null ? defaultGeo.getString("geoCode") : "US";
//            PhoneNumber phNumber = phoneUtil.parse(phoneNumber, defaultGeoCode);
//            if (phoneUtil.isValidNumber(phNumber) || phoneUtil.isPossibleNumber(phNumber)) {
//                String nationalSignificantNumber = phoneUtil.getNationalSignificantNumber(phNumber);
//                int areaCodeLength = phoneUtil.getLengthOfGeographicalAreaCode(phNumber);
//                result.put("countryCode", Integer.toString(phNumber.getCountryCode()));
//                if (areaCodeLength > 0) {
//                    result.put("areaCode", nationalSignificantNumber.substring(0, areaCodeLength));
//                    result.put("contactNumber", nationalSignificantNumber.substring(areaCodeLength));
//                } else {
//                    result.put("areaCode", "");
//                    result.put("contactNumber", nationalSignificantNumber);
//                }
//            } else {
//                Debug.logError("Invalid phone number " + phoneNumber, module);
//                result.put(ModelService.ERROR_MESSAGE, "Invalid phone number");
//            }
//        } catch (GenericEntityException | NumberParseException ex) {
//            Debug.logError(ex, module);
//            result.put(ModelService.ERROR_MESSAGE, ex.getMessage());
//        }
//        return result;
//    }

    /**
     * SCIPIO: Gets map entries matching the given prefix.
     */
    public static Map<String, Object> getPrefixedMapEntries(Map<String, Object> map, String prefix) {
        Map<String, Object> res = new HashMap<>();
        for(Map.Entry<String, Object> entry : map.entrySet()) {
            String name = entry.getKey();
            if (name != null && name.startsWith(prefix)) {
                res.put(name.substring(prefix.length()), entry.getValue());
            }
        }
        return res;
    }

    /**
     * SCIPIO: Creates a new, empty map (abstraction).
     * <p>
     * This returns a general-purpose map type, such as HashMap, and does not
     * guarantee insertion order.
     * <p>
     * This and the other methods below are useful for Freemarker workarounds and to guarantee a map
     * is of the same type as the other toMap calls in this class.
     */
    public static <K, V> Map<K, V> newMap() {
        return new HashMap<>();
    }

    /**
     * SCIPIO: Creates a new map initialized from the given map (abstraction).
     * <p>
     * This returns a general-purpose map type, such as HashMap, and does not
     * guarantee insertion order.
     * @see #newMap()
     */
    public static <K, V> Map<K, V> newMap(Map<? extends K, ? extends V> map) {
        return new HashMap<>(map);
    }

    /**
     * SCIPIO: Creates a new map with given initial capacity hint (abstraction).
     * <p>
     * This returns a general-purpose map type, such as HashMap, and does not
     * guarantee insertion order. The initial capacity hint may or may not
     * be honored.
     * @see #newMap()
     */
    public static <K, V> Map<K, V> newMap(int initialCapacity) {
        return new HashMap<>(initialCapacity);
    }

    /**
     * SCIPIO: Creates a new, insert-order-preserving empty map (abstraction).
     * <p>
     * This returns a general-purpose insert-order-preserving map type, such as LinkedHashMap.
     * <p>
     * This is useful for Freemarker workarounds and to guarantee a map
     * is of the same type as the other toMap calls in this class.
     */
    public static <K, V> Map<K, V> newInsertOrderMap() {
        return new LinkedHashMap<>();
    }

    /**
     * SCIPIO: Creates a new, insert-order-preserving empty map initialized from the
     * given collection (abstraction).
     * <p>
     * This returns a general-purpose insert-order-preserving map type, such as LinkedHashMap.
     * @see #newInsertOrderMap()
     */
    public static <K, V> Map<K, V> newInsertOrderMap(Map<? extends K, ? extends V> map) {
        return new LinkedHashMap<>(map);
    }

    /**
     * SCIPIO: Creates a new, empty list (abstraction).
     * <p>
     * This returns a general-purpose list type with no specific initial capacity or structure,
     * appropriate for general use in most Scipio code and services - it may be ArrayList,
     * LinkedList, or even another.
     * <p>
     * NOTE: Often it is better to choose a specific List type such as ArrayList or LinkedList
     * for a given situation; this method is for code which has not been performance written or analyzed.
     * In particular, it may be used to replace instances of javolution FastList usage in old
     * code. This type is likely - but not guaranteed - to remain ArrayList.
     * <p>
     * This is useful for Freemarker workarounds and to guarantee a list
     * is of a type common used by Scipio code.
     */
    public static <V> List<V> newList() {
        return new ArrayList<>(); // new LinkedList<V>()
    }

    /**
     * SCIPIO: Creates a new list initialized from the given collection (abstraction).
     * <p>
     * This returns a general-purpose list type with no specific structure,
     * with contents duplicated from the passed collection,
     * appropriate for general use in most Scipio code and services - it may be ArrayList,
     * LinkedList, or even another.
     * <p>
     * NOTE: Usually it is better to choose a specific List type such as ArrayList or LinkedList
     * for a given situation; this method is for code which has not been performance analyzed.
     * In particular, it may be used to replace instances of javolution FastList usage in old
     * code. This type is likely - but not guaranteed - to remain ArrayList.
     * @see #newList()
     */
    public static <V> List<V> newList(Collection<? extends V> c) {
        return new ArrayList<>(c); // new LinkedList<V>(c)
    }

    /**
     * SCIPIO: Creates a new, empty list, with given initial capacity hint (abstraction).
     * <p>
     * This returns a general-purpose list type with no specific structure,
     * and which may or may not honor the passed initialCapacity,
     * appropriate for general use in most Scipio code and services - it may be ArrayList,
     * LinkedList, or even another.
     * <p>
     * NOTE: Usually it is better to choose a specific List type such as ArrayList or LinkedList
     * for a given situation; this method is for code which has not been performance analyzed.
     * In particular, it may be used to replace instances of javolution FastList usage in old
     * code. This type is likely - but not guaranteed - to remain ArrayList.
     * @see #newList()
     */
    public static <V> List<V> newList(int initialCapacity) {
        return new ArrayList<>(initialCapacity); // new LinkedList<V>()
    }

    /**
     * SCIPIO: Creates a new, empty set (abstraction).
     * <p>
     * This returns a general-purpose set type with no specific initial capacity, structure,
     * and not guaranteed to preserve order,
     * appropriate for general use in most Scipio code and services.
     * <p>
     * This is useful for Freemarker workarounds and to guarantee a list
     * is of a type common used by Scipio code.
     */
    public static <V> Set<V> newSet() {
        return new HashSet<>();
    }

    /**
     * SCIPIO: Creates a new set initialized from the given collection (abstraction).
     * <p>
     * This returns a general-purpose set type with no specific initial capacity, structure,
     * and not guaranteed to preserve order,
     * appropriate for general use in most Scipio code and services.
     * @see #newSet()
     */
    public static <V> Set<V> newSet(Collection<? extends V> c) {
        return new HashSet<>(c);
    }

    /**
     * SCIPIO: Creates a new, empty insert-order-preserving set (abstraction).
     * <p>
     * This returns a general-purpose insert-order-preserving set type with no specific
     * initial capacity or structure,
     * appropriate for general use in most Scipio code and services.
     * <p>
     * This is useful for Freemarker workarounds and to guarantee a list
     * is of a type common used by Scipio code.
     */
    public static <V> Set<V> newInsertOrderSet() {
        return new LinkedHashSet<>();
    }

    /**
     * SCIPIO: Creates a new insert-order-preserving set initialized from the given collection (abstraction).
     * <p>
     * This returns a general-purpose insert-order-preserving set type with no specific
     * initial capacity or structure, appropriate for general use in most Scipio code and services.
     * @see #newInsertOrderSet()
     */
    public static <V> Set<V> newInsertOrderSet(Collection<? extends V> c) {
        return new LinkedHashSet<>(c);
    }

    /**
     * SCIPIO: Returns a new list of entries, where each entry corresponds to the value
     * for the given key for each map in the collection.
     */
    public static <K, V extends R, R> List<R> collectMapValuesForKey(Collection<Map<K, V>> collection, K key) {
        List<R> res = new ArrayList<>(collection.size());
        for(Map<K, V> map : collection) {
            res.add(map.get(key));
        }
        return res;
    }

    /**
     * Returns the first non-null value, or null if none.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    @SafeVarargs
    public static <T> T firstNonNull(T... values) {
        return UtilObject.firstNonNull(values);
    }

    /**
     * Returns the first non-null value, or null.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    public static <T> T firstNonNull(Collection<?> values) {
        return UtilObject.firstNonNull(values);
    }

    /**
     * Returns the first element of collection or list using get() if possible.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    public static <T> T first(Collection<?> values) {
        return (values instanceof List) ? UtilGenerics.<List<T>>cast(values).get(0) : UtilGenerics.cast(values.iterator().next());
    }

    /**
     * Returns the first element of collection or list using get() if possible, or null if empty.
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    public static <T> T firstSafe(Collection<?> values) {
        return UtilValidate.isNotEmpty(values) ? first(values) : null;
    }

    /**
     * Returns the first value in a collection, array, or the value itself, throwing
     * an exception if collections are empty.
     *
     * <p>NOTE: This method does not handle maps (too poorly defined - entry vs value).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @see #firstSafe(Object)
     * @see #firstValue(Object)
     */
    public static <T> T first(Object values) throws IllegalArgumentException {
        if (values instanceof Collection) {
            return UtilMisc.first(UtilGenerics.<Collection<String>>cast(values));
        } else if (values != null && values.getClass().isArray()) {
            return firstInArray(values);
        } else {
            return UtilGenerics.cast(values);
        }
    }

    /**
     * Returns the first value in a collection, array, or the value itself, or null if empty collection.
     *
     * <p>NOTE: This method does not handle maps (too poorly defined - entry vs value).</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @see #firstSafe(Object)
     * @see #firstValueSafe(Object)
     */
    public static <T> T firstSafe(Object values) throws IllegalArgumentException {
        if (values instanceof Collection) {
            return UtilMisc.firstSafe(UtilGenerics.<Collection<String>>cast(values));
        } else if (values != null && values.getClass().isArray()) {
            return firstInArraySafe(values);
        } else {
            return UtilGenerics.cast(values);
        }
    }

    /**
     * Returns the first value (as opposed to entry) of non-empty map.
     *
     * <p>NOTE: If map is unordered or greater than size 1, result may be arbitrary.</p>
     *
     * <p>SCIPIO: 3.0.0: Added to replace ambiguous first(Map).</p>
     */
    public static <T> T firstValue(Map<?, ?> values) {
        return UtilGenerics.cast(values.entrySet().iterator().next().getValue());
    }

    /**
     * Returns the first value of map (as opposed to entry), or null if empty map.
     *
     * <p>NOTE: If map is unordered or greater than size 1, result may be arbitrary.</p>
     *
     * <p>SCIPIO: 3.0.0: Added to replace ambiguous firstSafe(Map).</p>
     */
    public static <T> T firstValueSafe(Map<?, ?> values) {
        return UtilValidate.isNotEmpty(values) ? firstValue(values) : null;
    }

    /**
     * Returns the first value of map (as opposed to entry) or collection, or the value itself.
     *
     * <p>NOTE: If map is unordered or greater than size 1, result may be arbitrary.</p>
     *
     * <p>SCIPIO: 3.0.0: Added to replace ambiguous firstOrSelfSafe(Object).</p>
     */
    public static <T> T firstValue(Object value) {
        if (value instanceof Map) {
            return UtilMisc.firstValue(UtilGenerics.<Map<String, ?>>cast(value));
        } else {
            return UtilMisc.first(value);
        }
    }

    /**
     * Returns the first value (as opposed to entry) of map, or null if empty collection or map.
     *
     * <p>NOTE: If map is unordered or greater than size 1, result may be arbitrary.</p>
     *
     * <p>SCIPIO: 3.0.0: Added to replace ambiguous firstOrSelfSafe(Object).</p>
     */
    public static <T> T firstValueSafe(Object value) {
        if (value instanceof Map) {
            return UtilMisc.firstValueSafe(UtilGenerics.<Map<String, ?>>cast(value));
        } else {
            return UtilMisc.firstSafe(value);
        }
    }

    private static <T> T firstInArray(Object values) {
        Object value;
        if (values instanceof Object[]) {
            value = ((Object[]) values)[0];
        } else if (values instanceof boolean[]) {
            value = ((boolean[]) values)[0];
        } else if (values instanceof byte[]) {
            value = ((byte[]) values)[0];
        } else if (values instanceof short[]) {
            value = ((short[]) values)[0];
        } else if (values instanceof char[]) {
            value = ((char[]) values)[0];
        } else if (values instanceof int[]) {
            value = ((int[]) values)[0];
        } else if (values instanceof long[]) {
            value = ((long[]) values)[0];
        } else if (values instanceof float[]) {
            value = ((float[]) values)[0];
        } else if (values instanceof double[]) {
            value = ((double[]) values)[0];
        } else {
            throw new IllegalArgumentException("Unknown array type: " + values.getClass().getName());
        }
        return UtilGenerics.cast(value);
    }

    private static <T> T firstInArraySafe(Object values) {
        Object value;
        if (values instanceof Object[]) {
            value = (((Object[]) values).length > 0) ? ((Object[]) values)[0] : null;
        } else if (values instanceof boolean[]) {
            value = (((boolean[]) values).length > 0) ? ((boolean[]) values)[0] : null;
        } else if (values instanceof byte[]) {
            value = (((byte[]) values).length > 0) ? ((byte[]) values)[0] : null;
        } else if (values instanceof short[]) {
            value = (((short[]) values).length > 0) ? ((short[]) values)[0] : null;
        } else if (values instanceof char[]) {
            value = (((char[]) values).length > 0) ? ((char[]) values)[0] : null;
        } else if (values instanceof int[]) {
            value = (((int[]) values).length > 0) ? ((int[]) values)[0] : null;
        } else if (values instanceof long[]) {
            value = (((long[]) values).length > 0) ? ((long[]) values)[0] : null;
        } else if (values instanceof float[]) {
            value = (((float[]) values).length > 0) ? ((float[]) values)[0] : null;
        } else if (values instanceof double[]) {
            value = (((double[]) values).length > 0) ? ((double[]) values)[0] : null;
        } else {
            throw new IllegalArgumentException("Unknown array type: " + values.getClass().getName());
        }
        return UtilGenerics.cast(value);
    }

    /**
     * SCIPIO: Returns the first element of non-empty map.
     * @deprecated SCIPIO: 3.0.0: Use {@link #firstValueSafe(Map)}, too ambiguous - TODO: REMOVE
     */
    @Deprecated
    public static <T> T first(Map<?, ?> values) {
        return UtilGenerics.cast(values.entrySet().iterator().next().getValue());
    }

    /**
     * SCIPIO: Returns the first element of map, with empty map check.
     * @deprecated SCIPIO: 3.0.0: Use {@link #firstValueSafe(Map)}, too ambiguous - TODO: REMOVE
     */
    @Deprecated
    public static <T> T firstSafe(Map<?, ?> values) {
        return UtilValidate.isNotEmpty(values) ? first(values) : null;
    }

    /**
     * SCIPIO: Returns the first element of map or collection, or other the value itself.
     * @deprecated SCIPIO: 3.0.0: Use {@link #firstValueSafe(Object)} - TODO: REMOVE
     */
    @Deprecated
    public static <T> T firstOrSelfSafe(Object values) {
        return firstValueSafe(values);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE or "true", or Boolean.FALSE
     * if value is Boolean.FALSE or "false", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValue(Object value) {
        return UtilValidate.booleanValue(value);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is "true", or Boolean.FALSE
     * if value is "false", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValue(String value) {
        return UtilValidate.booleanValue(value);
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE or "true", or false
     * if value is Boolean.FALSE or "false", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValue(Object value, boolean defaultValue) {
        return UtilValidate.booleanValue(value, defaultValue);
    }

    /**
     * SCIPIO: Returns true if value is "true", or false
     * if value is "false", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValue(String value, boolean defaultValue) {
        return UtilValidate.booleanValue(value, defaultValue);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE or "Y", or Boolean.FALSE
     * if value is Boolean.FALSE or "N", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValueIndicator(Object value) {
        return UtilValidate.booleanValueIndicator(value);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is "Y", or Boolean.FALSE
     * if value is "N", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValueIndicator(String value) {
        return UtilValidate.booleanValueIndicator(value);
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE or "Y", or false
     * if value is Boolean.FALSE or "N", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValueIndicator(Object value, boolean defaultValue) {
        return UtilValidate.booleanValueIndicator(value, defaultValue);
    }

    /**
     * SCIPIO: Returns true if value is "Y", or false
     * if value is "N", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValueIndicator(String value, boolean defaultValue) {
        return UtilValidate.booleanValueIndicator(value, defaultValue);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE, "true" or "Y", or Boolean.FALSE
     * if value is Boolean.FALSE, "false" or "N", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValueVersatile(Object value) {
        return UtilValidate.booleanValueVersatile(value);
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is "true" or "Y", or Boolean.FALSE
     * if value is "false" or "N", or null if anything else (case-sensitive).
     */
    public static Boolean booleanValueVersatile(String value) {
        return UtilValidate.booleanValueVersatile(value);
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE, "true" or "Y", or false
     * if value is Boolean.FALSE, "false" or "N", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValueVersatile(Object value, boolean defaultValue) {
        return UtilValidate.booleanValueVersatile(value, defaultValue);
    }

    /**
     * SCIPIO: Returns true if value is "true" or "Y", or false
     * if value is "false" or "N", or defaultValue if anything else (case-sensitive).
     */
    public static boolean booleanValueVersatile(String value, boolean defaultValue) {
        return UtilValidate.booleanValueVersatile(value, defaultValue);
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE or "Y", or "N"
     * if value is Boolean.FALSE or "N", or null if anything else (case-sensitive).
     */
    public static String indicatorValue(Object value) {
        return UtilValidate.indicatorValue(value);
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE or "Y", or "N"
     * if value is Boolean.FALSE or "N", or defaultValue if anything else (case-sensitive).
     */
    public static String indicatorValue(Object value, String defaultValue) {
        return UtilValidate.indicatorValue(value, defaultValue);
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE, "true" or "Y", or "N"
     * if value is Boolean.FALSE, "false" or "N", or null if anything else (case-sensitive).
     */
    public static String indicatorValueVersatile(Object value) {
        return UtilValidate.indicatorValueVersatile(value);
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE, "true" or "Y", or "N"
     * if value is Boolean.FALSE, "false" or "N", or defaultValue if anything else (case-sensitive).
     */
    public static String indicatorValueVersatile(Object value, String defaultValue) {
        return UtilValidate.indicatorValueVersatile(value, defaultValue);
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set.
     * (We use this pattern constantly.)
     *
     * TODO: Deprecate in favor of {@link #constSet}
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> unmodifiableHashSet(T... elems) {
        return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set copied from the given collection.
     *
     * TODO: Deprecate in favor of {@link #constSetCopy}
     */
    public static <T> Set<T> unmodifiableHashSetCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableSet(new HashSet<T>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set copied from the given collection, with support for extra elements.
     * If collection is null, a new one is created.
     */
    @Deprecated
    public static <T> Set<T> unmodifiableHashSetCopyAdd(Collection<? extends T> collection, T... addValues) {
        Set<T> set = (collection != null) ? new HashSet<>(collection) : new HashSet<>();
        for (T value : addValues) {
            set.add(value);
        }
        return Collections.unmodifiableSet(set);
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set copied from the given collection, with support for extra elements.
     */
    @Deprecated
    public static <T> Set<T> unmodifiableHashSetCopyRemove(Collection<? extends T> collection, T... removeValues) {
        Set<T> set = new HashSet<>(collection);
        for (T value : removeValues) {
            set.remove(value);
        }
        return Collections.unmodifiableSet(set);
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set.
     *
     * TODO: Deprecate in favor of {@link #constSet}
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> unmodifiableLinkedHashSet(T... elems) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set.
     *
     * TODO: Deprecate in favor of {@link #constSetCopy}
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> unmodifiableLinkedHashSetCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set copied from the given collection, with support for extra elements.
     * If collection is null, a new one is created.
     */
    @Deprecated
    public static <T> Set<T> unmodifiableLinkedHashSetCopyAdd(Collection<? extends T> collection, T... addValues) {
        Set<T> set = (collection != null) ? new LinkedHashSet<>(collection) : new LinkedHashSet<>();
        for(T value : addValues) {
            set.add(value);
        }
        return Collections.unmodifiableSet(set);
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set copied from the given collection, with support for extra elements.
     */
    @Deprecated
    public static <T> Set<T> unmodifiableLinkedHashSetCopyRemove(Collection<? extends T> collection, T... removeValues) {
        Set<T> set = new LinkedHashSet<>(collection);
        for(T value : removeValues) {
            set.remove(value);
        }
        return Collections.unmodifiableSet(set);
    }

    /**
     * SCIPIO: Returns an unmodifiable array list.
     *
     * TODO: Deprecate in favor of List.of
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> unmodifiableArrayList(T... elems) {
        return Collections.unmodifiableList(new ArrayList<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable array list copied from the given collection.
     *
     * TODO: Deprecate in favor of List.copyOf
     */
    public static <T> List<T> unmodifiableArrayListCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableList(new ArrayList<>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable array list copied from the given collection, with support for extra elements.
     * If collection is null, a new one is created.
     */
    @Deprecated
    public static <T> List<T> unmodifiableArrayListCopyAdd(Collection<? extends T> collection, T... addValues) {
        List<T> list = (collection != null) ? new ArrayList<>(collection) : new ArrayList<>();
        for(T value : addValues) {
            list.add(value);
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * SCIPIO: Returns an unmodifiable array list copied from the given collection, with support for extra elements.
     */
    @Deprecated
    public static <T> List<T> unmodifiableArrayListCopyRemove(Collection<? extends T> collection, T... removeValues) {
        List<T> list = new ArrayList<>(collection);
        for(T value : removeValues) {
            list.remove(value);
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> List<T> unmodifiableLinkedList(T... elems) {
        return Collections.unmodifiableList(new LinkedList<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list copied from the given collection.
     */
    @Deprecated
    public static <T> List<T> unmodifiableLinkedListCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableList(new LinkedList<>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list copied from the given collection, with support for extra elements.
     * If collection is null, a new one is created.
     */
    @Deprecated
    public static <T> List<T> unmodifiableLinkedListCopyAdd(Collection<? extends T> collection, T... addValues) {
        List<T> list = (collection != null) ? new LinkedList<>(collection) : new LinkedList<>();
        for(T value : addValues) {
            list.add(value);
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list copied from the given collection, with support for extra elements.
     */
    @Deprecated
    public static <T> List<T> unmodifiableLinkedListCopyRemove(Collection<? extends T> collection, T... removeValues) {
        List<T> list = new LinkedList<>(collection);
        for(T value : removeValues) {
            list.remove(value);
        }
        return Collections.unmodifiableList(list);
    }

    /**
     * SCIPIO: Creates a hash map copy with specified keys.
     * Added 2017-12-04.
     */
    @Deprecated
    public static <K, V> Map<K, V> toHashMapWithKeys(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new HashMap<>();
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
        return outMap;
    }

    /**
     * SCIPIO: Creates a hash map copy including specified keys.
     * Added 2017-12-04.
     */
    @Deprecated
    @SafeVarargs
    public static <K, V> Map<K, V> toHashMapWithKeys(Map<? extends K, ? extends V> inMap, K... keys) {
        Map<K, V> outMap = new HashMap<>();
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
        return outMap;
    }

    /**
     * SCIPIO: Creates a hash map copy excluding specified keys.
     * Added 2017-12-04.
     */
    @Deprecated
    public static <K, V> Map<K, V> toHashMapWithoutKeys(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new HashMap<>();
        for(Map.Entry<? extends K, ? extends V> entry : inMap.entrySet()) {
            if (!keys.contains(entry.getKey())) { outMap.put(entry.getKey(), entry.getValue()); }
        }
        return outMap;
    }

    /**
     * SCIPIO: Creates a hash map copy excluding specified keys.
     * Added 2017-12-04.
     */
    @Deprecated
    @SafeVarargs
    public static <K, V> Map<K, V> toHashMapWithoutKeys(Map<? extends K, ? extends V> inMap, K... keys) {
        return toHashMapWithoutKeys(inMap, new HashSet<>(Arrays.asList(keys)));
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy with specified keys, preserving
     * the original insertion order.
     * NOTE: this is slower than {@link #toLinkedHashMapWithKeysNewOrder}.
     * Added 2017-12-04.
     */
    @Deprecated
    public static <K, V> Map<K, V> toLinkedHashMapWithKeysOrigOrder(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new LinkedHashMap<>();
        for(Map.Entry<? extends K, ? extends V> entry : inMap.entrySet()) {
            if (keys.contains(entry.getKey())) { outMap.put(entry.getKey(), entry.getValue()); }
        }
        return outMap;
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy with specified keys, preserving
     * the original insertion order.
     * NOTE: this is slower than {@link #toLinkedHashMapWithKeysNewOrder}.
     * Added 2017-12-04.
     */
    @Deprecated
    @SafeVarargs
    public static <K, V> Map<K, V> toLinkedHashMapWithKeysOrigOrder(Map<? extends K, ? extends V> inMap, K... keys) {
        return toLinkedHashMapWithKeysOrigOrder(inMap, new HashSet<>(Arrays.asList(keys)));
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy including specified keys, with the
     * key order determined by the order of the passed keys collection parameter.
     * Added 2017-12-04.
     */
    @Deprecated
    public static <K, V> Map<K, V> toLinkedHashMapWithKeysNewOrder(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new HashMap<>();
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
        return outMap;
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy including specified keys, with the
     * key order determined by the order of the passed keys collection parameter.
     * Added 2017-12-04.
     */
    @Deprecated
    @SafeVarargs
    public static <K, V> Map<K, V> toLinkedHashMapWithKeysNewOrder(Map<? extends K, ? extends V> inMap, K... keys) {
        Map<K, V> outMap = new HashMap<>();
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
        return outMap;
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy excluding specified keys.
     * The original key order is preserved.
     * Added 2017-12-04.
     */
    @Deprecated
    public static <K, V> Map<K, V> toLinkedHashMapWithoutKeys(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new LinkedHashMap<>();
        for(Map.Entry<? extends K, ? extends V> entry : inMap.entrySet()) {
            if (!keys.contains(entry.getKey())) { outMap.put(entry.getKey(), entry.getValue()); }
        }
        return outMap;
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy excluding specified keys.
     * The original key order is preserved.
     * Added 2017-12-04.
     */
    @Deprecated
    @SafeVarargs
    public static <K, V> Map<K, V> toLinkedHashMapWithoutKeys(Map<? extends K, ? extends V> inMap, K... keys) {
        return toLinkedHashMapWithoutKeys(inMap, new HashSet<>(Arrays.asList(keys)));
    }

    /**
     * SCIPIO: For each map in the given collection, extracts the value for the specified key and adds
     * it to the given outValueCollection.
     * Added 2018-05-29.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V, C extends Collection<T>> C extractValuesForKey(Collection<? extends Map<K, V>> mapList,
            K key, C outValueCollection) {
        for(Map<K, V> map : mapList) {
            outValueCollection.add((T) map.get(key));
        }
        return outValueCollection;
    }

    /**
     * SCIPIO: For each map in the given collection, extracts the value for the specified key and adds
     * it to the given outValueCollection; skips null values.
     * Added 2018-05-29.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V, C extends Collection<T>> C extractValuesForKeyNonNull(Collection<? extends Map<K, V>> mapList,
            K key, C outValueCollection) {
        for(Map<K, V> map : mapList) {
            V value = map.get(key);
            if (value != null) {
                outValueCollection.add((T) map.get(key));
            }
        }
        return outValueCollection;
    }

    /**
     * SCIPIO: For each map in the given collection, extracts the value for the specified key and
     * maps it to the original map value in the outValueMap.
     * Added 2018-05-29.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V, E extends Map<K, V>, M extends Map<T, E>> M extractValuesForKeyAsMap(Collection<E> mapList,
            K key, M outValueMap) {
        for(E map : mapList) {
            outValueMap.put((T) map.get(key), map);
        }
        return outValueMap;
    }

    /**
     * SCIPIO: For each map in the given collection, extracts the value for the specified key and
     * maps it to the original map value in the outValueMap; skips null values.
     * Added 2018-05-29.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V, E extends Map<K, V>, M extends Map<T, E>> M extractValuesForKeyAsMapNonNull(Collection<E> mapList,
            K key, M outValueMap) {
        for(E map : mapList) {
            V value = map.get(key);
            if (value != null) {
                outValueMap.put((T) map.get(key), map);
            }
        }
        return outValueMap;
    }

    /**
     * SCIPIO: Returns true if and only if all of the test values are in the given collection.
     * Added 2018-09-26.
     */
    public static <T> boolean containsAll(Collection<T> collection, Iterable<? extends T> testValues) {
        if (collection == null) {
            return false;
        }
        for (T value : testValues) {
            if (!collection.contains(value)) {
                return false;
            }
        }
        return true;
    }

    /**
     * SCIPIO: If the given value is already an ArrayList or null, returns it as-is; otherwise returns a copy as an ArrayList.
     * Added 2018-11-23.
     */
    @Deprecated
    public static <T> List<T> asArrayList(Collection<T> value) {
        return (value instanceof ArrayList || value == null) ? (List<T>) value : new ArrayList<>(value);
    }

    /**
     * SCIPIO: If the given value is an ArrayList, returns it as-is; if Collection, returns a new
     * ArrayList copy from it; if null, returns null; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> List<T> asArrayList(Object value) {
        return (value instanceof ArrayList || value == null) ? (List<T>) value : new ArrayList<T>((Collection<T>) value);
    }

    /**
     * SCIPIO: If the given value is not already a HashSet or null, returns it as-is; otherwise returns a copy as a HashSet.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <T> Set<T> asHashSet(Collection<T> value) {
        return (value instanceof HashSet || value == null) ? (Set<T>) value : new HashSet<>(value);
    }

    /**
     * SCIPIO: If the given value is an HashSet, returns it as-is; if Collection, returns a new
     * HashSet copy from it; if null, returns null; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> Set<T> asHashSet(Object value) {
        return (value instanceof HashSet || value == null) ? (Set<T>) value : new HashSet<T>((Collection<T>) value);
    }

    /**
     * SCIPIO: If the given value is not already a LinkedHashSet or null, returns it as-is; otherwise returns a copy as a LinkedHashSet.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <T> Set<T> asLinkedHashSet(Collection<T> value) {
        return (value instanceof LinkedHashSet || value == null) ? (Set<T>) value : new LinkedHashSet<>(value);
    }

    /**
     * SCIPIO: If the given value is an LinkedHashSet, returns it as-is; if Collection, returns a new
     * LinkedHashSet copy from it; if null, returns null; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> Set<T> asLinkedHashSet(Object value) {
        return (value instanceof LinkedHashSet || value == null) ? (Set<T>) value : new LinkedHashSet<T>((Collection<T>) value);
    }

    /**
     * SCIPIO: If the given value is not already a HashMap or null, returns it as-is; otherwise returns a copy as a HashMap.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <K, V> Map<K, V> asHashMap(Map<K, V> value) {
        return (value instanceof HashMap || value == null) ? (Map<K, V>) value : new HashMap<>(value);
    }

    /**
     * SCIPIO: If the given value is an HashMap, returns it as-is; if another Map, returns a new
     * HashMap copy from it; if null, returns null; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> asHashMap(Object value) {
        return (value instanceof HashMap || value == null) ? (Map<K, V>) value : new HashMap<K, V>((Map<K, V>) value);
    }

    /**
     * SCIPIO: If the given value is not already a LinkedHashMap or null, returns it as-is; otherwise returns a copy as a LinkedHashMap.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <K, V> Map<K, V> asLinkedHashMap(Map<K, V> value) {
        return (value instanceof LinkedHashMap || value == null) ? (Map<K, V>) value : new LinkedHashMap<>(value);
    }

    /**
     * SCIPIO: If the given value is an LinkedHashMap, returns it as-is; if another Map, returns a new
     * LinkedHashMap copy from it; if null, returns null; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> asLinkedHashMap(Object value) {
        return (value instanceof LinkedHashMap || value == null) ? (Map<K, V>) value : new LinkedHashMap<K, V>((Map<K, V>) value);
    }

    /**
     * SCIPIO: If the given value is not already an ArrayList, returns it as-is; otherwise creates a copy as an ArrayList;
     * if null, returns empty.
     * Added 2018-11-23.
     */
    @Deprecated
    public static <T> List<T> asArrayListNonNull(Collection<T> value) {
        return (value instanceof ArrayList) ? (List<T>) value : ((value == null) ? new ArrayList<>() : new ArrayList<>(value));
    }

    /**
     * SCIPIO: If the given value is an ArrayList, returns it as-is; if Collection, returns a new
     * ArrayList copy from it; if null, returns empty; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> List<T> asArrayListNonNull(Object value) {
        return (value instanceof ArrayList) ? (List<T>) value : ((value == null) ? new ArrayList<>() : new ArrayList<>((Collection<T>) value));
    }

    /**
     * SCIPIO: If the given value is not already a HashSet, returns it as-is; otherwise returns a copy as a HashSet;
     * if null, returns empty.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <T> Set<T> asHashSetNonNull(Collection<T> value) {
        return (value instanceof HashSet) ? (Set<T>) value : ((value == null) ? new HashSet<>() : new HashSet<>(value));
    }

    /**
     * SCIPIO: If the given value is an HashSet, returns it as-is; if Collection, returns a new
     * HashSet copy from it; if null, returns empty; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> Set<T> asHashSetNonNull(Object value) {
        return (value instanceof HashSet) ? (Set<T>) value : ((value == null) ? new HashSet<>() : new HashSet<>((Collection<T>) value));
    }

    /**
     * SCIPIO: If the given value is not already a LinkedHashSet, returns it as-is; otherwise returns a copy as a HashSet;
     * if null, returns empty.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <T> Set<T> asLinkedHashSetNonNull(Collection<T> value) {
        return (value instanceof LinkedHashSet) ? (Set<T>) value : ((value == null) ? new LinkedHashSet<>() : new LinkedHashSet<>(value));
    }

    /**
     * SCIPIO: If the given value is an LinkedHashSet, returns it as-is; if Collection, returns a new
     * LinkedHashSet copy from it; if null, returns empty; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <T> Set<T> asLinkedHashSetNonNull(Object value) {
        return (value instanceof LinkedHashSet) ? (Set<T>) value : ((value == null) ? new LinkedHashSet<>() : new LinkedHashSet<>((Collection<T>) value));
    }

    /**
     * SCIPIO: If the given value is not already a HashMap, returns it as-is; otherwise returns a copy as a HashMap;
     * if null, returns empty.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <K, V> Map<K, V> asHashMapNonNull(Map<K, V> value) {
        return (value instanceof HashMap) ? (Map<K, V>) value : ((value == null) ? new HashMap<>() : new HashMap<>(value));
    }

    /**
     * SCIPIO: If the given value is an HashMap, returns it as-is; if another Map, returns a new
     * HashMap copy from it; if null, returns empty; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> asHashMapNonNull(Object value) {
        return (value instanceof HashMap) ? (Map<K, V>) value : ((value == null) ? new HashMap<>() : new HashMap<>((Map<K, V>) value));
    }

    /**
     * SCIPIO: If the given value is not already a LinkedHashMap, returns it as-is; otherwise returns a copy as a LinkedHashMap;
     * if null, returns empty.
     * Added 2019-01-23.
     */
    @Deprecated
    public static <K, V> Map<K, V> asLinkedHashMapNonNull(Map<K, V> value) {
        return (value instanceof LinkedHashMap) ? (Map<K, V>) value : ((value == null) ? new LinkedHashMap<>() : new LinkedHashMap<>(value));
    }

    /**
     * SCIPIO: If the given value is an LinkedHashMap, returns it as-is; if another Map, returns a new
     * LinkedHashMap copy from it; if null, returns empty; if other type, throws ClassCastException.
     * Added 2019-01-23.
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> asLinkedHashMapNonNull(Object value) {
        return (value instanceof LinkedHashMap) ? (Map<K, V>) value : ((value == null) ? new LinkedHashMap<>() : new LinkedHashMap<>((Map<K, V>) value));
    }

    /**
     * SCIPIO: Returns a new list composed of the given collection but reversed.
     * Added 2018-12-03.
     */
    public static <T> List<T> asReversedList(Collection<T> list) {
        List<T> newList = new ArrayList<>(list);
        Collections.reverse(newList);
        return newList;
    }

    /**
     * SCIPIO: Returns a new list composed of the given collection but reversed.
     * Added 2018-12-03.
     */
    public static <T> List<T> unmodifiableReversedList(Collection<T> list) {
        return Collections.unmodifiableList(asReversedList(list));
    }

    /**
     * SCIPIO: Takes a list of maps and groups them using the specified key value.
     */
    public static <K, V, M extends Map<K, V>> Map<K, List<M>> groupMapsByKey(Iterable<M> records, K groupByKey) {
        Map<K, List<M>> map = new HashMap<>();
        for(M elem : records) {
            addToListInMap(elem, map, groupByKey);
        }
        return map;
    }

    /**
     * SCIPIO: A small helper to extra all the values for a given field from each map.
     * Convenience method to avoid null pointers and stream API.
     * Does nothing if either mapList or out are null.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V, C extends Collection<T>> C getMapValuesForKey(Collection<? extends Map<K, V>> mapList, K key, C out) {
        if (mapList != null && out != null) {
            for(Map<K, V> map : mapList) {
                out.add((T) map.get(key));
            }
        }
        return out;
    }

    /**
     * SCIPIO: A small helper to extra all the values for a given field from each map, as a list of same length.
     * Convenience method to avoid null pointers and stream API.
     * If mapList is null or mapList is empty, returns unmodifiable empty list.
     * Use {@link #getMapValuesForKeyOrNewList} to force new list in all cases.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V> List<T> getMapValuesForKey(Collection<? extends Map<K, V>> mapList, K key) {
        if (mapList == null || mapList.isEmpty()) {
            return Collections.emptyList();
        }
        List<T> out = new ArrayList<>(mapList.size());
        for(Map<K, V> map : mapList) {
            out.add((T) map.get(key));
        }
        return out;
    }

    /**
     * SCIPIO: A small helper to extra all the values for a given field from each map, as a same-sized ArrayList.
     * Convenience method to avoid null pointers and stream API.
     * NOTE: Always creates a new ArrayList even if mapList is null or empty.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, T extends V> List<T> getMapValuesForKeyOrNewList(Collection<? extends Map<K, V>> mapList, K key) {
        if (mapList == null || mapList.isEmpty()) {
            return new ArrayList<>();
        }
        List<T> out = new ArrayList<>(mapList.size());
        for(Map<K, V> map : mapList) {
            out.add((T) map.get(key));
        }
        return out;
    }

    /**
     * SCIPIO: Wraps an Enumeration in an Iterator.
     */
    public static <T> Iterator<T> toIterator(Enumeration<T> enumeration) {
        return new EnumerationIterator<T>(enumeration);
    }

    /**
     * SCIPIO: Wraps an Iterator in an Iterable.
     * Useful when you have an API that returns only Iterator.
     */
    public static <T> Iterable<T> toIterable(Iterator<T> iterator) {
        return new Iterable<T>() {
            @Override
            public Iterator<T> iterator() {
                return iterator;
            }
        };
    }

    /**
     * SCIPIO: Wraps an Enumeration in an Iterable.
     * NOTE: This could become inefficient if you are dealing with a large number of collections.
     */
    public static <T> Iterable<T> toIterable(Enumeration<T> enumeration) {
        return toIterable(toIterator(enumeration));
    }

    /**
     * Adds the given values pairs to the collection and returns the collection, for chaining (SCIPIO).
     * @deprecated SCIPIO: 3.0.0: Redundant/misnamed; use {@link #addAll(Collection, Collection)}
     */
    @Deprecated
    public static <C extends Collection<E>, E> C add(C collection, Collection<? extends E> newElems) {
        if (collection != null && newElems != null) {
            collection.addAll(newElems);
        }
        return collection;
    }

    /**
     * Adds the given values pairs to the collection and returns the collection, for chaining (SCIPIO).
     */
    @SafeVarargs
    public static <C extends Collection<E>, E> C add(C collection, E... newElems) {
        if (collection != null && newElems != null) {
            collection.addAll(List.of(newElems));
        }
        return collection;
    }

    /**
     * Calls out.addAll for every passed collection on the outCollection.
     *
     * <p>SCIPIO: 3.0.0: More robust; removed varargs array due to compiler warnings.</p>
     *
     * @param outCollection The collection to add the inCollection values to
     * @param inCollection The source collection
     * @return The outCollection, for convenience
     */
    public static <C extends Collection<O>, O> C addAll(C outCollection, Collection<? extends O> inCollection) {
        if (inCollection != null && outCollection != null) {
            outCollection.addAll(inCollection);
        }
        return outCollection;
    }

    /**
     * SCIPIO: Adds the given enumeration's elements to the given out collection.
     *
     * @param outCollection The collection to add the inEnumeration values to
     * @param inEnumeration The source enumeration
     * @return The outCollection, for convenience
     */
    public static <T, C extends Collection<T>> C addAll(C outCollection, Enumeration<T> inEnumeration) {
        if (inEnumeration != null && outCollection != null) {
            while (inEnumeration.hasMoreElements()) {
                outCollection.add(inEnumeration.nextElement());
            }
        }
        return outCollection;
    }

    /**
     * Calls out.removeAll for every passed collection on the outCollection.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param outCollection The collection to remove the inCollection values from
     * @param inCollection The source collection
     * @return The outCollection, for convenience
     */
    public static <C extends Collection<O>, O> C removeAll(C outCollection, Collection<? extends O> inCollection) {
        if (inCollection != null && outCollection != null) {
            outCollection.removeAll(inCollection);
        }
        return outCollection;
    }

    /**
     * Adds the given enumeration's elements to the given out collection.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     *
     * @param outCollection The collection to remove the inEnumeration values from
     * @param inEnumeration The source enumeration
     * @return The outCollection, for convenience
     */
    public static <T, C extends Collection<T>> C removeAll(C outCollection, Enumeration<T> inEnumeration) {
        if (inEnumeration != null && outCollection != null) {
            while (inEnumeration.hasMoreElements()) {
                outCollection.remove(inEnumeration.nextElement());
            }
        }
        return outCollection;
    }

    /**
     * SCIPIO: Returns a new List containing the enumeration's elements.
     */
    public static <T> List<T> toList(Enumeration<T> enumeration) {
        return Collections.list(enumeration);
    }

    /**
     * SCIPIO: Returns a new Set containing the enumeration's elements.
     */
    public static <T> Set<T> toSet(Enumeration<T> enumeration) {
        return addAll(new HashSet<>(), enumeration);
    }

    /**
     * SCIPIO: Returns a new HashSet containing the enumeration's elements.
     */
    public static <T> Set<T> toHashSet(Enumeration<T> enumeration) {
        return addAll(new HashSet<>(), enumeration);
    }

    /**
     * SCIPIO: Returns a new LinkedHashSet containing the enumeration's elements.
     */
    public static <T> Set<T> toLinkedHashSet(Enumeration<T> enumeration) {
        return addAll(new HashSet<>(), enumeration);
    }

    /**
     * SCIPIO: Checks if the given enumeration contains the given object
     * using the {@link #equals(Object)} method.
     */
    public static boolean contains(Enumeration<?> enumeration, Object object) {
        if (enumeration == null) {
            return false;
        }
        if (object != null) {
            while(enumeration.hasMoreElements()) {
                if (object.equals(enumeration.nextElement())) {
                    return true;
                }
            }
        } else {
            while(enumeration.hasMoreElements()) {
                if (enumeration.nextElement() == null) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * SCIPIO: DO NOT USE: Returns a "dummy" static instance, for use by <code>FreeMarkerWorker</code>.
     * Subject to change without notice.
     * Added 2019-01-31.
     */
    public static UtilMisc getStaticInstance() {
        return INSTANCE;
    }

    /**
     * SCIPIO: Returns true if either the passed value is non-null or if the given map contains the passed key.
     * This can be used to optimize Map accesses when the values are biased toward predominantly non-null.
     */
    public static <K, V> boolean containsKey(Map<K, V> map, K key, Object value) {
        return (value != null || map.containsKey(key));
    }

    /**
     * SCIPIO: Transfers the specified keys from srcMap to dstMap and returns dstMap.
     */
    public static <K, V> Map<K, V> copyKeys(Map<K, ? extends V> srcMap, Map<K, V> dstMap, Iterable<K> keys) {
        for(K key : keys) {
            dstMap.put(key, srcMap.get(key));
        }
        return dstMap;
    }

    /**
     * SCIPIO: Copy the collection to an ArrayList of same capacity plus the given members.
     */
    public static <T> List<T> copyExtendList(Collection<? extends T> collection, Collection<? extends T> membersToAppend) {
        List<T> list = new ArrayList<>(collection.size() + membersToAppend.size());
        list.addAll(collection);
        list.addAll(membersToAppend);
        return list;
    }

    /**
     * SCIPIO: Copy the collection to an ArrayList of same capacity plus the given members.
     */
    public static <T, A extends T> List<T> copyExtendList(Collection<? extends T> collection, A... membersToAppend) {
        List<T> list = new ArrayList<>(collection.size() + membersToAppend.length);
        list.addAll(collection);
        for(T member : membersToAppend) {
            list.add(member);
        }
        return list;
    }

    /**
     * SCIPIO: Returns an ordered (currently tree) map where all keys are converted to integers and sorted naturally.
     */
    public static <V> Map<Integer, V> asOrderedIntegerMap(Map<String, V> map) {
        Map<Integer, V> orderedMap = new TreeMap<>();
        for(Map.Entry<String, V> entry : map.entrySet()) {
            orderedMap.put(Integer.parseInt(entry.getKey()), entry.getValue());
        }
        return orderedMap;
    }

    /**
     * SCIPIO: Returns null if collection is empty, otherwise the collection itself.
     */
    public static <C extends Collection> C nullIfEmpty(C collection) {
        return (collection == null) || !collection.isEmpty() ? collection : null;
    }

    /**
     * SCIPIO: Returns null if map is empty, otherwise the map itself.
     */
    public static <M extends Map<?, ?>> M nullIfEmpty(M map) {
        return (map == null) || !map.isEmpty() ? map : null;
    }

    /**
     * SCIPIO: Simple method to extract keys having prefix.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, W extends V> Map<K, V> getEntriesWithKeyPrefix(Map<K, V> out, Map<String, W> map, String prefix) {
        for(Map.Entry<String, W> entry : map.entrySet()) {
            if (entry.getKey().startsWith(prefix)) {
                out.put((K) entry.getKey(), entry.getValue());
            }
        }
        return out;
    }

    /**
     * SCIPIO: Simple method to extract keys having prefix.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, W extends V> Map<K, V> getEntriesWithKeyPrefix(Map<String, W> map, String prefix) {
        return getEntriesWithKeyPrefix(new LinkedHashMap<>(), map, prefix);
    }

    /**
     * SCIPIO: Simple method to extract keys having prefix, with ability to strip prefix whole or partial.
     */
    @SuppressWarnings("unchecked")
    public static <K, V, W extends V> Map<K, V> getStripEntriesWithKeyPrefix(Map<K, V> out, Map<String, W> map, String prefix, String stripPrefix) {
        for(Map.Entry<String, W> entry : map.entrySet()) {
            if (entry.getKey().startsWith(prefix)) {
                out.put((K) entry.getKey().substring(stripPrefix.length()), entry.getValue());
            }
        }
        return out;
    }

    public static <V, T extends V> V getFirst(Iterable<T> iterable) {
        if (iterable instanceof List) {
            return getFirst(UtilGenerics.<List<T>>cast(iterable));
        } else {
            return (iterable != null) ? iterable.iterator().next() : null;
        }
    }

    public static <V, T extends V> V getFirst(List<T> list) {
        return (list != null) ? list.get(0) : null;
    }

    /**
     * Returns an Iterator as-is or from an Iterable (SCIPIO).
     */
    public static <T, I extends Iterator<T>> Iterator<T> asIterator(Object object) {
        if (object instanceof Iterator) {
            return UtilGenerics.cast(object);
        } else if (object instanceof Iterable) {
            return UtilGenerics.<Iterable<T>>cast(object).iterator();
        } else {
            throw new IllegalArgumentException("Not iterable or iterator");
        }
    }

    /**
     * Static next() method that can handle any iterator type including NextOnlyIterator/EntityListIterator (SCIPIO),
     * returns null when done.
     * Example:
     * <pre>{@code
     *     Iterator<GenericValue> iterator = ...;
     *     GenericValue entity;
     *     while((entity = UtilMisc.next(iterator)) != null) {
     *         ...
     *     }
     * }</pre>
     * NOTE: Will not work properly if the iterator contains null entries.
     * @return the next value or null if no more elements.
     */
    public static <T> T next(Iterator<T> iterator) {
        return (iterator instanceof NextOnlyIterator) ? iterator.next() : (iterator.hasNext() ? iterator.next() : null);
    }

    public static <K, V> Map<K, V> subMap(Map<K, V> out, Map<K, V> map, Collection<? extends K> keys) { // SCIPIO
        for(K key : keys) {
            out.put(key, map.get(key));
        }
        return out;
    }

    public static <K, V> Map<K, V> subMap(Map<K, V> map, Collection<? extends K> keys) { // SCIPIO
        return subMap(new LinkedHashMap<>(), map, keys);
    }

    public static <K, V> Map<K, V> subMapNonNull(Map<K, V> out, Map<K, V> map, Collection<? extends K> keys) { // SCIPIO
        for(K key : keys) {
            V value = map.get(key);
            if (value != null) {
                out.put(key, value);
            }
        }
        return out;
    }

    public static <K, V> Map<K, V> subMapNonNull(Map<K, V> map, Collection<? extends K> keys) { // SCIPIO
        return subMapNonNull(new LinkedHashMap<>(), map, keys);
    }

    /**
     * SCIPIO: For each record extracts a shortPk-like string that maps to it.
     */
    public static <M extends Map<String, Object>> Map<String, M> makeShortKeyRecordMap(Collection<? extends M> records,
                                                                                       Collection<String> keyNames,
                                                                                       String keyDelim,
                                                                                       Map<String, M> outMap) throws IllegalArgumentException {
        if (records == null) {
            return null;
        }
        if (keyNames.size() == 1) {
            String pkFieldName = UtilMisc.first(keyNames);
            for (M record : records) {
                outMap.put((String) record.get(pkFieldName), record);
            }
        } else {
            for (M record : records) {
                StringBuilder key = new StringBuilder();
                for(String pkFieldName : keyNames) {
                    if (key.length() > 0) {
                        key.append(keyDelim);
                    }
                    key.append((String) record.get(pkFieldName));
                }
                outMap.put(key.toString(), record);
            }
        }
        return outMap;
    }

    /**
     * SCIPIO: For each record extracts a shortPk-like string that maps to it.
     */
    public static <M extends Map<String, Object>> Map<String, M> makeShortKeyRecordMap(Collection<? extends M> records,
                                                                                       Collection<String> keyNames,
                                                                                       String keyDelim) throws IllegalArgumentException {
        return makeShortKeyRecordMap(records, keyNames, keyDelim, new LinkedHashMap<>());
    }

    /**
     * SCIPIO: Returns the Collections.unmodifiableXxx or Collections.emptyXxx instance corresponding to the underlying type of collection, best-effort.
     */
    public static <C extends Collection<V>, V> C unmodifiableAdapted(C collection) {
        if (collection == null) {
            return null;
        } else if (collection instanceof List) {
            return UtilGenerics.cast(!collection.isEmpty() ? Collections.unmodifiableList(UtilGenerics.cast(collection)) : Collections.emptyList());
        } else if (collection instanceof NavigableSet) {
            return UtilGenerics.cast(!collection.isEmpty() ? Collections.unmodifiableNavigableSet(UtilGenerics.cast(collection)) : Collections.emptyNavigableSet());
        } else if (collection instanceof SortedSet) {
            return UtilGenerics.cast(!collection.isEmpty() ? Collections.unmodifiableSortedSet(UtilGenerics.cast(collection)) : Collections.emptySortedSet());
        } else if (collection instanceof Set) {
            return UtilGenerics.cast(!collection.isEmpty() ? Collections.unmodifiableSet(UtilGenerics.cast(collection)) : Collections.emptySet());
        } else {
            // NOTE: This may produce ClassCastException if wrong result type, but let's let caller handles this case, for simplicity
            return UtilGenerics.cast(Collections.unmodifiableCollection(collection));
        }
    }

    /**
     * SCIPIO: Returns the Collections.unmodifiableXxx or Collections.emptyXxx instance corresponding to the underlying type of map, best-effort.
     */
    public static <K, V> Map<K, V> unmodifiableAdapted(Map<K, V> map) {
        if (map == null) {
            return null;
        } else if (map instanceof NavigableMap) {
            return UtilGenerics.cast(!map.isEmpty() ? Collections.unmodifiableNavigableMap(UtilGenerics.cast(map)) : Collections.emptyNavigableMap());
        } else if (map instanceof SortedMap) {
            return UtilGenerics.cast(!map.isEmpty() ? Collections.unmodifiableSortedMap(UtilGenerics.cast(map)) : Collections.emptySortedMap());
        } else {
            return UtilGenerics.cast(!map.isEmpty() ? Collections.unmodifiableMap(map) : Collections.emptyMap());
        }
    }

    /**
     * SCIPIO: Returns the Collections.unmodifiableXxx or Collections.emptyXxx instance corresponding to the underlying type of map, best-effort.
     */
    public static <T> T unmodifiableGeneric(Object collOrMap) {
        if (collOrMap == null) {
            return null;
        } else if (collOrMap instanceof Collection) {
            return UtilGenerics.cast(unmodifiableAdapted((Collection<?>) collOrMap));
        } else if (collOrMap instanceof Map) {
            return UtilGenerics.cast(unmodifiableAdapted((Map<?, ?>) collOrMap));
        } else {
            throw new IllegalArgumentException("Unsupported type for unmodifiable collections: " + collOrMap.getClass().getName());
        }
    }

    /**
     * SCIPIO: Returns the Collections.emptyXxx instance corresponding to the specified type, best-effort.
     */
    public static <T> T emptyGeneric(Class<?> type) {
        if (List.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptyList());
        } else if (NavigableSet.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptyNavigableSet());
        } else if (SortedSet.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptySortedSet());
        } else if (Set.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptySet());
        } else if (NavigableMap.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptyNavigableMap());
        } else if (SortedMap.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptySortedMap());
        } else if (Map.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptyMap());
        } else if (Collection.class.isAssignableFrom(type)) {
            return UtilGenerics.cast(Collections.emptyList());
        } else {
            throw new IllegalArgumentException("Unrecognized collection type: " + type);
        }
    }

    /**
     * SCIPIO: Attempts to optimize the given collection.
     * Currently simply trims ArrayLists.
     * NOTE: future versions may return a different instance, so the result must be used.
     */
    public static <C extends Collection<V>, V> C optimized(C collection) {
        if (collection instanceof ArrayList) {
            UtilGenerics.<ArrayList<V>>cast(collection).trimToSize();
        }
        return collection;
    }

    /**
     * SCIPIO: Attempts to optimize the given collection and returns a read-only version.
     */
    public static <C extends Collection<V>, V> C unmodifiableOptimized(C collection) {
        return unmodifiableAdapted(optimized(collection));
    }

    /**
     * SCIPIO: Attempts to optimize the given collection and returns a read-only version and produces null for empty.
     */
    public static <C extends Collection<V>, V> C unmodifiableOptimizedOrNull(C collection) {
        if (collection == null || collection.isEmpty()) {
            return null;
        } else {
            return unmodifiableAdapted(optimized(collection));
        }
    }

    /**
     * SCIPIO: Attempts to optimize the given map.
     * NOTE: future versions may return a different instance, so the result must be used.
     */
    public static <K, V> Map<K, V> optimized(Map<K, V> map) {
        return map;
    }

    /**
     * SCIPIO: Attempts to optimize the given map and returns a read-only version.
     */
    public static <K, V> Map<K, V> unmodifiableOptimized(Map<K, V> map) {
        return unmodifiableAdapted(optimized(map));
    }

    /**
     * SCIPIO: Attempts to optimize the given map and returns a read-only version and produces null for empty.
     */
    public static <K, V> Map<K, V> unmodifiableOptimizedOrNull(Map<K, V> map) {
        if (map == null || map.isEmpty()) {
            return null;
        } else {
            return unmodifiableAdapted(optimized(map));
        }
    }

    /**
     * SCIPIO: Returns a best-possible optimized views of the combined keys of both maps, unless one is not set.
     */
    public static <K> Set<K> keySet(Map<K, ?> firstMap, Map<K, ?> secondMap) { // SCIPIO
        if (UtilValidate.isNotEmpty(firstMap)) {
            if (UtilValidate.isNotEmpty(secondMap)) {
                Set<K> keys = new LinkedHashSet<>(firstMap.keySet());
                keys.addAll(secondMap.keySet());
                return keys;
            } else {
                return firstMap.keySet();
            }
        } else {
            if (UtilValidate.isNotEmpty(secondMap)) {
                return secondMap.keySet();
            } else {
                return Collections.emptySet();
            }
        }
    }

    /**
     * Returns the given parameter as a collection if it already is, otherwise puts it in a new collection;
     * if null, returns null.
     *
     * <p>WARN: If param is wrong type, collection type may be violated.</p>
     *
     * <p>SCIPIO: 3.0.0: Renamed.</p>
     */
    public static <T, C extends Collection<T>> C toCollection(Object param, Supplier<? extends Collection<? extends T>> containerFactory) {
        if (param == null) {
            return null;
        } else if (param instanceof Collection) {
            return UtilGenerics.cast(param);
        } else {
            Collection<? extends T> collection = containerFactory.get();
            collection.add(UtilGenerics.cast(param));
            return UtilGenerics.cast(collection);
        }
    }

    /**
     * Returns the given parameter as a collection if it already is, otherwise puts it in a new collection;
     * if null, returns null.
     *
     * <p>WARN: If param is wrong type, collection type may be violated.</p>
     *
     * <p>SCIPIO: 3.0.0: Renamed.</p>
     */
    public static <T, C extends Collection<T>> C toCollection(Object param) {
        return toCollection(param, ArrayList::new);
    }

    /**
     * Returns the given parameter as a collection if it already is, otherwise puts it in a new collection;
     * if null, returns null.
     * @deprecated SCIPIO: 3.0.0: Use {@link #toCollection(Object, Supplier)}
     *
     * <p>WARN: If param is wrong type, collection type may be violated.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    @Deprecated
    public static <T> Collection<T> asCollectionNonNull(Object param, Supplier<? extends Collection<? extends T>> containerFactory) {
        return toCollection(param, containerFactory);
    }

    /**
     * SCIPIO: Returns the given parameter as a collection if it already is, otherwise puts it in a new collection;
     * if null, returns null.
     * @deprecated SCIPIO: 3.0.0: Use {@link #toCollection(Object)}
     *
     * <p>WARN: If param is wrong type, collection type may be violated.</p>
     *
     * <p>SCIPIO: 2.x.x: Added.</p>
     */
    @Deprecated
    public static <T> Collection<T> asCollectionNonNull(Object param) {
        return toCollection(param);
    }

    public static <C extends Collection<Pattern>> C getPatterns(Object collectionOrPat, Supplier<C> containerFactory) {
        C out = null;
        if (collectionOrPat == null) {
            ;
        } else if (collectionOrPat instanceof Collection) {
            Collection<?> collection = UtilGenerics.cast(collectionOrPat);
            if (!collection.isEmpty()) {
                out = containerFactory.get();
                for (Object elem : collection) {
                    if (elem instanceof Pattern) {
                        out.add((Pattern) elem);
                    } else if (elem instanceof String) {
                        out.add(Pattern.compile((String) elem));
                    } else {
                        throw new IllegalArgumentException("Not a Pattern or String: " + (elem != null ? elem.getClass().getName() : null));
                    }
                }
            }
        } else if (collectionOrPat instanceof Pattern) {
            out = containerFactory.get();
            out.add((Pattern) collectionOrPat);
        } else if (collectionOrPat instanceof String) {
            out = containerFactory.get();
            out.add(Pattern.compile((String) collectionOrPat));
        } else {
            throw new IllegalArgumentException("Unknown pattern or pattern collection argument type: " +
                    collectionOrPat.getClass().getName());
        }
        return out;
    }

    public static List<Pattern> getPatterns(Object collectionOrPat) {
        return getPatterns(collectionOrPat, ArrayList::new);
    }

    public static <V> Set<V> excludedSet(Collection<? extends V> in, Collection<? extends V> excludes) {
        Set<V> set = (in != null) ? new LinkedHashSet<>(in) : new LinkedHashSet<>();
        if (excludes != null) {
            set.removeAll(excludes);
        }
        return set;
    }

    public static <V> Set<V> excludedSet(Collection<? extends V> in, V... excludes) {
        return excludedSet(in, Arrays.asList(excludes));
    }

    /**
     * Returns the size of the passed collection or map, or IllegalArgumentException.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static int size(Object coll) throws IllegalArgumentException {
        if (coll instanceof Collection) {
            return UtilGenerics.<Collection<?>>cast(coll).size();
        } else if (coll instanceof Map) {
            return UtilGenerics.<Map<?, Map>>cast(coll).size();
        } else if (coll == null) {
            throw new IllegalArgumentException("Missing container for size");
        } else {
            throw new IllegalArgumentException("Unknown container type for size: " + coll.getClass().getName());
        }
    }

    /**
     * Returns the size of the passed collection or map, or null instead of exception.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static Integer sizeOrNull(Object coll) {
        try {
            return size(coll);
        } catch(IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Collection returns as-is; map returns key set; otherwise IllegalArgumentException.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C keys(Object coll) throws IllegalArgumentException {
        if (coll instanceof Collection) {
            return UtilGenerics.cast(coll);
        } else if (coll instanceof Map) {
            return UtilGenerics.cast(UtilGenerics.<Map<?, ?>>cast(coll).keySet());
        } else if (coll == null) {
            throw new IllegalArgumentException("Missing container for size");
        } else {
            throw new IllegalArgumentException("Unknown container type for size: " + coll.getClass().getName());
        }
    }

    /**
     * Collection returns as-is; map returns key set; otherwise null.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C keysOrNull(Object coll) {
        try {
            return keys(coll);
        } catch(IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Collection returns as-is; map returns key set; otherwise immutable empty.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C keysOrEmpty(Object coll) {
        try {
            return keys(coll);
        } catch(IllegalArgumentException e) {
            return UtilGenerics.cast(Collections.emptySet());
        }
    }

    /**
     * Collection returns as-is; map returns values; otherwise IllegalArgumentException.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C values(Object coll) throws IllegalArgumentException {
        if (coll instanceof Collection) {
            return UtilGenerics.cast(coll);
        } else if (coll instanceof Map) {
            return UtilGenerics.cast(UtilGenerics.<Map<?, ?>>cast(coll).values());
        } else if (coll == null) {
            throw new IllegalArgumentException("Missing container for size");
        } else {
            throw new IllegalArgumentException("Unknown container type for size: " + coll.getClass().getName());
        }
    }

    /**
     * Collection returns as-is; map returns values; otherwise null.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C valuesOrNull(Object coll) {
        try {
            return values(coll);
        } catch(IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Collection returns as-is; map returns values; otherwise immutable empty.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static <C extends Collection<?>> C valuesOrEmpty(Object coll) {
        try {
            return values(coll);
        } catch(IllegalArgumentException e) {
            return UtilGenerics.cast(Collections.emptySet());
        }
    }
}
