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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.ofbiz.base.util.collections.MapComparator;

/**
 * UtilMisc - Misc Utility Functions
 */
public final class UtilMisc {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final BigDecimal ZERO_BD = BigDecimal.ZERO; // SCIPIO: 2018-08-30: keeping public for backward-compat

    private UtilMisc () {}

    public static final <T extends Throwable> T initCause(T throwable, Throwable cause) {
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
     * Create a map from passed nameX, valueX parameters
     * @return The resulting Map
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> toMap(Object... data) {
        if (data.length == 1 && data[0] instanceof Map) {
            return UtilGenerics.<K, V>checkMap(data[0]);
        }
        if (data.length % 2 == 1) {
            IllegalArgumentException e = new IllegalArgumentException("You must pass an even sized array to the toMap method (size = " + data.length + ")");
            Debug.logInfo(e, module);
            throw e;
        }
        Map<K, V> map = new HashMap<>();
        for (int i = 0; i < data.length;) {
            map.put((K) data[i++], (V) data[i++]);
        }
        return map;
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
        List<T> result = (col != null) ? new ArrayList<>(col) : new ArrayList<>(); // SCIPIO: switched to ArrayList
        //if (col != null) result.addAll(col);
        return result;
    }

    public static <K, V> Map<K, V> makeMapWritable(Map<K, ? extends V> map) {
        if (map == null) {
            return new HashMap<>();
        }
        Map<K, V> result = new HashMap<>(map.size());
        result.putAll(map);
        return result;
    }

    public static <T> Set<T> makeSetWritable(Collection<? extends T> col) {
        Set<T> result = new LinkedHashSet<>();
        if (col != null) {
            result.addAll(col);
        }
        return result;
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
            Collections.sort(toSort, mc);
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
            currentNumber = ZERO_BD;
        } else if (currentNumberObj instanceof BigDecimal) {
            currentNumber = (BigDecimal) currentNumberObj;
        } else if (currentNumberObj instanceof Double) {
            currentNumber = new BigDecimal((Double) currentNumberObj);
        } else if (currentNumberObj instanceof Long) {
            currentNumber = new BigDecimal((Long) currentNumberObj);
        } else {
            throw new IllegalArgumentException("In addToBigDecimalInMap found a Map value of a type not supported: " + currentNumberObj.getClass().getName());
        }

        if (addNumber == null || ZERO_BD.compareTo(addNumber) == 0) {
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
        Set<T> theSet = null;
        if (c instanceof Set<?>) {
            theSet = (Set<T>) c;
        } else {
            theSet = new LinkedHashSet<>();
            c.remove(null);
            theSet.addAll(c);
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
     * Create a set from the passed objects.
     * @param data
     * @return theSet
     */
    @SafeVarargs
    public static <T> Set<T> toSet(T... data) {
        if (data == null) {
            return null;
        }
        Set<T> theSet = new LinkedHashSet<>();
        for (T elem : data) {
            theSet.add(elem);
        }
        return theSet;
    }

    public static <T> Set<T> toSet(Collection<T> collection) {
        if (collection == null) {
            return null;
        }
        if (collection instanceof Set<?>) {
            return (Set<T>) collection;
        }
        Set<T> theSet = new LinkedHashSet<>();
        theSet.addAll(collection);
        return theSet;
    }

    public static <T> Set<T> toSetArray(T[] data) {
        if (data == null) {
            return null;
        }
        Set<T> set = new LinkedHashSet<>();
        for (T value: data) {
            set.add(value);
        }
        return set;
    }

    /**
     * SCIPIO: Create a HashSet from passed objX parameters
     * @return The resulting HashSet
     */
    @SafeVarargs
    public static <T> Set<T> toHashSet(T... obj) {
        return new HashSet<T>(Arrays.asList(obj));
    }

    /**
     * SCIPIO: Create a HashSet from collection
     * @return The resulting HashSet
     */
    public static <T> Set<T> toHashSet(Collection<? extends T> collection) {
        return new HashSet<T>(collection);
    }

    /**
     * Creates a list from passed objects.
     * @param data
     * @return list
     */
    @SafeVarargs
    public static <T> List<T> toList(T... data) {
        if(data == null){
            return null;
        }
        /* SCIPIO: switched to ArrayList
        List<T> list = new LinkedList<>();

        for(T t : data){
            list.add(t);
        }

        return list;
        */
        return new ArrayList<>(Arrays.asList(data));
    }

    public static <T> List<T> toList(Collection<T> collection) {
        if (collection == null) {
            return null;
        }
        if (collection instanceof List<?>) {
            return (List<T>) collection;
        }
        List<T> list = new ArrayList<>(collection); // SCIPIO: switched to ArrayList
        //collection.list.addAll(collection);
        return list;
    }

    public static <T> List<T> toListArray(T[] data) {
        if (data == null) {
            return null;
        }
        List<T> list = new ArrayList<T>(Arrays.asList(data)); // SCIPIO: switched to ArrayList
        //for (T value: data) {
        //    list.add(value);
        //}
        return list;
    }

    public static <K, V> void addToListInMap(V element, Map<K, Object> theMap, K listKey) {
        List<V> theList = UtilGenerics.checkList(theMap.get(listKey));
        if (theList == null) {
            theList = new ArrayList<>(); // SCIPIO: switched to ArrayList
            theMap.put(listKey, theList);
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

    /** The input can be a String, Locale, or even null and a valid Locale will always be returned; if nothing else works, returns the default locale.
     * @param localeObject An Object representing the locale
     */
    public static Locale ensureLocale(Object localeObject) {
        if (localeObject instanceof String) {
            Locale locale = parseLocale((String) localeObject);
            if (locale != null)  {
                return locale;
            }
        } else if (localeObject instanceof Locale) {
            return (Locale) localeObject;
        }
        return Locale.getDefault();
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
     * SCIPIO: Returns the first non-null value, or null if none.
     * <p>
     * Added 2018-07-12.
     */
    @SafeVarargs
    public static <T> T firstNonNull(T... values) {
        for(T value : values) {
            if (value != null) return value;
        }
        return null;
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE or "true", or
     * Boolean.FALSE if value is Boolean.FALSE or "false", or null if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static Boolean booleanValue(Object value) {
        if (value == null || value instanceof Boolean) {
            return (Boolean) value;
        } else if ("true".equals(value)) {
            return Boolean.TRUE;
        } else if ("false".equals(value)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE or "true", or
     * false if value is Boolean.FALSE or "false", or defaultValue if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static boolean booleanValue(Object value, boolean defaultValue) {
        Boolean res = booleanValue(value);
        return res != null ? res : defaultValue;
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE or "Y", or
     * Boolean.FALSE if value is Boolean.FALSE or "N", or null if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static Boolean booleanValueIndicator(Object value) {
        if (value == null || value instanceof Boolean) {
            return (Boolean) value;
        } else if ("Y".equals(value)) {
            return Boolean.TRUE;
        } else if ("N".equals(value)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE or "Y", or
     * false if value is Boolean.FALSE or "N", or defaultValue if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static boolean booleanValueIndicator(Object value, boolean defaultValue) {
        Boolean res = booleanValueIndicator(value);
        return res != null ? res : defaultValue;
    }

    /**
     * SCIPIO: Returns Boolean.TRUE if value is Boolean.TRUE, "true" or "Y", or
     * Boolean.FALSE if value is Boolean.FALSE, "false" or "N", or null if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static Boolean booleanValueVersatile(Object value) {
        if (value == null || value instanceof Boolean) {
            return (Boolean) value;
        } else if ("true".equals(value) || "Y".equals(value)) {
            return Boolean.TRUE;
        } else if ("false".equals(value) || "N".equals(value)) {
            return Boolean.FALSE;
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Returns true if value is Boolean.TRUE, "true" or "Y", or
     * false if value is Boolean.FALSE, "false" or "N", or defaultValue if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static boolean booleanValueVersatile(Object value, boolean defaultValue) {
        Boolean res = booleanValueVersatile(value);
        return res != null ? res : defaultValue;
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE or "Y", or
     * "N" if value is Boolean.FALSE or "N", or null if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static String indicatorValue(Object value) {
        if (value instanceof Boolean) {
            return ((Boolean) value) ? "Y" : "N";
        } else if ("Y".equals(value) || "N".equals(value)) {
            return (String) value;
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE or "Y", or
     * "N" if value is Boolean.FALSE or "N", or defaultValue if
     * anything else.
     */
    public static String indicatorValue(Object value, String defaultValue) {
        String res = indicatorValue(value);
        return (res != null) ? res : defaultValue;
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE, "true" or "Y", or
     * "N" if value is Boolean.FALSE, "false" or "N", or null if
     * anything else.
     * <p>
     * NOTE: case-sensitive.
     */
    public static String indicatorValueVersatile(Object value) {
        if (value instanceof Boolean) {
            return ((Boolean) value) ? "Y" : "N";
        } else if ("true".equals(value) || "Y".equals(value)) {
            return "Y";
        } else if ("false".equals(value) || "N".equals(value)) {
            return "N";
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Returns "Y" if value is Boolean.TRUE, "true" or "Y", or
     * "N" if value is Boolean.FALSE, "false" or "N", or defaultValue if
     * anything else.
     */
    public static String indicatorValueVersatile(Object value, String defaultValue) {
        String res = indicatorValueVersatile(value);
        return (res != null) ? res : defaultValue;
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set.
     * (We use this pattern constantly.)
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> unmodifiableHashSet(T... elems) {
        return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable hash set copied from the given collection.
     */
    public static <T> Set<T> unmodifiableHashSetCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableSet(new HashSet<T>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set.
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> unmodifiableLinkedHashSet(T... elems) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked hash set copied from the given collection.
     */
    public static <T> Set<T> unmodifiableLinkedHashSetCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable array list.
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> unmodifiableArrayList(T... elems) {
        return Collections.unmodifiableList(new ArrayList<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable array list copied from the given collection.
     */
    public static <T> List<T> unmodifiableArrayListCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableList(new ArrayList<>(collection));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list.
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> unmodifiableLinkedList(T... elems) {
        return Collections.unmodifiableList(new LinkedList<>(Arrays.asList(elems)));
    }

    /**
     * SCIPIO: Returns an unmodifiable linked list copied from the given collection.
     */
    public static <T> List<T> unmodifiableLinkedListCopy(Collection<? extends T> collection) {
        return Collections.unmodifiableList(new LinkedList<>(collection));
    }

    /**
     * SCIPIO: For an inMap with generics Map&lt;K, V&gt;, populates and returns the opposite mapping outMap, Map&lt;V, K&gt;
     * Added 2017-07-12.
     */
    public static <K, V> Map<V, K> putAllReverseMapping(Map<V, K> outMap, Map<? extends K, ? extends V> inMap) {
        for(Map.Entry<? extends K, ? extends V> entry : inMap.entrySet()) {
            outMap.put(entry.getValue(), entry.getKey());
        }
        return outMap;
    }

    /**
     * SCIPIO: Transfers the specified keys from inMap to outMap.
     * Added 2017-12-04.
     */
    public static <K, V> void putKeys(Map<K, V> outMap, Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
    }

    /**
     * SCIPIO: Transfers the specified keys from inMap to outMap.
     * Added 2017-12-04.
     */
    @SafeVarargs
    public static <K, V> void putKeys(Map<K, V> outMap, Map<? extends K, ? extends V> inMap, K... keys) {
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
    }

    /**
     * SCIPIO: Creates a hash map copy with specified keys.
     * Added 2017-12-04.
     */
    public static <K, V> Map<K, V> toHashMapWithKeys(Map<? extends K, ? extends V> inMap, Collection<K> keys) {
        Map<K, V> outMap = new HashMap<>();
        for(K key : keys) { outMap.put(key, inMap.get(key)); }
        return outMap;
    }

    /**
     * SCIPIO: Creates a hash map copy including specified keys.
     * Added 2017-12-04.
     */
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
    @SafeVarargs
    public static <K, V> Map<K, V> toLinkedHashMapWithKeysOrigOrder(Map<? extends K, ? extends V> inMap, K... keys) {
        return toLinkedHashMapWithKeysOrigOrder(inMap, new HashSet<>(Arrays.asList(keys)));
    }

    /**
     * SCIPIO: Creates a linked (ordered) hash map copy including specified keys, with the
     * key order determined by the order of the passed keys collection parameter.
     * Added 2017-12-04.
     */
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
}
