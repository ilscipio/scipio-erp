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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.ServiceLoader;

import org.ofbiz.base.lang.Factory;
import org.ofbiz.base.lang.SourceMonitored;

/**
 * Object and reflection utilities.
 */
@SourceMonitored
public final class UtilObject {

    private UtilObject() {
    }

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Casts object to given type.
     *
     * <p>DEV NOTE: This was the correct location for this call because it object has nothing to do with generics or collections
     * and is a primitive language feature.</p>
     *
     * <p>SCIPIO: 3.0.0: Migrated from {@link UtilGenerics#cast(Object)} as more succinct and UtilGenerics is headed toward deprecation.</p>
     */
    @SuppressWarnings("unchecked")
    private static <V> V cast(Object object) {
        return (V) object;
    }

    /**
     * If the given name value is a string starting with an uppercase character ("MY_VALUE"), returns it as-is, otherwise applies
     * a project-standard name conversion heuristic, which consists of uppercasing and substituting "-" with "_" ("my-value" -> "MY_VALUE").
     *
     * <p>If name is null or empty, returns as-is (this method cannot check the validity of the name anyway),
     * because in some cases empty string is significant (use nullIfEmpty).</p>
     *
     * <p>The result is intended for use with {@link Enum#valueOf(Class, String)} but does not guarantee a valid
     * value since there is no class parameter.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static String constantName(Object name) {
        if (name == null) {
            return null;
        }
        String nameStr = (String) name;
        if (nameStr.isEmpty()) {
            return "";
        }
        char first = nameStr.charAt(0);
        if (first >= 'A' && first <= 'Z') {
            return nameStr;
        } else {
            return nameStr.replace('-', '_').toUpperCase();
        }
    }

    /**
     * If the given name value is a string starting with a lowercase character ("my-value"), returns it as-is, otherwise applies
     * a project-standard name conversion heuristic, which consists of lowercasing and substituting "_" with "-" ("MY_VALUE" -> "my-value").
     *
     * <p>If name is null or empty, returns as-is (this method cannot check the validity of the name anyway),
     * because in some cases empty string is significant (use nullIfEmpty).</p>
     *
     * <p>This can be used to convert an enum value to an attribute value.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static String attrName(Object name) {
        if (name == null) {
            return null;
        }
        String nameStr = (String) name;
        if (nameStr.isEmpty()) {
            return "";
        }
        char first = nameStr.charAt(0);
        if (first >= 'a' && first <= 'z') {
            return nameStr;
        } else {
            return nameStr.replace('_', '-').toLowerCase();
        }
    }

    /**
     * If the given name value is a string starting with an uppercase character, returns the given corresponding enum value,
     * otherwise applies a project-standard name conversion heuristic, which consists of lowercasing and substituting "-" with "_",
     * and returns the given corresponding enum.
     *
     * <p>The result is intended as drop-in replacement for {@link Enum#valueOf(Class, String)}.</p>
     *
     * <p>If enum name is null or empty, returns null, but if the value is non-empty and invalid, IllegalArgumentException is thrown.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static <E extends Enum<E>> E enumValue(Class<E> cls, Object name) throws IllegalArgumentException {
        String enumValueName = constantName(name);
        if (enumValueName == null || enumValueName.isEmpty()) {
            return null;
        }
        return Enum.valueOf(cls, enumValueName);
    }


    public static byte[] getBytes(InputStream is) {
        byte[] buffer = new byte[4 * 1024];
        byte[] data = null;
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            try {

                int numBytesRead;
                while ((numBytesRead = is.read(buffer)) != -1) {
                    bos.write(buffer, 0, numBytesRead);
                }
                data = bos.toByteArray();
            } finally {
                bos.close();
            }
        } catch (IOException e) {
            Debug.logError(e, module);
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (IOException e) {
                Debug.logError(e, module);
            }
        }

        return data;
    }

    /** Serialize an object to a byte array */
    public static byte[] getBytes(Object obj) {
        try {
            return getBytesOrEx(obj); // SCIPIO: 2019-03-08: Refactored, delegated
        } catch (IOException e) {
            Debug.logError(e, module);
        }
        return null;
    }

    /** SCIPIO: Serialize an object to a byte array, or exception. Refactored 2019-03-08 from {@link #getBytes(Object)}. */
    public static byte[] getBytesOrEx(Object obj) throws IOException {
        byte[] data = null;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        IOException currentEx = null;
        try {
            ObjectOutputStream oos = new ObjectOutputStream(bos);
            try {
                oos.writeObject(obj);
                data = bos.toByteArray();
            } catch (IOException e) {
                currentEx = e;
            } finally {
                try {
                    oos.flush();
                } catch(IOException e) {
                    currentEx = handleSecondaryException("Error flushing ObjectOutputStream", e, currentEx);
                }
                try {
                    oos.close();
                } catch(IOException e) {
                    currentEx = handleSecondaryException("Error closing ObjectOutputStream", e, currentEx);
                }
            }
        } finally {
            try {
                bos.close();
            } catch(IOException e) {
                currentEx = handleSecondaryException("Error closing ByteArrayOutputStream", e, currentEx);
            }
        }
        if (currentEx != null) {
            throw currentEx;
        }
        return data;
    }

    private static <E extends Exception> E handleSecondaryException(String msg, E ex, E currentEx) { // SCIPIO
        if (currentEx == null) {
            return ex;
        }
        if (Debug.verboseOn()) {
            Debug.logError(msg + ": " + ex.toString(), module);
        }
        return currentEx;
    }

    /** Returns the size of a serializable object. Non-serializable objects
     * will throw an <code>IOException</code>.<p>It is important to note
     * that the returned value is length of the byte stream after the object has
     * been serialized. The returned value does not represent the amount of memory
     * the object uses. There is no accurate way to determine the size of an
     * object in memory.</p>
     * @param obj
     * @return the number of bytes in the serialized object
     * @throws IOException
     */
    public static long getByteCount(Object obj) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(bos);
        oos.writeObject(obj);
        oos.flush();
        long size = bos.size();
        bos.close();
        return size;
    }

    /** Deserialize a byte array back to an object; if there is an error, it is logged, and null is returned. */
    public static Object getObject(byte[] bytes) {
        Object obj = null;
        try {
            obj = getObjectException(bytes);
        } catch (ClassNotFoundException | IOException e) {
            Debug.logError(e, module);
        }
        return obj;
    }

    /** Deserialize a byte array back to an object */
    public static Object getObjectException(byte[] bytes) throws ClassNotFoundException, IOException {
        ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
        try {
            ObjectInputStream ois = new ObjectInputStream(bis, Thread.currentThread().getContextClassLoader());
            try {
                return ois.readObject();
            } finally {
                ois.close();
            }
        } finally {
            bis.close();
        }
    }

    public static boolean equalsHelper(Object o1, Object o2) {
        if (o1 == o2) {
            // handles same-reference, or null
            return true;
        } else if (o1 == null || o2 == null) {
            // either o1 or o2 is null, but not both
            return false;
        } else {
            return o1.equals(o2);
        }
    }

    public static <T> int compareToHelper(Comparable<T> o1, T o2) {
        if (o1 == o2) {
            // handles same-reference, or null
            return 0;
        } else if (o1 == null) {
            return -1;
        } else if (o2 == null) {
            // either o1 or o2 is null, but not both
            return 1;
        } else {
            return o1.compareTo(o2);
        }
    }

    public static int doHashCode(Object o1) {
        if (o1 == null) {
            return 0;
        }
        if (o1.getClass().isArray()) {
            int length = Array.getLength(o1);
            int result = 0;
            for (int i = 0; i < length; i++) {
                result += doHashCode(Array.get(o1, i));
            }
            return result;
        }
        return o1.hashCode();
    }

    public static <A, R> R getObjectFromFactory(Class<? extends Factory<R, A>> factoryInterface, A obj) throws ClassNotFoundException {
        Iterator<? extends Factory<R, A>> it = ServiceLoader.load(factoryInterface).iterator();
        while (it.hasNext()) {
            Factory<R, A> factory = it.next();
            R instance = factory.getInstance(obj);
            if (instance != null) {
                return instance;
            }
        }
        // SCIPIO: 2019-04-03: stock bug
        //throw new ClassNotFoundException(factoryInterface.getClass().getName());
        throw new ClassNotFoundException(factoryInterface.getName());
    }

    /**
     * SCIPIO: Returns the first non-null value, or null.
     */
    @SafeVarargs
    public static <T> T firstNonNull(T... values) {
        for(T value : values) {
            if (value != null) return value;
        }
        return null;
    }

    /**
     * SCIPIO: Returns the first non-null value, or null.
     */
    public static <T> T firstNonNull(Collection<?> values) {
        for(Object value : values) {
            if (value != null) {
                return UtilGenerics.cast(value);
            }
        }
        return null;
    }

    /**
     * SCIPIO: If the object is null, returns the altValue; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T altIfNull(T value, T altValue) {
        return (value == null) ? altValue : value;
    }

    /**
     * SCIPIO: If the object is non-null, returns the altValue; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T altIfNotNull(T value, T altValue) {
        return (value != null) ? altValue : value;
    }

    /**
     * SCIPIO: If the object is equal to the target, returns null; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T nullIfEquals(T value, Object target) {
        return (value != null) ? (value.equals(target) ? null : value) : null;
    }

    /**
     * SCIPIO: If the object is not equal to the target, returns null; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T nullIfNotEquals(T value, Object target) {
        return (value != null) ? (!value.equals(target) ? null : value) : null;
    }

    /**
     * SCIPIO: If the object is equal to the target, returns the altValue; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T altIfEquals(T value, Object target, T altValue) {
        return (value != null) ? (value.equals(target) ? altValue : value) : ((value == altValue) ? altValue : value);
    }

    /**
     * SCIPIO: If the object is not equal to the target, returns the altValue; in all other cases returns the original value.
     * <p>
     * Acts as a convenient filter to minimize the number of intermediate variables and
     * repeated container accesses, much like the {@link Objects} methods.
     */
    public static <T> T altIfNotEquals(T value, Object target, T altValue) {
        return (value != null) ? (!value.equals(target) ? altValue : value) : ((value != altValue) ? altValue : value);
    }

    /** SCIPIO: Returns object toString() or null if null. */
    public static String toStringOrNull(Object object) {
        return (object != null) ? object.toString() : null;
    }

    /** SCIPIO: Returns object toString() or empty string if null. */
    public static String toStringOrEmpty(Object object) {
        return (object != null) ? object.toString() : "";
    }
}
