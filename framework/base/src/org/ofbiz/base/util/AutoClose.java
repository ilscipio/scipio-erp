package org.ofbiz.base.util;

import org.ofbiz.base.util.collections.MapContext;

import java.io.Closeable;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * SCIPIO: Facade class to register to a list of instances to be auto-closed at a certain event, such as {@link MapContext#pop()} calls.
 * Helps fix the EntityListIterator closing problems.
 * <p>
 * NOTE: Originally this was a nested class of {@link UtilIO}, but it needed to be available easily from the
 * org.ofbiz.base.util package.
 */
public final class AutoClose implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private AutoClose() {}

    /**
     * Registers the given {@link AutoCloseable} or {@link Closeable} in a special context list field, and it will be
     * closed when appropriate (by whatever code is handling the context, e.g. widget renderer section pop).
     */
    public static <K, V> void register(Map<K, V> context, String listFieldName, AutoCloseable closeableObject) {
        register(context, listFieldName, (Object) closeableObject);
    }

    /**
     * Registers a closeable object (internal) - DEV NOTE: Do not expose this for the time being! There is no reason to accept anything other
     * than AutoCloseable for now ({@link #register(Map, String, AutoCloseable)}), but there are other possibilities...
     */
    private static <K, V> void register(Map<K, V> context, String listFieldName, Object closeableObject) {
        List<Object> autoCloseList = getList(context, listFieldName);
        for(Object autoCloseObject : autoCloseList) {
            if (autoCloseObject == closeableObject) {
                return;
            }
        }
        autoCloseList.add(closeableObject);
    }

    /**
     * Returns the list of registered {@link AutoCloseable} or {@link Closeable} to be auto-closed when appropriate;
     * if none exists, creates a new one in the context (unsafe for read-only operations).
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<Object> getList(Map<K, V> context, String listFieldName) {
        List<Object> autoCloseList = getListIfSet(context, listFieldName);
        if (autoCloseList == null) {
            autoCloseList = new ArrayList<>();
            ((Map<String, Object>) context).put(listFieldName, autoCloseList);
        }
        return autoCloseList;
    }

    /**
     * Returns the list of registered {@link AutoCloseable} or {@link Closeable} to be auto-closed when appropriate,
     * if it is defined in context, or null otherwise.
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<Object> getListIfSet(Map<K, V> context, String listFieldName) {
        return (List<Object>) context.get(listFieldName);
    }

    public static <K, V> List<Object> getListUnique(Map<K, V> context, String listFieldName) {
        return getListUniqueIfSet(getList(context, listFieldName));
    }

    public static <K, V> List<Object> getListUniqueIfSet(Map<K, V> context, String listFieldName) {
        return getListUniqueIfSet(getListIfSet(context, listFieldName));
    }

    public static List<Object> getListUniqueIfSet(List<?> autoCloseList) {
        if (autoCloseList == null) {
            return null;
        }
        List<Object> autoCloseListUnique = new ArrayList<>(autoCloseList.size());
        for(Object autoCloseObject : autoCloseList) {
            if (!isRegistered(autoCloseList, autoCloseObject)) {
                autoCloseListUnique.add(autoCloseObject);
            }
        }
        return autoCloseListUnique;
    }

    public static <K, V> boolean isRegistered(Map<K, V> context, String listFieldName, Object object) {
        return isRegistered(getListIfSet(context, listFieldName), object);
    }

    public static boolean isRegistered(List<?> autoCloseList, Object object) {
        if (autoCloseList == null) {
            return false;
        }
        for(Object autoCloseObject : autoCloseList) {
            if (autoCloseObject == object) {
                return true;
            }
        }
        return false;
    }

    /**
     * Empties the auto-close list registered in context *and* removes its context field.
     */
    @SuppressWarnings("unchecked")
    public static <K, V> void clear(Map<K, V> context, String listFieldName) {
        // not necessary: better to avoid modifying the map itself, and this was less consistent
        //clear((List<Object>) context.remove(listFieldName));
        clear((List<Object>) context.get(listFieldName));
    }

    /**
     * Empties the auto-close list.
     */
    public static void clear(List<?> autoCloseList) {
        if (autoCloseList != null) {
            autoCloseList.clear();
        }
    }

    /**
     * Calls the {@link AutoCloseable#close()} method on all the registered auto-close {@link AutoCloseable} and
     * {@link Closeable} instances. All exceptions are caught and logged.
     */
    public static <K, V> void close(Map<K, V> context, String listFieldName) {
        close(getListIfSet(context, listFieldName));
    }

    public static void close(List<?> autoCloseList) {
        closeIntrn(autoCloseList, null);
    }

    private static void closeIntrn(List<?> autoCloseList, String listFieldName) {
        if (autoCloseList == null) {
            // NOTE: This will happen frequently and flood the log with... no useful information whatsoever.
            //if (Debug.verboseOn()) {
            //    Debug.logVerbose("AutoClose.close() called: null list", module);
            //}
            return;
        }
        List<Object> autoCloseListUnique = getListUniqueIfSet(autoCloseList);
        if (Debug.verboseOn() && !autoCloseList.isEmpty()) { // NOTE: isEmpty() not frequent like null, but useless information nonetheless
            Debug.logVerbose("AutoClose.close() called for field [" + (listFieldName != null ? listFieldName : "unknown")
                    + ": closing " + autoCloseList.size() + " objects", module);
        }
        int i = 0;
        for(Object object : autoCloseListUnique) {
            i++;
            if (object instanceof AutoCloseable) {
                try {
                    ((AutoCloseable) object).close();
                } catch (Exception e) {
                    Debug.logError(e, "Error closing context auto-close object [" + (listFieldName != null ? listFieldName : "unknown")
                            + "] (" + i + "/" + autoCloseListUnique.size() + "): " + e.toString(), module);
                }
            }
        }
    }

    /**
     * Calls the {@link AutoCloseable#close()} method on all the registered auto-close {@link AutoCloseable} and
     * {@link Closeable} instances, and then clears the auto-close list. All exceptions are caught and logged.
     */
    public static <K, V> void closeAndClear(Map<K, V> context, String listFieldName) {
        close(context, listFieldName);
        clear(context, listFieldName);
    }

    public static void closeAndClear(List<?> autoCloseList) {
        close(autoCloseList);
        clear(autoCloseList);
    }
}
