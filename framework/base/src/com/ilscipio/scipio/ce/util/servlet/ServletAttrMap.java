package com.ilscipio.scipio.ce.util.servlet;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilMisc;

import com.ilscipio.scipio.ce.util.collections.ExtendedMap;

/**
 * SCIPIO: A convenient combination of the ServletAttributeContainer and ExtendedMap interfaces.
 */
public interface ServletAttrMap extends ExtendedMap<String, Object>, ServletAttrContainer {

    /*
     * ******************************************************************************
     * Primary API interfaces
     * ******************************************************************************
     */

    public interface ServletMapAdapter extends ServletAttrMap, ServletAttrContainer.Adapter {
    }

    public interface RequestMapAdapter extends ServletMapAdapter, ServletAttrContainer.RequestAdapter {
    }

    public interface SessionMapAdapter extends ServletMapAdapter, ServletAttrContainer.SessionAdapter {
    }

    public interface ServletContextMapAdapter extends ServletMapAdapter, ServletAttrContainer.ServletContextAdapter {
    }

    /*
     * ******************************************************************************
     * Factory methods
     * ******************************************************************************
     */

    /**
     * Returns a full-featured, best-possible-correctness request map adapter.
     * <p>
     * WARN: It is possible that some of the {@link java.util.Map} interface contract is
     * not fully adhered, so this is meant to try best-possible effort at correctness.
     */
    public static RequestMapAdapter getAdapter(ServletRequest request) {
        return new StdRequestMapAdapter(request);
    }

    /**
     * Returns a full-featured, best-possible-correctness session map adapter.
     * <p>
     * WARN: It is possible that some of the {@link java.util.Map} interface contract is
     * not fully adhered, so this is meant to try best-possible effort at correctness.
     */
    public static SessionMapAdapter getAdapter(HttpSession session) {
        return new StdSessionMapAdapter(session);
    }

    /**
     * Returns a full-featured, best-possible-correctness servlet context map adapter.
     * <p>
     * WARN: It is possible that some of the {@link java.util.Map} interface contract is
     * not fully adhered, so this is meant to try best-possible effort at correctness.
     */
    public static ServletContextMapAdapter getAdapter(ServletContext servletContext) {
        return new StdServletContextMapAdapter(servletContext);
    }

    /**
     * Returns an optimized request map adapter <strong>that makes several {@link java.util.Map}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>Map modification methods such as {@link #put} and {@link #remove}
     *     break the Map contract and do NOT return the previous/old value; they return null or a dummy default instead.</li>
     * <li>{@link #keySet}, {@link #values()} and {@link entrySet} are considered unmodifiable and may
     *     either throw exception or simply fail without notice at any modification attempt to the elements;
     *     they may also be either adapted or copied collections, and change without notice.</li>
     * </ul>
     */
    public static RequestMapAdapter getFastAdapter(ServletRequest request) {
        return new FastRequestMapAdapter(request);
    }

    /**
     * Returns an optimized session map adapter <strong>that makes several {@link java.util.Map}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>Map modification methods such as {@link #put} and {@link #remove}
     *     break the Map contract and do NOT return the previous/old value; they return null or a dummy default instead.</li>
     * <li>{@link #keySet}, {@link #values()} and {@link entrySet} are considered unmodifiable and may
     *     either throw exception or simply fail without notice at any modification attempt to the elements;
     *     they may also be either adapted or copied collections, and change without notice.</li>
     * </ul>
     */
    public static SessionMapAdapter getFastAdapter(HttpSession session) {
        return new FastSessionMapAdapter(session);
    }

    /**
     * Returns an optimized servlet context map adapter <strong>that makes several {@link java.util.Map}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>Map modification methods such as {@link #put} and {@link #remove}
     *     break the Map contract and do NOT return the previous/old value; they return null or a dummy default instead.</li>
     * <li>{@link #keySet}, {@link #values()} and {@link entrySet} are considered unmodifiable and may
     *     either throw exception or simply fail without notice at any modification attempt to the elements;
     *     they may also be either adapted or copied collections, and change without notice.</li>
     * </ul>
     */
    public static ServletContextMapAdapter getFastAdapter(ServletContext servletContext) {
        return new FastServletContextMapAdapter(servletContext);
    }

    /*
     * ******************************************************************************
     * Servlets implementations
     * ******************************************************************************
     */

    public abstract class StdServletMapAdapter<T> extends StdServletExtendedMapAdapter implements ServletMapAdapter {
        private final T adapted;
        protected StdServletMapAdapter(T adapted) {
            this.adapted = adapted;
        }

        @Override
        public T getAdapted() { return adapted; }
        @Override
        protected ServletAttrContainer getContainer() { return this; }
    }

    public static class StdRequestMapAdapter extends StdServletMapAdapter<ServletRequest> implements ServletAttrContainer.RequestAdapter, RequestMapAdapter {
        StdRequestMapAdapter(ServletRequest adapted) {
            super(adapted);
        }
    }

    public static class StdSessionMapAdapter extends StdServletMapAdapter<HttpSession> implements ServletAttrContainer.SessionAdapter, SessionMapAdapter {
        StdSessionMapAdapter(HttpSession adapted) {
            super(adapted);
        }
    }

    public static class StdServletContextMapAdapter extends StdServletMapAdapter<ServletContext> implements ServletAttrContainer.ServletContextAdapter, ServletContextMapAdapter {
        StdServletContextMapAdapter(ServletContext adapted) {
            super(adapted);
        }
    }

    public abstract class FastServletMapAdapter<T> extends FastServletExtendedMapAdapter implements ServletMapAdapter {
        private final T adapted;
        protected FastServletMapAdapter(T adapted) {
            this.adapted = adapted;
        }

        @Override
        public T getAdapted() { return adapted; }
        @Override
        protected ServletAttrContainer getContainer() { return this; }
    }

    public static class FastRequestMapAdapter extends FastServletMapAdapter<ServletRequest> implements ServletAttrContainer.RequestAdapter, RequestMapAdapter {
        protected FastRequestMapAdapter(ServletRequest adapted) {
            super(adapted);
        }
    }

    public static class FastSessionMapAdapter extends FastServletMapAdapter<HttpSession> implements ServletAttrContainer.SessionAdapter, SessionMapAdapter {
        protected FastSessionMapAdapter(HttpSession adapted) {
            super(adapted);
        }
    }

    public static class FastServletContextMapAdapter extends FastServletMapAdapter<ServletContext> implements ServletAttrContainer.ServletContextAdapter, ServletContextMapAdapter {
        protected FastServletContextMapAdapter(ServletContext adapted) {
            super(adapted);
        }
    }

    /*
     * ******************************************************************************
     * ExtendedMap adapters
     * ******************************************************************************
     */

    /**
     * ExtendedMap interface method default implementations.
     */
    static abstract class StdServletExtendedMapAdapter extends AbstractMap<String, Object> implements ExtendedMap<String, Object> {

        protected abstract ServletAttrContainer getContainer();

        @Override
        public int size() {
            int size = 0;
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements(); size++) {
            }
            return size;
        }

        @Override
        public boolean isEmpty() {
            return !getContainer().getAttributeNames().hasMoreElements();
        }

        @Override
        public boolean containsKey(Object key) {
            try {
                return getContainer().containsAttribute((String) key);
            } catch(ClassCastException e) {
                return false;
            }
        }

        @Override
        public boolean containsValue(Object value) {
            if (value != null) {
                for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                    if (value.equals(getContainer().getAttribute(attrNames.nextElement()))) {
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public Object get(Object key) {
            try {
                return getContainer().getAttribute((String) key);
            } catch(ClassCastException e) {
                return null;
            }
        }

        @Override
        public Object put(String key, Object value) {
            Object oldValue = getContainer().getAttribute((String) key);
            getContainer().setAttribute(key, value);
            return oldValue;
        }

        @Override
        public void putOnly(String key, Object value) {
            getContainer().setAttribute(key, value);
        }

        @Override
        public Object remove(Object key) {
            try {
                Object oldValue = getContainer().getAttribute((String) key);
                getContainer().removeAttribute((String) key);
                return oldValue;
            } catch(ClassCastException e) {
            }
            return null;
        }

        @Override
        public void removeOnly(Object key) {
            try {
                getContainer().removeAttribute((String) key);
            } catch(ClassCastException e) {
            }
        }

        @Override
        public void putAll(Map<? extends String, ? extends Object> m) {
            for(Map.Entry<? extends String, ? extends Object> entry : m.entrySet()) {
                getContainer().setAttribute(entry.getKey(), entry.getValue());
            }
        }

        @Override
        public void clear() {
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                getContainer().removeAttribute(attrNames.nextElement());
            }
        }

        @Override
        public Set<String> keySetCopy() { return getContainer().getAttributeNamesSet(); }

        @Override
        public List<String> keyListCopy() { return getContainer().getAttributeNamesList(); }

        @Override
        public Collection<Object> valuesCopy() { return valuesListCopy(); }

        @Override
        public List<Object> valuesListCopy() {
            List<Object> copy = UtilMisc.newList();
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                copy.add(getContainer().getAttribute(attrNames.nextElement()));
            }
            return copy;
        }

        @Override
        public Set<Map.Entry<String, Object>> entrySet() { // WARN: FIXME: Not backed by servlet attributes! Unmodifiable for now...
            //return Collections.unmodifiableSet(entrySetCopy());
            return new ServletAttributeEntrySet();
        }

        @Override
        public Set<Map.Entry<String, Object>> entrySetCopy() {
            Set<Map.Entry<String, Object>> entrySet = new HashSet<>();
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                String attrName = attrNames.nextElement();
                entrySet.add(new AbstractMap.SimpleEntry<String, Object>(attrName, getContainer().getAttribute(attrName)));
            }
            return entrySet;
        }

        @Override
        public List<Map.Entry<String, Object>> entryListCopy() {
            List<Map.Entry<String, Object>> entrySet = new ArrayList<>();
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                String attrName = attrNames.nextElement();
                entrySet.add(new AbstractMap.SimpleEntry<String, Object>(attrName, getContainer().getAttribute(attrName)));
            }
            return entrySet;
        }

        @Override
        public Map<String, Object> mapCopy() {
            Map<String, Object> copy = UtilMisc.newMap();
            for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                String attrName = attrNames.nextElement();
                copy.put(attrName, getContainer().getAttribute(attrName));
            }
            return copy;
        }

        private class ServletAttributeEntrySet extends AbstractSet<Map.Entry<String, Object>> implements Set<Map.Entry<String, Object>> {

            protected final List<EntrySet> entries;

            ServletAttributeEntrySet() {
                List<EntrySet> entries = new ArrayList<>();
                for(Enumeration<String> attrNames = getContainer().getAttributeNames(); attrNames.hasMoreElements();) {
                    String attrName = attrNames.nextElement();
                    entries.add(new EntrySet(attrName, getContainer().getAttribute(attrName)));
                }
                this.entries = entries;
            }

            @Override
            public int size() {
                return entries.size();
            }

            @Override
            public Iterator<Map.Entry<String, Object>> iterator() {
                return new EntrySetIterator();
            }

            @Override
            public boolean add(Map.Entry<String, Object> e) { // NOTE: This is probably unused
                boolean contained = getContainer().containsAttribute(e.getKey());
                if (!contained && e.getValue() != null) {
                    getContainer().setAttribute(e.getKey(), e.getValue());
                    entries.add(new EntrySet(e));
                }
                return contained;
            }

            @Override
            public boolean remove(Object o) {
                try {
                    return removeEntry((String) o) != null;
                } catch(ClassCastException e) {
                    return false;
                }
            }

            private Object removeEntry(String name) {
                for(int i = 0; i < entries.size(); i++) {
                    if (name.equals(entries.get(i).getKey())) {
                        return entries.remove(i).getValue();
                    }
                }
                return null;
            }

            @SuppressWarnings("serial")
            private class EntrySet extends AbstractMap.SimpleEntry<String, Object> {
                EntrySet(String key, Object value) {
                    super(key, value);
                }

                EntrySet(Map.Entry<? extends String, ? extends Object> entry) {
                    super(entry);
                }

                @Override
                public Object setValue(Object value) {
                    getContainer().setAttribute(getKey(), value);
                    return (value != null) ? super.setValue(value) : removeEntry(getKey());
                }
            }

            private class EntrySetIterator implements Iterator<Map.Entry<String, Object>> {
                private int listIndex = -1;

                @Override
                public boolean hasNext() {
                    int size = entries.size();
                    return (size > 0) && (listIndex < size);
                }

                @Override
                public Map.Entry<String, Object> next() {
                    if (listIndex < 0) {
                        listIndex = 0;
                    }
                    if (listIndex < entries.size()) {
                        return entries.get(listIndex++);
                    } else {
                        throw new NoSuchElementException();
                    }
                }

                @Override
                public void remove() {
                    if (listIndex < 0) {
                        throw new IllegalStateException("remove() called without a prior call to next()");
                    }

                    listIndex--;
                    if (listIndex < 0) {
                        listIndex = 0;
                    }
                    if (listIndex < entries.size()) {
                        getContainer().removeAttribute(entries.get(listIndex).getKey());
                        entries.remove(listIndex);
                    } else {
                        throw new IllegalStateException("no next element");
                    }
                }
            }
        }
    }

    /**
     * ExtendedMap interface method for "fast" implementations.
     * <strong>WARNING:</strong> These implementations <em>intentionally</em> violate the {@link #Map} interface!
     * <p>
     * <ul>Characteristics:</ul>
     * <li>Map modification methods such as {@link #put} and {@link #remove}
     *     break the Map contract and do NOT return the previous/old value; they return null or a dummy default instead.</li>
     * <li>{@link #keySet}, {@link #values()} and {@link entrySet} are considered unmodifiable and may
     *     either throw exception or simply fail without notice at any modification attempt to the elements;
     *     they may also be either adapted or copied collections, and change without notice.</li>
     */
    static abstract class FastServletExtendedMapAdapter extends StdServletExtendedMapAdapter {
        @Override
        public Object put(String key, Object value) {
            putOnly(key, value);
            return null;
        }

        @Override
        public Object remove(Object key) {
            removeOnly(key);
            return null;
        }

        /* This is actually slower
        @Override
        public Set<Entry<String, Object>> entrySet() {
            return entrySetCopy();
        }
        */

        @Override
        public Set<String> keySet() {
            return keySetCopy();
        }

        @Override
        public Collection<Object> values() {
            return valuesCopy();
        }
    }
}
