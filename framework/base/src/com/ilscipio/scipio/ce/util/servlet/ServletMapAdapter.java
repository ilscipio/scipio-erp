package com.ilscipio.scipio.ce.util.servlet;

import org.ofbiz.base.util.UtilHttp;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpSession;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;

/**
 * Convenient {@link Map} interface wrappers around {@link ServletRequest} request attributes and parameters,
 * {@link HttpSession} session attributes and {@link ServletContext} servlet context attributes.
 * <p>This is a simpler design to replace {@link ServletAttrMap} with fewer wrapper implementations to standardize
 * usage away from servlet API-specific code toward simple types like Map and List.</p>
 */
public interface ServletMapAdapter extends Map<String, Object> {

    /**
     * Returns a {@link ServletMapAdapter} equivalent to an unmodifiable map.
     * Mainly for compabitility purposes and for the interface.
     * @see Collections#unmodifiableMap(Map)
     */
    static ServletMapAdapter empty() {
        return EmptyAttributeMapAdapter.getInstance();
    }

    /**
     * Wraps request attributes in a {@link Map} wrapper.
     * <p>The request and wrapper are non-synchronized.</p>
     */
    static ServletMapAdapter wrapAttributes(ServletRequest request) {
        return new RequestAttributeMapAdapter(request);
    }

    /**
     * Wraps request attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     */
    static ServletMapAdapter wrapAttributesReadonly(ServletRequest request) {
        return new ReadonlyRequestAttributeMapAdapter(request);
    }

    /**
     * Wraps session attributes in an explicitly synchronized and thread-safe {@link Map} wrapper.
     * @see #wrapAttributesSync(HttpSession)
     */
    static ServletMapAdapter wrapAttributes(HttpSession session) {
        return wrapAttributesUnsync(session);
    }

    /**
     * Wraps session attributes in an explicitly synchronized and thread-safe {@link Map} wrapper.
     */
    static ServletMapAdapter wrapAttributesSync(HttpSession session) {
        if (session == null) {
            return empty();
        }
        // TODO
        //return new SyncSessionAttributeMapAdapter(session);
        throw new UnsupportedOperationException();
    }

    /**
     * Wraps session attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     * <p>Designed for thread-safe operation using immutable copies.</p>
     */
    static ServletMapAdapter wrapAttributesReadonly(HttpSession session) {
        if (session == null) {
            return empty();
        }
        return new ReadonlySessionAttributeMapAdapter(session);
    }

    /**
     * Wraps session attributes in a possibly non-synchronized {@link Map} wrapper intended to leave synchronization
     * to the caller.
     * @see #wrapAttributesSync(HttpSession)
     */
    static ServletMapAdapter wrapAttributesUnsync(HttpSession session) {
        if (session == null) {
            return empty();
        }
        return new SessionAttributeMapAdapter(session);
    }

    /**
     * Wraps servlet context attributes in an explicitly thread-safe {@link Map} wrapper.
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapAdapter wrapAttributes(ServletContext servletContext) {
        return wrapAttributesUnsync(servletContext);
    }

    /**
     * Wraps servlet context attributes in an explicitly thread-safe {@link Map} wrapper.
     */
    static ServletMapAdapter wrapAttributesSync(ServletContext servletContext) {
        // TODO
        //return new SyncServletContextAttributeMapAdapter(servletContext);
        throw new UnsupportedOperationException();
    }

    /**
     * Wraps servlet context attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     * <p>Designed for thread-safe operation using immutable copies.</p>
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapAdapter wrapAttributesReadonly(ServletContext servletContext) {
        return new ReadonlyServletContextAttributeMapAdapter(servletContext);
    }

    /**
     * Wraps servlet context attributes in a possibly non-synchronized {@link Map} wrapper intended to leave synchronization
     * to the caller.
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapAdapter wrapAttributesUnsync(ServletContext servletContext) {
        return new ServletContextAttributeMapAdapter(servletContext);
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper returns {@link String} for single parameters and {@link List}
     * for parameters with multiple values (legacy system default).
     * <p>The request parameters and wrapper are read-only.</p>
     * @see #wrapParametersDynamic(ServletRequest)
     */
    static ServletMapAdapter wrapParameters(ServletRequest request, Object parameterMode) {
        return new RequestParameterMapAdapter(request, parameterMode);
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper that only calls {@link ServletRequest#getParameter(String)}
     * and returns only the first value for all parameter names.
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapAdapter wrapParametersSingle(ServletRequest request) {
        return wrapParameters(request, "SINGLE");
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper that only calls {@link ServletRequest#getParameterValues(String)}
     * and always returns {@link List} as map values.
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapAdapter wrapParametersMulti(ServletRequest request) {
        return wrapParameters(request, "MULTI");
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper returns {@link String} for single parameters and {@link List}
     * for parameters with multiple values (legacy system default).
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapAdapter wrapParametersDynamic(ServletRequest request) {
        return wrapParameters(request, "DYNAMIC");
    }

    /**
     * Given a {@link Map} implemented using a ServletMapAdapter implementation, return its wrapped container
     * in the desired type.
     * <p>DEV NOTE: This is a static method to avoid adding public methods to the implementations, to avoid
     * problems with the bean engines.</p>
     */
    static <T> T getContainer(Object container) {
        if (container instanceof ContainerMapAdapter) {
            @SuppressWarnings("unchecked")
            T result = (T) ((ContainerMapAdapter) container).getContainer();
            return result;
        } else {
            throw new IllegalArgumentException("Unrecognized ServletMapAdapter container type: " +
                    (container != null ? container.getClass().getName() : "null"));
        }
    }

    /**
     * Returns the attribute synchronization object in a HttpSession or SerlvetContext container or wrapper.
     */
    static Object getSyncObject(Object container) {
        if (container instanceof SessionAttributeMapAdapter) {
            return getSyncObject(((SessionAttributeMapAdapter) container).getContainer());
        } else if (container instanceof ServletContextAttributeMapAdapter) {
            return getSyncObject(((ServletContextAttributeMapAdapter) container).getContainer());
        } else if (container instanceof HttpSession) {
            return getSyncObject((HttpSession) container);
        } else if (container instanceof ServletContext) {
            return getSyncObject((ServletContext) container);
        } else {
            return null;
        }
    }

    static Object getSyncObject(HttpSession session) {
        return UtilHttp.getSessionSyncObject(session);
    }

    static Object getSyncObject(ServletContext servletContext) {
        return UtilHttp.getServletContextSyncObject(servletContext);
    }

    /**
     * Servlet API container map wrapper common implementation.
     * <p>DEV NOTE: Avoid adding any new public methods, to avoid problems with dynamic languages and bean engines;
     * helps promote Map over the Servlet API interfaces.</p>
     */
    abstract class ContainerMapAdapter extends AbstractMap<String, Object> implements ServletMapAdapter {
        private final Object container;

        protected ContainerMapAdapter(Object container) {
            this.container = container;
        }

        protected Object getContainer() {
            return container;
        }

        protected abstract Object getAttribute(String name);

        protected abstract Object setAttribute(String name, Object o);

        protected abstract Object removeAttribute(String name);

        protected abstract Enumeration<String> getAttributeNamesEnum();

        protected List<String> getAttributeNames() {
            return Collections.list(getAttributeNamesEnum());
        }

        protected boolean containsAttribute(String name) {
            return (getAttribute(name) != null);
        }

        @Override
        public int size() {
            int size = 0;
            Enumeration<String> attributeNames = getAttributeNamesEnum();
            while(attributeNames.hasMoreElements()) {
                attributeNames.nextElement();
                size++;
            }
            return size;
        }

        @Override
        public boolean isEmpty() {
            return !getAttributeNamesEnum().hasMoreElements();
        }

        @Override
        public boolean containsKey(Object key) {
            Enumeration<String> attributeNames = getAttributeNamesEnum();
            while(attributeNames.hasMoreElements()) {
                Object attributeName = attributeNames.nextElement();
                if (Objects.equals(attributeName, key)) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean containsValue(Object value) {
            Enumeration<String> attributeNames = getAttributeNamesEnum();
            while(attributeNames.hasMoreElements()) {
                String attributeName = attributeNames.nextElement();
                Object attributeValue = getAttribute(attributeName);
                if (Objects.equals(attributeValue, value)) {
                    return true;
                }
            }
            return false;
        }

        @Override
        public Object get(Object key) {
            if (key != null && !(key instanceof String)) {
                throw new ClassCastException("Servlet maps only support string keys; got key of type [" + key.getClass().getName() + "]");
            }
            return getAttribute((String) key);
        }

        @Override
        public Object put(String key, Object value) {
            return setAttribute(key, value);
        }

        @Override
        public Object remove(Object key) {
            if (key != null && !(key instanceof String)) {
                throw new ClassCastException("Servlet maps only support string keys; got key of type [" + key.getClass().getName() + "]");
            }
            return removeAttribute((String) key);
        }

        @Override
        public void putAll(Map<? extends String, ?> m) {
            for(Map.Entry<? extends String, ? extends Object> entry : m.entrySet()) {
                setAttribute(entry.getKey(), entry.getValue());
            }
        }

        @Override
        public void clear() {
            Enumeration<String> attributeNames = getAttributeNamesEnum();
            while(attributeNames.hasMoreElements()) {
                removeAttribute(attributeNames.nextElement());
            }
        }

        @Override
        public Set<Entry<String, Object>> entrySet() {
            return makeAttributeEntrySet();
        }

        protected AttributeEntrySet makeAttributeEntrySet() {
            return new AttributeEntrySet();
        }

        protected class AttributeEntrySet extends AbstractSet<Entry<String, Object>> {
            private final List<AttributeEntry> entries;

            protected AttributeEntrySet() {
                List<AttributeEntry> entries = new ArrayList<>();
                Enumeration<String> attributeNames = getAttributeNamesEnum();
                while(attributeNames.hasMoreElements()) {
                    String attributeName = attributeNames.nextElement();
                    entries.add(makeEntrySet(attributeName, getAttribute(attributeName)));
                }
                this.entries = entries;
            }

            protected List<AttributeEntry> getEntries() {
                return entries;
            }

            @Override
            public int size() {
                return getEntries().size();
            }

            @Override
            public Iterator<Entry<String, Object>> iterator() {
                return makeEntrySetIterator();
            }

            @Override
            public boolean add(Map.Entry<String, Object> e) { // NOTE: This is probably unused
                boolean contained = containsAttribute(e.getKey());
                if (!contained && e.getValue() != null) {
                    setAttribute(e.getKey(), e.getValue());
                    getEntries().add(new AttributeEntry(e));
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

            protected AttributeEntry makeEntrySet(String key, Object value) {
                return new AttributeEntry(key, value);
            }

            protected class AttributeEntry extends AbstractMap.SimpleEntry<String, Object> {
                protected AttributeEntry(String key, Object value) {
                    super(key, value);
                }

                protected AttributeEntry(Map.Entry<? extends String, ? extends Object> entry) {
                    super(entry);
                }

                @Override
                public Object setValue(Object value) {
                    ContainerMapAdapter.this.setAttribute(getKey(), value);
                    return (value != null) ? super.setValue(value) : removeEntry(getKey());
                }
            }

            protected AttributeEntryIterator makeEntrySetIterator() {
                return new AttributeEntryIterator();
            }

            protected class AttributeEntryIterator implements Iterator<Map.Entry<String, Object>> {
                protected int listIndex = -1;

                @Override
                public boolean hasNext() {
                    int size = getEntries().size();
                    return (size > 0) && (listIndex < size);
                }

                @Override
                public AttributeEntry next() {
                    if (listIndex < 0) {
                        listIndex = 0;
                    }
                    List<AttributeEntry> entries = getEntries();
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
                    List<AttributeEntry> entries = getEntries();
                    if (listIndex < entries.size()) {
                        removeAttribute(entries.get(listIndex).getKey());
                        entries.remove(listIndex);
                    } else {
                        throw new IllegalStateException("no next element");
                    }
                }
            }
        }
    }

    class EmptyAttributeMapAdapter extends ContainerMapAdapter {
        protected static final EmptyAttributeMapAdapter INSTANCE = new EmptyAttributeMapAdapter(null);

        protected EmptyAttributeMapAdapter(Object container) {
            super(container);
        }

        public static EmptyAttributeMapAdapter getInstance() {
            return INSTANCE;
        }

        @Override
        protected Object getAttribute(String name) {
            return null;
        }

        @Override
        protected Object setAttribute(String name, Object o) {
            throw new UnsupportedOperationException();
        }

        @Override
        protected Object removeAttribute(String name) {
            throw new UnsupportedOperationException();
        }

        @Override
        protected Enumeration<String> getAttributeNamesEnum() {
            return new Enumeration<String>() {
                @Override
                public boolean hasMoreElements() {
                    return false;
                }

                @Override
                public String nextElement() {
                    throw new NoSuchElementException();
                }
            };
        }

        @Override
        protected List<String> getAttributeNames() {
            return Collections.emptyList();
        }
    }

    class RequestAttributeMapAdapter extends ContainerMapAdapter {
        protected RequestAttributeMapAdapter(ServletRequest container) {
            super(container);
        }

        @Override
        protected ServletRequest getContainer() {
            return (ServletRequest) super.getContainer();
        }

        @Override
        protected Object getAttribute(String name) {
            return getContainer().getAttribute(name);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().setAttribute(name, value);
            return oldValue;
        }

        @Override
        protected Object removeAttribute(String name) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().removeAttribute(name);
            return oldValue;
        }

        @Override
        protected Enumeration<String> getAttributeNamesEnum() {
            return getContainer().getAttributeNames();
        }
    }

    class ReadonlyRequestAttributeMapAdapter extends RequestAttributeMapAdapter {
        protected ReadonlyRequestAttributeMapAdapter(ServletRequest container) {
            super(container);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            throw new UnsupportedOperationException();
        }

        @Override
        protected Object removeAttribute(String name) {
            throw new UnsupportedOperationException();
        }
    }

    class SessionAttributeMapAdapter extends ContainerMapAdapter {
        protected SessionAttributeMapAdapter(HttpSession container) {
            super(container);
        }

        @Override
        protected HttpSession getContainer() {
            return (HttpSession) super.getContainer();
        }

        @Override
        protected Object getAttribute(String name) {
            return getContainer().getAttribute(name);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().setAttribute(name, value);
            return oldValue;
        }

        @Override
        protected Object removeAttribute(String name) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().removeAttribute(name);
            return oldValue;
        }

        @Override
        protected Enumeration<String> getAttributeNamesEnum() {
            return getContainer().getAttributeNames();
        }
    }

    class ReadonlySessionAttributeMapAdapter extends SessionAttributeMapAdapter {
        protected ReadonlySessionAttributeMapAdapter(HttpSession container) {
            super(container);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            throw new UnsupportedOperationException();
        }

        @Override
        protected Object removeAttribute(String name) {
            throw new UnsupportedOperationException();
        }
    }

    class ServletContextAttributeMapAdapter extends ContainerMapAdapter {
        protected ServletContextAttributeMapAdapter(ServletContext container) {
            super(container);
        }

        @Override
        protected ServletContext getContainer() {
            return (ServletContext) super.getContainer();
        }

        @Override
        protected Object getAttribute(String name) {
            return getContainer().getAttribute(name);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().setAttribute(name, value);
            return oldValue;
        }

        @Override
        protected Object removeAttribute(String name) {
            Object oldValue = getContainer().getAttribute(name);
            getContainer().removeAttribute(name);
            return oldValue;
        }

        @Override
        protected Enumeration<String> getAttributeNamesEnum() {
            return getContainer().getAttributeNames();
        }
    }

    class ReadonlyServletContextAttributeMapAdapter extends ServletContextAttributeMapAdapter {
        protected ReadonlyServletContextAttributeMapAdapter(ServletContext container) {
            super(container);
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            throw new UnsupportedOperationException();
        }

        @Override
        protected Object removeAttribute(String name) {
            throw new UnsupportedOperationException();
        }
    }

    class RequestParameterMapAdapter extends ContainerMapAdapter {
        private final Object parameterMode;

        protected RequestParameterMapAdapter(ServletRequest container, Object parameterMode) {
            super(container);
            this.parameterMode = (parameterMode != null) ? parameterMode : "DYNAMIC";
        }

        @Override
        protected ServletRequest getContainer() {
            return (ServletRequest) super.getContainer();
        }

        protected Object getParameterMode() {
            return parameterMode;
        }

        @Override
        protected Object getAttribute(String name) {
            Object parameterMode = getParameterMode();
            if (parameterMode.equals("SINGLE")) {
                return getContainer().getParameter(name);
            } else {
                String[] parameterValues = getContainer().getParameterValues(name);
                if (parameterValues != null) {
                    if (parameterValues.length != 1 || parameterMode.equals("MULTI")) {
                        return Arrays.asList(parameterValues);
                    } else {
                        return parameterValues[0];
                    }
                } else {
                    return null;
                }
            }
        }

        @Override
        protected Object setAttribute(String name, Object value) {
            throw new UnsupportedOperationException("Request parameters are read-only");
        }

        @Override
        protected Object removeAttribute(String name) {
            throw new UnsupportedOperationException("Request parameters are read-only");
        }

        @Override
        protected Enumeration<String> getAttributeNamesEnum() {
            return getContainer().getParameterNames();
        }
    }
}
