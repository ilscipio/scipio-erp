package com.ilscipio.scipio.ce.util.servlet;

import org.ofbiz.base.util.UtilHttp;

import javax.annotation.concurrent.NotThreadSafe;
import javax.annotation.concurrent.ThreadSafe;
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
public interface ServletMapWrapper extends Map<String, Object> {

    /**
     * Wraps request attributes in a {@link Map} wrapper.
     * <p>The request and wrapper are non-synchronized.</p>
     */
    static ServletMapWrapper wrapAttributes(ServletRequest request) {
        return new RequestAttributeMapWrapper(request);
    }

    /**
     * Wraps request attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     */
    static ServletMapWrapper wrapAttributesReadonly(ServletRequest request) {
        return new ReadonlyRequestAttributeMapWrapper(request);
    }

    /**
     * Wraps session attributes in an explicitly synchronized and thread-safe {@link Map} wrapper.
     * @see #wrapAttributesSync(HttpSession)
     */
    static ServletMapWrapper wrapAttributes(HttpSession session) {
        return wrapAttributesSync(session);
    }

    /**
     * Wraps session attributes in an explicitly synchronized and thread-safe {@link Map} wrapper.
     */
    static ServletMapWrapper wrapAttributesSync(HttpSession session) {
        return new SyncSessionAttributeMapWrapper(session);
    }

    /**
     * Wraps session attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     * <p>Designed for thread-safe operation using immutable copies.</p>
     */
    static ServletMapWrapper wrapAttributesReadonly(HttpSession session) {
        return new ReadonlySessionAttributeMapWrapper(session);
    }

    /**
     * Wraps session attributes in a possibly non-synchronized {@link Map} wrapper intended to leave synchronization
     * to the caller.
     * @see #wrapAttributesSync(HttpSession)
     */
    static ServletMapWrapper wrapAttributesUnsync(HttpSession session) {
        return new SessionAttributeMapWrapper(session);
    }

    /**
     * Wraps serlvet context attributes in an explicitly thread-safe {@link Map} wrapper.
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapWrapper wrapAttributes(ServletContext servletContext) {
        return wrapAttributesSync(servletContext);
    }

    /**
     * Wraps serlvet context attributes in an explicitly thread-safe {@link Map} wrapper.
     */
    static ServletMapWrapper wrapAttributesSync(ServletContext servletContext) {
        return new SyncServletContextAttributeMapWrapper(servletContext);
    }

    /**
     * Wraps serlvet context attributes in a {@link Map} wrapper that is only appropriate and optimized for read operations.
     * <p>Designed for thread-safe operation using immutable copies.</p>
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapWrapper wrapAttributesReadonly(ServletContext servletContext) {
        return new ReadonlyServletContextAttributeMapWrapper(servletContext);
    }

    /**
     * Wraps serlvet context attributes in a possibly non-synchronized {@link Map} wrapper intended to leave synchronization
     * to the caller.
     * @see #wrapAttributesSync(ServletContext)
     */
    static ServletMapWrapper wrapAttributesUnsync(ServletContext servletContext) {
        return new ServletContextAttributeMapWrapper(servletContext);
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper returns {@link String} for single parameters and {@link List}
     * for parameters with multiple values (legacy system default).
     * <p>The request parameters and wrapper are read-only.</p>
     * @see #wrapParametersDynamic(ServletRequest)
     */
    static ServletMapWrapper wrapParameters(ServletRequest request, ParameterMode parameterMode) {
        return new RequestParameterMapWrapper(request, (parameterMode != null) ? parameterMode : ParameterMode.getDefault());
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper that only calls {@link ServletRequest#getParameter(String)}
     * and returns only the first value for all parameter names.
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapWrapper wrapParametersSingle(ServletRequest request) {
        return wrapParameters(request, ParameterMode.SINGLE);
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper that only calls {@link ServletRequest#getParameterValues(String)}
     * and always returns {@link List} as map values.
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapWrapper wrapParametersMulti(ServletRequest request) {
        return wrapParameters(request, ParameterMode.MULTI);
    }

    /**
     * Wraps request parameters in a {@link Map} wrapper returns {@link String} for single parameters and {@link List}
     * for parameters with multiple values (legacy system default).
     * <p>The request parameters and wrapper are read-only.</p>
     */
    static ServletMapWrapper wrapParametersDynamic(ServletRequest request) {
        return wrapParameters(request, ParameterMode.DYNAMIC);
    }

    enum ParameterMode {
        /**
         * Sets the map values to and causes {@link Map#get} to return only the first {@link String} value for the given parameter name.
         */
        SINGLE,

        /**
         * Sets the map values to and causes {@link Map#get} to return {@link List} instances for the given parameter name values even if only one.
         */
        MULTI,

        /**
         * Sets the map values to and causes {@link Map#get} to return lists for the given parameter name values if multiple or string if one.
         * <p><strong>WARN:</strong> Occasionally this can lead to unexpected class cast exceptions around code that
         * handles the "parameters" map in screen rendering, but due to being the system legacy default the default is not easily changed to list,
         * and typically for security reasons it leads to immediate render aborts when encountered.</p>
         */
        DYNAMIC;

        /**
         * Returns the default ParameterMode for the framework, for legacy reasons always {@link ParameterMode#DYNAMIC}.
         */
        public static ParameterMode getDefault() {
            return DYNAMIC;
        }
    }

    /**
     * Given a {@link Map} implemented using a ServletMapWrapper implementation, return its wrapped container
     * in the desired type.
     * <p>DEV NOTE: This is a static method to avoid adding public methods to the implementations, to avoid
     * problems with the bean engines.</p>
     */
    static <T> T getContainer(Object container) {
        if (container instanceof ContainerMapWrapper) {
            @SuppressWarnings("unchecked")
            T result = (T) ((ContainerMapWrapper) container).getContainer();
            return result;
        } else {
            throw new IllegalArgumentException("Unrecognized ServletMapWrapper container type: " +
                    (container != null ? container.getClass().getName() : "null"));
        }
    }

    /**
     * Returns the attribute synchronization object in a HttpSession or SerlvetContext container or wrapper.
     */
    static Object getSyncObject(Object container) {
        if (container instanceof SessionAttributeMapWrapper) {
            return getSyncObject(((SessionAttributeMapWrapper) container).getContainer());
        } else if (container instanceof ServletContextAttributeMapWrapper) {
            return getSyncObject(((ServletContextAttributeMapWrapper) container).getContainer());
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
    @NotThreadSafe
    abstract class ContainerMapWrapper extends AbstractMap<String, Object> implements ServletMapWrapper {
        private final Object container;

        protected ContainerMapWrapper(Object container) {
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
                    setAttribute(getKey(), value);
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

    @NotThreadSafe
    class RequestAttributeMapWrapper extends ContainerMapWrapper {
        protected RequestAttributeMapWrapper(ServletRequest container) {
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

    @ThreadSafe
    class ReadonlyRequestAttributeMapWrapper extends RequestAttributeMapWrapper {
        protected ReadonlyRequestAttributeMapWrapper(ServletRequest container) {
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

    @NotThreadSafe
    class SessionAttributeMapWrapper extends ContainerMapWrapper {
        protected SessionAttributeMapWrapper(HttpSession container) {
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

    @ThreadSafe
    class SyncSessionAttributeMapWrapper extends SessionAttributeMapWrapper {
        protected SyncSessionAttributeMapWrapper(HttpSession container) {
            super(container);
        }

        // TODO
    }

    @ThreadSafe
    class ReadonlySessionAttributeMapWrapper extends SessionAttributeMapWrapper {
        protected ReadonlySessionAttributeMapWrapper(HttpSession container) {
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

    @NotThreadSafe
    class ServletContextAttributeMapWrapper extends ContainerMapWrapper {
        protected ServletContextAttributeMapWrapper(ServletContext container) {
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

    @ThreadSafe
    class SyncServletContextAttributeMapWrapper extends ServletContextAttributeMapWrapper {
        protected SyncServletContextAttributeMapWrapper(ServletContext container) {
            super(container);
        }

        // TODO
    }

    @ThreadSafe
    class ReadonlyServletContextAttributeMapWrapper extends ServletContextAttributeMapWrapper {
        protected ReadonlyServletContextAttributeMapWrapper(ServletContext container) {
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

    class RequestParameterMapWrapper extends ContainerMapWrapper {
        private final ParameterMode parameterMode;

        protected RequestParameterMapWrapper(ServletRequest container, ParameterMode parameterMode) {
            super(container);
            this.parameterMode = parameterMode;
        }

        @Override
        protected ServletRequest getContainer() {
            return (ServletRequest) super.getContainer();
        }

        protected ParameterMode getParameterMode() {
            return parameterMode;
        }

        @Override
        protected Object getAttribute(String name) {
            ParameterMode parameterMode = getParameterMode();
            if (parameterMode == ParameterMode.SINGLE) {
                return getContainer().getParameter(name);
            } else {
                String[] parameterValues = getContainer().getParameterValues(name);
                if (parameterValues != null) {
                    if (parameterMode != ParameterMode.MULTI && parameterValues.length == 1) {
                        return parameterValues[0];
                    } else {
                        return Arrays.asList(parameterValues);
                    }
                }
            }
            return null;
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
