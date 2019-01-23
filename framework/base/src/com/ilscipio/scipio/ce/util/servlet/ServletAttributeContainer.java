package com.ilscipio.scipio.ce.util.servlet;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilMisc;

/**
 * SCIPIO: Interface for any servlet object meant to contain attributes.
 * <p>
 * Created derived on the implicit equivalent behavior of HttpServletRequest, HttpSession and ServletContext
 * with respect to attributes, but which unfortunately don't define an interface like this.
 * <p>
 * NOTE: Unlike most Maps, servlet attribute cannot contain the value <code>null</code> (setting to
 * null removes the attribute).
 */
public interface ServletAttributeContainer {

    /*
     * ******************************************************************************
     * Factory methods
     * ******************************************************************************
     */

    /**
     * Returns a request map adapter.
     */
    public static RequestAdapter getAdapter(ServletRequest request) {
        return new StdRequestAdapter(request);
    }

    /**
     * Returns a session map adapter.
     */
    public static SessionAdapter getAdapter(HttpSession session) {
        return new StdSessionAdapter(session);
    }

    /**
     * Returns a servlet context map adapter.
     */
    public static ServletContextAdapter getAdapter(ServletContext servletContext) {
        return new StdServletContextAdapter(servletContext);
    }


    /*
     * ******************************************************************************
     * Main API instance methods
     * ******************************************************************************
     * These signatures correspond to the ones of ServletRequest, HttpSession and ServletContext.
     */

    public Object getAttribute(String name);

    public void setAttribute(String name, Object o);

    public void removeAttribute(String name);

    public Enumeration<String> getAttributeNames();

    /*
     * ******************************************************************************
     * Additional API helper instance methods
     * ******************************************************************************
     */

    /**
     * Checks if the given attribute is contained in the container.
     * <p>
     * NOTE: For {@link ServletRequest} and other Servlet API classes, this is equivalent to checking for <code>null</code> value.
     * @see
     */
    public default boolean containsAttribute(String name) { return (getAttribute(name) != null); }

    public default Iterator<String> getAttributeNamesIterator() { return getAttributeNamesIterator(getAttributeNames()); }

    public default Set<String> getAttributeNamesSet() { return getAttributeNamesSet(getAttributeNames()); }

    public default List<String> getAttributeNamesList() { return getAttributeNamesList(getAttributeNames()); }

    /*
     * ******************************************************************************
     * Adapter interfaces and default implementations
     * ******************************************************************************
     * NOTE: These are convenience functions that performs the default operations, avoid null
     * and are slightly faster.
     */

    public interface Adapter extends ServletAttributeContainer {
        Object getAdapted();
    }

    public interface RequestAdapter extends Adapter {
        @Override
        ServletRequest getAdapted();

        @Override
        public default Object getAttribute(String name) {
            return getAdapted().getAttribute(name);
        }

        @Override
        public default Enumeration<String> getAttributeNames() {
            return getAdapted().getAttributeNames();
        }

        @Override
        public default void setAttribute(String name, Object o) {
            getAdapted().setAttribute(name, o);
        }

        @Override
        public default void removeAttribute(String name) {
            getAdapted().removeAttribute(name);
        }
    }

    public static abstract class StdServletAdapter<T> implements Adapter {
        private final T adapted;

        protected StdServletAdapter(T adapted) {
            this.adapted = adapted;
        }

        @Override
        public T getAdapted() {
            return adapted;
        }
    }

    public static class StdRequestAdapter extends StdServletAdapter<ServletRequest> implements RequestAdapter {
        public StdRequestAdapter(ServletRequest adapted) {
            super(adapted);
        }
    }

    public interface SessionAdapter extends Adapter {
        @Override
        HttpSession getAdapted();

        @Override
        public default Object getAttribute(String name) {
            return getAdapted().getAttribute(name);
        }

        @Override
        public default Enumeration<String> getAttributeNames() {
            return getAdapted().getAttributeNames();
        }

        @Override
        public default void setAttribute(String name, Object o) {
            getAdapted().setAttribute(name, o);
        }

        @Override
        public default void removeAttribute(String name) {
            getAdapted().removeAttribute(name);
        }
    }

    public static class StdSessionAdapter extends StdServletAdapter<HttpSession> implements SessionAdapter {
        public StdSessionAdapter(HttpSession adapted) {
            super(adapted);
        }
    }

    public interface ServletContextAdapter extends Adapter {
        @Override
        ServletContext getAdapted();

        @Override
        public default Object getAttribute(String name) {
            return getAdapted().getAttribute(name);
        }

        @Override
        public default Enumeration<String> getAttributeNames() {
            return getAdapted().getAttributeNames();
        }

        @Override
        public default void setAttribute(String name, Object o) {
            getAdapted().setAttribute(name, o);
        }

        @Override
        public default void removeAttribute(String name) {
            getAdapted().removeAttribute(name);
        }
    }

    public static class StdServletContextAdapter extends StdServletAdapter<ServletContext> implements ServletContextAdapter {
        public StdServletContextAdapter(ServletContext adapted) {
            super(adapted);
        }
    }

    /*
     * ******************************************************************************
     * Static helpers
     * ******************************************************************************
     * NOTE: These are convenience functions that performs the default operations, avoid null
     * and are slightly faster.
     */

    public static boolean containsAttribute(Enumeration<String> attrNames, String name) { return UtilMisc.contains(attrNames, name); }
    public static Iterator<String> getAttributeNamesIterator(Enumeration<String> attrNames) { return UtilMisc.toIterator(attrNames); }
    public static Set<String> getAttributeNamesSet(Enumeration<String> attrNames) { return UtilMisc.toSet(attrNames); }
    public static List<String> getAttributeNamesList(Enumeration<String> attrNames) { return UtilMisc.toList(attrNames); }

    public static Object getAttribute(ServletAttributeContainer container, String name) { return (container != null) ? container.getAttribute(name) : null; }
    public static void setAttribute(ServletAttributeContainer container, String name, Object o) { if (container != null) { container.setAttribute(name, o); } }
    public static void removeAttribute(ServletAttributeContainer container, String name) { if (container != null) { container.removeAttribute(name); } }
    public static Enumeration<String> getAttributeNames(ServletAttributeContainer container) { return (container != null) ? container.getAttributeNames() : null; }
    public static Iterator<String> getAttributeNamesIterator(ServletAttributeContainer container) { return (container != null) ? container.getAttributeNamesIterator() : null; }
    public static Set<String> getAttributeNamesSet(ServletAttributeContainer container) { return (container != null) ? container.getAttributeNamesSet() : null; }
    public static List<String> getAttributeNamesList(ServletAttributeContainer container) { return (container != null) ? container.getAttributeNamesList() : null; }
    public static boolean containsAttribute(ServletAttributeContainer container, String name) { return (container != null) ? container.containsAttribute(name) : false; }

    public static Object getAttribute(ServletRequest container, String name) { return (container != null) ? container.getAttribute(name) : null; }
    public static void setAttribute(ServletRequest container, String name, Object o) { if (container != null) { container.setAttribute(name, o); } }
    public static void removeAttribute(ServletRequest container, String name) { if (container != null) { container.removeAttribute(name); } }
    public static Enumeration<String> getAttributeNames(ServletRequest container) { return (container != null) ? container.getAttributeNames() : null; }
    public static Iterator<String> getAttributeNamesIterator(ServletRequest container) { return (container != null) ? getAttributeNamesIterator(container.getAttributeNames()) : null; }
    public static Set<String> getAttributeNamesSet(ServletRequest container) { return (container != null) ? getAttributeNamesSet(container.getAttributeNames()) : null; }
    public static List<String> getAttributeNamesList(ServletRequest container) { return (container != null) ? getAttributeNamesList(container.getAttributeNames()) : null; }
    public static boolean containsAttribute(ServletRequest container, String name) { return (container != null) ? containsAttribute(container.getAttributeNames(), name) : false; }

    public static Object getAttribute(HttpSession container, String name) { return (container != null) ? container.getAttribute(name) : null; }
    public static void setAttribute(HttpSession container, String name, Object o) { if (container != null) { container.setAttribute(name, o); } }
    public static void removeAttribute(HttpSession container, String name) { if (container != null) { container.removeAttribute(name); } }
    public static Enumeration<String> getAttributeNames(HttpSession container) { return (container != null) ? container.getAttributeNames() : null; }
    public static Iterator<String> getAttributeNamesIterator(HttpSession container) { return (container != null) ? getAttributeNamesIterator(container.getAttributeNames()) : null; }
    public static Set<String> getAttributeNamesSet(HttpSession container) { return (container != null) ? getAttributeNamesSet(container.getAttributeNames()) : null; }
    public static List<String> getAttributeNamesList(HttpSession container) { return (container != null) ? getAttributeNamesList(container.getAttributeNames()) : null; }
    public static boolean containsAttribute(HttpSession container, String name) { return (container != null) ? containsAttribute(container.getAttributeNames(), name) : false; }

    public static Object getAttribute(ServletContext container, String name) { return (container != null) ? container.getAttribute(name) : null; }
    public static void setAttribute(ServletContext container, String name, Object o) { if (container != null) { container.setAttribute(name, o); } }
    public static void removeAttribute(ServletContext container, String name) { if (container != null) { container.removeAttribute(name); } }
    public static Enumeration<String> getAttributeNames(ServletContext container) { return (container != null) ? container.getAttributeNames() : null; }
    public static Iterator<String> getAttributeNamesIterator(ServletContext container) { return (container != null) ? getAttributeNamesIterator(container.getAttributeNames()) : null; }
    public static Set<String> getAttributeNamesSet(ServletContext container) { return (container != null) ? getAttributeNamesSet(container.getAttributeNames()) : null; }
    public static List<String> getAttributeNamesList(ServletContext container) { return (container != null) ? getAttributeNamesList(container.getAttributeNames()) : null; }
    public static boolean containsAttribute(ServletContext container, String name) { return (container != null) ? containsAttribute(container.getAttributeNames(), name) : false; }

}
