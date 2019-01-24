package com.ilscipio.scipio.ce.util.servlet;

import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpSession;

import com.ilscipio.scipio.ce.util.collections.MapEntryAdapter;

/**
 * Servlet API adapters for the {@link com.ilscipio.scipio.ce.util.collections.MapEntryAdapter}
 * ({@link Map.Entry}) interface.
 * <p>
 * NOTE: See also {@link com.ilscipio.scipio.ce.util.collections.MapEntryAdapters}.
 */
public final class ServletMapEntryAdapters {

    private ServletMapEntryAdapters() {
    }

    /**
     * Returns a full-featured, best-possible-correctness request map entry adapter.
     */
    public static RequestMapEntryAdapter getAdapter(ServletRequest request, String key) {
        return new StdRequestMapEntryAdapter(request, key);
    }

    /**
     * Returns a full-featured, best-possible-correctness session map entry adapter.
     */
    public static SessionMapEntryAdapter getAdapter(HttpSession session, String key) {
        return new StdSessionMapEntryAdapter(session, key);
    }

    /**
     * Returns a full-featured, best-possible-correctness servlet context map entry adapter.
     */
    public static ServletContextMapEntryAdapter getAdapter(ServletContext servletContext, String key) {
        return new StdServletContextMapEntryAdapter(servletContext, key);
    }

    /**
     * Returns an optimized request map entry adapter <strong>that makes {@link java.util.Map.Entry}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>{@link #setValue} intentionally breaks the Map contract and does NOT return the previous/old value;
     *     it may returns null or a dummy default instead.</li>
     * </ul>
     */
    public static RequestMapEntryAdapter getFastAdapter(ServletRequest request, String key) {
        return new FastRequestMapEntryAdapter(request, key);
    }

    /**
     * Returns an optimized session map entry adapter <strong>that makes {@link java.util.Map.Entry}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>{@link #setValue} intentionally breaks the Map contract and does NOT return the previous/old value;
     *     it may returns null or a dummy default instead.</li>
     * </ul>
     */
    public static SessionMapEntryAdapter getFastAdapter(HttpSession session, String key) {
        return new FastSessionMapEntryAdapter(session, key);
    }

    /**
     * Returns an optimized session map entry adapter <strong>that makes {@link java.util.Map.Entry}
     * interface violations</strong>.
     * <p>
     * Characteristics:
     * <ul>
     * <li>{@link #setValue} intentionally breaks the Map contract and does NOT return the previous/old value;
     *     it may returns null or a dummy default instead.</li>
     * </ul>
     */
    public static ServletContextMapEntryAdapter getFastAdapter(ServletContext servletContext, String key) {
        return new FastServletContextMapEntryAdapter(servletContext, key);
    }

    public interface ServletMapEntryAdapter extends MapEntryAdapter<String, Object>, ServletAttrContainer {
        @Override
        public default void setValueOnly(Object value) {
            setAttribute(getKey(), value);
        }
    }

    public interface RequestMapEntryAdapter extends ServletMapEntryAdapter {
    }

    public interface SessionMapEntryAdapter extends ServletMapEntryAdapter {
    }

    public interface ServletContextMapEntryAdapter extends ServletMapEntryAdapter {
    }

    public interface StdServletMapEntryAdapter extends ServletMapEntryAdapter {
        @Override
        public default Object getValue() {
            return getAttribute(getKey());
        }

        @Override
        public default Object setValue(Object value) {
            Object oldValue = getAttribute(getKey());
            setValueOnly(value);
            return oldValue;
        }
    }

    public static class StdRequestMapEntryAdapter implements StdServletMapEntryAdapter, ServletAttrContainer.RequestAdapter, RequestMapEntryAdapter {
        private final ServletRequest request;
        private final String key;

        public StdRequestMapEntryAdapter(ServletRequest request, String key) {
            this.request = request;
            this.key = key;
        }

        @Override
        public String getKey() {
            return key;
        }

        @Override
        public ServletRequest getAdapted() {
            return request;
        }
    }

    public static class StdSessionMapEntryAdapter implements StdServletMapEntryAdapter, ServletAttrContainer.SessionAdapter, SessionMapEntryAdapter {
        private final HttpSession session;
        private final String key;

        public StdSessionMapEntryAdapter(HttpSession session, String key) {
            this.session = session;
            this.key = key;
        }

        @Override
        public String getKey() {
            return key;
        }

        @Override
        public HttpSession getAdapted() {
            return session;
        }
    }

    public static class StdServletContextMapEntryAdapter implements StdServletMapEntryAdapter, ServletAttrContainer.ServletContextAdapter, ServletContextMapEntryAdapter {
        private final ServletContext servletContext;
        private final String key;

        public StdServletContextMapEntryAdapter(ServletContext servletContext, String key) {
            this.servletContext = servletContext;
            this.key = key;
        }

        @Override
        public String getKey() {
            return key;
        }

        @Override
        public ServletContext getAdapted() {
            return servletContext;
        }
    }

    public interface FastServletMapEntryAdapter extends StdServletMapEntryAdapter {
        @Override
        public default Object setValue(Object value) {
            setValueOnly(value);
            return null;
        }
    }

    public static class FastRequestMapEntryAdapter extends StdRequestMapEntryAdapter implements FastServletMapEntryAdapter {
        public FastRequestMapEntryAdapter(ServletRequest request, String key) {
            super(request, key);
        }

        @Override
        public Object setValue(Object value) {
            setValueOnly(value);
            return null;
        }
    }

    public static class FastSessionMapEntryAdapter extends StdSessionMapEntryAdapter implements FastServletMapEntryAdapter {
        public FastSessionMapEntryAdapter(HttpSession session, String key) {
            super(session, key);
        }

        @Override
        public Object setValue(Object value) {
            setValueOnly(value);
            return null;
        }
    }

    public static class FastServletContextMapEntryAdapter extends StdServletContextMapEntryAdapter implements FastServletMapEntryAdapter {
        public FastServletContextMapEntryAdapter(ServletContext servletContext, String key) {
            super(servletContext, key);
        }

        @Override
        public Object setValue(Object value) {
            setValueOnly(value);
            return null;
        }
    }
}
