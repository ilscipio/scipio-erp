package org.ofbiz.webapp.control;

import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletRegistration;

import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.control.FilterUtil.FilterConfigInitParamsMapAdapter;

/**
 * SCIPIO: Servlet and ServletContext generic utilities.
 * 
 * @see RequestUtil
 */
public abstract class ServletUtil {

    public static final String module = ServletUtil.class.getName();
    
    protected ServletUtil() {
    }

    public static Collection<String> getServletMappings(ServletContext servletContext, String servletName) {
        ServletRegistration reg = servletContext.getServletRegistration(servletName);
        if (reg == null) return null;
        return reg.getMappings();
    }
    
    /**
     * Returns the first mapping for the given servlet name, or null or none or not found.
     */
    public static String getServletMapping(ServletContext servletContext, String servletName) {
        Collection<String> mappings = getServletMappings(servletContext, servletName);
        if (mappings == null || mappings.size() == 0) return null;
        if (mappings.size() > 1) {
            Debug.logWarning("Scipio: Servlet with name '" + servletName + "' has multiple mappings in web.xml; returning first only", module);
        }
        return mappings.iterator().next();
    }
    
    /**
     * Returns the first mapping for the given servlet name with wildcard removed, or null or none or not found.
     */
    public static String getBaseServletMapping(ServletContext servletContext, String servletName) {
        return getBaseServletMapping(getServletMapping(servletContext, servletName));
    }
    
    /**
     * Removes any wildcard in servlet mapping.
     * The returned never ends with "/" unless it's the root "/" mapping.
     */
    public static String getBaseServletMapping(String mapping) {
        if (mapping == null) return null;
        if (mapping.endsWith("/*")) mapping = mapping.substring(0, mapping.length() - "/*".length());
        if (mapping.isEmpty()) mapping = "/";
        return mapping;
    }
    
    /**
     * Gets map of context-params from servlet context
     * Fill-in for missing java servlet API method.
     */
    public static Map<String, String> getContextParams(ServletContext servletContext) {
        Map<String, String> initParams = new HashMap<>();
        Enumeration<String> names = servletContext.getInitParameterNames();
        while(names.hasMoreElements()) {
            String name = names.nextElement();
            initParams.put(name, servletContext.getInitParameter(name));
        }
        return initParams;
    }
    
    public static ServletContextInitParamsMapAdapter getContextParamsMapAdapter(ServletContext servletContext) {
        return new ServletContextInitParamsMapAdapter(servletContext);
    }
    
    /**
     * Wraps a ServletContext and avoids creating whole map if only <code>get</code> calls are done.
     */
    public static class ServletContextInitParamsMapAdapter implements Map<String, String> {

        private final ServletContext servletContext;
        private Map<String, String> initParams = null;

        public ServletContextInitParamsMapAdapter(ServletContext config) {
            this.servletContext = config;
        }
        
        protected Map<String, String> getInitParamsMap() {
            Map<String, String> initParams = this.initParams;
            if (initParams == null) {
                initParams = Collections.unmodifiableMap(ServletUtil.getContextParams(servletContext));
                this.initParams = initParams;
            }
            return initParams;
        }

        @Override
        public int size() { return getInitParamsMap().size(); }
        @Override
        public boolean isEmpty() { return getInitParamsMap().size() == 0; }
        @Override
        public boolean containsKey(Object key) { return getInitParamsMap().containsKey(key); }
        @Override
        public boolean containsValue(Object value) { return getInitParamsMap().containsValue(value); }
        @Override
        public String get(Object key) { return servletContext.getInitParameter((String) key); }
        @Override
        public String put(String key, String value) { throw new UnsupportedOperationException(); }
        @Override
        public String remove(Object key) { throw new UnsupportedOperationException(); }
        @Override
        public void putAll(Map<? extends String, ? extends String> m) { throw new UnsupportedOperationException(); }
        @Override
        public void clear() { throw new UnsupportedOperationException(); }
        @Override
        public Set<String> keySet() { return getInitParamsMap().keySet(); }
        @Override
        public Collection<String> values() { return getInitParamsMap().values(); }
        @Override
        public Set<java.util.Map.Entry<String, String>> entrySet() { return getInitParamsMap().entrySet(); }        
    }
}
