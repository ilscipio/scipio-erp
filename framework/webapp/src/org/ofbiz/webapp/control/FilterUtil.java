package org.ofbiz.webapp.control;

import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.servlet.FilterConfig;

/**
 * SCIPIO: Filter utilities.
 * 
 * @see RequestUtil
 */
public abstract class FilterUtil {

    protected FilterUtil() {
    }

    /**
     * Gets map of init-params from filter config.
     * Fill-in for missing java servlet API method.
     */
    public static Map<String, String> getInitParams(FilterConfig filterConfig) {
        Map<String, String> initParams = new HashMap<>();
        Enumeration<String> names = filterConfig.getInitParameterNames();
        while(names.hasMoreElements()) {
            String name = names.nextElement();
            initParams.put(name, filterConfig.getInitParameter(name));
        }
        return initParams;
    }
    
    public static FilterConfigInitParamsMapAdapter getInitParamsMapAdapter(FilterConfig filterConfig) {
        return new FilterConfigInitParamsMapAdapter(filterConfig);
    }
    
    /**
     * Wraps a FilterConfig and avoids creating whole map if only <code>get</code> calls are done.
     */
    public static class FilterConfigInitParamsMapAdapter implements Map<String, String> {

        private final FilterConfig config;
        private Map<String, String> initParams = null;

        public FilterConfigInitParamsMapAdapter(FilterConfig config) {
            this.config = config;
        }
        
        protected Map<String, String> getInitParamsMap() {
            Map<String, String> initParams = this.initParams;
            if (initParams == null) {
                initParams = Collections.unmodifiableMap(FilterUtil.getInitParams(config));
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
        public String get(Object key) { return config.getInitParameter((String) key); }
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
