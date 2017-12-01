package org.ofbiz.webapp.control;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

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

}
