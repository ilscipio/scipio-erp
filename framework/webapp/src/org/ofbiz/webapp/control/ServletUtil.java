package org.ofbiz.webapp.control;

import java.util.Collection;

import javax.servlet.ServletContext;
import javax.servlet.ServletRegistration;

import org.ofbiz.base.util.Debug;

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
    
}
