package org.ofbiz.base.util;

import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

/**
 * SCIPIO: Generic rendering utilities, for any rendering context.
 * <p>
 * NOTE: because this is in base component, it has limited access.
 */
public abstract class UtilRender {

    public static final String module = UtilRender.class.getName();
    
    public static final String RENDER_EXCEPTION_MODE_VAR = "scipioRenderExMode";
    
    // WARN: implicit dependency on common component (circular)...
    private static final RenderExceptionMode globalRenderExMode = RenderExceptionMode.valueOfPermissive(UtilProperties.getPropertyValue("general.properties", 
            "render.global.exception.mode"), RenderExceptionMode.RETHROW);
    
    protected UtilRender() {
    }

    public enum RenderExceptionMode {
        /**
         * Exceptions are rethrown (and logged); no details are printed to output (secure mode).
         */
        RETHROW,
        /**
         * Exceptions are printed out directly to output in Freemarker templates (insecure).
         */
        DEBUG,
        /**
         * Where possible (e.g. from Freemarker error handler), exceptions are logged
         * but nothing is printed; otherwise performs RETHROW (security trade-off, whether acceptable
         * may depend on application).
         */
        BLANK;

        /**
         * Gets value permissively or null for any invalid value. 
         */
        public static RenderExceptionMode valueOfPermissive(String val) {
            try {
                RenderExceptionMode res = RenderExceptionMode.valueOf(val);
                if (res != null) {
                    return res;
                }
            } catch(Exception e) {
                ;
            }
            return null;
        }
        
        public static RenderExceptionMode valueOfPermissive(String val, RenderExceptionMode defaultVal) {
            RenderExceptionMode mode = valueOfPermissive(val);
            return mode != null ? mode : defaultVal;
        }
        
        /**
         * Gets value permissively or null for any invalid value. 
         * Supports RenderExceptionMode instance and its string representation.
         */
        public static RenderExceptionMode valueOfPermissive(Object val) {
            if (val instanceof RenderExceptionMode) {
                return (RenderExceptionMode) val;
            } else if (val instanceof String) {
                return valueOfPermissive((String) val);
            }
            return null;
        }
        
        public static RenderExceptionMode valueOfPermissive(Object val, RenderExceptionMode defaultVal) {
            RenderExceptionMode mode = valueOfPermissive(val);
            return mode != null ? mode : defaultVal;
        }
    }
    
    public static RenderExceptionMode getGlobalRenderExceptionMode() {
        return globalRenderExMode;
    }
    
    /**
     * Gets the render exception mode from the context or more generic variables (best-effort).
     */
    public static RenderExceptionMode getRenderExceptionMode(Map<String, ?> context) {
        // TODO: REVIEW SECURITY IMPLICATIONS 
        // (currently moot because Ofbiz already relies heavily on context for security e.g. simpleEncoder)
        if (context != null) {
            RenderExceptionMode mode = RenderExceptionMode.valueOfPermissive(context.get(RENDER_EXCEPTION_MODE_VAR));
            if (mode != null) return mode;
            Object requestObj = context.get("request");
            if (requestObj instanceof ServletRequest) {
                return getRenderExceptionMode((ServletRequest) requestObj);
            }
        }
        return getGlobalRenderExceptionMode();
    }
    
    /**
     * Gets the render exception mode from the request or more generic variables (best-effort).
     */
    public static RenderExceptionMode getRenderExceptionMode(ServletRequest request) {
        // TODO: REVIEW SECURITY IMPLICATIONS 
        // (currently moot because Ofbiz already relies heavily on context for security e.g. simpleEncoder)
        if (request != null) {
            RenderExceptionMode mode = RenderExceptionMode.valueOfPermissive(request.getAttribute(RENDER_EXCEPTION_MODE_VAR));
            if (mode != null) return mode;
            ServletContext servletContext = request.getServletContext();
            if (servletContext != null) {
                // TODO?: consider getInitParameter...
                mode = RenderExceptionMode.valueOfPermissive(servletContext.getAttribute(RENDER_EXCEPTION_MODE_VAR));
                if (mode != null) return mode;
            }
        }
        return getGlobalRenderExceptionMode();
    }
    
    /**
     * Gets the render exception mode from the exception or its causes that implement RenderExceptionModeHolder.
     * If skipNull i
     */
    public static RenderExceptionMode getRenderExceptionMode(Throwable t, boolean checkCauses, boolean skipNull) {
        while (t != null) {
            if (t instanceof UtilRender.RenderExceptionModeHolder) {
                UtilRender.RenderExceptionMode res = ((UtilRender.RenderExceptionModeHolder) t).getRenderExceptionMode();
                if (!skipNull || res != null) {
                    return res;
                }
            }
            t = checkCauses ? t.getCause() : null;
        }
        return null;
    }
    
    /**
     * Tries to get a non-null RenderExceptionMode from the exception or the FIRST cause it finds that
     * implements RenderExceptionModeHolder (even if it's null). (skipNull==false)
     * If the exception wants to delegate, it can in turn invoke this call recursively in its implementation.
     */
    public static RenderExceptionMode getRenderExceptionMode(Throwable t) {
        return getRenderExceptionMode(t, true, false);
    }

    public interface RenderExceptionModeHolder {
        RenderExceptionMode getRenderExceptionMode();
    }
}
