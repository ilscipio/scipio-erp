package org.ofbiz.webapp.control;

import java.util.Locale;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;

/**
 * SCIPIO: Request utilities.
 */
public abstract class RequestUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    // TODO: unhardcode
    private static final String GENERICERRMSG_EN_FALLBACK = "An error occurred. Please contact support.";
    
    protected RequestUtil() {
    }
    
    /**
     * @deprecated 2018-02-26: This should now be done by Freemarker files or 
     * whatever output method is used, unless the error is going directly out to browser (bypassing renderer);
     * in such cases, you may still use {@link #encodeErrorMessage}.
     */
    @Deprecated
    public static String getEncodedSecureErrorMessage(HttpServletRequest request, Throwable t) {
        return encodeErrorMessage(request, getSecureErrorMessage(request, t.toString()));
    }
    
    public static String getSecureErrorMessage(HttpServletRequest request, Throwable t) {
        return getSecureErrorMessage(request, t.toString());
    }
    
    /**
     * SCIPIO: prevents setting a too specific _ERROR_MESSAGE_ that may compromise security.
     * Added 2017-05-12.
     */
    public static String getSecureErrorMessage(HttpServletRequest request, String msg) {
        if (UtilRender.getRenderExceptionMode(request) == UtilRender.RenderExceptionMode.DEBUG) {
            return msg;
        } else {
            return getGenericErrorMessage(request);
        }
    }
    
    /**
     * HTML-encodes the given message.
     * <p>
     * NOTE: 2018-02-26: This should now be done by Freemarker files or whatever output method is used in almost all cases;
     * this method is only for direct HTML output to browser (bypassing renderer).
     */
    public static String encodeErrorMessage(HttpServletRequest request, String msg) {
        // FIXME: this is stock ofbiz behavior, originally from ControlServlet: hardcoding as html
        // is not appropriate for JSON and possibly other cases...
        return UtilCodec.getEncoder("html").encode(msg);
    }
    
    /**
     * Returns a generic "An error occurred. Please contact support." error message.
     */
    public static String getGenericErrorMessage(HttpServletRequest request) {
        String msg = null;
        try {
            Locale locale = UtilHttp.getLocale(request);
            msg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", locale);
        } catch(Throwable t) {
            ;
            //Debug.logError(t, module);
        }
        if (UtilValidate.isEmpty(msg)) {
            msg = GENERICERRMSG_EN_FALLBACK;
        }
        return msg;
    }
    
    public static String getGenericErrorMessage(Locale locale) {
        String msg = null;
        try {
            msg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", locale);
        } catch(Throwable t) {
            ;
            //Debug.logError(t, module);
        }
        if (UtilValidate.isEmpty(msg)) {
            msg = GENERICERRMSG_EN_FALLBACK;
        }
        return msg;
    }
    
    public static String getGenericErrorMessage() {
        return getGenericErrorMessage(Locale.ENGLISH);
    }

    /**
     * Obtains the delegator from current request in a read-only (does not create session
     * or populate any attributes), best-effort fashion.
     * <p>
     * WARN: TODO: REVIEW: For tenant delegators, this may be one request late
     * in returning the tenant delegator, during the tenant login; implications unclear.
     * DEV NOTE: If this is fixed in the future, it may need to do redundant tenant
     * delegator preparation.
     * <p>
     * DEV NOTE: This must stay in sync with logic from {@link ContextFilter}.
     * <p>
     * Added 2018-07-31.
     */
    public static Delegator getDelegatorReadOnly(HttpServletRequest request) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        HttpSession session = request.getSession(false);
        if (session != null) {
            delegator = (Delegator) session.getAttribute("delegator");
            if (delegator != null) {
                return delegator;
            }
            String delegatorName = (String) session.getAttribute("delegatorName");
            if (delegatorName != null) {
                delegator = DelegatorFactory.getDelegator(delegatorName);
                if (delegator != null) {
                    return delegator;
                } else {
                    Debug.logError("ERROR: delegator factory returned null for delegatorName \"" 
                            + delegatorName + "\" from session attributes", module);
                }
            }
        }
        return getDelegatorReadOnly(request.getServletContext());
    }

    public static Delegator getDelegatorReadOnly(ServletContext servletContext) {
        Delegator delegator = (Delegator) servletContext.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        String delegatorName = servletContext.getInitParameter("entityDelegatorName");
        if (delegatorName == null || delegatorName.length() <= 0) {
            delegatorName = "default";
        }
        delegator = DelegatorFactory.getDelegator(delegatorName);
        if (delegator != null) {
            return delegator;
        } else {
            Debug.logError("ERROR: delegator factory returned null for delegatorName \"" 
                    + delegatorName + "\" from servlet context", module);
        }
        if (!"default".equals(delegatorName)) {
            delegator = DelegatorFactory.getDelegator("default");
            if (delegator != null) {
                return delegator;
            } else {
                Debug.logError("ERROR: delegator factory returned null for delegatorName \"" 
                        + "default\" from servlet context", module);
            }
        }
        return delegator;
    }

    /**
     * Obtains the delegator from current request in a read-only (does not create session
     * or populate any attributes), best-effort fashion, suitable for calls from early filters.
     * <p>
     * WARN: TODO: REVIEW: For tenant delegators, this may be one request late
     * in returning the tenant delegator, during the tenant login; implications unclear.
     * DEV NOTE: If this is fixed in the future, it may need to do redundant tenant
     * delegator preparation.
     * <p>
     * Added 2018-07-31.
     */
    public static Delegator getDelegatorFilterSafe(HttpServletRequest request) {
        return getDelegatorReadOnly(request);
    }

    public static Delegator getDelegatorFilterSafe(ServletContext servletContext) {
        return getDelegatorReadOnly(servletContext);
    }
}
