package org.ofbiz.webapp.control;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilValidate;

/**
 * SCIPIO: Request utilities.
 */
public abstract class RequestUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
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
    
}
