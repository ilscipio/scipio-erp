package com.ilscipio.scipio.cms.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider;
import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider.AccessToken;
import com.ilscipio.scipio.cms.content.CmsPage;

/**
 * Generates and cleanups access tokens for CMS, mainly for preview mode.
 * <p>
 * Added 2018-05-06.
 */
public class CmsAccessHandler implements HttpSessionListener {

    public static final String module = CmsAccessHandler.class.getName();
    
    public static final String SESSION_TOKEN_ATTR = "cmsAccessToken";
    
    /**
     * The global token hash map - associates token to UserLogin value.
     * <p>
     * The value is registered using the controller.xml after-login event
     * that calls 
     * <p>
     * DEV NOTE: Will be trying out the NON-weak provider for now because it's faster.
     * This requires perfect session listening. In theory it should work fine
     * as the Ofbiz LoginWorker externalLoginKey seems to work fine that way without
     * weak hash keys, but there's a small chance of issues with the listener.
     */
    private static final AccessTokenProvider<GenericValue> tokens = AccessTokenProvider.newAccessTokenProvider();
    
    public CmsAccessHandler() {
    }
    
    public static String getAccessTokenString(HttpServletRequest request, CmsPage cmsPage, String cmsPagePath) {
        // TODO?: we are not currently using the page path or ID (ID may not work; in future a path could lead to different pages)
        // but could try to combine somehow to create more secure per-page tokens, using:
        //   hash(sessiontoken + cmsPagePrimaryPath)
        String tokenString = tokens.getSessionTokenString(request.getSession(), SESSION_TOKEN_ATTR);
        if (tokenString == null) {
            Debug.logWarning("Cms: Access token: No access token string found in session"
                    + " (bad controller config?) - preview mode may fail", module);
        }
        return tokenString;
    }
    
    public static boolean isValidAccessToken(HttpServletRequest request, CmsPage page, String cmsPagePath, String paramToken) {
        // TODO?: in future could want a hash(paramToken + cmsPagePath) or so
        return (tokens.get(paramToken) != null);
    }
    
    public static boolean isValidAccessToken(String internalToken, String paramToken) {
        if (internalToken == null) return false;
        else return internalToken.equals(paramToken);
    }

    @Override
    public void sessionCreated(HttpSessionEvent se) {
        // IMPORTANT: If the session was persisted and deserialized, we must re-add the token to the global hash here
        restoreSessionTokenToProvider(se.getSession());
    }
    
    /**
     * Restores token stored in session to this provider's tokens map.
     * <p>
     * Needed only for persisted session deserialization.
     */
    public static String restoreSessionTokenToProvider(HttpSession session) {
        AccessToken token = tokens.restoreSessionTokenToProvider(session, SESSION_TOKEN_ATTR, 
                (GenericValue) session.getAttribute("userLogin"));
        return (token != null) ? token.toString() : null;
    }

    public static String registerSessionToken(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession(true);
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin == null) {
            Debug.logWarning("Cms: Access Token: userLogin is not set in session"
                    + "; cannot associate the access token with a userLogin"
                    + "- preview mode may fail"
                    + "; is the CmsAccessHandler.registerSessionToken method properly"
                    + " associated to the controller after-login events?", module);
        }
        tokens.updateAndGetSessionToken(session, SESSION_TOKEN_ATTR, userLogin);
        Debug.logInfo("Cms: Access Token: Generated new access token for session " + session.getId()
            + " (userLoginId: " + (userLogin != null ? userLogin.getString("userLoginId") : "[MISSING]") + ")", module);
        
        return "success";
    }
    
    @Override
    public void sessionDestroyed(HttpSessionEvent se) {
        HttpSession session = se.getSession();
        tokens.cleanupSessionToken(session, SESSION_TOKEN_ATTR);
    }

}
