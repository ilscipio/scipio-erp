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
        String tokenString = tokens.getSessionTokenString(request.getSession(), request, SESSION_TOKEN_ATTR);
        if (tokenString == null) {
            Debug.logWarning("Cms: Access token: No access token string found in session"
                    + " (bad controller config?) - preview mode may fail", module);
        }
        return tokenString;
    }
    
    public static boolean isValidAccessToken(HttpServletRequest request, String cmsPagePath, String paramToken) {
        // TODO?: in future could want a hash(paramToken + cmsPagePath) or so
        return (tokens.get(paramToken) != null);
    }
    
    public static boolean isValidAccessToken(String internalToken, String paramToken) {
        if (internalToken == null) return false;
        else return internalToken.equals(paramToken);
    }

    /**
     * Creates a new session access token and registers in the central provider.
     * A userLogin is required in session.
     */
    public static String createNewSessionAccessToken(HttpServletRequest request, HttpServletResponse response) {
        createNewSessionAccessToken(request.getSession(), request);
        return "success";
    }
    
    /**
     * Creates a new session access token and registers in the central provider.
     * A userLogin is required in session.
     */
    public static String createNewSessionAccessToken(HttpSession session, HttpServletRequest request) {
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin != null) {
            // create new token
            AccessToken token = tokens.createNewSessionToken(session, request, SESSION_TOKEN_ATTR, userLogin);
            Debug.logInfo("Cms: Access Token: Generated new CMS access token for session " + session.getId()
                + " (userLoginId: " + userLogin.getString("userLoginId") + ")", module);
            return token.toString();
        } else {
            // we have no user login, but we can get rid of any old lingering token
            tokens.cleanupSessionToken(session, request, SESSION_TOKEN_ATTR);
        }
        return null;
    }
    
    /**
     * Ensures the session token is present in the central provider/registry.
     * Implements the persisted deserialized session handling.
     */
    public static String ensureSessionTokenRegistered(HttpServletRequest request, HttpServletResponse response) {
        ensureSessionTokenRegistered(request.getSession(), request);
        return "success";
    }
    
    /**
     * Ensures the session token is present in the central provider/registry.
     * Implements the persisted deserialized session handling.
     */
    public static String ensureSessionTokenRegistered(HttpSession session, HttpServletRequest request) {
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin != null) {
            // create new token
            AccessToken token = tokens.ensureSessionTokenRegistered(session, request, SESSION_TOKEN_ATTR, userLogin);
            return token.toString();
        }
        return null;
    }
    
    /**
     * Removes the current session access token from session and central provider.
     */
    public static String cleanupSessionAccessToken(HttpServletRequest request, HttpServletResponse response) {
        cleanupSessionAccessToken(request.getSession(), request);
        return "success";
    }
    
    /**
     * Removes the current session access token from session and central provider.
     */
    public static void cleanupSessionAccessToken(HttpSession session, HttpServletRequest request) {
        tokens.cleanupSessionToken(session, request, SESSION_TOKEN_ATTR);
    }

    @Override
    public void sessionCreated(HttpSessionEvent se) {
        // nothing useful is in the session at this point
    }
    
    @Override
    public void sessionDestroyed(HttpSessionEvent se) {
        cleanupSessionAccessToken(se.getSession(), null);
    }

}
