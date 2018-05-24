package com.ilscipio.scipio.cms.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider;
import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider.EventHandler;
import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider.AccessToken;
import com.ilscipio.scipio.cms.content.CmsPage;

/**
 * Generates and cleanups access tokens for CMS, mainly for preview mode.
 * <p>
 * Added 2018-05-06.
 */
public class CmsAccessHandler implements HttpSessionListener {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

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
     * <p>
     * TODO: REVIEW: This uses the non-weak key-based token provider (ConcurrentHashMap)
     * because WeakHashMap is not synchronized; ideally we should use a 3rd-party
     * synchronized WeakHashMap implementation with a fast get(Object) call
     * - the Collections.synchronizedMap(WeakHashMap) has a slow locking get(Object)
     * call so even though it would help prevent memory leaks it's not appropriate.
     */
    private static final AccessTokenProvider<GenericValue> tokens = AccessTokenProvider.newAccessTokenProvider(
            new EventHandler<GenericValue>() {
                @Override
                public void sessionTokenCreated(HttpSession session, HttpServletRequest request, String attrName,
                        AccessToken token, GenericValue initialValue) {
                    Debug.logInfo("Cms: Access Token: Generated new CMS access token for session " + session.getId()
                        + " (userLoginId: " + initialValue.getString("userLoginId") + ")", module);
                }
            });

    public CmsAccessHandler() {
    }

    public static String getAccessTokenString(HttpServletRequest request, CmsPage cmsPage, String cmsPagePath) {
        // TODO?: we are not currently using the page path or ID (ID may not work; in future a path could lead to different pages)
        // but could try to combine somehow to create more secure per-page tokens, using:
        //   hash(sessiontoken + cmsPagePrimaryPath)
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        if (userLogin == null) return null;
        return tokens.getSessionToken(request.getSession(), request, SESSION_TOKEN_ATTR, userLogin).toString();
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
    public static String createSessionAccessToken(HttpServletRequest request, HttpServletResponse response) {
        createSessionAccessToken(request.getSession(), request);
        return "success";
    }

    /**
     * Creates a new session access token and registers in the central provider.
     * A userLogin is required in session.
     */
    public static String createSessionAccessToken(HttpSession session, HttpServletRequest request) {
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin != null) {
            // create new token
            return tokens.createSessionToken(session, request, SESSION_TOKEN_ATTR, userLogin).toString();
        } else {
            // we have no user login, but we can get rid of any old lingering token
            tokens.cleanupSessionToken(session, request, SESSION_TOKEN_ATTR);
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
