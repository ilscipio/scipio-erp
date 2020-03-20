package com.ilscipio.scipio.cms.control;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;
import javax.transaction.Transaction;

import com.ibm.icu.util.Calendar;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider;
import com.ilscipio.scipio.ce.webapp.control.util.AccessTokenProvider.EventHandler;
import com.ilscipio.scipio.cms.content.CmsPage;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.transaction.TransactionUtil;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * Generates and cleanups access tokens for CMS, mainly for preview mode.
 * <p>
 * Added 2018-05-06.
 */
public class CmsAccessHandler implements HttpSessionListener {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final int TOKEN_EXPIRY = UtilProperties.getPropertyAsInteger("cms", "cms.access.token.expiry", 60 * 60 * 24); // in seconds
    private static final int TOKEN_HALFLIFE = UtilProperties.getPropertyAsInteger("cms", "cms.access.token.halfLife", 60 * 60 * 6); // in seconds

    // 2020-03-12: This is now only used to generate new tokens, otherwise the token is handled in DB.
    private static final AccessTokenProvider<GenericValue> tokenProvider = AccessTokenProvider.newAccessTokenProvider(new EventHandler<GenericValue>() {});

    public CmsAccessHandler() {
    }

    public static String getAccessTokenString(HttpServletRequest request, CmsPage cmsPage, String cmsPagePath) {
        if (cmsPage == null || cmsPage.getId() == null) {
            return null;
        }
        GenericValue tokenRecord = getOrCreateUserAccessToken(UtilHttp.getSessionUserLogin(request), cmsPage.getId(), null, true);
        return (tokenRecord != null) ? tokenRecord.getString("token") : null;
    }

    private static GenericValue getOrCreateUserAccessToken(GenericValue userLogin, String pageId, Timestamp nowTimestamp, boolean deleteExpired) {
        if (userLogin == null) {
            return null;
        }
        Delegator delegator = userLogin.getDelegator();
        if (nowTimestamp == null) {
            nowTimestamp = UtilDateTime.nowTimestamp();
        }
        String userId = userLogin.getString("userLoginId");
        List<GenericValue> tokens = delegator.from("CmsAccessToken").where("userId", userId, "pageId", pageId).queryListSafe();
        if (UtilValidate.isNotEmpty(tokens)) {
            GenericValue activeToken = null;
            List<GenericValue> expiredTokens = null;
            for(GenericValue token : tokens) {
                if (activeToken == null && isActiveAccessToken(token, nowTimestamp, TOKEN_HALFLIFE)) { // for token generation, use half-life
                    activeToken = token;
                    if (!deleteExpired) {
                        return activeToken;
                    }
                } else if (deleteExpired && !isActiveAccessToken(token, nowTimestamp, TOKEN_EXPIRY)) {
                    if (expiredTokens == null) {
                        expiredTokens = new ArrayList<>();
                    }
                    expiredTokens.add(token);
                }
            }
            if (expiredTokens != null) {
                // delete expired tokens, in separate transaction to not affect screens
                String errMsg = "removing " + expiredTokens.size() + " expired access tokens for user '"
                        + userId + "'" + (pageId != null ? " for page '" + pageId + "'" : "");
                Debug.logInfo("Cms: getOrCreateUserAccessToken: " + errMsg, module);
                final List<GenericValue> expiredTokensFinal = expiredTokens;
                try {
                    TransactionUtil.doNewTransaction(new Callable<Object>() {
                        @Override
                        public Object call() throws Exception {
                            return delegator.removeAll(expiredTokensFinal);
                        }
                    }, "Cms: Error " + errMsg, 0, true);
                } catch (Exception e) {
                    Debug.logError(e, module);
                }
            }
        }
        try {
            GenericValue tokenRecord = delegator.makeValue("CmsAccessToken",
                    "token", tokenProvider.newTokenString(), "userId", userId, "pageId", pageId, "createdDate", nowTimestamp);
            return delegator.createSetNextSeqId(tokenRecord);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Cms: Access Token: Could not create user access token for user '" + userId + "', page '" + pageId + "'", module);
            return null;
        }
    }

    public static boolean isValidAccessToken(HttpServletRequest request, Delegator delegator, String cmsPagePath, String paramToken) {
        return isValidAccessToken(delegator, cmsPagePath, paramToken);
    }

    public static boolean isValidAccessToken(Delegator delegator, String cmsPagePath, String paramToken) {
        if (paramToken == null || paramToken.isEmpty()) {
            return false;
        }
        // TODO: REVIEW: Here we can query only by token and not by pageId because the page is not yet known,
        //  but this has minimal impact on security because the pageId is mostly needed to provide variety in token strings rather than bind to any specific page.
        //  If we try to do the pageId here it may cause complications for process mappings in the future.
        GenericValue tokenRecord = delegator.from("CmsAccessToken").where("token", paramToken).queryOneSafe();
        if (tokenRecord == null) {
            return false;
        }
        if (!isActiveAccessToken(tokenRecord, UtilDateTime.nowTimestamp(), TOKEN_EXPIRY)) { // for token validation, use expiry
            Debug.logWarning("Cms: Access Token: Expired token '" + tokenRecord.get("tokenId") + "' accessed; denying", module);
            return false;
        }
        return true;
    }

    private static boolean isActiveAccessToken(GenericValue tokenRecord, Timestamp nowTimestamp, int maxTime) {
        return (nowTimestamp.getTime() - tokenRecord.getTimestamp("createdDate").getTime()) < (maxTime * 1000);
    }

    public static String cleanupUserAccessTokens(HttpServletRequest request, HttpServletResponse response) {
        cleanupUserAccessTokens(UtilHttp.getSessionUserLogin(request), null, null);
        return "success";
    }

    public static void cleanupUserAccessTokens(GenericValue userLogin, String pageId, Timestamp nowTimestamp) {
        if (userLogin == null) {
            return;
        }
        Delegator delegator = userLogin.getDelegator();
        if (nowTimestamp == null) {
            nowTimestamp = UtilDateTime.nowTimestamp();
        }
        String userId = userLogin.getString("userLoginId");
        List<EntityCondition> conds = UtilMisc.toList(EntityCondition.makeCondition("userId", userId),
                EntityCondition.makeCondition("createdDate", EntityOperator.LESS_THAN, UtilDateTime.adjustTimestamp(nowTimestamp, Calendar.SECOND, -TOKEN_EXPIRY)));
        if (pageId != null) {
            conds.add(EntityCondition.makeCondition("pageId", userLogin.get("pageId")));
        }
        List<GenericValue> tokens = delegator.from("CmsAccessToken").where(conds).queryListSafe();
        if (UtilValidate.isEmpty(tokens)) {
            return;
        }
        Debug.logInfo("Cms: cleanupUserAccessTokens: Removing " + tokens.size() + " expired access tokens for user '"
                + userId + "'" + (pageId != null ? " for page '" + pageId + "'" : ""), module);
        try {
            delegator.removeAll(tokens);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
    }

    @Override
    public void sessionCreated(HttpSessionEvent se) {
        // nothing useful is in the session at this point
    }

    @Override
    public void sessionDestroyed(HttpSessionEvent se) {
    }
}