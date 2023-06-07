package org.ofbiz.webapp.control;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import javax.servlet.http.HttpServletRequest;
import java.sql.Timestamp;

/**
 * Auto login cookie info with auth token support.
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public class AutoUserLoginInfo {

    private final LoginConfig config;
    private final Delegator delegator;
    private final String userLoginId;
    private final String appId;
    private String authToken;
    private Timestamp authTokenDate;
    private GenericValue userLogin;
    private GenericValue userLoginAppInfo;
    private String cookieValue;
    private Boolean authTokenValid; // cache this so operations are consistent when only read from DB

    protected AutoUserLoginInfo(LoginConfig config, Delegator delegator, String userLoginId, String appId, String authToken) throws GeneralException {
        this.config = config;
        this.delegator = delegator;
        this.userLoginId = UtilValidate.nullIfEmpty((userLoginId != null) ? userLoginId.trim() : null); // FIXME: Extra space should be prevented at creation time, less here
        this.appId = UtilValidate.nullIfEmpty(appId);
        this.authToken = UtilValidate.nullIfEmpty(authToken);
    }

    protected AutoUserLoginInfo(LoginConfig config, Delegator delegator, GenericValue userLogin, String appId) throws GeneralException {
        this.config = config;
        this.delegator = delegator;
        this.userLoginId = userLogin.getString("userLoginId").trim(); // FIXME: Extra space should be prevented at creation time, less here
        this.appId = UtilValidate.nullIfEmpty(appId);
        this.authToken = null;
        this.userLogin = userLogin;
    }

    public static AutoUserLoginInfo from(LoginConfig config, HttpServletRequest request, GenericValue userLogin) throws GeneralException {
        Delegator delegator = Delegator.from(request);
        return new AutoUserLoginInfo(config != null ? config : LoginConfig.from(delegator, request), delegator, userLogin, UtilHttp.getApplicationName(request));
    }

    public static AutoUserLoginInfo parse(LoginConfig config, HttpServletRequest request, String value) throws GeneralException {
        String userLoginId = null;
        String authToken = null;
        if (value != null) {
            int i = value.lastIndexOf("::");
            if (i >= 0) {
                userLoginId = value.substring(0, i);
                authToken = value.substring(i + "::".length());
            } else {
                userLoginId = value;
            }
        }
        Delegator delegator = Delegator.from(request);
        return new AutoUserLoginInfo(config != null ? config : LoginConfig.from(delegator, request),
                delegator, userLoginId, UtilHttp.getApplicationName(request), authToken);
    }

    public LoginConfig getConfig() {
        return config;
    }

    /**
     * Only returns false if cookie value turned out to be empty; otherwise userLoginId will be set and this returns true.
     */
    public boolean hasValue() {
        return (getUserLoginId() != null);
    }

    /**
     * Returns the userLoginId, only valid if {@link #authTokenValid()} was called, and usually non-null unless missing cookie value.
     */
    public String getUserLoginId() {
        return userLoginId;
    }

    public String getAppId() {
        return appId;
    }

    public String getAuthToken(boolean readCreateIfMissing) throws GeneralException {
        if (authToken != null) {
            return authToken;
        } else if (readCreateIfMissing) {
            readCreateAuthToken();
            return authToken;
        }
        return null;
    }

    public Timestamp getAuthTokenDate(boolean readCreateIfMissing) throws GeneralException {
        if (authTokenDate != null) {
            return authTokenDate;
        } else if (readCreateIfMissing) {
            readCreateAuthToken();
            return authTokenDate;
        }
        return null;
    }

    /**
     * Validates that 1) the cookie value matched a real UserLogin.userLoginId and
     * 2) the auth token matches non-expired UserLoginAppInfo.autoLoginAuthToken.
     */
    public boolean authTokenValid() throws GeneralException {
        Boolean authTokenValid = this.authTokenValid;
        if (authTokenValid == null) {
            GenericValue userLoginAppInfo = getUserLoginAppInfo();
            if (userLoginAppInfo == null) {
                authTokenValid = false;
            } else {
                String authToken = getAuthToken(false);
                String storedAuthToken = userLoginAppInfo.getString("autoLoginAuthToken");
                Timestamp storedAuthTokenDate = userLoginAppInfo.getTimestamp("autoLoginAuthDate");
                authTokenValid = (authTokenTimeValid(storedAuthTokenDate) && UtilValidate.isNotEmpty(authToken) && authToken.equals(storedAuthToken));
            }
            this.authTokenValid = authTokenValid;
        }
        return authTokenValid;
    }

    protected boolean authTokenTimeValid(Timestamp authTokenDate) {
        if (authTokenDate == null) {
            return false;
        }
        Timestamp now = UtilDateTime.nowTimestamp();
        Timestamp expireTime = UtilDateTime.addSecondsToTimestamp(authTokenDate, getConfig().getAutoUserLoginAuthTokenExpireTime());
        return now.before(expireTime);
    }

    public GenericValue getUserLogin() throws GeneralException {
        GenericValue userLogin = this.userLogin;
        if (userLogin == null) {
            String userLoginId = getUserLoginId();
            if (userLoginId != null) {
                userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", userLoginId).queryOne();
            }
            this.userLogin = userLogin;
        }
        return userLogin;
    }

    public GenericValue getUserLoginAppInfo() throws GeneralException {
        GenericValue userLoginAppInfo = this.userLoginAppInfo;
        if (userLoginAppInfo == null) {
            String userLoginId = getUserLoginId();
            String appId = getAppId();
            if (userLoginId != null && appId != null) {
                userLoginAppInfo = delegator.query().from("UserLoginAppInfo")
                        .where("userLoginId", userLoginId, "appId", appId).queryOne();
            }
            this.userLoginAppInfo = userLoginAppInfo;
        }
        return userLoginAppInfo;
    }

    public boolean regenerateAuthToken() throws GeneralException {
        return readCreateAuthToken(true);
    }

    public boolean readCreateAuthToken() throws GeneralException {
        return readCreateAuthToken(false);
    }

    private boolean readCreateAuthToken(boolean regenerate) throws GeneralException {
        GenericValue userLoginAppInfo = getUserLoginAppInfo();
        String authToken;
        Timestamp authTokenDate;
        if (userLoginAppInfo != null) {
            if (!regenerate) {
                authToken = userLoginAppInfo.getString("autoLoginAuthToken");
                authTokenDate = userLoginAppInfo.getTimestamp("autoLoginAuthDate");
                if (authToken != null && authTokenDate != null && authTokenTimeValid(authTokenDate)) {
                    // Update this anyway
                    this.authToken = authToken;
                    this.authTokenDate = authTokenDate;
                    this.authTokenValid = true;
                    return false;
                }
            }
            authToken = generateAuthToken(getConfig());
            authTokenDate = UtilDateTime.nowTimestamp();
            userLoginAppInfo.setFields("autoLoginAuthToken", authToken, "autoLoginAuthDate", authTokenDate);
            userLoginAppInfo.store();
        } else {
            authToken = generateAuthToken(getConfig());
            authTokenDate = UtilDateTime.nowTimestamp();
            userLoginAppInfo = delegator.makeValue("UserLoginAppInfo",
                    "userLoginId", getUserLoginId(), "appId", getAppId(),
                    "autoLoginAuthToken", authToken, "autoLoginAuthDate", authTokenDate).createOrStore();
            this.userLoginAppInfo = userLoginAppInfo;
        }
        this.authToken = authToken;
        this.authTokenDate = authTokenDate;
        this.authTokenValid = true;
        return true;
    }

    public String toCookieValue() throws GeneralException {
        String cookieValue = this.cookieValue;
        if (cookieValue == null) {
            String userLoginId = getUserLoginId();
            String authToken = getAuthToken(true);
            if (userLoginId == null || authToken == null) {
                throw new IllegalStateException("autoUserLogin missing auth token (internal error)"); // should not happen
            }
            cookieValue = userLoginId + "::" + authToken;
            this.cookieValue = cookieValue;
        }
        return cookieValue;
    }

    public String toCookieRemovalValue() {
        return getUserLoginId();
    }

    public static String generateAuthToken(LoginConfig config) {
        return UtilRandom.generateAlphaNumericString(config.getAutoUserLoginAuthTokenLength());
    }

}
