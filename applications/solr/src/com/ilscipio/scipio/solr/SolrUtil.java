package com.ilscipio.scipio.solr;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transaction;

import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.request.SolrPing;
import org.apache.solr.client.solrj.response.SolrPingResponse;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.start.Start.ServerState;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;

import com.ilscipio.scipio.solr.util.ScipioHttpSolrClient;

/**
 * Generic Solr utility class, and helpers to get HttpSolrClient for the Scipio Solr instance.
 */
public abstract class SolrUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String solrConfigName = "solrconfig";

    private static final boolean solrEnabled = getSolrSysPropCfgBool("enabled", true);
    private static final boolean solrEcaEnabled = getSolrSysPropCfgBool("eca.enabled", false);
    private static final boolean solrWebappCheckEnabled = getSolrSysPropCfgBool("eca.useSolrWebappLoadedCheck", true);

    private static final String solrWebappProtocol = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.protocol");
    private static final String solrWebappHost = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.domainName");
    private static final Integer solrWebappPort = readSolrWebappPort();
    private static final String solrWebappPath = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.path", "/solr");
    private static final String solrWebappServer = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.server", "default-server");
    private static final String solrDefaultCore = UtilProperties.getPropertyValue(solrConfigName, "solr.core.default");

    private static final String effectiveConfigVersion = determineSolrConfigVersion(
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version"),
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version.custom"),
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version.extra"));

    /**
     * @deprecated use {@link #getSolrWebappUrl} instead.
     */
    @Deprecated
    public static final String solrUrl = makeSolrWebappUrl();
    /**
     * @deprecated use {@link #getSolrDefaultCoreUrl} instead.
     */
    @Deprecated
    public static final String solrFullUrl = makeSolrDefaultCoreUrl();

    public static String getSolrConfigName() {
        return solrConfigName;
    }

    /**
     * Returns the EFFECTIVE Solr config version, which is a combination
     * of the solrconfig.properties values
     * <code>solr.config.version</code>,
     * <code>solr.config.version.custom</code> and
     * <code>solr.config.version.extra</code>.
     */
    public static String getSolrConfigVersionStatic() {
        return effectiveConfigVersion;
    }

    public static String determineSolrConfigVersion(String baseVersion, String customVersion, String extraVersion) {
        String version = baseVersion;
        if (UtilValidate.isNotEmpty(customVersion)) {
            version = customVersion;
        }
        if (UtilValidate.isNotEmpty(extraVersion)) {
            if (extraVersion.startsWith(".")) version += extraVersion;
            else version += "." + extraVersion;
        }
        return version;
    }

    public static String getSolrDefaultCore() {
        return solrDefaultCore;
    }

    public static String getSolrWebappProtocol() {
        return solrWebappProtocol;
    }

    public static String getSolrWebappHost() {
        return solrWebappHost;
    }

    public static int getSolrWebappPort() {
        return solrWebappPort;
    }

    private static int readSolrWebappPort() {
        Integer solrPort = UtilProperties.getPropertyAsInteger(solrConfigName, "solr.webapp.portOverride", null);
        if (solrPort != null) {
            return solrPort;
        }
        solrPort = LocalUrlUtil.getWebappContainerPort(solrWebappProtocol);
        if (solrPort != null) {
            return solrPort;
        }
        solrPort = LocalUrlUtil.getStandardPort(solrWebappProtocol);
        Debug.logWarning("Solr: Could not determine a solr webapp port from " + solrConfigName
                + ", url.properties or container; using default: " + solrPort, module);
        return solrPort;
    }

    public static boolean isSolrWebappLocal() {
        if (!LocalUrlUtil.isLocalhost(getSolrWebappHost())) {
            return false;
        }
        return LocalUrlUtil.isWebappContainerPort(getSolrWebappPort());
    }

    public static String getSolrWebappPath() {
        return solrWebappPath;
    }

    public static String getSolrWebappUrl() {
        return solrUrl;
    }

    private static String makeSolrWebappUrl() {
        StringBuilder sb = new StringBuilder();
        sb.append(getSolrWebappProtocol());
        sb.append("://");
        sb.append(getSolrWebappHost());
        if (!LocalUrlUtil.isStandardPort(getSolrWebappPort(), getSolrWebappProtocol())) {
            sb.append(":");
            sb.append(getSolrWebappPort());
        }
        sb.append(getSolrWebappPath());
        if (sb.charAt(sb.length() - 1) == '/') {
            sb.setLength(sb.length() - 1);
        }
        Debug.logInfo("Solr: Determined Solr webapp URL: " + sb.toString(), module);
        return sb.toString();
    }

    public static String getSolrCoreUrl(String core) {
        return getSolrWebappUrl() + "/" + core;
    }

    public static String getSolrDefaultCoreUrl() {
        return solrFullUrl;
    }

    private static String makeSolrDefaultCoreUrl() {
        return getSolrCoreUrl(solrDefaultCore);
    }

    /**
     * @deprecated bad name; use {@link #getSolrDefaultCoreUrl()} instead.
     */
    @Deprecated
    public static String makeFullSolrWebappUrl() {
        return makeSolrDefaultCoreUrl();
    }

    public static WebappInfo getSolrWebappInfo() {
        WebappInfo solrApp = null;
        try {
            ComponentConfig cc = ComponentConfig.getComponentConfig("solr");
            for(WebappInfo currApp : cc.getWebappInfos()) {
                if ("solr".equals(currApp.getName())) {
                    solrApp = currApp;
                    break;
                }
            }
        } catch(ComponentException e) {
            throw new IllegalStateException(e);
        }
        return solrApp;
    }

    private static boolean getSolrSysPropCfgBool(String name, boolean defaultValue) {
        Boolean value = UtilMisc.booleanValueVersatile(System.getProperty("scipio.solr."+name));
        if (value == null) {
            value = UtilMisc.booleanValueVersatile(System.getProperty("ofbiz.solr."+name));
            if (value == null) {
                value = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr."+name, defaultValue);
            }
        }
        return value;
    }

    public static boolean isSolrEnabled() {
        return solrEnabled;
    }

    public static boolean isSolrEcaEnabled(Delegator delegator) {
        return UtilProperties.asBoolean(getSolrEcaEnabledSystemProperty(delegator), solrEcaEnabled);
    }

    public static String getSolrEcaEnabledSystemProperty(Delegator delegator) {
        return EntityUtilProperties.getSystemPropertyValueOrNull(solrConfigName, "solr.eca.enabled", delegator);
    }

    /**
     * Checks if solr is enabled and scipio is initialized for solr and the system/server
     * is in a running state in which queries may be attempted against the solr webapp.
     * <p>
     * NOTE: true does NOT mean that solr webapp is fully loaded and pingable,
     * because the webapp may be remote and such a check must be done physically using
     * waitSolrReady service, {@link #isSolrWebappPingOk()} or {@link #isSolrWebappReady()}.
     */
    public static boolean isSystemInitialized() {
        return isSolrEnabled() && (Start.getInstance().getCurrentState() == ServerState.RUNNING);
    }

    /**
     * Same as {@link #isSystemInitialized()} but assumes solr is enabled.
     */
    public static boolean isSystemInitializedAssumeEnabled() {
        return (Start.getInstance().getCurrentState() == ServerState.RUNNING);
    }

    /**
     * Returns true if the LOCAL Solr webapp has reached initialization.
     */
    public static boolean isSolrLocalWebappStarted() {
        return ScipioSolrInfoServlet.isServletInitStatusReached();
    }

    /**
     * Returns true if the LOCAL Solr webapp is initialized.
     * @deprecated 2018-05-25: ambiguous; use {@link #isSystemInitialized()} or {@link #isSolrLocalWebappStarted()}.
     */
    @Deprecated
    public static boolean isSolrWebappInitialized() {
        return isSolrLocalWebappStarted();
    }

    /**
     * If the solr webapp/initialization check if enabled in config,
     * returns {@link #isSystemInitialized()}, otherwise returns true.
     */
    public static boolean isSolrEcaWebappInitCheckPassed() {
        return (solrWebappCheckEnabled) ? isSystemInitialized() : true;
    }

    public static boolean isEcaTreatConnectErrorNonFatal() {
        Boolean treatConnectErrorNonFatal = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.treatConnectErrorNonFatal", true);
        return Boolean.TRUE.equals(treatConnectErrorNonFatal);
    }

    /**
     * Returns true if the LOCAL Solr webapp is present in the system (by context root),
     * as defined in applications/solr/ofbiz-component.xml.
     * <p>
     * For stock Scipio configuration, this always returns true.
     * It only returns false if the webapp is disabled/commented in ofbiz-component.xml.
     * <p>
     * WARN: Do not call this before or while components are being read.
     */
    public static boolean isSolrLocalWebappPresent() {
        return (ComponentConfig.getWebAppInfo(solrWebappServer, solrWebappPath) != null);
    }

    /**
     * Returns true if the LOCAL Solr webapp is present in the system (by context root).
     * @deprecated 2018-05-25: ambiguous; use {@link #isSystemInitialized()} or {@link #isSolrLocalWebappPresent()}.
     */
    @Deprecated
    public static boolean isSolrWebappPresent() {
        return isSolrLocalWebappPresent();
    }

    /**
     * Returns true if the LOCAL Solr webapp is present in the system (by context root) and running state.
     * @deprecated 2018-05-25: ambiguous; use {@link #isSystemInitialized()} or {@link #isSolrLocalWebappPresent()}.
     */
    @Deprecated
    public static boolean isSolrWebappEnabled() {
        return isSolrEnabled() && isSolrLocalWebappPresent();
    }

    public static boolean isSolrWebappPingOk(HttpSolrClient client) throws Exception {
        try {
            return isSolrWebappPingOkRaw(client);
        } catch(Exception e) {
            // FIXME: we are not supposed to catch this, but in current setup with Tomcat
            // solr can't handle the incomplete loading 503 and throws exception, so have no choice
            // but because this is only a Ping, we can usually assume this means not yet loaded...
            Debug.logInfo("Solr: isSolrWebappPingOk: Solr webapp not pingable; exception: " + e.getMessage(), module);
            return false;
        }
    }

    public static boolean isSolrWebappPingOkRaw(HttpSolrClient client) throws Exception {
        SolrPing solrPing = new SolrPing();
        SolrPingResponse rsp = solrPing.process(client);
        int status = rsp.getStatus();
        if (status == 0) {
            if (Debug.verboseOn()) Debug.logVerbose("isSolrWebappPingOk: ping response status: " + status, module);
            return true;
        } else {
            Debug.logInfo("Solr: isSolrWebappPingOk: Solr webapp not pingable; status: " + status, module);
            return false;
        }
    }

    /**
     * Returns true if Solr is loaded and available for queries.
     */
    public static boolean isSolrWebappReady(HttpSolrClient client) throws Exception {
        return isSystemInitialized() && isSolrWebappPingOk(client);
    }

    /**
     * Returns true if Solr is loaded and available for queries, using default core and client.
     */
    public static boolean isSolrWebappReady() throws Exception {
        return isSolrWebappReady(getQueryHttpSolrClient(null));
    }

    public static boolean isSolrWebappReadyRaw(HttpSolrClient client) throws Exception {
        return isSystemInitialized() && isSolrWebappPingOkRaw(client);
    }

    public static boolean isSolrWebappReadyRaw() throws Exception {
        return isSolrWebappReadyRaw(getQueryHttpSolrClient(null));
    }

    public static GenericValue getSolrStatus(Delegator delegator) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                    .where("solrId", "SOLR-MAIN").cache(false).queryOne();
            if (solrStatus == null) {
                Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - seed data missing?", module);
            } else {
                return solrStatus;
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return null;
    }

    public static String getSolrDataStatusId(Delegator delegator) {
        GenericValue solrStatus = getSolrStatus(delegator);
        return solrStatus != null ? solrStatus.getString("dataStatusId") : null;
    }

    public static void setSolrDataStatusId(Delegator delegator, String dataStatusId, boolean updateVersion) throws GenericEntityException {
        GenericValue solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                .where("solrId", "SOLR-MAIN").cache(false).queryOne();
        //solrStatus = delegator.findOne("SolrStatus", UtilMisc.toMap("solrId", "SOLR-MAIN"), false);
        if (solrStatus == null) {
            Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - creating new", module);
            solrStatus = delegator.create("SolrStatus",
                    "solrId", "SOLR-MAIN",
                    "dataStatusId", dataStatusId,
                    "dataCfgVersion", getSolrConfigVersionStatic());
        } else {
            solrStatus.setString("dataStatusId", dataStatusId);
            if (updateVersion) {
                solrStatus.setString("dataCfgVersion", getSolrConfigVersionStatic());
            }
            solrStatus.store();
        }
    }

    public static void setSolrDataStatusIdSafe(Delegator delegator, String dataStatusId, boolean updateVersion) {
        try {
            setSolrDataStatusId(delegator, dataStatusId, updateVersion);
        } catch (Exception t) {
            final String errMsg = "Error while trying to mark Solr data status (" + dataStatusId + "): " + t.getMessage();
            Debug.logError(t, "Solr: " + errMsg, module);
        }
    }

    public static void setSolrDataStatusId(Delegator delegator, String dataStatusId) throws GenericEntityException {
        setSolrDataStatusId(delegator, dataStatusId, false);
    }

    // DEV NOTE: could have done this with service call, but just in case need fine-grained control...
    public static boolean setSolrDataStatusIdSepTxSafe(Delegator delegator, String dataStatusId, boolean updateVersion) {
        Transaction parentTransaction = null;
        boolean beganTrans = false;
        try {
            try {
                if (TransactionUtil.isTransactionInPlace()) {
                    parentTransaction = TransactionUtil.suspend();
                    // now start a new transaction
                    beganTrans = TransactionUtil.begin();
                } else {
                    beganTrans = TransactionUtil.begin();
                }
            } catch (GenericTransactionException t) {
                Debug.logError(t, "Solr: Cannot create transaction to mark Solr data status (" + dataStatusId + ")", module);
            }
            try {
                SolrUtil.setSolrDataStatusId(delegator, dataStatusId, updateVersion);
                return true;
            } catch (Throwable t) {
                final String errMsg = "Error while trying to mark Solr data status (" + dataStatusId + "): " + t.getMessage();
                Debug.logError(t, "Solr: " + errMsg, module);
                try {
                    TransactionUtil.rollback(beganTrans, errMsg, t);
                } catch (GenericTransactionException te) {
                    Debug.logError(te, "Solr: Cannot rollback transaction to mark Solr data status (" + dataStatusId + ")", module);
                }
            } finally {
                try {
                    TransactionUtil.commit(beganTrans);
                } catch (GenericTransactionException e) {
                    Debug.logError(e, "Solr: Could not commit transaction to mark Solr data status (" + dataStatusId + ")", module);
                }
            }
        } finally {
            if (parentTransaction != null) {
                try {
                    TransactionUtil.resume(parentTransaction);
                } catch (GenericTransactionException t) {
                    Debug.logError(t, "Solr: Error resuming parent transaction after marking Solr data status (" + dataStatusId + ")", module);
                }
            }
        }
        return false;
    }

    /**
     * Returns a Solr client for making read-only queries, for given core or default core (if null).
     * <p>
     * Supports explicit solr basic auth username/password, if not empty; otherwise
     * includes the basic auth defined in solrconfig.properties/solr.query.login.*,
     * which can be further overridden on individual requests (using QueryRequest).
     */
    public static HttpSolrClient getQueryHttpSolrClient(String core, String solrUsername, String solrPassword) {
        return SolrClientFactory.queryClientFactory.getClientForCore(core, solrUsername, solrPassword);
    }

    /**
     * Returns a Solr client for making read-only queries, for given core or default core (if null).
     * <p>
     * This client includes the basic auth defined in solrconfig.properties/solr.query.login.*,
     * which can be overridden on individual requests (using QueryRequest).
     */
    public static HttpSolrClient getQueryHttpSolrClient(String core) {
        return SolrClientFactory.queryClientFactory.getClientForCore(core, null, null);
    }

    public static HttpSolrClient getQueryHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.queryClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    public static HttpSolrClient getQueryHttpSolrClientFromUrl(String url) {
        return SolrClientFactory.queryClientFactory.getClientFromUrl(url, null, null);
    }

    /**
     * Returns a Solr client for making update/indexing queries, for given core or default core (if null).
     * <p>
     * Supports explicit solr basic auth username/password, if not empty; otherwise
     * includes the basic auth defined in solrconfig.properties/solr.update.login.*,
     * which can be further overridden on individual requests (using QueryRequest).
     */
    public static HttpSolrClient getUpdateHttpSolrClient(String core, String solrUsername, String solrPassword) {
        return SolrClientFactory.updateClientFactory.getClientForCore(core, solrUsername, solrPassword);
    }

    /**
     * Returns a Solr client for making update/indexing queries, for given core or default core (if null).
     * <p>
     * This client includes the basic auth defined in solrconfig.properties/solr.update.login.*,
     * which can be overridden on individual requests (using UpdateRequest).
     */
    public static HttpSolrClient getUpdateHttpSolrClient(String core) {
        return SolrClientFactory.updateClientFactory.getClientForCore(core, null, null);
    }

    public static HttpSolrClient getUpdateHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.updateClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    public static HttpSolrClient getUpdateHttpSolrClientFromUrl(String url) {
        return SolrClientFactory.updateClientFactory.getClientFromUrl(url, null, null);
    }

    /**
     * Returns a Solr client for making admin queries, for given core or default core (if null).
     * <p>
     * Supports explicit solr basic auth username/password, if not empty; otherwise
     * includes the basic auth defined in solrconfig.properties/solr.admin.login.*,
     * which can be further overridden on individual requests.
     */
    public static HttpSolrClient getAdminHttpSolrClient(String core, String solrUsername, String solrPassword) {
        return SolrClientFactory.adminClientFactory.getClientForCore(core, solrUsername, solrPassword);
    }

    /**
     * Returns a Solr client for making admin queries, for given core or default core (if null).
     * <p>
     * This client includes the basic auth defined in solrconfig.properties/solr.admin.login.*,
     * which can be overridden on individual requests.
     */
    public static HttpSolrClient getAdminHttpSolrClient(String core) {
        return SolrClientFactory.adminClientFactory.getClientForCore(core, null, null);
    }

    public static HttpSolrClient getAdminHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.adminClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    public static HttpSolrClient getAdminHttpSolrClientFromUrl(String url) {
        return SolrClientFactory.adminClientFactory.getClientFromUrl(url, null, null);
    }

    /**
     * Returns a Solr client for making read-only queries.
     * @deprecated 2018-04-17: now ambiguous; use {@link #getQueryHttpSolrClient(String) instead
     */
    @Deprecated
    public static HttpSolrClient getHttpSolrClient(String core) {
        return getQueryHttpSolrClient(core);
    }

    /**
     * Returns a Solr client for making read-only queries.
     * @deprecated 2018-04-17: now ambiguous; use {@link #getQueryHttpSolrClient(String) instead
     */
    @Deprecated
    public static HttpSolrClient getHttpSolrClient() {
        return getQueryHttpSolrClient(null);
    }

    /**
     * Returns a new Solr client for making read-only queries.
     * @deprecated 2018-04-27: now ambiguous; use {@link #makeQueryHttpSolrClient(String) instead
     */
    @Deprecated
    public static HttpSolrClient makeHttpSolrClientFromUrl(String url) {
        // use query username/password for backward compat, but this is not guaranteed to work
        return makeQueryHttpSolrClientFromUrl(url, getSolrQueryConnectConfig().getSolrUsername(), getSolrQueryConnectConfig().getSolrPassword());
    }

    public static HttpSolrClient makeQueryHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.newQueryClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    public static HttpSolrClient makeUpdateHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.newUpdateClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    public static HttpSolrClient makeAdminHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.newAdminClientFactory.getClientFromUrl(url, solrUsername, solrPassword);
    }

    static SolrConnectConfig getSolrQueryConnectConfig() {
        return SolrConnectConfig.queryConnectConfig;
    }

    static SolrConnectConfig getSolrUpdateConnectConfig() {
        return SolrConnectConfig.updateConnectConfig;
    }

    static SolrConnectConfig getSolrAdminConnectConfig() {
        return SolrConnectConfig.adminConnectConfig;
    }

    public enum SolrClientMode {
        QUERY("query"),
        UPDATE("update"),
        ADMIN("admin");

        private final String name;

        private SolrClientMode(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public static class SolrConnectConfig {
        private static final SolrConnectConfig queryConnectConfig = SolrConnectConfig.fromProperties(SolrClientMode.QUERY, solrConfigName, "solr.query.connect.");
        private static final SolrConnectConfig updateConnectConfig = SolrConnectConfig.fromProperties(SolrClientMode.UPDATE, solrConfigName, "solr.update.connect.");
        private static final SolrConnectConfig adminConnectConfig = SolrConnectConfig.fromProperties(SolrClientMode.ADMIN, solrConfigName, "solr.admin.connect.");

        private final SolrClientMode clientMode;
        private final String solrUsername;
        private final String solrPassword;
        private final boolean reuseClient;
        private final Integer connectTimeout;
        private final Integer socketTimeout;
        //private final Boolean noDelay; // TODO: FLAG NOT IMPLEMENTED (client building issue)
        private final Integer maxConnections;
        private final Integer maxConnectionsPerHost;

        public SolrConnectConfig(SolrClientMode clientMode, Map<String, ?> configMap) {
            this.clientMode = clientMode;
            this.solrUsername = UtilProperties.valueOrNull((String) configMap.get("login.username"));
            this.solrPassword = UtilProperties.valueOrNull((String) configMap.get("login.password"));
            this.reuseClient = UtilProperties.asBoolean(configMap.get("reuseClient"), false);
            this.connectTimeout = UtilProperties.asInteger(configMap.get("connectTimeout"));
            this.socketTimeout = UtilProperties.asInteger(configMap.get("socketTimeout"));
            //this.noDelay = UtilProperties.asBoolean(configMap.get("noDelay"));
            // NOTE: Defaults are based on Solr 7 (not 6) - see org.apache.solr.client.solrj.impl.HttpClientUtil source
            // For Solr 6, the defaults were 128 and 32, respectively
            this.maxConnections = UtilProperties.asInteger(configMap.get("maxConnections"), 10000);
            this.maxConnectionsPerHost = UtilProperties.asInteger(configMap.get("maxConnectionsPerHost"), 10000);
        }

        public static SolrConnectConfig fromProperties(SolrClientMode clientMode, String resource, String prefix) {
            return new SolrConnectConfig(clientMode,
                    UtilProperties.getPropertiesWithPrefix(UtilProperties.getProperties(resource), prefix));
        }

        public SolrClientMode getClientMode() { return clientMode; }
        public String getSolrUsername() { return solrUsername; }
        public String getSolrPassword() { return solrPassword; }
        public boolean isReuseClient() { return reuseClient; }
        public String getSolrCore() { return SolrUtil.getSolrDefaultCore(); }
        public String getSolrCoreUrl() { return SolrUtil.getSolrDefaultCoreUrl(); }
        public Integer getConnectTimeout() { return connectTimeout; }
        public Integer getSocketTimeout() { return socketTimeout; }
        //public Boolean getNoDelay() { return noDelay; }
        public Integer getMaxConnections() { return maxConnections; }
        public Integer getMaxConnectionsPerHost() { return maxConnectionsPerHost; }

        /**
         * Sets basic Solr auth on the given request.
         * <p>
         * NOTE: 2018-04-17: This should largely not be needed now; it should be
         * already handled in ScipioHttpSolrClient returned by {@link #getQueryHttpSolrClient},
         * although doing it in double here on the SolrRequest will not cause any problems.
         */
        public void setSolrRequestAuth(SolrRequest<?> req, String solrUsername, String solrPassword) {
            if (solrUsername == null) {
                solrUsername = this.getSolrUsername();
                solrPassword = this.getSolrPassword();
            }
            if (solrUsername != null) {
                req.setBasicAuthCredentials(solrUsername, solrPassword);
            }
        }

        public void setSolrRequestAuth(SolrRequest<?> req) {
            String solrUsername = this.getSolrUsername();
            if (solrUsername != null) {
                String solrPassword = this.getSolrPassword();
                req.setBasicAuthCredentials(solrUsername, solrPassword);
            }
        }

        private String makeClientLogDesc(String url, String solrUsername) {
            return getClientMode().getName()
                    + " client: " + (solrUsername != null ? solrUsername + "@" : "") + url
                    + (connectTimeout != null ? " (timeout: " + connectTimeout + ")" : "")
                    + (socketTimeout != null ? " (sotimeout: " + socketTimeout + ")" : "")
                    + (maxConnections != null ? " (maxconn: " + maxConnections + ")" : "")
                    + (maxConnectionsPerHost != null ? " (hostmaxconn: " + maxConnectionsPerHost + ")" : "");
        }
    }

    private static abstract class SolrClientFactory {

        /**
         * Factory that always creates a new client.
         */
        static final SolrClientFactory newQueryClientFactory = NewSolrClientFactory.create(SolrConnectConfig.queryConnectConfig);
        static final SolrClientFactory newUpdateClientFactory = NewSolrClientFactory.create(SolrConnectConfig.updateConnectConfig);
        static final SolrClientFactory newAdminClientFactory = NewSolrClientFactory.create(SolrConnectConfig.adminConnectConfig);

        /**
         * Abstracted factory that gets a cached or new client.
         */
        static final SolrClientFactory queryClientFactory = create(SolrConnectConfig.queryConnectConfig);
        static final SolrClientFactory updateClientFactory = create(SolrConnectConfig.updateConnectConfig);
        static final SolrClientFactory adminClientFactory = create(SolrConnectConfig.adminConnectConfig);

        public static SolrClientFactory create(SolrConnectConfig connectConfig) {
            if (connectConfig.isReuseClient()) return CachedSolrClientFactory.create(connectConfig);
            else return NewSolrClientFactory.create(connectConfig);
        }

        public abstract HttpSolrClient getClientFromUrlRaw(String url, String solrUsername, String solrPassword);

        public HttpSolrClient getClientFromUrl(String url, String solrUsername, String solrPassword) {
            if (UtilValidate.isEmpty(solrUsername)) {
                solrUsername = getConnectConfig().getSolrUsername();
                solrPassword = getConnectConfig().getSolrPassword();
            } else if ("_none_".equals(solrUsername)) {
                solrUsername = null;
                solrPassword = null;
            }
            return getClientFromUrlRaw(url, solrUsername, solrPassword);
        }

        public HttpSolrClient getClientForCore(String core, String solrUsername, String solrPassword) {
            if (UtilValidate.isEmpty(solrUsername)) {
                solrUsername = getConnectConfig().getSolrUsername();
                solrPassword = getConnectConfig().getSolrPassword();
            } else if ("_none_".equals(solrUsername)) {
                solrUsername = null;
                solrPassword = null;
            }
            if (UtilValidate.isNotEmpty(core)) {
                return getClientFromUrlRaw(getSolrCoreUrl(core), solrUsername, solrPassword);
            } else {
                return getClientFromUrlRaw(getSolrDefaultCoreUrl(), solrUsername, solrPassword);
            }
        }

        public abstract SolrConnectConfig getConnectConfig();

        static class NewSolrClientFactory extends SolrClientFactory {
            private final SolrConnectConfig connectConfig;

            NewSolrClientFactory(SolrConnectConfig connectConfig) {
                this.connectConfig = connectConfig;
            }

            public static NewSolrClientFactory create(SolrConnectConfig connectConfig) {
                return new NewSolrClientFactory(connectConfig);
            }

            @Override
            public HttpSolrClient getClientFromUrlRaw(String url, String solrUsername, String solrPassword) {
                return makeClient(connectConfig, url, solrUsername, solrPassword);
            }

            public static HttpSolrClient makeClient(SolrConnectConfig connectConfig, String url, String solrUsername, String solrPassword) {
                if (Debug.verboseOn()) Debug.logVerbose("Solr: Creating new solr " + connectConfig.makeClientLogDesc(url,  solrUsername), module);
                return ScipioHttpSolrClient.create(url, null, solrUsername, solrPassword,
                            connectConfig.getMaxConnections(), connectConfig.getMaxConnectionsPerHost(),
                            connectConfig.getConnectTimeout(), connectConfig.getSocketTimeout());
            }

            @Override
            public SolrConnectConfig getConnectConfig() {
                return connectConfig;
            }
        }

        /**
         * Special client cache, optimized using read-only map for thread safety and default clients.
         */
        static class CachedSolrClientFactory extends SolrClientFactory {
            private final SolrConnectConfig connectConfig;
            private final String defaultClientCacheKey;
            private final HttpSolrClient defaultClient;
            private Map<String, HttpSolrClient> clientCache;

            CachedSolrClientFactory(SolrConnectConfig connectConfig, String defaultClientCacheKey, HttpSolrClient defaultClient) {
                this.connectConfig = connectConfig;
                this.defaultClientCacheKey = defaultClientCacheKey;
                this.defaultClient = defaultClient;
                Map<String, HttpSolrClient> clientCache = new HashMap<>();
                clientCache.put(defaultClientCacheKey, defaultClient);
                this.clientCache = Collections.unmodifiableMap(clientCache);
            }

            public static CachedSolrClientFactory create(SolrConnectConfig connectConfig) {
                String defaultClientCacheKey = (connectConfig.getSolrUsername() != null) ?
                        (connectConfig.getSolrCoreUrl() + ":" + connectConfig.getSolrUsername() + ":" + connectConfig.getSolrPassword())
                        : connectConfig.getSolrCoreUrl();
                HttpSolrClient defaultClient = NewSolrClientFactory.makeClient(connectConfig, connectConfig.getSolrCoreUrl(),
                        connectConfig.getSolrUsername(), connectConfig.getSolrPassword());
                return new CachedSolrClientFactory(connectConfig, defaultClientCacheKey, defaultClient);
            }

            @Override
            public HttpSolrClient getClientFromUrlRaw(String url, String solrUsername, String solrPassword) {
                final String cacheKey = (solrUsername != null) ? (url + ":" + solrUsername + ":" + solrPassword) : url;
                if (defaultClientCacheKey.equals(cacheKey)) {
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: Using default solr " + connectConfig.makeClientLogDesc(url,  solrUsername), module);
                    return defaultClient;
                }

                HttpSolrClient client = clientCache.get(cacheKey);
                if (client == null) {
                    synchronized(this) {
                        client = clientCache.get(cacheKey);
                        if (client == null) {
                            client = NewSolrClientFactory.makeClient(connectConfig, url, solrUsername, solrPassword);
                            // use immutable map pattern for performance (the map only ever contains a few entries)
                            Map<String, HttpSolrClient> newCache = new HashMap<>(clientCache);
                            newCache.put(cacheKey, client);
                            clientCache = Collections.unmodifiableMap(newCache);
                        } else {
                            if (Debug.verboseOn()) Debug.logVerbose("Solr: Using cached solr " + connectConfig.makeClientLogDesc(url,  solrUsername), module);
                        }
                    }
                } else {
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: Using cached solr " + connectConfig.makeClientLogDesc(url,  solrUsername), module);
                }
                return client;
            }

            @Override
            public SolrConnectConfig getConnectConfig() {
                return connectConfig;
            }
        }
    }

    static class LocalUrlUtil {
        public static boolean isLocalhost(String host) {
            return "localhost".equals(host) || "127.0.0.1".equals(host);
        }

        public static int getStandardPort(String protocol) {
            return ("https".equals(protocol)) ? 443 : 80;
        }

        public static boolean isStandardPort(int port, String protocol) {
            return ("https".equals(protocol) && port == 443) || ("http".equals(protocol) && port == 80);
        }

        public static Integer getWebappContainerPort(String protocol) {
            Integer port = UtilProperties.getPropertyAsInteger("url", "https".equals(protocol) ? "port.https" : "port.http", null);
            // TODO: should try to lookup a container port in this case
            return port;
        }

        public static boolean isWebappContainerPort(int port) {
            if (port == UtilProperties.getPropertyAsInteger("url", "port.https", -1)) {
                return true;
            }
            if (port == UtilProperties.getPropertyAsInteger("url", "port.http", -1)) {
                return true;
            }
            // TODO: should try to lookup container ports in this case
            return false;
        }
    }
}
