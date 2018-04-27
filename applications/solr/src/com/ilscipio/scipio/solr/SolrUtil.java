package com.ilscipio.scipio.solr;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transaction;

import org.apache.http.client.HttpClient;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.impl.HttpClientUtil;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.request.SolrPing;
import org.apache.solr.client.solrj.response.SolrPingResponse;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.component.ComponentException;
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

import com.ilscipio.scipio.solr.util.ScipioHttpSolrClient;

/**
 * Generic Solr utility class, and helpers to get HttpSolrClient for the Scipio Solr instance.
 */
public abstract class SolrUtil {
    
    public static final String module = SolrUtil.class.getName();
    
    public static final String solrConfigName = "solrconfig";
    
    private static final String solrWebappPath = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.path", "/solr");
    private static final String solrWebappServer = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.server", "default-server");
    private static final String solrDefaultCore = UtilProperties.getPropertyValue(solrConfigName, "solr.core.default");
    
    private static final String effectiveConfigVersion = determineSolrConfigVersion(
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version"),
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version.custom"),
            UtilProperties.getPropertyValue(solrConfigName, "solr.config.version.extra"));
    /**
     * @deprecated use {@link #getSolrWebappUrl} instead
     */
    @Deprecated
    public static final String solrUrl = makeSolrWebappUrl();
    /**
     * @deprecated use {@link #getSolrDefaultCoreUrl} instead
     */
    @Deprecated
    public static final String solrFullUrl = makeSolrDefaultCoreUrl();

    private static Boolean solrWebappPresent = null; // NOTE: don't really need volatile/sync; optimization only
    
    public static String getSolrConfigName() {
        return solrConfigName;
    }

    private static String getSolrPropValueOrNull(String name) {
        String value = UtilProperties.getPropertyValue(solrConfigName, name);
        return (value.isEmpty() ? null : value);
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
    
    public static String getSolrWebappUrl() {
        return solrUrl;
    }
    
    public static String makeSolrWebappUrl() {
        final String solrWebappProtocol = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.protocol");
        final String solrWebappDomainName = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.domainName");
        final String solrWebappPath = SolrUtil.solrWebappPath;
        final String solrWebappPortOverride = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.portOverride");
        
        String solrPort;
        if (UtilValidate.isNotEmpty(solrWebappPortOverride)) {
            solrPort = solrWebappPortOverride;
        } else {
            solrPort = UtilProperties.getPropertyValue("url", ("https".equals(solrWebappProtocol) ? "port.https" : "port.http"));
        }
        StringBuilder sb = new StringBuilder();
        sb.append(solrWebappProtocol);
        sb.append("://");
        sb.append(solrWebappDomainName);
        sb.append(":");
        sb.append(solrPort);
        sb.append(solrWebappPath);
        if (sb.charAt(sb.length() - 1) == '/') {
            sb.setLength(sb.length() - 1);
        }
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

    public static boolean isSolrEcaEnabled() {
        Boolean ecaEnabled = null;
        String sysProp = System.getProperty("ofbiz.solr.eca.enabled");
        if (UtilValidate.isNotEmpty(sysProp)) {
            if ("true".equalsIgnoreCase(sysProp))  {
                ecaEnabled = Boolean.TRUE;
            } else if ("false".equalsIgnoreCase(sysProp)) {
                ecaEnabled = Boolean.FALSE;
            }
        }
        if (ecaEnabled == null) {
            ecaEnabled = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.enabled", false);
        }
        return Boolean.TRUE.equals(ecaEnabled);
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
        }
        catch(ComponentException e) {
            throw new IllegalStateException(e);
        }
        return solrApp;
    }
    
    public static boolean isSolrEcaWebappInitCheckPassed() {
        Boolean webappCheckEnabled = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.useSolrWebappLoadedCheck", true);
        if (Boolean.TRUE.equals(webappCheckEnabled)) {
            return isSolrWebappInitialized();
        } else {
            // If webapp check disabled, then we say the check passed.
            return true;
        }
    }
    
    public static boolean isSolrWebappInitialized() {
        return ScipioSolrInfoServlet.isServletInitStatusReached();
    }
    
    public static boolean isEcaTreatConnectErrorNonFatal() {
        Boolean treatConnectErrorNonFatal = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.treatConnectErrorNonFatal", true);
        return Boolean.TRUE.equals(treatConnectErrorNonFatal);
    }
 
    /**
     * Returns true if the Solr webapp is present in the system (by context root).
     * <p>
     * For stock Scipio configuration, this always returns true.
     * <p>
     * WARN: Do not call this before or while components are being read.
     */
    public static boolean isSolrWebappPresent() {
        Boolean present = solrWebappPresent;
        if (present == null) {
            present = (ComponentConfig.getWebAppInfo(solrWebappServer, solrWebappPath) != null);
            solrWebappPresent = present;
        }
        return present;
    }
    
    /**
     * Returns true if the Solr webapp is present and enabled.
     * <p>
     * WARN: Do not call this before or while components are being read.
     */
    public static boolean isSolrWebappEnabled() {
        return isSolrWebappPresent();
    }
    
    public static boolean isSolrWebappPingOk(HttpSolrClient client) throws Exception {
        try {
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
        } catch(Exception e) {
            // FIXME: we are not supposed to catch this, but in current setup with Tomcat
            // solr can't handle the incomplete loading 503 and throws exception, so have no choice
            // but because this is only a Ping, we can usually assume this means not yet loaded...
            Debug.logInfo("Solr: isSolrWebappPingOk: Solr webapp not pingable; exception: " + e.getMessage(), module);
            return false;
        }
    }
    
    /**
     * Returns true if Solr is loaded and available for queries.
     */
    public static boolean isSolrWebappReady(HttpSolrClient client) throws Exception {
        return isSolrWebappInitialized() && isSolrWebappPingOk(client);
    }
    
    /**
     * Returns true if Solr is loaded and available for queries, using default core and client.
     */
    public static boolean isSolrWebappReady() throws Exception {
        return isSolrWebappReady(getQueryHttpSolrClient(null));
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
     * Returns a Solr client for making read-only queries.
     * <p>
     * This client includes the basic auth defined in solrconfig.properties/solr.query.login.*,
     * which can be overridden on individual requests (using QueryRequest).
     */
    public static HttpSolrClient getQueryHttpSolrClient(String core) {
        if (UtilValidate.isNotEmpty(core)) {
            return SolrClientFactory.queryClientFactory.getClient(getSolrCoreUrl(core), 
                    getSolrQueryConnectConfig().getSolrUsername(), getSolrQueryConnectConfig().getSolrPassword());
        } else {
            return SolrClientFactory.queryClientFactory.getClient(getSolrDefaultCoreUrl(), 
                    getSolrQueryConnectConfig().getSolrUsername(), getSolrQueryConnectConfig().getSolrPassword());
        }
    }

    public static HttpSolrClient getQueryHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.queryClientFactory.getClient(url, solrUsername, solrPassword);
    }
    
    /**
     * Returns a Solr client for making update/indexing queries.
     * <p>
     * This client includes the basic auth defined in solrconfig.properties/solr.update.login.*,
     * which can be overridden on individual requests (using UpdateRequest).
     */
    public static HttpSolrClient getUpdateHttpSolrClient(String core) {
        if (UtilValidate.isNotEmpty(core)) {
            return SolrClientFactory.updateClientFactory.getClient(getSolrCoreUrl(core), 
                    getSolrUpdateConnectConfig().getSolrUsername(), getSolrUpdateConnectConfig().getSolrPassword());
        } else {
            return SolrClientFactory.updateClientFactory.getClient(getSolrDefaultCoreUrl(), 
                    getSolrUpdateConnectConfig().getSolrUsername(), getSolrUpdateConnectConfig().getSolrPassword());
        }
    }
 
    public static HttpSolrClient getUpdateHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.updateClientFactory.getClient(url, solrUsername, solrPassword);
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
        return SolrClientFactory.newQueryClientFactory.getClient(url, solrUsername, solrPassword);
    }
    
    public static HttpSolrClient makeUpdateHttpSolrClientFromUrl(String url, String solrUsername, String solrPassword) {
        return SolrClientFactory.newUpdateClientFactory.getClient(url, solrUsername, solrPassword);
    }
    
    static SolrConnectConfig getSolrQueryConnectConfig() {
        return SolrConnectConfig.queryConnectConfig;
    }
    
    static SolrConnectConfig getSolrUpdateConnectConfig() {
        return SolrConnectConfig.updateConnectConfig;
    }
    
    public enum SolrClientMode {
        QUERY("query"),
        UPDATE("update");
        
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
        
        private final SolrClientMode clientMode;
        private final String solrUsername;
        private final String solrPassword;
        private final boolean reuseClient;
        private final Integer connectTimeout;
        private final Integer socketTimeout;
        private final boolean noDelay; // TODO: FLAG NOT IMPLEMENTED (client building issue)
        private final Integer maxConnections;
        private final Integer maxConnectionsPerHost;
        
        public SolrConnectConfig(SolrClientMode clientMode, Map<String, ?> configMap) {
            this.clientMode = clientMode;
            this.solrUsername = UtilProperties.valueOrNull((String) configMap.get("login.username"));
            this.solrPassword = UtilProperties.valueOrNull((String) configMap.get("login.password"));
            this.reuseClient = UtilProperties.asBoolean(configMap.get("reuseConnections"), false);
            this.connectTimeout = UtilProperties.asInteger(configMap.get("connectTimeout"));
            this.socketTimeout = UtilProperties.asInteger(configMap.get("socketTimeout"));
            this.noDelay = UtilProperties.asBoolean(configMap.get("noDelay"), true);
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
        public boolean isNoDelay() { return noDelay; }
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

        /**
         * Abstracted factory that gets a cached or new client.
         */
        static final SolrClientFactory queryClientFactory = create(SolrConnectConfig.queryConnectConfig);
        static final SolrClientFactory updateClientFactory = create(SolrConnectConfig.updateConnectConfig);
        
        public static SolrClientFactory create(SolrConnectConfig connectConfig) {
            if (connectConfig.isReuseClient()) return CachedSolrClientFactory.create(connectConfig);
            else return NewSolrClientFactory.create(connectConfig);
        }
       
        public abstract HttpSolrClient getClient(String url, String solrUsername, String solrPassword);
        
        static class NewSolrClientFactory extends SolrClientFactory {
            private final SolrConnectConfig connectConfig;
            
            NewSolrClientFactory(SolrConnectConfig connectConfig) {
                this.connectConfig = connectConfig;
            }
            
            public static NewSolrClientFactory create(SolrConnectConfig connectConfig) {
                return new NewSolrClientFactory(connectConfig);
            }
            
            @Override
            public HttpSolrClient getClient(String url, String solrUsername, String solrPassword) {
                return makeClient(connectConfig, url, solrUsername, solrPassword);
            }
            
            public static HttpSolrClient makeClient(SolrConnectConfig connectConfig, String url, String solrUsername, String solrPassword) {
                if (Debug.verboseOn()) Debug.logVerbose("Solr: Creating new solr " + connectConfig.makeClientLogDesc(url,  solrUsername), module);
                
                ModifiableSolrParams params = new ModifiableSolrParams();
                if (connectConfig.getMaxConnections() != null) {
                    params.set(HttpClientUtil.PROP_MAX_CONNECTIONS, connectConfig.getMaxConnections());
                }
                if (connectConfig.getMaxConnectionsPerHost() != null) {
                    params.set(HttpClientUtil.PROP_MAX_CONNECTIONS_PER_HOST, connectConfig.getMaxConnectionsPerHost());
                }
                params.set(HttpClientUtil.PROP_FOLLOW_REDIRECTS, true);
                HttpClient httpClient = HttpClientUtil.createClient(params);

                HttpSolrClient client = ScipioHttpSolrClient.create(url, httpClient, solrUsername, solrPassword);
                
                // TODO: In Solr 7, these are deprecated and moved to Builder/constructor 
                if (connectConfig.getConnectTimeout() != null) {
                    client.setConnectionTimeout(connectConfig.getConnectTimeout());
                }
                if (connectConfig.getSocketTimeout() != null) {
                    client.setSoTimeout(connectConfig.getSocketTimeout());
                }
                
                return client;
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
            public HttpSolrClient getClient(String url, String solrUsername, String solrPassword) {
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
        }
    }
}
