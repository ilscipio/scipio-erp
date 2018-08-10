/*
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements.  See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership.  The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License.  You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.
 */

package org.ofbiz.service.soap;

import java.security.SecureRandom;
import java.util.HashMap;
import java.util.Map;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.apache.axis2.client.Options;
import org.apache.axis2.client.ServiceClient;
import org.apache.axis2.transport.http.HTTPConstants;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.PlainSocketFactory;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.params.HttpParams;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.TrustManagers;
import org.ofbiz.base.util.TrustManagers.CertCheckConfig;
import org.ofbiz.base.util.TrustManagers.DelegatingTrustManager;
import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: Custom SOAP HttpClient4 and cert validation config handler.
 * <p>
 * Holds the shared instance of the HTTP client for SOAPClientEngine and axis2 clients ({@link #getHttpClient()).
 * <p>
 * TODO: All this deprecation is intentional and will need to be rewritten when someday axis2 upgrades their httpclient4 code
 * and they get rid of {@link org.apache.http.impl.client.AbstractHttpClient} references internally; until
 * they do we are locked-in to using AbstractHttpClient from httpclient 4.2.
 * <p>
 * WARN: Because of the previous point, this handler cannot yet be generalized to other
 * SOAP or non-SOAP clients support HttpClient4.
 * <p>
 * Added 2018-07-12.
 */
@SuppressWarnings("deprecation")
public class SOAPClientConnectConfig {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final SOAPClientConnectConfig INSTANCE = createInstance("service", "soap.connect.", "soap.cert.", true);

    protected final String propResource;
    protected final String connectPropPrefix;
    protected final String certPropPrefix;

    protected final ConnectCfgMode connectCfgMode;
    public enum ConnectCfgMode {
        CUSTOM_CLIENT("custom-client"),
        CUSTOM_CONNMANAGER("custom-connmanager"),
        STOCK("stock");
        
        private static final Map<String, ConnectCfgMode> nameMap;
        static {
            Map<String, ConnectCfgMode> map = new HashMap<>();
            for(ConnectCfgMode mode : ConnectCfgMode.values()) { map.put(mode.getName(), mode); }
            nameMap = map;
        }
        private final String name;
        private ConnectCfgMode(String name) { this.name = name; }
        public String getName() { return name; }
        public boolean isCustom() { return this != STOCK; }
        public static ConnectCfgMode fromNameSafe(String name, ConnectCfgMode defMode) { ConnectCfgMode mode = nameMap.get(name); return (mode != null) ? mode : defMode; }
        public static ConnectCfgMode fromNameSafe(String name) { return nameMap.get(name); }
    }

    protected final boolean reuseClient;
    protected final boolean reuseConnManager;

    protected final CertCheckCfgMode certCheckCfgMode;
    public enum CertCheckCfgMode {
        CUSTOM("custom"),
        STOCK("stock");
        
        private static final Map<String, CertCheckCfgMode> nameMap;
        static {
            Map<String, CertCheckCfgMode> map = new HashMap<>();
            for(CertCheckCfgMode mode : CertCheckCfgMode.values()) { map.put(mode.getName(), mode); }
            nameMap = map;
        }
        private final String name;
        private CertCheckCfgMode(String name) { this.name = name; }
        public String getName() { return name; }
        public boolean isCustom() { return this != STOCK; }
        public static CertCheckCfgMode fromNameSafe(String name, CertCheckCfgMode defMode) { CertCheckCfgMode mode = nameMap.get(name); return (mode != null) ? mode : defMode; }
        public static CertCheckCfgMode fromNameSafe(String name) { return nameMap.get(name); }
    }
    protected final CertCheckConfig certCheckConfig;

    protected final Integer maxConnections;
    protected final Integer maxConnectionsPerHost;
    protected final Integer connectTimeout;
    protected final Integer socketTimeout;
    
    protected final String sslProtocol;

    protected final X509TrustManager trustManager; // NOTE: shared even when reuseClient==false
    protected final SchemeRegistry schemeRegistry; // NOTE: shared even when reuseClient==false
    protected final ClientConnectionManager poolingConnManager; // NOTE: only shared when reuseConnManager==true
    protected final CloseableHttpClient httpClient; // NOTE: only shared when reuseClient==true

    // TODO?: does nothing because SOAPClientEngine is written with no "memory" anyway...
    //protected final ThreadLocal<Object> prevHttpClient = new ThreadLocal<>();
    //protected final ThreadLocal<Object> prevConnManager = new ThreadLocal<>();

    /**
     * Default constructor.
     * NOTE: use getInstance instead, this is for future...
     */
    protected SOAPClientConnectConfig(String propResource, String connectPropPrefix, String certPropPrefix, boolean log) {
        this.propResource = propResource;
        this.connectPropPrefix = connectPropPrefix;
        this.certPropPrefix = certPropPrefix;

        connectCfgMode = ConnectCfgMode.fromNameSafe(UtilProperties.getPropertyValue(propResource, connectPropPrefix+"configMode"), ConnectCfgMode.STOCK);
        certCheckCfgMode = CertCheckCfgMode.fromNameSafe(UtilProperties.getPropertyValue(propResource, certPropPrefix+"validate.configMode"), CertCheckCfgMode.STOCK);
        if (connectCfgMode.isCustom()) {
            reuseClient = (connectCfgMode == ConnectCfgMode.CUSTOM_CONNMANAGER) ? 
                    false : UtilProperties.getPropertyAsBoolean(propResource, connectPropPrefix+"reuseClient", false);
            reuseConnManager = (reuseClient) ? true : 
                UtilProperties.getPropertyAsBoolean(propResource, connectPropPrefix+"reuseConnManager", false);
           
            maxConnections = UtilProperties.getPropertyAsIntegerInRange(propResource, connectPropPrefix+"maxConnections", 0, null, 200);
            maxConnectionsPerHost = UtilProperties.getPropertyAsIntegerInRange(propResource, connectPropPrefix+"maxConnectionsPerHost", 0, null, 200);
            connectTimeout = UtilProperties.getPropertyAsIntegerInRange(propResource, connectPropPrefix+"connectTimeout", 0, null, null);
            socketTimeout = UtilProperties.getPropertyAsIntegerInRange(propResource, connectPropPrefix+"socketTimeout", 0, null, null);
            sslProtocol = UtilProperties.getPropertyValue(propResource, certPropPrefix+"validate.sslProtocol", "TLS");
                    
            if (log) Debug.logInfo("SOAP config: Basic SOAP Custom HttpClient/certificate configuration: ["
                    + "connectCfgMode=" + connectCfgMode.getName()
                    + ", reuseClient=" + reuseClient
                    + ", reuseConnManager=" + reuseConnManager
                    + ", certCheckCfgMode=" + certCheckCfgMode.getName()
                    + ", maxConnections=" + maxConnections
                    + ", maxConnectionsPerHost=" + maxConnectionsPerHost
                    + ", connectTimeout=" + connectTimeout
                    + ", socketTimeout=" + socketTimeout
                    + ", sslProtocol=" + sslProtocol
                    + "]", module);

            if (certCheckCfgMode.isCustom()) {
                certCheckConfig = CertCheckConfig.fromPropertiesSafe(propResource, certPropPrefix+"validate.", // NOTE: callee appends "methods."
                        new CertCheckConfig.GeneralConfig().useServerCheckDefaults().setOptionalDefault(false));
                if (log) Debug.logInfo("SOAP config: Read certificate validation method configuration from " + propResource + ".properties (uninitialized): "
                        + certCheckConfig, module);
                trustManager = makeTrustManager(certCheckConfig, log);
            } else {
                if (log) Debug.logInfo("SOAP config: Custom cert check disabled; skipping creating custom trust manager", module);
                certCheckConfig = null;
                trustManager = null;
            }

            schemeRegistry = makeSchemeRegistry(trustManager);
            poolingConnManager = (reuseConnManager) ? makePoolingConnectionManager(schemeRegistry) : null;
            httpClient = reuseClient ? makeHttpClient() : null;

            if (log) Debug.logInfo("SOAP config: Basic SOAP Custom HttpClient/certificate configuration finished", module);
        } else {
            if (log) Debug.logInfo("SOAP config: Custom HttpClient handling is DISABLED (automatically disables custom certficiate validation);"
                    + " using stock SOAP (axis2) behavior for HttpClient and certificate validation", module);
            reuseClient = false;
            reuseConnManager = false;
            maxConnections = null;
            maxConnectionsPerHost = null;
            connectTimeout = null;
            socketTimeout = null;
            sslProtocol = null;
            certCheckConfig = null;
            trustManager = null;
            schemeRegistry = null;
            poolingConnManager = null;
            httpClient = null;
        }
    }
    
    protected SOAPClientConnectConfig() {
        propResource = null;
        connectPropPrefix = null;
        certPropPrefix = null;
        connectCfgMode = null;
        certCheckCfgMode = null;
        reuseClient = false;
        reuseConnManager = false;
        maxConnections = null;
        maxConnectionsPerHost = null;
        connectTimeout = null;
        socketTimeout = null;
        sslProtocol = null;
        certCheckConfig = null;
        trustManager = null;
        schemeRegistry = null;
        poolingConnManager = null;
        httpClient = null;
    }
            
    public static SOAPClientConnectConfig getDefaultInstance() {
        return INSTANCE;
    }

    public static SOAPClientConnectConfig createInstance(String propResource, String connectPropPrefix, String certPropPrefix) {
        return createInstance(propResource, connectPropPrefix, certPropPrefix, false);
    }

    public static SOAPClientConnectConfig createInstance(String propResource, String connectPropPrefix, String certPropPrefix, boolean log) {
        SOAPClientConnectConfig connCfg;
        try {
            connCfg = new SOAPClientConnectConfig(propResource, connectPropPrefix, certPropPrefix, log);
            if (!connCfg.connectCfgMode.isCustom()) {
                connCfg = DisabledSOAPClientConnectConfig.INSTANCE;
            }
        } catch(Exception e) {
            Debug.logError(e, "SOAP: Error reading or initializing client config configuration from " 
                    + propResource + " (system defaults will be used): " + e.getMessage(), module);
            connCfg = DisabledSOAPClientConnectConfig.INSTANCE;
        }
        return connCfg;
    }

    public ConnectCfgMode getConnectCfgMode() { return connectCfgMode; }
    public CertCheckCfgMode getCertCheckCfgMode() { return certCheckCfgMode; }
    public boolean isReuseClient() { return reuseClient; }
    public boolean isReuseConnManager() { return reuseConnManager; }
    public Integer getConnectTimeout() { return connectTimeout; }
    public Integer getSocketTimeout() { return socketTimeout; }
    public String getSslProtocol() { return sslProtocol; }

    public boolean configureSOAPHttpClient(ServiceClient client, Options options) {
        // TODO?: does nothing because SOAPClientEngine is written with no "memory" anyway...
        //prevHttpClient.set(options.getProperty(HTTPConstants.CACHED_HTTP_CLIENT));
        //prevConnManager.set(options.getProperty(HTTPConstants.MULTITHREAD_HTTP_CONNECTION_MANAGER));
        
        ClientConnectionManager connManager = getConnectionManager();
        if (connManager == null) {
            // can do nothing without this
            return false;
        }
        
        if (connectCfgMode == ConnectCfgMode.CUSTOM_CLIENT) {
            // TODO: REVIEW: 2018-07-12: not clear if the HttpClient
            // returned here is thread-safe-enough or not...
            // axis2 source with similar code seems to believe it isn't,
            // can't tell...
            CloseableHttpClient httpClient = getHttpClient(connManager);
            if (httpClient != null) {
                if (reuseClient) {
                    // NOTE: This flag does not seem to do much at this time...
                    // SOAPClientEngine does not reuse instances enough for that.
                    options.setProperty(HTTPConstants.REUSE_HTTP_CLIENT, true);
                }
                
                // This is the axis2 "official" way to pass a custom HttpClient
                options.setProperty(HTTPConstants.CACHED_HTTP_CLIENT, httpClient);
                
                if (reuseConnManager) {
                    options.setProperty(HTTPConstants.MULTITHREAD_HTTP_CONNECTION_MANAGER, connManager);
                }
                return true;
            }
        } else if (connectCfgMode == ConnectCfgMode.CUSTOM_CONNMANAGER) {
            options.setProperty(HTTPConstants.MULTITHREAD_HTTP_CONNECTION_MANAGER, connManager);
            
            // use axis2 interface to set timeouts in this case; just about the only thing we _can_ set with it...

            if (connectTimeout != null) {
                options.setProperty(HTTPConstants.CONNECTION_TIMEOUT, connectTimeout);
            }
            if (socketTimeout != null) {
                options.setProperty(HTTPConstants.SO_TIMEOUT, socketTimeout);
            }
            return true;
        }
        return false;
    }
    
    /**
     * Returns a custom HttpClient4 instance if applicable and configured successfully, or null otherwise.
     */
    public CloseableHttpClient getHttpClient() {
        return getHttpClient(getConnectionManager());
    }
    
    public CloseableHttpClient getHttpClient(ClientConnectionManager connManager) {
        if (connectCfgMode != ConnectCfgMode.CUSTOM_CLIENT) return null;
        if (reuseClient) return httpClient;
        return makeHttpClient();
    }

    public CloseableHttpClient makeHttpClient() {
        return makeHttpClient(getConnectionManager());
    }

    /**
     * Creates an HttpClient(4) for Axis2 (1.7.8) with ability to recognized
     * local trusted certs.
     * <p>
     * WARN: This all shows deprecation warnings for a reason: the implementation
     * can't be upgraded until axis2 changes theirs. (FIXME)
     */
    public CloseableHttpClient makeHttpClient(ClientConnectionManager connManager) {
        // SCIPIO: 2018-07-11: TODO: upgrade: we cannot do this the right way yet, because
        // axis 1.7.8 uses AbstractHttpClient as the CACHED_HTTP_CLIENT type,
        // but it is deprecated and the type returned by HttpsClients.custom().build()
        // does not implement AbtractHttpClient...
        //PoolingHttpClientConnectionManager cm = new PoolingHttpClientConnectionManager();
        //cm.setMaxTotal(100);
        //cm.setDefaultMaxPerRoute(100);
        //RequestConfig config = RequestConfig.custom()
        //        .setConnectionRequestTimeout(500)
        //        .setConnectTimeout(10)
        //        .setSocketTimeout(2000)
        //        .setExpectContinueEnabled(true)
        //         .build();
        //httpClient = HttpClients.custom()
        //        .setDefaultRequestConfig(config)
        //        .setConnectionManager(cm)
        //        .build();
        try {
            HttpParams clientParams = new BasicHttpParams();
            clientParams.setParameter(CoreProtocolPNames.HTTP_ELEMENT_CHARSET, "UTF-8");
            if (connectTimeout != null) {
                clientParams.setParameter(CoreConnectionPNames.CONNECTION_TIMEOUT, connectTimeout);
            }
            if (socketTimeout != null) {
                clientParams.setParameter(CoreConnectionPNames.SO_TIMEOUT, socketTimeout);
            }
            return new DefaultHttpClient(connManager, clientParams);
        } catch(Exception e) {
            Debug.logError(e, "SOAP config: Error creating custom HttpClient: " + e.getMessage(), module);
            return null;
        }
    }

    public ClientConnectionManager getConnectionManager() {
        if (reuseConnManager) {
            return poolingConnManager;
        } else {
            return makePoolingConnectionManager(getSchemeRegistry());
        }
    }

    protected ClientConnectionManager makePoolingConnectionManager(SchemeRegistry schemeRegistry) {
        // NOTE: axis2 always used PoolingClientConnectionManager even when not reused
        PoolingClientConnectionManager connManager = new PoolingClientConnectionManager(schemeRegistry);
        if (maxConnections != null) ((PoolingClientConnectionManager)connManager).setMaxTotal(maxConnections);
        if (maxConnectionsPerHost != null) ((PoolingClientConnectionManager)connManager).setDefaultMaxPerRoute(maxConnectionsPerHost);
        return connManager;
    }
    
    public X509TrustManager getTrustManager() {
        return trustManager;
    }

    protected X509TrustManager makeTrustManager(CertCheckConfig certCheckConfig, boolean log) {
        if (certCheckConfig != null) {
            DelegatingTrustManager tm = (DelegatingTrustManager) TrustManagers.getDelegatingTrustManager(certCheckConfig);
            if (log) {
                Debug.logInfo("SOAP config: Created DelegatingTrustManager for SOAP certificate validation: " + tm, module);
            }
            return tm;
        } else {
            if (log) {
                Debug.logWarning("SOAP config: Missing or broken " + propResource + ".properties " + certPropPrefix+"validate.* configuration"
                        + "; cannot create DelegatingTrustManager; using TrustNooneManager (all certs will be rejected until configuration is fixed)", module);
            }
            return TrustManagers.getTrustNooneManager();
        }
    }

    public SchemeRegistry getSchemeRegistry() {
        return schemeRegistry;
    }

    protected SchemeRegistry makeSchemeRegistry(X509TrustManager trustManager) {
        SSLSocketFactory sf;
        if (certCheckCfgMode.isCustom()) {
            try {
               
                SSLContext context = SSLContext.getInstance(sslProtocol); // "SSL", "TLS"
                context.init(null, new TrustManager[] { trustManager }, new SecureRandom());
                sf = new SSLSocketFactory(context);
            } catch (Exception e) {
                Debug.logError(e, "Could not create SSL context for SOAP axis2 client engine - trusted certs will fail! Using stock axis2 cert trust setup...", module);
                sf = SSLSocketFactory.getSocketFactory();
            }
        } else {
            // stock axis2 1.7.8 behavior
            sf = SSLSocketFactory.getSocketFactory();
        }

        SchemeRegistry schemeRegistry = new SchemeRegistry();
        schemeRegistry.register(
                new Scheme("http", 80, PlainSocketFactory.getSocketFactory()));
        schemeRegistry.register(
                new Scheme("https", 443, sf));
        return schemeRegistry;
    }
    
    protected static class DisabledSOAPClientConnectConfig extends SOAPClientConnectConfig {
        private static final DisabledSOAPClientConnectConfig INSTANCE = new DisabledSOAPClientConnectConfig();
        private DisabledSOAPClientConnectConfig() { super(); }

        @Override public boolean configureSOAPHttpClient(ServiceClient client, Options options) { return false; }
        @Override public CloseableHttpClient makeHttpClient(ClientConnectionManager connManager) { return null; }
        @Override protected ClientConnectionManager makePoolingConnectionManager(SchemeRegistry schemeRegistry) { return null; }
        @Override protected X509TrustManager makeTrustManager(CertCheckConfig certCheckConfig, boolean log) { return null; }
        @Override protected SchemeRegistry makeSchemeRegistry(X509TrustManager trustManager) { return null; } 
    }
}
