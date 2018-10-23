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

package org.ofbiz.base.util;

import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.TrustManagers.DelegatingTrustManager.TrustManagerEntry;

/**
 * SCIPIO: A collection of {@link javax.net.ssl.X509TrustManager} implementations and wrappers to supplement
 * {@link SSLUtil} and {@link MultiTrustManager}.
 * <p>
 * Added 2018-07-12.
 */
public abstract class TrustManagers {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private TrustManagers() {
    }

    public static X509TrustManager getX509TrustManager(TrustManager[] tms) {
        if (tms == null) return null;
        for(int i=0; i < tms.length; i++) {
            if (tms[i] instanceof X509TrustManager) return (X509TrustManager) tms[i];
        }
        return null;
    }

    /**
     * Returns the default system TrustManager (JVM).
     * Usually includes public infrastructure certs.
     * <p>
     * Added 2018-07-12.
     */
    public static X509TrustManager getSystemTrustManager() {
        TrustManagerFactory tmf;
        try {
            tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
        } catch (NoSuchAlgorithmException e) {
            Debug.logError(e, module); // shouldn't happen (default!)
            return null;
        }
        try {
            tmf.init((KeyStore) null);
        } catch (KeyStoreException e) {
            Debug.logError(e, module); // shouldn't happen (null!)
            return null;
        }
        return getX509TrustManager(tmf.getTrustManagers());
    }

    public static X509TrustManager getTrustNooneManager() {
        return TrustNooneManager.INSTANCE;
    }

    public static X509TrustManager getTrustSelfSignedManager() {
        return TrustSelfSignedManager.INSTANCE;
    }

    public static X509TrustManager getTrustAnyManager() {
        return getX509TrustManager(SSLUtil.getTrustAnyManagers());
    }

    public static X509TrustManager getComponentTrustStoresManager() throws IOException {
        try {
            return getX509TrustManager(SSLUtil.getComponentTrustManagers());
        } catch (Exception e) {
            throw new IOException(e);
        }
    }

    /**
     * Returns the ofbiz manager, which is combination of a system keystore
     * and all the component keystore element definitions in ofbiz-component.xml files.
     * <p>
     * NOTE: This may include some system trust stoke keystores; however it does not
     * fully include the system (JVM) trust manager itself, so it should not be
     * relied on for public certs.
     */
    public static X509TrustManager getOfbizTrustStoresManager() throws IOException {
        try {
            return getX509TrustManager(SSLUtil.getTrustManagers());
        } catch (Exception e) {
            throw new IOException(e);
        }
    }

    /**
     * Creates a TrustManager for a single KeyStore.
     */
    public static X509TrustManager getKeyStoreTrustManager(KeyStore keyStore) throws IOException {
        if (keyStore == null) return null;
        //return new SingleKeyStoreTrustManager(keyStore);
        TrustManagerFactory tmfactory;
        try {
            tmfactory = TrustManagerFactory.getInstance(
                    TrustManagerFactory.getDefaultAlgorithm());
        } catch (NoSuchAlgorithmException e) {
            Debug.logError(e, module); // shouldn't happen (default)
            return null;
        }
        try {
            tmfactory.init(keyStore);
        } catch (KeyStoreException e) {
            throw new IOException(e);
        }
        return getX509TrustManager(tmfactory.getTrustManagers());
    }

    public static X509TrustManager getDelegatingTrustManager(Collection<TrustManagerEntry> trustManagers) {
        return new DelegatingTrustManager(trustManagers);
    }

    public static X509TrustManager getDelegatingTrustManager(TrustManagerEntry... trustManagers) {
        return new DelegatingTrustManager(Arrays.asList(trustManagers));
    }

    public static X509TrustManager getDelegatingTrustManager(CertCheckConfig config) {
        return config.produceDelegatingTrustManager();
    }

    public static class TrustNooneManager implements X509TrustManager {
        private static final TrustNooneManager INSTANCE = new TrustNooneManager();

        /**
         * Constructor; clients should use {@link TrustManagers#getTrustNooneManager} instead.
         */
        protected TrustNooneManager() {}

        public void checkClientTrusted(X509Certificate[] certs, String string) throws CertificateException {
            throw new CertificateException("Certificate could not be validated (trusting no one)");
        }

        public void checkServerTrusted(X509Certificate[] certs, String string) throws CertificateException {
            throw new CertificateException("Certificate could not be validated (trusting no one)");
        }

        public X509Certificate[] getAcceptedIssuers() {
            return new X509Certificate[0];
        }
    }

    public static class TrustSelfSignedManager implements X509TrustManager {
        private static final TrustSelfSignedManager INSTANCE = new TrustSelfSignedManager();

        /**
         * Constructor; clients should use {@link TrustManagers#getTrustSelfSignedManager} instead.
         */
        protected TrustSelfSignedManager() {}

        public void checkClientTrusted(X509Certificate[] certs, String string) throws CertificateException {
            if (certs.length != 1) throw new CertificateException("Certificate could not be validated (not self-signed)");
            Debug.logImportant("Trusting (un-trusted) client self-signed certificate:\n"
                    + "---- " + certs[0].getSubjectX500Principal().getName() + " valid: " + certs[0].getNotAfter(), module);
        }

        public void checkServerTrusted(X509Certificate[] certs, String string) throws CertificateException {
            if (certs.length != 1) throw new CertificateException("Certificate could not be validated (not self-signed)");
            Debug.logImportant("Trusting (un-trusted) server self-signed certificate:\n"
                    + "---- " + certs[0].getSubjectX500Principal().getName() + " valid: " + certs[0].getNotAfter(), module);
        }

        public X509Certificate[] getAcceptedIssuers() {
            return new X509Certificate[0];
        }
    }

    /**
     * @deprecated see {@link TrustManagers#getKeyStoreTrustManager(KeyStore)} implementation (better?)
     */
    @Deprecated
    static class SingleKeyStoreTrustManager implements X509TrustManager {
        // TODO: REVIEW: this was based on MultiTrustManager
        protected final KeyStore store;

        /**
         * Constructor; clients should use {@link TrustManagers#getKeyStoreTrustManager(KeyStore)} instead.
         */
        protected SingleKeyStoreTrustManager(KeyStore store) {
            this.store = store;
        }

        public void checkClientTrusted(X509Certificate[] certs, String alg) throws CertificateException {
            if (isTrusted(certs)) {
                return;
            }
            throw new CertificateException("No trusted certificate found");
        }

        public void checkServerTrusted(X509Certificate[] certs, String alg) throws CertificateException {
            if (isTrusted(certs)) {
                return;
            }
            throw new CertificateException("No trusted certificate found");
        }

        public X509Certificate[] getAcceptedIssuers() {
            List<X509Certificate> issuers = new LinkedList<X509Certificate>();
            try {
                Enumeration<String> e = store.aliases();
                while (e.hasMoreElements()) {
                    String alias = e.nextElement();
                    Certificate[] chain = store.getCertificateChain(alias);
                    if (chain != null) {
                        for (Certificate cert: chain) {
                            if (cert instanceof X509Certificate) {
                                if (Debug.verboseOn())
                                    Debug.logInfo("Read certificate (chain) : " + ((X509Certificate) cert).getSubjectX500Principal().getName(), module);
                                issuers.add((X509Certificate) cert);
                            }
                        }
                    } else {
                        Certificate cert = store.getCertificate(alias);
                        if (cert != null && cert instanceof X509Certificate) {
                            if (Debug.verboseOn())
                                Debug.logInfo("Read certificate : " + ((X509Certificate) cert).getSubjectX500Principal().getName(), module);
                            issuers.add((X509Certificate) cert);
                        }
                    }
                }
            } catch (KeyStoreException e) {
                Debug.logError(e, module);
            }
            return issuers.toArray(new X509Certificate[issuers.size()]);
        }

        protected boolean isTrusted(X509Certificate[] cert) {
            if (cert != null) {
                X509Certificate[] issuers = this.getAcceptedIssuers();
                if (issuers != null) {
                    for (X509Certificate issuer: issuers) {
                        for (X509Certificate c: cert) {
                            if (Debug.verboseOn())
                                Debug.logInfo("--- Checking cert: " + issuer.getSubjectX500Principal() + " vs " + c.getSubjectX500Principal(), module);
                            if (issuer.equals(c)) {
                                if (Debug.verboseOn())
                                    Debug.logInfo("--- Found trusted cert: " + issuer.getSerialNumber().toString(16) + " : " + issuer.getSubjectX500Principal(), module);
                                return true;
                            }
                        }
                    }
                }
            }
            return false;
        }
    }

    public static class DelegatingTrustManager implements X509TrustManager {
        protected final List<X509TrustManager> startClientTms;
        protected final X509TrustManager finalClientTm;
        protected final List<X509TrustManager> startServerTms;
        protected final X509TrustManager finalServerTm;
        protected final List<X509TrustManager> issuerTms;

        /**
         * Constructor; clients should use {@link TrustManagers#getDelegatingTrustManager} instead.
         */
        protected DelegatingTrustManager(Collection<TrustManagerEntry> trustManagers) {
            // compile the info down to 3 arrays, otherwise this won't do what you'd expect
            ArrayList<X509TrustManager> clientTms = new ArrayList<>(trustManagers.size());
            ArrayList<X509TrustManager> serverTms = new ArrayList<>(trustManagers.size());
            ArrayList<X509TrustManager> issuerTms = new ArrayList<>(trustManagers.size());
            for(TrustManagerEntry tmInfo : trustManagers) {
                if (tmInfo == null || tmInfo.getTrustManager() == null) continue;
                if (tmInfo.isCheckClient()) clientTms.add(tmInfo.getTrustManager());
                if (tmInfo.isCheckServer()) serverTms.add(tmInfo.getTrustManager());
                if (tmInfo.isAcceptedIssuers()) issuerTms.add(tmInfo.getTrustManager());
            }
            if (clientTms.size() > 0) {
                this.finalClientTm = clientTms.get(clientTms.size() - 1);
                clientTms.remove(clientTms.size() - 1);
            } else {
                this.finalClientTm = null;
            }
            if (serverTms.size() > 0) {
                this.finalServerTm = serverTms.get(serverTms.size() - 1);
                serverTms.remove(serverTms.size() - 1);
            } else {
                this.finalServerTm = null;
            }
            clientTms.trimToSize();
            serverTms.trimToSize();
            issuerTms.trimToSize();
            this.startClientTms = clientTms;
            this.startServerTms = serverTms;
            this.issuerTms = issuerTms;
        }

        @Override
        public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
            for(X509TrustManager tm : startClientTms) {
                try {
                    tm.checkClientTrusted(chain, authType);
                    return; // first found
                } catch(CertificateException e) {
                    ; // proceed
                }
            }
            // last try
            if (finalClientTm == null) {
                throw new CertificateException("Cannot validate client certificate (no delegated trust managers for client check)");
            }
            finalClientTm.checkClientTrusted(chain, authType);
        }

        @Override
        public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
            for(X509TrustManager tm : startServerTms) {
                try {
                    tm.checkServerTrusted(chain, authType);
                    return; // first found
                } catch(CertificateException e) {
                    ; // proceed
                }
            }
            // last try
            if (finalServerTm == null) {
                throw new CertificateException("Cannot validate server certificate (no delegated trust managers for server check)");
            }
            finalServerTm.checkServerTrusted(chain, authType);
        }

        @Override
        public X509Certificate[] getAcceptedIssuers() {
            if (issuerTms.size() == 1) return issuerTms.get(0).getAcceptedIssuers();
            else if (issuerTms.size() == 0) return new X509Certificate[] {};

            List<X509Certificate[]> issuerLists = new ArrayList<>(issuerTms.size());
            int totalIssuers = 0;
            for(X509TrustManager tm : issuerTms) { // pre-loop to determine array size
                X509Certificate[] issuers = tm.getAcceptedIssuers();
                issuerLists.add(issuers);
                totalIssuers += issuers.length;
            }

            X509Certificate[] allIssuers = new X509Certificate[totalIssuers];
            int i = 0;
            for(X509Certificate[] issuerList : issuerLists) {
                if (issuerList.length == 0) continue;
                System.arraycopy(issuerList, 0, allIssuers, i, issuerList.length);
                i += issuerList.length;
            }
            return allIssuers;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("[clientTms=");
            sb.append(getClientTms());
            sb.append(", serverTms=");
            sb.append(getServerTms());
            sb.append(", issuerTms=");
            sb.append(getIssuerTms());
            sb.append("]");
            return sb.toString();
        }

        protected List<X509TrustManager> getClientTms() {
            List<X509TrustManager> tmList = new ArrayList<>(startClientTms);
            if (finalClientTm != null) tmList.add(finalClientTm);
            return tmList;
        }

        protected List<X509TrustManager> getServerTms() {
            List<X509TrustManager> tmList = new ArrayList<>(startServerTms);
            if (finalServerTm != null) tmList.add(finalServerTm);
            return tmList;
        }

        protected List<X509TrustManager> getIssuerTms() {
            return issuerTms;
        }

        public static class TrustManagerEntry {
            protected final X509TrustManager trustManager;
            protected final boolean checkClient;
            protected final boolean checkServer;
            protected final boolean acceptedIssuers;

            public TrustManagerEntry(X509TrustManager trustManager, boolean checkClient, boolean checkServer,
                    boolean acceptedIssuers) {
                this.trustManager = trustManager;
                this.checkClient = checkClient;
                this.checkServer = checkServer;
                this.acceptedIssuers = acceptedIssuers;
            }
            public TrustManagerEntry(X509TrustManager trustManager) {
                this(trustManager, true, true, true);
            }

            public X509TrustManager getTrustManager() { return trustManager; }
            public boolean isCheckClient() { return checkClient; }
            public boolean isCheckServer() { return checkServer; }
            public boolean isAcceptedIssuers() { return acceptedIssuers; }
        }
    }

    /**
     * Parses certificate validation ("trust methods") configuration from properties
     * to configure and produce a {@link DelegatingTrustManager}.
     */
    @SuppressWarnings("serial")
    public static class CertCheckConfig implements Serializable {
        protected final List<TrustMethodConfig> configs;
        protected final GeneralConfig generalCfg;

        public static class GeneralConfig {
            protected Boolean checkClientDefault;
            protected Boolean checkServerDefault;
            protected Boolean acceptedIssuersDefault;
            protected Boolean optionalDefault; // default false

            public GeneralConfig() {}

            public Boolean getCheckClientDefault() { return checkClientDefault; }
            public GeneralConfig setCheckClientDefault(Boolean checkClientDefault) { this.checkClientDefault = checkClientDefault; return this; }
            public Boolean getCheckServerDefault() { return checkServerDefault; }
            public GeneralConfig setCheckServerDefault(Boolean checkServerDefault) { this.checkServerDefault = checkServerDefault; return this; }
            public Boolean getAcceptedIssuersDefault() { return acceptedIssuersDefault; }
            public GeneralConfig setAcceptedIssuersDefault(Boolean acceptedIssuersDefault) { this.acceptedIssuersDefault = acceptedIssuersDefault; return this; }
            public Boolean getOptionalDefault() { return optionalDefault; }
            public GeneralConfig setOptionalDefault(Boolean optionalDefault) { this.optionalDefault = optionalDefault; return this; }

            public GeneralConfig useServerCheckDefaults() { checkClientDefault = false; checkServerDefault = true; acceptedIssuersDefault = false; return this; }
            public GeneralConfig useClientServerCheckDefaults() { checkClientDefault = true; checkServerDefault = true; acceptedIssuersDefault = true; return this; }

            @Override
            public String toString() {
                return "[checkClientDefault=" + checkClientDefault + ", checkServerDefault="
                        + checkServerDefault + ", acceptedIssuersDefault=" + acceptedIssuersDefault
                        + ", optionalDefault=" + optionalDefault + "]";
            }
        }

        private CertCheckConfig(GeneralConfig generalConfig, Map<String, Map<String, Object>> entries) {
            this.generalCfg = generalConfig;
            List<TrustMethodConfig> configs = new ArrayList<>(entries.size());
            for(Map.Entry<String, Map<String, Object>> entry : entries.entrySet()) {
                configs.add(createTrustMethodConfig(entry.getKey(), entry.getValue()));
            }
            Collections.sort(configs, new Comparator<TrustMethodConfig>() {
                @Override
                public int compare(TrustMethodConfig o1, TrustMethodConfig o2) {
                    return Integer.compare(o1.prio, o2.prio);
                }
            });
            this.configs = configs;
        }

        public CertCheckConfig(GeneralConfig generalConfig) {
            this.generalCfg = generalConfig;
            this.configs = Collections.emptyList();
        }

        public static CertCheckConfig fromProperties(Properties properties, String propPrefix, GeneralConfig generalConfig) throws RuntimeException {
            Map<String, Map<String, Object>> entries = new HashMap<>();
            UtilProperties.extractPropertiesWithPrefixAndId(entries, properties, propPrefix + "methods.");
            // NOTE: the "methods." is one layer down so we can accept other more general properties if needed outside the list...
            return new CertCheckConfig(generalConfig, entries);
        }

        public static CertCheckConfig fromPropertiesSafe(String resource, String propPrefix, GeneralConfig generalConfig) {
            try {
                return fromProperties(UtilProperties.getProperties(resource), propPrefix, generalConfig);
            } catch(Exception e) {
                Debug.logError(e, "Error in '" + resource + ".properties' configuration of '" + propPrefix + "': "
                        + e.getMessage(), module);
                return null;
            }
        }

        public List<TrustMethodConfig> getConfigs() {
            return configs;
        }

        protected X509TrustManager produceDelegatingTrustManager() {
            List<TrustManagerEntry> tmInfos = new ArrayList<>();
            for(TrustMethodConfig conf : getConfigs()) {
                tmInfos.add(conf.produceTrustManagerEntry());
            }
            return new DelegatingTrustManager(tmInfos);
        }

        @Override
        public String toString() {
            return "[general: " + generalCfg.toString() + ", methods: " + configs.toString() + "]";
        }

        protected TrustMethodConfig createTrustMethodConfig(String namePrio, Map<String, Object> conf) {
            int prio;
            String prioStr = (String) conf.get("prio");
            if (prioStr != null && !prioStr.isEmpty()) {
                prio = Integer.parseInt(prioStr);
            } else {
                prio = Integer.parseInt(namePrio);
            }
            String type = (String) conf.get("type");
            return createTrustMethodConfig(type, prio, conf);
        }

        protected TrustMethodConfig createTrustMethodConfig(String type, int prio, Map<String, Object> conf) {
            switch(type) {
                case "system":
                    return new SystemMethodConfig(prio, conf);
                case "keystore":
                    return new KeyStoreMethodConfig(prio, conf);
                case "component-keystore":
                    return new ComponentKeyStoreMethodConfig(prio, conf);
                case "component-truststores":
                    return new ComponentTrustStoresMethodConfig(prio, conf);
                case "system-truststore-prop":
                    return new SystemTrustStorePropMethodConfig(prio, conf);
                case "system-truststore-ext":
                    return new SystemTrustStoreExtMethodConfig(prio, conf);
                case "ofbiz-truststores":
                    return new OfbizKeyStoresMethodConfig(prio, conf);
                case "self-signed":
                    return new SelfSignedMethodConfig(prio, conf);
                case "any":
                    return new AnyMethodConfig(prio, conf);
                default:
                    throw new IllegalArgumentException("Invalid certification validation (trust method) type: " + type);
            }
        }

        public abstract class TrustMethodConfig implements Serializable {
            private final String type;
            private final int prio;
            private final boolean checkClient;
            private final boolean checkServer;
            private final boolean acceptedIssuers;
            //private final Map<String, Object> conf;
            private final boolean optional;

            protected TrustMethodConfig(int prio, Map<String, Object> conf) {
                this.type = (String) conf.get("type");
                this.prio = prio;
                //this.conf = conf;
                this.checkClient = UtilMisc.firstNonNull(UtilMisc.booleanValue(conf.get("checkClient")), generalCfg.getCheckClientDefault(), false);
                this.checkServer = UtilMisc.firstNonNull(UtilMisc.booleanValue(conf.get("checkServer")), generalCfg.getCheckServerDefault(), true);
                this.acceptedIssuers = UtilMisc.firstNonNull(UtilMisc.booleanValue(conf.get("acceptedIssuers")), generalCfg.getAcceptedIssuersDefault(), false);
                this.optional = UtilMisc.firstNonNull(UtilMisc.booleanValue(conf.get("optional")), generalCfg.getOptionalDefault(), false);
            }

            public String getType() { return type; }
            public abstract X509TrustManager getTrustManager();

            public TrustManagerEntry produceTrustManagerEntry() {
                X509TrustManager tm = getTrustManager();
                if (tm == null) {
                    if (optional) {
                        return null;
                    } else {
                        throw new IllegalStateException("trust method config '" + getType()
                            + "' could not generate a TrustManager and is not optional");
                    }
                }
                return new TrustManagerEntry(tm, checkClient, checkServer, acceptedIssuers);
            }

            @Override
            public String toString() {
                return toStringOpen() + "]";
            }

            protected String toStringOpen() {
                return "[type=" + getType() + ", prio=" + prio + ", checkClient=" + checkClient + ", checkServer="
                        + checkServer + ", acceptedIssuers=" + acceptedIssuers;
            }
        }

        protected class SystemMethodConfig extends TrustMethodConfig {
            protected SystemMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override public X509TrustManager getTrustManager() { return getSystemTrustManager(); }
        }

        protected class KeyStoreMethodConfig extends TrustMethodConfig {
            protected final URL keystoreFile;
            protected final String keystoreType;
            protected final String keystorePass;
            protected KeyStoreMethodConfig(int prio, Map<String, Object> conf) {
                super(prio, conf);
                try {
                    this.keystoreFile = FlexibleLocation.resolveLocation((String) conf.get("keystoreFile"));
                } catch (MalformedURLException e) {
                    throw new IllegalArgumentException("Invalid keystoreFile location: " + e.getMessage());
                }
                this.keystoreType = (String) conf.get("keystoreType");
                this.keystorePass = (String) conf.get("keystorePass");
                if (UtilValidate.isEmpty(keystoreFile)) {
                    throw new IllegalArgumentException("Missing keystoreFile location");
                }
                if (UtilValidate.isEmpty(keystoreType)) {
                    throw new IllegalArgumentException("Missing keystoreType");
                }
                if (UtilValidate.isEmpty(keystorePass)) {
                    throw new IllegalArgumentException("Missing keystorePass (password)");
                }
            }
            @Override
            public X509TrustManager getTrustManager() {
                try {
                    KeyStore keystore = KeyStoreUtil.getStore(keystoreFile, keystorePass, keystoreType);
                    return getKeyStoreTrustManager(keystore);
                } catch (Exception e) {
                    Debug.logError(e, "Could not load keystore '" + keystoreFile + "': " + e.getMessage(), module);
                    return null;
                }
            }
            @Override
            public String toString() {
                return super.toStringOpen() + ", keystoreFile=" + keystoreFile + ", keystoreType=" + keystoreType
                        + ", keystorePass=xxx]";
            }
        }

        protected class ComponentKeyStoreMethodConfig extends TrustMethodConfig {
            protected final String keystoreName;
            protected final String componentName;
            protected ComponentKeyStoreMethodConfig(int prio, Map<String, Object> conf) {
                super(prio, conf);
                this.keystoreName = (String) conf.get("keystoreName");
                if (UtilValidate.isEmpty(keystoreName)) {
                    throw new IllegalArgumentException("Missing keystoreName (<keystore name=\"...\"/>");
                }
                this.componentName = (String) conf.get("componentName");
            }
            @Override
            public X509TrustManager getTrustManager() {
                List<ComponentConfig.KeystoreInfo> ksiList;
                if (UtilValidate.isNotEmpty(this.componentName)) {
                    try {
                        ComponentConfig cc = ComponentConfig.getComponentConfig(this.componentName);
                        ksiList = cc.getKeystoreInfos();
                    } catch (ComponentException e) {
                        Debug.logError(e, "Could not load component keystore with name '" + keystoreName + "': " + e.getMessage(), module);
                        return null;
                    }
                } else {
                    ksiList = ComponentConfig.getAllKeystoreInfos();
                }
                ListIterator<ComponentConfig.KeystoreInfo> it = ksiList.listIterator(ksiList.size());
                while(it.hasPrevious()) {
                    ComponentConfig.KeystoreInfo ksi = it.previous();
                    if (keystoreName.equals(ksi.getName())) {
                        KeyStore ks = ksi.getKeyStore();
                        try {
                            if (ks == null) {
                                Debug.logError("Unable to load keystore: " + ksi.createResourceHandler().getFullLocation(), module);
                                return null;
                            }
                            return getKeyStoreTrustManager(ks);
                        } catch (Exception e) {
                            Debug.logError(e, "Could not load component keystore with name '" + keystoreName + "': " + e.getMessage(), module);
                        }
                    }
                }
                Debug.logWarning("Could not find component keystore for name '" + keystoreName + "'; can't create TrustManager", module);
                return null;
            }
        }

        protected class SystemTrustStorePropMethodConfig extends TrustMethodConfig {
            protected SystemTrustStorePropMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override
            public X509TrustManager getTrustManager() {
                try {
                    KeyStore keystore = KeyStoreUtil.getSystemTrustStorePropIfSet();
                    if (keystore == null) {
                        Debug.logInfo("The system truststore property javax.net.ssl.trustStore does not point to a valid file; skipping", module);
                        return null;
                    }
                    return getKeyStoreTrustManager(keystore);
                } catch (Exception e) {
                    Debug.logError(e, "Could not read keystore for system truststore property javax.net.ssl.trustStore: "
                            + e.getMessage(), module);
                    return null;
                }
            }
        }

        protected class SystemTrustStoreExtMethodConfig extends TrustMethodConfig {
            protected SystemTrustStoreExtMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override
            public X509TrustManager getTrustManager() {
                try {
                    KeyStore keystore = KeyStoreUtil.getSystemTrustStore();
                    if (keystore == null) {
                        Debug.logInfo("The system truststore (javax.net.ssl.trustStore or other locations) is not set; skipping", module);
                        return null;
                    }
                    return getKeyStoreTrustManager(keystore);
                } catch (Exception e) {
                    Debug.logError(e, "Could not read keystore for system truststore (javax.net.ssl.trustStore or other locations): "
                            + e.getMessage(), module);
                    return null;
                }
            }
        }

        protected class ComponentTrustStoresMethodConfig extends TrustMethodConfig {
            protected ComponentTrustStoresMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override public X509TrustManager getTrustManager() {
                try {
                    return getComponentTrustStoresManager();
                } catch (IOException e) {
                    Debug.logError(e, module);
                    return null;
                }
            }
        }

        protected class OfbizKeyStoresMethodConfig extends TrustMethodConfig {
            protected OfbizKeyStoresMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override public X509TrustManager getTrustManager() {
                try {
                    return getOfbizTrustStoresManager();
                } catch (IOException e) {
                    Debug.logError(e, module);
                    return null;
                }
            }
        }

        protected class SelfSignedMethodConfig extends TrustMethodConfig {
            protected SelfSignedMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override public X509TrustManager getTrustManager() { return getTrustSelfSignedManager(); }
        }

        protected class AnyMethodConfig extends TrustMethodConfig {
            protected AnyMethodConfig(int prio, Map<String, Object> conf) { super(prio, conf); }
            @Override public X509TrustManager getTrustManager() { return getTrustAnyManager(); }
        }
    }
}
