package com.ilscipio.scipio.solr;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.solr.client.solrj.impl.HttpSolrClient;
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
import org.ofbiz.entity.util.EntityQuery;

/**
 * Generic Solr utility class, and helpers to get HttpSolrClient for the Scipio Solr instance.
 */
public abstract class SolrUtil {
    
    public static final String module = SolrUtil.class.getName();
    
    public static final String solrConfigName = "solrconfig";
    public static final String solrUrl = makeSolrWebappUrl();
    public static final String solrFullUrl = makeFullSolrWebappUrl();
    
    // TODO: REVIEW: the "!" standalone character appears not recognized in solr 5 query parser;
    // it only works if space after is removed. but it shouldn't do any harm here so leaving in...
    static final Set<String> noPrefixTerms = UtilMisc.unmodifiableHashSet("AND", "OR", "NOT", "&&", "||", "!", "/*");
    static final Set<Character> noPrefixTermCharPrefixes = UtilMisc.unmodifiableHashSet('+', '-');
    static final Map<Character, Character> termEnclosingCharMap; 
    static {
        Map<Character, Character> map = new HashMap<>();
        map.put('"', '"');
        map.put('{', '}');
        map.put('[', ']');
        map.put('(', ')');
        termEnclosingCharMap = Collections.unmodifiableMap(map);
    }
    
    public static String getSolrConfigVersionStatic() {
        return UtilProperties.getPropertyValue(solrConfigName, "solr.config.version");
    }
    
    public static String makeSolrWebappUrl() {
        final String solrWebappProtocol = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.protocol");
        final String solrWebappDomainName = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.domainName");
        final String solrWebappPath = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.path");
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
    
    public static String makeFullSolrWebappUrl() {
        final String solrDefaultCore = UtilProperties.getPropertyValue(solrConfigName, "solr.core.default");
        return makeSolrWebappUrl() + "/" + solrDefaultCore;
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
    
    public static boolean setSolrDataStatusId(Delegator delegator, String dataStatusId, boolean updateVersion) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
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
            return true;
            
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return false;
        }
    }
    
    public static boolean setSolrDataStatusId(Delegator delegator, String dataStatusId) {
        return setSolrDataStatusId(delegator, dataStatusId, false);
    }
    
    public static HttpSolrClient getHttpSolrClient(String core) {
        if (UtilValidate.isNotEmpty(core)) {
            return makeHttpSolrClientFromUrl(SolrUtil.solrUrl + "/" + core);
        }
        else {
            return getHttpSolrClient();
        }
    }
    
    public static HttpSolrClient getHttpSolrClient() {
        return makeHttpSolrClientFromUrl(SolrUtil.solrFullUrl);
    }
    
    public static HttpSolrClient makeHttpSolrClientFromUrl(String url) {
        // TODO: REVIEW: .allowCompression(false)
        return new HttpSolrClient.Builder(url).build();
    }
    
}
