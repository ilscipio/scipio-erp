package com.ilscipio.solr;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
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
 * Solr utility class.
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
        return UtilProperties.getPropertyValue("solrconfig", "solr.config.version");
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
            solrPort = UtilProperties.getPropertyValue("url.properties", ("https".equals(solrWebappProtocol) ? "port.https" : "port.http"));
        }
        
        return solrWebappProtocol + "://" + solrWebappDomainName + ":" + solrPort + solrWebappPath;
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
        return OfbizSolrInfoServlet.isServletInitStatusReached();
    }
    
    public static boolean isEcaTreatConnectErrorNonFatal() {
        Boolean treatConnectErrorNonFatal = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.treatConnectErrorNonFatal", true);
        return Boolean.TRUE.equals(treatConnectErrorNonFatal);
    }
    
    
    /**
     * @deprecated Use {@link ProductUtil#generateSolrProductDocument(Map)} instead
     */
    public static SolrInputDocument generateSolrDocument(Map<String, Object> context) throws GenericEntityException {
        return ProductUtil.generateSolrProductDocument(null, null, context);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId,categoryId,productId,null,displayproducts,viewIndex,viewSize, null);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize, null);
    }

    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, String core) {
        // create the data model
        Map<String, Object> result = new HashMap<>();
        HttpSolrClient client = null;
        QueryResponse returnMap = new QueryResponse();
        try {
            // do the basic query
            client = getHttpSolrClient(core);
            // create Query Object
            String query = "inStock[1 TO *]";
            if (categoryId != null)
                query += " +cat:"+ SolrExprUtil.escapeTermFull(categoryId);
            else if (productId != null)
                query += " +productId:" + SolrExprUtil.escapeTermFull(productId);
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery(query);

            if (catalogId != null)
                solrQuery.setFilterQueries("catalog:" + SolrExprUtil.escapeTermFull(catalogId));
            if (displayproducts) {
                if (viewSize > -1) {
                    solrQuery.setRows(viewSize);
                } else
                    solrQuery.setRows(50000);
                if (viewIndex > -1) {
                    // 2016-04-01: This must be calculated
                    //solrQuery.setStart(viewIndex);
                    if (viewSize > 0) {
                        solrQuery.setStart(viewSize * viewIndex);
                    }
                }
            } else {
                solrQuery.setFields("cat");
                solrQuery.setRows(0);
            }
            
            if(UtilValidate.isNotEmpty(facetPrefix)){
                solrQuery.setFacetPrefix(facetPrefix);
            }
            
            solrQuery.setFacetMinCount(0);
            solrQuery.setFacet(true);
            solrQuery.addFacetField("cat");
            solrQuery.setFacetLimit(-1);
            if (Debug.verboseOn()) Debug.logVerbose("solr: solrQuery: " + solrQuery, module);
            returnMap = client.query(solrQuery,METHOD.POST);
            result.put("rows", returnMap);
            result.put("numFound", returnMap.getResults().getNumFound());
        } catch (Exception e) {
            Debug.logError(e.getMessage(), module);
        }
        return result;
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
    
    /**
     * Returns the closest whole viewIndex.
     */
    public static Integer calcResultViewIndex(SolrDocumentList results, Integer viewSize) {
        Integer viewIndex = null;
        if (results != null && viewSize != null && viewSize > 0) {
            long start = results.getStart();
            viewIndex = (int) (start / (long) viewSize);
        }
        return viewIndex;
    }
    
    public static HttpSolrClient getHttpSolrClient(String core) {
        if (UtilValidate.isNotEmpty(core)) return new HttpSolrClient(SolrUtil.solrUrl + "/" + core);
        else return getHttpSolrClient();
    }
    
    public static HttpSolrClient getHttpSolrClient() {
        return new HttpSolrClient(SolrUtil.solrFullUrl);
    }
    
    /**
     * Checks if the exception extracted by {@link #getSolrNestedException} is a syntax error.
     * FIXME: AWFUL HEURISTIC
     */
    public static boolean isSolrQuerySyntaxError(Throwable t) {
        // exception message usually contains the string: "org.apache.solr.search.SyntaxError"
        // hopefully this is accurate enough... how else to check? cause is not set and
        // the root SyntaxError is from an inaccessible jar (CANNOT add it to classpath)
        return ((t instanceof SolrException) && t.getMessage().toLowerCase().contains("syntax")); 
    }
    
}
