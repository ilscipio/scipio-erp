package com.ilscipio.solr;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import javolution.util.FastMap;

/**
 * Solr utility class.
 */
public abstract class SolrUtil {
    
    public static final String module = SolrUtil.class.getName();
    private static String[] solrProdAttribute = { "productId", "internalName", "manu", "size", "smallImage", "mediumImage", "largeImage", "listPrice", "defaultPrice", "inStock", "isVirtual" };

    public static final String solrConfigName = "solrconfig.properties";
    public static final String solrUrl = makeSolrWebappUrl();
    public static final String solrFullUrl = makeFullSolrWebappUrl();
    
    public static String makeSolrWebappUrl() {
        final String solrWebappProtocol = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.protocol");
        final String solrWebappDomainName = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.domainName");
        final String solrWebappPath = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.path");
        final String solrWebappPortOverride = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.portOverride");
        
        String solrPort;
        if (UtilValidate.isNotEmpty(solrWebappPortOverride)) {
            solrPort = solrWebappPortOverride;
        }
        else {
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
            }
            else if ("false".equalsIgnoreCase(sysProp)) {
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
        }
        else {
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
    
    
    public static SolrInputDocument generateSolrDocument(Map<String, Object> context) throws GenericEntityException {
        SolrInputDocument doc1 = new SolrInputDocument();

        // add defined attributes
        for (int i = 0; i < solrProdAttribute.length; i++) {
            if (context.get(solrProdAttribute[i]) != null) {
                doc1.addField(solrProdAttribute[i], context.get(solrProdAttribute[i]).toString());
            }
        }

        // add catalog
        if (context.get("catalog") != null) {
            List<String> catalog = UtilGenerics.<String>checkList(context.get("catalog"));
            for (String c : catalog) {
                doc1.addField("catalog", c);
            }
        }

        // add categories
        if (context.get("category") != null) {
            List<String> category = UtilGenerics.<String>checkList(context.get("category"));
            Iterator<String> catIter = category.iterator();
            while (catIter.hasNext()) {
                /*
                 * GenericValue cat = (GenericValue) catIter.next(); GenericValue prodCategory = cat.getRelatedOneCache("ProductCategory"); if (prodCategory.get("description") != null) {
                 * doc1.addField("category", prodCategory.get("description")); } doc1.addField("cat", prodCategory.get("productCategoryId"));
                 */
                String cat = (String) catIter.next();
                doc1.addField("cat", cat);
            }
        }

        // add features
        if (context.get("features") != null) {
            Set<String> features = UtilGenerics.<String>checkSet(context.get("features"));
            Iterator<String> featIter = features.iterator();
            while (featIter.hasNext()) {
                String feat = featIter.next();
                doc1.addField("features", feat);
            }
        }

        // add attributes
        if (context.get("attributes") != null) {
            List<String> attributes = UtilGenerics.<String>checkList(context.get("attributes"));
            Iterator<String> attrIter = attributes.iterator();
            while (attrIter.hasNext()) {
                String attr = attrIter.next();
                doc1.addField("attributes", attr);
            }
        }

        // add title
        if (context.get("title") != null) {
            Map<String, String> title = UtilGenerics.<String, String>checkMap(context.get("title"));
            for (Map.Entry<String, String> entry : title.entrySet()) {
                doc1.addField("title_i18n_" + entry.getKey(), entry.getValue());
            }
        }

        // add short_description
        if (context.get("description") != null) {
            Map<String, String> description = UtilGenerics.<String, String>checkMap(context.get("description"));
            for (Map.Entry<String, String> entry : description.entrySet()) {
                doc1.addField("description_i18n_" + entry.getKey(), entry.getValue());
            }
        }

        // add short_description
        if (context.get("longDescription") != null) {
            Map<String, String> longDescription = UtilGenerics.<String, String>checkMap(context.get("longDescription"));
            for (Map.Entry<String, String> entry : longDescription.entrySet()) {
                doc1.addField("longdescription_i18n_" + entry.getKey(), entry.getValue());
            }
        }

        return doc1;
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId,categoryId,productId,null,displayproducts,viewIndex,viewSize, null);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize, null);
    }

    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, String core) {
        // create the data model
        Map<String, Object> result = FastMap.newInstance();
        HttpSolrClient client = null;
        QueryResponse returnMap = new QueryResponse();
        try {
            // do the basic query
            if (UtilValidate.isNotEmpty(core))
                client = new HttpSolrClient(SolrUtil.solrUrl + "/" + core);
            else
                client = new HttpSolrClient(SolrUtil.solrFullUrl);
            // create Query Object
            String query = "inStock[1 TO *]";
            if (categoryId != null)
                query += " +cat:"+ categoryId;
            else if (productId != null)
                query += " +productId:" + productId;
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery(query);

            if (catalogId != null)
                solrQuery.setFilterQueries("catalog:" + catalogId);
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
            Debug.logVerbose("solr: solrQuery: " + solrQuery, module);
            returnMap = client.query(solrQuery,METHOD.POST);
            result.put("rows", returnMap);
            result.put("numFound", returnMap.getResults().getNumFound());
        } catch (Exception e) {
            Debug.logError(e.getMessage(), module);
        }
        return result;
    }

    public static String getSolrDataStatusId(Delegator delegator) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                    .where("solrId", "SOLR-MAIN").cache(false).queryOne();
            if (solrStatus == null) {
                Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - seed data missing?", module);
            }
            else {
                return solrStatus.getString("dataStatusId");
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return null;
    }
    
    public static boolean setSolrDataStatusId(Delegator delegator, String dataStatusId) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                    .where("solrId", "SOLR-MAIN").cache(false).queryOne();
            //solrStatus = delegator.findOne("SolrStatus", UtilMisc.toMap("solrId", "SOLR-MAIN"), false);
            if (solrStatus == null) {
                Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - creating new", module);
                solrStatus = delegator.create("SolrStatus", "solrId", "SOLR-MAIN", "dataStatusId", dataStatusId);
            }
            else {
                solrStatus.setString("dataStatusId", dataStatusId);
                solrStatus.store();
            }
            return true;
            
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return false;
        }
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
    
    /**
     * Escapes all special solr/query characters in the given query expression
     * <em>not</em> enclosed in quotes (simple term).
     * At current time, this includes at least: 
     * <code>+ - && || ! ( ) { } [ ] ^ " ~ * ? : \ /</code> and whitespace.
     * NOTE: The result should NOT be enclosed in quotes; use {@link #escapeQueryPhrase} for that.
     * @see #escapeQueryQuoted
     */
    public static String escapeQueryPlain(String term) {
        return ClientUtils.escapeQueryChars(term);
        // Reference implementation:
//        StringBuilder sb = new StringBuilder();
//        for (int i = 0; i < s.length(); i++) {
//          char c = s.charAt(i);
//          // These characters are part of the query syntax and must be escaped
//          if (c == '\\' || c == '+' || c == '-' || c == '!'  || c == '(' || c == ')' || c == ':'
//            || c == '^' || c == '[' || c == ']' || c == '\"' || c == '{' || c == '}' || c == '~'
//            || c == '*' || c == '?' || c == '|' || c == '&'  || c == ';' || c == '/'
//            || Character.isWhitespace(c)) {
//            sb.append('\\');
//          }
//          sb.append(c);
//        }
//        return sb.toString();
    }
    
    /**
     * Escapes all special solr/query characters in the given query expression intended to be
     * enclosed in double-quotes (phrase).
     * At current time, this escapes the backslash and double-quote characters only.
     * @see #escapeQueryPlain
     */
    public static String escapeQueryQuoted(String phrase) {
        final String s = phrase;
        // Reference implementation: http://api.drupalhelp.net/api/apachesolr/SolrPhpClient--Apache--Solr--Service.php/function/Apache_Solr_Service%3A%3AescapePhrase/5
        // TODO: REVIEW: make sure this actually corresponds to the solr/lucene parser implementation,
        // w.r.t. the backslash handling; the php reference might be unofficial...
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            // there is no provided implementation for this...
            if (c == '\\' || c == '\"') {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }
    
}
