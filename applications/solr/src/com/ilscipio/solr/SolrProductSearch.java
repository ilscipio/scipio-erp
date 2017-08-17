package com.ilscipio.solr;

import java.io.IOException;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.FacetField.Count;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.SpellCheckResponse.Suggestion;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import javolution.util.FastList;
import javolution.util.FastMap;

/**
 * Base class for OFBiz Test Tools test case implementations.
 */
public abstract class SolrProductSearch {

    public static final String module = SolrProductSearch.class.getName();

    /**
     * Adds product to solr, with product denoted by productId field in instance
     * attribute - intended for use with ECAs/SECAs.
     */
    public static Map<String, Object> addToSolr(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        // NOTE: 2017-04-13: type may be org.ofbiz.entity.GenericValue or GenericPk, so use common parent GenericEntity
        //GenericValue productInstance = (GenericValue) context.get("instance");
        GenericEntity productInstance = (GenericEntity) context.get("instance");
        
        String productId = (String) productInstance.get("productId");

        boolean indexed = false;
        Boolean webappInitPassed = null;
        boolean skippedDueToWebappInit = false;
        
        if (SolrUtil.isSolrEcaEnabled()) {
            webappInitPassed = SolrUtil.isSolrEcaWebappInitCheckPassed();
            if (webappInitPassed) {
                Debug.logVerbose("Solr: addToSolr: Running indexing for productId '" + productId + "'", module);

                try {
                    GenericValue product = delegator.findOne("Product", UtilMisc.toMap("productId", productId), false);
                    Map<String, Object> dispatchContext = ProductUtil.getProductContent(product, dctx, context);
                    dispatchContext.put("treatConnectErrorNonFatal", SolrUtil.isEcaTreatConnectErrorNonFatal());
                    copyStdServiceFieldsNotSet(context, dispatchContext);
                    Map<String, Object> runResult = dispatcher.runSync("addToSolrIndex", dispatchContext);
                    String runMsg = ServiceUtil.getErrorMessage(runResult);
                    if (UtilValidate.isEmpty(runMsg)) {
                        runMsg = null;
                    }
                    if (ServiceUtil.isError(runResult)) {
                        result = ServiceUtil.returnError(runMsg);
                    } else if (ServiceUtil.isFailure(runResult)) {
                        result = ServiceUtil.returnFailure(runMsg);
                    } else {
                        result = ServiceUtil.returnSuccess();
                        indexed = true;
                    }
                } catch (Exception e) {
                    Debug.logError(e, e.getMessage(), module);
                    result = ServiceUtil.returnError(e.toString());
                }
            } else {
                if (Debug.verboseOn()) {
                    final String statusMsg = "Solr webapp not available; skipping indexing for productId '" + productId + "'";
                    Debug.logVerbose("Solr: addToSolr: " + statusMsg, module);
                }
                result = ServiceUtil.returnSuccess();
                skippedDueToWebappInit = true;
            }
        } else {
            if (Debug.verboseOn()) {
                final String statusMsg = "Solr ECA indexing disabled; skipping indexing for productId '" + productId + "'";
                Debug.logVerbose("Solr: addToSolr: " + statusMsg, module);
            }
            result = ServiceUtil.returnSuccess();
        }
        
        if (!indexed && UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.enabled", false)) {
            boolean markDirtyNoWebappCheck = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.noWebappCheck", false);
            if (!(markDirtyNoWebappCheck && skippedDueToWebappInit)) {
                if (Debug.verboseOn()) {
                    final String statusMsg = "Did not index productId '" + productId + "'; marking SOLR data as dirty (old)";
                    Debug.logVerbose("Solr: addToSolr: " + statusMsg, module);
                }
                SolrUtil.setSolrDataStatusId(delegator, "SOLR_DATA_OLD");
            }
        }
        
        return result;
    }

    /**
     * Adds product to solr index.
     */
    public static Map<String, Object> addToSolrIndex(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        HttpSolrClient client = null;
        Map<String, Object> result;
        String productId = (String) context.get("productId");
        // connectErrorNonFatal is a necessary option because in some cases it
        // may be considered normal that solr server is unavailable;
        // don't want to return error and abort transactions in these cases.
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        try {
            Debug.logInfo("Solr: Generating and indexing document for productId '" + productId + "'", module);

            if (UtilValidate.isNotEmpty(context.get("core")))
                client = new HttpSolrClient(SolrUtil.solrUrl + "/" + context.get("core"));
            else
                client = new HttpSolrClient(SolrUtil.solrFullUrl);
            // Debug.log(server.ping().toString());

            // Construct Documents
            SolrInputDocument doc1 = SolrUtil.generateSolrDocument(context);
            Collection<SolrInputDocument> docs = new ArrayList<SolrInputDocument>();

            if (Debug.verboseOn()) {
                Debug.logVerbose("Solr: Indexing document: " + doc1.toString(), module);
            }

            docs.add(doc1);

            // push Documents to server
            client.add(docs);
            client.commit();

            final String statusStr = "Document for productId " + productId + " added to solr index";
            Debug.logInfo("Solr: " + statusStr, module);
            result = ServiceUtil.returnSuccess(statusStr);
        } catch (MalformedURLException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "urlError");
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to commit productId " + context.get("productId") + "; product not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
                result.put("errorType", "connectError");
            } else {
                Debug.logError(e, e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "solrServerError");
            }
        } catch (IOException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "ioError");
        } finally {
            try {
                if (UtilValidate.isNotEmpty(client))
                    client.close();
            } catch (IOException e) {
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "ioError");
            }
        }
        return result;
    }

    /**
     * Adds a List of products to the solr index.
     * <p>
     * This is faster than reflushing the index each time.
     */
    public static Map<String, Object> addListToSolrIndex(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        HttpSolrClient client = null;
        Map<String, Object> result;
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        try {
            Collection<SolrInputDocument> docs = new ArrayList<SolrInputDocument>();

            // Construct Documents
            List<Map<String, Object>> fieldList = UtilGenerics.<Map<String, Object>> checkList(context.get("fieldList"));

            if (fieldList.size() > 0) {
                Debug.logInfo("Solr: Generating and adding " + fieldList.size() + " documents to solr index", module);
    
                for (Iterator<Map<String, Object>> fieldListIterator = fieldList.iterator(); fieldListIterator.hasNext();) {
                    SolrInputDocument doc1 = SolrUtil.generateSolrDocument(fieldListIterator.next());
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("Solr: Indexing document: " + doc1.toString(), module);
                    }
                    docs.add(doc1);
                }
                // push Documents to server
                if (UtilValidate.isNotEmpty(context.get("core"))) {
                    client = new HttpSolrClient(SolrUtil.solrUrl + "/" + context.get("core"));
                } else {
                    client = new HttpSolrClient(SolrUtil.solrFullUrl);
                }
                client.add(docs);
                client.commit();
            } else {
                Debug.logInfo("Solr: No documents to index", module);
            }

            final String statusStr = "Added " + fieldList.size() + " documents to solr index";
            Debug.logInfo("Solr: " + statusStr, module);
            result = ServiceUtil.returnSuccess(statusStr);
        } catch (MalformedURLException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "urlError");
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to commit product list; products not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
                result.put("errorType", "connectError");
            } else {
                Debug.logError(e, e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "solrServerError");
            }
        } catch (IOException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "ioError");
        } finally {
            try {
                if (UtilValidate.isNotEmpty(client))
                    client.close();
            } catch (IOException e) {
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "ioError");
            }
        }
        return result;
    }

    /**
     * Runs a query on the Solr Search Engine and returns the results.
     * <p>
     * This function only returns an object of type QueryResponse, so it is
     * probably not a good idea to call it directly from within the groovy files
     * (As a decent example on how to use it, however, use keywordSearch
     * instead).
     */
    public static Map<String, Object> runSolrQuery(DispatchContext dctx, Map<String, Object> context) {
        // get Connection
        HttpSolrClient client = null;
        Map<String, Object> result;
        try {
            if (UtilValidate.isNotEmpty(context.get("core")))
                client = new HttpSolrClient(SolrUtil.solrUrl + "/" + context.get("core"));
            else
                client = new HttpSolrClient(SolrUtil.solrFullUrl);
            // create Query Object
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery((String) context.get("query"));
            // solrQuery.setQueryType("dismax");
            boolean faceted = (Boolean) context.get("facet");
            if (faceted) {
                solrQuery.setFacet(faceted);
                solrQuery.addFacetField("manu");
                solrQuery.addFacetField("cat");
                solrQuery.setFacetMinCount(1);
                solrQuery.setFacetLimit(8);

                solrQuery.addFacetQuery("listPrice:[0 TO 50]");
                solrQuery.addFacetQuery("listPrice:[50 TO 100]");
                solrQuery.addFacetQuery("listPrice:[100 TO 250]");
                solrQuery.addFacetQuery("listPrice:[250 TO 500]");
                solrQuery.addFacetQuery("listPrice:[500 TO 1000]");
                solrQuery.addFacetQuery("listPrice:[1000 TO 2500]");
                solrQuery.addFacetQuery("listPrice:[2500 TO 5000]");
                solrQuery.addFacetQuery("listPrice:[5000 TO 10000]");
                solrQuery.addFacetQuery("listPrice:[10000 TO 50000]");
                solrQuery.addFacetQuery("listPrice:[50000 TO *]");
            }

            boolean spellCheck = (Boolean) context.get("spellcheck");
            if (spellCheck) {
                solrQuery.setParam("spellcheck", spellCheck);
            }

            boolean highLight = (Boolean) context.get("highlight");
            if (highLight) {
                solrQuery.setHighlight(highLight);
                solrQuery.setHighlightSimplePre("<span class=\"highlight\">");
                solrQuery.addHighlightField("description");
                solrQuery.setHighlightSimplePost("</span>");
                solrQuery.setHighlightSnippets(2);
            }

            // Set additional Parameter
            // SolrQuery.ORDER order = SolrQuery.ORDER.desc;

            // 2016-04-01: start must be calculated
            //if (context.get("viewIndex") != null && (Integer) context.get("viewIndex") > 0) {
            //    solrQuery.setStart((Integer) context.get("viewIndex"));
            //}
            //if (context.get("viewSize") != null && (Integer) context.get("viewSize") > 0) {
            //    solrQuery.setRows((Integer) context.get("viewSize"));
            //}
            Integer start = (Integer) context.get("start");
            Integer viewIndex = (Integer) context.get("viewIndex");
            Integer viewSize = (Integer) context.get("viewSize");
            if (viewSize != null && viewSize > 0) {
                solrQuery.setRows(viewSize);
            }
            if (start != null) {
                if (start > 0) {
                    solrQuery.setStart(start);
                }
            }
            else if (viewIndex != null) {
                if (viewIndex > 0 && viewSize != null && viewSize > 0) {
                    solrQuery.setStart(viewIndex * viewSize);
                }
            }
            

            // if ((List) context.get("queryFilter") != null &&
            // ((ArrayList<SolrDocument>) context.get("queryFilter")).size() >
            // 0) {
            // List filter = (List) context.get("queryFilter");
            // String[] tn = new String[filter.size()];
            // Iterator it = filter.iterator();
            // for (int i = 0; i < filter.size(); i++) {
            // tn[i] = (String) filter.get(i);
            // }
            // solrQuery.setFilterQueries(tn);
            // }
            String queryFilter = (String) context.get("queryFilter");
            if (UtilValidate.isNotEmpty(queryFilter))
                solrQuery.setFilterQueries(queryFilter.split(" "));
            if ((String) context.get("returnFields") != null) {
                solrQuery.setFields((String) context.get("returnFields"));
            }

            // if((Boolean)context.get("sortByReverse"))order.reverse();
            if ((String) context.get("sortBy") != null && ((String) context.get("sortBy")).length() > 0) {
                SolrQuery.ORDER order;
                if (!((Boolean) context.get("sortByReverse")))
                    order = SolrQuery.ORDER.asc;
                else
                    order = SolrQuery.ORDER.desc;
                solrQuery.setSort(((String) context.get("sortBy")).replaceFirst("-", ""), order);
            }

            if ((String) context.get("facetQuery") != null) {
                solrQuery.addFacetQuery((String) context.get("facetQuery"));
            }

            QueryResponse rsp = client.query(solrQuery, METHOD.POST);
            result = ServiceUtil.returnSuccess();
            result.put("queryResult", rsp);
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        return result;
    }

    /**
     * Performs solr products search.
     */
    public static Map<String, Object> productsSearch(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();

        try {
            Map<String, Object> dispatchMap = FastMap.newInstance();
            if (UtilValidate.isNotEmpty(context.get("productCategoryId"))) {
                String productCategoryId = (String) context.get("productCategoryId");
                dispatchMap.put("query", "cat:*" + productCategoryId + "*");
            } else
                return ServiceUtil.returnError("Missing product category id");
            Integer viewSize = null;
            if (context.get("viewSize") != null) {
                viewSize = Integer.parseInt((String) context.get("viewSize"));
                dispatchMap.put("viewSize", viewSize);
            }
            Integer viewIndex = null;
            if (context.get("viewIndex") != null) {
                viewIndex = Integer.parseInt((String) context.get("viewIndex"));
                dispatchMap.put("viewIndex", viewIndex);
            }
            if (context.get("queryFilter") != null) {
                dispatchMap.put("queryFilter", context.get("queryFilter"));
            }
            dispatchMap.put("facet", false);
            dispatchMap.put("spellcheck", true);
            dispatchMap.put("highlight", true);
            copyStdServiceFieldsNotSet(context, dispatchMap);
            Map<String, Object> searchResult = dispatcher.runSync("runSolrQuery", dispatchMap);
            QueryResponse queryResult = (QueryResponse) searchResult.get("queryResult");
            result = ServiceUtil.returnSuccess();
            result.put("results", queryResult.getResults());
            result.put("listSize", queryResult.getResults().getNumFound());
            // 2016-04-01: Need to translate this
            //result.put("viewIndex", queryResult.getResults().getStart());
            result.put("start", queryResult.getResults().getStart());
            result.put("viewIndex", SolrUtil.calcResultViewIndex(queryResult.getResults(), viewSize));
            result.put("viewSize", viewSize);
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        return result;
    }

    /**
     * Performs keyword search.
     * <p>
     * The search form requires the result to be in a specific layout, so this
     * will generate the proper results.
     */
    public static Map<String, Object> keywordSearch(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();

        try {
            if (context.get("query") == null || context.get("query").equals(""))
                context.put("query", "*:*");

            Map<String, Object> dispatchMap = FastMap.newInstance();
            Integer viewSize = null;
            if (context.get("viewSize") != null) {
                viewSize = Integer.parseInt((String) context.get("viewSize"));
                dispatchMap.put("viewSize", viewSize);
            }
            Integer viewIndex = null;
            if (context.get("viewIndex") != null) {
                viewIndex = Integer.parseInt((String) context.get("viewIndex"));
                dispatchMap.put("viewIndex", viewIndex);
            }
            if (context.get("query") != null)
                dispatchMap.put("query", context.get("query"));
            if (context.get("queryFilter") != null)
                dispatchMap.put("queryFilter", context.get("queryFilter"));
            dispatchMap.put("spellcheck", true);
            copyStdServiceFieldsNotSet(context, dispatchMap);
            Map<String, Object> searchResult = dispatcher.runSync("runSolrQuery", dispatchMap);
            QueryResponse queryResult = (QueryResponse) searchResult.get("queryResult");

            List<List<String>> suggestions = FastList.newInstance();
            if (queryResult.getSpellCheckResponse() != null && queryResult.getSpellCheckResponse().getSuggestions() != null) {
                Iterator<Suggestion> iter = queryResult.getSpellCheckResponse().getSuggestions().iterator();
                while (iter.hasNext()) {
                    Suggestion resultDoc = iter.next();
                    Debug.logInfo("Suggestion " + resultDoc.getAlternatives(), module);
                    suggestions.add(resultDoc.getAlternatives());
                }
            }

            Boolean isCorrectlySpelled = true;
            if (queryResult.getSpellCheckResponse() != null) {
                isCorrectlySpelled = queryResult.getSpellCheckResponse().isCorrectlySpelled();
            }

            result = ServiceUtil.returnSuccess();
            result.put("isCorrectlySpelled", isCorrectlySpelled);

            Map<String, Integer> facetQuery = queryResult.getFacetQuery();
            Map<String, String> facetQueries = FastMap.newInstance();
            for (String fq : facetQuery.keySet()) {
                if (facetQuery.get(fq).intValue() > 0)
                    facetQueries.put(fq, fq.replaceAll("^.*\\u005B(.*)\\u005D", "$1") + " (" + facetQuery.get(fq).intValue() + ")");
            }

            Map<String, Map<String, Long>> facetFields = FastMap.newInstance();
            List<FacetField> facets = queryResult.getFacetFields();
            for (FacetField facet : facets) {
                Map<String, Long> facetEntry = FastMap.newInstance();
                List<FacetField.Count> facetEntries = facet.getValues();
                if (UtilValidate.isNotEmpty(facetEntries)) {
                    for (FacetField.Count fcount : facetEntries)
                        facetEntry.put(fcount.getName(), fcount.getCount());
                    facetFields.put(facet.getName(), facetEntry);
                }
            }

            result.put("results", queryResult.getResults());
            result.put("facetFields", facetFields);
            result.put("facetQueries", facetQueries);
            result.put("queryTime", queryResult.getElapsedTime());
            result.put("listSize", queryResult.getResults().getNumFound());
            // 2016-04-01: Need to translate this
            //result.put("viewIndex", queryResult.getResults().getStart());
            result.put("start", queryResult.getResults().getStart());
            result.put("viewIndex", SolrUtil.calcResultViewIndex(queryResult.getResults(), viewSize));
            result.put("viewSize", viewSize);
            result.put("suggestions", suggestions);

        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        return result;
    }

    /**
     * Returns a map of the categories currently available under the root
     * element.
     */
    public static Map<String, Object> getAvailableCategories(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        try {
            boolean displayProducts = false;
            if (UtilValidate.isNotEmpty(context.get("displayProducts")))
                displayProducts = (Boolean) context.get("displayProducts");

            int viewIndex = 0;
            int viewSize = 9;
            if (displayProducts) {
                viewIndex = (Integer) context.get("viewIndex");
                viewSize = (Integer) context.get("viewSize");
            }
            String catalogId = null;
            if (UtilValidate.isNotEmpty(context.get("catalogId")))
                catalogId = (String) context.get("catalogId");
            
            List<String> currentTrail = UtilGenerics.checkList(context.get("currentTrail"));

            // String productCategoryId = (String)
            // context.get("productCategoryId") != null ?
            // CategoryUtil.getCategoryNameWithTrail((String)
            // context.get("productCategoryId"), catalogId, dctx, currentTrail): null;
            String productCategoryId = (String) context.get("productCategoryId") != null
                    ? CategoryUtil.getCategoryNameWithTrail((String) context.get("productCategoryId"), catalogId, dctx, currentTrail) : null;
            Debug.logInfo("productCategoryId " + productCategoryId, module);
            Map<String, Object> query = SolrUtil.categoriesAvailable(catalogId, productCategoryId, (String) context.get("productId"), displayProducts,
                    viewIndex, viewSize);

            QueryResponse cat = (QueryResponse) query.get("rows");
            result = ServiceUtil.returnSuccess();
            result.put("numFound", (long) 0);
            Map<String, Object> categories = FastMap.newInstance();
            List<FacetField> catList = (List<FacetField>) cat.getFacetFields();
            for (Iterator<FacetField> catIterator = catList.iterator(); catIterator.hasNext();) {
                FacetField field = (FacetField) catIterator.next();
                List<Count> catL = (List<Count>) field.getValues();
                if (catL != null) {
                    // log.info("FacetFields = "+catL);
                    for (Iterator<Count> catIter = catL.iterator(); catIter.hasNext();) {
                        FacetField.Count f = (FacetField.Count) catIter.next();
                        if (f.getCount() > 0) {
                            categories.put(f.getName(), Long.toString(f.getCount()));
                        }
                    }
                    result.put("categories", categories);
                    result.put("numFound", cat.getResults().getNumFound());
                    // log.info("The returned map is this:"+result);
                }
            }
        } catch (Exception e) {
            result = ServiceUtil.returnError(e.toString());
            result.put("numFound", (long) 0);
        }
        return result;
    }

    /**
     * Return a map of the side deep categories.
     */
    public static Map<String, Object> getSideDeepCategories(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        try {
            String catalogId = null;
            if (UtilValidate.isNotEmpty(context.get("catalogId")))
                catalogId = (String) context.get("catalogId");
            
            List<String> currentTrail = UtilGenerics.checkList(context.get("currentTrail"));

            // 2016-03-22: FIXME?: I think we could call getCategoryNameWithTrail with showDepth=false,
            // instead of check in loop...
            String productCategoryId = (String) context.get("productCategoryId") != null
                    ? CategoryUtil.getCategoryNameWithTrail((String) context.get("productCategoryId"), catalogId, dctx, currentTrail) : null;
            result = ServiceUtil.returnSuccess();
            Map<String, List<Map<String, Object>>> catLevel = FastMap.newInstance();
            Debug.logInfo("productCategoryId: " + productCategoryId, module);

            // Add toplevel categories
            String[] trailElements = productCategoryId.split("/");

            long numFound = 0;
            
            boolean isFirstElement = true;
            
            // iterate over actual results
            for (String element : trailElements) {
                Debug.logInfo("element: " + element, module);
                List<Map<String, Object>> categories = FastList.newInstance();
                int level;
                // 2016-03-22: Don't make a query for the first element, which is the count,
                // but for compatibility, still make a map entry for it
                // NOTE: I think this could be skipped entirely because level 0 is replaced/taken by the
                // first category, but leaving in to play it safe
                if (isFirstElement) {
                    level = 0;
                    isFirstElement = false;
                }
                else {
                    String categoryPath = CategoryUtil.getCategoryNameWithTrail(element, catalogId, dctx, currentTrail);
                    String[] categoryPathArray = categoryPath.split("/");
                    level = Integer.parseInt(categoryPathArray[0]);
                    String facetPrefix = CategoryUtil.getFacetFilterForCategory(categoryPath, dctx);
                    // 2016-03-22: IMPORTANT: the facetPrefix MUST end with / otherwise it will return unrelated categories!
                    // solr facetPrefix is not aware of our path delimiters
                    if (!facetPrefix.endsWith("/")) {
                        facetPrefix += "/";
                    }
                    // Debug.logInfo("categoryPath: "+categoryPath + "
                    // facetPrefix: "+facetPrefix,module);
                    Map<String, Object> query = SolrUtil.categoriesAvailable(catalogId, categoryPath, null, facetPrefix, false, 0, 0);
                    QueryResponse cat = (QueryResponse) query.get("rows");
                    Long subNumFound = (Long) query.get("numFound");
                    if (subNumFound != null) {
                        numFound += subNumFound;
                    }
                    List<FacetField> catList = (List<FacetField>) cat.getFacetFields();
                    for (Iterator<FacetField> catIterator = catList.iterator(); catIterator.hasNext();) {
                        FacetField field = (FacetField) catIterator.next();
                        List<Count> catL = (List<Count>) field.getValues();
                        if (catL != null) {
                            for (Iterator<Count> catIter = catL.iterator(); catIter.hasNext();) {
                                FacetField.Count f = (FacetField.Count) catIter.next();
                                if (f.getCount() > 0) {
                                    Map<String, Object> catMap = FastMap.newInstance();
                                    FastList<String> iName = FastList.newInstance();
                                    iName.addAll(Arrays.asList(f.getName().split("/")));
                                    // Debug.logInfo("topLevel "+topLevel,"");
                                    // int l = Integer.parseInt((String)
                                    // iName.getFirst());
                                    catMap.put("catId", iName.getLast());
                                    iName.removeFirst();
                                    String path = f.getName();
                                    catMap.put("path", path);
                                    if (level > 0) {
                                        iName.removeLast();
                                        catMap.put("parentCategory", StringUtils.join(iName, "/"));
                                    } else {
                                        catMap.put("parentCategory", null);
                                    }
                                    catMap.put("count", Long.toString(f.getCount()));
                                    categories.add(catMap);
                                }
                            }
                        }
                    }
                }
                catLevel.put("menu-" + level, categories);
            }
            result.put("categories", catLevel);
            result.put("numFound", numFound);

        } catch (Exception e) {
            result = ServiceUtil.returnError(e.toString());
            result.put("numFound", (long) 0);
        }
        return result;
    }

    /**
     * Rebuilds the solr index.
     */
    public static Map<String, Object> rebuildSolrIndex(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        HttpSolrClient client = null;
        Map<String, Object> result;
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        //GenericValue userLogin = (GenericValue) context.get("userLogin");
        //Locale locale = new Locale("de_DE");

        // 2016-03-29: Only if dirty (or unknown)
        Boolean onlyIfDirty = (Boolean) context.get("onlyIfDirty");
        if (Boolean.TRUE.equals(onlyIfDirty)) {
            String dataStatusId = SolrUtil.getSolrDataStatusId(delegator);
            if ("SOLR_DATA_OK".equals(dataStatusId)) {
                result = ServiceUtil.returnSuccess("SOLR data is already marked OK; not rebuilding");
                result.put("numDocs", (int) 0);
                result.put("executed", Boolean.FALSE);
                return result;
            }
        }
        
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");

        int numDocs = 0;
        try {
            if (UtilValidate.isNotEmpty(context.get("core")))
                client = new HttpSolrClient(SolrUtil.solrUrl + "/" + context.get("core"));
            else
                client = new HttpSolrClient(SolrUtil.solrFullUrl);
            // now lets fetch all products
            List<Map<String, Object>> solrDocs = FastList.newInstance();
            List<GenericValue> products = delegator.findList("Product", null, null, null, null, true);
            if (products != null) {
                numDocs = products.size();
            }

            Debug.logInfo("Solr: Clearing solr index and rebuilding with " + numDocs + " found products", module);

            Iterator<GenericValue> productIterator = products.iterator();
            while (productIterator.hasNext()) {
                GenericValue product = productIterator.next();
                Map<String, Object> dispatchContext = ProductUtil.getProductContent(product, dctx, context);
                solrDocs.add(dispatchContext);
            }

            // this removes everything from the index
            client.deleteByQuery("*:*");
            client.commit();

            // This adds all products to the Index (instantly)
            Map<String, Object> servCtx = UtilMisc.toMap("fieldList", solrDocs, "treatConnectErrorNonFatal", treatConnectErrorNonFatal);
            copyStdServiceFieldsNotSet(context, servCtx);
            Map<String, Object> runResult = dispatcher.runSync("addListToSolrIndex", servCtx);

            String runMsg = ServiceUtil.getErrorMessage(runResult);
            if (UtilValidate.isEmpty(runMsg)) {
                runMsg = null;
            }
            if (ServiceUtil.isError(runResult)) {
                result = ServiceUtil.returnError(runMsg);
            } else if (ServiceUtil.isFailure(runResult)) {
                result = ServiceUtil.returnFailure(runMsg);
            } else {
                final String statusMsg = "Cleared solr index and reindexed " + numDocs + " documents";
                result = ServiceUtil.returnSuccess(statusMsg);
            }
        } catch (MalformedURLException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to rebuild index; index not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
            } else {
                Debug.logError(e, e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
            }
        } catch (IOException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        } catch (ServiceAuthException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        } catch (ServiceValidationException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        } catch (GenericServiceException e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        
        // If success, mark data as good
        if (ServiceUtil.isSuccess(result)) {
            SolrUtil.setSolrDataStatusId(delegator, "SOLR_DATA_OK");
        }
        result.put("numDocs", numDocs);
        result.put("executed", Boolean.TRUE);
        
        return result;
    }
    
    /**
     * Rebuilds the solr index - auto run.
     */
    public static Map<String, Object> rebuildSolrIndexAuto(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();

        boolean autoRunEnabled = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.enabled", false);
        
        if (autoRunEnabled) {
            Boolean onlyIfDirty = (Boolean) context.get("onlyIfDirty");
            if (onlyIfDirty == null) {
                onlyIfDirty = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.onlyIfDirty", false);
            }
            
            Debug.logInfo("Solr: auto-run index rebuild: starting (onlyIfDirty: " + onlyIfDirty + ")", module);

            Map<String, Object> servCtx;
            try {
                servCtx = dctx.makeValidContext("rebuildSolrIndex", ModelService.IN_PARAM, context);
                
                servCtx.put("onlyIfDirty", onlyIfDirty);
                
                Map<String, Object> servResult = dispatcher.runSync("rebuildSolrIndex", servCtx);
                
                if (ServiceUtil.isSuccess(servResult)) {
                    String respMsg = (String) servResult.get(ModelService.SUCCESS_MESSAGE);
                    if (respMsg != null) {
                        Debug.logInfo("Solr: auto-run index rebuild: rebuildSolrIndex returned success: " + respMsg, module);
                    } else {
                        Debug.logInfo("Solr: auto-run index rebuild: rebuildSolrIndex returned success", module);
                    }
                } else {
                    Debug.logError("Solr: auto-run index rebuild: rebuildSolrIndex returned an error: " + 
                            ServiceUtil.getErrorMessage(servResult), module);
                }

                // Just pass it all back, hackish but should work
                result = FastMap.newInstance();
                result.putAll(servResult);
            } catch (GenericServiceException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.getMessage());
            }
            
        } else {
            Debug.logInfo("Solr: auto-run index rebuild: not running - disabled", module);
            result = ServiceUtil.returnSuccess();
        }

        return result;
    }    
    
    
    /**
     * Marks SOLR data as dirty.
     */
    public static Map<String, Object> setSolrDataStatus(DispatchContext dctx, Map<String, Object> context) throws GenericEntityException {
        Map<String, Object> result;
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        
        boolean success = SolrUtil.setSolrDataStatusId(delegator, (String) context.get("dataStatusId"));
        if (success) {
            result = ServiceUtil.returnSuccess();
        }
        else {
            result = ServiceUtil.returnError("Unable to set SOLR data status");
        }
        
        return result;
    }
    
    private static void copyStdServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx) {
        copyServiceFieldsNotSet(srcCtx, destCtx, "locale", "userLogin", "timeZone");
    }
    
    private static void copyServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx, String... fieldNames) {
        for(String fieldName : fieldNames) {
            if (!destCtx.containsKey(fieldName)) destCtx.put(fieldName, srcCtx.get(fieldName));
        }
    }
}
