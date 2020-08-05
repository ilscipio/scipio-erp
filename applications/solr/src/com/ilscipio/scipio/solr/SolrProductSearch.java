package com.ilscipio.scipio.solr;

import java.io.IOException;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.request.QueryRequest;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.FacetField.Count;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.SpellCheckResponse;
import org.apache.solr.client.solrj.response.SpellCheckResponse.Collation;
import org.apache.solr.client.solrj.response.SpellCheckResponse.Suggestion;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ProcessSignals;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.solr.util.DirectJsonRequest;

/**
 * SCIPIO: Main solr services implementations.
 */
public abstract class SolrProductSearch {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean rebuildClearAndUseCacheDefault = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName,
            "solr.index.rebuild.clearAndUseCache", false);
    private static final String defaultRegisterUpdateToSolrUpdateSrv = UtilProperties.getPropertyValue(SolrUtil.solrConfigName,
            "solr.service.registerUpdateToSolr.updateSrv", "updateToSolr");

    private static final String reindexStartupForceSysProp = "scipio.solr.index.rebuild.startup.force";
    private static final String reindexStartupForceSysPropLegacy = "scipio.solr.reindex.startup.force"; // old name
    private static final String reindexStartupForceConfigProp = "solr.index.rebuild.startup.force";
    private static final int maxLogIds = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName,
            "solr.log.max.id", 10);
    private static final int maxLogIdsSize = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName,
            "solr.log.max.id.size", 250);
    private static final boolean ecaAsync = "async".equals(UtilProperties.getPropertyValue(SolrUtil.solrConfigName,
            "solr.eca.service.mode", "async"));
    private static final ProcessSignals rebuildSolrIndexSignals = ProcessSignals.make("rebuildSolrIndex", true);
    private static final int defaultBufSize = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.record.buffer.size", 1000);

    /**
     * Manual update helper service and variants, superseded by {@link ProductIndexer} which feeds {@link #commitToSolr(DispatchContext, Map)}.
     */
    public static Map<String, Object> updateToSolr(DispatchContext dctx, Map<String, Object> context) {
        try { // Skip queue and process
            context = UtilMisc.put(new HashMap<>(context), "topics", UtilMisc.toHashSet("solr"));
            ProductIndexer indexer = ProductIndexer.getDefault();
            Collection<EntityIndexer.Entry> entries = indexer.extractEntries(dctx.getDelegator(), context, System.currentTimeMillis(), null);
            if (UtilValidate.isEmpty(entries)) {
                return ServiceUtil.returnError("Missing product");
            }
            IndexingStatus status = indexer.readDocsAndCommit(dctx, context, entries);
            return status.isSuccess() ? ServiceUtil.returnSuccess() : ServiceUtil.returnError("Error committing documents to solr");
        } catch(Exception e) {
            Debug.logError(e, "Solr: removeFromSolr: " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> addToSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolr(dctx, UtilMisc.put(new HashMap<>(context), "action", "add"));
    }

    public static Map<String, Object> removeFromSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolr(dctx, UtilMisc.put(new HashMap<>(context), "action", "remove"));
    }

    private static String makeLogIdList(Collection<String> idList) {
        return makeLogIdList(idList, idList.size());
    }

    private static String makeLogIdList(Collection<String> idList, int realSize) {
        StringBuilder sb = new StringBuilder();
        int i = 0;
        for(String id : idList) {
            sb.append(", ");
            sb.append(id);
            i++;
            if (i >= maxLogIds) {
                break;
            }
        }
        if (sb.length() > 0) {
            sb.delete(0, ", ".length());
        }
        if (realSize > maxLogIds) {
            sb.append("...");
        }
        return sb.toString();
    }

    public static Map<String, Object> commitToSolr(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        boolean manual = Boolean.TRUE.equals(context.get("manual"));
        boolean indexed = false;
        Boolean webappInitPassed;
        boolean skippedDueToWebappInit = false;
        if (manual || SolrUtil.isSolrEcaEnabled(dctx.getDelegator())) {
            webappInitPassed = SolrUtil.isSolrEcaWebappInitCheckPassed();
            if (webappInitPassed) {
                Collection<? extends EntityIndexer.DocEntry> docs = UtilGenerics.cast(context.get("docs"));
                SolrDocBuilder docBuilder = SolrDocBuilder.getInstance(dctx, context);
                HttpSolrClient client = (HttpSolrClient) context.get("client");
                if (UtilValidate.isNotEmpty(docs)) {
                    try {
                        Map<String, Object> addResult = commitAddToSolr(docBuilder, client,
                                docs, (Boolean) context.get("treatConnectErrorNonFatal"), docs.size() + "");
                        if (ServiceUtil.isError(addResult)) {
                            return addResult;
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Solr: commit: Error committing " + docs.size() + " documents to solr: " + e.getMessage(), module);
                        return ServiceUtil.returnError("Error committing " + docs.size() + " documents to solr: " + e.toString());
                    }
                }
                Collection<? extends EntityIndexer.Entry> docsToRemove = UtilGenerics.cast(context.get("docsToRemove"));
                if (UtilValidate.isNotEmpty(docsToRemove)) {
                    try {
                        Map<String, Object> addResult = commitRemoveFromSolr(dctx, context, docBuilder, docsToRemove, client);
                        if (ServiceUtil.isError(addResult)) {
                            return addResult;
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Solr: commit: Error removing " + docsToRemove.size() + " documents from solr: " + e.getMessage(), module);
                        return ServiceUtil.returnError("Error committing " + docsToRemove.size() + " documents from solr: " + e.toString());
                    }
                }
                indexed = true;
                result = ServiceUtil.returnSuccess();
            } else {
                if (SolrUtil.verboseOn()) {
                    Debug.logInfo("Solr: commit: Solr webapp not available; skipping indexing for product", module);
                }
                result = ServiceUtil.returnSuccess();
                skippedDueToWebappInit = true;
            }
        } else {
            if (SolrUtil.verboseOn()) {
                Debug.logInfo("Solr: commit: Solr ECA indexing disabled; skipping indexing for product", module);
            }
            result = ServiceUtil.returnSuccess();
        }
        /* TODO: REVIEW: for now this causes more complications than it solves
        if (!manual && !indexed && UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.enabled", false)) {
            boolean markDirtyNoWebappCheck = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.noWebappCheck", false);
            if (!(markDirtyNoWebappCheck && skippedDueToWebappInit)) {
                if (SolrUtil.verboseOn()) {
                    Debug.logInfo("Solr: commit: Did not update index for product; marking SOLR data as dirty (old)", module);
                }
                // 2019-10-31: Performance fix/hack: if it's already marked old in the entity cache, do not bother to update it; this is important for operations
                // that affect many products. This could theoretically cause issue in load-balanced setups, but this is best-effort anyhow.
                if (!"SOLR_DATA_OLD".equals(SolrUtil.getSolrDataStatusId(dctx.getDelegator(), true))) {
                    SolrUtil.setSolrDataStatusIdSepTxSafe(dctx.getDelegator(), "SOLR_DATA_OLD", false);
                }
            }
        }
         */
        return result;
    }

    /**
     * Adds a List of products to the solr index. NOTE: 2020: This is generally now only needed internally.
     * <p>
     * This is faster than reflushing the index each time.
     */
    protected static Map<String, Object> commitAddToSolr(SolrDocBuilder docBuilder, HttpSolrClient client, Collection<?> docList, Boolean treatConnectErrorNonFatal, String progressMsg) {
        Map<String, Object> result;
        try {
            Collection<SolrInputDocument> docs = new ArrayList<>();
            if (docList.size() > 0) {
                if (Debug.infoOn()) {
                    List<String> idList = new ArrayList<>(maxLogIds);
                    for (Object doc : docList) {
                        if (idList.size() >= maxLogIds) {
                            break;
                        }
                        idList.add(docBuilder.getDocId(doc));
                    }
                    Debug.logInfo("Solr: commit: Generating and adding " + progressMsg + " documents to solr index: " +
                            makeLogIdList(idList, docList.size()), module);
                }
                // Construct Documents
                for (Object docObj : docList) {
                    docs.add(docBuilder.asSolrDoc(docObj));
                }
                // push Documents to server
                if (client == null) {
                    client = SolrUtil.getUpdateHttpSolrClient(docBuilder.getCore());
                }
                client.add(docs);
                client.commit();
            }
            String statusStr = "Added " + progressMsg + " documents to solr index";
            if (SolrUtil.verboseOn()) {
                Debug.logInfo("Solr: commit: " + statusStr, module);
            }
            result = ServiceUtil.returnSuccess(statusStr);
        } catch (MalformedURLException e) {
            Debug.logError(e, "Solr: commit: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "urlError");
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to commit product list; products not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: commit: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: commit: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
                result.put("errorType", "connectError");
            } else {
                Debug.logError(e, "Solr: commit: " + e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "solrServerError");
            }
        } catch (IOException e) {
            Debug.logError(e, "Solr: commit: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "ioError");
        } catch (Exception e) {
            Debug.logError(e, "Solr: commit: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "general");
        } finally {
            try {
                if (client != null) client.close();
            } catch (IOException e) {
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "ioError");
            }
        }
        return result;
    }

    /**
     * Adds a List of products to the solr index. NOTE: 2020: This is generally now only needed internally.
     * <p>
     * This is faster than reflushing the index each time.
     */
    public static Map<String, Object> addListToSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        List<?> docList = UtilGenerics.cast(context.get("docList"));
        return commitAddToSolr(SolrDocBuilder.getInstance(dctx, context), (HttpSolrClient) context.get("client"),
                docList, (Boolean) context.get("treatConnectErrorNonFatal"), docList.size()+"");
    }

    protected static Map<String, Object> commitRemoveFromSolr(DispatchContext dctx, Map<String, Object> context,
                                                              SolrDocBuilder indexer, Collection<?> docsToRemove, HttpSolrClient client) {
        StringBuilder query = new StringBuilder();
        try {
            for(Object docId : docsToRemove) {
                query.append(' ');
                String id = indexer.getDocId(docId);
                if (UtilValidate.isEmpty(id)) {
                    throw new IllegalArgumentException("Invalid document reference or value, could not extract ID: " + docId);
                }
                query.append(SolrExprUtil.escapeTermFull(id));
            }
            if (Debug.infoOn()) {
                Debug.logInfo("Solr: commit: Removing " + docsToRemove.size() + " products from index: " +
                        (query.length() > maxLogIdsSize ? query.substring(0, maxLogIdsSize) : query), module);
            }
            client.deleteByQuery("id:(" + query + ")");
            client.commit();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("numRemoved", docsToRemove.size());
            result.put("numFailures", 0);
            return result;
        } catch (Exception e) {
            Debug.logError(e, "Solr: commit: Error removing " + docsToRemove.size() + " products (" +
                    (query.length() > maxLogIdsSize ? query.substring(0, maxLogIdsSize) : query) + ") from solr index: " + e.getMessage(), module);
            return ServiceUtil.returnError("Error removing " + docsToRemove.size() + " products (" +
                    (query.length() > maxLogIdsSize ? query.substring(0, maxLogIdsSize) : query) + ") from solr index: " + e.toString());
        }
    }

    /**
     * Adds product to solr index. NOTE: 2020: This is generally now only needed internally.
     */
    @Deprecated
    public static Map<String, Object> addToSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        HttpSolrClient client = null;
        Map<String, Object> result;
        String productId = (String) context.get("productId");
        // connectErrorNonFatal is a necessary option because in some cases it
        // may be considered normal that solr server is unavailable;
        // don't want to return error and abort transactions in these cases.
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        //boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        try {
            SolrDocBuilder docBuilder = SolrDocBuilder.getInstance(dctx, context);
            Debug.logInfo("Solr: Indexing product '" + productId + "'", module);
            client = SolrUtil.getUpdateHttpSolrClient(docBuilder.getCore());

            // Construct Documents
            SolrInputDocument doc1 = docBuilder.makeSolrDoc(context);
            Collection<SolrInputDocument> docs = new ArrayList<>();

            if (SolrUtil.verboseOn()) Debug.logInfo("Solr: Indexing document: " + doc1.toString(), module);
            docs.add(doc1);

            // push Documents to server
            client.add(docs);
            client.commit();

            final String statusStr = "Product '" + productId + "' indexed";
            if (SolrUtil.verboseOn()) Debug.logInfo("Solr: " + statusStr, module);
            result = ServiceUtil.returnSuccess(statusStr);
        } catch (MalformedURLException e) {
            Debug.logError(e, "Solr: addToSolrIndex: " + e.getMessage(), module);
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
                Debug.logError(e, "Solr: addToSolrIndex: " + e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "solrServerError");
            }
        } catch (IOException e) {
            Debug.logError(e, "Solr: addToSolrIndex: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "ioError");
        } catch (Exception e) {
            Debug.logError(e, "Solr: addToSolrIndex: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "general");
        } finally {
            try {
                if (client != null) {
                    client.close();
                }
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
            // DEV NOTE: WARN: 2017-08-22: BEWARE PARSING FIELDS HERE - should be avoided here
            // the passed values may not be simple fields names, they require complex expressions containing spaces and special chars
            // (for example the old "queryFilter" parameter was unusable, so now have "queryFilters" list in addition).

            String solrUsername = (String) context.get("solrUsername");
            String solrPassword = (String) context.get("solrPassword");
            client = SolrUtil.getQueryHttpSolrClient((String) context.get("core"), solrUsername, solrPassword);

            // create Query Object
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery((String) context.get("query"));

            String queryType = (String) context.get("queryType");
            if (UtilValidate.isNotEmpty(queryType)) {
                solrQuery.setRequestHandler(queryType);
            }

            String defType = (String) context.get("defType");
            if (UtilValidate.isNotEmpty(defType)) {
                solrQuery.set("defType", defType);
            }

            // FIXME: "facet" is a leftover from ancient code and should be redone somehow...
            Boolean faceted = (Boolean) context.get("facet");
            if (Boolean.TRUE.equals(faceted)) {
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

            String facetQuery = (String) context.get("facetQuery");
            if (UtilValidate.isNotEmpty(facetQuery)) {
                solrQuery.setFacet(true);
                solrQuery.addFacetQuery(facetQuery);
            }

            Collection<String> facetQueryList = UtilGenerics.cast(context.get("facetQueryList"));
            if (UtilValidate.isNotEmpty(facetQueryList)) {
                solrQuery.setFacet(true);
                SolrQueryUtil.addFacetQueries(solrQuery, facetQueryList);
            }

            Collection<String> facetFieldList = UtilGenerics.cast(context.get("facetFieldList"));
            if (UtilValidate.isNotEmpty(facetFieldList)) {
                solrQuery.setFacet(true);
                SolrQueryUtil.addFacetFields(solrQuery, facetFieldList);
            }

            Integer facetMinCount = (Integer) context.get("facetMinCount");
            if (facetMinCount != null) {
                solrQuery.setFacet(true);
                solrQuery.setFacetMinCount(facetMinCount);
            }

            Integer facetLimit = (Integer) context.get("facetLimit");
            if (facetLimit != null) {
                solrQuery.setFacet(true);
                solrQuery.setFacetLimit(facetLimit);
            }

            Boolean spellCheck = (Boolean) context.get("spellcheck");
            if (Boolean.TRUE.equals(spellCheck)) {
                solrQuery.setParam("spellcheck", true);
                solrQuery.setParam("spellcheck.collate", true);

                Object spellDictObj = context.get("spellDict");
                if (spellDictObj instanceof String) {
                    if (UtilValidate.isNotEmpty((String) spellDictObj)) {
                        solrQuery.setParam("spellcheck.dictionary", (String) spellDictObj);
                    }
                } else if (spellDictObj instanceof Collection) {
                    for(String spellDict : UtilGenerics.<String>checkCollection(spellDictObj)) {
                        solrQuery.add("spellcheck.dictionary", spellDict);
                    }
                }
            }

            Boolean highlight = (Boolean) context.get("highlight");
            if (Boolean.TRUE.equals(highlight)) {
                // FIXME: unhardcode markup
                solrQuery.setHighlight(highlight);
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
            } else if (viewIndex != null) {
                if (viewIndex > 0 && viewSize != null && viewSize > 0) {
                    solrQuery.setStart(viewIndex * viewSize);
                }
            }

            String queryFilter = (String) context.get("queryFilter");
            if (UtilValidate.isNotEmpty((String) queryFilter)) {
                // WARN: 2017-08-17: we don't really want splitting on whitespace anymore, because it
                // slaughters complex queries and ignores escaping; callers should use queryFilters list instead.
                // However, we can at least fix a bug here where we can do better and split on \\s+ instead
                //solrQuery.addFilterQuery(((String) queryFilter).split(" "));
                solrQuery.addFilterQuery(((String) queryFilter).trim().split("\\s+"));
            }
            Collection<String> queryFilters = UtilGenerics.checkCollection(context.get("queryFilters"));
            SolrQueryUtil.addFilterQueries(solrQuery, queryFilters);

            String returnFields = (String) context.get("returnFields");
            if (returnFields != null) {
                solrQuery.setFields(returnFields);
            }

            String defaultOp = (String) context.get("defaultOp");
            if (UtilValidate.isNotEmpty(defaultOp)) {
                solrQuery.set("q.op", defaultOp);
            }

            String queryFields = (String) context.get("queryFields");
            if (UtilValidate.isNotEmpty(queryFields)) {
                solrQuery.set("qf", queryFields);
            }

            Boolean lowercaseOperators = (Boolean) context.get("lowercaseOperators");
            if (lowercaseOperators != null) {
                solrQuery.set("lowercaseOperators", lowercaseOperators);
            }

            Map<String, ?> queryParams = UtilGenerics.checkMap(context.get("queryParams"));
            if (queryParams != null) {
                for(Map.Entry<String, ?> entry : queryParams.entrySet()) {
                    String name = entry.getKey();
                    Object value = entry.getValue();
                    if (value == null) {
                        // NOTE: this removes the param when null
                        solrQuery.set(name, (String) value);
                    } else if (value instanceof String) {
                        solrQuery.set(name, (String) value);
                    } else if (value instanceof Integer) {
                        solrQuery.set(name, (Integer) value);
                    } else if (value instanceof Long) {
                        solrQuery.set(name, ((Long) value).intValue());
                    } else if (value instanceof Boolean) {
                        solrQuery.set(name, (Boolean) value);
                    } else {
                        throw new IllegalArgumentException("queryParams entry '" + name
                                + "' value unsupported type (supported: String, Integer, Long, Boolean): " + value.getClass().getName());
                    }
                }
            }

            // if((Boolean)context.get("sortByReverse"))order.reverse();
            String sortBy = (String) context.get("sortBy");
            if (UtilValidate.isNotEmpty(sortBy)) {
                SolrQuery.ORDER order = null;
                Boolean sortByReverse = (Boolean) context.get("sortByReverse");
                if (sortByReverse != null) {
                    order = sortByReverse ? SolrQuery.ORDER.desc : SolrQuery.ORDER.asc;
                }

                // TODO?: REVIEW?: 2017-08-22: this parsing poses a problem and may interfere with queries.
                // I have restricted it to only remove the first "-" if it's preceeded by whitespace, but
                // there's no guarantee it still might not interfere with query too...
                //sortBy = sortBy.replaceFirst("-", "");

                // TODO: REVIEW: trim would probably be fine & simplify check, but I don't know for sure
                //sortBy = sortBy.trim();

                int dashIndex = sortBy.indexOf('-');
                if (dashIndex >= 0 && sortBy.substring(0, dashIndex).trim().isEmpty()) { // this checks if dash is first char or preceeded by space only
                    if (order == null) {
                        order = SolrQuery.ORDER.desc;
                    }
                    sortBy = sortBy.substring(dashIndex + 1);
                }

                if (order == null) {
                    order = SolrQuery.ORDER.asc;
                }
                solrQuery.setSort(sortBy, order);
            }

            List<String> sortByList = UtilGenerics.checkList(context.get("sortByList"));
            if (sortByList != null) {
                for(String sortByEntry : sortByList) {
                    SolrQuery.ORDER order;
                    if (sortByEntry.toLowerCase().endsWith(" desc")) {
                        order = SolrQuery.ORDER.desc;
                        sortByEntry = sortByEntry.substring(0, sortByEntry.length() - " desc".length());
                    } else if (sortByEntry.toLowerCase().endsWith(" asc")) {
                        sortByEntry = sortByEntry.substring(0, sortByEntry.length() - " asc".length());
                        order = SolrQuery.ORDER.asc;
                    } else if (sortByEntry.startsWith("-")) {
                        order = SolrQuery.ORDER.desc;
                        sortByEntry = sortByEntry.substring("-".length());
                    } else {
                        order = SolrQuery.ORDER.asc;
                    }
                    solrQuery.addSort(sortByEntry, order);
                }
            }

            if (SolrUtil.verboseOn()) {
                Debug.logInfo("Solr: runSolrQuery: Submitting query: " + solrQuery, module);
            }

            //QueryResponse rsp = client.query(solrQuery, METHOD.POST); // old way (can't configure the request)
            QueryRequest req = new QueryRequest(solrQuery, METHOD.POST);
            if (solrUsername != null) {
                // This will override the credentials stored in (Scipio)HttpSolrClient, if any
                req.setBasicAuthCredentials(solrUsername, solrPassword);
            }
            QueryResponse rsp = req.process(client);

            result = ServiceUtil.returnSuccess();
            result.put("queryResult", rsp);
        } catch (Exception e) {
            Debug.logError(e, "Solr: runSolrQuery: Error: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            if (SolrQueryUtil.isSolrQuerySyntaxError(e)) {
                result.put("errorType", "query-syntax");
            } else {
                result.put("errorType", "general");
            }
            // TODO? nestedErrorMessage: did not succeed extracting this reliably
        }
        return result;
    }

    /**
     * Performs solr products search.
     */
    public static Map<String, Object> solrProductsSearch(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();

        try {
            Map<String, Object> dispatchMap = dctx.makeValidContext("runSolrQuery", ModelService.IN_PARAM, context);

            if (UtilValidate.isNotEmpty(context.get("productCategoryId"))) {
                String productCategoryId = (String) context.get("productCategoryId");
                // causes erroneous results for similar-name categories
                //dispatchMap.put("query", "cat:*" + SolrUtil.escapeTermFull(productCategoryId) + "*");
                boolean includeSubCategories = !Boolean.FALSE.equals(context.get("includeSubCategories"));
                dispatchMap.put("query", SolrExprUtil.makeCategoryIdFieldQueryEscape("cat", productCategoryId, includeSubCategories));
            } else {
                return ServiceUtil.returnError("Missing productCategoryId"); // TODO: localize
            }
            Integer viewSize = (Integer) dispatchMap.get("viewSize");
            //Integer viewIndex = (Integer) dispatchMap.get("viewIndex");
            if (dispatchMap.get("facet") == null) dispatchMap.put("facet", false);
            if (dispatchMap.get("spellcheck") == null) dispatchMap.put("spellcheck", false); // 2017-09: default changed to false
            if (dispatchMap.get("highlight") == null) dispatchMap.put("highlight", false); // 2017-09: default changed to false

            List<String> queryFilters = getEnsureQueryFiltersModifiable(dispatchMap);
            SolrQueryUtil.addDefaultQueryFilters(queryFilters, context); // 2018-05-25

            Map<String, Object> searchResult = dispatcher.runSync("runSolrQuery", dispatchMap);
            if (!ServiceUtil.isSuccess(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnResultSysFields(searchResult));
            }
            QueryResponse queryResult = (QueryResponse) searchResult.get("queryResult");
            result = ServiceUtil.returnSuccess();

            Map<String, Integer> facetQuery = queryResult.getFacetQuery();
            Map<String, String> facetQueries = null;
            if (facetQuery != null) {
                facetQueries = new HashMap<>();
                for (String fq : facetQuery.keySet()) {
                    if (facetQuery.get(fq) > 0) {
                        facetQueries.put(fq, fq.replaceAll("^.*\\u005B(.*)\\u005D", "$1") + " (" + facetQuery.get(fq).intValue() + ")");
                    }
                }
            }

            List<FacetField> facets = queryResult.getFacetFields();
            Map<String, Map<String, Long>> facetFields = null;
            if (facets != null) {
                facetFields = new HashMap<>();
                for (FacetField facet : facets) {
                    Map<String, Long> facetEntry = new HashMap<>();
                    List<FacetField.Count> facetEntries = facet.getValues();
                    if (UtilValidate.isNotEmpty(facetEntries)) {
                        for (FacetField.Count fcount : facetEntries) {
                            facetEntry.put(fcount.getName(), fcount.getCount());
                        }
                        facetFields.put(facet.getName(), facetEntry);
                    }
                }
            }

            result.put("results", queryResult.getResults());
            result.put("facetFields", facetFields);
            result.put("facetQueries", facetQueries);
            result.put("listSize", queryResult.getResults().getNumFound());
            // 2016-04-01: Need to translate this
            //result.put("viewIndex", queryResult.getResults().getStart());
            result.put("start", queryResult.getResults().getStart());
            result.put("viewIndex", SolrQueryUtil.calcResultViewIndex(queryResult.getResults(), viewSize));
            result.put("viewSize", viewSize);
        } catch (Exception e) {
            Debug.logError(e, "Solr: productsSearch: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        return result;
    }

    private static Map<String, Object> copySolrQueryExtraOutParams(Map<String, Object> src, Map<String, Object> dest) {
        if (src.containsKey("errorType")) dest.put("errorType", src.get("errorType"));
        if (src.containsKey("nestedErrorMessage")) dest.put("nestedErrorMessage", src.get("nestedErrorMessage"));
        return dest;
    }

    private static List<String> getEnsureQueryFiltersModifiable(Map<String, Object> context) {
        List<String> queryFilters = UtilGenerics.checkList(context.get("queryFilters"));
        if (queryFilters != null) queryFilters = new ArrayList<>(queryFilters);
        else queryFilters = new ArrayList<>();
        context.put("queryFilters", queryFilters);
        return queryFilters;
    }

    /**
     * Performs keyword search.
     * <p>
     * The search form requires the result to be in a specific layout, so this
     * will generate the proper results.
     */
    public static Map<String, Object> solrKeywordSearch(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        LocalDispatcher dispatcher = dctx.getDispatcher();

        try {
            Map<String, Object> dispatchMap = dctx.makeValidContext("runSolrQuery", ModelService.IN_PARAM, context);

            if (UtilValidate.isEmpty((String) dispatchMap.get("query"))) {
                dispatchMap.put("dispatchMap", "*:*");
            }
            Integer viewSize = (Integer) dispatchMap.get("viewSize");
            //Integer viewIndex = (Integer) dispatchMap.get("viewIndex");
            if (dispatchMap.get("facet") == null) dispatchMap.put("facet", false); // 2017-09: default changed to false
            if (dispatchMap.get("spellcheck") == null) dispatchMap.put("spellcheck", false); // 2017-09: default changed to false
            if (dispatchMap.get("highlight") == null) dispatchMap.put("highlight", false); // 2017-09: default changed to false

            List<String> queryFilters = getEnsureQueryFiltersModifiable(dispatchMap);
            SolrQueryUtil.addDefaultQueryFilters(queryFilters, context); // 2018-05-25

            Map<String, Object> searchResult = dispatcher.runSync("runSolrQuery", dispatchMap);
            if (!ServiceUtil.isSuccess(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnResultSysFields(searchResult));
            }
            QueryResponse queryResult = (QueryResponse) searchResult.get("queryResult");

            Boolean isCorrectlySpelled = Boolean.TRUE.equals(dispatchMap.get("spellcheck")) ? Boolean.TRUE : null;
            Map<String, List<String>> tokenSuggestions = null;
            List<String> fullSuggestions = null;
            SpellCheckResponse spellResp = queryResult.getSpellCheckResponse();
            if (spellResp != null) {
                isCorrectlySpelled = spellResp.isCorrectlySpelled();
                if (spellResp.getSuggestions() != null) {
                    tokenSuggestions = new LinkedHashMap<>();
                    for(Suggestion suggestion : spellResp.getSuggestions()) {
                        tokenSuggestions.put(suggestion.getToken(), suggestion.getAlternatives());
                    }
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: Spelling: Token suggestions: " + tokenSuggestions, module);
                }
                // collations 2017-09-14, much more useful than the individual word suggestions
                if (spellResp.getCollatedResults() != null) {
                    fullSuggestions = new ArrayList<>();
                    for(Collation collation : spellResp.getCollatedResults()) {
                        fullSuggestions.add(collation.getCollationQueryString());
                    }
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: Spelling: Collations: " + fullSuggestions, module);
                }
            }

            result = ServiceUtil.returnSuccess();
            result.put("isCorrectlySpelled", isCorrectlySpelled);

            Map<String, Integer> facetQuery = queryResult.getFacetQuery();
            Map<String, String> facetQueries = null;
            if (facetQuery != null) {
                facetQueries = new HashMap<>();
                for (String fq : facetQuery.keySet()) {
                    if (facetQuery.get(fq) > 0) {
                        facetQueries.put(fq, fq.replaceAll("^.*\\u005B(.*)\\u005D", "$1") + " (" + facetQuery.get(fq).intValue() + ")");
                    }
                }
            }

            List<FacetField> facets = queryResult.getFacetFields();
            Map<String, Map<String, Long>> facetFields = null;
            if (facets != null) {
                facetFields = new HashMap<>();
                for (FacetField facet : facets) {
                    Map<String, Long> facetEntry = new HashMap<>();
                    List<FacetField.Count> facetEntries = facet.getValues();
                    if (UtilValidate.isNotEmpty(facetEntries)) {
                        for (FacetField.Count fcount : facetEntries) {
                            facetEntry.put(fcount.getName(), fcount.getCount());
                        }
                        facetFields.put(facet.getName(), facetEntry);
                    }
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
            result.put("viewIndex", SolrQueryUtil.calcResultViewIndex(queryResult.getResults(), viewSize));
            result.put("viewSize", viewSize);
            result.put("tokenSuggestions", tokenSuggestions);
            result.put("fullSuggestions", fullSuggestions);

        } catch (Exception e) {
            Debug.logError(e, "Solr: keywordSearch: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
        }
        return result;
    }

    /**
     * Returns a map of the categories currently available under the root
     * element.
     */
    public static Map<String, Object> solrAvailableCategories(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        try {
            boolean displayProducts = Boolean.TRUE.equals(context.get("displayProducts"));
            int viewIndex = 0;
            int viewSize = 9;
            if (displayProducts) {
                viewIndex = (Integer) context.get("viewIndex");
                viewSize = (Integer) context.get("viewSize");
            }
            String catalogId = (String) context.get("catalogId");
            if (catalogId != null && catalogId.isEmpty()) catalogId = null; // TODO: REVIEW: is this necessary?

            List<String> currentTrail = UtilGenerics.checkList(context.get("currentTrail"));
            String productCategoryId = SolrCategoryUtil.getCategoryNameWithTrail((String) context.get("productCategoryId"),
                    catalogId, dctx, currentTrail);
            String productId = (String) context.get("productId");
            if (Debug.verboseOn()) Debug.logVerbose("Solr: getAvailableCategories: productCategoryId: " + productCategoryId, module);
            Map<String, Object> query = getAvailableCategories(dctx, context, catalogId, productCategoryId, productId, null,
                    displayProducts, viewIndex, viewSize);
            if (ServiceUtil.isError(query)) {
                throw new Exception(ServiceUtil.getErrorMessage(query));
            }

            QueryResponse cat = (QueryResponse) query.get("rows");
            result = ServiceUtil.returnSuccess();
            result.put("numFound", (long) 0);
            Map<String, Object> categories = new HashMap<>();
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
            return result;
        }
        return result;
    }
    
    /**
     * SCIPIO (2019-05-02): Gets all available categories in order to be consumed in a menu-like format (like getSideDeepCategories)
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> solrAvailableCategoriesExtended(DispatchContext dctx, Map<String, Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> result;
        try {
//            Map<String, Map<String, Object>> catLevel = new HashMap<>();
            List<Map<String, List<Map<String, Object>>>> catLevel = UtilMisc.newList();

            String catalogId = (String) context.get("catalogId");

            Map<String, Object> categories = null;
            Map<String, Object> solrAvailableCategoriesCtx = ServiceUtil.setServiceFields(dispatcher, "solrAvailableCategories", context, (GenericValue) context.get("userLogin"), (TimeZone) context.get("timeZone"),
                    (Locale) context.get("locale"));
            result = dispatcher.runSync("solrAvailableCategories", solrAvailableCategoriesCtx);
            if (ServiceUtil.isSuccess(result)) {
                categories = (Map<String, Object>) result.get("categories");
            }
            if (UtilValidate.isEmpty(categories)) {
                return ServiceUtil.returnError("Categories not found.");
            }

            Map<String, Object> allCategoriesMap = UtilMisc.newInsertOrderMap();
            for (String currentTrail : categories.keySet()) {
                String[] trailElements = currentTrail.split("/");
                if (Debug.isOn(Debug.VERBOSE))
                    Debug.logVerbose("Solr: currentTrail: " + currentTrail, module);

                String[] elements = Arrays.copyOfRange(trailElements, 1, trailElements.length);
                Map<String, Object> categoriesExtended = getAllCategoriesFromMap(allCategoriesMap, elements);
                allCategoriesMap.putAll(categoriesExtended);
            }
            
            for (String catKey : allCategoriesMap.keySet()) {
//                Debug.log("building extended top category: " + catKey);                
                Map<String, List<Map<String, Object>>> categoryMap = UtilMisc.newInsertOrderMap();
                categoryMap.put("menu-0", prepareAndRunSorlCategoryQuery(dctx, context, catalogId, "0/" + catKey, "1/" + catKey + "/", 0));
                catLevel.add(buildCategoriesExtendedFromMap(categoryMap, (Map<String, Object>) allCategoriesMap.get(catKey), "/" + catKey,
                        dctx, context, catalogId));
            }

            result.put("categoriesMap", allCategoriesMap);
            result.put("categories", catLevel);
//                result.put("numFound", numFound);

        } catch (Exception e) {
            result = ServiceUtil.returnError(e.toString());
            result.put("numFound", (long) 0);
            Debug.logError(e, module);
            return result;
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, List<Map<String, Object>>> buildCategoriesExtendedFromMap(Map<String, List<Map<String, Object>>> categoriesExtended,
            Map<String, Object> multiLevelEntryCategoryMap, String currentTrail, DispatchContext dctx, Map<String, Object> context, String catalogId)
            throws Exception {
        for (String catKey : multiLevelEntryCategoryMap.keySet()) {
            String trail = currentTrail + "/" + catKey; 
            Map<String, Object> multiLevelEntryCategory = (Map<String, Object>) multiLevelEntryCategoryMap.get(catKey);
            if (UtilValidate.isNotEmpty(multiLevelEntryCategory)) {
                categoriesExtended.putAll(buildCategoriesExtendedFromMap(categoriesExtended, multiLevelEntryCategory, trail, dctx, context, catalogId));
            }
            String[] elements = trail.split("/");
            String categoryPath = SolrCategoryUtil.getCategoryNameWithTrail(catKey, catalogId, dctx, Arrays.asList(elements));
            int level = Integer.valueOf(categoryPath.split("/")[0]);
            String facetPrefix = SolrCategoryUtil.getFacetFilterForCategory(categoryPath, dctx);
            if (!facetPrefix.endsWith("/")) {
                facetPrefix += "/";
            }
            if (Debug.isOn(Debug.VERBOSE)) {
                Debug.logInfo("Solr: getSideDeepCategories: level: " + level + " iterating element: " + catKey + "   categoryPath: " + categoryPath
                        + " facetPrefix: " + facetPrefix, module);
            }
            List<Map<String, Object>> tempList = prepareAndRunSorlCategoryQuery(dctx, context, catalogId, categoryPath, facetPrefix, level);
            if (categoriesExtended.containsKey("menu-" + level)) {
                categoriesExtended.get("menu-" + level).addAll(tempList);
            } else {
                categoriesExtended.put("menu-" + level, tempList);
            }
        }
        return categoriesExtended;
    }

    private static List<Map<String, Object>> prepareAndRunSorlCategoryQuery(DispatchContext dctx, Map<String, Object> context, String catalogId,
            String categoryPath, String facetPrefix, int level) throws Exception {
        // TODO: Use this method in sideDeepCategories
        Map<String, Object> query = getAvailableCategories(dctx, context, catalogId, categoryPath, null, facetPrefix, false, 0, 0);
        if (ServiceUtil.isError(query)) {
            throw new Exception(ServiceUtil.getErrorMessage(query));
        }
        QueryResponse cat = (QueryResponse) query.get("rows");
        List<Map<String, Object>> result = UtilMisc.newList();
        List<FacetField> catList = cat.getFacetFields();
        for (Iterator<FacetField> catIterator = catList.iterator(); catIterator.hasNext();) {
            FacetField field = catIterator.next();
            List<Count> catL = field.getValues();
            if (catL != null) {
                for (Iterator<Count> catIter = catL.iterator(); catIter.hasNext();) {
                    FacetField.Count facet = catIter.next();
                    if (facet.getCount() > 0) {
                        Map<String, Object> catMap = new HashMap<>();
                        List<String> iName = new LinkedList<>();
                        iName.addAll(Arrays.asList(facet.getName().split("/")));
                        catMap.put("catId", iName.get(iName.size() - 1)); 
                        iName.remove(0); // remove first
                        String path = facet.getName();
                        catMap.put("path", path);
                        if (level > 0) {
                            iName.remove(iName.size() - 1); // remove last
                            catMap.put("parentCategory", StringUtils.join(iName, "/"));
                        } else {
                            catMap.put("parentCategory", null);
                        }
                        catMap.put("count", Long.toString(facet.getCount()));
                        result.add(catMap);
                    }
                }
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getAllCategoriesFromMap(Map<String, Object> currentMap, String[] categoryIds) {
        
        int catsSize = categoryIds.length;
        if (catsSize > 0) {
            int i = 0;
            for (String key : currentMap.keySet()) {
                if (i < catsSize && key.equals(categoryIds[i])) {
                    i++;
                    Map<String, Object> currentSubMap = (Map<String, Object>) currentMap.get(key);
                    if (i + 1 == catsSize) {                    
                        currentSubMap.put(categoryIds[i], UtilMisc.newInsertOrderMap());
                    } else {
                        getAllCategoriesFromMap(currentSubMap, Arrays.copyOfRange(categoryIds, i, categoryIds.length));
                    }
                    return currentMap; 
                }
            }
            return UtilMisc.toMap(categoryIds[i], UtilMisc.newInsertOrderMap());
        }
        return null;
    }

    /**
     * NOTE: This method is package-private for backward compat only and should not be made public; its interface is subject to change.
     * Client code should call the solrAvailableCategories or solrSideDeepCategory service instead.
     */
    static Map<String, Object> getAvailableCategories(DispatchContext dctx, Map<String, Object> context,
            String catalogId, String categoryId, String productId, String facetPrefix, boolean displayProducts, int viewIndex, int viewSize) {
        Map<String, Object> result;

        try {
            HttpSolrClient client = SolrUtil.getQueryHttpSolrClient((String) context.get("core"));
            SolrQuery solrQuery = new SolrQuery();

            String query;
            if (categoryId != null) {
                query = "+cat:"+ SolrExprUtil.escapeTermFull(categoryId);
            } else if (productId != null) {
                query = "+productId:" + SolrExprUtil.escapeTermFull(productId);
            } else {
                query = "*:*";
            }
            solrQuery.setQuery(query);

            if (catalogId != null) {
                solrQuery.addFilterQuery("+catalog:" + SolrExprUtil.escapeTermFull(catalogId));
            }

            SolrQueryUtil.addDefaultQueryFilters(solrQuery, context);
            SolrQueryUtil.addFilterQueries(solrQuery, UtilGenerics.<String>checkList(context.get("queryFilters")));

            if (displayProducts) {
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
            QueryResponse returnMap = client.query(solrQuery, METHOD.POST);
            result = ServiceUtil.returnSuccess();
            result.put("rows", returnMap);
            result.put("numFound", returnMap.getResults().getNumFound());
        } catch (Exception e) {
            Debug.logError(e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }

    /**
     * Return a map of the side deep categories.
     */
    public static Map<String, Object> solrSideDeepCategory(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        try {
            String catalogId = (String) context.get("catalogId");
            if (catalogId != null && catalogId.isEmpty()) catalogId = null; // TODO: REVIEW: is this necessary?
            List<String> currentTrail = UtilGenerics.checkList(context.get("currentTrail"));

            // 2016-03-22: FIXME?: I think we could call getCategoryNameWithTrail with showDepth=false,
            // instead of check in loop...
            String productCategoryId = SolrCategoryUtil.getCategoryNameWithTrail((String) context.get("productCategoryId"), catalogId, dctx, currentTrail);
            result = ServiceUtil.returnSuccess();
            Map<String, List<Map<String, Object>>> catLevel = new HashMap<>();
            if (Debug.verboseOn()) Debug.logVerbose("Solr: getSideDeepCategories: productCategoryId: " + productCategoryId, module);

            // Add toplevel categories
            String[] trailElements = productCategoryId.split("/");

            long numFound = 0;
            boolean isFirstElement = true;

            // iterate over actual results
            for (String element : trailElements) {
                if (Debug.verboseOn()) Debug.logVerbose("Solr: getSideDeepCategories: iterating element: " + element, module);
                List<Map<String, Object>> categories = new ArrayList<>();
                int level;
                // 2016-03-22: Don't make a query for the first element, which is the count,
                // but for compatibility, still make a map entry for it
                // NOTE: I think this could be skipped entirely because level 0 is replaced/taken by the
                // first category, but leaving in to play it safe
                if (isFirstElement) {
                    level = 0;
                    isFirstElement = false;
                } else {
                    String categoryPath = SolrCategoryUtil.getCategoryNameWithTrail(element, catalogId, dctx, currentTrail);
                    String[] categoryPathArray = categoryPath.split("/");
                    level = Integer.parseInt(categoryPathArray[0]);
                    String facetPrefix = SolrCategoryUtil.getFacetFilterForCategory(categoryPath, dctx);
                    // 2016-03-22: IMPORTANT: the facetPrefix MUST end with / otherwise it will return unrelated categories!
                    // solr facetPrefix is not aware of our path delimiters
                    if (!facetPrefix.endsWith("/")) {
                        facetPrefix += "/";
                    }
                    // Debug.logInfo("categoryPath: "+categoryPath + "
                    // facetPrefix: "+facetPrefix,module);
                    Map<String, Object> query = getAvailableCategories(dctx, context, catalogId, categoryPath, null, facetPrefix, false, 0, 0);
                    if (ServiceUtil.isError(query)) {
                        throw new Exception(ServiceUtil.getErrorMessage(query));
                    }

                    QueryResponse cat = (QueryResponse) query.get("rows");
                    Long subNumFound = (Long) query.get("numFound");
                    if (subNumFound != null) {
                        numFound += subNumFound;
                    }
                    List<FacetField> catList = cat.getFacetFields();
                    for (Iterator<FacetField> catIterator = catList.iterator(); catIterator.hasNext();) {
                        FacetField field = catIterator.next();
                        List<Count> catL = field.getValues();
                        if (catL != null) {
                            for (Iterator<Count> catIter = catL.iterator(); catIter.hasNext();) {
                                FacetField.Count facet = catIter.next();
                                if (facet.getCount() > 0) {
                                    Map<String, Object> catMap = new HashMap<>();
                                    List<String> iName = new LinkedList<>();
                                    iName.addAll(Arrays.asList(facet.getName().split("/")));
                                    // Debug.logInfo("topLevel "+topLevel,"");
                                    // int l = Integer.parseInt((String)
                                    // iName.getFirst());
                                    catMap.put("catId", iName.get(iName.size() - 1)); // get last
                                    iName.remove(0); // remove first
                                    String path = facet.getName();
                                    catMap.put("path", path);
                                    if (level > 0) {
                                        iName.remove(iName.size() - 1); // remove last
                                        catMap.put("parentCategory", StringUtils.join(iName, "/"));
                                    } else {
                                        catMap.put("parentCategory", null);
                                    }
                                    catMap.put("count", Long.toString(facet.getCount()));
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
            return result;
        }
        return result;
    }

    /**
     * Rebuilds the solr index.
     */
    public static Map<String, Object> rebuildSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        try {
            rebuildSolrIndexSignals.clear();
            return rebuildSolrIndexImpl(dctx, context, rebuildSolrIndexSignals);
        } finally {
            rebuildSolrIndexSignals.clear();
        }
    }

    public static Map<String, Object> abortRebuildSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        rebuildSolrIndexSignals.put("stop");
        return ServiceUtil.returnSuccess();
    }

    private static Map<String, Object> rebuildSolrIndexImpl(DispatchContext dctx, Map<String, Object> context, ProcessSignals processSignals) {
        HttpSolrClient client;
        Map<String, Object> result = null;
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        //GenericValue userLogin = (GenericValue) context.get("userLogin");
        //Locale locale = new Locale("de_DE");

        // 2016-03-29: Only if dirty (or unknown)
        Boolean onlyIfDirty = (Boolean) context.get("onlyIfDirty");
        if (onlyIfDirty == null) onlyIfDirty = false;
        // 2017-08-23: Only if effective Solr config version changed
        Boolean ifConfigChange = (Boolean) context.get("ifConfigChange");
        if (ifConfigChange == null) ifConfigChange = false;
        if (onlyIfDirty || ifConfigChange) {
            GenericValue solrStatus = SolrUtil.getSolrStatus(delegator);
            String cfgVersion = SolrUtil.getSolrConfigVersionStatic();
            String dataStatusId = solrStatus != null ? solrStatus.getString("dataStatusId") : null;
            String dataCfgVersion = solrStatus != null ? solrStatus.getString("dataCfgVersion") : null;

            boolean dataStatusOk = "SOLR_DATA_OK".equals(dataStatusId);
            boolean dataCfgVerOk = cfgVersion.equals(dataCfgVersion);

            // TODO: simplify this code structure (one more bool and it will be unmaintainable)
            if (onlyIfDirty && ifConfigChange) {
                if (dataStatusOk && dataCfgVerOk) {
                    result = ServiceUtil.returnSuccess("SOLR data is already marked OK; SOLR data is already at config version " + cfgVersion + "; not rebuilding");
                    result.put("numDocs", 0);
                    result.put("executed", false);
                    return result;
                }
            } else if (onlyIfDirty) {
                if (dataStatusOk) {
                    result = ServiceUtil.returnSuccess("SOLR data is already marked OK; not rebuilding");
                    result.put("numDocs", 0);
                    result.put("executed", false);
                    return result;
                }
            } else if (ifConfigChange) {
                if (dataCfgVerOk) {
                    result = ServiceUtil.returnSuccess("SOLR data is already at config version " + cfgVersion + "; not rebuilding");
                    result.put("numDocs", 0);
                    result.put("executed", false);
                    return result;
                }
            }

            if (onlyIfDirty && !dataStatusOk) {
                Debug.logInfo("Solr: rebuildSolrIndex: [onlyIfDirty] Data is marked dirty (status: " + dataStatusId + "); reindexing proceeding...", module);
            }
            if (ifConfigChange && !dataCfgVerOk) {
                Debug.logInfo("Solr: rebuildSolrIndex: [ifConfigChange] Data config version has changed (current: " + cfgVersion + ", previous: " + dataCfgVersion + "); reindexing proceeding...", module);
            }
        }

        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        Boolean clearAndUseCache = (Boolean) context.get("clearAndUseCache");
        if (clearAndUseCache == null) clearAndUseCache = rebuildClearAndUseCacheDefault;

        int numDocs = 0;
        EntityListIterator prodIt = null;
        boolean executed = false;
        try {
            client = SolrUtil.getUpdateHttpSolrClient((String) context.get("core"));
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            // 2018-02-20: new ability to wait for Solr to load
            if (Boolean.TRUE.equals(context.get("waitSolrReady"))) {
                // NOTE: skipping runSync for speed; we know the implementation...
                Map<String, Object> waitCtx = new HashMap<>();
                waitCtx.put("client", client);
                Map<String, Object> waitResult = waitSolrReady(dctx, waitCtx, processSignals); // params will be null
                if (processSignals != null && processSignals.isSet("stop")) {
                    return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
                }
                if (!ServiceUtil.isSuccess(waitResult)) {
                    throw new ScipioSolrException(ServiceUtil.getErrorMessage(waitResult)).setLightweight(true);
                }
            }

            executed = true;
            if ("delete-all-first".equals(context.get("deleteMode"))) {
                Debug.logInfo("Solr: rebuildSolrIndex: Clearing solr index (deleteMode: delete-all-first)", module);
                // this removes everything from the index
                client.deleteByQuery("*:*");
                client.commit();
            } else if ("no-delete".equals(context.get("deleteMode"))) {
                Debug.logInfo("Solr: rebuildSolrIndex: Not clearing solr index (deleteMode: no-delete) - deleted products will remain in index", module);
            }

            // NEW 2017-09-14: clear all entity caches at beginning, and then enable caching during
            // the product reading - this should significantly speed up the process
            // NOTE: we also clear it again at the end to avoid filling up entity cache with rarely-accessed records
            if (clearAndUseCache) {
                SolrProductUtil.clearProductEntityCaches(delegator, dispatcher);
            }

            Integer bufSize = (Integer) context.get("bufSize");
            if (bufSize == null) {
                bufSize = defaultBufSize;
            }

            // now lets fetch all products
            EntityFindOptions findOptions = new EntityFindOptions();
            //findOptions.setResultSetType(EntityFindOptions.TYPE_SCROLL_INSENSITIVE); // not needed anymore, only for getPartialList (done manual instead)
            prodIt = delegator.find("Product", null, null, null, null, findOptions);
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            Map<String, Object> productContext = new HashMap<>(context);
            productContext.put("useCache", clearAndUseCache);
            SolrDocBuilder docBuilder = SolrDocBuilder.getInstance(dctx, productContext);
            numDocs = prodIt.getResultsSizeAfterPartialList();
            IndexingStatus.Standard status = new IndexingStatus.Standard(dctx, IndexingHookHandler.HookType.REINDEX, docBuilder,
                    numDocs, bufSize, "Solr: rebuildSolrIndex: ");
            // NOTE: use ArrayList instead of LinkedList (EntityListIterator) in buffered mode because it will use less total memory
            List<Map<String, Object>> docs = (bufSize > 0) ? new ArrayList<>(Math.min(bufSize, status.getMaxDocs())) : new LinkedList<>();

            Collection<String> includeMainStoreIds = UtilMisc.nullIfEmpty(UtilGenerics.<Collection<String>>cast(context.get("includeMainStoreIds")));
            Collection<String> includeAnyStoreIds = UtilMisc.nullIfEmpty(UtilGenerics.<Collection<String>>cast(context.get("includeAnyStoreIds")));
            SolrDocBuilder.ProductFilter productFilter = docBuilder.makeStoreProductFilter(includeMainStoreIds, includeAnyStoreIds);

            List<? extends IndexingHookHandler> hookHandlers = IndexingHookHandler.Handlers.getHookHandlers(
                    IndexingHookHandler.Handlers.getHookHandlerFactories(IndexingHookHandler.HookType.REINDEX));
            for(IndexingHookHandler hookHandler : hookHandlers) {
                try {
                    hookHandler.begin(status);
                } catch (Exception e) {
                    status.registerHookFailure(null, e, hookHandler, "begin");
                }
            }
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            int docsConsumed = 0;
            boolean lastReached = false;
            while (!lastReached) {
                if (processSignals != null && processSignals.isSet("stop")) {
                    return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
                }
                status.updateStartEndIndex(docsConsumed);
                docs.clear();
                docsConsumed = 0;

                for(IndexingHookHandler hookHandler : hookHandlers) {
                    try {
                        hookHandler.beginBatch(status);
                    } catch (Exception e) {
                        status.registerHookFailure(null, e, hookHandler, "beginBatch");
                    }
                }

                Debug.logInfo("Solr: rebuildSolrIndex: Reading products " + status.getIndexProgressString() + " for indexing", module);

                int numLeft = bufSize;
                while ((bufSize <= 0 || numLeft > 0) && !lastReached) {
                    GenericValue product = prodIt.next();
                    if (product != null) {
                        docsConsumed++;
                        try {
                            Timestamp moment = UtilDateTime.nowTimestamp();
                            ProductIndexer.ProductDocEntry docEntry = docBuilder.asDocEntry(product, productFilter, moment);
                            docs.add(docEntry.getDoc());
                            status.increaseNumDocs(1);
                            numLeft--;

                            for(IndexingHookHandler hookHandler : hookHandlers) {
                                try {
                                    hookHandler.processDocAdd(status, docEntry);
                                } catch (Exception e) {
                                    status.registerHookFailure(null, e, hookHandler, "processDocAdd");
                                }
                            }
                        } catch (Exception e) {
                            //return ServiceUtil.returnError("Error reading product '" + productId + "': " + e.getMessage());
                            status.registerGeneralFailure("Error reading product '" + product.get("productId") + "'", e);
                        }
                    } else {
                        lastReached = true;
                    }
                }

                if (docsConsumed == 0) {
                    for(IndexingHookHandler hookHandler : hookHandlers) {
                        try {
                            hookHandler.endBatch(status);
                        } catch (Exception e) {
                            status.registerHookFailure(null, e, hookHandler, "endBatch");
                        }
                    }
                    break;
                } else if (docs.size() > 0) {
                    // Add all products to the index
                    Map<String, Object> runResult = commitAddToSolr(docBuilder, client, docs, treatConnectErrorNonFatal, status.getIndexProgressString());
                    if (!ServiceUtil.isSuccess(runResult)) {
                        result = ServiceUtil.returnResultSysFields(runResult);
                        break;
                    }
                }
                for(IndexingHookHandler hookHandler : hookHandlers) {
                    try {
                        hookHandler.endBatch(status);
                    } catch (Exception e) {
                        status.registerHookFailure(null, e, hookHandler, "endBatch");
                    }
                }
            }

            for(IndexingHookHandler hookHandler : hookHandlers) {
                try {
                    hookHandler.end(status);
                } catch (Exception e) {
                    status.registerHookFailure(null, e, hookHandler, "end");
                }
            }

            if (result == null) {
                String cacheStats = docBuilder.getLogStatsShort();
                cacheStats = (cacheStats != null) ? " (caches: " + cacheStats + ")" : "";
                Debug.logInfo("Solr: rebuildSolrIndex: Finished with " + status.getNumDocs() + " documents indexed; failures: " +
                        status.getGeneralFailures() + "; hook failures: " + status.getHookFailures() + cacheStats, module);
                final String statusMsg = "Cleared solr index and reindexed " + status.getNumDocs() + " documents; failures: " +
                        status.getGeneralFailures() + "; hook failures: " + status.getHookFailures() + cacheStats;
                result = (status.getGeneralFailures() > 0) ? ServiceUtil.returnFailure(statusMsg) : ServiceUtil.returnSuccess(statusMsg);
            }
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to rebuild index; index not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: rebuildSolrIndex: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: rebuildSolrIndex: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
            } else {
                Debug.logError(e, "Solr: rebuildSolrIndex: Server error: " + e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
            }
        } catch (Exception e) {
            if (e instanceof ScipioSolrException && ((ScipioSolrException) e).isLightweight()) {
                // don't print the error itself, too verbose
                Debug.logError("Solr: rebuildSolrIndex: Error: " + e.getMessage(), module);
            } else {
                Debug.logError(e, "Solr: rebuildSolrIndex: Error: " + e.getMessage(), module);
            }
            result = ServiceUtil.returnError(e.toString());
        } finally {
            if (prodIt != null) {
                try {
                    prodIt.close();
                } catch(Exception e) {
                }
            }
            if (clearAndUseCache) {
                SolrProductUtil.clearProductEntityCaches(delegator, dispatcher);
            }
        }

        // If success, mark data as good
        if (result != null && ServiceUtil.isSuccess(result)) {
            // TODO?: REVIEW?: 2018-01-03: for this method, for now, unlike updateToSolr,
            // we will leave the status update in the same transaction as parent service,
            // because it is technically possible that there was an error in the parent transaction
            // prior to this service call that modified product data, or a transaction snafu
            // that causes a modification in the product data being indexed to be rolled back,
            // and in that case we don't want to mark the data as OK because another reindex
            // will be needed as soon as possible.
            // NOTE: in such case, we should even explicitly mark data as dirty, but I'm not
            // certain we can do that reliably from here.
            //SolrUtil.setSolrDataStatusIdSepTxSafe(delegator, "SOLR_DATA_OK", true);
            SolrUtil.setSolrDataStatusIdSafe(delegator, "SOLR_DATA_OK", true);
        }
        result.put("numDocs", numDocs);
        result.put("executed", executed);
        return result;
    }

    public static Map<String, Object> rebuildSolrIndexNoDelete(DispatchContext dctx, Map<String, Object> context) {
        try {
            Map<String, Object> servCtx = dctx.makeValidContext("rebuildSolrIndex", ModelService.IN_PARAM, context);
            return dctx.getDispatcher().runSync("rebuildSolrIndex", servCtx);
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.toString());
        }
    }

    public static Map<String, Object> rebuildSolrIndexIfDirty(DispatchContext dctx, Map<String, Object> context) {
        try {
            Map<String, Object> servCtx = dctx.makeValidContext("rebuildSolrIndex", ModelService.IN_PARAM, context);
            return dctx.getDispatcher().runSync("rebuildSolrIndex", servCtx);
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.toString());
        }
    }

    public static Map<String, Object> rebuildSolrIndexAuto(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();

        Map<String, Object> scipioJobCtx = UtilGenerics.checkMap(context.get("scipioJobCtx"));
        String eventId = (scipioJobCtx != null) ? (String) scipioJobCtx.get("eventId") : null;
        Boolean startupForce = null;
        if ("SCH_EVENT_STARTUP".equals(eventId)) {
            startupForce = getReindexStartupForceProperty(delegator, dispatcher);
            if (Debug.verboseOn()) {
                Debug.logInfo("rebuildSolrIndexAuto: Running as startup job (SCH_EVENT_STARTUP)", module);
            }
        }
        boolean force = Boolean.TRUE.equals(startupForce);
        boolean autoRunEnabled = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.enabled", false);

        if ((force || autoRunEnabled) && !Boolean.FALSE.equals(startupForce)) {
            Boolean onlyIfDirty = (Boolean) context.get("onlyIfDirty");
            if (onlyIfDirty == null) {
                onlyIfDirty = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.onlyIfDirty", false);
            }
            Boolean ifConfigChange = (Boolean) context.get("ifConfigChange");
            if (ifConfigChange == null) {
                ifConfigChange = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.ifConfigChange", false);
            }
            if (force) {
                onlyIfDirty = false;
                ifConfigChange = false;
            }

            Boolean waitSolrReady = (Boolean) context.get("waitSolrReady");
            if (waitSolrReady == null) {
                waitSolrReady = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.waitSolrReady", true);
            }

            Debug.logInfo("Solr: rebuildSolrIndexAuto: Launching index check/rebuild (onlyIfDirty: " + onlyIfDirty
                    + ", ifConfigChange: " + ifConfigChange + ", waitSolrReady: " + waitSolrReady + ", startupForce: " + startupForce
                    + ", eventId: " + eventId + ")", module);

            Map<String, Object> servCtx;
            try {
                servCtx = dctx.makeValidContext("rebuildSolrIndex", ModelService.IN_PARAM, context);

                servCtx.put("onlyIfDirty", onlyIfDirty);
                servCtx.put("ifConfigChange", ifConfigChange);
                servCtx.put("waitSolrReady", waitSolrReady);

                Map<String, Object> servResult = dispatcher.runSync("rebuildSolrIndex", servCtx);

                if (ServiceUtil.isSuccess(servResult)) {
                    String respMsg = (String) servResult.get(ModelService.SUCCESS_MESSAGE);
                    if (UtilValidate.isNotEmpty(respMsg)) {
                        Debug.logInfo("Solr: rebuildSolrIndexAuto: rebuildSolrIndex returned success: " + respMsg, module);
                    } else {
                        Debug.logInfo("Solr: rebuildSolrIndexAuto: rebuildSolrIndex returned success", module);
                    }
                } else {
                    Debug.logError("Solr: rebuildSolrIndexAuto: rebuildSolrIndex returned an error: " + ServiceUtil.getErrorMessage(servResult), module);
                }

                // Just pass it all back, hackish but should work
                result = new HashMap<>();
                result.putAll(servResult);
            } catch (Exception e) {
                Debug.logError(e, "Solr: rebuildSolrIndexAuto: Error: " + e.getMessage(), module);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            Debug.logInfo("Solr: rebuildSolrIndexAuto: not running - disabled (startupForce: " + startupForce + ", eventId: " + eventId + ")", module);
            result = ServiceUtil.returnSuccess();
        }
        return result;
    }

    private static Boolean getReindexStartupForceProperty(Delegator delegator, LocalDispatcher dispatcher) {
        Boolean force = UtilMisc.booleanValueVersatile(System.getProperty(reindexStartupForceSysProp));
        if (force != null) return force;
        force = UtilMisc.booleanValueVersatile(System.getProperty(reindexStartupForceSysPropLegacy));
        if (force != null) return force;
        String forceStr = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, reindexStartupForceConfigProp);
        if ("false".equals(forceStr)) {
            // COMPATIBILITY MODE: the value from .properties when false must be interpreted as 'unset' (property ignored); instead "N" may be passed
            return null;
        }
        return UtilMisc.booleanValueVersatile(forceStr);
    }

    public static Map<String, Object> setSolrDataStatus(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        try {
            SolrUtil.setSolrDataStatusId(dctx.getDelegator(), (String) context.get("dataStatusId"), false);
            result = ServiceUtil.returnSuccess();
        } catch (Exception e) {
            result = ServiceUtil.returnError("Unable to set SOLR data status: " + e.getMessage());
        }
        return result;
    }

    public static Map<String, Object> markSolrDataDirty(DispatchContext dctx, Map<String, Object> context) {
        context = new HashMap<>(context);
        context.put("dataStatusId", "SOLR_DATA_OLD");
        return setSolrDataStatus(dctx, context);
    }

    static void copyStdServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx) {
        copyServiceFieldsNotSet(srcCtx, destCtx, "locale", "userLogin", "timeZone");
    }

    static void copyServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx, String... fieldNames) {
        for(String fieldName : fieldNames) {
            if (!destCtx.containsKey(fieldName)) destCtx.put(fieldName, srcCtx.get(fieldName));
        }
    }

    public static Map<String, Object> checkSolrReady(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        boolean enabled = SolrUtil.isSystemInitialized(); // NOTE: this must NOT use SolrUtil.isSolrLocalWebappPresent() anymore
        result.put("enabled", enabled);
        if (enabled) {
            try {
                HttpSolrClient client = (HttpSolrClient) context.get("client");
                if (client == null) client = SolrUtil.getQueryHttpSolrClient((String) context.get("core"));
                result.put("ready", SolrUtil.isSolrWebappReady(client));
            } catch (Exception e) {
                Debug.logWarning(e, "Solr: checkSolrReady: error trying to check if Solr ready: " + e.getMessage(), module);
                result = ServiceUtil.returnFailure("Error while checking if Solr ready");
                result.put("enabled", enabled);
                result.put("ready", false);
                return result;
            }
        } else {
            result.put("ready", false);
        }
        return result;
    }

    public static Map<String, Object> waitSolrReady(DispatchContext dctx, Map<String, Object> context) {
        return waitSolrReady(dctx, context, null);
    }

    private static Map<String, Object> waitSolrReady(DispatchContext dctx, Map<String, Object> context, ProcessSignals processSignals) {
        if (!SolrUtil.isSolrEnabled()) { // NOTE: this must NOT use SolrUtil.isSolrLocalWebappPresent() anymore
            return ServiceUtil.returnFailure("Solr not enabled");
        }
        HttpSolrClient client;
        try {
            client = (HttpSolrClient) context.get("client");
            if (client == null) client = SolrUtil.getQueryHttpSolrClient((String) context.get("core"));

            if (SolrUtil.isSystemInitializedAssumeEnabled() && SolrUtil.isSolrWebappReady(client)) {
                if (Debug.verboseOn()) Debug.logInfo("Solr: waitSolrReady: Solr is ready, continuing", module);
                return ServiceUtil.returnSuccess();
            }
        } catch (Exception e) {
            Debug.logWarning(e, "Solr: waitSolrReady: error trying to check if Solr ready: " + e.getMessage(), module);
            return ServiceUtil.returnFailure("Error while checking if Solr ready");
        }

        Integer maxChecks = (Integer) context.get("maxChecks");
        if (maxChecks == null) maxChecks = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.service.waitSolrReady.maxChecks", null);
        if (maxChecks != null && maxChecks < 0) maxChecks = null;

        Integer sleepTime = (Integer) context.get("sleepTime");
        if (sleepTime == null) sleepTime = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.service.waitSolrReady.sleepTime", null);
        if (sleepTime == null || sleepTime < 0) sleepTime = 3000;

        int checkNum = 2; // first already done above
        while((maxChecks == null || checkNum <= maxChecks)) {
            Debug.logInfo("Solr: waitSolrReady: Solr not ready, waiting " + sleepTime + "ms (check " + checkNum + (maxChecks != null ? "/" + maxChecks : "") + ")", module);
            if (processSignals != null && processSignals.isSet("stop")) {
                return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted");
            }

            try {
                Thread.sleep(sleepTime);
            } catch (Exception e) {
                Debug.logWarning("Solr: waitSolrReady: interrupted while waiting for Solr: " + e.getMessage(), module);
                return ServiceUtil.returnFailure("Solr not ready, interrupted while waiting");
            }

            try {
                if (SolrUtil.isSystemInitializedAssumeEnabled() && SolrUtil.isSolrWebappReady(client)) {
                    Debug.logInfo("Solr: waitSolrReady: Solr is ready, continuing", module);
                    return ServiceUtil.returnSuccess();
                }
            } catch (Exception e) {
                Debug.logWarning(e, "Solr: waitSolrReady: error trying to check if Solr ready: " + e.getMessage(), module);
                return ServiceUtil.returnFailure("Error while checking if Solr ready");
            }
            checkNum++;
        }

        return ServiceUtil.returnFailure("Solr not ready, reached max wait time");
    }

    public static Map<String, Object> reloadSolrSecurityAuthorizations(DispatchContext dctx, Map<String, Object> context) {
        if (!SolrUtil.isSystemInitialized()) { // NOTE: this must NOT use SolrUtil.isSolrLocalWebappPresent() anymore
            return ServiceUtil.returnFailure("Solr not enabled or system not ready");
        }
        try {
            HttpSolrClient client = SolrUtil.getAdminHttpSolrClientFromUrl(SolrUtil.getSolrWebappUrl());
            //ModifiableSolrParams params = new ModifiableSolrParams();
            //// this is very sketchy, I don't think ModifiableSolrParams were meant for this
            //params.set("set-user-role", (String) null);
            //SolrRequest<?> request = new GenericSolrRequest(METHOD.POST, CommonParams.AUTHZ_PATH, params);
            SolrRequest<?> request = new DirectJsonRequest(CommonParams.AUTHZ_PATH,
                    "{\"set-user-role\":{}}"); // "{\"set-user-role\":{\"dummy\":\"dummy\"}}"
            client.request(request);
            Debug.logInfo("Solr: reloadSolrSecurityAuthorizations: invoked reload", module);
            return ServiceUtil.returnSuccess();
        } catch (Exception e) {
            Debug.logError("Solr: reloadSolrSecurityAuthorizations: error: " + e.getMessage(), module);
            return ServiceUtil.returnError("Error reloading Solr security authorizations: " + e.getMessage());
        }
    }

    public static Map<String, Object> setSolrSystemProperty(DispatchContext dctx, Map<String, Object> context) {
        Object value = context.get("value");
        boolean removeIfEmpty = Boolean.TRUE.equals(context.get("removeIfEmpty"));
        if (removeIfEmpty && (value == null || value.toString().isEmpty())) {
            return removeSolrSystemProperty(dctx, context);
        }
        String property = (String) context.get("property");
        try {
            GenericValue sysProp = EntityQuery.use(dctx.getDelegator()).from("SystemProperty")
                .where("systemResourceId", SolrUtil.solrConfigName, "systemPropertyId", property)
                .queryOne();
            if (sysProp == null) {
                sysProp = dctx.getDelegator().makeValue("SystemProperty");
                sysProp.put("systemResourceId", SolrUtil.solrConfigName);
                sysProp.put("systemPropertyId", property);
            }
            sysProp.put("systemPropertyValue", (value != null) ? value.toString() : null);
            dctx.getDelegator().createOrStore(sysProp);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: Error setting SystemProperty " + property, module);
            return ServiceUtil.returnError("Error setting SystemProperty " + property + ": " + e.getMessage());
        }
    }

    public static Map<String, Object> removeSolrSystemProperty(DispatchContext dctx, Map<String, Object> context) {
        String property = (String) context.get("property");
        try {
            GenericValue sysProp = EntityQuery.use(dctx.getDelegator()).from("SystemProperty")
                .where("systemResourceId", SolrUtil.solrConfigName, "systemPropertyId", property)
                .queryOne();
            if (sysProp != null) {
                sysProp.remove();
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: Error removing SystemProperty " + property, module);
            return ServiceUtil.returnError("Error removing SystemProperty " + property + ": " + e.getMessage());
        }
    }

    public static int getMaxLogIds() {
        return maxLogIds;
    }

    public static int getMaxLogIdsSize() {
        return maxLogIdsSize;
    }
}
