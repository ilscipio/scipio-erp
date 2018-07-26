package com.ilscipio.scipio.solr;

import java.io.IOException;
import java.net.ConnectException;
import java.net.MalformedURLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
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
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceSyncRegistrations;
import org.ofbiz.service.ServiceSyncRegistrations.ServiceSyncRegistration;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.solr.util.DirectJsonRequest;

/**
 * Base class for OFBiz Test Tools test case implementations.
 */
public abstract class SolrProductSearch {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean rebuildClearAndUseCacheDefault = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, 
            "solr.index.rebuild.clearAndUseCache", false);
    private static final String defaultRegisterUpdateToSolrUpdateSrv = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, 
            "solr.service.registerUpdateToSolr.updateSrv", "updateToSolr");

    private static boolean reindexAutoForceRan = false;
    private static final String reindexStartupForceSysProp = "scipio.solr.reindex.startup.force";
    private static final String reindexStartupForceConfigProp = "solr.index.rebuild.startup.force";

    public static Map<String, Object> addToSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolrCommon(dctx, context, Boolean.TRUE, true);
    }
    
    private static Map<String, Object> addToSolrCore(DispatchContext dctx, Map<String, Object> context, GenericValue product, String productId) {
        Map<String, Object> result;
        if (Debug.verboseOn()) Debug.logVerbose("Solr: addToSolr: Running indexing for productId '" + productId + "'", module);
        try {
            Map<String, Object> dispatchContext = SolrProductUtil.getProductContent(product, dctx, context);
            dispatchContext.put("treatConnectErrorNonFatal", SolrUtil.isEcaTreatConnectErrorNonFatal());
            copyStdServiceFieldsNotSet(context, dispatchContext);
            Map<String, Object> runResult = dctx.getDispatcher().runSync("addToSolrIndex", dispatchContext);
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
            }
        } catch (Exception e) {
            Debug.logError(e, "Solr: addToSolr: Error adding product '" + productId + "' to solr index: " + e.getMessage(), module);
            result = ServiceUtil.returnError("Error adding product '" + productId + "' to solr index: " + e.toString());
        }
        return result;
    }
    
    public static Map<String, Object> removeFromSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolrCommon(dctx, context, Boolean.FALSE, true);
    }
    
    private static Map<String, Object> removeFromSolrCore(DispatchContext dctx, Map<String, Object> context, String productId) {
        Map<String, Object> result;
        // NOTE: log this as info because it's the only log line
        Debug.logInfo("Solr: removeFromSolr: Removing productId '" + productId + "' from index", module);
        try {
            HttpSolrClient client = SolrUtil.getUpdateHttpSolrClient((String) context.get("core"));
            client.deleteByQuery("productId:" + SolrExprUtil.escapeTermFull(productId));
            client.commit();
            result = ServiceUtil.returnSuccess();
        } catch (Exception e) {
            Debug.logError(e, "Solr: removeFromSolr: Error removing product '" + productId + "' from solr index: " + e.getMessage(), module);
            result = ServiceUtil.returnError("Error removing product '" + productId + "' from solr index: " + e.toString());
        }
        return result;
    }
    
    public static final Map<String, Boolean> updateToSolrActionMap = UtilMisc.toMap("add", true, "remove", false); // default: "auto" -> null
    private static final String getUpdateToSolrAction(Boolean forceAdd) {
        if (forceAdd == null) return null;
        else return forceAdd ? "add" : "remove";
    }
    
    public static Map<String, Object> updateToSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolrCommon(dctx, context, updateToSolrActionMap.get(context.get("action")), true);
    }
    
    
    public static Map<String, Object> registerUpdateToSolr(DispatchContext dctx, Map<String, Object> context) {
        return updateToSolrCommon(dctx, context, updateToSolrActionMap.get(context.get("action")), false);
    }
    
    /**
     * Core implementation for the updateToSolr, addToSolr, removeFromSolr, and registerUpdateToSolr services.
     * <p>
     * Upon error, unless instructed otherwise (manual flag or config), this marks the Solr data as dirty.
     */
    private static Map<String, Object> updateToSolrCommon(DispatchContext dctx, Map<String, Object> context, Boolean forceAdd, boolean immediate) {
        Map<String, Object> result;

        // DEV NOTE: BOILERPLATE ECA ENABLE CHECKING PATTERN, KEEP SAME FOR SERVICES ABOVE/BELOW
        boolean manual = Boolean.TRUE.equals(context.get("manual"));
        boolean indexed = false;
        Boolean webappInitPassed = null;
        boolean skippedDueToWebappInit = false;
        
        if (manual || SolrUtil.isSolrEcaEnabled()) {
            webappInitPassed = SolrUtil.isSolrEcaWebappInitCheckPassed();
            if (webappInitPassed) {
                String productId;
                // NOTE: 2017-04-13: type may be org.ofbiz.entity.GenericValue or GenericPk, so use common parent GenericEntity (even better: Map)
                //GenericValue productInstance = (GenericValue) context.get("instance");
                @SuppressWarnings("unchecked")
                Map<String, Object> productInst = (Map<String, Object>) context.get("instance");
                if (productInst != null) productId = (String) productInst.get("productId");
                else productId = (String) context.get("productId");
                @SuppressWarnings("unchecked")
                Map<String, Map<String, Object>> productIds = (Map<String, Map<String, Object>>) context.get("productIds");
                
                if (immediate) {
                    if (productId == null && productIds == null) {
                        Debug.logError("Solr: updateToSolr: Missing product instance, productId or productIds map", module);
                        return ServiceUtil.returnError("Missing product instance, productId or productIds map");
                    }
                    result = updateToSolrCore(dctx, context, forceAdd, productId, productInst, productIds);
                    if (ServiceUtil.isSuccess(result)) indexed = true;
                } else {
                    if (TransactionUtil.isTransactionInPlaceSafe()) {
                        if (productId == null) {
                            Debug.logError("Solr: registerUpdateToSolr: Missing product instance or productId", module);
                            // DEV NOTE: This *should* be okay to return error, without interfering with running transaction,
                            // because default ECA flags are: rollback-on-error="false" abort-on-error="false"
                            return ServiceUtil.returnError("Missing product instance or productId");
                        }
                        result = registerUpdateToSolrForTxCore(dctx, context, forceAdd, productId, productInst);
                        if (ServiceUtil.isSuccess(result)) indexed = true; // NOTE: not really indexed; this is just to skip the mark-dirty below
                    } else {
                        final String reason = "No transaction in place";
                        if ("update".equals(context.get("noTransMode"))) {
                            if (productId == null && productIds == null) {
                                Debug.logError("Solr: updateToSolr: Missing product instance, productId or productIds map", module);
                                return ServiceUtil.returnError("Missing product instance, productId or productIds map");
                            }
                            Debug.logInfo("Solr: registerUpdateToSolr: " + reason + "; running immediate index update", module);
                            result = updateToSolrCore(dctx, context, forceAdd, productId, productInst, productIds);
                            if (ServiceUtil.isSuccess(result)) indexed = true;
                        } else { // "mark-dirty"
                            result = ServiceUtil.returnSuccess();
                            if (manual) {
                                Debug.logInfo("Solr: registerUpdateToSolr: " + reason + "; skipping solr indexing completely (manual mode; not marking dirty)", module);
                            } else {
                                Debug.logInfo("Solr: registerUpdateToSolr: " + reason + "; skipping solr indexing, will mark dirty", module);
                                indexed = false;
                            }
                        }
                    }
                }
            } else {
                if (Debug.verboseOn()) Debug.logVerbose("Solr: updateToSolr: Solr webapp not available; skipping indexing for product", module);
                result = ServiceUtil.returnSuccess();
                skippedDueToWebappInit = true;
            }
        } else {
            if (Debug.verboseOn()) Debug.logVerbose("Solr: updateToSolr: Solr ECA indexing disabled; skipping indexing for product", module);
            result = ServiceUtil.returnSuccess();
        }
        
        if (!manual && !indexed && UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.enabled", false)) {
            boolean markDirtyNoWebappCheck = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.markDirty.noWebappCheck", false);
            if (!(markDirtyNoWebappCheck && skippedDueToWebappInit)) {
                if (Debug.verboseOn()) Debug.logVerbose("Solr: updateToSolr: Did not update index for product; marking SOLR data as dirty (old)", module);
                SolrUtil.setSolrDataStatusIdSepTxSafe(dctx.getDelegator(), "SOLR_DATA_OLD", false);
            }
        }
        
        return result;
    }
    
    /**
     * Multi-product core update.
     * <p>
     * WARN: this edits the products map in-place. We're assuming this isn't an issue...
     * Added 2018-07-23.
     * <p>
     * DEV NOTE: 2018-07-26: For updateToSolr(Core), the global commit hook ignores the returned error message
     * from this, so logError statements are essential.
     */
    private static Map<String, Object> updateToSolrCore(DispatchContext dctx, Map<String, Object> context, Boolean forceAdd, String productId, Map<String, Object> productInst,
            Map<String, Map<String, Object>> products) {
        // WARN: this edits the products map in-place. We're assuming this isn't an issue...
        if (productId != null) {
            if (products == null) products = new HashMap<>(); // SPECIAL: here no need for LinkedHashMap, but other cases should be LinkedHashMap
            Map<String, Object> productProps;
            try {
                productProps = dctx.getModelService("updateToSolrSingleInterface").makeValid(context, ModelService.IN_PARAM, false, null);
            } catch (GenericServiceException e) {
                Debug.logError(e, "Solr: updateToSolr: Error parsing service parameters for updateToSolrSingleInterface: " + e.getMessage() , module);
                return ServiceUtil.returnError("Error parsing service parameters for updateToSolrSingleInterface: " + e.toString());
            }
            productProps.put("instance", productInst);
            productProps.put("action", getUpdateToSolrAction(forceAdd));
            products.put(productId, productProps);
        }
        return updateToSolrCore(dctx, context, products);
    }
    
    /**
     * Multi-product core update.
     * Added 2018-07-19.
     * <p>
     * DEV NOTE: 2018-07-26: For updateToSolr(Core), the global commit hook ignores the returned error message
     * from this, so logError statements are essential.
     */
    private static Map<String, Object> updateToSolrCore(DispatchContext dctx, Map<String, Object> context, Map<String, Map<String, Object>> products) {
        // SPECIAL: 2018-01-03: do not update to Solr if the current transaction marked as rollback,
        // because the rollbacked data may be (even likely to be) product data that we don't want in index
        // FIXME?: this cannot handle transaction rollbacks triggered after us! Some client diligence still needed for that...
        try {
            if (TransactionUtil.isTransactionInPlace() && TransactionUtil.getStatus() == TransactionUtil.STATUS_MARKED_ROLLBACK) {
                Debug.logWarning("Solr: updateToSolr: Current transaction is marked for rollback; aborting solr index update", module);
                return ServiceUtil.returnFailure("Current transaction is marked for rollback; aborting solr index update");
            }
        } catch (Exception e) {
            Debug.logError("Solr: updateToSolr: Failed to check transaction status; aborting solr index update: " + e.toString(), module);
            return ServiceUtil.returnError("Failed to check transaction status; aborting solr index update: " + e.toString());
        }

        // pre-process for variants
        // NOTE: products should be a LinkedHashMap;
        // expandedProducts doesn't need to be LinkedHashMap, but do it anyway
        // to keep more readable order of indexing operations in the log
        Map<String, Map<String, Object>> expandedProducts = new LinkedHashMap<>();
        
        for(Map.Entry<String, Map<String, Object>> entry : products.entrySet()) {
            String productId = entry.getKey();
            Map<String, Object> props = entry.getValue();
            Map<String, Object> productInst = UtilGenerics.checkMap(props.get("instance"));
            Boolean forceAdd = updateToSolrActionMap.get(props.get("action"));
            boolean updateVariantsDeep = Boolean.TRUE.equals(props.get("updateVariantsDeep"));
            boolean updateVariants = updateVariantsDeep || Boolean.TRUE.equals(props.get("updateVariants"));
            boolean updateVirtualDeep = Boolean.TRUE.equals(props.get("updateVirtualDeep"));
            boolean updateVirtual = updateVirtualDeep || Boolean.TRUE.equals(props.get("updateVirtual"));

            expandedProducts.put(productId, props);
            if (updateVariants || updateVirtual) {
                if (Boolean.FALSE.equals(forceAdd)) {
                    // Here the Product will already have been removed so we can't determine its
                    // variants; but that's ok because it makes no sense for virtual to have been
                    // removed without its variants removed, which should have come through an ECA
                    // either in the same transaction or another before it.
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("Solr: updateToSolr: Ignoring updateVariants/updateVirtual request for (forced) removal of product '" 
                                + productId + "' (the variants/virtual should be scheduled for removal through separate invocation)", module); 
                    }
                } else {
                    GenericValue product;
                    if (productInst instanceof GenericValue) {
                        product = (GenericValue) productInst;
                    } else {
                        try {
                            product = dctx.getDelegator().findOne("Product", UtilMisc.toMap("productId", productId), false);
                        } catch (Exception e) {
                            Debug.logError(e, "Solr: updateToSolr: Could not lookup product '" + productId + "': " + e.getMessage(), module);
                            return ServiceUtil.returnError("Could not lookup product '" + productId + "': " + e.toString());
                        }
                        if (Boolean.TRUE.equals(forceAdd) && product == null) {
                            return ServiceUtil.returnError("Product not found for productId: " + productId);
                        }
                        // update the props with the instance to prevent double lookup in updateToSolrCore below
                        // NOTE: SPECIAL MARKER for null
                        props = new HashMap<>(props);
                        props.put("instance", (product != null) ? product : Collections.emptyMap());
                        expandedProducts.put(productId, props);
                    }
                    if (product == null) {
                        if (Debug.verboseOn()) {
                            Debug.logVerbose("Solr: updateToSolr: Ignoring updateVariants/updateVirtual request for (forced) removal of product '" 
                                    + productId + "' (the variants/virtual should be scheduled for removal through separate invocation)", module); 
                        }
                    } else {
                        Timestamp moment = UtilDateTime.nowTimestamp();
                        if (updateVariants && Boolean.TRUE.equals(product.getBoolean("isVirtual"))) {
                            if (Debug.verboseOn()) {
                                Debug.logVerbose("Solr: updateToSolr: Product '" + productId + "' is virtual for add operation"
                                        + " and updateVariants true; looking up variants for update", module); 
                            }
                            List<GenericValue> variantProducts;
                            try {
                                if (updateVariantsDeep) {
                                    variantProducts = ProductWorker.getVariantProductsDeepDfs(dctx.getDelegator(), dctx.getDispatcher(), 
                                            product, null, moment, false);
                                } else {
                                    variantProducts = ProductWorker.getVariantProducts(dctx.getDelegator(), dctx.getDispatcher(), 
                                        product, null, moment, false);
                                }
                            } catch (GeneralException e) {
                                Debug.logError(e, "Solr: updateToSolr: Could not lookup product variants for '" 
                                        + productId + "' for updateVariants: " + e.getMessage(), module);
                                return ServiceUtil.returnError("Could not lookup product variants for '"
                                        + productId + "' for updateVariants: " + e.toString());
                            }
                            for(GenericValue variantProduct : variantProducts) {
                                // NOTE: we crush any prior entry for same product; 
                                // chronological last update request in transaction has priority
                                Map<String, Object> variantProps = new HashMap<>();
                                variantProps.put("instance", variantProduct);
                                variantProps.put("forceAdd", forceAdd); // no need: getUpdateToSolrAction(forceAdd)
                                String variantProductId = variantProduct.getString("productId");
                                // re-add the key to LinkedHashMap keep a readable order in log
                                expandedProducts.remove(variantProductId);
                                expandedProducts.put(variantProductId, variantProps);
                            }
                        }
                        
                        if (updateVirtual && Boolean.TRUE.equals(product.getBoolean("isVariant"))) {
                            if (Debug.verboseOn()) {
                                Debug.logVerbose("Solr: updateToSolr: Product '" + productId + "' is variant for add operation"
                                        + " and updateVirtual true; looking up virtual for update", module); 
                            }
                            
                            List<GenericValue> virtualProducts;
                            try {
                                // TODO: REVIEW: in most stock code these would be "-fromDate" and maxPerLevel=1,
                                // but I see no good reason to limit it here (needless ordering)...
                                final List<String> orderBy = null;
                                final Integer maxPerLevel = null;
                                if (updateVirtualDeep) {
                                    virtualProducts = ProductWorker.getVirtualProductsDeepDfs(dctx.getDelegator(), dctx.getDispatcher(), 
                                            product, orderBy, maxPerLevel, moment, false);
                                } else {
                                    virtualProducts = ProductWorker.getVirtualProducts(dctx.getDelegator(), dctx.getDispatcher(), 
                                            product, orderBy, maxPerLevel, moment, false);
                                }
                            } catch(Exception e) {
                                Debug.logError(e, "Solr: updateToSolr: Could not lookup virtual product for variant product '" 
                                        + productId + "': " + e.getMessage(), module);
                                return ServiceUtil.returnError("Could not lookup virtual product for variant product '" 
                                        + productId + "': " + e.toString());
                            }
                            // This can be a valid state for ALTERNATIVE_PACKAGE products
                            //if (virtualProducts.isEmpty()) {
                            //    Debug.logWarning("Solr: updateToSolr: Product '" 
                            //            + productId + "' is variant but found no parent product (either the association is being"
                            //            + " removed, or data error)", module);
                            //} else {
                            for(GenericValue virtualProduct : virtualProducts) {
                                Map<String, Object> variantProps = new HashMap<>();
                                variantProps.put("instance", virtualProduct);
                                variantProps.put("forceAdd", forceAdd); // no need: getUpdateToSolrAction(forceAdd)
                                String virtualProductId = virtualProduct.getString("productId");
                                // re-add the key to LinkedHashMap keep a readable order in log
                                expandedProducts.remove(virtualProductId);
                                expandedProducts.put(virtualProductId, variantProps);
                            }
                            //}
                        }
                    }
                }
            }
        }
        
        Map<String, String> productIndexErrorMsgs = new HashMap<>();
        
        for(Map.Entry<String, Map<String, Object>> entry : expandedProducts.entrySet()) {
            String productId = entry.getKey();
            Map<String, Object> props = entry.getValue();
            Map<String, Object> productInst = UtilGenerics.checkMap(props.get("instance"));
            Object actionObj = props.get("action");
            Boolean forceAdd = (actionObj instanceof Boolean) ? (Boolean) actionObj : updateToSolrActionMap.get(actionObj);
            
            Map<String, Object> updateSingleResult = updateToSolrCoreSingleImpl(dctx, context, forceAdd, productId, productInst);
            if (!ServiceUtil.isSuccess(updateSingleResult)) {
                productIndexErrorMsgs.put(productId, ServiceUtil.getErrorMessage(updateSingleResult));
            }
        }

        if (productIndexErrorMsgs.size() == 0) {
            return ServiceUtil.returnSuccess();
        } else {
            List<String> errorMsgs = new ArrayList<>();
            for(Map.Entry<String, String> entry : productIndexErrorMsgs.entrySet()) {
                errorMsgs.add("Error updating index for product '" + entry.getKey() + "': " + entry.getValue());
            }
            Map<String, Object> result = ServiceUtil.returnError(errorMsgs);
            // DEV NOTE: this log statement is _probably_ redundant, but do it just in case
            Debug.logError("Solr: registerUpdateToSolr: Error(s) indexing product(s): " + errorMsgs, module);
            return result;
        }
    }

    private static Map<String, Object> updateToSolrCoreSingleImpl(DispatchContext dctx, Map<String, Object> context, Boolean forceAdd,
            String productId, Map<String, Object> productInst) {
        Map<String, Object> result;
        if (Boolean.FALSE.equals(forceAdd)) {
            result = removeFromSolrCore(dctx, context, productId);
        } else {
            GenericValue product;
            if (productInst instanceof GenericValue) {
                product = (GenericValue) productInst;
            } else if (productInst != null && productInst.isEmpty()) {  // SPECIAL MARKER to prevent double-lookup when null
                product = null;
            } else { // SPECIAL MARKER to prevent re-lookup
                try {
                    product = dctx.getDelegator().findOne("Product", UtilMisc.toMap("productId", productId), false);
                } catch (Exception e) {
                    Debug.logError(e, "Solr: updateToSolr: Could not lookup product '" + productId + "': " + e.getMessage(), module);
                    return ServiceUtil.returnError("Could not lookup product '" + productId + "': " + e.toString());
                }
            }
            if (Boolean.TRUE.equals(forceAdd)) {
                if (product == null) {
                    Debug.logError("Solr: updateToSolr: Explicit add action requested, but product not found for productId: " + productId, module);
                    return ServiceUtil.returnError("Explicit add action requested, but product not found for productId: " + productId);
                }
                result = addToSolrCore(dctx, context, product, productId);
            } else {
                if (product != null) {
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: updateToSolr: productId '" + productId + "' found in system; running solr add", module); 
                    result = addToSolrCore(dctx, context, product, productId);
                } else {
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: updateToSolr: productId '" + productId + "' not found in system; running solr remove", module); 
                    result = removeFromSolrCore(dctx, context, productId);
                }
            }
        }
        return result;
    }
    
    /**
     * Registers an updateToSolr call at the end of the transaction using global-commit event.
     * <p>
     * NOTE: the checking loop below may not currently handle all possible cases imaginable, 
     * but should cover the ones we're using.
     * <p>
     * UPDATE: 2018-07-24: We now use a single update service call with a map of productIds,
     * to minimize overhead.
     * WARN: DEV NOTE: This now edits the update service productIds context map in-place
     * in the already-registered service; this is not "proper" but there is no known current case
     * where it should cause an issue and it removes more overhead.
     * <p>
     * DEV NOTE: This *should* be okay to return error, without interfering with running transaction,
     * because default ECA flags are: rollback-on-error="false" abort-on-error="false"
     */
    private static Map<String, Object> registerUpdateToSolrForTxCore(DispatchContext dctx, Map<String, Object> context, Boolean forceAdd, String productId, Map<String, Object> productInst) {
        String updateSrv = (String) context.get("updateSrv");
        if (updateSrv == null) updateSrv = defaultRegisterUpdateToSolrUpdateSrv;
        try {
            LocalDispatcher dispatcher = dctx.getDispatcher();
            ServiceSyncRegistrations regs = dispatcher.getServiceSyncRegistrations();

            Map<String, Object> productProps = dctx.getModelService("updateToSolrSingleInterface")
                    .makeValid(context, ModelService.IN_PARAM, false, null);
            // IMPORTANT: DO NOT PASS AN INSTANCE; use productId to force updateToSolr to re-query the Product
            // after transaction committed (by the time updateSrv is called, this instance may be invalid)
            productProps.remove("instance");
            productProps.put("action", getUpdateToSolrAction(forceAdd));
            
            Collection<ServiceSyncRegistration> updateSrvRegs = regs.getCommitRegistrationsForService(updateSrv);
            if (updateSrvRegs.size() >= 1) {
                if (updateSrvRegs.size() >= 2) {
                    Debug.logError("Solr: registerUpdateToSolr: Found more than one transaction commit registration"
                            + " for update service '" + updateSrv + "'; should not happen! (coding or ECA config error)", module);
                }
                ServiceSyncRegistration reg = updateSrvRegs.iterator().next();
                
                // WARN: editing existing registration's service context in-place
                @SuppressWarnings("unchecked")
                Map<String, Map<String, Object>> productIds = (Map<String, Map<String, Object>>) reg.getContext().get("productIds");
                productIds.remove(productId); // this is a LinkedHashMap, so remove existing first so we keep the "real" order
                productIds.put(productId, productProps);
                
                return ServiceUtil.returnSuccess("Updated transaction global-commit registration for " + updateSrv + " to include productId '" + productId + ")");
            } else {
                // register the service
                Map<String, Object> servCtx = new HashMap<>();
                // NOTE: this ditches the "manual" boolean (updateToSolrControlInterface), because can't support it here with only one updateSrv call
                servCtx.put("locale", context.get("locale"));
                servCtx.put("userLogin", context.get("userLogin"));
                servCtx.put("timeZone", context.get("timeZone"));
                
                // IMPORTANT: LinkedHashMap keeps order of changes across transaction
                Map<String, Map<String, Object>> productIds = new LinkedHashMap<>();
                productIds.put(productId, productProps);
                servCtx.put("productIds", productIds);

                regs.addCommitService(dctx, updateSrv, null, servCtx, false, false);
                return ServiceUtil.returnSuccess("Registered " + updateSrv + " to run at transaction global-commit for productId '" + productId + ")");
            }
        } catch (Exception e) {
            final String errMsg = "Could not register " + updateSrv + " to run at transaction global-commit for product '" + productId + "'";
            Debug.logError(e, "Solr: registerUpdateToSolr: " + errMsg, module);
            return ServiceUtil.returnError(errMsg + ": " + e.toString());
        }
    }

    /**
     * Adds product to solr index.
     */
    public static Map<String, Object> addToSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        HttpSolrClient client = null;
        Map<String, Object> result;
        String productId = (String) context.get("productId");
        // connectErrorNonFatal is a necessary option because in some cases it
        // may be considered normal that solr server is unavailable;
        // don't want to return error and abort transactions in these cases.
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        try {
            Debug.logInfo("Solr: Indexing product '" + productId + "'", module);

            client = SolrUtil.getUpdateHttpSolrClient((String) context.get("core"));

            // Construct Documents
            SolrInputDocument doc1 = SolrProductUtil.generateSolrProductDocument(dctx.getDelegator(), dctx.getDispatcher(), context, useCache);
            Collection<SolrInputDocument> docs = new ArrayList<>();

            if (Debug.verboseOn()) Debug.logVerbose("Solr: Indexing document: " + doc1.toString(), module);

            docs.add(doc1);

            // push Documents to server
            client.add(docs);
            client.commit();

            final String statusStr = "Product '" + productId + "' indexed";
            if (Debug.verboseOn()) Debug.logVerbose("Solr: " + statusStr, module);
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
    public static Map<String, Object> addListToSolrIndex(DispatchContext dctx, Map<String, Object> context) {
        HttpSolrClient client = null;
        Map<String, Object> result;
        Boolean treatConnectErrorNonFatal = (Boolean) context.get("treatConnectErrorNonFatal");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        try {
            Collection<SolrInputDocument> docs = new ArrayList<>();
            List<Map<String, Object>> fieldList = UtilGenerics.<Map<String, Object>> checkList(context.get("fieldList"));

            if (fieldList.size() > 0) {
                Debug.logInfo("Solr: addListToSolrIndex: Generating and adding " + fieldList.size() + " documents to solr index", module);
    
                // Construct Documents
                for (Map<String, Object> productContent : fieldList) {
                    SolrInputDocument doc1 = SolrProductUtil.generateSolrProductDocument(dctx.getDelegator(), dctx.getDispatcher(), productContent, useCache);
                    if (Debug.verboseOn()) Debug.logVerbose("Solr: addListToSolrIndex: Processed document for indexing: " + doc1.toString(), module);
                    docs.add(doc1);
                }
                // push Documents to server
                client = SolrUtil.getUpdateHttpSolrClient((String) context.get("core"));
                client.add(docs);
                client.commit();
            }

            final String statusStr = "Added " + fieldList.size() + " documents to solr index";
            Debug.logInfo("Solr: addListToSolrIndex: " + statusStr, module);
            result = ServiceUtil.returnSuccess(statusStr);
        } catch (MalformedURLException e) {
            Debug.logError(e, "Solr: addListToSolrIndex: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "urlError");
        } catch (SolrServerException e) {
            if (e.getCause() != null && e.getCause() instanceof ConnectException) {
                final String statusStr = "Failure connecting to solr server to commit product list; products not updated";
                if (Boolean.TRUE.equals(treatConnectErrorNonFatal)) {
                    Debug.logWarning(e, "Solr: addListToSolrIndex: " + statusStr, module);
                    result = ServiceUtil.returnFailure(statusStr);
                } else {
                    Debug.logError(e, "Solr: addListToSolrIndex: " + statusStr, module);
                    result = ServiceUtil.returnError(statusStr);
                }
                result.put("errorType", "connectError");
            } else {
                Debug.logError(e, "Solr: addListToSolrIndex: " + e.getMessage(), module);
                result = ServiceUtil.returnError(e.toString());
                result.put("errorType", "solrServerError");
            }
        } catch (IOException e) {
            Debug.logError(e, "Solr: addListToSolrIndex: " + e.getMessage(), module);
            result = ServiceUtil.returnError(e.toString());
            result.put("errorType", "ioError");
        } catch (Exception e) {
            Debug.logError(e, "Solr: addListToSolrIndex: " + e.getMessage(), module);
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
            
            if ((String) context.get("returnFields") != null) {
                solrQuery.setFields((String) context.get("returnFields"));
            }
            
            String defaultOp = (String) context.get("defaultOp");
            if (UtilValidate.isNotEmpty(defaultOp)) {
                solrQuery.set("q.op", defaultOp);
            }
            
            String queryFields = (String) context.get("queryFields");
            if (UtilValidate.isNotEmpty(queryFields)) {
                solrQuery.set("qf", queryFields);
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

            if ((String) context.get("facetQuery") != null) {
                solrQuery.addFacetQuery((String) context.get("facetQuery"));
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
    public static Map<String, Object> productsSearch(DispatchContext dctx, Map<String, Object> context) {
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
            dispatchMap.put("facet", false); // (always false)
            dispatchMap.put("spellcheck", false); // 2017-09: changed to false (always false)
            if (dispatchMap.get("highlight") == null) dispatchMap.put("highlight", false); // 2017-09: default changed to false
            
            List<String> queryFilters = getEnsureQueryFiltersModifiable(dispatchMap);
            SolrQueryUtil.addDefaultQueryFilters(queryFilters, context); // 2018-05-25
            
            Map<String, Object> searchResult = dispatcher.runSync("runSolrQuery", dispatchMap);
            if (ServiceUtil.isFailure(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnFailure(ServiceUtil.getErrorMessage(searchResult)));
            } else if (ServiceUtil.isError(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnError(ServiceUtil.getErrorMessage(searchResult)));
            }
            QueryResponse queryResult = (QueryResponse) searchResult.get("queryResult");
            result = ServiceUtil.returnSuccess();
            result.put("results", queryResult.getResults());
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
    public static Map<String, Object> keywordSearch(DispatchContext dctx, Map<String, Object> context) {
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
            if (ServiceUtil.isFailure(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnFailure(ServiceUtil.getErrorMessage(searchResult)));
            } else if (ServiceUtil.isError(searchResult)) {
                return copySolrQueryExtraOutParams(searchResult, ServiceUtil.returnError(ServiceUtil.getErrorMessage(searchResult)));
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
                    if (facetQuery.get(fq).intValue() > 0)
                        facetQueries.put(fq, fq.replaceAll("^.*\\u005B(.*)\\u005D", "$1") + " (" + facetQuery.get(fq).intValue() + ")");
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
                        for (FacetField.Count fcount : facetEntries)
                            facetEntry.put(fcount.getName(), fcount.getCount());
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
    public static Map<String, Object> getAvailableCategories(DispatchContext dctx, Map<String, Object> context) {
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
    public static Map<String, Object> getSideDeepCategories(DispatchContext dctx, Map<String, Object> context) {
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
                    List<FacetField> catList = (List<FacetField>) cat.getFacetFields();
                    for (Iterator<FacetField> catIterator = catList.iterator(); catIterator.hasNext();) {
                        FacetField field = (FacetField) catIterator.next();
                        List<Count> catL = (List<Count>) field.getValues();
                        if (catL != null) {
                            for (Iterator<Count> catIter = catL.iterator(); catIter.hasNext();) {
                                FacetField.Count facet = (FacetField.Count) catIter.next();
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
        HttpSolrClient client = null;
        Map<String, Object> result = null;
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
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
                    result.put("numDocs", (int) 0);
                    result.put("executed", Boolean.FALSE);
                    return result;
                }
            } else if (onlyIfDirty) {
                if (dataStatusOk) {
                    result = ServiceUtil.returnSuccess("SOLR data is already marked OK; not rebuilding");
                    result.put("numDocs", (int) 0);
                    result.put("executed", Boolean.FALSE);
                    return result;
                }
            } else if (ifConfigChange) {
                if (dataCfgVerOk) {
                    result = ServiceUtil.returnSuccess("SOLR data is already at config version " + cfgVersion + "; not rebuilding");
                    result.put("numDocs", (int) 0);
                    result.put("executed", Boolean.FALSE);
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
        int numDocsIndexed = 0; // 2018-02: needed for accurate stats in case a client edit filters out products within loop
        EntityListIterator prodIt = null;
        boolean executed = false;
        try {
            client = SolrUtil.getUpdateHttpSolrClient((String) context.get("core"));
            
            // 2018-02-20: new ability to wait for Solr to load
            if (Boolean.TRUE.equals(context.get("waitSolrReady"))) {
                // NOTE: skipping runSync for speed; we know the implementation...
                Map<String, Object> waitCtx = new HashMap<>();
                waitCtx.put("client", client);
                Map<String, Object> waitResult = waitSolrReady(dctx, waitCtx); // params will be null
                if (!ServiceUtil.isSuccess(waitResult)) {
                    throw new ScipioSolrException(ServiceUtil.getErrorMessage(waitResult)).setLightweight(true);
                }
            }
            
            executed = true;
            Debug.logInfo("Solr: rebuildSolrIndex: Clearing solr index", module);
            // this removes everything from the index
            client.deleteByQuery("*:*");
            client.commit();
            
            // NEW 2017-09-14: clear all entity caches at beginning, and then enable caching during 
            // the product reading - this should significantly speed up the process
            // NOTE: we also clear it again at the end to avoid filling up entity cache with rarely-accessed records
            if (clearAndUseCache) {
                SolrProductUtil.clearProductEntityCaches(delegator, dispatcher);
            }
            
            Integer bufSize = (Integer) context.get("bufSize");
            if (bufSize == null) {
                bufSize = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.record.buffer.size", 1000);
            }
            
            // now lets fetch all products
            EntityFindOptions findOptions = new EntityFindOptions();
            //findOptions.setResultSetType(EntityFindOptions.TYPE_SCROLL_INSENSITIVE); // not needed anymore, only for getPartialList (done manual instead)
            prodIt = delegator.find("Product", null, null, null, null, findOptions);
            
            numDocs = prodIt.getResultsSizeAfterPartialList();
            int startIndex = 1;
            int bufNumDocs = 0;
            
            // NOTE: use ArrayList instead of LinkedList (EntityListIterator) in buffered mode because it will use less total memory
            List<Map<String, Object>> solrDocs = (bufSize > 0) ? new ArrayList<>(Math.min(bufSize, numDocs)) : new LinkedList<>();
            
            Map<String, Object> productContext = new HashMap<>(context);
            productContext.put("useCache", clearAndUseCache);
            
            boolean lastReached = false;
            while (!lastReached) {
                startIndex = startIndex + bufNumDocs;
                
                // NOTE: the endIndex is actually a prediction, but if it's ever false, there is a serious DB problem
                int endIndex;
                if (bufSize > 0) endIndex = startIndex + Math.min(bufSize, numDocs-(startIndex-1)) - 1;
                else endIndex = numDocs;
                Debug.logInfo("Solr: rebuildSolrIndex: Reading products " + startIndex + "-" + endIndex + " / " + numDocs + " for indexing", module);

                solrDocs.clear();
                int numLeft = bufSize;
                while ((bufSize <= 0 || numLeft > 0) && !lastReached) {
                    GenericValue product = prodIt.next();
                    if (product != null) {
                        Map<String, Object> dispatchContext = SolrProductUtil.getProductContent(product, dctx, productContext);
                        solrDocs.add(dispatchContext);
                        numDocsIndexed++;
                        numLeft--;
                    } else {
                        lastReached = true;
                    }
                }
                bufNumDocs = solrDocs.size();
                if (bufNumDocs == 0) {
                    break;
                }
    
                // This adds all products to the Index (instantly)
                Map<String, Object> servCtx = UtilMisc.toMap("fieldList", solrDocs, "treatConnectErrorNonFatal", treatConnectErrorNonFatal);
                servCtx.put("useCache", clearAndUseCache);
                copyStdServiceFieldsNotSet(context, servCtx);
                Map<String, Object> runResult = dispatcher.runSync("addListToSolrIndex", servCtx);
                
                if (ServiceUtil.isError(runResult) || ServiceUtil.isFailure(runResult)) {
                    String runMsg = ServiceUtil.getErrorMessage(runResult);
                    if (UtilValidate.isEmpty(runMsg)) {
                        runMsg = null;
                    }
                    if (ServiceUtil.isFailure(runResult)) result = ServiceUtil.returnFailure(runMsg);
                    else result = ServiceUtil.returnError(runMsg);
                    break;
                }
            }
            
            if (result == null) {
                Debug.logInfo("Solr: rebuildSolrIndex: Finished with " + numDocsIndexed + " documents indexed", module);
                final String statusMsg = "Cleared solr index and reindexed " + numDocsIndexed + " documents";
                result = ServiceUtil.returnSuccess(statusMsg);
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
    
    /**
     * Rebuilds the solr index - auto run.
     */
    public static Map<String, Object> rebuildSolrIndexAuto(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();

        boolean startupForce = isReindexStartupForce(delegator, dispatcher);
        if (startupForce) {
            Debug.logInfo("Solr: rebuildSolrIndexAuto: Execution forced by force-startup system or config property", module);
        }
        boolean force = startupForce;

        boolean autoRunEnabled = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.autoRun.enabled", false);
        
        if (force || autoRunEnabled) {
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
                    + ", ifConfigChange: " + ifConfigChange + ", waitSolrReady: " + waitSolrReady + ")", module);

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
                    Debug.logError("Solr: rebuildSolrIndexAuto: rebuildSolrIndex returned an error: " + 
                            ServiceUtil.getErrorMessage(servResult), module);
                }

                // Just pass it all back, hackish but should work
                result = new HashMap<>();
                result.putAll(servResult);
            } catch (Exception e) {
                Debug.logError(e, "Solr: rebuildSolrIndexAuto: Error: " + e.getMessage(), module);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            Debug.logInfo("Solr: rebuildSolrIndexAuto: not running - disabled", module);
            result = ServiceUtil.returnSuccess();
        }

        return result;
    }    

    private static boolean isReindexStartupForce(Delegator delegator, LocalDispatcher dispatcher) {
        if (reindexAutoForceRan) return false;
        synchronized(SolrProductSearch.class) {
            if (reindexAutoForceRan) return false;
            reindexAutoForceRan = true;
            return getReindexStartupForceProperty(delegator, dispatcher, false);
        }
    }

    private static Boolean getReindexStartupForceProperty(Delegator delegator, LocalDispatcher dispatcher, Boolean defaultValue) {
        Boolean force = UtilMisc.booleanValueVersatile(System.getProperty(reindexStartupForceSysProp));
        if (force != null) return force;
        return UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, reindexStartupForceConfigProp, defaultValue);
    }

    /**
     * Rebuilds the solr index - only if dirty.
     */
    public static Map<String, Object> rebuildSolrIndexIfDirty(DispatchContext dctx, Map<String, Object> context) {
        return rebuildSolrIndex(dctx, context);
    }

    /**
     * Marks SOLR data as dirty.
     */
    public static Map<String, Object> setSolrDataStatus(DispatchContext dctx, Map<String, Object> context) {
        Map<String, Object> result;
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        try {
            SolrUtil.setSolrDataStatusId(delegator, (String) context.get("dataStatusId"), false);
            result = ServiceUtil.returnSuccess();
        } catch (Exception e) {
            result = ServiceUtil.returnError("Unable to set SOLR data status: " + e.getMessage());
        }
        return result;
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
        if (!SolrUtil.isSolrEnabled()) { // NOTE: this must NOT use SolrUtil.isSolrLocalWebappPresent() anymore
            return ServiceUtil.returnFailure("Solr not enabled");
        }
        HttpSolrClient client = null;
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
            return ServiceUtil.returnError("Error reloading Solr security authorizations");
        }
    }
}
