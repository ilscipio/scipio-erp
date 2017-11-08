package com.ilscipio.scipio.product.seo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer;
import com.ilscipio.scipio.util.SeoStringUtil;

/**
 * SCIPIO: SEO catalog services.
 * TODO: localize error msgs
 */
public abstract class SeoCatalogServices {

    public static final String module = SeoCatalogServices.class.getName();
    
    protected SeoCatalogServices() {
    }

    /**
     * Returned instance can only perform generic (non-website-specific) operations. 
     */
    private static CatalogAltUrlSanitizer getCatalogAltUrlSanitizer(DispatchContext dctx, Map<String, ? extends Object> context) {
        return SeoCatalogUrlWorker.getDefaultInstance(dctx.getDelegator()).getCatalogAltUrlSanitizer();
    }
    
    /**
     * Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml.
     * TODO: localize error msgs
     * <p>
     * TODO: REVIEW: the defaultLocalString is NOT trivial to retrieve, for now we simply use
     * the one from the source ProductContent text IF there is one, but if there is not
     * then it is left empty because it's too hard to retrieve for now (same problem as Solr).
     */
    public static Map<String, Object> generateProductAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        String productId = (String) context.get("productId");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        boolean removeOldLocales = !Boolean.FALSE.equals(context.get("removeOldLocales"));
        Timestamp moment = UtilDateTime.nowTimestamp();

        GenericValue product = (GenericValue) context.get("product");
        try {
            if (product == null) {
                product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache(false).queryOne();
            }
            if (product == null) {
                return ServiceUtil.returnError("product not found for ID: " + productId);
            }
            return generateProductAlternativeUrls(dctx, context, product, 
                    replaceExisting, removeOldLocales, moment);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, message, module);
            return ServiceUtil.returnError(message);
        }
    }

    static Map<String, Object> generateProductAlternativeUrls(DispatchContext dctx, Map<String, ?> context,
            GenericValue product, boolean replaceExisting, boolean removeOldLocales, Timestamp moment) throws Exception {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        final String nameField = "PRODUCT_NAME"; // TODO: UNHARDCODE
        
        // WARN/TODO?: this service does not run for variant or other child products
        
        String productId = product.getString("productId");
        
        // lookup existing alternative url
        GenericValue productContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductContent")
                .where("productId", productId, "productContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(false).queryList();
        if (UtilValidate.isNotEmpty(altUrlContent)) {
            if (!replaceExisting) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("productUpdated", Boolean.FALSE);
                return result;
            }
            productContent = altUrlContent.get(0);
        }
        
        GenericValue mainContent = null;
        
        // get names by locale and default name
        String defaultLocaleString = null;
        GenericValue nameMainContent = null;
        Map<String, String> localeTextMap = null;
        GenericValue nameProductContent = EntityQuery.use(delegator).from("ProductContent")
                .where("productId", productId, "productContentTypeId", nameField).filterByDate(moment).cache(false).queryFirst();
        if (nameProductContent == null) {
            // CHECK VIRTUAL PRODUCT for source texts - see product content wrapper
            if ("Y".equals(product.getString("isVariant"))) {
                GenericValue parent = ProductWorker.getParentProduct(productId, delegator, false);
                if (parent != null) {
                    nameProductContent = EntityQuery.use(delegator).from("ProductContent")
                            .where("productId", parent.getString("productId"), "productContentTypeId", nameField).filterByDate(moment).cache(false).queryFirst();
                }
            }
        }
        if (nameProductContent != null) {
            nameMainContent = nameProductContent.getRelatedOne("Content", false);
            localeTextMap = getSimpleTextsByLocaleString(delegator, dispatcher, 
                    nameMainContent, moment, false);
            defaultLocaleString = nameMainContent.getString("localeString"); // TODO: REVIEW: setting this to the main name record localeString, or null if none
        }
        String defaultName = determineDefaultName(delegator, dispatcher, productId, 
                ProductContentWrapper.getEntityFieldValue(product, nameField, delegator, dispatcher, false),
                defaultLocaleString, nameMainContent, localeTextMap); 
        
        // make seo names
        CatalogAltUrlSanitizer sanitizer = getCatalogAltUrlSanitizer(dctx, context);
        Map<String, String> localeUrlMap = localeTextMap != null ? sanitizer.convertProductNamesToAltUrls(localeTextMap) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = sanitizer.convertProductNameToAltUrl(defaultName, sanitizer.parseLocale(defaultLocaleString));
        
        // store 
        if (productContent != null) {
            mainContent = productContent.getRelatedOne("Content", false);
        }
        mainContent = replaceAltUrlContentLocalized(delegator, dispatcher, context, mainContent, defaultLocaleString, 
                defaultLocaleUrl, localeUrlMap, removeOldLocales, moment);
        if (productContent == null) {
            productContent = delegator.makeValue("ProductContent");
            productContent.put("productId", productId);
            productContent.put("contentId", mainContent.getString("contentId"));
            productContent.put("productContentTypeId", "ALTERNATIVE_URL");
            productContent.put("fromDate", UtilDateTime.nowTimestamp());
            //productContent.put("sequenceNum", new Long(1)); // no
            productContent = delegator.create(productContent);
        }
        
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("mainContentId", mainContent.getString("contentId"));
        result.put("productUpdated", Boolean.TRUE);
        return result;
    }
    
    /**
     * SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateProductCategoryAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String productCategoryId = (String) context.get("productCategoryId");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        boolean removeOldLocales = !Boolean.FALSE.equals(context.get("removeOldLocales"));
        Timestamp moment = UtilDateTime.nowTimestamp();
        
        GenericValue productCategory = (GenericValue) context.get("productCategory");
        try {
            if (productCategory == null) {
                productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", productCategoryId).cache(false).queryOne();
            }
            if (productCategory == null) {
                return ServiceUtil.returnError("category not found for ID: " + productCategoryId);
            }
            return generateProductCategoryAlternativeUrls(dctx, context, 
                    productCategory, replaceExisting, removeOldLocales, moment);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, message, module);
            return ServiceUtil.returnError(message);
        }
    }
    
    static Map<String, Object> generateProductCategoryAlternativeUrls(DispatchContext dctx, Map<String, ?> context,
            GenericValue productCategory, boolean replaceExisting, boolean removeOldLocales, Timestamp moment) throws Exception {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        final String nameField = "CATEGORY_NAME"; // TODO: UNHARDCODE
        String productCategoryId = productCategory.getString("productCategoryId");
        
        // lookup existing alternative url
        GenericValue productCategoryContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductCategoryContent")
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(false).queryList();
        if (UtilValidate.isNotEmpty(altUrlContent)) {
            if (!replaceExisting) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("categoryUpdated", Boolean.FALSE);
                return result;
            }
            productCategoryContent = altUrlContent.get(0);
        }
        
        GenericValue mainContent = null;
        
        // get names by locale and default name
        String defaultLocaleString = null;
        GenericValue nameMainContent = null;
        Map<String, String> localeTextMap = null;
        GenericValue nameProductCategoryContent = EntityQuery.use(delegator).from("ProductCategoryContent")
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", nameField).filterByDate(moment).cache(false).queryFirst();
        if (nameProductCategoryContent != null) {
            nameMainContent = nameProductCategoryContent.getRelatedOne("Content", false);
            localeTextMap = getSimpleTextsByLocaleString(delegator, dispatcher, 
                    nameMainContent, moment, false);
            defaultLocaleString = nameMainContent.getString("localeString"); // TODO: REVIEW: setting this to the main name record localeString, or null if none
        }
        String defaultName = determineDefaultName(delegator, dispatcher, productCategoryId, 
                CategoryContentWrapper.getEntityFieldValue(productCategory, nameField, delegator, dispatcher, false),
                defaultLocaleString, nameMainContent, localeTextMap); 
        
        // make seo names
        CatalogAltUrlSanitizer sanitizer = getCatalogAltUrlSanitizer(dctx, context);
        Map<String, String> localeUrlMap = localeTextMap != null ? sanitizer.convertCategoryNamesToAltUrls(localeTextMap) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = sanitizer.convertCategoryNameToAltUrl(defaultName, sanitizer.parseLocale(defaultLocaleString));
        
        // store
        if (productCategoryContent != null) {
            mainContent = productCategoryContent.getRelatedOne("Content", false);
        }
        mainContent = replaceAltUrlContentLocalized(delegator, dispatcher, context, mainContent, defaultLocaleString, 
                defaultLocaleUrl, localeUrlMap, removeOldLocales, moment);
    
        if (productCategoryContent == null) {
            productCategoryContent = delegator.makeValue("ProductCategoryContent");
            productCategoryContent.put("productCategoryId", productCategoryId);
            productCategoryContent.put("contentId", mainContent.getString("contentId"));
            productCategoryContent.put("prodCatContentTypeId", "ALTERNATIVE_URL");
            productCategoryContent.put("fromDate", UtilDateTime.nowTimestamp());
            //productCategoryContent.put("sequenceNum", new Long(1)); // no
            productCategoryContent = delegator.create(productCategoryContent);
        }
        
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("mainContentId", mainContent.getString("contentId"));
        result.put("categoryUpdated", Boolean.TRUE);
        return result;
    }
    
    /**
     * This uses the content wrapper-like behavior to get a default name.
     */
    private static String determineDefaultName(Delegator delegator, LocalDispatcher dispatcher, String id, String entityFieldValue, 
            String defaultLocaleString, GenericValue nameMainContent, Map<String, String> localeTextMap) throws GenericEntityException {
        String defaultName = entityFieldValue;
        
        // default lang name fallbacks
        if (UtilValidate.isEmpty(defaultName)) {
            if (defaultLocaleString != null && localeTextMap != null) {
                defaultName = localeTextMap.get(defaultLocaleString);
            }
            if (UtilValidate.isEmpty(defaultName) && nameMainContent != null) {
                defaultName = getContentElectronicText(delegator, dispatcher, nameMainContent).getString("textData");
            }
        }
        if (UtilValidate.isEmpty(defaultName)) {
            defaultName = id;
        }
        return defaultName;
    }

    /**
     * TODO: move this, I need it elsewhere too...
     */
    private static Map<String, String> getSimpleTextsByLocaleString(Delegator delegator, LocalDispatcher dispatcher,
            GenericValue mainContent, Timestamp moment, boolean useCache) throws GenericEntityException {
        Map<String, String> localeTextMap = new HashMap<>();
        if (UtilValidate.isNotEmpty(mainContent.getString("localeString"))) {
            GenericValue elecText = getContentElectronicText(delegator, dispatcher, mainContent);
            if (UtilValidate.isNotEmpty(elecText.getString("textData"))) {
                localeTextMap.put(mainContent.getString("localeString"), elecText.getString("textData"));
            }
        }
        List<GenericValue> assocViewList = EntityQuery.use(delegator).from("ContentAssocToElectronicText")
                .where("contentIdStart", mainContent.getString("contentId"), 
                        "contentAssocTypeId", "ALTERNATE_LOCALE").filterByDate(moment).cache(useCache).queryList();
        for(GenericValue assocView : assocViewList) {
            if (UtilValidate.isNotEmpty(assocView.getString("localeString"))) {
                localeTextMap.put(assocView.getString("localeString"), assocView.getString("textData"));
            }
        }
        return localeTextMap;
    }
    
    /**
     * TODO: move elsewhere, will need to reuse...
     */
    private static GenericValue replaceAltUrlContentLocalized(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context,
            GenericValue mainContent, String defaultLocaleString, String defaultLocaleUrl, Map<String, String> localeUrlMap,
            boolean removeOldLocales, Timestamp moment) throws Exception {
        Set<String> remainingLocales = new HashSet<>(localeUrlMap.keySet());
        
        // update the main content record
        String mainLocaleString;
        String mainTextData;
        if (mainContent != null) {
            // TODO: REVIEW: for now ALWAYS change the main record locale to the defaultLocaleString
            // because the only way this is manageable is if it follows the source localized texts setup
            mainLocaleString = defaultLocaleString;

            // SPECIAL CASE: lang override for main content in the map
            mainTextData = localeUrlMap.get(mainLocaleString);
            if (UtilValidate.isNotEmpty(mainTextData)) {
                remainingLocales.remove(mainLocaleString);
            } else {
                mainTextData = defaultLocaleUrl;
            }
        } else {
            mainLocaleString = defaultLocaleString;
            mainTextData = defaultLocaleUrl;
        }
        // update main content
        if (mainContent == null) {
            mainContent = createAltUrlSimpleTextContent(delegator, dispatcher, mainLocaleString, mainTextData);
        } else {
            updateAltUrlSimpleTextContent(delegator, dispatcher, mainContent, mainLocaleString, mainTextData);
        }
        
        List<GenericValue> contentAssocList = EntityQuery.use(delegator).from("ContentAssoc")
                .where("contentId", mainContent.getString("contentId"), "contentAssocTypeId", "ALTERNATE_LOCALE")
                .filterByDate(moment).cache(false).queryList();
        
        // update in-place or remove existing assoc records
        for(GenericValue contentAssoc : contentAssocList) {
            GenericValue content = contentAssoc.getRelatedOne("ToContent", false);
            String localeString = content.getString("localeString");
            
            String textData = localeUrlMap.get(localeString);
            if (UtilValidate.isNotEmpty(textData)) {
                updateAltUrlSimpleTextContent(delegator, dispatcher, content, textData);
                remainingLocales.remove(localeString);
            } else {
                if (removeOldLocales) {
                    Map<String, Object> servCtx = new HashMap<>();
                    servCtx.put("userLogin", context.get("userLogin"));
                    servCtx.put("locale", context.get("locale"));
                    servCtx.put("contentId", content.get("contentId"));
                    Map<String, Object> servResult = dispatcher.runSync("removeContentAndRelated", servCtx);
                    if (ServiceUtil.isError(servResult)) {
                        throw new SeoCatalogException("Cannot remove ALTERNATE_LOCALE record contentId '" 
                                + content.get("contentId") + "': " + ServiceUtil.getErrorMessage(servResult));
                    }
                }
            }
        }
        
        // create new assoc records
        for(String localeString : remainingLocales) {
            String textData = localeUrlMap.get(localeString);
            if (UtilValidate.isNotEmpty(textData)) {
                GenericValue content = createAltUrlSimpleTextContent(delegator, dispatcher, localeString, textData);
                GenericValue contentAssoc = delegator.makeValue("ContentAssoc");
                contentAssoc.put("contentId", mainContent.getString("contentId"));
                contentAssoc.put("contentIdTo", content.getString("contentId"));
                contentAssoc.put("fromDate", moment);
                contentAssoc.put("contentAssocTypeId", "ALTERNATE_LOCALE");
                contentAssoc = delegator.create(contentAssoc);
            }
        }

        return mainContent;
    }
    
    private static GenericValue createAltUrlSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, String localeString, String textData) throws GenericEntityException {
        // create dataResource
        GenericValue dataResource = delegator.makeValue("DataResource");
        dataResource.put("dataResourceTypeId", "ELECTRONIC_TEXT");
        dataResource.put("statusId", "CTNT_PUBLISHED");
        dataResource = delegator.createSetNextSeqId(dataResource);
        String dataResourceId = dataResource.getString("dataResourceId");

        // create electronicText
        GenericValue electronicText = delegator.makeValue("ElectronicText");
        electronicText.put("dataResourceId", dataResourceId);

        electronicText.put("textData", textData);
        electronicText = delegator.create(electronicText);

        // create content
        GenericValue content = delegator.makeValue("Content");
        if (UtilValidate.isNotEmpty(localeString)) content.put("localeString", localeString);
        content.put("dataResourceId", dataResourceId);
        content.put("contentTypeId", "DOCUMENT");
        content.put("description", "Alternative URL");
        content = delegator.createSetNextSeqId(content);
        
        return content;
    }
    
    private static void updateAltUrlSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, GenericValue content, String localeString, String textData) throws GenericEntityException {
        updateAltUrlSimpleTextContent(delegator, dispatcher, content, textData);
        content.put("localeString", localeString);
        content.store();
    }
    
    private static void updateAltUrlSimpleTextContent(Delegator delegator, LocalDispatcher dispatcher, GenericValue content, String textData) throws GenericEntityException {
        GenericValue elecText = getContentElectronicText(delegator, dispatcher, content);
        elecText.put("textData", textData);
        elecText.store();
    }
    
    private static GenericValue getContentElectronicText(Delegator delegator, LocalDispatcher dispatcher, GenericValue content) throws GenericEntityException {
        GenericValue elecText = EntityQuery.use(delegator).from("ElectronicText")
                .where("dataResourceId", content.getString("dataResourceId"))
                .cache(false).queryOne();
        if (elecText == null) {
            throw new GenericEntityException("Simple text content '" + content.getString("contentId") + "' has no ElectronicText");
        }
        return elecText;
    }
    
    /**
     * Re-generates alternative urls for store/website based on ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateWebsiteAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
       
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        if (typeGenerate == null) typeGenerate = Collections.emptyList();
        boolean doAll = typeGenerate.contains("all");
        boolean doProducts = doAll || typeGenerate.contains("product");
        boolean doCategory = doAll || typeGenerate.contains("category");
        
        String webSiteId = (String) context.get("webSiteId");
        String productStoreId = (String) context.get("productStoreId");
        String prodCatalogId = (String) context.get("prodCatalogId");
        
        boolean useCache = Boolean.TRUE.equals(context.get("useCache")); // FALSE default
        
        GenStats stats = new GenStats(doProducts, doCategory);
        
        List<GenericValue> prodCatalogList;
        try {
            if (UtilValidate.isNotEmpty(prodCatalogId)) {
                GenericValue prodCatalog = delegator.findOne("ProdCatalog", UtilMisc.toMap("prodCatalogId", prodCatalogId), useCache);
                if (prodCatalog == null) return ServiceUtil.returnError("catalog not found: " + prodCatalogId);
                prodCatalogList = UtilMisc.toList(prodCatalog);
            } else {
                if (UtilValidate.isEmpty(productStoreId)) {
                    if (UtilValidate.isEmpty(webSiteId)) {
                        return ServiceUtil.returnError("missing webSiteId, productStoreId or prodCatalogId");
                    }
                    GenericValue webSite = delegator.findOne("WebSite", UtilMisc.toMap("webSiteId", webSiteId), useCache);
                    if (webSiteId == null) return ServiceUtil.returnError("web site not found: " + webSiteId);
                    productStoreId = webSite.getString("productStoreId");
                    if (UtilValidate.isEmpty(productStoreId)) return ServiceUtil.returnError("web site has no product store: " + webSiteId);
    
                }
                GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
                if (productStore == null) return ServiceUtil.returnError("store not found: " + productStoreId);
                
                prodCatalogList = EntityQuery.use(delegator).from("ProductStoreCatalog").where("productStoreId", productStoreId)
                        .filterByDate().cache(useCache).queryList();
            }
            
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> catalogCategories = EntityQuery.use(delegator).from("ProdCatalogCategory")
                        .where("prodCatalogId", prodCatalog.getString("prodCatalogId")).filterByDate().cache(false).queryList();
                generateCategoryAltUrlsDeep(dctx, context, stats, catalogCategories, doProducts, doCategory, useCache);
            }
        } catch(Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, "Seo: " + message, module);
            return ServiceUtil.returnError(message);
        }
        
        String msg = stats.toMsg();
        return stats.hasError() ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg);
    }
    
    /**
     * Re-generates alternative urls for all stores/websites based on ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateAllAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
       
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        if (typeGenerate == null) typeGenerate = Collections.emptyList();
        boolean doAll = typeGenerate.contains("all");
        boolean doProducts = doAll || typeGenerate.contains("product");
        boolean doCategory = doAll || typeGenerate.contains("category");
        
        boolean useCache = Boolean.TRUE.equals(context.get("useCache")); // FALSE default
        
        //boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting")); // makeValidContext transfers
        
        // IMPORTANT: USE LIST ITERATOR
        // TODO: REVIEW: currently avoids store lookup overhead but may not last,
        // we may be forced to lookup stores here due to defaultLocaleString and other setting...
        
        GenStats stats = new GenStats(doProducts, doCategory);
        
        if (doProducts) {
            EntityListIterator productIt = null;
            try {
                productIt = EntityQuery.use(delegator).from("Product").cache(useCache).queryIterator();
                GenericValue product;
                while ((product = productIt.next()) != null) {
                    // NOTE: doChildProducts = false, because already counted in our global query here
                    generateProductAltUrls(dctx, context, product, stats, false, useCache);
                }
            } catch (Exception e) {
                String message = "Error while generating alternative links: " + e.getMessage();
                Debug.logError(e, "Seo: " + message, module);
                return ServiceUtil.returnError(message);
            } finally {
                if (productIt != null) {
                    try {
                        productIt.close();
                    } catch(Throwable t) {
                        Debug.logError(t, module);
                    }
                }
            }
        }
        
        if (doCategory) {
            EntityListIterator productCategoryIt = null;
            try {
                productCategoryIt = EntityQuery.use(delegator).from("ProductCategory").cache(useCache).queryIterator();
                GenericValue productCategory;
                while ((productCategory = productCategoryIt.next()) != null) {
                    generateCategoryAltUrls(dctx, context, productCategory, stats, useCache);
                }
            } catch (Exception e) {
                String message = "Error while generating alternative links: " + e.getMessage();
                Debug.logError(e, "Seo: " + message, module);
                return ServiceUtil.returnError(message);
            } finally {
                if (productCategoryIt != null) {
                    try {
                        productCategoryIt.close();
                    } catch(Throwable t) {
                        Debug.logError(t, module);
                    }
                }
            }
        }
        
        // TODO?: FUTURE
        //if (doContent) {
        //}
        
        String msg = stats.toMsg();
        return stats.hasError() ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg);
    }
    
    private static class GenStats {
        public final boolean doProducts;
        public final boolean doCategory;
        
        public int productSuccess = 0;
        public int productSkipped = 0;
        public int productError = 0;
        
        public int categorySuccess = 0;
        public int categoryError = 0;
        public int categorySkipped = 0;

        public GenStats(boolean doProducts, boolean doCategory) {
            this.doProducts = doProducts;
            this.doCategory = doCategory;
        }

        public boolean hasError() {
            return productError > 0 || categoryError > 0;
        }
        
        public void toMsgLists(List<String> msgList, List<String> errMsgList) {
            if (doProducts) {
                msgList.add("Products updated: " + productSuccess);
                msgList.add("Products skipped: " + productSkipped);
                if (productError > 0) errMsgList.add("Products failed: " + productError);
            }
            
            if (doCategory) {
                msgList.add("Categories updated: " + categorySuccess);
                msgList.add("Categories skipped: " + categorySkipped);
                if (categoryError > 0) errMsgList.add("Categories failed: " + categoryError);
            }
        }
        
        public String toMsg() {
            List<String> msgList = new ArrayList<>();
            List<String> errMsgList = new ArrayList<>();
            toMsgLists(msgList, errMsgList);
            
            List<String> allMsgs = new ArrayList<>();
            allMsgs.addAll(msgList);
            allMsgs.addAll(errMsgList);
            return StringUtils.join(allMsgs, "; ");
        }
    }
    
    // recursive helper, rewritten from getTreeCategories
    private static void generateCategoryAltUrlsDeep(DispatchContext dctx, Map<String, ?> context, GenStats stats,
            List<GenericValue> productCategories, boolean doProducts, boolean doCategory, boolean useCache) throws GeneralException {
        for (GenericValue productCategory : productCategories) {
            GenericValue category = null;
            if (productCategory.getModelEntity().getEntityName().equals("ProductCategoryRollup")) {
                category = productCategory.getRelatedOne("CurrentProductCategory", useCache);
            } else if (productCategory.getModelEntity().getEntityName().equals("ProdCatalogCategory")) {
                category = productCategory.getRelatedOne("ProductCategory", useCache);
            }
            if (category != null) {
                // self
                if (doCategory) {
                    generateCategoryAltUrls(dctx, context, category, stats, useCache);
                }
                
                // child cats (recursive)
                List<GenericValue> childProductCategoryRollups = EntityQuery.use(dctx.getDelegator()).from("ProductCategoryRollup")
                        .where("parentProductCategoryId", category.getString("productCategoryId")).filterByDate().cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                    generateCategoryAltUrlsDeep(dctx, context, stats, 
                            childProductCategoryRollups, doProducts, doCategory, useCache);
                }

                // products
                if (doProducts) {
                    List<GenericValue> productCategoryMembers = EntityQuery.use(dctx.getDelegator()).from("ProductCategoryMember")
                            .where("productCategoryId", category.getString("productCategoryId")).filterByDate()
                            .cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                    if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                        for (GenericValue productCategoryMember : productCategoryMembers) {
                            GenericValue product = productCategoryMember.getRelatedOne("Product", useCache);
                            generateProductAltUrls(dctx, context, product, stats, true, useCache);
                        }
                    }
                }
            }
        }
    }
    
    // helper wrapper
    private static void generateCategoryAltUrls(DispatchContext dctx, Map<String, ? extends Object> context, GenericValue productCategory, GenStats stats, boolean useCache) throws GeneralException {
        String productCategoryId = productCategory.getString("productCategoryId");
        Map<String, Object> servCtx = dctx.makeValidContext("generateProductCategoryAlternativeUrlsCore", ModelService.IN_PARAM, context);
        servCtx.put("productCategory", productCategory);
        servCtx.put("productCategoryId", productCategoryId);
        // TODO: REVIEW: FORCED TO USE SERVICE (SLOWER) FOR SEPARATE TRANSACTION
        Map<String, Object> recordResult = dctx.getDispatcher().runSync("generateProductCategoryAlternativeUrlsCore", servCtx, -1, true);
        if (ServiceUtil.isSuccess(recordResult)) {
            if (Boolean.TRUE.equals(recordResult.get("categoryUpdated"))) {
                stats.categorySuccess++;
            } else {
                stats.categorySkipped++;
            }
        } else {
            Debug.logError("Scipio: Seo: Error generating alternative links for category '" 
                    + productCategoryId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
            stats.categoryError++;
        }
    }
    
    // helper wrapper
    private static void generateProductAltUrls(DispatchContext dctx, Map<String, ? extends Object> context, GenericValue product, GenStats stats, boolean doChildProducts, boolean useCache) throws GeneralException {
        boolean includeVariant = Boolean.TRUE.equals(context.get("includeVariant"));
        if (!includeVariant && "Y".equals(product.getString("isVariant"))) {
            return;
        }
        
        String productId = product.getString("productId");
        
        Map<String, Object> servCtx = dctx.makeValidContext("generateProductAlternativeUrlsCore", ModelService.IN_PARAM, context);
        servCtx.put("product", product);
        servCtx.put("productId", productId);
        // TODO: REVIEW: FORCED TO USE SERVICE (SLOWER) FOR SEPARATE TRANSACTION
        Map<String, Object> recordResult = dctx.getDispatcher().runSync("generateProductAlternativeUrlsCore", servCtx, -1, true);
        //Map<String, Object> recordResult = generateProductAlternativeUrls(delegator, dispatcher, 
        //        context, product, replaceExisting, moment);
        if (ServiceUtil.isSuccess(recordResult)) {
            if (Boolean.TRUE.equals(recordResult.get("productUpdated"))) {
                stats.productSuccess++;
            } else {
                stats.productSkipped++;
            }
        } else {
            Debug.logError("Scipio: Seo: Error generating alternative links for product '" 
                    + productId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
            stats.productError++;
        }
        
        if (doChildProducts) {
            
            // TODO?: other child product associations?
            
            if (includeVariant && "Y".equals(product.getString("isVirtual"))) {
                List<GenericValue> variantAssocList = EntityQuery.use(dctx.getDelegator()).from("ProductAssoc")
                        .where("productId", productId, "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().cache(useCache).queryList();
                for(GenericValue variantAssoc : variantAssocList) {
                    GenericValue variantProduct = variantAssoc.getRelatedOne("AssocProduct", useCache);
                    generateProductAltUrls(dctx, context, variantProduct, stats, doChildProducts, useCache);
                }
            }
        }
    }
    
    /**
     * SCIPIO: SEO Service to generate an alternative product URL; relies on a common SEO ruleset
     * @deprecated not usable, for reference only
     * */
    @Deprecated
    static Map<String, Object> generateAlternativeURL(DispatchContext dctx, Map<String, ? extends Object> context) throws GenericEntityException {
        Delegator delegator = dctx.getDelegator();
        
        List<GenericValue> products = null;
        try {
            String productId = (String) context.get("product");
            if(UtilValidate.isNotEmpty(productId)){
                products = delegator.findByAnd("Product", UtilMisc.toMap("productId",productId),null,false);
            }else{
                products = delegator.findAll("Product", true);  
            }
        }
        catch (Exception ex) {
            String message = "Cannot determine products.";
            Debug.logError(ex, message, module);
            return ServiceUtil.returnError(message);
        }

        if (UtilValidate.isEmpty(products)) {
            return ServiceUtil.returnError("No products found.");
        }


        Map<String, String> seoNames;
        for (GenericValue product : products) {
            seoNames = new HashMap<String, String>();

            String productId = product.getString("productId");
            String productName = product.getString("productName");
            String seoName = SeoStringUtil.constructSeoName(productName);

            if (seoName.lastIndexOf("-") >= 100) {
                seoName = seoName.substring(0, seoName.lastIndexOf("-"));
            }

            seoNames.put("productId", productId);
            seoNames.put("seoLink", seoName);
            
            try {
                String contentId = productId + "_AU";

                // create dataResource
                GenericValue dataResource = delegator.makeValue("DataResource");
                dataResource.put("dataResourceId", contentId);
                dataResource.put("dataResourceTypeId", "ELECTRONIC_TEXT");
                dataResource.put("dataTemplateTypeId", "FTL");
                dataResource.put("statusId", "CTNT_PUBLISHED");
                // delegator.create(dataResource, false);
                dataResource.create();

                // create electronicText
                GenericValue electronicText = delegator.makeValue("ElectronicText");
                electronicText.put("dataResourceId", contentId);

                electronicText.put("textData", seoName);
                // delegator.create(electronicText, false);
                electronicText.create();

                // create content
                GenericValue content = delegator.makeValue("Content");
                content.put("contentId", contentId);

                content.put("dataResourceId", contentId);
                content.put("contentTypeId", "DOCUMENT");
                content.put("description", "Alternative URL");
                //content.put("localeString", "de"); // no
                // delegator.create(content, false);
                content.create();

                GenericValue productContent = null;
                productContent = delegator.makeValue("ProductContent");
                productContent.put("productId", productId);
                productContent.put("contentId", contentId);
                productContent.put("productContentTypeId", "ALTERNATIVE_URL");
                productContent.put("fromDate", UtilDateTime.nowTimestamp());
                productContent.put("sequenceNum", new Long(1));
                // delegator.create(productContent, false);
                productContent.create();
            }
            catch (Exception ex) {
                String message = "Error while generating alternative links.";
                Debug.logError(ex, message, module);
                return ServiceUtil.returnError(message);
            }
        }

        return ServiceUtil.returnSuccess();
    }
    
    
    @SuppressWarnings("serial")
    public static class SeoCatalogException extends Exception {

        protected SeoCatalogException() {
            super();
        }

        protected SeoCatalogException(String message, Throwable cause, boolean enableSuppression,
                boolean writableStackTrace) {
            super(message, cause, enableSuppression, writableStackTrace);
        }

        protected SeoCatalogException(String message, Throwable cause) {
            super(message, cause);
        }

        protected SeoCatalogException(String message) {
            super(message);
        }

        protected SeoCatalogException(Throwable cause) {
            super(cause);
        }
    }
}
