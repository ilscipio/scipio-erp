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
     * Re-generates alternative urls for product based on the ruleset outlined in SeoConfig.xml.
     * TODO: localize error msgs
     * <p>
     * TODO: REVIEW: the defaultLocalString is NOT trivial to retrieve, for now we simply use
     * the one from the source ProductContent text IF there is one, but if there is not
     * then it is left empty because it's too hard to retrieve for now (same problem as Solr).
     */
    public static Map<String, Object> generateProductAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String productId = (String) context.get("productId");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        Timestamp moment = UtilDateTime.nowTimestamp();

        GenericValue product = (GenericValue) context.get("product");
        try {
            if (product == null) {
                product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache(false).queryOne();
            }
            if (product == null) {
                return ServiceUtil.returnError("product not found for ID: " + productId);
            }
            return generateProductAlternativeUrls(delegator, dispatcher, context, product, 
                    replaceExisting, moment);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, message, module);
            return ServiceUtil.returnError(message);
        }
    }

    public static Map<String, Object> generateProductAlternativeUrls(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context,
            GenericValue product, boolean replaceExisting, Timestamp moment) throws Exception {
        final String nameField = "PRODUCT_NAME"; // TODO: UNHARDCODE
        
        String productId = product.getString("productId");
        
        // lookup existing alternative url
        GenericValue productContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductContent")
                .where("productId", productId, "productContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(false).queryList();
        if (UtilValidate.isNotEmpty(altUrlContent)) {
            if (!replaceExisting) {
                return ServiceUtil.returnSuccess("Product '" + product + "' already has alternative URLs - not recreating");
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
        Map<String, String> localeUrlMap = localeTextMap != null ? makeProductAltUrls(localeTextMap) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = makeProductAltUrl(defaultName);
        
        // store 
        if (productContent != null) {
            mainContent = productContent.getRelatedOne("Content", false);
        }
        mainContent = replaceAltUrlContentLocalized(delegator, dispatcher, context, mainContent, defaultLocaleString, 
                defaultLocaleUrl, localeUrlMap, moment);
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
        return result;
    }
    
    private static String makeProductAltUrl(String name) {
        if (UtilValidate.isEmpty(name)) return name;
        
        name = applyCommonAltUrl(name);
        
        name = SeoConfigUtil.limitProductNameLength(name);

        return name;
    }
    
    private static Map<String, String> makeProductAltUrls(Map<String, String> localeTextMap) {
        Map<String, String> localeSeoNameMap = new HashMap<>();
        for(Map.Entry<String, String> entry : localeTextMap.entrySet()) {
            localeSeoNameMap.put(entry.getKey(), makeProductAltUrl(entry.getValue()));
        }
        return localeSeoNameMap;
    }
    
    /**
     * SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateProductCategoryAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String productCategoryId = (String) context.get("productCategoryId");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        Timestamp moment = UtilDateTime.nowTimestamp();
        
        GenericValue productCategory = (GenericValue) context.get("productCategory");
        try {
            if (productCategory == null) {
                productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", productCategoryId).cache(false).queryOne();
            }
            if (productCategory == null) {
                return ServiceUtil.returnError("category not found for ID: " + productCategoryId);
            }
            return generateProductCategoryAlternativeUrls(delegator, dispatcher, context, productCategory, replaceExisting, moment);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, message, module);
            return ServiceUtil.returnError(message);
        }
    }
    
    public static Map<String, Object> generateProductCategoryAlternativeUrls(Delegator delegator, LocalDispatcher dispatcher, Map<String, ?> context,
            GenericValue productCategory, boolean replaceExisting, Timestamp moment) throws Exception {
        final String nameField = "CATEGORY_NAME"; // TODO: UNHARDCODE
        String productCategoryId = productCategory.getString("productCategoryId");
        
        // lookup existing alternative url
        GenericValue productCategoryContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductCategoryContent")
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(false).queryList();
        if (UtilValidate.isNotEmpty(altUrlContent)) {
            if (!replaceExisting) {
                return ServiceUtil.returnSuccess("ProductCategory '" + productCategoryId + "' already has alternative URLs - not recreating");
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
        Map<String, String> localeUrlMap = localeTextMap != null ? makeCategoryAltUrls(localeTextMap) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = makeCategoryAltUrl(defaultName);
        
        // store
        if (productCategoryContent != null) {
            mainContent = productCategoryContent.getRelatedOne("Content", false);
        }
        mainContent = replaceAltUrlContentLocalized(delegator, dispatcher, context, mainContent, defaultLocaleString, 
                defaultLocaleUrl, localeUrlMap, moment);
    
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
        return result;
    }
    
    private static String makeCategoryAltUrl(String name) {
        if (UtilValidate.isEmpty(name)) return name;
        
        name = applyCommonAltUrl(name);
        
        name = SeoConfigUtil.limitCategoryNameLength(name);

        return name;
    }
    
    private static Map<String, String> makeCategoryAltUrls(Map<String, String> localeTextMap) {
        Map<String, String> localeSeoNameMap = new HashMap<>();
        for(Map.Entry<String, String> entry : localeTextMap.entrySet()) {
            localeSeoNameMap.put(entry.getKey(), makeCategoryAltUrl(entry.getValue()));
        }
        return localeSeoNameMap;
    }
    
    private static String applyCommonAltUrl(String name) {
        name = SeoStringUtil.constructSeoName(name);

        // TODO: REVIEW
        name = SeoUrlUtil.replaceSpecialCharsUrl(name, SeoConfigUtil.getCharFilters());
        
        // ?
//        if (name.lastIndexOf("-") >= 100) {
//            name = name.substring(0, seoName.lastIndexOf("-"));
//        }
        
        return name;
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
            Timestamp moment) throws Exception {
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
    public static Map<String, Object> generateStoreAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
       
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        boolean doProducts = (typeGenerate == null) || typeGenerate.contains("product");
        boolean doCategory = (typeGenerate == null) || typeGenerate.contains("category");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));

        String webSiteId = (String) context.get("webSiteId");
        String productStoreId = (String) context.get("productStoreId");
        String prodCatalogId = (String) context.get("prodCatalogId");
        
        // TODO
        if (true) {
            return ServiceUtil.returnError("NOT IMPLEMENTED: only allStores=true supported");
        }
        
        Map<String, Object> result = ServiceUtil.returnSuccess();
        return result;
    }
    
    /**
     * Re-generates alternative urls for all stores/websites based on ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateAllAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
       
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        boolean doProducts = (typeGenerate == null) || typeGenerate.contains("product");
        boolean doCategory = (typeGenerate == null) || typeGenerate.contains("category");
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        
        // IMPORTANT: USE LIST ITERATOR
        // TODO: REVIEW: currently avoids store lookup overhead but may not last,
        // we may be forced to lookup stores here due to defaultLocaleString and other setting...
        
        List<String> msgList = new ArrayList<>();
        List<String> errMsgList = new ArrayList<>();
        
        if (doProducts) {
            int productSuccess = 0;
            int productError = 0;
            
            EntityListIterator productIt = null;
            try {
                productIt = EntityQuery.use(delegator).from("Product").cache(false).queryIterator();
                GenericValue product;
                while ((product = productIt.next()) != null) {
                    String productId = product.getString("productId");
                    Map<String, Object> servCtx = dctx.makeValidContext("generateProductAlternativeUrls", ModelService.IN_PARAM, context);
                    servCtx.put("product", product);
                    servCtx.put("productId", productId);
                    // TODO: REVIEW: FORCED TO USE SERVICE (SLOWER) FOR SEPARATE TRANSACTION
                    Map<String, Object> recordResult = dispatcher.runSync("generateProductAlternativeUrls", servCtx, -1, true);
                    //Map<String, Object> recordResult = generateProductAlternativeUrls(delegator, dispatcher, 
                    //        context, product, replaceExisting, moment);
                    if (ServiceUtil.isSuccess(recordResult)) {
                        productSuccess++;
                    } else {
                        Debug.logError("Scipio: Seo: Error generating alternative links for product '" 
                                + productId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
                        productError++;
                    }
                }
            } catch (Exception e) {
                String message = "Error while generating alternative links: " + e.getMessage();
                Debug.logError(e, message, module);
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
            
            msgList.add("products updated: " + productSuccess);
            if (productError > 0) errMsgList.add("products failed: " + productError);
        }
        
        if (doCategory) {
            int categorySuccess = 0;
            int categoryError = 0;
            
            EntityListIterator productCategoryIt = null;
            try {
                productCategoryIt = EntityQuery.use(delegator).from("ProductCategory").cache(false).queryIterator();
                GenericValue productCategory;
                while ((productCategory = productCategoryIt.next()) != null) {
                    String productCategoryId = productCategory.getString("productCategoryId");
                    Map<String, Object> servCtx = dctx.makeValidContext("generateProductCategoryAlternativeUrls", ModelService.IN_PARAM, context);
                    servCtx.put("productCategory", productCategory);
                    servCtx.put("productCategoryId", productCategoryId);
                    // TODO: REVIEW: FORCED TO USE SERVICE (SLOWER) FOR SEPARATE TRANSACTION
                    Map<String, Object> recordResult = dispatcher.runSync("generateProductCategoryAlternativeUrls", servCtx, -1, true);
                    if (ServiceUtil.isSuccess(recordResult)) {
                        categorySuccess++;
                    } else {
                        Debug.logError("Scipio: Seo: Error generating alternative links for category '" 
                                + productCategoryId + "': " + ServiceUtil.getErrorMessage(recordResult), module);
                        categoryError++;
                    }
                }
            } catch (Exception e) {
                String message = "Error while generating alternative links: " + e.getMessage();
                Debug.logError(e, message, module);
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
            
            msgList.add("categories updated: " + categorySuccess);
            if (categoryError > 0) errMsgList.add("categories failed: " + categoryError);
        }
        
        // TODO?: FUTURE
        //if (doContent) {
        //}

        List<String> allMsgs = new ArrayList<>();
        allMsgs.addAll(msgList);
        allMsgs.addAll(errMsgList);
        String msg = StringUtils.join(allMsgs, "; ");
        
        Map<String, Object> result = (errMsgList.size() > 0) ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg);
        return result;
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
