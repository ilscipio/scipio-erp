package com.ilscipio.scipio.product.seo;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer;
import com.ilscipio.scipio.product.category.CatalogUrlType;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlExporter.ExportTraversalConfig;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlGenerator.GenTraversalConfig;

/**
 * SCIPIO: SEO catalog services.
 * <p>
 * TODO: localize error msgs
 * TODO?: other child product associations (only virtual-variant covered)
 */
public abstract class SeoCatalogServices {

    public static final String module = SeoCatalogServices.class.getName();
    
    private static final String logPrefix = "Seo: ";
    
    private static final String productNameField = "PRODUCT_NAME";
    private static final String categoryNameField = "CATEGORY_NAME";
    
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
        
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        boolean removeOldLocales = !Boolean.FALSE.equals(context.get("removeOldLocales"));
        Timestamp moment = UtilDateTime.nowTimestamp();
        final boolean useCache = false; // probably bad idea
        boolean doChildProducts = Boolean.TRUE.equals(context.get("doChildProducts"));
        boolean includeVariant = !Boolean.FALSE.equals(context.get("includeVariant"));

        GenericEntity productEntity = (GenericEntity) context.get("product");
        try {
            GenericValue product;
            if (productEntity instanceof GenericValue) {
                product = (GenericValue) productEntity;
            } else {
                String productId = (productEntity != null) ? productEntity.getString("productId") : (String) context.get("productId");
                product = delegator.findOne("Product", UtilMisc.toMap("productId", productId), useCache);
                if (product == null) {
                    return ServiceUtil.returnError("product not found for ID: " + productId);
                }
            }
            return generateProductAlternativeUrls(dctx, context, product, 
                    replaceExisting, removeOldLocales, moment, doChildProducts, includeVariant, useCache);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+message, module);
            Map<String, Object> result = ServiceUtil.returnError(message);
            // NOTE: this is simplified by using only one transaction; otherwise would have real numbers here
            result.put("numUpdated", 0);
            result.put("numSkipped", 0);
            result.put("numError", 1);
            return result;
        }
    }

    static Map<String, Object> generateProductAlternativeUrls(DispatchContext dctx, Map<String, ?> context,
            GenericValue product, boolean replaceExisting, boolean removeOldLocales, Timestamp moment, boolean doChildProducts, boolean includeVariant, boolean useCache) throws Exception {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        
        // WARN/TODO?: this service does not run for variant or other child products
        
        String productId = product.getString("productId");
        
        // lookup existing alternative url
        GenericValue productContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductContent")
                .where("productId", productId, "productContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(useCache).queryList();
        if (UtilValidate.isNotEmpty(altUrlContent)) {
            if (!replaceExisting) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("numUpdated", (int) 0);
                result.put("numSkipped", (int) 1);
                result.put("numError", (int) 0);
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
                .where("productId", productId, "productContentTypeId", productNameField).filterByDate(moment).cache(useCache).queryFirst();
        if (nameProductContent == null) {
            // CHECK VIRTUAL PRODUCT for source texts - see product content wrapper
            if ("Y".equals(product.getString("isVariant"))) {
                GenericValue parent = ProductWorker.getParentProduct(productId, delegator, useCache);
                if (parent != null) {
                    nameProductContent = EntityQuery.use(delegator).from("ProductContent")
                            .where("productId", parent.getString("productId"), "productContentTypeId", productNameField).filterByDate(moment).cache(useCache).queryFirst();
                }
            }
        }
        if (nameProductContent != null) {
            nameMainContent = nameProductContent.getRelatedOne("Content", useCache);
            localeTextMap = getSimpleTextsByLocaleString(delegator, dispatcher, 
                    nameMainContent, moment, useCache);
            defaultLocaleString = nameMainContent.getString("localeString"); // TODO: REVIEW: setting this to the main name record localeString, or null if none
        }
        String defaultName = determineDefaultName(delegator, dispatcher, productId, 
                ProductContentWrapper.getEntityFieldValue(product, productNameField, delegator, dispatcher, useCache),
                defaultLocaleString, nameMainContent, localeTextMap); 
        
        // make seo names
        CatalogAltUrlSanitizer sanitizer = getCatalogAltUrlSanitizer(dctx, context);
        Map<String, String> localeUrlMap = localeTextMap != null ? sanitizer.convertNamesToDbAltUrls(localeTextMap, CatalogUrlType.PRODUCT) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = sanitizer.convertNameToDbAltUrl(defaultName, sanitizer.parseLocale(defaultLocaleString), CatalogUrlType.PRODUCT);
        
        // store 
        if (productContent != null) {
            mainContent = productContent.getRelatedOne("Content", useCache);
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
        
        int numUpdated = 1;
        int numSkipped = 0;
        int numError = 0;
        
        if (doChildProducts) {
            if (includeVariant && "Y".equals(product.getString("isVirtual"))) {
                List<GenericValue> variantAssocList = EntityQuery.use(dctx.getDelegator()).from("ProductAssoc")
                        .where("productId", productId, "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().cache(useCache).queryList();
                if (variantAssocList.size() > 0) {
                    if (Debug.infoOn()) Debug.logInfo(logPrefix+"generateProductAlternativeUrls: virtual product '" + productId 
                            + "' has " + variantAssocList.size() + " variants for alternative URL generation", module);
                                        
                    for(GenericValue variantAssoc : variantAssocList) {
                        GenericValue variantProduct = variantAssoc.getRelatedOne("AssocProduct", useCache);
                        
                        Map<String, Object> childResult = generateProductAlternativeUrls(dctx, context, variantProduct, replaceExisting, removeOldLocales, 
                                moment, doChildProducts, includeVariant, useCache);
                        
                        // NOTE: most of this
                        Integer childrenUpdated = (Integer) childResult.get("numUpdated");
                        Integer childrenSkipped = (Integer) childResult.get("numSkipped");
                        Integer childrenError = (Integer) childResult.get("numError");
                        if (childrenUpdated != null) numUpdated += childrenUpdated;
                        if (childrenSkipped != null) numSkipped += childrenSkipped;
                        if (ServiceUtil.isSuccess(childResult)) {
                            if (childrenError != null) numError += childrenError;
                        } else {
                            if (childrenError != null) numError += childrenError;
                            else numError++; // count could be missing in error case
                        }
                    }
                }
            }
            // TODO?: handle other child product associations here? (non-virtual/variant)
        }
        
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("mainContentId", mainContent.getString("contentId"));
        result.put("numUpdated", numUpdated);
        result.put("numSkipped", numSkipped);
        result.put("numError", numError);
        return result;
    }
    
    /**
     * SCIPIO: Re-generates alternative urls for category based on the ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateProductCategoryAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        
        boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting"));
        boolean removeOldLocales = !Boolean.FALSE.equals(context.get("removeOldLocales"));
        Timestamp moment = UtilDateTime.nowTimestamp();
        final boolean useCache = false; // probably bad idea
        
        GenericEntity productCategoryEntity = (GenericEntity) context.get("productCategory");
        try {
            GenericValue productCategory;
            if (productCategoryEntity instanceof GenericValue) {
                productCategory = (GenericValue) productCategoryEntity;
            } else {
                String productCategoryId = (productCategoryEntity != null) ? productCategoryEntity.getString("productCategoryId") : (String) context.get("productCategoryId");
                productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", productCategoryId), useCache);
                if (productCategory == null) {
                    return ServiceUtil.returnError("category not found for ID: " + productCategoryId);
                }
            }
            return generateProductCategoryAlternativeUrls(dctx, context, 
                    productCategory, replaceExisting, removeOldLocales, moment, useCache);
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+message, module);
            return ServiceUtil.returnError(message);
        }
    }
    
    static Map<String, Object> generateProductCategoryAlternativeUrls(DispatchContext dctx, Map<String, ?> context,
            GenericValue productCategory, boolean replaceExisting, boolean removeOldLocales, Timestamp moment, boolean useCache) throws Exception {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String productCategoryId = productCategory.getString("productCategoryId");
        
        // lookup existing alternative url
        GenericValue productCategoryContent = null;
        List<GenericValue> altUrlContent = EntityQuery.use(delegator).from("ProductCategoryContent")
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", "ALTERNATIVE_URL").filterByDate().cache(useCache).queryList();
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
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", categoryNameField).filterByDate(moment).cache(useCache).queryFirst();
        if (nameProductCategoryContent != null) {
            nameMainContent = nameProductCategoryContent.getRelatedOne("Content", useCache);
            localeTextMap = getSimpleTextsByLocaleString(delegator, dispatcher, 
                    nameMainContent, moment, useCache);
            defaultLocaleString = nameMainContent.getString("localeString"); // TODO: REVIEW: setting this to the main name record localeString, or null if none
        }
        String defaultName = determineDefaultName(delegator, dispatcher, productCategoryId, 
                CategoryContentWrapper.getEntityFieldValue(productCategory, categoryNameField, delegator, dispatcher, useCache),
                defaultLocaleString, nameMainContent, localeTextMap); 
        
        // make seo names
        CatalogAltUrlSanitizer sanitizer = getCatalogAltUrlSanitizer(dctx, context);
        Map<String, String> localeUrlMap = localeTextMap != null ? sanitizer.convertNamesToDbAltUrls(localeTextMap, CatalogUrlType.CATEGORY) : Collections.<String, String>emptyMap();
        String defaultLocaleUrl = sanitizer.convertNameToDbAltUrl(defaultName, sanitizer.parseLocale(defaultLocaleString), CatalogUrlType.CATEGORY);
        
        // store
        if (productCategoryContent != null) {
            mainContent = productCategoryContent.getRelatedOne("Content", useCache);
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
        //Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
       
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        if (typeGenerate == null) typeGenerate = Collections.emptyList();
        boolean doAll = typeGenerate.contains("all");
        boolean doProduct = doAll || typeGenerate.contains("product");
        boolean doCategory = doAll || typeGenerate.contains("category");
        
        // NOTE: we must enable search for child product in this case, because they are not automatically
        // covered by ProductCategoryMember in the iteration
        final boolean doChildProducts = true;
        
        String webSiteId = (String) context.get("webSiteId");
        String productStoreId = (String) context.get("productStoreId");
        String prodCatalogId = (String) context.get("prodCatalogId");
        if ("all".equals(prodCatalogId)) prodCatalogId = null; // legacy compat
        Collection<String> prodCatalogIdList = UtilGenerics.checkCollection(context.get("prodCatalogIdList"));
        
        boolean useCache = Boolean.TRUE.equals(context.get("useCache")); // FALSE default
        boolean preventDuplicates = !Boolean.FALSE.equals(context.get("preventDuplicates"));
        
        SeoCatalogUrlGenerator traverser;
        try {
            GenTraversalConfig travConfig = (GenTraversalConfig) new GenTraversalConfig()
                    .setServCtxOpts(context)
                    .setDoChildProducts(doChildProducts)
                    .setUseCache(useCache).setDoCategory(doCategory).setDoProduct(doProduct)
                    .setPreventDupAll(preventDuplicates);
            traverser = new SeoCatalogUrlGenerator(dctx.getDelegator(), dctx.getDispatcher(), travConfig);
        } catch (Exception e) {
            String message = "Error preparing to generate alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+"generateWebsiteAlternativeUrls: "+message, module);
            return ServiceUtil.returnError(message);
        }
        
        try {
            List<GenericValue> prodCatalogList = traverser.getTargetCatalogList(prodCatalogId, prodCatalogIdList, 
                    productStoreId, webSiteId, false);
            traverser.traverseCatalogsDepthFirst(prodCatalogList);
        } catch(Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+"generateWebsiteAlternativeUrls: "+message, module);
            return ServiceUtil.returnError(message);
        }
        
        String resultMsg = traverser.getStats().toMsg(locale);
        Debug.logInfo(logPrefix+"generateWebsiteAlternativeUrls: Generated alternative links: " + resultMsg, module);
        return traverser.getStats().toServiceResultSuccessFailure(resultMsg);
    }
    
    /**
     * Re-generates alternative urls for all stores/websites based on ruleset outlined in SeoConfig.xml.
     */
    public static Map<String, Object> generateAllAlternativeUrls(DispatchContext dctx, Map<String, ? extends Object> context) {
        //Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        
        Collection<String> typeGenerate = UtilGenerics.checkCollection(context.get("typeGenerate"));
        if (typeGenerate == null) typeGenerate = Collections.emptyList();
        boolean doAll = typeGenerate.contains("all");
        boolean doProduct = doAll || typeGenerate.contains("product");
        boolean doCategory = doAll || typeGenerate.contains("category");
        
        // NOTE: don't search for child product in this case, because using massive all-products query
        final boolean doChildProducts = false;
        
        boolean useCache = Boolean.TRUE.equals(context.get("useCache")); // FALSE default
        
        //boolean replaceExisting = !Boolean.FALSE.equals(context.get("replaceExisting")); // makeValidContext transfers

        SeoCatalogUrlGenerator traverser;
        try {
            GenTraversalConfig travConfig = (GenTraversalConfig) new GenTraversalConfig()
                    .setServCtxOpts(context)
                    .setDoChildProducts(doChildProducts)
                    .setUseCache(useCache).setDoCategory(doCategory).setDoProduct(doProduct);
            traverser = new SeoCatalogUrlGenerator(dctx.getDelegator(), dctx.getDispatcher(), travConfig);
        } catch (Exception e) {
            String message = "Error preparing to generate alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+"generateAllAlternativeUrls: "+message, module);
            return ServiceUtil.returnError(message);
        }

        try {
            traverser.traverseAllInSystem();
        } catch (Exception e) {
            String message = "Error while generating alternative links: " + e.getMessage();
            Debug.logError(e, logPrefix+"generateAllAlternativeUrls: " + message, module);
            return ServiceUtil.returnError(message);
        }
        
        // TODO?: FUTURE
        //if (doContent) {
        //}
        
        String resultMsg = traverser.getStats().toMsg(locale);
        Debug.logInfo(logPrefix+"generateAllAlternativeUrls: Generated alternative links: " + resultMsg, module);
        return traverser.getStats().toServiceResultSuccessFailure(resultMsg);
    }
    
}
