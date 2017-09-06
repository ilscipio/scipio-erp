package com.ilscipio.solr;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceDispatcher;

/**
 * Product utility class for solr.
 */
public abstract class ProductUtil {
    public static final String module = ProductUtil.class.getName();

    /**
     * Maps a few simple Product entity fields to solr product fields.
     * NOTE: not all fields available this way, because they don't correspond exactly.
     * NOTE: these have no relation to the addToSolrIndex interface.
     */
    public static final Map<String, String> PRODSIMPLEFIELDMAP_ENTITY_TO_SOLR;
    /**
     * Maps a few simple Solr product fields to Product entity fields.
     * NOTE: not all fields available this way, because they don't correspond exactly.
     * NOTE: these have no relation to the addToSolrIndex interface.
     */
    public static final Map<String, String> PRODSIMPLEFIELDMAP_SOLR_TO_ENTITY;
    static {
        Map<String, String> map = new HashMap<>();
        
        map.put("productId", "productId");
        map.put("internalName", "internalName");
        map.put("smallImageUrl", "smallImageUrl");
        map.put("mediumImage", "mediumImage");
        map.put("largeImage", "largeImage");
        map.put("inStock", "inStock");
        map.put("isVirtual", "isVirtual");
        map.put("isVariant", "isVariant");
        // NOT REAL Product ENTITY FIELDS
        //map.put("defaultPrice", "defaultPrice");
        //map.put("listPrice", "listPrice");
        
        PRODSIMPLEFIELDMAP_ENTITY_TO_SOLR = Collections.unmodifiableMap(map);
        
        Map<String, String> solrEntMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            solrEntMap.put(entry.getValue(), entry.getKey());
        }
        PRODSIMPLEFIELDMAP_SOLR_TO_ENTITY = Collections.unmodifiableMap(solrEntMap);
    }
    
    /**
     * Cached names of solrProductAttributesSimple service interface field names.
     * DEV NOTE: there used to be a hardcoded list here (well, in SolrUtil), 
     * but it was out of sync with the service params; this should help prevent that.
     * TODO: REVIEW: ideally might want to get rid of this third layer of naming...
     */
    private static List<String> solrProdAttrSimple = null;
    
    public static String getProductSolrFieldNameFromEntity(String entityFieldName, Locale locale) {
        if (entityFieldName == null) return null;
        else if ("productName".equals(entityFieldName)) return "title_i18n_" + locale.toString();
        else if ("description".equals(entityFieldName)) return "description_i18n_" + locale.toString();
        else if ("longDescription".equals(entityFieldName)) return "longdescription_i18n_" + locale.toString();
        return PRODSIMPLEFIELDMAP_ENTITY_TO_SOLR.get(entityFieldName);
    }
    
    public static String getProductEntityFieldNameFromSolr(String solrFieldName, Locale locale) {
        if (solrFieldName == null) return null;
        else if (solrFieldName.startsWith("title_i18n_")) return "productName";
        else if (solrFieldName.startsWith("description_i18n_")) return "description";
        else if (solrFieldName.startsWith("longdescription_i18n_")) return "longDescription";
        else return PRODSIMPLEFIELDMAP_SOLR_TO_ENTITY.get(solrFieldName);
    }
    
    public static String getProductSolrSortFieldNameFromSolr(String solrFieldName, Locale locale) {
        if (solrFieldName == null) return null;
        else if ("internalName".equals(solrFieldName)) return "alphaNameSort";
        else if (solrFieldName.startsWith("title_i18n_")) return "alphaTitleSort_" + solrFieldName.substring("title_i18n_".length());
        else return solrFieldName;
    }
    
    /**
     * SPECIAL sort expressions needed to fill in locales with missing texts.
     * WARN: Locales must already be normalized
     */
    public static String makeProductSolrSortFieldExpr(String solrFieldName, Locale locale, Locale fallbackLocale) {
        // SOLVED AT indexing time
        return solrFieldName;
//        if (solrFieldName == null) return null;
//        else if (solrFieldName.startsWith("title_i18n_")) solrFieldName = "title_i18n_";
//        else if (solrFieldName.startsWith("description_i18n_")) solrFieldName = "description_i18n_";
//        else if (solrFieldName.startsWith("longdescription_i18n_")) solrFieldName = "longdescription_i18n_";
//        else if (solrFieldName.startsWith("alphaTitleSort_")) solrFieldName = "alphaTitleSort_";
//        else return solrFieldName;
//        
//        List<String> fieldNames = new ArrayList<>();
//        if (locale != null) fieldNames.add(solrFieldName + locale.toString());
//        if (fallbackLocale != null && (locale == null || !locale.toString().equals(fallbackLocale.toString()))) fieldNames.add(solrFieldName + fallbackLocale.toString());
//        fieldNames.add(solrFieldName + "general");
//        return SolrExprUtil.makeSortFieldFallbackExpr(fieldNames);
    }
    
    public static String getProductSolrPriceFieldNameFromEntityPriceType(String productPriceTypeId, Locale locale, String logPrefix) {
        if ("LIST_PRICE".equals(productPriceTypeId)) {
            return "listPrice";
        } else {
            if (!"DEFAULT_PRICE".equals(productPriceTypeId) && logPrefix != null) {
                Debug.logWarning(logPrefix + "Requested sort price type '" + productPriceTypeId + "' " +
                        "is not supported in current solr product schema; using defaultPrice (DEFAULT_PRICE) instead", module);
            }
            return "defaultPrice";
        }
    }
    
    private static ModelService getModelServiceStaticSafe(String serviceName) {
        try {
            LocalDispatcher dispatcher = ServiceDispatcher.getLocalDispatcher("default", DelegatorFactory.getDelegator("default"));
            return dispatcher.getDispatchContext().getModelService(serviceName);
        } catch(Exception e) {
            Debug.logFatal(e, "Solr: Fatal Error: could not find " + serviceName + " service - solr will fail: " + e.getMessage(), module);
            return null;
        }
    }
    
    /**
     * Generates a map of product content that may be passed to the addToSolrIndex service.
     * NOTE: the result field names match the addToSolrIndex service fields, NOT the 
     * Solr schema product fields; these are extra intermediates.
     * DEV NOTE: FIXME: this extra layer of renaming is confusing and problematic; should get rid of it...
     */
    public static Map<String, Object> getProductContent(GenericValue product, DispatchContext dctx, Map<String, Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String productId = (String) product.get("productId");
        Map<String, Object> dispatchContext = new HashMap<String, Object>();

        if (Debug.verboseOn()) Debug.logVerbose("Solr: Getting product content for productId '" + productId + "'", module);

        try {
            String productStoreId = null; // FIXME?: this will be needed at some point to determine default locales needed
            
            // Generate special ProductContentWrapper for the supported languages
            // FIXME?: ideally this should be configured per-store... 
            Map<String, ProductContentWrapper> pcwMap = new HashMap<>();
            List<Locale> locales = SolrLocaleUtil.getConfiguredLocales(delegator, productStoreId);
            
            List<ProductContentWrapper> pcwList = new ArrayList<>(locales.size());
            for(Locale locale : locales) {
                ProductContentWrapper pcw = new ProductContentWrapper(dispatcher, product, locale, null);
                pcwMap.put(locale.toString(), pcw);
                pcwList.add(pcw);
            }
            
            // FIXME: this should REALLY be configured per-store...
            // but looking up the ProductStore for Product is inexact and slow...
            Locale defLocale = Locale.getDefault();
            
            if (productId != null) {
                dispatchContext.put("productId", productId);
                // if (product.get("sku") != null) dispatchContext.put("sku", product.get("sku"));
                if (product.get("internalName") != null)
                    dispatchContext.put("internalName", product.get("internalName"));
                if (product.get("productTypeId") != null)
                    dispatchContext.put("productTypeId", product.get("productTypeId"));
                // GenericValue manu = product.getRelatedOneCache("Manufacturer");
                // if (product.get("manu") != null) dispatchContext.put("manu", "");
                String smallImage = (String) product.get("smallImageUrl");
                if (smallImage != null)
                    dispatchContext.put("smallImage", smallImage);
                String mediumImage = (String) product.get("mediumImageUrl");
                if (mediumImage != null)
                    dispatchContext.put("mediumImage", mediumImage);
                String largeImage = (String) product.get("largeImageUrl");
                if (largeImage != null)
                    dispatchContext.put("largeImage", largeImage);                
                
                // if(product.get("weight") != null) dispatchContext.put("weight", "");

                // Trying to set a correctand trail
                List<GenericValue> category = delegator.findList("ProductCategoryMember", EntityCondition.makeCondition(UtilMisc.toMap("productId", productId)), null, null, null, false);
                List<String> trails = new ArrayList<String>();
                for (Iterator<GenericValue> catIterator = category.iterator(); catIterator.hasNext();) {
                    GenericValue cat = (GenericValue) catIterator.next();
                    String productCategoryId = (String) cat.get("productCategoryId");
                    List<List<String>> trailElements = CategoryUtil.getCategoryTrail(productCategoryId, dctx);
                    for (List<String> trailElement : trailElements) {
                        StringBuilder catMember = new StringBuilder();
                        int i = 0;
                        Iterator<String> trailIter = trailElement.iterator();
                       
                        while (trailIter.hasNext()) {
                            String trailString = (String) trailIter.next();
                            if (catMember.length() > 0){
                                catMember.append("/");
                                i++;
                            }
                            catMember.append(trailString);
                            String cm = i +"/"+ catMember.toString();
                            if (!trails.contains(cm)) {
                                //Debug.logInfo("Solr: getProductContent: cm: " + cm, module);
                                trails.add(cm);
                            }
                        }
                        
                    }
                }
                dispatchContext.put("category", trails);

                // Get the catalogs that have associated the categories
                List<String> catalogs = new ArrayList<>();
                for (String trail : trails) {
                    String productCategoryId = (trail.split("/").length > 0) ? trail.split("/")[1] : trail;
                    List<String> catalogMembers = CategoryUtil.getCatalogIdsByCategoryId(delegator, productCategoryId);
                    for (String catalogMember : catalogMembers)
                        if (!catalogs.contains(catalogMember))
                            catalogs.add(catalogMember);
                }
                dispatchContext.put("catalog", catalogs);

                // Alternative
                // if(category.size()>0) dispatchContext.put("category", category);
                // if(product.get("popularity") != null) dispatchContext.put("popularity", "");

                Map<String, Object> featureSet = dispatcher.runSync("getProductFeatureSet", UtilMisc.toMap("productId", productId, "emptyAction", "success"));
                if (featureSet != null) {
                    dispatchContext.put("features", (Set<?>) featureSet.get("featureSet"));
                }

                Map<String, Object> productInventoryAvailable = dispatcher.runSync("getProductInventoryAvailable", UtilMisc.toMap("productId", productId));
                String inStock = null;
                BigDecimal availableToPromiseTotal = (BigDecimal) productInventoryAvailable.get("availableToPromiseTotal");
                if (availableToPromiseTotal != null) {
                    inStock = availableToPromiseTotal.toBigInteger().toString();
                }
                dispatchContext.put("inStock", inStock);

                Boolean isVirtual = ProductWorker.isVirtual(delegator, productId);
                if (isVirtual)
                    dispatchContext.put("isVirtual", isVirtual);
                Boolean isVariant = ProductWorker.isVariant(delegator, productId);
                if (isVariant) // new 2017-08-17
                    dispatchContext.put("isVariant", isVariant); 
                Boolean isDigital = ProductWorker.isDigital(product);
                if (isDigital)
                    dispatchContext.put("isDigital", isDigital);
                Boolean isPhysical = ProductWorker.isPhysical(product);
                if (isPhysical)
                    dispatchContext.put("isPhysical", isPhysical);

                dispatchContext.put("title", getLocalizedContentStringMap(delegator, dispatcher, product, "PRODUCT_NAME", locales, defLocale, pcwList));
                dispatchContext.put("description", getLocalizedContentStringMap(delegator, dispatcher, product, "DESCRIPTION", locales, defLocale, pcwList));
                dispatchContext.put("longDescription", getLocalizedContentStringMap(delegator, dispatcher, product, "LONG_DESCRIPTION", locales, defLocale, pcwList));

                // dispatchContext.put("comments", "");
                // dispatchContext.put("keywords", "");
                // dispatchContext.put("last_modified", "");

                // this is the currencyUomId that the prices in solr should use...
                // FIXME: if null, calculateProductPrice is currently reading this from general.properties ALWAYS;
                // the stored data may not match this!
                // _may_ be causing problems reading prices...
                String currencyUomId = null;
                
                if (product != null && "AGGREGATED".equals(product.getString("productTypeId"))) {
                    // FIXME: locale should be looked up differently, but shouldn't have any impacts to price selection...
                    //Locale priceConfigLocale = new Locale("de_DE");
                    //Locale priceConfigLocale = (Locale) context.get("locale");
                    Locale priceConfigLocale = SolrLocaleUtil.getConfiguredDefaultLocale(delegator, productStoreId);
                    
                    ProductConfigWrapper configWrapper = new ProductConfigWrapper(delegator, dispatcher, productId, null, null, null, currencyUomId, priceConfigLocale, userLogin);
                    configWrapper.setDefaultConfig(); // 2017-08-22: if this is not done, the price will always be zero
                    BigDecimal listPrice = configWrapper.getTotalListPrice();
                    // 2017-08-22: listPrice is NEVER null here - getTotalListPrice returns 0 if there was no list price - and 
                    // this creates 0$ list prices we can't validate in queries; this logic requires an extra check + ofbiz patch
                    //if (listPrice != null) {
                    if (listPrice != null && ((listPrice.compareTo(BigDecimal.ZERO) != 0) || configWrapper.hasOriginalListPrice())) {
                        dispatchContext.put("listPrice", listPrice.setScale(2, BigDecimal.ROUND_HALF_DOWN).toString());
                    }
                    BigDecimal defaultPrice = configWrapper.getTotalPrice();
                    if (defaultPrice != null) {
                        dispatchContext.put("defaultPrice", defaultPrice.setScale(2, BigDecimal.ROUND_HALF_DOWN).toString());
                    }
                } else {
                    Map<String, Object> priceContext = UtilMisc.toMap("product", product);
                    priceContext.put("currencyUomId", currencyUomId);
                    SolrProductSearch.copyStdServiceFieldsNotSet(context, priceContext);
                    Map<String, Object> priceMap = dispatcher.runSync("calculateProductPrice", priceContext);
                    if (priceMap.get("listPrice") != null) {
                        String listPrice = ((BigDecimal) priceMap.get("listPrice")).setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                        dispatchContext.put("listPrice", listPrice);
                    }
                    if (priceMap.get("defaultPrice") != null) {
                        String defaultPrice = ((BigDecimal) priceMap.get("defaultPrice")).setScale(2, BigDecimal.ROUND_HALF_DOWN).toString();
                        if (defaultPrice != null)
                            dispatchContext.put("defaultPrice", defaultPrice);
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, e.getMessage(), module);
        } catch (Exception e) {
            Debug.logError(e, e.getMessage(), module);
        }
        return dispatchContext;
    }
    
    private static Map<String, String> getLocalizedContentStringMap(Delegator delegator, LocalDispatcher dispatcher, GenericValue product, 
            String productContentTypeId, List<Locale> locales, Locale defLocale, List<ProductContentWrapper> pcwList) throws GeneralException, IOException {
        Map<String, String> contentMap = new HashMap<>();
        contentMap.put("general", ProductContentWrapper.getEntityFieldValue(product, productContentTypeId, delegator, dispatcher, false)); // NEW 2017-08-21 - handled by addLocalizedContentStringMapToSolrDoc
        
        Map<String, String> localizedContent = getProductContentForLocales(delegator, dispatcher, product, productContentTypeId, locales, false);
        contentMap.putAll(localizedContent);
        
//        for(ProductContentWrapper productContent : pcwList) {
            // 2017-09-05: this creates problems for the language parsing due to the ProductContentWrapper impl.
//            String value = productContent.get(productContentTypeId);
//            Locale locale = productContent.getLocale();
//            if (value != null) {
//                contentMap.put(locale.toString(), value);
//            } else {
//                // currently no real fallback for this
////                if (SolrUtil.SOLR_CONTENT_LOCALES_REQUIREALL || locale.equals(defLocale)) {
////                    contentMap.put(locale.toString(), noLocaleValue);
////                }
//                // FIXME?: the productContent.get already gets the field name... so this is probably never called...
//                // it's possible we don't really want the ProductContentWrapper behavior here...
//                contentMap.put(locale.toString(), noLocaleValue);
//            }
//        }
        return contentMap;
    }
    
    /**
     * Based on a mix of
     * {@link org.ofbiz.product.product.ProductContentWrapper#getProductContentAsText(String, GenericValue, String, Locale, String, String, String, Delegator, LocalDispatcher, Writer)}
     * and
     * {@link org.ofbiz.content.content.ContentWorker#findContentForRendering(Delegator, String, Locale, String, String, boolean)}
     * .
     * Unlike ProductContentWrapper, this NEVER returns a fallback language for the locales, and
     * does not consult the entity field - no map entry if there's no text in the given language.
     */
    static Map<String, String> getProductContentForLocales(Delegator delegator, LocalDispatcher dispatcher, GenericValue product, String productContentTypeId, Collection<Locale> locales, boolean useCache) throws GeneralException, IOException {
        Map<String, String> contentMap = new HashMap<>();
        
        String productId = product.getString("productId");
        
        List<GenericValue> productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", productId, "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(useCache).filterByDate().queryList();
        if (UtilValidate.isEmpty(productContentList) && ("Y".equals(product.getString("isVariant")))) {
            GenericValue parent = ProductWorker.getParentProduct(productId, delegator);
            if (UtilValidate.isNotEmpty(parent)) {
                productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", parent.get("productId"), "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(useCache).filterByDate().queryList();
            }
        }
        GenericValue productContent = EntityUtil.getFirst(productContentList);
        if (productContent == null) {
            return contentMap;
        }
        String contentId = productContent.getString("contentId");
        
        GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).cache(useCache).queryOne();
        if (content == null) {
            return contentMap;
        }
        
        String thisLocaleString = (String) content.get("localeString");
        thisLocaleString = (thisLocaleString != null) ? thisLocaleString : "";
        for(Locale locale : locales) {
            String targetLocaleString = locale.toString();
            GenericValue targetContent = null;
            if (targetLocaleString.equalsIgnoreCase(thisLocaleString)) {
                targetContent = content;
            } else {
                // FIXME: useCache can't propagate here!
                GenericValue altContent = ContentWorker.findAlternateLocaleContent(delegator, content, locale);
                if (altContent != null && !contentId.equals(altContent.getString("contentId"))) {
                    targetContent = altContent;
                }
            }
            if (targetContent != null) {
                Writer out = new StringWriter();
                Map<String, Object> inContext = new HashMap<>();
                inContext.put("product", product);
                inContext.put("productContent", productContent);
                ContentWorker.renderContentAsText(dispatcher, delegator, targetContent, out, inContext, locale, "text/plain", false, null);
                String res = out.toString();
                if (res.length() > 0) {
                    contentMap.put(locale.toString(), res);
                }
            }
        }
        return contentMap;
    }
    

    static List<String> getSolrProdAttrSimple() {
        List<String> attrList = solrProdAttrSimple;
        if (attrList == null) {
            ModelService model = getModelServiceStaticSafe("solrProductAttributesSimple");
            if (model != null) attrList = Collections.unmodifiableList(new ArrayList<>(model.getParameterNames(ModelService.IN_PARAM, true, false)));
            else attrList = Collections.emptyList();
            if (Debug.verboseOn()) Debug.logVerbose("Solr: Product attributes simple: " + attrList, module);
            solrProdAttrSimple = attrList;
        }
        return attrList;
    }
    
    /**
     * Generates a Solr schema product from the fields of the solrProductAttributes service interface.
     * DEV NOTE: TODO: REVIEW: the solrProductAttributes interface may be an undesirable intermediate...
     */
    public static SolrInputDocument generateSolrProductDocument(Delegator delegator, LocalDispatcher dispatcher, Map<String, Object> context) throws GenericEntityException {
        SolrInputDocument doc = new SolrInputDocument();
    
        // add defined attributes
        for (String attrName : getSolrProdAttrSimple()) {
            if (context.get(attrName) != null) {
                doc.addField(attrName, context.get(attrName).toString());
            }
        }
    
        addStringValuesToSolrDoc(doc, "catalog", UtilGenerics.<String>checkCollection(context.get("catalog")));
        addStringValuesToSolrDoc(doc, "cat", UtilGenerics.<String>checkCollection(context.get("category")));
        addStringValuesToSolrDoc(doc, "features", UtilGenerics.<String>checkCollection(context.get("features")));
        addStringValuesToSolrDoc(doc, "attributes", UtilGenerics.<String>checkCollection(context.get("attributes")));

        addLocalizedContentStringMapToSolrDoc(delegator, doc, "title_i18n_", "title_i18n_general", UtilGenerics.<String, String>checkMap(context.get("title")));
        addLocalizedContentStringMapToSolrDoc(delegator, doc, "description_i18n_", "description_i18n_general", UtilGenerics.<String, String>checkMap(context.get("description")));
        addLocalizedContentStringMapToSolrDoc(delegator, doc, "longdescription_i18n_", "longdescription_i18n_general", UtilGenerics.<String, String>checkMap(context.get("longDescription")));
    
        // FIXME?: MANUAL population of the alpha sort field, because it's complex
        addAlphaLocalizedContentStringMapToSolrDoc(delegator, doc, "alphaTitleSort_", "alphaTitleSort_general", "title_i18n_", "title_i18n_general", UtilGenerics.<String, String>checkMap(context.get("title")));
        
        return doc;
    }
    
    private static void addStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values) {
        if (values == null) return;
        Iterator<?> attrIter = values.iterator();
        while (attrIter.hasNext()) {
            Object attr = attrIter.next();
            doc.addField(solrFieldName, attr.toString());
        }
    }
    
    private static void addLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc, String solrFieldNamePrefix, String solrDefaultFieldName, Map<String, String> contentMap) {
        if (contentMap == null) return;
        for (Map.Entry<String, String> entry : contentMap.entrySet()) {
            if ("general".equals(entry.getKey())) {
                if (solrDefaultFieldName != null) {
                    doc.addField(solrDefaultFieldName, entry.getValue());
                }
            } else {
                doc.addField(solrFieldNamePrefix + entry.getKey(), entry.getValue());
            }
        }
    }
    
    /**
     * FIXME?: this is a homemade replacements for the removal of 
     * {@code
     *  <copyField source="title_i18n_*" dest="alphaTitleSort_*"/>
     * }
     * in the solr schema; we should really do this with a solr directive, but not set up for that.
     */
    private static void addAlphaLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc, String alphaFieldNamePrefix, String alphaDefaultFieldName, String solrFieldNamePrefix, String solrDefaultFieldName, Map<String, String> contentMap) {
        if (contentMap == null) return;
        
        String generalValue = null;
        if (contentMap.containsKey("general")) {
            generalValue = contentMap.get("general");
            doc.addField(alphaDefaultFieldName, generalValue);
        }
        
        String defLocStr = SolrLocaleUtil.getConfiguredDefaultLocale(delegator).toString();
        
        // fill in ALL the locales
        for(Locale locale : SolrLocaleUtil.getConfiguredLocales(delegator)) {
            String locStr = locale.toString();
            
            String value = contentMap.get(solrFieldNamePrefix + locStr);
            if (UtilValidate.isEmpty(value)) {
                // if no exact locale, use general next, because there's a chance it's in the right language
                value = generalValue;
                if (UtilValidate.isEmpty(value)) {
                    // if there's nothing else, check entry for sys default lang; even though
                    // this is sure to be the wrong language, it's better than nothing for sorting...
                    value = contentMap.get(solrFieldNamePrefix + defLocStr);
                }
            }
            
            doc.addField(alphaFieldNamePrefix + locStr, value);
        }
    }
}