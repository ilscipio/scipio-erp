package com.ilscipio.scipio.solr;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
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
public abstract class SolrProductUtil {
    public static final String module = SolrProductUtil.class.getName();

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
    
    private static final Map<String, Object> defaultUserProductSearchConfig;
    static {
        Map<String, Object> config = null;
        try {
            config = readUserProductSearchConfig(DelegatorFactory.getDelegator("default"), 
                    SolrUtil.solrConfigName, "solr.search.user.");
        } catch(Exception e) {
            Debug.logError(e, "Solr: Could not read default user product search config from solr.properties: " + e.getMessage(), module);
            config = new HashMap<>();
        }
        defaultUserProductSearchConfig = Collections.unmodifiableMap(config); // FIXME: contents are not properly unmodifiable
    }
    
    private static final String configuredFallbackDefaultCurrency = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.content.currency.default.fallback", null);
    private static final String configuredForceDefaultCurrency = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.content.currency.default.force", null);

    
    /**
     * Cached names of solrProductAttributesSimple service interface field names.
     * DEV NOTE: there used to be a hardcoded list here (well, in SolrUtil), 
     * but it was out of sync with the service params; this should help prevent that.
     * TODO: REVIEW: ideally might want to get rid of this third layer of naming...
     */
    private static List<String> solrProdAttrSimple = null;
    
    
    public static String getConfiguredDefaultCurrency(Delegator delegator, GenericValue productStore) {
        if (configuredForceDefaultCurrency != null) return configuredForceDefaultCurrency;
        if (productStore != null) {
            String currency = productStore.getString("defaultCurrencyUomId");
            if (UtilValidate.isNotEmpty(currency)) return currency;
        }
        if (configuredFallbackDefaultCurrency != null) return configuredFallbackDefaultCurrency;
        return EntityUtilProperties.getPropertyValue("general", "currency.uom.id.default", "USD", 
                delegator != null ? delegator : (productStore != null ? productStore.getDelegator() : null));
    }
    
    public static String getConfiguredDefaultCurrency(GenericValue productStore) {
        return getConfiguredDefaultCurrency(null, productStore);
    }
    
    /**
     * NOTE: Locales must already be normalized ({@link SolrLocaleUtil#getCompatibleLocaleValid}).
     */
    public static String getProductSolrFieldNameFromEntity(String entityFieldName, Locale locale) {
        if (entityFieldName == null) return null;
        else if ("productName".equals(entityFieldName)) return "title_i18n_" + SolrLocaleUtil.getLangCode(locale);
        else if ("description".equals(entityFieldName)) return "description_i18n_" + SolrLocaleUtil.getLangCode(locale);
        else if ("longDescription".equals(entityFieldName)) return "longdescription_i18n_" + SolrLocaleUtil.getLangCode(locale);
        return PRODSIMPLEFIELDMAP_ENTITY_TO_SOLR.get(entityFieldName);
    }
    
    /**
     * NOTE: Locales must already be normalized ({@link SolrLocaleUtil#getCompatibleLocaleValid}).
     */
    public static String getProductEntityFieldNameFromSolr(String solrFieldName, Locale locale) {
        if (solrFieldName == null) return null;
        else if (solrFieldName.startsWith("title_i18n_")) return "productName";
        else if (solrFieldName.startsWith("description_i18n_")) return "description";
        else if (solrFieldName.startsWith("longdescription_i18n_")) return "longDescription";
        else return PRODSIMPLEFIELDMAP_SOLR_TO_ENTITY.get(solrFieldName);
    }
    
    /**
     * NOTE: Locales must already be normalized ({@link SolrLocaleUtil#getCompatibleLocaleValid}).
     */
    public static String getProductSolrSortFieldNameFromSolr(String solrFieldName, Locale locale) {
        if (solrFieldName == null) return null;
        else if ("internalName".equals(solrFieldName)) return "alphaNameSort";
        else if (solrFieldName.startsWith("title_i18n_")) return "alphaTitleSort_" + solrFieldName.substring("title_i18n_".length());
        else return solrFieldName;
    }
    
    /**
     * SPECIAL sort expressions needed to fill in locales with missing texts.
     * NOTE: Locales must already be normalized ({@link SolrLocaleUtil#getCompatibleLocaleValid}).
     * NOTE: 2017-09-14: This currently does nothing, because is solved at indexing time in current schema;
     * the field returned by {@link #getProductSolrSortFieldNameFromSolr} will end up working as-is, but leaving
     * this here as a factoring point.
     */
    public static String makeProductSolrSortFieldExpr(String solrFieldName, Locale locale, Locale fallbackLocale) {
        // solved at indexing time instead in current schema
        return solrFieldName;
//        if (solrFieldName == null) return null;
//        else if (solrFieldName.startsWith("title_i18n_")) solrFieldName = "title_i18n_";
//        else if (solrFieldName.startsWith("description_i18n_")) solrFieldName = "description_i18n_";
//        else if (solrFieldName.startsWith("longdescription_i18n_")) solrFieldName = "longdescription_i18n_";
//        else if (solrFieldName.startsWith("alphaTitleSort_")) solrFieldName = "alphaTitleSort_";
//        else return solrFieldName;
//        
//        List<String> fieldNames = new ArrayList<>();
//        if (locale != null) fieldNames.add(solrFieldName + SolrLocaleUtil.getLangCode(locale));
//        if (fallbackLocale != null && (locale == null || !SolrLocaleUtil.isSameLangCode(locale, fallbackLocale))) fieldNames.add(solrFieldName + SolrLocaleUtil.getLangCode(fallbackLocale));
//        fieldNames.add(solrFieldName + I18N_FIELD_GENERAL);
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
    
    public static Map<String, Object> getDefaultUserProductSearchConfig(GenericValue productStore) {
        return defaultUserProductSearchConfig;
    }
    
    /**
     * Returns set of query fields (with optional power expressions, e.g. ^2) for user/public searches
     * appropriate for locale and store and according to config, to use instead of the default search field ("text").
     * FOR USE WITH USER MODE AND EDISMAX ONLY
     * <p>
     * Automatically normalizes the locales.
     * 
     * @see #readUserProductSearchConfig for config
     */
    public static Set<String> determineUserProductSearchQueryFields(Delegator delegator, Locale userLocale, GenericValue productStore, Map<String, Object> config) {
        setDefaultsUserProductSearchConfig(config, getDefaultUserProductSearchConfig(productStore));
        
        String i18nFieldsSelect = (String) config.get("i18nFieldsSelect");
        Collection<Locale> i18nForceLocales = UtilGenerics.checkCollection(config.get("i18nForceLocales"));
        String userLocalePower = (String) config.get("userLocalePower");
        Collection<String> commonFields = UtilGenerics.checkCollection(config.get("product.commonFields"));
        
        Set<String> queryFields = null;
        if (UtilValidate.isNotEmpty(i18nFieldsSelect)) {
            if ("user-store".equals(i18nFieldsSelect)) {
                // FIXME: Set strings are not check for power e.g. ^2 suffix
                queryFields = SolrLocaleUtil.determineI18nQueryFieldsForUserLocale(userLocale, productStore, true, i18nForceLocales, "text_i18n_", userLocalePower, null, null);
                if (commonFields != null) queryFields.addAll(commonFields);
            } else if ("user".equals(i18nFieldsSelect)) {
                queryFields = SolrLocaleUtil.determineI18nQueryFieldsForUserLocale(userLocale, productStore, false, i18nForceLocales, "text_i18n_", userLocalePower, null, null);
                
            } else if ("all".equals(i18nFieldsSelect)) {
                queryFields = new LinkedHashSet<>();
                queryFields.add("text");
            }
            if (queryFields != null) {
                if (commonFields != null) queryFields.addAll(commonFields);
            } else {
                if (!"NONE".equals(i18nFieldsSelect)) {
                    Debug.logError("Solr: Unrecognized i18nFieldsSelect config value (may be from properties or screen code): " 
                            + i18nFieldsSelect + "; using \"NONE\" instead", module);
                }
                // return nothing; indicates use default and don't use qf param
                queryFields = new LinkedHashSet<>();
            }
        } else { // "NONE"
            // return nothing; indicates use default and don't use qf param
            queryFields = new LinkedHashSet<>();
        }
        return queryFields;
    }
    
    /**
     * Reads common solr user search options from a properties file.
     * Example/Reference: Scipio shop.properties values having "shop.search.solr." prefix.
     */
    public static Map<String, Object> readUserProductSearchConfig(Delegator delegator, String propResource, String propNamePrefix, boolean parseCfg) {
        Map<String, Object> config = new HashMap<>();
        // TODO: optimize
        config.put("queryType", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "queryType", null, delegator));
        config.put("i18nFieldsSelect", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "i18nFieldsSelect", null, delegator));
        config.put("i18nForceLocales", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "i18nForceLocales", null, delegator));
        config.put("userLocalePower", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "userLocalePower", null, delegator));
        config.put("product.commonFields", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "product.commonFields", null, delegator));
        
        if (parseCfg) parseUserProductSearchConfig(delegator, config);
        return config;
    }
    
    public static Map<String, Object> readUserProductSearchConfig(Delegator delegator, String propResource, String propNamePrefix) {
        return readUserProductSearchConfig(delegator, propResource, propNamePrefix, true);
    }
    
    /**
     * Reads common user search options from a properties file plus overrides.
     * Example: Scipio shop.properties values having "shop.search.solr." prefix.
     */
    public static Map<String, Object> readUserProductSearchConfig(Delegator delegator, String propResource, String propNamePrefix, Map<String, Object> overrideConfig, boolean parseCfg) {
        Map<String, Object> config = readUserProductSearchConfig(delegator, propResource, propNamePrefix, false);
        if (overrideConfig != null) {
            overrideUserProductSearchConfig(config, overrideConfig);
        }
        if (parseCfg) parseUserProductSearchConfig(delegator, config);
        return config;
    }
    
    public static Map<String, Object> readUserProductSearchConfig(Delegator delegator, String propResource, String propNamePrefix, Map<String, Object> overrideConfig) {
        return readUserProductSearchConfig(delegator, propResource, propNamePrefix, overrideConfig, true);
    }
    
    public static void overrideUserProductSearchConfig(Map<String, Object> destConfig, Map<String, Object> overrideConfig) {
        for(Map.Entry<String, ?> entry : overrideConfig.entrySet()) {
            if (entry.getValue() != null) destConfig.put(entry.getKey(), entry.getValue());
        }
    }
    
    public static void setDefaultsUserProductSearchConfig(Map<String, Object> destConfig, Map<String, Object> defaultsConfig) {
        for(Map.Entry<String, Object> entry : defaultsConfig.entrySet()) {
            if (entry.getValue() != null) {
                Object oldValue = destConfig.get(entry.getKey());
                if (oldValue == null) {
                    destConfig.put(entry.getKey(), entry.getValue());
                }
            }
        }
    }
    
    public static void parseUserProductSearchConfig(Delegator delegator, Map<String, Object> config) {
        Object i18nForceLocalesObj = config.get("i18nForceLocales");
        if (i18nForceLocalesObj instanceof String) {
            Collection<Locale> i18nForceLocales;
            if (((String) i18nForceLocalesObj).length() == 0 || "NONE".equals(i18nForceLocalesObj)) {
                i18nForceLocales = Collections.emptySet();
            } else {
                i18nForceLocales = SolrLocaleUtil.parseCompatibleLocalesValidSpecial((String) i18nForceLocalesObj);
            }
            config.put("i18nForceLocales", i18nForceLocales);
        }

        Object commonFieldsObj = config.get("product.commonFields");
        if (commonFieldsObj instanceof String) {
            Collection<String> commonFields;
            if (((String) commonFieldsObj).length() == 0 || "NONE".equals(commonFieldsObj)) {
                commonFields = Collections.emptySet();
            } else {
                commonFields = Arrays.asList(((String) commonFieldsObj).split("\\s*,\\s*"));
            }
            config.put("product.commonFields", commonFields);
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
     * Clears all Product and related entity caches that are used in Solr indexing.
     * Should be done at beginning of <code>rebuildSolrIndex</code> so that useCache=true can
     * be used during the indexing, which should significantly speed up indexing.
     * FIXME?: broad entity-level clears affects non-product stuff
     */
    public static void clearProductEntityCaches(Delegator delegator, LocalDispatcher dispatcher) {
        try {
            Debug.logInfo("Solr: Clearing product entity caches", module);
            
            for(String entityName: delegator.getModelReader().getEntityNames()) {
                if (entityName.startsWith("Prod")) {
                    String rest = entityName.substring("Prod".length());
                    if (rest.startsWith("uct") || rest.startsWith("Catalog") 
                            || rest.startsWith("ConfItem")) {
                        clearCacheLine(delegator, entityName);
                    }
                }
            }
            clearCacheLine(delegator, "Content");
            clearCacheLine(delegator, "ContentAssoc");
            clearCacheLine(delegator, "DataResource");
            clearCacheLine(delegator, "ElectronicText");
        } catch(Exception e) {
            Debug.logError("Solr: Error trying to clear product entity caches: " + e.getMessage(), module);
        }
    }
    
    private static void clearCacheLine(Delegator delegator, String entityName) {
        if (Debug.verboseOn()) Debug.logVerbose("Solr: Clearing entity cache for: " + entityName, module);
        delegator.clearCacheLine(entityName);
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
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        Map<String, Object> dispatchContext = new HashMap<String, Object>();

        if (Debug.verboseOn()) Debug.logVerbose("Solr: Getting product content for productId '" + productId + "'", module);

        try {
            if (UtilValidate.isEmpty(productId)) { // sanity check
                throw new IllegalArgumentException("Missing productId");
            }
            
            List<GenericValue> productVariantAssocs = ProductWorker.getVariantVirtualAssocs(product, useCache);
            
            // 2017-09: do EARLY cat lookup so that we can find out a ProductStore
            EntityCondition cond = EntityCondition.makeCondition(EntityCondition.makeCondition("productId", productId),
                        EntityOperator.AND,
                        EntityUtil.getFilterByDateExpr());
            List<GenericValue> categories = delegator.findList("ProductCategoryMember", cond, null, null, null, useCache);
            Set<String> productCategoryIds = new LinkedHashSet<>();
            SolrCategoryUtil.addAllStringFieldList(productCategoryIds, categories, "productCategoryId");
            
            // 2017-09: if variant, must also get virtual's categories
            if (UtilValidate.isNotEmpty(productVariantAssocs)) {
                for(GenericValue productVariantAssoc : productVariantAssocs) {
                    String virtualProductId = productVariantAssoc.getString("productId");
                    cond = EntityCondition.makeCondition(EntityCondition.makeCondition("productId", virtualProductId),
                            EntityOperator.AND,
                            EntityUtil.getFilterByDateExpr());
                    List<GenericValue> virtualCategories = delegator.findList("ProductCategoryMember", cond, null, null, null, useCache);
                    SolrCategoryUtil.addAllStringFieldList(productCategoryIds, virtualCategories, "productCategoryId");
                }
            }
            
            // Trying to set a correctand trail
            Collection<String> trails = new LinkedHashSet<String>();
            for (String productCategoryId : productCategoryIds) {
                List<List<String>> trailElements = SolrCategoryUtil.getCategoryTrail(productCategoryId, dctx);
                for (List<String> trailElement : trailElements) {
                    StringBuilder catMember = new StringBuilder();
                    int i = 0;
                    for(String trailString : trailElement) {
                        if (catMember.length() > 0){
                            catMember.append("/");
                            i++;
                        }
                        catMember.append(trailString);
                        String cm = i +"/"+ catMember.toString();
                        trails.add(cm);
                    }
                }
            }
            dispatchContext.put("category", new ArrayList<String>(trails));

            // Get the catalogs that have associated the categories
            Collection<String> catalogs = new LinkedHashSet<>();
            Map<String, List<String>> categoryIdCatalogIdMap = new HashMap<>(); // 2017-09: local cache; multiple lookups for same
            for (String trail : trails) {
                String productCategoryId = (trail.split("/").length > 0) ? trail.split("/")[1] : trail;
                List<String> catalogMembers = categoryIdCatalogIdMap.get(productCategoryId); 
                if (catalogMembers == null) {          
                    catalogMembers = SolrCategoryUtil.getCatalogIdsByCategoryId(delegator, productCategoryId, useCache);
                    categoryIdCatalogIdMap.put(productCategoryId, catalogMembers);
                }
                for (String catalogMember : catalogMembers) {
                    catalogs.add(catalogMember);
                }
            }
            dispatchContext.put("catalog", new ArrayList<>(catalogs));
            
            List<GenericValue> productStores = SolrCategoryUtil.getProductStoresFromCatalogIds(delegator, catalogs, useCache);
            dispatchContext.put("productStore", SolrCategoryUtil.getStringFieldList(productStores, "productStoreId"));
            
            // MAIN STORE SELECTION AND LOCALE LOOKUP
            GenericValue productStore = EntityUtil.getFirst(productStores);
            List<Locale> locales = SolrLocaleUtil.getConfiguredLocales(productStore);
            Locale defaultProductLocale = SolrLocaleUtil.getConfiguredDefaultLocale(productStore);
            
            // Generate special ProductContentWrapper for the supported languages
            Map<String, ProductContentWrapper> pcwMap = new HashMap<>();
            List<ProductContentWrapper> pcwList = new ArrayList<>(locales.size());
            for(Locale locale : locales) {
                ProductContentWrapper pcw = new ProductContentWrapper(dispatcher, product, locale, null);
                pcwMap.put(SolrLocaleUtil.getLangCode(locale), pcw);
                pcwList.add(pcw);
            }
            
            String parentProductId = null;
            if ("Y".equals(product.getString("isVariant"))) {
                // IMPORTANT: same parent lookup logic as used by ProductContentWrapper
                parentProductId = ProductWorker.getParentProductId(productId, delegator, useCache);
            }
            
            // FIXME: this should REALLY be configured per-store...
            // but looking up the ProductStore for Product is inexact and slow...
            Locale defLocale = Locale.getDefault();
            
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

            dispatchContext.put("title", getLocalizedContentStringMap(delegator, dispatcher, product, "PRODUCT_NAME", locales, defLocale, pcwList, useCache));
            dispatchContext.put("description", getLocalizedContentStringMap(delegator, dispatcher, product, "DESCRIPTION", locales, defLocale, pcwList, useCache));
            dispatchContext.put("longDescription", getLocalizedContentStringMap(delegator, dispatcher, product, "LONG_DESCRIPTION", locales, defLocale, pcwList, useCache));

            // dispatchContext.put("comments", "");
            // dispatchContext.put("keywords", "");
            // dispatchContext.put("last_modified", "");

            // this is the currencyUomId that the prices in solr should use...
            String currencyUomId = getConfiguredDefaultCurrency(delegator, productStore);
            
            if (product != null && "AGGREGATED".equals(product.getString("productTypeId"))) {
                Locale priceConfigLocale = defaultProductLocale;
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
            
            // 2017-09-12: added missing ProductKeyword lookup, otherwise can't input keywords from ofbiz
            Set<String> keywords = new LinkedHashSet<>();
            // NOTE: for variant products, we also include the keywords from the virtual/parent
            getProductKeywords(keywords, delegator, useCache, productId, parentProductId);
            dispatchContext.put("keywords", new ArrayList<>(keywords));
        } catch (GenericEntityException e) {
            Debug.logError(e, "Solr: getProductContent: " + e.getMessage(), module);
        } catch (Exception e) {
            Debug.logError(e, "Solr: getProductContent: " + e.getMessage(), module);
        }
        return dispatchContext;
    }
    
    static void getProductKeywords(Collection<String> keywords, Delegator delegator, boolean useCache, String... productIds) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        
        List<EntityCondition> productIdOrList = new ArrayList<>(productIds.length);
        for(String productId : productIds) {
            if (productId != null) productIdOrList.add(EntityCondition.makeCondition("productId", productId));
        }
        condList.add(productIdOrList.size() == 1 ? productIdOrList.get(0) : EntityCondition.makeCondition(productIdOrList, EntityOperator.OR));
        // IMPORTANT: ONLY add keywords IF auto-generation by ofbiz is disabled for the product
        
        // FIXME?: we can only index a subset of cases; we use simplest condition possible; see eecas.xml for details
        //condList.add(EntityCondition.makeCondition("keywordTypeId", KWT_KEYWORD));
//        EntityCondition tagCond = EntityCondition.makeCondition(EntityCondition.makeCondition("keywordTypeId", "KWT_TAG"),
//                EntityOperator.AND,
//                EntityCondition.makeCondition(EntityCondition.makeCondition("statusId", null),
//                        EntityOperator.OR,
//                        EntityCondition.makeCondition("statusId", "KW_APPROVED")));
//        EntityCondition keywordCond = EntityCondition.makeCondition(EntityCondition.makeCondition("keywordTypeId", "KWT_KEYWORD"),
//                EntityOperator.AND,
//                EntityCondition.makeCondition("statusId", "KW_APPROVED")); // DO NOT allow empty status, because it might be auto-generated
//        condList.add(EntityCondition.makeCondition(tagCond, EntityOperator.OR, keywordCond));
        condList.add(EntityCondition.makeCondition("statusId", "KW_APPROVED"));
        List<GenericValue> productKeywords = delegator.findList("ProductKeyword", 
                EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
        for(GenericValue productKeyword : productKeywords) {
            keywords.add(productKeyword.getString("keyword"));
        }
    }
    
    private static Map<String, String> getLocalizedContentStringMap(Delegator delegator, LocalDispatcher dispatcher, GenericValue product, 
            String productContentTypeId, List<Locale> locales, Locale defaultProductLocale, List<ProductContentWrapper> pcwList, boolean useCache) throws GeneralException, IOException {
        Map<String, String> contentMap = new HashMap<>();
        
        contentMap.put(SolrLocaleUtil.I18N_GENERAL, ProductContentWrapper.getEntityFieldValue(product, productContentTypeId, delegator, dispatcher, useCache));
        
        getProductContentForLocales(contentMap, delegator, dispatcher, product, productContentTypeId, locales, defaultProductLocale, useCache);
        
        refineLocalizedContentValues(contentMap, locales, defaultProductLocale);
        
        return contentMap;
    }
    
    /**
     * Refines the map of localized content values (locale->value) by filling in missing values for locales where possible.
     * <p>
     * 2017-09-14: This applies the new behavior where "_i18n_general" and "_i18n_[storedefault]" fields
     * are copied to each other if either one is missing. It assumes the passed defaultProductLocale
     * accurately reflects the language that the general entries are written in - this is
     * normally ProductStore.defaultLocaleString and found using {@link SolrLocaleUtil#getConfiguredDefaultLocale(GenericValue)}.
     * This simplifies queries significantly. See solrconfig.properties and schema.
     * 
     * @see SolrLocaleUtil#getConfiguredDefaultLocale(GenericValue)
     */
    public static void refineLocalizedContentValues(Map<String, String> contentMap, List<Locale> locales, Locale defaultProductLocale) {
        if (defaultProductLocale != null) {
            String generalValue = contentMap.get(SolrLocaleUtil.I18N_GENERAL);
            String defaultLangValue = contentMap.get(SolrLocaleUtil.getLangCode(defaultProductLocale));
            
            if (UtilValidate.isEmpty(defaultLangValue)) {
                if (UtilValidate.isNotEmpty(generalValue)) {
                    contentMap.put(SolrLocaleUtil.getLangCode(defaultProductLocale), generalValue);
                }
            } else if (UtilValidate.isEmpty(generalValue)) {
                if (UtilValidate.isNotEmpty(defaultLangValue)) {
                    contentMap.put(SolrLocaleUtil.I18N_GENERAL, defaultLangValue);
                }
            }
        }
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
    private static void getProductContentForLocales(Map<String, String> contentMap, Delegator delegator, LocalDispatcher dispatcher, 
            GenericValue product, String productContentTypeId, Collection<Locale> locales, Locale defaultProductLocale, boolean useCache) throws GeneralException, IOException {
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
            return;
        }
        String contentId = productContent.getString("contentId");
        
        GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).cache(useCache).queryOne();
        if (content == null) {
            return;
        }
        
        String thisLocaleString = (String) content.get("localeString");
        thisLocaleString = (thisLocaleString != null) ? thisLocaleString : "";
        // special case: no locale string: treat as general (NOTE: here give the EntityFieldValue prio like orig ProductContentWrapper)
        if (thisLocaleString.isEmpty() && UtilValidate.isEmpty((String) contentMap.get(SolrLocaleUtil.I18N_GENERAL))) {
            GenericValue targetContent = content;
            Locale locale = defaultProductLocale;
            String res = getContentText(delegator, dispatcher, targetContent, product, productContent, locale, useCache);
            if (res.length() > 0) {
                contentMap.put(SolrLocaleUtil.getLangCode(locale), res);
            }
        }
        for(Locale locale : locales) {
            String targetLocaleString = SolrLocaleUtil.getLangCode(locale);
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
                String res = getContentText(delegator, dispatcher, targetContent, product, productContent, locale, useCache);
                if (res.length() > 0) {
                    contentMap.put(SolrLocaleUtil.getLangCode(locale), res);
                }
            }
        }
    }
    
    static String getContentText(Delegator delegator, LocalDispatcher dispatcher, GenericValue targetContent, 
            GenericValue product, GenericValue productContent, Locale locale, boolean useCache) throws GeneralException, IOException {
        Writer out = new StringWriter();
        Map<String, Object> inContext = new HashMap<>();
        inContext.put("product", product);
        inContext.put("productContent", productContent);
        ContentWorker.renderContentAsText(dispatcher, delegator, targetContent, out, inContext, locale, "text/plain", useCache, null);
        return out.toString();
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
    public static SolrInputDocument generateSolrProductDocument(Delegator delegator, LocalDispatcher dispatcher, 
            Map<String, Object> context, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        SolrInputDocument doc = new SolrInputDocument();
        
        String productId = (String) context.get("productId");
        if (UtilValidate.isEmpty(productId)) throw new IllegalArgumentException("generateSolrProductDocument: missing productId");
        
        GenericValue productStore = null;
        Collection<String> productStoreIds = asStringCollection(context.get("productStore"));
        if (UtilValidate.isNotEmpty(productStoreIds)) {
            productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", 
                    productStoreIds.iterator().next()), useCache);
        }
        List<Locale> locales = SolrLocaleUtil.getConfiguredLocales(productStore);
        Locale defaultLocale = SolrLocaleUtil.getConfiguredDefaultLocale(productStore);
        
        // add defined attributes
        for (String attrName : getSolrProdAttrSimple()) {
            if (context.get(attrName) != null) {
                doc.addField(attrName, context.get(attrName).toString());
            }
        }
    
        addStringValuesToSolrDoc(doc, "catalog", asStringCollection(context.get("catalog")));
        addStringValuesToSolrDoc(doc, "productStore", productStoreIds);
        addStringValuesToSolrDoc(doc, "cat", asStringCollection(context, "category"));
        addStringValuesToSolrDoc(doc, "features", asStringCollection(context, "features"));
        addStringValuesToSolrDoc(doc, "attributes", asStringCollection(context, "attributes"));
        // TODO: REVIEW: for now concatenating the keywords into one string and letting solr re-split it afterward
        // this could be more efficient storage-wise than using multiValued="true", and it's what the solr demo implies to do,
        // but unclear which is optimal...
        addConcatenatedStringValuesToSolrDoc(doc, "keywords", asStringCollection(context, "keywords"), " ");

        addLocalizedContentStringMapToSolrDoc(delegator, doc, "title_i18n_", "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, UtilGenerics.<String, String>checkMap(context.get("title")));
        addLocalizedContentStringMapToSolrDoc(delegator, doc, "description_i18n_", "description_i18n_"+SolrLocaleUtil.I18N_GENERAL, UtilGenerics.<String, String>checkMap(context.get("description")));
        addLocalizedContentStringMapToSolrDoc(delegator, doc, "longdescription_i18n_", "longdescription_i18n_"+SolrLocaleUtil.I18N_GENERAL, UtilGenerics.<String, String>checkMap(context.get("longDescription")));
    
        // FIXME?: MANUAL population of the alpha sort field, because it's complex
        addAlphaLocalizedContentStringMapToSolrDoc(delegator, doc, "alphaTitleSort_", "alphaTitleSort_"+SolrLocaleUtil.I18N_GENERAL, "title_i18n_", "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, 
                UtilGenerics.<String, String>checkMap(context.get("title")), locales, defaultLocale);
        
        return doc;
    }
    
    private static Collection<String> asStringCollection(Object value) {
        if (value == null) return null;
        else if (value instanceof Collection) return UtilGenerics.checkCollection(value);
        else if (value instanceof String) return UtilMisc.<String>toList((String)value);
        else throw new IllegalArgumentException("generateSolrProductDocument: Expected Collection or String for parameter, instead got: " + value.getClass().getName());
    }
    
    private static Collection<String> asStringCollection(Map<String, Object> context, String paramName) {
        try {
            return asStringCollection(context.get(paramName));
        } catch(IllegalArgumentException e) {
            throw new IllegalArgumentException("generateSolrProductDocument: Expected Collection or String for parameter '" + paramName + "', instead got: " + context.get(paramName).getClass().getName());
        }
    }
    
    private static void addStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values) {
        if (values == null) return;
        Iterator<?> attrIter = values.iterator();
        while (attrIter.hasNext()) {
            Object attr = attrIter.next();
            doc.addField(solrFieldName, attr.toString());
        }
    }
    
    private static void addConcatenatedStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values, String joinStr) {
        if (values == null) return;
        doc.addField(solrFieldName, StringUtils.join(values, joinStr));
    }
    
    private static void addLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc, String solrFieldNamePrefix, String solrDefaultFieldName, Map<String, String> contentMap) {
        if (contentMap == null) return;
        for (Map.Entry<String, String> entry : contentMap.entrySet()) {
            if (SolrLocaleUtil.I18N_GENERAL.equals(entry.getKey())) {
                if (solrDefaultFieldName != null) {
                    doc.addField(solrDefaultFieldName, entry.getValue());
                }
            } else {
                doc.addField(solrFieldNamePrefix + entry.getKey(), entry.getValue());
            }
        }
    }
    
    /**
     * FIXME: This is a WORKAROUND replacement following the removal of 
     * {@code
     *  <copyField source="title_i18n_*" dest="alphaTitleSort_*"/>
     * }
     * in the solr schema. 
     * There are 2 problems:
     * 1) this code should be done by solr, e.g. using existing or custom field processor: https://wiki.apache.org/solr/UpdateRequestProcessor
     * 2) we should not store strings for missing languages at all anymore, it causes
     *    unnecessary storage space taken and may give wrong-language processing issues (e.g. deutsch rules applied to english).
     *    The reason this is being done at indexing is limitations in query sort; it could be
     *    considered an optimization to do it an indexing (at expense of correct-language),
     *    but this was done as a workaround.
     */
    private static void addAlphaLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc, 
            String alphaFieldNamePrefix, String alphaDefaultFieldName, String solrFieldNamePrefix, String solrDefaultFieldName, 
            Map<String, String> contentMap, List<Locale> locales, Locale defaultProductLocale) {
        if (contentMap == null) return;
        
        String generalValue = null;
        if (contentMap.containsKey(SolrLocaleUtil.I18N_GENERAL)) {
            generalValue = contentMap.get(SolrLocaleUtil.I18N_GENERAL);
            doc.addField(alphaDefaultFieldName, generalValue);
        }
        
        // fill in ALL the locales
        for(Locale locale : locales) {
            String locStr = SolrLocaleUtil.getLangCode(locale);
            
            String value = contentMap.get(solrFieldNamePrefix + locStr);
            if (UtilValidate.isEmpty(value)) {
                value = generalValue;
                // 2017-09-14: no longer needed because general and target lang are 
//                if (UtilValidate.isEmpty(value)) {
//                    // if there's nothing else, check entry for sys default lang; even though
//                    // this is sure to be the wrong language, it's better than nothing for sorting...
//                    value = contentMap.get(solrFieldNamePrefix + defaultProductLocale);
//                }
            }
            
            doc.addField(alphaFieldNamePrefix + locStr, value);
        }
    }
    
    /**
     * Adds a variant exclude filter ot the list.
     * Emulates ProductSearchSession's 
     * <code>EntityCondition.makeCondition("prodIsVariant", EntityOperator.NOT_EQUAL, "Y")</code>
     */
    public static void addExcludeVariantsFilter(List<String> queryFilters) {
        queryFilters.add("-isVariant:true");
    }
    
    /**
     * Adds a variant exclude filter ot the list.
     * Emulates ProductSearchSession's 
     * <code>EntityCondition.makeCondition("prodIsVariant", EntityOperator.NOT_EQUAL, "Y")</code>
     */
    public static void addExcludeVariantsFilter(SolrQuery solrQuery) {
        solrQuery.addFilterQuery("-isVariant:true");
    }
    
}