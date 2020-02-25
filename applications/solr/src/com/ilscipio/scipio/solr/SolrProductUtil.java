package com.ilscipio.scipio.solr;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.config.ProductConfigFactory;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductSearch;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceDispatcher;

/**
 * Product utility class for solr.
 */
public abstract class SolrProductUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

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
        map.put("requireAmount_b", "requireAmount");
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

    public static final String PRODUCTFIELD_SALESDISCDATE = "salesDiscDate_dt";

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

    public static String getConfiguredForceDefaultCurrency(GenericValue productStore) {
        return configuredForceDefaultCurrency;
    }

    public static String getConfiguredForceDefaultCurrency(Delegator delegator) {
        return configuredForceDefaultCurrency;
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

        String defaultField = (String) config.get("product.defaultField");
        if (defaultField == null || defaultField.isEmpty()) defaultField = "text_i18n_";

        String i18nFieldsSelect = (String) config.get("i18nFieldsSelect");
        Collection<Locale> i18nForceLocales = UtilGenerics.checkCollection(config.get("i18nForceLocales"));
        String userLocalePower = (String) config.get("userLocalePower");
        Collection<String> commonFields = UtilGenerics.checkCollection(config.get("product.commonFields"));

        List<FlexibleStringExpander> extraLangFields = UtilGenerics.checkList(config.get("product.extraLangFields"));
        String extraUserLocalePower = (String) config.get("extraUserLocalePower");
        String extraStoreLocalePower = (String) config.get("extraStoreLocalePower");

        Set<String> queryFields = null;
        if (UtilValidate.isNotEmpty(i18nFieldsSelect)) {
            if ("user-store".equals(i18nFieldsSelect)) {
                // FIXME: Set strings are not check for power e.g. ^2 suffix
                queryFields = SolrLocaleUtil.determineI18nQueryFieldsForUserLocale(userLocale, productStore, true, i18nForceLocales, defaultField, userLocalePower, null, null,
                        extraLangFields, extraUserLocalePower, extraStoreLocalePower);
                if (commonFields != null) queryFields.addAll(commonFields);
            } else if ("user".equals(i18nFieldsSelect)) {
                queryFields = SolrLocaleUtil.determineI18nQueryFieldsForUserLocale(userLocale, productStore, false, i18nForceLocales, defaultField, userLocalePower, null, null,
                        extraLangFields, extraUserLocalePower, extraStoreLocalePower);
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

        config.put("product.defaultField", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "product.defaultField", null, delegator));

        config.put("product.extraLangFields", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "product.extraLangFields", null, delegator));
        config.put("extraUserLocalePower", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "extraUserLocalePower", null, delegator));
        config.put("extraStoreLocalePower", EntityUtilProperties.getPropertyValue(propResource, propNamePrefix + "extraStoreLocalePower", null, delegator));

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

        Object extraLangFieldsObj = config.get("product.extraLangFields");
        if (extraLangFieldsObj instanceof String) {
            Collection<String> extraLangFields;
            if (((String) extraLangFieldsObj).length() == 0 || "NONE".equals(extraLangFieldsObj)) {
                extraLangFields = Collections.emptySet();
            } else {
                extraLangFields = Arrays.asList(((String) extraLangFieldsObj).split("\\s*,\\s*"));
            }
            List<FlexibleStringExpander> exdrList = new ArrayList<>(extraLangFields.size());
            for(String expr : extraLangFields) {
                exdrList.add(FlexibleStringExpander.getInstance(expr));
            }
            config.put("product.extraLangFields", exdrList);
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
     * <p>
     * NOTE: Prefer <code>fields</code> over <code>targetCtx</code>, which is old method and requires needless patching to make work.
     * For <code>fields</code>, the field names are the final Solr schema field names; for <code>targetCtx</code>, they are
     * intermediate names found in services.xml.
     * <p>
     * <b>WARNING:</b> DO NOT USE ENTITY CACHE HERE FOR ANYTHING! It is an error and you will end up with serious problems!
     * Do not call any utilities that use the entity cache! If you need them, you must patch them to not use entity cache.
     * The useCache parameter here will always be false for the foreseeable future.
     * <p>
     * <b>WARNING:</b> You should use the provided nowTimestamp for filter-by-date operations.
     */
    public static Map<String, Object> getProductContent(GenericValue product, DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String productId = (String) product.get("productId");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        Map<String, Object> targetCtx = new HashMap<>();
        Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
        Timestamp moment = nowTimestamp;

        if (Debug.verboseOn()) Debug.logVerbose("Solr: Getting product content for product '" + productId + "'", module);

        try {
            // 2018: The fields map is needed for arbitrarily-named fields such as dynamicFields.
            // It is much more flexible than the stock static field names in the
            // solrProductAttributes service interface and in the future may replace it entirely.
            Map<String, Object> fields = getGenSolrDocFieldsMap(targetCtx);

            targetCtx.put("productId", productId);

            // Get all product assoc
            List<GenericValue> productAssocFromList = delegator.from("ProductAssoc").where("productId", productId).filterByDate(moment).cache(useCache).queryList();
            List<GenericValue> productAssocToList = delegator.from("ProductAssoc").where("productIdTo", productId).filterByDate(moment).cache(useCache).queryList();

            List<GenericValue> productVariantAssocs = null;
            if (Boolean.TRUE.equals(product.getBoolean("isVariant"))) { // 2018-12-20: reuse productAssocTo
                productVariantAssocs = EntityUtil.filterByAnd(productAssocToList, UtilMisc.toMap("productAssocTypeId", "PRODUCT_VARIANT"));
            }
            List<GenericValue> productVariantAssocsOrEmpty = (productVariantAssocs != null) ? productVariantAssocs : Collections.emptyList(); // to optimize worker calls

            // 2017-09: do EARLY cat lookup so that we can find out a ProductStore
            Set<String> ownCategoryIds = ProductWorker.getOwnCategoryIdsForProduct(new LinkedHashSet<>(), delegator, productId, product, moment, true, useCache);
            Set<String> assocCategoryIds = ProductWorker.getAssocCategoryIdsForProduct(new LinkedHashSet<>(), delegator, productId, product, productVariantAssocsOrEmpty, moment, true, useCache);
            Set<String> productCategoryIds = new LinkedHashSet<>(ownCategoryIds);
            productCategoryIds.addAll(assocCategoryIds);
            // Trying to set a correct trail
            Collection<String> ownTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), dctx, ownCategoryIds, moment, true, useCache);
            Collection<String> assocTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), dctx, assocCategoryIds, moment, true, useCache);
            Collection<String> trails = new LinkedHashSet<>(ownTrails);
            trails.addAll(assocTrails);
            // Get the catalogs that have associated the categories
            Collection<String> catalogs = SolrCategoryUtil.getCatalogIdsFromCategoryTrails(new LinkedHashSet<>(), dctx, trails, moment, useCache);

            List<GenericValue> productStores;
            if (!catalogs.isEmpty()) {
                productStores = CatalogWorker.getProductStoresForCatalogIds(delegator, catalogs, moment, true, useCache);
            } else {
                productStores = new ArrayList<>();
            }

            Collection<String> relatedCategoryIds = null;
            Collection<String> relatedTrails = null;
            Collection<String> relatedCatalogs = null;
            if (catalogs.isEmpty()) { // 2019-12: REMOVED: || productStores.isEmpty() -> if there's a catalog but not associated to a store, something is misconfigured
                // TODO: REVIEW: If we could not determine catalog directly, usually due to config, alternative package
                //  or other complex products, search product assoc categories to try to determine a catalog, so can get a store
                // NOTE: The found relatedCategoryIds are NOT added to the categoryIds in solr, must only be used to determine logical store/catalog
                relatedCategoryIds = new LinkedHashSet<>();
                relatedTrails = new LinkedHashSet<>();
                relatedCatalogs = new LinkedHashSet<>();
                // Self and virtuals are covered above
                Set<String> catCheckedProductIds = new HashSet<>(); // don't requery ProductCategoryMember for these already checked (productId and its virtual(s))
                catCheckedProductIds.add(productId);
                catCheckedProductIds.addAll(UtilMisc.getMapValuesForKey(productVariantAssocsOrEmpty, "productId"));
                ProductWorker.getAnyRelatedCategoryIdsForProduct(relatedCategoryIds, new LinkedHashSet<>(), catCheckedProductIds, delegator, productId, product,
                        productVariantAssocsOrEmpty, productAssocFromList, productAssocToList,false, moment, true, useCache); // NOTE: firstFoundOnly==false
                SolrCategoryUtil.getCategoryTrails(relatedTrails, dctx, relatedCategoryIds, moment, true, useCache);
                SolrCategoryUtil.getCatalogIdsFromCategoryTrails(relatedCatalogs, dctx, relatedTrails, moment, useCache);
                catalogs.addAll(relatedCatalogs);
                productStores.addAll(CatalogWorker.getProductStoresForCatalogIds(delegator, relatedCatalogs, moment, true, useCache));
            }
            productStores = EntityUtil.orderBy(productStores, UtilMisc.toList("defaultPriority"));

            List<String> productStoreIdList = UtilMisc.getMapValuesForKeyOrNewList(productStores, "productStoreId");
            targetCtx.put("productStore", productStoreIdList);

            if (productStores.isEmpty()) {
                if (catalogs.isEmpty()) {
                    Debug.logInfo("Solr: Could not determine store for product '" + productId + "'; no catalogs", module);
                } else {
                    Debug.logInfo("Solr: Could not determine store for product '" + productId + "' from catalogs: " + catalogs, module);
                }
            } else {
                if (SolrUtil.verboseOn()) {
                    if (relatedCatalogs != null) {
                        Debug.logInfo("Solr: Determined store(s) for product '" + productId + "' indirectly (" + productStoreIdList + ") from related catalogs: " + catalogs, module);
                    } else {
                        Debug.logInfo("Solr: Determined store(s) for product '" + productId + "' directly (" + productStoreIdList + ") from catalogs: " + catalogs, module);
                    }
                }
            }

            targetCtx.put("category", new ArrayList<>(trails));
            fields.put("ownCat_ss", new ArrayList<>(ownTrails));
            targetCtx.put("catalog", new ArrayList<>(catalogs));
            
            // MAIN STORE SELECTION AND LOCALE LOOKUP
            // NOTE: we skip the isContentReference warning if there's both a forced locale and forced currency.
            GenericValue productStore = ProductStoreWorker.getContentReferenceStoreOrFirst(productStores,
                    (SolrLocaleUtil.getConfiguredForceDefaultLocale(delegator) == null || SolrProductUtil.getConfiguredForceDefaultCurrency(delegator) == null)
                        ? ("product '" + productId + "'") : null);
            List<Locale> locales = SolrLocaleUtil.getConfiguredLocales(productStore);
            Locale defaultProductLocale = SolrLocaleUtil.getConfiguredDefaultLocale(productStore);

            // Generate special ProductContentWrapper for the supported languages
            Map<String, ProductContentWrapper> pcwMap = new HashMap<>();
            List<ProductContentWrapper> pcwList = new ArrayList<>(locales.size());
            for(Locale locale : locales) {
                ProductContentWrapper pcw = new ProductContentWrapper(dispatcher, product, locale, null, useCache);
                pcwMap.put(SolrLocaleUtil.getLangCode(locale), pcw);
                pcwList.add(pcw);
            }

            String parentProductId = null;
            if ("Y".equals(product.getString("isVariant"))) {
                // IMPORTANT: same parent lookup logic as used by ProductContentWrapper
                parentProductId = ProductWorker.getParentProductId(productId, delegator, useCache);
            }

            // if (product.get("sku") != null) targetCtx.put("sku", product.get("sku"));
            if (product.get("internalName") != null) {
                targetCtx.put("internalName", product.get("internalName"));
            }
            if (product.get("productTypeId") != null) {
                targetCtx.put("productTypeId", product.get("productTypeId"));
            }
            String smallImage = (String) product.get("smallImageUrl");
            if (smallImage != null) {
                targetCtx.put("smallImage", smallImage);
            }
            String mediumImage = (String) product.get("mediumImageUrl");
            if (mediumImage != null) {
                targetCtx.put("mediumImage", mediumImage);
            }
            String largeImage = (String) product.get("largeImageUrl");
            if (largeImage != null) {
                targetCtx.put("largeImage", largeImage);
            }

            Map<String, Object> featureSet = dispatcher.runSync("getProductFeatureSet", UtilMisc.toMap("productId", productId, "emptyAction", "success", "useCache", useCache));
            if (featureSet != null) {
                targetCtx.put("features", (Set<?>) featureSet.get("featureSet"));
            }

            /* 2018-05-29: Use a more precise, total AND per-store count
            Map<String, Object> productInventoryAvailable = dispatcher.runSync("getProductInventoryAvailable", UtilMisc.toMap("productId", productId, "useCache", useCache));
            String inStock = null;
            BigDecimal availableToPromiseTotal = (BigDecimal) productInventoryAvailable.get("availableToPromiseTotal");
            if (availableToPromiseTotal != null) {
                inStock = availableToPromiseTotal.toBigInteger().toString();
            }
            */
            final boolean useTotal = true;
            // WARN: here the total (inStock) behavior for variants is determined by the first store found only!
            final boolean useVariantStockCalcForTotal = (productStore != null && Boolean.TRUE.equals(productStore.getBoolean("useVariantStockCalc")));
            Map<String, BigDecimal> productStoreInventories = ProductWorker.getProductStockPerProductStore(delegator, dispatcher, product,
                    productStores, useTotal, useVariantStockCalcForTotal, nowTimestamp, useCache);
            for (Map.Entry<String, BigDecimal> entry : productStoreInventories.entrySet()) {
                if ("_total_".equals(entry.getKey())) {
                    targetCtx.put("inStock", entry.getValue().toBigInteger().intValue());
                } else {
                    String fieldName = "storeStock_" + SolrExprUtil.escapeFieldNamePart(entry.getKey()) + "_pi";
                    if (fields.containsKey(fieldName)) {
                        Debug.logError("Solr: DATA ERROR - DUPLICATE PRODUCT STORE storeStock_ VARIABLE DETECTED (" + fieldName
                                + ", for productStoreId '" + entry.getKey() + "') - productStoreId clash - Solr cannot index data for this store!"
                                + " This means that your system contains two ProductStores that have productStoreIds"
                                + " too similar so they cannot be uniquely represented in the Solr schema field names."
                                + " You will need to change the ID of one of the ProductStores and reindex using rebuildSolrIndex.", module);
                    } else {
                        fields.put(fieldName, entry.getValue().toBigInteger().intValue());
                    }
                }
            }


            boolean isVirtual = "Y".equals(product.getString("isVirtual"));
            if (isVirtual) targetCtx.put("isVirtual", isVirtual);
            boolean isVariant = "Y".equals(product.getString("isVariant"));
            if (isVariant) targetCtx.put("isVariant", isVariant); // new 2017-08-17
            boolean isDigital = ProductWorker.isDigital(product);
            if (isDigital) targetCtx.put("isDigital", isDigital);
            boolean isPhysical = ProductWorker.isPhysical(product);
            if (isPhysical) targetCtx.put("isPhysical", isPhysical);

            Boolean requireAmount = product.getBoolean("requireAmount");
            if (Boolean.TRUE.equals(requireAmount)) fields.put("requireAmount_b", requireAmount);

            targetCtx.put("title", getLocalizedContentStringMap(delegator, dispatcher, product, "PRODUCT_NAME", locales, defaultProductLocale, pcwList, moment, useCache));
            targetCtx.put("description", getLocalizedContentStringMap(delegator, dispatcher, product, "DESCRIPTION", locales, defaultProductLocale, pcwList, moment, useCache));
            targetCtx.put("longDescription", getLocalizedContentStringMap(delegator, dispatcher, product, "LONG_DESCRIPTION", locales, defaultProductLocale, pcwList, moment, useCache));

            // targetCtx.put("comments", "");
            // targetCtx.put("keywords", "");
            // targetCtx.put("last_modified", "");

            // this is the currencyUomId that the prices in solr should use...
            String currencyUomId = getConfiguredDefaultCurrency(delegator, productStore);

            Map<String, Object> stdPriceMap = null;
            ProductConfigWrapper cfgPriceWrapper = null;
            if ("AGGREGATED".equals(product.get("productTypeId")) || "AGGREGATED_SERVICE".equals(product.get("productTypeId"))) {
                cfgPriceWrapper = getConfigurableProductStartingPrices(targetCtx, delegator, dispatcher, userLogin, context, product,
                        productStore, currencyUomId, defaultProductLocale, useCache);
            } else {
                stdPriceMap = getProductStandardPrices(targetCtx, delegator, dispatcher, userLogin, context, product,
                        productStore, currencyUomId, defaultProductLocale, useCache);
            }

            Timestamp salesDiscDate = product.getTimestamp("salesDiscontinuationDate");
            if (salesDiscDate != null) {
                fields.put("salesDiscDate_dt", salesDiscDate);
            }

            // 2017-09-12: added missing ProductKeyword lookup, otherwise can't input keywords from ofbiz
            Set<String> keywords = new LinkedHashSet<>();
            // NOTE: for variant products, we also include the keywords from the virtual/parent
            getProductKeywords(keywords, delegator, useCache, productId, parentProductId);
            targetCtx.put("keywords", new ArrayList<>(keywords));

            /*
             * CLIENT CODE HOOK
             * As a quick solution, client code may be patched in after this line (recommended: call to external method)
             */

            /*
             * /CLIENT CODE HOOK (END)
             */

        } catch (Exception e) {
            Debug.logError(e, "Solr: getProductContent: Error reading product '" + productId + "': " + e.getMessage(), module);
        }
        return targetCtx;
    }

    /**
     * Gets or creates and stores the unabstracted "fields" map for addToSolrIndex.
     */
    protected static Map<String, Object> getGenSolrDocFieldsMap(Map<String, Object> targetCtx) {
        Map<String, Object> fields = UtilGenerics.checkMap(targetCtx.get("fields"));
        if (fields == null) {
            fields = new HashMap<>();
            targetCtx.put("fields", fields);
        }
        return fields;
    }

    /**
     * Gets the given field from the "fields" map or from the root target context (which should contain the fields map).
     * FIXME: unchecked cast, IDEA doesn't work.
     */
    protected static <T> T getField(Map<String, Object> targetCtx, Map<String, Object> fields, String fieldName) {
        if (fields.containsKey(fieldName)) {
            return (T) fields.get(fieldName);
        }
        return (T) targetCtx.get(fieldName);
    }

    /**
     * Gets the given field from the "fields" map or from the root target context (which should contain the fields map).
     */
    protected static <T> T getField(Map<String, Object> targetCtx, String fieldName) {
        return getField(targetCtx, getGenSolrDocFieldsMap(targetCtx), fieldName);
    }

    protected static void getProductKeywords(Collection<String> keywords, Delegator delegator, boolean useCache, String... productIds) throws GenericEntityException {
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

    protected static Map<String, String> getLocalizedContentStringMap(Delegator delegator, LocalDispatcher dispatcher, GenericValue product,
            String productContentTypeId, List<Locale> locales, Locale defaultProductLocale, List<ProductContentWrapper> pcwList, Timestamp moment, boolean useCache) throws GeneralException, IOException {
        Map<String, String> contentMap = new HashMap<>();

        contentMap.put(SolrLocaleUtil.I18N_GENERAL, ProductContentWrapper.getEntityFieldValue(product, productContentTypeId, delegator, dispatcher, useCache));

        getProductContentForLocales(contentMap, delegator, dispatcher, product, productContentTypeId, locales, defaultProductLocale, moment, useCache);

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
    protected static void getProductContentForLocales(Map<String, String> contentMap, Delegator delegator, LocalDispatcher dispatcher,
            GenericValue product, String productContentTypeId, Collection<Locale> locales, Locale defaultProductLocale, Timestamp moment, boolean useCache) throws GeneralException, IOException {
        String productId = product.getString("productId");

        List<GenericValue> productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", productId, "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(useCache).filterByDate(moment).queryList();
        if (UtilValidate.isEmpty(productContentList) && ("Y".equals(product.getString("isVariant")))) {
            GenericValue parent = ProductWorker.getParentProduct(productId, delegator, useCache);
            if (UtilValidate.isNotEmpty(parent)) {
                productContentList = EntityQuery.use(delegator).from("ProductContent").where("productId", parent.get("productId"), "productContentTypeId", productContentTypeId).orderBy("-fromDate").cache(useCache).filterByDate(moment).queryList();
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

        //boolean deepCache = useCache; // SCIPIO: SPECIAL: only way to prevent all caching

        String thisLocaleString = (String) content.get("localeString");
        thisLocaleString = (thisLocaleString != null) ? thisLocaleString : "";
        // special case: no locale string: treat as general
        if (thisLocaleString.isEmpty()) { // 2017-11-24: this would actually have priority over entity field now:  && UtilValidate.isEmpty((String) contentMap.get(SolrLocaleUtil.I18N_GENERAL))
            // NOTE: 2017-11-24: due to ContentWrapper changes, this case now has priority over the entity field for
            // the value of I18N_GENERAL
            GenericValue targetContent = content;
            Locale locale = defaultProductLocale;
            String res = getContentText(delegator, dispatcher, targetContent, product, productContent, locale, useCache);
            if (res.length() > 0) {
                contentMap.put(SolrLocaleUtil.I18N_GENERAL, res);
                // not needed anymore, because of ContentWrapper prio change and because
                // refineLocalizedContentValues will copy it over anyway
//                if (locale != null) {
//                    contentMap.put(SolrLocaleUtil.getLangCode(locale), res);
//                }
            }
        }
        for(Locale locale : locales) {
            String targetLocaleString = SolrLocaleUtil.getLangCode(locale);
            GenericValue targetContent = null;
            if (targetLocaleString.equalsIgnoreCase(thisLocaleString)) {
                targetContent = content;
            } else {
                GenericValue altContent = ContentWorker.findAlternateLocaleContent(delegator, content, locale, null, useCache);
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

    protected static String getContentText(Delegator delegator, LocalDispatcher dispatcher, GenericValue targetContent,
            GenericValue product, GenericValue productContent, Locale locale, boolean useCache) throws GeneralException, IOException {
        Writer out = new StringWriter();
        Map<String, Object> inContext = new HashMap<>();
        inContext.put("product", product);
        inContext.put("productContent", productContent);
        boolean deepCache = useCache; // SCIPIO: SPECIAL: only way to prevent all caching
        ContentWorker.renderContentAsText(dispatcher, delegator, targetContent, out, inContext, locale, "text/plain", null, useCache, deepCache, null);
        return out.toString();
    }

    protected static Map<String, Object> getProductStandardPrices(Map<String, Object> out, Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> context, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws Exception {
        Map<String, Object> priceContext = UtilMisc.toMap("product", product);
        priceContext.put("currencyUomId", currencyUomId);
        SolrProductSearch.copyStdServiceFieldsNotSet(context, priceContext);
        priceContext.put("useCache", useCache);
        Map<String, Object> priceMap = dispatcher.runSync("calculateProductPrice", priceContext);
        if (priceMap.get("listPrice") != null) {
            String listPrice = scaleCurrency((BigDecimal) priceMap.get("listPrice")).toString();
            out.put("listPrice", listPrice);
        }
        if (priceMap.get("defaultPrice") != null) {
            String defaultPrice = scaleCurrency((BigDecimal) priceMap.get("defaultPrice")).toString();
            if (defaultPrice != null) {
                out.put("defaultPrice", defaultPrice);
            }
        }
        return priceMap;
    }

    protected static ProductConfigWrapper getConfigurableProductStartingPrices(Map<String, Object> out, Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> context, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws Exception {
        String productStoreId = null;
        if (productStore != null) {
            productStoreId = productStore.getString("productStoreId");
        } else {
            //Debug.logWarning("getConfigurableProductStartingPrices: missing product store", module);
        }
        // TODO: REVIEW: do we need to pass a specific catalog or webSiteId here?
        ProductConfigWrapper configWrapper = ProductConfigFactory.createProductConfigWrapper(delegator, dispatcher, product.getString("productId"), productStoreId, null, null, currencyUomId, priceLocale, userLogin); // SCIPIO: Use factory
        configWrapper.setDefaultConfig(); // 2017-08-22: if this is not done, the price will always be zero
        BigDecimal listPrice = configWrapper.getTotalListPrice();
        // 2017-08-22: listPrice is NEVER null here - getTotalListPrice returns 0 if there was no list price - and
        // this creates 0$ list prices we can't validate in queries; this logic requires an extra check + ofbiz patch
        //if (listPrice != null) {
        if (listPrice != null && ((listPrice.compareTo(BigDecimal.ZERO) != 0) || configWrapper.hasOriginalListPrice())) {
            out.put("listPrice", scaleCurrency(listPrice).toString());
        }
        BigDecimal defaultPrice = configWrapper.getTotalPrice();
        if (defaultPrice != null) {
            out.put("defaultPrice", scaleCurrency(defaultPrice).toString());
        }
        return configWrapper;
    }

    protected static BigDecimal scaleCurrency(BigDecimal amount) {
        return amount.setScale(2, RoundingMode.HALF_UP);
    }

    protected static List<String> getSolrProdAttrSimple() {
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
     * <p>
     * 2019-08-01: Renamed to generateSolrDocument and added support for non-product documents that use only the <code>fields</code> map.
     */
    public static SolrInputDocument generateSolrDocument(Delegator delegator, LocalDispatcher dispatcher,
                                                         Map<String, Object> context, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        SolrInputDocument doc = new SolrInputDocument();

        String productId = (String) context.get("productId");
        if (UtilValidate.isEmpty(productId) || Boolean.TRUE.equals(context.get("fieldsOnly"))) {
            return addSolrDocumentFields(doc, UtilGenerics.checkMap(context.get("fields")));
        }

        // 2019-08-01: fill the generalized "id" field with productId if not set
        Map<String, Object> fields = UtilGenerics.checkMap(context.get("fields"));
        if (fields != null && UtilValidate.isEmpty((String) fields.get("id"))) {
            doc.addField("id", productId);
        }

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

        // SCIPIO: 2018-02-05: new "manual" fields map, without abstraction
        addSolrDocumentFields(doc, UtilGenerics.checkMap(context.get("fields")));
        return doc;
    }

    /**
     * generateSolrProductDocument.
     * @deprecated 2019-08-01: generalized to {@link #generateSolrDocument} to support non-product documents and exploit the "fields" map more.
     */
    @Deprecated
    public static SolrInputDocument generateSolrProductDocument(Delegator delegator, LocalDispatcher dispatcher,
                                                                Map<String, Object> context, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        return generateSolrDocument(delegator, dispatcher, context, useCache);
    }

    public static SolrInputDocument addSolrDocumentFields(SolrInputDocument doc, Map<String, Object> fields) {
        if (fields != null) {
            for(Map.Entry<String, Object> entry : fields.entrySet()) {
                doc.addField(entry.getKey(), entry.getValue());
            }
        }
        return doc;
    }

    protected static Collection<String> asStringCollection(Object value) {
        if (value == null) return null;
        else if (value instanceof Collection) return UtilGenerics.checkCollection(value);
        else if (value instanceof String) return UtilMisc.<String>toList((String)value);
        else throw new IllegalArgumentException("generateSolrProductDocument: Expected Collection or String for parameter, instead got: " + value.getClass().getName());
    }

    protected static Collection<String> asStringCollection(Map<String, Object> context, String paramName) {
        try {
            return asStringCollection(context.get(paramName));
        } catch(IllegalArgumentException e) {
            throw new IllegalArgumentException("generateSolrProductDocument: Expected Collection or String for parameter '" + paramName + "', instead got: " + context.get(paramName).getClass().getName());
        }
    }

    protected static void addStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values) {
        if (values == null) return;
        Iterator<?> attrIter = values.iterator();
        while (attrIter.hasNext()) {
            Object attr = attrIter.next();
            doc.addField(solrFieldName, attr.toString());
        }
    }

    /**
     * addConcatenatedStringValuesToSolrDoc.
     * NOTE: 2018-04-04: this now omits the field if empty string by default.
     */
    protected static void addConcatenatedStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values, String joinStr) {
        addConcatenatedStringValuesToSolrDoc(doc, solrFieldName, values, joinStr, true);
    }

    protected static void addConcatenatedStringValuesToSolrDoc(SolrInputDocument doc, String solrFieldName, Collection<?> values, String joinStr, boolean omitIfEmpty) {
        if (values == null) return;
        String joined = StringUtils.join(values, joinStr);
        if (!(omitIfEmpty && joined.isEmpty())) {
            doc.addField(solrFieldName, joined);
        }
    }

    protected static void addLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc, String solrFieldNamePrefix, String solrDefaultFieldName, Map<String, String> contentMap) {
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
    protected static void addAlphaLocalizedContentStringMapToSolrDoc(Delegator delegator, SolrInputDocument doc,
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

    public static String makeExcludeVariantsExpr() {
        return "-isVariant:true";
    }

    /**
     * Adds a variant exclude filter to the list.
     * Emulates ProductSearchSession's
     * <code>EntityCondition.makeCondition("prodIsVariant", EntityOperator.NOT_EQUAL, "Y")</code>
     */
    public static void addExcludeVariantsFilter(List<String> queryFilters) {
        queryFilters.add(makeExcludeVariantsExpr());
    }

    /**
     * Adds a variant exclude filter to the query.
     * Emulates ProductSearchSession's
     * <code>EntityCondition.makeCondition("prodIsVariant", EntityOperator.NOT_EQUAL, "Y")</code>
     */
    public static void addExcludeVariantsFilter(SolrQuery solrQuery) {
        solrQuery.addFilterQuery(makeExcludeVariantsExpr());
    }

    /**
     * Adds the default product filters, for solr service implementations.
     * <p>
     * NOTE: The defaults for ProductStore.showOutOfStockProducts and ProductStore.showDiscontinuedProducts
     * is FALSE (inherited from stock ofbiz; see entitymodel.xml descriptions for ProductStore).
     * The default for excludeVariants is TRUE.
     * @see SolrQueryUtil#addDefaultQueryFilters
     */
    public static void addDefaultProductFilters(List<String> queryFilters, Map<String, ?> context) {
        GenericValue productStore = (GenericValue) context.get("productStore");
        Timestamp filterTimestamp = (Timestamp) context.get("filterTimestamp");
        if (filterTimestamp == null) filterTimestamp = UtilDateTime.nowTimestamp();

        Boolean useStockFilter = (Boolean) context.get("useStockFilter"); // default FALSE
        if (Boolean.TRUE.equals(useStockFilter) ||
            (useStockFilter == null && productStore != null && Boolean.FALSE.equals(productStore.getBoolean("showOutOfStockProducts")))) {
            queryFilters.add(makeProductInStockExpr(productStore));
        }

        Boolean useDiscFilter = (Boolean) context.get("useDiscFilter"); // default FALSE
        if (Boolean.TRUE.equals(useDiscFilter) ||
            (useDiscFilter == null && productStore != null && Boolean.FALSE.equals(productStore.getBoolean("showDiscontinuedProducts")))) {
            queryFilters.add(SolrExprUtil.makeDateFieldAfterOrUnsetExpr(SolrProductUtil.PRODUCTFIELD_SALESDISCDATE, filterTimestamp));
        }

        Boolean excludeVariants = (Boolean) context.get("excludeVariants"); // default TRUE
        if (Boolean.TRUE.equals(excludeVariants) ||
            (excludeVariants == null && (productStore == null || !Boolean.FALSE.equals(productStore.getBoolean("prodSearchExcludeVariants"))))) {
            addExcludeVariantsFilter(queryFilters);
        }
    }

    /**
     * Makes a product in-stock filter expression, specific to the productStore if non-null,
     * or if null, for all the product's facilities combined.
     */
    public static String makeProductInStockExpr(GenericValue productStore) {
        if (productStore != null) {
            return "storeStock_"
                    + SolrExprUtil.escapeFieldNamePart(productStore.getString("productStoreId"))
                    + "_pi:[1 TO *]";
        } else {
            return "inStock:[1 TO *]";
        }
    }
    
    public static String getSearchSortByExpr(ProductSearch.ResultSortOrder sortOrder, String priceSortField, GenericValue productStore, Delegator delegator, Locale locale) {
        String sortBy = null;
        if (sortOrder instanceof ProductSearch.SortProductPrice) {
            ProductSearch.SortProductPrice so = (ProductSearch.SortProductPrice) sortOrder;
            sortBy = SolrProductUtil.getProductSolrPriceFieldNameFromEntityPriceType(so.getProductPriceTypeId(), 
                locale, "Keyword search: ");
            if (!"defaultPrice".equals(sortBy)) {
                // SPECIAL price search fallback - allows listPrice search to still work reasonably for products that don't have listPrice
                // TODO?: REVIEW: query would be faster without function, but unclear if want to create
                // a physical sortPrice or sortListPrice in the solr product schema
                // the solr sortBy doesn't support sorting on the extra returnFields, apparently - at least not in this version
                //kwsArgs.searchReturnFields = (kwsArgs.searchReturnFields ?: "*") + 
                //    ",sortPrice=if(exists(" + kwsArgs.sortBy + ")," + kwsArgs.sortBy + ",defaultPrice)";
                //kwsArgs.sortBy = "sortPrice";
                if ("min".equals(priceSortField)) {
                    sortBy = "if(exists(" + sortBy + "),min(" + sortBy + "," + "defaultPrice),defaultPrice)";
                } else if ("exists".equals(priceSortField)) {
                    sortBy = "if(exists(" + sortBy + ")," + sortBy + ",defaultPrice)";
                } else { // if ("exact".equals(priceSortField)) {
                    //sortBy = sortBy; // redundant
                }
            }
        //} else if (sortOrder instanceof ProductSearch.SortProductFeature) {
            // TODO?
            //ProductSearch.SortProductFeature so = (ProductSearch.SortProductFeature) sortOrder;
        } else if (sortOrder instanceof ProductSearch.SortKeywordRelevancy) {
            //ProductSearch.SortKeywordRelevancy so = (ProductSearch.SortKeywordRelevancy) sortOrder;
            //sortBy = null;
        } else if (sortOrder instanceof ProductSearch.SortProductField) {
            ProductSearch.SortProductField so = (ProductSearch.SortProductField) sortOrder;
            // DEV NOTE: if you don't use this method, solr queries may crash on extra locales
            Locale simpleLocale = SolrLocaleUtil.getCompatibleLocaleValidOrProductStoreDefault(locale, productStore);
            sortBy = SolrProductUtil.getProductSolrFieldNameFromEntity(so.getFieldName(), simpleLocale);
            if (UtilValidate.isEmpty(sortBy)) {
                sortBy = so.getFieldName();
            }
            if (UtilValidate.isNotEmpty(sortBy)) {
                String newSortBy = SolrProductUtil.getProductSolrSortFieldNameFromSolr(sortBy, simpleLocale);
                if (UtilValidate.isNotEmpty(newSortBy)) {
                    sortBy = newSortBy;
                }
                newSortBy = SolrProductUtil.makeProductSolrSortFieldExpr(
                        sortBy, 
                        SolrLocaleUtil.getCompatibleLocaleValid(locale, productStore),
                        SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore)
                    );
                if (UtilValidate.isNotEmpty(newSortBy)) {
                    sortBy = newSortBy;
                }
            }
        }
        return sortBy;
    }
}
