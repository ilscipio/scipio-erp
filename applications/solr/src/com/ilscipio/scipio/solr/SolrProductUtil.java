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
    private static final boolean storePriceSortDefault = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.search.storePriceSort", true);

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

    @Deprecated
    public static String getSearchSortByExpr(ProductSearch.ResultSortOrder sortOrder, String priceSortField, GenericValue productStore, Delegator delegator, Locale locale) {
        return getSearchSortByExpr(sortOrder, priceSortField, productStore, delegator, locale, null);
    }

    public static String getSearchSortByExpr(ProductSearch.ResultSortOrder sortOrder, String priceSortField, GenericValue productStore, Delegator delegator, Locale locale, Map<String, Object> config) {
        String sortBy = null;
        if (sortOrder instanceof ProductSearch.SortProductPrice) {
            ProductSearch.SortProductPrice so = (ProductSearch.SortProductPrice) sortOrder;
            sortBy = SolrProductUtil.getProductSolrPriceFieldNameFromEntityPriceType(so.getProductPriceTypeId(), 
                locale, "Keyword search: ");
            if ("defaultPrice".equals(sortBy)) {
                if (isStorePriceSort(productStore, config)) {
                    String normStoreId = SolrExprUtil.escapeFieldNamePart(productStore.getString("productStoreId"));
                    sortBy = sortBy + "_" + normStoreId + "_pf";
                }
            } else {
                String mainPriceField = sortBy;
                String fallbackPriceField = "defaultPrice";
                // FIXME: Store-specific schema type support to be checked here
                //if (isStorePriceSort(productStore, config) && !SolrUtil.isStoreSchemaType(productStore)) {
                if (isStorePriceSort(productStore, config)) {
                    String normStoreId = SolrExprUtil.escapeFieldNamePart(productStore.getString("productStoreId"));
                    mainPriceField = mainPriceField + "_" + normStoreId + "_pf";
                    fallbackPriceField = fallbackPriceField + "_" + normStoreId + "_pf";
                }
                // SPECIAL price search fallback - allows listPrice search to still work reasonably for products that don't have listPrice
                // TODO?: REVIEW: query would be faster without function, but unclear if want to create
                // a physical sortPrice or sortListPrice in the solr product schema
                // the solr sortBy doesn't support sorting on the extra returnFields, apparently - at least not in this version
                //kwsArgs.searchReturnFields = (kwsArgs.searchReturnFields ?: "*") + 
                //    ",sortPrice=if(exists(" + kwsArgs.sortBy + ")," + kwsArgs.sortBy + ",defaultPrice)";
                //kwsArgs.sortBy = "sortPrice";
                if ("min".equals(priceSortField)) {
                    sortBy = "if(exists(" + mainPriceField + "),min(" + mainPriceField + "," + fallbackPriceField + ")," + fallbackPriceField + ")";
                } else if ("exists".equals(priceSortField)) {
                    sortBy = "if(exists(" + mainPriceField + ")," + mainPriceField + "," + fallbackPriceField + ")";
                } else { // if ("exact".equals(priceSortField)) {
                    sortBy = mainPriceField;
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

    public static boolean isStorePriceSort(GenericValue productStore, Map<String, Object> config) {
        if (productStore == null) {
            return false;
        }
        if (config != null) {
            Boolean enabled = UtilMisc.booleanValueVersatile(config.get("storePriceSort"));
            if (enabled != null) {
                return enabled;
            }
        }
        return storePriceSortDefault;
    }
}
