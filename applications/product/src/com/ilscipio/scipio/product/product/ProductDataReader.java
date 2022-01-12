package com.ilscipio.scipio.product.product;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.config.ProductConfigFactory;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * Supplies general product-related data (including category/catalog data) from db, mainly used by <code>SolrDocBuilder</code> (SCIPIO).
 * Use {@link ProductDataCache} for caching version. These were previously a bunch of helpers in SolrProductUtil.
 * <p>
 * NOTE: This is generally a work-in-progress interface and mainly for use by solr.
 * DEV NOTE: (For Scipio) If you add a method, make sure to add proper {@link ProductDataCache} override.
 * </p>
 */
public class ProductDataReader {
    static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final ProductDataReader DEFAULT = new ProductDataReader();

    /** Returns the effective delegator that methods should use internally. May be overridden by subclasses and may be different that than supplied if needed. */
    protected Delegator getDelegator(DispatchContext dctx) {
        return dctx.getDelegator();
    }

    /** Returns the effective dispatcher that methods should use internally. May be overridden by subclasses and may be different that than supplied if needed. */
    protected LocalDispatcher getDispatcher(DispatchContext dctx) {
        return dctx.getDispatcher();
    }

    /*
     * *************************************************************
     * Product data
     * *************************************************************
     */

    public GenericValue getProduct(DispatchContext dctx, String productId, boolean useCache) throws GenericEntityException {
        return getDelegator(dctx).findOne("Product", UtilMisc.toMap("productId", productId), useCache);
    }

    public GenericValue getProduct(DispatchContext dctx, Map<String, ?> pkFields, boolean useCache) throws GenericEntityException {
        return getDelegator(dctx).findOne("Product", pkFields, useCache);
    }

    public List<GenericValue> getProductAssocFrom(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return getDelegator(dctx).from("ProductAssoc").where("productId", productId).filterByDate(moment).cache(useCache).queryList();
    }

    public List<GenericValue> getProductAssocTo(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return getDelegator(dctx).from("ProductAssoc").where("productIdTo", productId).filterByDate(moment).cache(useCache).queryList();
    }

    public List<GenericValue> getProductAssocFromVariant(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return EntityUtil.filterByAnd(getProductAssocFrom(dctx, productId, moment, useCache), UtilMisc.toMap("productAssocTypeId", "PRODUCT_VARIANT"));
    }

    public List<GenericValue> getProductAssocToVariant(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return EntityUtil.filterByAnd(getProductAssocTo(dctx, productId, moment, useCache), UtilMisc.toMap("productAssocTypeId", "PRODUCT_VARIANT"));
    }

    public GenericValue getParentProductAssoc(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return ProductWorker.getParentProductAssoc(getProductAssocTo(dctx, productId, moment, useCache), false);
    }

    /**
     * Returns the variant products of a virtual product, first-level only.
     * The returned instances are specifically Product instances.
     * Based on {@link ProductWorker#getVariantProductIds(org.ofbiz.entity.Delegator, org.ofbiz.service.LocalDispatcher, java.lang.String, java.util.List, java.sql.Timestamp, boolean)}.
     */
    public List<String> getVariantProductIds(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GeneralException {
        List<GenericValue> variantProductAssocs = getProductAssocFromVariant(dctx, productId, moment, useCache);
        List<String> variantProductIds = new ArrayList<>(variantProductAssocs.size());
        for (GenericValue assoc : variantProductAssocs) {
            variantProductIds.add(assoc.getString("productIdTo"));
        }
        return variantProductIds;
    }

    /**
     * SCIPIO: Returns the variant products of a virtual product, deep, results depth-first.
     * The returned instances are specifically Product instances.
     * Based on {@link ProductWorker#getVariantProductIdsDeepDfs(org.ofbiz.entity.Delegator, org.ofbiz.service.LocalDispatcher, java.lang.String, java.util.List, java.sql.Timestamp, boolean)}.
     */
    public List<String> getVariantProductIdsDeepDfs(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GeneralException {
        return getVariantProductIdsDeepDfs(dctx, productId, moment, useCache, new ArrayList<>());
    }

    protected List<String> getVariantProductIdsDeepDfs(DispatchContext dctx, String productId, Timestamp moment, boolean useCache, List<String> variantProductIds) throws GeneralException {
        List<GenericValue> variantProductAssocs = getProductAssocFromVariant(dctx, productId, moment, useCache);
        for (GenericValue assoc : variantProductAssocs) {
            String variantProductId = assoc.getString("productIdTo");
            variantProductIds.add(variantProductId);
            GenericValue variantProduct = getProduct(dctx, variantProductId, useCache);
            if (Boolean.TRUE.equals(variantProduct.getBoolean("isVirtual"))) {
                getVariantProductIdsDeepDfs(dctx, variantProductId, moment, useCache, variantProductIds);
            }
        }
        return variantProductIds;
    }

    /**
     * SCIPIO: Returns the virtual products of a variant product, first-level only.
     * The returned instances are specifically Product instances.
     * <p>
     * NOTE: Normally, orderBy is set to "-fromDate" and only the first product is consulted;
     * this method is a generalization.
     * Based on
     */
    public List<String> getVirtualProductIds(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GeneralException {
        List<GenericValue> variantProductAssocs = getProductAssocToVariant(dctx, productId, moment, useCache);
        List<String> variantProductIds = new ArrayList<>(variantProductAssocs.size());
        int i = 0;
        for (GenericValue assoc : variantProductAssocs) {
            variantProductIds.add(assoc.getString("productId"));
            i++;
            //if (maxResults != null && i >= maxResults) break;
        }
        return variantProductIds;
    }

    /**
     * SCIPIO: Returns the virtual products of a variant product, deep, results depth-first.
     * The returned instances are specifically Product instances.
     * <p>
     * NOTE: Normally, orderBy is set to "-fromDate" and only the first product is returned
     * on each level; this method is a generalization.
     * Based on {@link ProductWorker#getVirtualProductIdsDeepDfs(org.ofbiz.entity.Delegator, org.ofbiz.service.LocalDispatcher, java.lang.String, java.util.List, java.lang.Integer, java.sql.Timestamp, boolean)}.
     */
    public List<String> getVirtualProductIdsDeepDfs(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GeneralException {
        return getVirtualProductIdsDeepDfs(dctx, productId, moment, useCache, new ArrayList<>());
    }

    protected List<String> getVirtualProductIdsDeepDfs(DispatchContext dctx, String productId, Timestamp moment, boolean useCache, List<String> virtualProductIds) throws GeneralException {
        List<GenericValue> virtualProductAssocs = getProductAssocToVariant(dctx, productId, moment, useCache);
        int i = 0;
        for (GenericValue assoc : virtualProductAssocs) {
            String virtualProductId = assoc.getString("productId");
            virtualProductIds.add(virtualProductId);
            GenericValue virtualProduct = getProduct(dctx, virtualProductId, useCache);
            if (Boolean.TRUE.equals(virtualProduct.getBoolean("isVariant"))) {
                getVirtualProductIdsDeepDfs(dctx, virtualProductId, moment, useCache, virtualProductIds);
            }
            i++;
            //if (maxPerLevel != null && i >= maxPerLevel) break;
        }
        return virtualProductIds;
    }

    public List<GenericValue> getProductCategoryMembers(DispatchContext dctx, String productId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return ProductWorker.getProductCategoryMembers(dctx.getDelegator(), productId, moment, ordered, useCache);
    }

    public Set<String> getOwnCategoryIdsForProduct(DispatchContext dctx, String productId, Timestamp moment, boolean ordered, boolean useCache) throws GenericEntityException {
        return ProductWorker.getOwnCategoryIdsForProduct(ordered ? new LinkedHashSet<>() : new HashSet<>(), getDelegator(dctx), productId, null, moment, ordered, useCache);
    }

    public Set<String> getOwnCategoryIdsForProduct(DispatchContext dctx, String productId, List<GenericValue> productCategoryMembers) throws GenericEntityException {
        return ProductWorker.getOwnCategoryIdsForProduct(new LinkedHashSet<>(), getDelegator(dctx), productId, null, productCategoryMembers);
    }

    public Set<String> getAssocCategoryIdsForProduct(DispatchContext dctx, String productId, List<GenericValue> assocToVariant, Timestamp moment, boolean ordered, boolean useCache) throws GenericEntityException {
        return ProductWorker.getAssocCategoryIdsForProduct(ordered ? new LinkedHashSet<>() : new HashSet<>(), getDelegator(dctx), productId, null, assocToVariant, moment, ordered, useCache);
    }

    public Map<String, Object> getProductStandardPrices(DispatchContext dctx, Map<String, Object> context, GenericValue userLogin, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache, Map<String, ?> ovrdFields) throws GeneralException {
        Map<String, Object> priceContext = UtilMisc.toMap("product", product);
        priceContext.put("currencyUomId", currencyUomId);
        priceContext.put("useCache", useCache);
        // TODO: REVIEW: Doing this here may currently bias results unwantedly toward a specific store that don't apply to other stores, so for now override using ovrdFields
        //if (productStore != null) {
        //    priceContext.put("productStoreId", productStore.get("productStoreId"));
        //}
        copyStdServiceFieldsNotSet(context, priceContext);
        if (ovrdFields != null) {
            priceContext.putAll(ovrdFields);
        }
        Map<String, Object> priceMap = getDispatcher(dctx).runSync("calculateProductPrice", priceContext);
        return priceMap;
    }

    public Map<String, Object> getProductStandardPrices(DispatchContext dctx, Map<String, Object> context, GenericValue userLogin, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws GeneralException {
        return getProductStandardPrices(dctx, context, userLogin, product, productStore, currencyUomId, priceLocale, useCache, null);
    }

    public ProductConfigWrapper getConfigurableProductStartingPrices(DispatchContext dctx, Map<String, Object> context, GenericValue userLogin, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws GeneralException {
        // TODO: REVIEW: do we need to pass a specific catalog or webSiteId here?
        ProductConfigWrapper pcw = null;
        try {
            pcw = ProductConfigFactory.createProductConfigWrapper(getDelegator(dctx), getDispatcher(dctx), product.getString("productId"),
                    (productStore != null) ? productStore.getString("productStoreId") : null, null, null, currencyUomId, priceLocale, userLogin);
        } catch (Exception e) {
            throw new GeneralException(e);
        }
        pcw.setDefaultConfig(); // 2017-08-22: if this is not done, the price will always be zero
        return pcw;
    }

    public <C extends Collection<String>> C getProductKeywords(C outKeywords, Delegator delegator, boolean useCache, String... productIds) throws GeneralException {
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
            outKeywords.add(productKeyword.getString("keyword"));
        }
        return outKeywords;
    }

    public List<GenericValue> getProductContent(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GeneralException {
        return getDelegator(dctx).from("ProductContent").where("productId", productId).filterByDate(moment).cache(useCache).queryList();
    }

    public String getProductContentText(DispatchContext dctx, GenericValue targetContent,
                                        GenericValue product, GenericValue productContent, Locale locale, boolean useCache) throws GeneralException {
        Writer out = new StringWriter();
        Map<String, Object> inContext = new HashMap<>();
        inContext.put("product", product);
        inContext.put("productContent", productContent);
        boolean deepCache = useCache; // SPECIAL: only way to prevent all caching
        try {
            ContentWorker.renderContentAsText(getDispatcher(dctx), getDelegator(dctx), targetContent, out, inContext, locale, "text/plain",
                    null, useCache, deepCache, null);
        } catch (IOException e) {
            throw new GeneralException(e);
        }
        return out.toString();
    }

    public Map<String, String> getLocalizedProductContentStringMap(DispatchContext dctx, GenericValue product, String productContentTypeId,
                                                                   Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey,
                                                                   List<ProductContentWrapper> pcwList, Timestamp moment, boolean useCache) throws GeneralException {
        Map<String, String> contentMap = new HashMap<>();
        try {
            contentMap.put(generalKey, ProductContentWrapper.getEntityFieldValue(product, productContentTypeId, getDelegator(dctx), getDispatcher(dctx), useCache));
        } catch (IOException e) {
            throw new GeneralException(e);
        }
        getProductContentForLocales(contentMap, dctx, product, productContentTypeId, locales, defaultLocale, langCodeFn, generalKey, moment, useCache);
        refineLocalizedContentValues(contentMap, locales, defaultLocale, langCodeFn, generalKey);
        return contentMap;
    }

    /**
     * Refines the map of localized content values (locale->value) by filling in missing values for locales where possible.
     * <p>
     * 2017-09-14: This applies the new behavior where "_i18n_general" and "_i18n_[storedefault]" fields
     * are copied to each other if either one is missing. It assumes the passed defaultLocale
     * accurately reflects the language that the general entries are written in - this is
     * normally ProductStore.defaultLocaleString and found using <code>SolrLocaleUtil#getConfiguredDefaultLocale(GenericValue)</code>.
     * This simplifies queries significantly. See solrconfig.properties and schema.
     */
    public void refineLocalizedContentValues(Map<String, String> contentMap, Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey) {
        if (defaultLocale != null) {
            String generalValue = contentMap.get(generalKey);
            String defaultLangValue = contentMap.get(langCodeFn.apply(defaultLocale));
            if (UtilValidate.isEmpty(defaultLangValue)) {
                if (UtilValidate.isNotEmpty(generalValue)) {
                    contentMap.put(langCodeFn.apply(defaultLocale), generalValue);
                }
            } else if (UtilValidate.isEmpty(generalValue)) {
                if (UtilValidate.isNotEmpty(defaultLangValue)) {
                    contentMap.put(generalKey, defaultLangValue);
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
    public void getProductContentForLocales(Map<String, String> contentMap, DispatchContext dctx, GenericValue product, String productContentTypeId,
                                            Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey,
                                            Timestamp moment, boolean useCache) throws GeneralException {
        Delegator delegator = getDelegator(dctx);
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
            Locale locale = defaultLocale;
            String res = getProductContentText(dctx, targetContent, product, productContent, locale, useCache);
            if (res.length() > 0) {
                contentMap.put(generalKey, res);
                // not needed anymore, because of ContentWrapper prio change and because
                // refineLocalizedContentValues will copy it over anyway
//                if (locale != null) {
//                    contentMap.put(SolrLocaleUtil.getLangCode(locale), res);
//                }
            }
        }
        for(Locale locale : locales) {
            String targetLocaleString = langCodeFn.apply(locale);
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
                String res = getProductContentText(dctx, targetContent, product, productContent, locale, useCache);
                if (res.length() > 0) {
                    contentMap.put(langCodeFn.apply(locale), res);
                }
            }
        }
    }

    public Map<String, ProductContentWrapper> getProductContentWrappersForLocales(DispatchContext dctx, GenericValue product,
                                                                                  Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn,
                                                                                  boolean useCache) throws GeneralException {
        Map<String, ProductContentWrapper> pcwMap = new LinkedHashMap<>();
        for(Locale locale : locales) {
            ProductContentWrapper pcw = new ProductContentWrapper(getDispatcher(dctx), product, locale, null, useCache);
            pcwMap.put(langCodeFn.apply(locale), pcw);
        }
        return pcwMap;
    }

    public List<GenericValue> getCategoryRollups(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return getDelegator(dctx).from("ProductCategoryRollup").where("productCategoryId", productCategoryId)
                .orderBy(ordered ? UtilMisc.toList("sequenceNum") : null).filterByDate(moment).cache(useCache).queryList();
    }

    public List<List<String>> getCategoryRollupTrails(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return CategoryWorker.getCategoryRollupTrails(getDelegator(dctx), productCategoryId, moment, ordered, useCache);
    }

    public List<GenericValue> getProductStoresForCatalogIds(DispatchContext dctx, Collection<String> catalogIds, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        // FIXME: duplication
        //return CatalogWorker.getProductStoresForCatalogIds(delegator, catalogIds, moment, ordered, useCache);
        List<GenericValue> stores = new ArrayList<>();
        Set<String> storeIds = new HashSet<>();
        for(String catalogId : catalogIds) {
            List<GenericValue> productStoreCatalogs = getProductStoreCatalogsForCatalogId(dctx, catalogId, moment, ordered, useCache);
            for(GenericValue productStoreCatalog : productStoreCatalogs) {
                if (!storeIds.contains(productStoreCatalog.getString("productStoreId"))) {
                    try {
                        //stores.add(productStoreCatalog.getRelatedOne("ProductStore", useCache));
                        stores.add(getProductStore(dctx, productStoreCatalog.getString("productStoreId"), useCache));
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Error looking up ProductStore for catalogId: " + catalogId, module);
                    }
                    storeIds.add(productStoreCatalog.getString("productStoreId"));
                }
            }
        }
        return stores;
    }

    public List<GenericValue> getProductStoreCatalogsForCatalogId(DispatchContext dctx, String catalogId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return CatalogWorker.getProductStoreCatalogsForCatalogId(getDelegator(dctx), catalogId, moment, ordered, useCache);
    }

    public GenericValue getProductStore(DispatchContext dctx, String productStoreId, boolean useCache) throws GenericEntityException {
        return getDelegator(dctx).findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
    }

    public List<String> getCatalogIdsByCategoryId(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) throws GeneralException {
        return UtilMisc.getMapValuesForKeyOrNewList(getProdCatalogCategoryByCategoryId(dctx, productCategoryId, moment, useCache), "prodCatalogId");
    }

    public List<GenericValue> getProdCatalogCategoryByCategoryId(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) throws GeneralException {
        return getDelegator(dctx).from("ProdCatalogCategory").where("productCategoryId", productCategoryId)
                .filterByDate(moment).orderBy("sequenceNum").cache(useCache).queryList();
    }

    /*
     * *************************************************************
     * Helpers
     * *************************************************************
     */

    protected void copyStdServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx) {
        copyServiceFieldsNotSet(srcCtx, destCtx, "locale", "userLogin", "timeZone");
    }

    protected void copyServiceFieldsNotSet(Map<String, Object> srcCtx, Map<String, Object> destCtx, String... fieldNames) {
        for(String fieldName : fieldNames) {
            if (!destCtx.containsKey(fieldName)) destCtx.put(fieldName, srcCtx.get(fieldName));
        }
    }

}
