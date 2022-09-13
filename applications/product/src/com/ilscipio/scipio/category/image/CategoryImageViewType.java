package com.ilscipio.scipio.category.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Essentially a wrapper for the new ProductCategoryContentType fields (SCIPIO).
 *
 */
public class CategoryImageViewType implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final GenericValue categoryContentType;
    protected final String categoryContentTypeId;

    protected CategoryImageViewType(GenericValue categoryContentType) throws IllegalArgumentException {
        this.categoryContentType = categoryContentType;
        this.categoryContentTypeId = categoryContentType.getString("prodCatContentTypeId");
    }

    /** Factory method from viewType/viewNumber/viewSize with auto ProductContentType lookup. */
    public static CategoryImageViewType from(Delegator delegator, String viewType, String viewNumber, String viewSize,
                                             boolean autoUpdateData, boolean emulateMissing, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (UtilValidate.isEmpty(viewType)) {
            throw new IllegalArgumentException("Missing viewType");
        }
        if (UtilValidate.isEmpty(viewNumber)) {
            throw new IllegalArgumentException("Missing viewNumber (pass 0 for main/original)");
        }
        if (UtilValidate.isEmpty(viewSize)) {
            throw new IllegalArgumentException("Missing viewSize");
        }
        GenericValue productCategoryContentType = delegator.from("ProductCategoryContentType").where("viewType", viewType,
                "viewNumber", viewNumber, "viewSize", viewSize).cache(useCache).queryFirst();
        if (productCategoryContentType == null) {
            if ((autoUpdateData || emulateMissing) && !"original".equals(viewSize)) {
                // FIXME: needs to work with client code
                GenericValue origProductContentType = delegator.from("ProductCategoryContentType")
                        .where("parentTypeId", "IMAGE_URL_FULL", "viewType", viewType, "viewNumber", viewNumber, "viewSize", "original")
                        .cache(useCache).queryFirst();
                if (origProductContentType == null) {
                    throw new IllegalArgumentException("ProductCategoryContentType not found for parentTypeId [IMAGE_URL_FULL] viewType [" +
                            viewType + "] viewNumber [" + viewNumber + "] viewSize [original] - cannot make variant ProductContentType");
                }
                productCategoryContentType = makeProductCategoryContentTypeFromFields(delegator, origProductContentType, viewType, viewNumber, viewSize);
                if (autoUpdateData) {
                    productCategoryContentType = productCategoryContentType.create();
                    Debug.logInfo("No ProductCategoryContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" +
                            viewSize + "]; auto-created: " + productCategoryContentType, module);
                }
            } else {
                throw new IllegalArgumentException("ProductContentType not found for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]");
            }
        }
        return new CategoryImageViewType(productCategoryContentType);
    }

    public static CategoryImageViewType from(Delegator delegator, String viewType, String viewNumber, String viewSize,
                                             boolean autoUpdateData, boolean useCache) throws GeneralException, IllegalArgumentException {
        return from(delegator, viewType, viewNumber, viewSize, autoUpdateData, false, useCache);
    }

    /** Factory method from ProductContentType, with backward-compatibility autoUpdateData. */
    public static CategoryImageViewType from(Delegator delegator, String prodCatContentTypeId, boolean autoUpdateData, boolean useCache) throws GeneralException, IllegalArgumentException {
        GenericValue productCategoryContentType = delegator.from("ProductCategoryContentType").where("prodCatContentTypeId", prodCatContentTypeId).cache(useCache).queryOne();
        if (productCategoryContentType == null) {
            throw new IllegalArgumentException("ProductCategoryContentType not found for prodCatContentTypeId [" + prodCatContentTypeId + "]");
        }
        return from(productCategoryContentType, useCache);
    }

    /** Factory method from ProductContentType. */
    public static CategoryImageViewType from(GenericValue productCategoryContentType, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        return new CategoryImageViewType(productCategoryContentType);
    }

    protected Delegator getDelegator() {
        return categoryContentType.getDelegator();
    }

    public GenericValue getCategoryContentType() {
        return categoryContentType;
    }

    public String getCategoryContentTypeId() {
        return categoryContentTypeId;
    }

    /** Returns the parent type ID, supposed to be IMAGE_URL_FULL, ORIGINAL_IMAGE_URL, ADDITIONAL_IMAGE_x or a custom "original" viewSize type. */
    public String getParentTypeId() {
        return categoryContentType.getString("parentTypeId");
    }

    /** Returns viewType (main, additional, ...). */
    public String getViewType() {
        return categoryContentType.getString("viewType");
    }

    /** Returns viewNumber (0 for original/main, 1-4 or higher for additional images). */
    public String getViewNumber() {
        return categoryContentType.getString("viewNumber");
    }

    public int getViewNumberInt() {
        return Integer.parseInt(getViewNumber());
    }

    /** Returns viewSize (original, large, 320x240, ...), also knows as sizeType. */
    public String getViewSize() {
        return categoryContentType.getString("viewSize");
    }

    public boolean isMain() {
        return "main".equals(getViewType());
    }

    public boolean isAdditional() {
        return "additional".equals(getViewType());
    }

    public boolean isZero() {
        return "0".equals(getViewNumber());
    }

    public boolean isOriginal() {
        return "original".equals(getViewSize());
    }

    @Override
    public String toString() {
        return "{prodCatContentTypeId='" + categoryContentTypeId + "'}'";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CategoryImageViewType that = (CategoryImageViewType) o;
        return getCategoryContentTypeId().equals(that.getCategoryContentTypeId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getCategoryContentTypeId());
    }

    public CategoryImageViewType getOriginal(boolean useCache) throws GeneralException {
        if (isOriginal()) {
            return this;
        }
        GenericValue pcct = getDelegator().from("ProductCategoryContentType").where("prodCatContentTypeId", getParentTypeId()).cache(useCache).queryOne();
        return CategoryImageViewType.from(pcct, true);
    }

    public String getOriginalProductContentTypeId(boolean useCache) throws GeneralException {
        if (isOriginal()) {
            return getCategoryContentTypeId();
        } else {
            return getParentTypeId();
        }
    }

    public List<GenericValue> getProductCategoryContentTypes(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<EntityCondition> orCondList = new ArrayList<>();

        if (isOriginal()) {
            if (includeOriginal) {
                orCondList.add(EntityCondition.makeCondition("productContentTypeId", getCategoryContentTypeId()));
            }
            orCondList.add(EntityCondition.makeCondition("parentTypeId", getCategoryContentTypeId()));
        } else if (getParentTypeId() != null && !"IMAGE_URL_FULL".equals(getParentTypeId())) {
            if (includeOriginal) {
                orCondList.add(EntityCondition.makeCondition("productContentTypeId", getParentTypeId()));
            }
            orCondList.add(EntityCondition.makeCondition("parentTypeId", getParentTypeId()));
        }

        List<GenericValue> pctList = getDelegator().from("ProductContentType").where(orCondList, EntityOperator.OR).queryList();
        return pctList != null ? pctList : Collections.emptyList();
    }

    public Map<String, GenericValue> getProductCategoryContentTypesById(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> pctList = getProductCategoryContentTypes(includeOriginal, useCache);
        if (pctList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, GenericValue> idMap = new LinkedHashMap<>();
        for(GenericValue pct : pctList) {
            idMap.put(pct.getString("productContentTypeId"), pct);
        }
        return idMap;
    }

    public Map<CategoryImageViewType, GenericValue> getProductCategoryContentTypesByViewType(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> pctList = getProductCategoryContentTypes(includeOriginal, useCache);
        if (pctList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<CategoryImageViewType, GenericValue> idMap = new LinkedHashMap<>();
        for(GenericValue pct : pctList) {
            idMap.put(CategoryImageViewType.from(pct, useCache), pct);
        }
        return idMap;
    }

    public List<String> getProductCategoryContentTypeIds(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> pctList = getProductCategoryContentTypes(includeOriginal, useCache);
        return pctList.isEmpty() ? Collections.emptyList() : pctList.stream().map(c -> c.getString("productContentTypeId")).collect(Collectors.toList());
    }

    public Map<String, GenericValue> getProductCategoryContentTypesByViewSize(boolean includeOriginal, boolean useCache) throws GeneralException {
        Map<String, GenericValue> pctMap;
        List<GenericValue> pctList = getProductCategoryContentTypes(includeOriginal, useCache);
        if (UtilValidate.isNotEmpty(pctList)) {
            pctMap = new LinkedHashMap<>();
            for(GenericValue pct : pctList) {
                pctMap.put(pct.getString("viewSize"), pct);
            }
        } else {
            pctMap = Collections.emptyMap();
        }
        return pctMap;
    }

    public static Map<String, GenericValue> getOriginalViewSizeProductCategoryContentTypes(Delegator delegator, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        Map<String, GenericValue> pctMap = new LinkedHashMap<>();

        String origPctId = "ORIGINAL_IMAGE_URL";
        GenericValue origPct = delegator.from("ProductCategoryContentType").where("prodCatContentTypeId", origPctId).cache(useCache).queryOne();
        if (origPct == null) {
            throw new IllegalArgumentException("Could not find ProductCategoryContentType [" + origPctId + "]");
        }
        pctMap.put(origPctId, origPct);

        List<GenericValue> pctList = delegator.from("ProductCategoryContentType")
                .where(EntityCondition.makeCondition(EntityCondition.makeCondition("viewSize", "original"),
                        EntityOperator.OR,
                        EntityCondition.makeCondition("productCategoryContentTypeId", EntityOperator.LIKE, "ADDITIONAL_IMAGE_%"))).cache(useCache).queryList();
        if (pctList != null) {
            for(GenericValue pct : pctList) {
                String pctId = pct.getString("productContentTypeId");
                if (!"ORIGINAL_IMAGE_URL".equals(pctId)) {
                    pctMap.put(pctId, pct);
                }
            }
        }

        return pctMap;
    }

    /** Implements compatibility mode for determining productCategoryContentTypeId from viewType/viewNumber/viewSize. */
    protected static String extractProductCategoryContentTypeId(Delegator delegator, String viewType, String viewNumber, String viewSize) {
        if ("main".equals(viewType)) {
            if ("original".equals(viewSize)) {
                if (!"0".equals(viewNumber)) {
                    throw new IllegalArgumentException("No or invalid ProductCategoryContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" +
                            viewSize + "] - viewNumber should be 0");
                }
                return "ORIGINAL_IMAGE_URL";
            } else {
                return viewSize.toUpperCase() + "_IMAGE_URL";
            }
        } else if ("additional".equals(viewType)) {
            if ("original".equals(viewSize)) {
                return "ADDITIONAL_IMAGE_" + viewNumber;
            } else {
                return "XTRA_IMG_" + viewNumber + "_" + viewSize.toUpperCase();
            }
        } else {
            throw new IllegalArgumentException("No ProductCategoryContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]");
        }
    }

    /** Implements compatibility mode for extracting viewType from productContentTypeId. */
    protected static String extractProductCategoryContentTypeIdViewType(Delegator delegator, String prodCatContentTypeId) throws IllegalArgumentException {
        if (prodCatContentTypeId.endsWith("_IMAGE_URL")) {
            return "main";
        } else if (prodCatContentTypeId.startsWith("ADDITIONAL_IMAGE_") || prodCatContentTypeId.startsWith("XTRA_IMG_")) {
            return "additional";
        } else {
            throw new IllegalArgumentException("Unrecognized image productCategoryContentTypeId for compatibility mode auto-determination: " + prodCatContentTypeId);
        }
    }

    /** Implements compatibility mode for extracting viewNumber from productContentTypeId. */
    protected static String extractProductCategoryContentTypeIdViewNumber(Delegator delegator, String prodCatContentTypeId) throws IllegalArgumentException {
        if (prodCatContentTypeId.endsWith("_IMAGE_URL")) {
            return "0";
        } else if (prodCatContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            return prodCatContentTypeId.substring("ADDITIONAL_IMAGE_".length());
        } else if (prodCatContentTypeId.startsWith("XTRA_IMG_")) {
            int sepIndex = prodCatContentTypeId.indexOf('_', "XTRA_IMG_".length());
            if (sepIndex <= 0) {
                throw new IllegalArgumentException("Unrecognized image prodCatContentTypeId for compatibility mode auto-determination (expected format: XTRA_IMG_%_%): " + prodCatContentTypeId);
            }
            return prodCatContentTypeId.substring("XTRA_IMG_".length(), sepIndex);
        } else {
            throw new IllegalArgumentException("Unrecognized image prodCatContentTypeId for compatibility mode auto-determination (expected %_IMAGE_URL, ADDITIONAL_IMAGE_%, XTRA_IMG_%_%): " + prodCatContentTypeId);
        }
    }

    /** Implements compatibility mode for extracting viewSize from productContentTypeId. */
    protected static String extractProductContentTypeIdViewSize(Delegator delegator, String prodCatContentTypeId) throws IllegalArgumentException {
        if (prodCatContentTypeId.equals("ORIGINAL_IMAGE_URL") || prodCatContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            return "original";
        } else if (prodCatContentTypeId.endsWith("_IMAGE_URL")) {
            return prodCatContentTypeId.substring(0, prodCatContentTypeId.length() - "_IMAGE_URL".length()).toLowerCase();
        } else if (prodCatContentTypeId.startsWith("XTRA_IMG_")) {
            return prodCatContentTypeId.substring(prodCatContentTypeId.indexOf('_', "XTRA_IMG_".length()) + 1).toLowerCase();
        } else {
            throw new IllegalArgumentException("Unrecognized image productContentTypeId for compatibility mode auto-determination: " + prodCatContentTypeId);
        }
    }

    protected static GenericValue makeProductCategoryContentTypeFromFields(Delegator delegator, GenericValue origProductCategoryContentType, String viewType,
                                                                           String viewNumber, String viewSize) throws GenericEntityException {
        Map<String, Object> exprCtx = UtilMisc.toMap("delegator", delegator, "origPctId", origProductCategoryContentType.get("productContentTypeId"),
                "viewType", viewType, "viewNumber", viewNumber, "viewSize", viewSize,
                "VIEWTYPE", viewType.toUpperCase(), "VIEWNUMBER", viewNumber.toUpperCase(), "VIEWSIZE", viewSize.toUpperCase());
        String productCategoryContentTypeId = FlexibleStringExpander.expandString(origProductCategoryContentType.getString("viewVariantId"), exprCtx);
        String description = FlexibleStringExpander.expandString(origProductCategoryContentType.getString("viewVariantDesc"), exprCtx);
        return delegator.makeValue("ProductCategoryContentType",
                "prodCatContentTypeId", productCategoryContentTypeId,
                "parentTypeId", determineParentTypeId(delegator, productCategoryContentTypeId, viewType, viewNumber, viewSize, false),
                "hasTable", "N",
                "description", description,
                "viewType", viewType,
                "viewNumber", viewNumber,
                "viewSize", viewSize);
    }

    protected static String determineParentTypeId(Delegator delegator, String productContentTypeId, String viewType,
                                                  String viewNumber, String viewSize, boolean useCache) throws GenericEntityException {
        if ("original".equals(viewSize)) {
            return "IMAGE_URL_FULL";
        }
        List<GenericValue> pctList = delegator.from("ProductContentType").where("parentTypeId", "IMAGE_URL_FULL",
                "viewType", viewType, "viewNumber", viewNumber, "viewSize", "original").cache(useCache).queryList();

        if (UtilValidate.isNotEmpty(pctList)) {
            if (pctList.size() > 1) {
                Debug.logWarning("Multiple ProductContentTypes found for parentTypeId [IMAGE_URL_FULL] viewType [" + viewType +
                        "] viewNumber [" + viewNumber + "] viewSize [original]; using first", module);
            }
            return pctList.get(0).getString("productContentTypeId");
        } else {
            Debug.logError("Could not determine ProductContentType parentTypeId for record update for productContentTypeId [" + productContentTypeId +
                    "] viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]; check data and productImageMigrateImageUrlProductContentTypeData service", module);
            return null;
        }

        /* backward-compatibility
        if ("main".equals(viewType)) {
            return "ORIGINAL_IMAGE_URL";
        } else if ("additional".equals(viewType)) {
            return "ADDITIONAL_IMAGE_" + viewNumber;
        } else {
            Debug.logError("Could not determine ProductContentType parentTypeId for record update for productContentTypeId [" + productContentTypeId +
                    "] viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]; data may need manual review", module);
            return null;
        }
         */
    }

}
