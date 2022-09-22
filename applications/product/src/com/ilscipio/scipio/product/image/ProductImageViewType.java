package com.ilscipio.scipio.product.image;

import com.ilscipio.scipio.category.image.CategoryImageViewType;
import com.ilscipio.scipio.content.image.ContentImageViewType;
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
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Essentially a wrapper for the new ProductContentType fields (SCIPIO).
 *
 * <p>NOTE: 2021-01: Client code custom records should define parentTypeId/viewType/viewNumber/viewSize directly on
 * ProductContentType - see .
 * Compatibility code is for stock code only and should not be needed otherwise. The service productImageMigrateImageUrlProductContentTypeData
 * migrates old stock data (not custom client definitions).</p>
 */
public class ProductImageViewType extends ContentImageViewType implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final GenericValue productContentType;
    protected final String productContentTypeId;

    protected ProductImageViewType(GenericValue productContentType) throws IllegalArgumentException {
        super(productContentType, "ProductContentType", "productContentTypeId");
        this.productContentType = productContentType;
        this.productContentTypeId = productContentType.getString("productContentTypeId");
    }

    /**
     * Factory method from viewType/viewNumber/viewSize with auto ProductContentType lookup.
     */
    public static ProductImageViewType from(Delegator delegator, String viewType, String viewNumber, String viewSize,
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
        GenericValue productContentType = delegator.from("ProductContentType").where("viewType", viewType,
                "viewNumber", viewNumber, "viewSize", viewSize).cache(useCache).queryFirst();
        if (productContentType == null) {
            if ((autoUpdateData || emulateMissing) && !"original".equals(viewSize)) {
                // FIXME: needs to work with client code
                //productContentTypeId = extractProductContentTypeId(delegator, viewType, viewNumber, viewSize);
                GenericValue origProductContentType = delegator.from("ProductContentType")
                        .where("parentTypeId", "IMAGE_URL_FULL", "viewType", viewType, "viewNumber", viewNumber, "viewSize", "original")
                        .cache(useCache).queryFirst();
                if (origProductContentType == null) {
                    throw new IllegalArgumentException("ProductContentType not found for parentTypeId [IMAGE_URL_FULL] viewType [" +
                            viewType + "] viewNumber [" + viewNumber + "] viewSize [original] - cannot make variant ProductContentType");
                }
                productContentType = makeProductContentTypeFromFields(delegator, origProductContentType, viewType, viewNumber, viewSize);
                if (autoUpdateData) {
                    productContentType = productContentType.create();
                    Debug.logInfo("No ProductContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" +
                            viewSize + "]; auto-created: " + productContentType, module);
                }
            } else {
                throw new IllegalArgumentException("ProductContentType not found for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]");
            }
        }
        return new ProductImageViewType(productContentType);
    }

    public static ProductImageViewType from(Delegator delegator, String viewType, String viewNumber, String viewSize,
                                            boolean autoUpdateData, boolean useCache) throws GeneralException, IllegalArgumentException {
        return from(delegator, viewType, viewNumber, viewSize, autoUpdateData, false, useCache);
    }

    /**
     * Factory method from ProductContentType, with backward-compatibility autoUpdateData.
     */
    public static ProductImageViewType from(Delegator delegator, String productContentTypeId, boolean autoUpdateData, boolean useCache) throws GeneralException, IllegalArgumentException {
        GenericValue productContentType = delegator.from("ProductContentType").where("productContentTypeId", productContentTypeId).cache(useCache).queryOne();
        if (productContentType == null) {
            throw new IllegalArgumentException("ProductContentType not found for productContentTypeId [" + productContentTypeId + "]");
        }
        return from(productContentType, useCache);
    }

    /**
     * Factory method from ProductContentType.
     */
    public static ProductImageViewType from(GenericValue productContentType, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        return new ProductImageViewType(productContentType);
    }

    public ProductImageViewType getOriginal(boolean useCache) throws GeneralException {
        if (isOriginal()) {
            return this;
        }
        GenericValue pct = getDelegator().from("ProductContentType").where("productContentTypeId", getParentTypeId()).cache(useCache).queryOne();
        return ProductImageViewType.from(pct, true);
    }

    @Override
    public Map<ContentImageViewType, GenericValue> getContentTypesByViewType(boolean includeOriginal, boolean useCache) throws GeneralException {
        List<GenericValue> ctList = getContentTypes(includeOriginal, useCache);
        if (ctList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<ContentImageViewType, GenericValue> idMap = new LinkedHashMap<>();
        for (GenericValue ct : ctList) {
            idMap.put(CategoryImageViewType.from(ct, useCache), ct);
        }
        return idMap;
    }

    public static Map<String, GenericValue> getOriginalViewSizeProductContentTypes(Delegator delegator, boolean useCache) throws GenericEntityException, IllegalArgumentException {
        Map<String, GenericValue> pctMap = new LinkedHashMap<>();

        String origPctId = "ORIGINAL_IMAGE_URL";
        GenericValue origPct = delegator.from("ProductContentType").where("productContentTypeId", origPctId).cache(useCache).queryOne();
        if (origPct == null) {
            throw new IllegalArgumentException("Could not find ProductContentType [" + origPctId + "]");
        }
        pctMap.put(origPctId, origPct);

        List<GenericValue> pctList = delegator.from("ProductContentType")
                .where(EntityCondition.makeCondition(EntityCondition.makeCondition("viewSize", "original"),
                        EntityOperator.OR,
                        EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, "ADDITIONAL_IMAGE_%"))).cache(useCache).queryList();
        if (pctList != null) {
            for (GenericValue pct : pctList) {
                String pctId = pct.getString("productContentTypeId");
                if (!"ORIGINAL_IMAGE_URL".equals(pctId)) {
                    pctMap.put(pctId, pct);
                }
            }
        }

        return pctMap;
    }

    /**
     * Implements compatibility mode for determining productContentTypeId from viewType/viewNumber/viewSize.
     */
    protected static String extractProductContentTypeId(Delegator delegator, String viewType, String viewNumber, String viewSize) {
        if ("main".equals(viewType)) {
            if ("original".equals(viewSize)) {
                if (!"0".equals(viewNumber)) {
                    throw new IllegalArgumentException("No or invalid ProductContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" +
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
            throw new IllegalArgumentException("No ProductContentType for viewType [" + viewType + "] viewNumber [" + viewNumber + "] viewSize [" + viewSize + "]");
        }
    }

    /**
     * Implements compatibility mode for extracting viewType from productContentTypeId.
     */
    protected static String extractProductContentTypeIdViewType(Delegator delegator, String productContentTypeId) throws IllegalArgumentException {
        if (productContentTypeId.endsWith("_IMAGE_URL")) {
            return "main";
        } else if (productContentTypeId.startsWith("ADDITIONAL_IMAGE_") || productContentTypeId.startsWith("XTRA_IMG_")) {
            return "additional";
        } else {
            throw new IllegalArgumentException("Unrecognized image productContentTypeId for compatibility mode auto-determination: " + productContentTypeId);
        }
    }

    /**
     * Implements compatibility mode for extracting viewNumber from productContentTypeId.
     */
    protected static String extractProductContentTypeIdViewNumber(Delegator delegator, String productContentTypeId) throws IllegalArgumentException {
        if (productContentTypeId.endsWith("_IMAGE_URL")) {
            return "0";
        } else if (productContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            return productContentTypeId.substring("ADDITIONAL_IMAGE_".length());
        } else if (productContentTypeId.startsWith("XTRA_IMG_")) {
            int sepIndex = productContentTypeId.indexOf('_', "XTRA_IMG_".length());
            if (sepIndex <= 0) {
                throw new IllegalArgumentException("Unrecognized image productContentTypeId for compatibility mode auto-determination (expected format: XTRA_IMG_%_%): " + productContentTypeId);
            }
            return productContentTypeId.substring("XTRA_IMG_".length(), sepIndex);
        } else {
            throw new IllegalArgumentException("Unrecognized image productContentTypeId for compatibility mode auto-determination (expected %_IMAGE_URL, ADDITIONAL_IMAGE_%, XTRA_IMG_%_%): " + productContentTypeId);
        }
    }

    /**
     * Implements compatibility mode for extracting viewSize from productContentTypeId.
     */
    protected static String extractProductContentTypeIdViewSize(Delegator delegator, String productContentTypeId) throws IllegalArgumentException {
        if (productContentTypeId.equals("ORIGINAL_IMAGE_URL") || productContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            return "original";
        } else if (productContentTypeId.endsWith("_IMAGE_URL")) {
            return productContentTypeId.substring(0, productContentTypeId.length() - "_IMAGE_URL".length()).toLowerCase();
        } else if (productContentTypeId.startsWith("XTRA_IMG_")) {
            return productContentTypeId.substring(productContentTypeId.indexOf('_', "XTRA_IMG_".length()) + 1).toLowerCase();
        } else {
            throw new IllegalArgumentException("Unrecognized image productContentTypeId for compatibility mode auto-determination: " + productContentTypeId);
        }
    }

    /* Migration and backward-compatibility, see ProductTypeData.xml
    protected static void ensureParentProductContentTypes(Delegator delegator) throws GenericEntityException {
        GenericValue baseImagePct = delegator.findOne("ProductContentType", UtilMisc.toMap("productContentTypeId", "IMAGE_URL_BASE"), false);
        if (baseImagePct == null) {
            baseImagePct = delegator.makeValue("ProductContentType", "productContentTypeId", "IMAGE_URL_BASE",
                    "hasTable", "N", "description", "Image - Base").create();
            Debug.logInfo("Created missing ProductContentType: " + baseImagePct, module);
        }
        GenericValue fullImagePct = delegator.findOne("ProductContentType", UtilMisc.toMap("productContentTypeId", "IMAGE_URL_FULL"), false);
        if (fullImagePct == null) {
            fullImagePct = delegator.makeValue("ProductContentType", "productContentTypeId", "IMAGE_URL_FULL",
                    "hasTable", "N", "description", "Image - Full", "parentTypeId", "IMAGE_URL_BASE").create();
            Debug.logInfo("Created missing ProductContentType: " + fullImagePct, module);
        }
        // The parentTypeId for variants is now the original image, to simplify lookups
        //GenericValue variantImagePct = delegator.findOne("ProductContentType", UtilMisc.toMap("productContentTypeId", "IMAGE_URL_VARIANT"), false);
        //if (variantImagePct == null) {
        //    variantImagePct = delegator.makeValue("ProductContentType", "productContentTypeId", "IMAGE_URL_VARIANT",
        //            "hasTable", "N", "description", "Image - Variant", "parentTypeId", "IMAGE_URL_BASE").create();
        //    Debug.logInfo("Created missing ProductContentType: " + variantImagePct, module);
        //}
    }
    */

    /* Migration and backward-compatibility, see ProductTypeData.xml
    protected static GenericValue ensureProductContentTypeFields(Delegator delegator, String productContentTypeId, String viewType,
                                                                 String viewNumber, String viewSize) throws GenericEntityException {
        // Re-query to bypass caching
        ensureParentProductContentTypes(delegator);
        GenericValue productContentType = delegator.from("ProductContentType").where("productContentTypeId", productContentTypeId).queryOne();
        if (productContentType == null) {
            throw new IllegalArgumentException("ProductContentType productContentTypeId [" + productContentTypeId + "] not found");
        }
        boolean modified = false;
        if (productContentType.get("parentTypeId") == null || "IMAGE_URL_VARIANT".equals(productContentType.get("parentTypeId"))) {
            // NOTE: IMAGE_URL_VARIANT was a temporary value and has been replaced by a reference to the original image as parent, to help with lookups
            productContentType.set("parentTypeId", determineParentTypeId(delegator, productContentTypeId, viewType, viewNumber, viewSize, false));
            modified = true;
        }
        if (productContentType.get("viewType") == null) {
            productContentType.set("viewType", viewType);
            modified = true;
        }
        if (productContentType.get("viewNumber") == null) {
            productContentType.set("viewNumber", viewNumber);
            modified = true;
        }
        if (productContentType.get("viewSize") == null) {
            productContentType.set("viewSize", viewSize);
            modified = true;
        }
        if (modified) {
            productContentType.store();
            Debug.logWarning("ProductContentType productContentTypeId [" + productContentTypeId + "] was missing parentTypeId/viewType/viewNumber/viewSize" +
                    " on record; auto-updated: " + productContentType, module);
        }
        return productContentType;
    }
     */

    protected static GenericValue makeProductContentTypeFromFields(Delegator delegator, GenericValue origProductContentType, String viewType,
                                                                   String viewNumber, String viewSize) throws GenericEntityException {
        //ensureParentProductContentTypes(delegator);
        Map<String, Object> exprCtx = UtilMisc.toMap("delegator", delegator, "origPctId", origProductContentType.get("productContentTypeId"),
                "viewType", viewType, "viewNumber", viewNumber, "viewSize", viewSize,
                "VIEWTYPE", viewType.toUpperCase(), "VIEWNUMBER", viewNumber.toUpperCase(), "VIEWSIZE", viewSize.toUpperCase());
        String productContentTypeId = FlexibleStringExpander.expandString(origProductContentType.getString("viewVariantId"), exprCtx);
        String description = FlexibleStringExpander.expandString(origProductContentType.getString("viewVariantDesc"), exprCtx);
        return delegator.makeValue("ProductContentType",
                "productContentTypeId", productContentTypeId,
                "parentTypeId", determineParentTypeId(delegator, productContentTypeId, viewType, viewNumber, viewSize, false),
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
