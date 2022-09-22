package com.ilscipio.scipio.product.image;


import com.ilscipio.scipio.content.image.ContentImageLocationInfo;
import com.ilscipio.scipio.content.image.ContentImageLocationInfoFactory;
import com.ilscipio.scipio.content.image.ContentImageServices;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceContext;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;

/**
 * Product image sizeType info parser, based on (duplicated from) {@link ContentImageServices#contentImageFileScaleInAllSizeCore(ServiceContext)}.
 * NOTE: Currently represents only default file locations, which in some circumstances are not respected in data (such as original file locations).
 * WARN: Subject to refactoring.
 */
public class ProductImageLocationInfo extends ContentImageLocationInfo implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final ProductImageLocationInfo.Factory FACTORY = (ProductImageLocationInfo.Factory) ProductImageLocationInfo.Factory.getInstance("product.image.location.info.factory");

    protected GenericValue product;

    protected ProductImageLocationInfo(DispatchContext dctx, String productId, ProductImageViewType imageViewType,
                                       ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                                       boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException {
        super(dctx, productId, imageViewType, variantConfig, imagePath, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    public static ContentImageLocationInfoFactory<ProductImageLocationInfo, ProductImageViewType> getFactory(DispatchContext dctx) {
        return FACTORY;
    }

    public static class Factory extends ContentImageLocationInfoFactory<ProductImageLocationInfo, ProductImageViewType> {
        @Override
        public ProductImageLocationInfo make(DispatchContext dctx, String id, ProductImageViewType imageViewType, ImageVariantConfig variantConfig,
                                             String imagePath, Collection sizeTypeList, boolean useEntityCache, boolean useProfileCache,
                                             Map extraParams) throws IllegalArgumentException {
            return new ProductImageLocationInfo(dctx, id, imageViewType, variantConfig, imagePath, sizeTypeList,
                    useEntityCache, useProfileCache, extraParams);
        }

        @Override
        public ProductImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue product, ProductImageViewType imageViewType,
                                             String imageUrl, Collection<String> sizeTypeList,
                                             Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
            Delegator delegator = dctx.getDelegator();
            String productContentTypeId = imageViewType.getContentTypeId();
            String origProductContentTypeId = imageViewType.getOriginal(true).getContentTypeId();
            if (locale == null) {
                locale = Locale.getDefault();
            }
            String productId = product.getString("productId");

            ContentImageLocationInfo.ImageContentInfo imageUrlInfo = ImageContentInfo.from(dctx, locale, product, productContentTypeId, imageUrl, useParentImageUrl, useEntityCache);
            if (imageUrlInfo.getImageUrl() == null) {
                Debug.logError("Could not determine image path or URL for product [" + productId + "] productContentTypeId [" + productContentTypeId + "]", module);
                return null;
            }
            imageUrl = imageUrlInfo.getImageUrl();

            ImageProfile imageProfile = ProductImageWorker.getProductImageProfileOrDefault(delegator, origProductContentTypeId, product, imageUrlInfo.getContent(), useEntityCache, useProfileCache);
            if (imageProfile == null) {
                Debug.logError("Could not find media profile for product [" + productId + "] productContentTypeId [" + productContentTypeId + "]", module);
                return null;
            }
            return from(dctx, productId, imageViewType, imageProfile, imageUrl, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }
    }

    public static ProductImageLocationInfo from(DispatchContext dctx, String productId, ProductImageViewType imageViewType,
                                                ImageVariantConfig variantConfig, String imageFilename,
                                                Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, productId, imageViewType, variantConfig, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    public static ProductImageLocationInfo from(DispatchContext dctx, String productId, ProductImageViewType imageViewType,
                                                ImageProfile imageProfile, String imageFilename,
                                                Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, productId, imageViewType, imageProfile, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    /**
     * Returns ProductImageLocationInfo or null if no image URL/location/variants applicable for the given product/productContentTypeId.
     * If passed productContent or imageUrl null attempts to determine from data.
     * Based on productImageAutoRescale (TODO?: deduplicate).
     */
    public static ProductImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue product, ProductImageViewType imageViewType,
                                                String imageUrl, Collection<String> sizeTypeList,
                                                Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, locale, product, imageViewType, imageUrl, sizeTypeList, useParentImageUrl, useEntityCache, useProfileCache, extraParams);
    }

    public String getProductId() throws GeneralException {
        return id;
    }

    public String getProductContentTypeId() throws GeneralException {
        return getImageViewType().getContentTypeId();
    }

    @Override
    protected Map<String, Object> makeImagePathArg(Map<String, Object> imagePathArgs) throws GeneralException {
        ProductImageViewType imageViewType = (ProductImageViewType) super.getImageViewType();
        String imageFnFmt = getImageFnFmtExpr();

        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();
        String id = this.id;

        if (getImageViewType().isMain()) {
            UtilMisc.put(imagePathArgs, "location", "products", "id", id, "type", "original");
        } else {
            if (imageFnFmt.endsWith("${id}")) {
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }
            UtilMisc.put(imagePathArgs, "location", "products", "id", id, "viewtype", viewType, "sizetype", "original");
        }

        imagePathArgs.put("tenantId", getDelegator().getDelegatorTenantId());
        return imagePathArgs;
    }

    public GenericValue getProduct() throws GeneralException {
        GenericValue product = this.product;
        if (product == null) {
            product = getDelegator().findOne("Product", UtilMisc.toMap("productId", getProductId()), false);
            this.product = product;
        }
        return (GenericValue.NULL_VALUE != product) ? product : null;
    }

    public static class ImageContentInfo extends ContentImageLocationInfo.ImageContentInfo {

        protected ImageContentInfo(String imageUrl, GenericValue entityContent, GenericValue content) {
            super(imageUrl, entityContent, content);
        }

        public static ContentImageLocationInfo.ImageContentInfo from(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId, String imageUrl, Boolean useParentImageUrl, boolean useEntityCache) throws GeneralException {
            String productId = product.getString("productId");
            GenericValue content = null;
            GenericValue productContent = dctx.getDelegator().from("ProductContent").where("productId", productId,
                    "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().cache(useEntityCache).queryFirst();
            String inlineImageUrl = null;
            if (productContent != null) {
                content = productContent.getRelatedOne("Content", useEntityCache);
            } else {
                String productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
                ModelEntity productModel = dctx.getDelegator().getModelEntity("Product");
                if (productModel.isField(productFieldName)) {
                    inlineImageUrl = product.getString(productFieldName);
                } else {
                    // May happen normally due to ADDITIONAL_IMAGE_x
                    //Debug.logError(logPrefix+"Invalid productContentTypeId [" + productContentTypeId
                    //        + "] for product [" + productId + "] for resize operation (parent products not consulted)", module);
                    //return null;
                }
            }

            if (imageUrl == null) {
                if (Boolean.TRUE.equals(useParentImageUrl)) {
                    // NOTE: this consults the parent product which we don't want, but in known cases should return right value
                    imageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                            locale, dctx.getDispatcher(), useEntityCache, "raw");
                } else {
                    if (content != null) {
                        imageUrl = ProductImageWorker.getDataResourceImageUrl(
                                dctx.getDelegator().from("DataResource").where("dataResourceId", content.get("dataResourceId")).queryOne(), useEntityCache);
                    } else if (inlineImageUrl != null) {
                        imageUrl = inlineImageUrl;
                    }
                }
            }
            if (UtilValidate.isEmpty(imageUrl)) {
                imageUrl = null;
            }
            return new ImageContentInfo(imageUrl, productContent, content);
        }
    }
}
