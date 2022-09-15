package com.ilscipio.scipio.category.image;

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
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceContext;

import java.io.Serializable;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;

/**
 * Category image sizeType info parser, based on (duplicated from) {@link ContentImageServices#contentImageFileScaleInAllSizeCore(ServiceContext)}.
 * NOTE: Currently represents only default file locations, which in some circumstances are not respected in data (such as original file locations).
 * WARN: Subject to refactoring.
 */
public class CategoryImageLocationInfo extends ContentImageLocationInfo implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Factory FACTORY = (Factory) Factory.getInstance("category.image.location.info.factory");

    private GenericValue productCategory;


    protected CategoryImageLocationInfo(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                        ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                                        boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException {
        super(dctx, productCategoryId, imageViewType, variantConfig, imagePath, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }


    public static class Factory extends ContentImageLocationInfoFactory<CategoryImageLocationInfo, CategoryImageViewType> {
        @Override
        public CategoryImageLocationInfo make(DispatchContext dctx, String id, CategoryImageViewType imageViewType, ImageVariantConfig variantConfig,
                                              String imagePath, Collection sizeTypeList, boolean useEntityCache, boolean useProfileCache,
                                              Map extraParams) throws IllegalArgumentException {
            return new CategoryImageLocationInfo(dctx, id, imageViewType, variantConfig, imagePath, sizeTypeList,
                    useEntityCache, useProfileCache, extraParams);
        }

        @Override
        public CategoryImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue mainEntityValue, CategoryImageViewType imageViewType,
                                              String imageUrl, Collection<String> sizeTypeList, Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
            Delegator delegator = dctx.getDelegator();
            String prodCatContentTypeId = imageViewType.getContentTypeId();
            String origCategoryContentTypeId = imageViewType.getOriginal(true).getContentTypeId();
            if (locale == null) {
                locale = Locale.getDefault();
            }
            String productCategoryId = mainEntityValue.getString("productCategoryId");

            ContentImageLocationInfo.ImageContentInfo imageUrlInfo = ImageContentInfo.from(dctx, locale, mainEntityValue, prodCatContentTypeId, imageUrl, useParentImageUrl, useEntityCache);
            if (imageUrlInfo.getImageUrl() == null) {
                Debug.logError("Could not determine image path or URL for product [" + productCategoryId + "] productContentTypeId [" + prodCatContentTypeId + "]", module);
                return null;
            }
            imageUrl = imageUrlInfo.getImageUrl();

            ImageProfile imageProfile = com.ilscipio.scipio.category.image.CategoryImageWorker.getCategoryImageProfileOrDefault(delegator, origCategoryContentTypeId, mainEntityValue, imageUrlInfo.getContent(), useEntityCache, useProfileCache);
            if (imageProfile == null) {
                Debug.logError("Could not find media profile for category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "]", module);
                return null;
            }
            return from(dctx, productCategoryId, imageViewType, imageProfile, imageUrl, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }
    }

    public static ContentImageLocationInfoFactory<CategoryImageLocationInfo, CategoryImageViewType> getFactory(DispatchContext dctx) {
        return FACTORY;
    }


    public static CategoryImageLocationInfo from(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                                 ImageVariantConfig variantConfig, String imageFilename,
                                                 Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, productCategoryId, imageViewType, variantConfig, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    public static CategoryImageLocationInfo from(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                                 ImageProfile imageProfile, String imageFilename,
                                                 Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, productCategoryId, imageViewType, imageProfile, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    /**
     * Returns CategoryImageLocationInfo or null if no image URL/location/variants applicable for the given product/productContentTypeId.
     * If passed productContent or imageUrl null attempts to determine from data.
     * Based on productImageAutoRescale (TODO?: deduplicate).
     */
    public static CategoryImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue productCategory, CategoryImageViewType imageViewType,
                                                 String imageUrl, Collection<String> sizeTypeList,
                                                 Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, locale, productCategory, imageViewType, imageUrl, sizeTypeList, useParentImageUrl, useEntityCache, useProfileCache, extraParams);
    }


    public String getProductCategoryId() throws GeneralException {
        return id;
    }

    public String getProdCatContentTypeId() throws GeneralException {
        return getImageViewType().getContentTypeId();
    }

    @Override
    protected Map<String, Object> makeImagePathArg(Map<String, Object> imagePathArgs) throws GeneralException {
        CategoryImageViewType imageViewType = (CategoryImageViewType) super.getImageViewType();
        String imageFnFmt = getImageFnFmtExpr();

        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();
        String id = this.id;

        if (getImageViewType().isMain()) {
            UtilMisc.put(imagePathArgs,"location", "categories", "id", id, "type", "original");
        } else {
            if (imageFnFmt.endsWith("${id}")) {
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }
            UtilMisc.put(imagePathArgs, "location", "categories", "id", id, "viewtype", viewType, "sizetype", "original");
        }

        imagePathArgs.put("tenantId", getDelegator().getDelegatorTenantId());
        return imagePathArgs;
    }

    public GenericValue getProductCategory() throws GeneralException {
        GenericValue productCategory = this.productCategory;
        if (productCategory == null) {
            productCategory = getDelegator().findOne("ProductCategory", UtilMisc.toMap("productCategoryId", getProductCategoryId()), false);
            this.productCategory = productCategory;
        }
        return (GenericValue.NULL_VALUE != productCategory) ? productCategory : null;
    }


    public static class ImageContentInfo extends ContentImageLocationInfo.ImageContentInfo {

        public ImageContentInfo(String imageUrl, GenericValue entityContent, GenericValue content) {
            super(imageUrl, entityContent, content);
        }

        public static ContentImageLocationInfo.ImageContentInfo from(DispatchContext dctx, Locale locale, GenericValue productCategory, String prodCatContentTypeId, String imageUrl, Boolean useParentImageUrl, boolean useEntityCache) throws GeneralException {
            String productCategoryId = productCategory.getString("productCategoryId");
            GenericValue content = null;
            GenericValue productCategoryContent = dctx.getDelegator().from("ProductCategoryContent").where("productCategoryId", productCategoryId,
                    "productContentTypeId", prodCatContentTypeId).orderBy("-fromDate").filterByDate().cache(useEntityCache).queryFirst();
            String inlineImageUrl = null;
            if (productCategoryContent != null) {
                content = productCategoryContent.getRelatedOne("Content", useEntityCache);
            } else {
                String productCategoryFieldName = ModelUtil.dbNameToVarName(prodCatContentTypeId);
                ModelEntity productCategoryModel = dctx.getDelegator().getModelEntity("ProductCategory");
                if (productCategoryModel.isField(productCategoryFieldName)) {
                    inlineImageUrl = productCategory.getString(productCategoryFieldName);
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
                    imageUrl = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, prodCatContentTypeId,
                            locale, dctx.getDispatcher(), useEntityCache, "raw");
                } else {
                    if (content != null) {
                        imageUrl = CategoryImageWorker.getDataResourceImageUrl(
                                dctx.getDelegator().from("DataResource").where("dataResourceId", content.get("dataResourceId")).queryOne(), useEntityCache);
                    } else if (inlineImageUrl != null) {
                        imageUrl = inlineImageUrl;
                    }
                }
            }
            if (UtilValidate.isEmpty(imageUrl)) {
                imageUrl = null;
            }
            return new ImageContentInfo(imageUrl, productCategoryContent, content);
        }

    }

}
