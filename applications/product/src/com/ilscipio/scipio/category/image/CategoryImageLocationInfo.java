package com.ilscipio.scipio.category.image;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.content.image.ContentImageServices;
import com.ilscipio.scipio.product.image.ProductImageWorker;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.service.ServiceContext;

import java.io.File;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Category image sizeType info parser, based on (duplicated from) {@link ContentImageServices#contentImageFileScaleInAllSizeCore(ServiceContext)}.
 * NOTE: Currently represents only default file locations, which in some circumstances are not respected in data (such as original file locations).
 * WARN: Subject to refactoring.
 */
public class CategoryImageLocationInfo implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final CategoryImageLocationInfo NULL = new CategoryImageLocationInfo();
    private static final Factory FACTORY = readFactory();

    private transient Delegator delegator;
    private final String delegatorName;
    private transient LocalDispatcher dispatcher;
    private final String dispatcherName;
    protected final String productCategoryId;
    protected final CategoryImageViewType imageViewType;
    protected final ImageVariantConfig variantConfig;
    protected final String imagePath; // may be path or URL
    protected final String imageFilename;
    protected Collection<String> sizeTypeList;
    protected final boolean useEntityCache;
    protected final boolean useProfileCache;

    protected String imageExtension;
    protected String imageServerPathExpr;
    protected String imageServerPath;
    protected String imageUrlPrefixExpr;
    protected String imageUrlPrefix;
    protected String imageFnFmtExpr;
    protected Map<String, Object> imagePathArgs;
    protected Map<String, VariantLocation> sizeTypeInfoMap;

    protected GenericValue productCategory;

    protected CategoryImageLocationInfo() {
        this.delegatorName = null;
        this.dispatcherName = null;
        this.productCategoryId = null;
        this.imageViewType = null;
        this.variantConfig = null;
        this.imagePath = null;
        this.imageFilename = null;
        this.sizeTypeList = null;
        this.useEntityCache = false;
        this.useProfileCache = false;
    }

    protected CategoryImageLocationInfo(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                        ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                                        boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException {
        this.delegator = dctx.getDelegator();
        this.delegatorName = this.delegator.getDelegatorName();
        this.dispatcher = dctx.getDispatcher();
        this.dispatcherName = this.dispatcher.getName();
        this.productCategoryId = productCategoryId;
        this.imageViewType = imageViewType;
        this.variantConfig = variantConfig;
        this.imagePath = imagePath;
        String imageFilename = imagePath;
        if (imageFilename.lastIndexOf("/") != -1) {
            imageFilename = imageFilename.substring(imageFilename.lastIndexOf("/") + 1);
        }
        if (imageFilename.lastIndexOf(".") <= 0 || imageFilename.lastIndexOf(".") >= (imageFilename.length() - 1)) { // SCIPIO: added this to prevent more serious problems
            throw new IllegalArgumentException("Original image filename [" + imagePath + "] has missing or improper file extension (image type)");
        }
        this.imageFilename = imageFilename;
        this.sizeTypeList = sizeTypeList;
        this.useEntityCache = useEntityCache;
        this.useProfileCache = useProfileCache;
    }

    public static Factory getFactory(DispatchContext dctx) {
        return FACTORY;
    }

    private static Factory readFactory() {
        String clsName = UtilProperties.getPropertyValue("catalog", "category.image.location.info.factory");
        if (UtilValidate.isEmpty(clsName)) {
            return new Factory();
        }
        try {
            Class<?> cls = Class.forName(clsName);
            return (Factory) cls.getConstructor().newInstance();
        } catch (Exception e) {
            Debug.logError(e, "Invalid catalog#product.image.location.info.factory", module);
            return new Factory();
        }
    }

    public static class Factory {
        public CategoryImageLocationInfo make(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                              ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                                              boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException {
            return new CategoryImageLocationInfo(dctx, productCategoryId, imageViewType, variantConfig, imagePath, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }

        public CategoryImageLocationInfo from(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                              ImageVariantConfig variantConfig, String imageFilename,
                                              Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
            return make(dctx, productCategoryId, imageViewType, variantConfig, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }

        public CategoryImageLocationInfo from(DispatchContext dctx, String productCategoryId, CategoryImageViewType imageViewType,
                                              ImageProfile imageProfile, String imageFilename,
                                              Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
            // NOTE: this currently calls the non-cached readVariantConfig, because this is intended for backend
            return from(dctx, productCategoryId, imageViewType, (imageProfile != null) ?
                            (useProfileCache ? imageProfile.getVariantConfig() : imageProfile.readVariantConfig()) : null,
                    imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }

        /**
         * Returns CategoryImageLocationInfo or null if no image URL/location/variants applicable for the given category/prodCatContentTypeId.
         * If passed productCategoryContent or imageUrl null attempts to determine from data.
         * Based on productImageAutoRescale (TODO?: deduplicate).
         */
        public CategoryImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue productCategory, CategoryImageViewType imageViewType,
                                              String imageUrl, Collection<String> sizeTypeList,
                                              Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
            Delegator delegator = dctx.getDelegator();
            String prodCatContentTypeId = imageViewType.getCategoryContentTypeId();
            String origCategoryContentTypeId = imageViewType.getOriginal(true).getCategoryContentTypeId();
            if (locale == null) {
                locale = Locale.getDefault();
            }
            String productCategoryId = productCategory.getString("productCategoryId");

            ImageContentInfo imageUrlInfo = ImageContentInfo.from(dctx, locale, productCategory, prodCatContentTypeId, imageUrl, useParentImageUrl, useEntityCache);
            if (imageUrlInfo.getImageUrl() == null) {
                Debug.logError("Could not determine image path or URL for product [" + productCategoryId + "] productContentTypeId [" + prodCatContentTypeId + "]", module);
                return null;
            }
            imageUrl = imageUrlInfo.getImageUrl();

            ImageProfile imageProfile = CategoryImageWorker.getCategoryImageProfileOrDefault(delegator, origCategoryContentTypeId, productCategory, imageUrlInfo.getContent(), useEntityCache, useProfileCache);
            if (imageProfile == null) {
                Debug.logError("Could not find media profile for category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "]", module);
                return null;
            }
            return from(dctx, productCategoryId, imageViewType, imageProfile, imageUrl, sizeTypeList, useEntityCache, useProfileCache, extraParams);
        }
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
     * Returns ProductImageLocationInfo or null if no image URL/location/variants applicable for the given product/productContentTypeId.
     * If passed productContent or imageUrl null attempts to determine from data.
     * Based on productImageAutoRescale (TODO?: deduplicate).
     */
    public static CategoryImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue productCategory, CategoryImageViewType imageViewType,
                                                 String imageUrl, Collection<String> sizeTypeList,
                                                 Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return getFactory(dctx).from(dctx, locale, productCategory, imageViewType, imageUrl, sizeTypeList, useParentImageUrl, useEntityCache, useProfileCache, extraParams);
    }

    public boolean isNull() throws GeneralException {
        return productCategoryId == null;
    }

    public Delegator getDelegator() throws GeneralException {
        Delegator delegator = this.delegator;
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator(getDelegatorName());
            this.delegator = delegator;
        }
        return delegator;
    }

    public String getDelegatorName() throws GeneralException {
        return delegatorName;
    }

    public LocalDispatcher getDispatcher() throws GeneralException {
        LocalDispatcher dispatcher = this.dispatcher;
        if (dispatcher == null) {
            dispatcher = ServiceContainer.getLocalDispatcher(getDispatcherName(), this.getDelegator());
            this.dispatcher = dispatcher;
        }
        return dispatcher;
    }

    public String getDispatcherName() throws GeneralException {
        return dispatcherName;
    }

    public DispatchContext getDctx() throws GeneralException {
        return getDispatcher().getDispatchContext();
    }

    public String getProductCategoryId() throws GeneralException {
        return productCategoryId;
    }

    public String getProductContentTypeId() throws GeneralException {
        return getImageViewType().getCategoryContentTypeId();
    }

    public ImageVariantConfig getVariantConfig() throws GeneralException {
        return variantConfig;
    }

    /** Returns either a URL or a file path (used to extract {@link #getImageFilename()}, unreliable. */
    public String getImagePath() throws GeneralException {
        return imagePath;
    }

    public String getImageFilename() throws GeneralException {
        return imageFilename;
    }

    public Collection<String> getSizeTypeList() throws GeneralException {
        Collection<String> sizeTypeList = this.sizeTypeList;
        if (sizeTypeList == null) {
            if (getVariantConfig() != null) {
                sizeTypeList = getVariantConfig().getVariantNames();
            } else {
                throw new IllegalArgumentException("Could not determine a sizeTypeList (none passed or missing profile/variant config)");
            }
            this.sizeTypeList = sizeTypeList;
        }
        return sizeTypeList;
    }

    protected boolean isUseEntityCache() throws GeneralException {
        return useEntityCache;
    }

    protected boolean isUseProfileCache() throws GeneralException {
        return useProfileCache;
    }

    public CategoryImageViewType getImageViewType() throws GeneralException {
        return imageViewType;
    }

    public String getImageServerPathExpr() throws GeneralException {
        String imageServerPathExpr = this.imageServerPathExpr;
        if (imageServerPathExpr == null) {
            imageServerPathExpr = readImageServerPathExpr();
            this.imageServerPathExpr = imageServerPathExpr;
        }
        return imageServerPathExpr;
    }

    protected String readImageServerPathExpr() throws GeneralException {
        String imageServerPathExpr = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", getDelegator());
        try {
            imageServerPathExpr = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPathExpr, imageServerPathExpr);
        } catch (MalformedURLException e) {
            throw new GeneralException(e);
        }
        return imageServerPathExpr;
    }

    public String getImageServerPath() throws GeneralException {
        String imageServerPath = this.imageServerPath;
        if (imageServerPath == null) {
            imageServerPath = makeImageServerPath(getImageServerPathExpr(), getImagePathArgs());
            this.imageServerPath = imageServerPath;
        }
        return imageServerPath;
    }

    protected String makeImageServerPath(String imageServerPathExpr, Map<String, Object> imageContext) throws GeneralException {
        return PathUtil.removeTrailDelim(FlexibleStringExpander.expandString(imageServerPathExpr, imageContext));
    }

    public String getImageUrlPrefixExpr() throws GeneralException {
        String imageUrlPrefixExpr = this.imageUrlPrefixExpr;
        if (imageUrlPrefixExpr == null) {
            imageUrlPrefixExpr = readImageUrlPrefixExpr();
            this.imageUrlPrefixExpr = imageUrlPrefixExpr;
        }
        return imageUrlPrefixExpr;
    }

    protected String readImageUrlPrefixExpr() throws GeneralException {
        return EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", getDelegator());
    }

    public String getImageUrlPrefix() throws GeneralException {
        String imageUrlPrefix = this.imageUrlPrefix;
        if (imageUrlPrefix == null) {
            imageUrlPrefix = makeImageUrlPrefix(getImageUrlPrefixExpr(), getImagePathArgs());
            this.imageUrlPrefix = imageUrlPrefix;
        }
        return imageUrlPrefix;
    }

    protected String makeImageUrlPrefix(String imageUrlPrefixExpr, Map<String, Object> imageContext) throws GeneralException {
        return PathUtil.removeTrailDelim(FlexibleStringExpander.expandString(imageUrlPrefixExpr, imageContext));
    }

    public void setImageFnFmtExpr(String imageFnFmtExpr) throws GeneralException {
        this.imageFnFmtExpr = UtilValidate.isNotEmpty(imageFnFmtExpr) ? imageFnFmtExpr : null;
    }

    public String getImageFnFmtExpr() throws GeneralException {
        String imageFnFmt = this.imageFnFmtExpr;
        if (imageFnFmt == null) {
            imageFnFmt = readImageFnFmtExpr();
            this.imageFnFmtExpr = imageFnFmt;
        }
        return imageFnFmt;
    }

    protected String readImageFnFmtExpr() throws GeneralException {
        if (getImageViewType().isMain()) {
            return EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", getDelegator());
        } else {
            return EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", getDelegator());
        }
    }

    public String getImageExtension() throws GeneralException {
        String imageExtension = this.imageExtension;
        if (imageExtension == null) {
            imageExtension = imageFilename.substring(imageFilename.lastIndexOf(".") + 1);
            this.imageExtension = imageExtension;
        }
        return imageExtension;
    }

    public Map<String, Object> getImagePathArgs() throws GeneralException {
        Map<String, Object> imagePathArgs = this.imagePathArgs;
        if (imagePathArgs == null) {
            imagePathArgs = makeImagePathArg(new HashMap<>());
            this.imagePathArgs = imagePathArgs;
        }
        return imagePathArgs;
    }

    protected Map<String, Object> makeImagePathArg(Map<String, Object> imagePathArgs) throws GeneralException {
        CategoryImageViewType imageViewType = getImageViewType();
        String imageFnFmt = getImageFnFmtExpr();

        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();
        String id = getProductCategoryId();

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

    public Map<String, VariantLocation> getVariantLocations() throws GeneralException {
        Map<String, VariantLocation> sizeTypeInfoMap = this.sizeTypeInfoMap;
        if (sizeTypeInfoMap == null) {
            sizeTypeInfoMap = readVariantLocations();
            this.sizeTypeInfoMap = sizeTypeInfoMap;
        }
        return sizeTypeInfoMap;
    }

    protected Map<String, VariantLocation> readVariantLocations() throws GeneralException {
        if (getVariantConfig() == null) {
            throw new IllegalArgumentException("No variant config available for product image");
        }
        Map<String, VariantLocation> sizeTypeInfoMap = new LinkedHashMap<>();
        for (String sizeType : getSizeTypeList()) {
            ImageVariantConfig.VariantInfo variantInfo = getVariantConfig().getVariant(sizeType);
            if (variantInfo == null) {
                throw new IllegalArgumentException("sizeType [" + sizeType + "] not found in image variant config [" + getVariantConfig().getName() + "]");
            }
            String newFileLocation = ContentImageServices.expandImageFnFmt(FlexibleStringExpander.getInstance(getImageFnFmtExpr()), sizeType, getImagePathArgs());
            String targetFileType = (variantInfo.getFormat() != null) ? variantInfo.resolveFormatExt(getDelegator()) : getImageExtension();
            String relativeLocation = newFileLocation + "." + targetFileType;
            sizeTypeInfoMap.put(sizeType, new VariantLocation(variantInfo, relativeLocation,
                    PathUtil.concatPaths(getImageServerPath(), relativeLocation),
                    PathUtil.concatPaths(getImageUrlPrefix(), relativeLocation)));
        }
        return sizeTypeInfoMap;
    }

    public Map<String, VariantLocation> getMissingVariants() throws GeneralException {
        Map<String, VariantLocation> missingVariants = null;
        for(Map.Entry<String, VariantLocation> entry : getVariantLocations().entrySet()) {
            if (!entry.getValue().hasSource()) {
                if (missingVariants == null) {
                    missingVariants = new LinkedHashMap<>();
                }
                missingVariants.put(entry.getKey(), entry.getValue());
            }
        }
        return (missingVariants != null) ? missingVariants : Collections.emptyMap();
    }

    public List<String> getMissingVariantNames() throws GeneralException {
        List<String> missingVariantNames = new ArrayList<>();
        for(Map.Entry<String, VariantLocation> entry : getVariantLocations().entrySet()) {
            if (!entry.getValue().hasSource()) {
                missingVariantNames.add(entry.getKey());
            }
        }
        return missingVariantNames;
    }

    public static class VariantLocation implements Serializable {
        //protected final ImageVariantConfig.VariantInfo variantInfo;
        protected final String name;
        protected final String relativeLocation;
        protected final String fileLocation;
        protected final String url;
        //protected Boolean hasSource;
        //protected Boolean hasSourceFile;

        protected VariantLocation(ImageVariantConfig.VariantInfo variantInfo, String relativeLocation, String fileLocation, String url) {
            this.name = variantInfo.getName();
            this.relativeLocation = relativeLocation;
            this.fileLocation = fileLocation;
            this.url = url;
        }

        public String getName() {
            return name;
        }

        //public ImageVariantConfig.VariantInfo getVariantInfo() {
        //    return variantInfo;
        //}

        public String getRelativeLocation() {
            return relativeLocation;
        }

        public String getFileLocation() {
            return fileLocation;
        }

        public String getUrl() {
            return url;
        }

        /** FIXME: currently only recognizes source files, not source records - this currently works out anyway... */
        public boolean hasSource() {
            /*
            Boolean hasSource = this.hasSource;
            if (hasSource == null) {
                hasSource = hasSourceFile();
                this.hasSource = hasSource;
            }
            return hasSource;

             */
            return hasSourceFile();
        }

        public boolean hasSourceFile() {
            /*
            Boolean hasSourceFile = this.hasSourceFile;
            if (hasSourceFile == null) {
                hasSourceFile = checkSourceFile();
                this.hasSourceFile = hasSourceFile;
            }
            return hasSourceFile;
             */
            return checkSourceFile();
        }

        public boolean checkSourceFile() {
            return new File(getFileLocation()).exists();
        }
    }

    public static class ImageContentInfo {
        protected String imageUrl;
        protected GenericValue productContent;
        protected GenericValue content;

        protected ImageContentInfo(String imageUrl, GenericValue productContent, GenericValue content) {
            this.imageUrl = imageUrl;
            this.productContent = productContent;
            this.content = content;
        }

        public static ImageContentInfo from(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId, String imageUrl, Boolean useParentImageUrl, boolean useEntityCache) throws GeneralException {
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

        public String getImageUrl() {
            return imageUrl;
        }

        public GenericValue getProductContent() {
            return productContent;
        }

        public GenericValue getContent() {
            return content;
        }
    }
}
