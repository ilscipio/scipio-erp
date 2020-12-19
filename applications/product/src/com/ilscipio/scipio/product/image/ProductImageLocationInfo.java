package com.ilscipio.scipio.product.image;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.content.image.ContentImageServices;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
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
 * Product image sizeType info parser, based on (duplicated from) {@link ContentImageServices#contentImageFileScaleInAllSizeCore(ServiceContext)}.
 * NOTE: Currently represents only default file locations, which in some circumstances are not respected in data (such as original file locations).
 * WARN: Subject to refactoring.
 */
public class ProductImageLocationInfo implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final ProductImageLocationInfo NULL = new ProductImageLocationInfo();

    private transient Delegator delegator;
    private final String delegatorName;
    private transient LocalDispatcher dispatcher;
    private final String dispatcherName;
    protected final String productId;
    protected final String productContentTypeId;
    protected final ImageVariantConfig variantConfig;
    protected final String imageFilename;
    protected final Collection<String> sizeTypeList;
    protected final boolean useEntityCache;
    protected final boolean useProfileCache;

    protected ProductImageWorker.ImageViewType imageViewType;
    protected String imageExtension;
    protected String imageServerPath;
    protected String imageUrlPrefix;
    protected FlexibleStringExpander imageFnFmt;
    protected Map<String, Object> imagePathArgs;
    protected Map<String, VariantLocation> sizeTypeInfoMap;

    protected ProductImageLocationInfo() {
        this.delegatorName = null;
        this.dispatcherName = null;
        this.productId = null;
        this.productContentTypeId = null;
        this.variantConfig = null;
        this.imageFilename = null;
        this.sizeTypeList = null;
        this.useEntityCache = false;
        this.useProfileCache = false;
    }

    protected ProductImageLocationInfo(DispatchContext dctx, String productId, String productContentTypeId,
                                       ImageVariantConfig variantConfig, String imageFilename, Collection<String> sizeTypeList,
                                       boolean useEntityCache, boolean useProfileCache) throws IllegalArgumentException {
        this.delegator = dctx.getDelegator();
        this.delegatorName = this.delegator.getDelegatorName();
        this.dispatcher = dctx.getDispatcher();
        this.dispatcherName = this.dispatcher.getName();
        this.productId = productId;
        this.productContentTypeId = productContentTypeId;
        this.variantConfig = variantConfig;
        if (imageFilename.lastIndexOf("/") != -1) {
            imageFilename = imageFilename.substring(imageFilename.lastIndexOf("/") + 1);
        }
        if (imageFilename.lastIndexOf(".") <= 0 || imageFilename.lastIndexOf(".") >= (imageFilename.length() - 1)) { // SCIPIO: added this to prevent more serious problems
            throw new IllegalArgumentException("Original image filename [" + imageFilename + "] has missing or improper file extension (image type)");
        }
        this.imageFilename = imageFilename;
        if (sizeTypeList == null) {
            sizeTypeList = variantConfig.getVariantNames();
        }
        this.sizeTypeList = sizeTypeList;
        this.useEntityCache = useEntityCache;
        this.useProfileCache = useProfileCache;
    }

    public static ProductImageLocationInfo from(DispatchContext dctx, String productId, String productContentTypeId,
                                                ImageVariantConfig variantConfig, String imageFilename,
                                                Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache) throws IllegalArgumentException, GeneralException {
        return new ProductImageLocationInfo(dctx, productId, productContentTypeId, variantConfig, imageFilename, sizeTypeList, useEntityCache, useProfileCache);
    }

    public static ProductImageLocationInfo from(DispatchContext dctx, String productId, String productContentTypeId,
                                                ImageProfile imageProfile, String imageFilename,
                                                Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache) throws IllegalArgumentException, GeneralException {
        // NOTE: this currently calls the non-cached readVariantConfig, because this is intended for backend
        return from(dctx, productId, productContentTypeId, useProfileCache ? imageProfile.getVariantConfig() : imageProfile.readVariantConfig(),
                imageFilename, sizeTypeList, useEntityCache, useProfileCache);
    }

    /**
     * Returns ProductImageLocationInfo or null if no image URL/location/variants applicable for the given product/productContentTypeId.
     * If passed productContent or imageUrl null attempts to determine from data.
     * Based on productImageAutoRescale (TODO?: deduplicate).
     */
    public static ProductImageLocationInfo from(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId,
                                                String imageUrl, Collection<String> sizeTypeList,
                                                Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache) throws IllegalArgumentException, GeneralException {
        Delegator delegator = dctx.getDelegator();
        if (locale == null) {
            locale = Locale.getDefault();
        }
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
            if (!productModel.isField(productFieldName)) {
                // May happen normally due to ADDITIONAL_IMAGE_x
                //Debug.logError(logPrefix+"Invalid productContentTypeId [" + productContentTypeId
                //        + "] for product [" + productId + "] for resize operation (parent products not consulted)", module);
                return null;
            }
            inlineImageUrl = product.getString(productFieldName);
        }

        if (imageUrl == null) {
            if (Boolean.TRUE.equals(useParentImageUrl)) {
                // NOTE: this consults the parent product which we don't want, but in known cases should return right value
                imageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                        locale, dctx.getDispatcher(), useEntityCache, "raw");
            } else {
                if (content != null) {
                    imageUrl = ProductImageWorker.getDataResourceImageUrl(
                            delegator.from("DataResource").where("dataResourceId", content.get("dataResourceId")).queryOne(), useEntityCache);
                } else if (inlineImageUrl != null) {
                    imageUrl = inlineImageUrl;
                }
            }
            if (UtilValidate.isEmpty(imageUrl)) {
                return null;
            }
        }

        ImageProfile imageProfile = ProductImageWorker.getProductImageProfileOrDefault(delegator, productContentTypeId, product, content, useEntityCache, useProfileCache);
        if (imageProfile == null) {
            Debug.logError("Could not find media profile for product [" + productId + "] productContentTypeId [" + productContentTypeId + "]", module);
            return null;
        }
        return ProductImageLocationInfo.from(dctx, productId, productContentTypeId, imageProfile, imageUrl, sizeTypeList, useEntityCache, useProfileCache);
    }

    public boolean isNull() {
        return productId == null;
    }

    public Delegator getDelegator() {
        Delegator delegator = this.delegator;
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator(delegatorName);
            this.delegator = delegator;
        }
        return delegator;
    }

    public LocalDispatcher getDispatcher() {
        LocalDispatcher dispatcher = this.dispatcher;
        if (dispatcher == null) {
            dispatcher = ServiceContainer.getLocalDispatcher(dispatcherName, this.getDelegator());
            this.dispatcher = dispatcher;
        }
        return dispatcher;
    }

    public DispatchContext getDctx() {
        return getDispatcher().getDispatchContext();
    }

    public String getProductId() {
        return productId;
    }

    public String getProductContentTypeId() {
        return productContentTypeId;
    }

    public ImageVariantConfig getVariantConfig() {
        return variantConfig;
    }

    public String getImageFilename() {
        return imageFilename;
    }

    public Collection<String> getSizeTypeList() {
        return sizeTypeList;
    }

    protected boolean isUseEntityCache() {
        return useEntityCache;
    }

    protected boolean isUseProfileCache() {
        return useProfileCache;
    }

    public ProductImageWorker.ImageViewType getImageViewType() throws IllegalArgumentException, GeneralException {
        ProductImageWorker.ImageViewType imageViewType = this.imageViewType;
        if (imageViewType == null) {
            imageViewType = ProductImageWorker.ImageViewType.from(productContentTypeId);
            this.imageViewType = imageViewType;
        }
        return imageViewType;
    }

    public String getImageServerPath() throws IllegalArgumentException, GeneralException {
        String imageServerPath = this.imageServerPath;
        if (imageServerPath == null) {
            initPathProperties();
            imageServerPath = this.imageServerPath;
        }
        return imageServerPath;
    }

    public String getImageUrlPrefix() throws IllegalArgumentException, GeneralException {
        String imageUrlPrefix = this.imageUrlPrefix;
        if (imageUrlPrefix == null) {
            initPathProperties();
            imageUrlPrefix = this.imageUrlPrefix;
        }
        return imageUrlPrefix;
    }

    public FlexibleStringExpander getImageFnFmt() throws IllegalArgumentException, GeneralException {
        FlexibleStringExpander imageFnFmt = this.imageFnFmt;
        if (imageFnFmt == null) {
            initPathProperties();
            imageFnFmt = this.imageFnFmt;
        }
        return imageFnFmt;
    }

    public String getImageExtension() throws IllegalArgumentException, GeneralException {
        String imageExtension = this.imageExtension;
        if (imageExtension == null) {
            imageExtension = imageFilename.substring(imageFilename.lastIndexOf(".") + 1);
            this.imageExtension = imageExtension;
        }
        return imageExtension;
    }

    public Map<String, Object> getImagePathArgs() throws IllegalArgumentException, GeneralException {
        Map<String, Object> imagePathArgs = this.imagePathArgs;
        if (imagePathArgs == null) {
            initPathProperties();
            imagePathArgs = this.imagePathArgs;
        }
        return imagePathArgs;
    }

    protected void initPathProperties() throws IllegalArgumentException, GeneralException {
        ProductImageWorker.ImageViewType imageViewType = getImageViewType();
        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();

        String imageServerPath = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", getDelegator());
        String imageUrlPrefix = EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", getDelegator());
        Map<String, Object> imagePathArgs = new HashMap<>();

        String imageFnFmt;
        String id = getProductId();
        if (viewType.toLowerCase().contains("main")) {
            imageFnFmt = EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", getDelegator());
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "type", "original"));
        } else if (viewType.toLowerCase().contains("additional") && viewNumber != null && !"0".equals(viewNumber)) {
            imageFnFmt = EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", getDelegator());
            if (imageFnFmt.endsWith("${id}")) { // TODO: REVIEW: I don't get this
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "viewtype", viewType, "sizetype", "original"));
        } else {
            throw new IllegalArgumentException("Unrecognized viewType [" + viewType + "] or viewNumber [" + viewNumber
                    + "] for productContentTypeId [" + getProductContentTypeId() + "] for product [" + id + "]");
        }

        try {
            imageServerPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPath, imageServerPath);
        } catch (MalformedURLException e) {
            throw new GeneralException(e);
        }

        Map<String, Object> imageContext = new HashMap<>(); // new HashMap<>(context);
        imageContext.put("tenantId", getDelegator().getDelegatorTenantId());
        imageContext.putAll(imagePathArgs);
        imageServerPath = FlexibleStringExpander.expandString(imageServerPath, imageContext);
        imageUrlPrefix = FlexibleStringExpander.expandString(imageUrlPrefix, imageContext);
        imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length() - 1) : imageServerPath;
        imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length() - 1) : imageUrlPrefix;

        this.imageServerPath = imageServerPath;
        this.imageUrlPrefix = imageUrlPrefix;
        this.imageFnFmt = FlexibleStringExpander.getInstance(imageFnFmt);
        this.imagePathArgs = imagePathArgs;
    }

    public Map<String, VariantLocation> getVariantLocations() throws IllegalArgumentException, GeneralException {
        Map<String, VariantLocation> sizeTypeInfoMap = this.sizeTypeInfoMap;
        if (sizeTypeInfoMap == null) {
            sizeTypeInfoMap = readVariantLocations();
            this.sizeTypeInfoMap = sizeTypeInfoMap;
        }
        return sizeTypeInfoMap;
    }

    protected Map<String, VariantLocation> readVariantLocations() throws IllegalArgumentException, GeneralException {
        Map<String, VariantLocation> sizeTypeInfoMap = new LinkedHashMap<>();
        for (String sizeType : getSizeTypeList()) {
            ImageVariantConfig.VariantInfo variantInfo = getVariantConfig().getVariant(sizeType);
            if (variantInfo == null) {
                throw new IllegalArgumentException("sizeType [" + sizeType + "] not found in image variant config [" + getVariantConfig().getName() + "]");
            }
            String newFileLocation = ContentImageServices.expandImageFnFmt(getImageFnFmt(), sizeType, getImagePathArgs());
            String targetFileType = (variantInfo.getFormat() != null) ? variantInfo.resolveFormatExt(getDelegator()) : getImageExtension();
            String relativeLocation = newFileLocation + "." + targetFileType;
            sizeTypeInfoMap.put(sizeType, new VariantLocation(variantInfo, relativeLocation,
                    PathUtil.concatPaths(getImageServerPath(), relativeLocation),
                    PathUtil.concatPaths(getImageUrlPrefix(), relativeLocation)));
        }
        return sizeTypeInfoMap;
    }

    public Map<String, VariantLocation> getMissingVariants() throws IllegalArgumentException, GeneralException {
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

    public List<String> getMissingVariantNames() throws IllegalArgumentException, GeneralException {
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
}
