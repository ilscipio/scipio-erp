package com.ilscipio.scipio.product.image;

import com.ilscipio.scipio.content.image.ImageVariants;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.ftl.ContentUrlDirective;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Product image and variants accessor and dedicated cache (SCIPIO).
 */
public class ProductImageVariants extends ImageVariants {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final ProductImageVariants NULL = new ProductImageVariants();
    private static final UtilCache<String, ProductImageVariants> CACHE = UtilCache.createUtilCache("product.image.variants", true);
    private static final boolean VARIANT_SOURCE_CHECK = UtilProperties.getPropertyAsBoolean("cache", "product.image.variants.sourceCheck", true);

    protected final GenericValue product;
    protected final GenericValue productMediaDetails;
    protected final String productContentTypeId;
    protected final GenericValue productContentView;
    protected final ProductVariant original;
    protected final boolean variantSourceCheck;
    protected final boolean useParents;
    protected Map<String, ProductVariant> variantMap;
    protected List<ProductVariant> variantList;
    protected List<GenericValue> variantRecords;
    //protected ProductImageLocationInfo defaultLocationInfo; // TODO: REVIEW: may lead to over-caching
    protected transient Map<String, ProductImageLocationInfo.VariantLocation> defaultVariantLocations;

    protected ProductImageVariants(GenericValue product, String productContentTypeId, GenericValue productContentView, GenericValue productMediaDetails, boolean useParents,
                                   Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache, Map<String, Object> options) {
        super(delegator, dispatcher, locale, useEntityCache, options);
        if (options == null) {
            options = Collections.emptyMap();
        }
        this.product = product;
        this.productContentTypeId = productContentTypeId;
        this.productContentView = productContentView;
        this.productMediaDetails = productMediaDetails;
        if (productContentView != null) {
            Long imageWidth = productContentView.getLong("drScpWidth");
            Long imageHeight = productContentView.getLong("drScpHeight");
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            this.original = new ProductVariant(variantInfo, productContentView);
        } else {
            // FIXME: CANNOT GET ORIGINAL IMAGE DIMENSIONS YET
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    -1, -1, null, null);
            this.original = new InlineProductVariant(variantInfo, productContentTypeId);
        }
        this.useParents = useParents;
        Boolean variantSourceCheck = (Boolean) options.get("variantSourceCheck");
        this.variantSourceCheck = (variantSourceCheck != null) ? variantSourceCheck : VARIANT_SOURCE_CHECK;
    }

    protected ProductImageVariants() {
        this.product = null;
        this.productContentTypeId = null;
        this.productContentView = null;
        this.original = null;
        this.variantSourceCheck = VARIANT_SOURCE_CHECK;
        this.useParents = false;
        this.productMediaDetails = null;
    }

    /** Main factory method with util cache support. */
    public static ProductImageVariants from(String productId, String productContentTypeId, boolean useParents, Delegator delegator, LocalDispatcher dispatcher,
                                            Locale locale, boolean useUtilCache, Map<String, Object> options)  {
        if (UtilValidate.isEmpty(productId) || UtilValidate.isEmpty(productContentTypeId)) {
            return null;
        }
        ProductImageVariants civ;
        String cacheKey = null;
        if (useUtilCache) {
            cacheKey = productId + "::" + productContentTypeId + "::" + useParents + "::" + delegator.getDelegatorName() + "::" + locale;
            civ = CACHE.get(cacheKey);
            if (civ != null) {
                return civ != NULL ? civ : null;
            }
        }
        civ = from(productId, productContentTypeId, useParents, delegator, dispatcher, locale, options);
        if (useUtilCache) {
            CACHE.put(cacheKey, civ != null ? civ : NULL);
        }
        //if (civ == null) {
        //    Debug.logWarning("Could not load product image records for product [" + productId + "] productContentTypeId [" + productContentTypeId + "]", module);
        //}
        return civ;
    }

    /** Uncached factory method. */
    public static ProductImageVariants from(String productId, String productContentTypeId, boolean useParents, Delegator delegator, LocalDispatcher dispatcher,
                                            Locale locale, Map<String, Object> options) {
        GenericValue product;
        GenericValue productContentView;
        GenericValue productMediaDetails;
        try {
            product = delegator.findOne("Product", UtilMisc.toMap("productId", productId), false);
            if (product == null) {
                Debug.logWarning("Could not find product [" + productId + "]", module);
                return null;
            }
            productContentView = delegator.from("ProductContentAndDataResource").where("productId", productId, "productContentTypeId", productContentTypeId)
                    .orderBy("-fromDate").filterByDate().queryFirst();
            if (useParents && productContentView == null && (Boolean.TRUE.equals(product.getBoolean("isVariant")))) {
                GenericValue parent = ProductWorker.getParentProduct(productId, delegator, false);
                if (parent != null) {
                    productContentView = delegator.from("ProductContentAndDataResource").where("productId", parent.get("productId"),
                            "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
                }
            }
            productMediaDetails = delegator.findOne("ProductMediaDetails", UtilMisc.toMap("productId", productId), false);
        } catch (Exception e) {
            Debug.logError(e, module);
            return null;
        }
        if (productContentView == null && !"ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
            return null;
        }
        ProductImageVariants imageVariants = new ProductImageVariants(product, productContentTypeId, productContentView, productMediaDetails, useParents,
                delegator, dispatcher, locale, false, options);
        String originalImageUrl = imageVariants.getOriginal().getStaticImageUrl();
        if (UtilValidate.isNotEmpty(originalImageUrl)) {
            // See also ProductContentWrapper.getImageUrl
            ProductImageWorker.ensureProductImage(dispatcher.getDispatchContext(), locale, product, productContentTypeId, originalImageUrl, true, true);
        }
        return imageVariants;
    }

    @Override
    public String getType() {
        return "product";
    }

    @Override
    public String getContentId() {
        return getOriginal().getContentId();
    }

    @Override
    public String getProfileName() {
        String profileName = getExplicitProfileName();
        if (profileName != null) {
            return profileName;
        }
        return ProductImageWorker.getDefaultProductImageProfileName(getDelegator(), getProductContentTypeId());
    }

    @Override
    public String getExplicitProfileName() {
        GenericValue productContentView = getProductContentView();
        if (productContentView != null) {
            return productContentView.getString("mediaProfile");
        } else {
            return getProduct().getString("imageProfile");
        }
    }

    @Override
    public ProductVariant getOriginal() {
        return original;
    }

    // confusing
    //public GenericValue getRecord() {
    //    return getOriginal().getRecord();
    //}

    public GenericValue getProduct() {
        return product;
    }

    public GenericValue getProductMediaDetails() {
        return productMediaDetails;
    }

    public String getProductId() {
        return product.getString("productId");
    }

    public GenericValue getProductContentView() {
        return productContentView;
    }

    public String getProductContentTypeId() {
        return productContentTypeId;
    }

    public boolean isUseParents() {
        return useParents;
    }

    public ProductImageLocationInfo getDefaultLocationInfo() {
        // TODO: REVIEW: caching appropriate when?
        ProductImageLocationInfo locationInfo = null;
        try {
            locationInfo = ProductImageLocationInfo.from(getDctx(), getProductId(),
                    ProductImageViewType.from(getProductContentTypeId()), getProfile(), getOriginal().getStaticImageUrl(), null, false, true, null);
        } catch (Exception e) {
            Debug.logError(e, "Error getting product image location info for product [" + getProductId()
                    + "] productContentTypeId [" + productContentTypeId + "]", module);
        }
        return locationInfo;
    }

    public Map<String, ProductImageLocationInfo.VariantLocation> getDefaultVariantLocations() {
        Map<String, ProductImageLocationInfo.VariantLocation> defaultVariantLocations = this.defaultVariantLocations;
        if (defaultVariantLocations == null) {
            try {
                defaultVariantLocations = getDefaultLocationInfo().getVariantLocations();
            } catch (Exception e) {
                Debug.logError(e, "Error getting product image location info for product [" + getProductId()
                        + "] productContentTypeId [" + productContentTypeId + "]", module);
            }
            if (defaultVariantLocations == null) {
                defaultVariantLocations = Collections.emptyMap();
            }
            this.defaultVariantLocations = defaultVariantLocations;
        }
        return defaultVariantLocations;
    }

    @Override
    public boolean isVariantSourceCheck() {
        return variantSourceCheck;
    }

    @Override
    public Map<String, ProductVariant> getVariantMap() {
        Map<String, ProductVariant> variants = this.variantMap;
        if (variants == null) {
            variants = new LinkedHashMap<>();
            variants = makeVariants(variants);
            variants = variants.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(variants);
            this.variantMap = variants;
        }
        return variants;
    }

    @Override
    public List<ProductVariant> getVariantList() {
        List<ProductVariant> variantList = this.variantList;
        if (variantList == null) {
            Map<String, ProductVariant> variants = getVariantMap();
            variantList = variants.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(variants.values()));
            this.variantList = variantList;
        }
        return variantList;
    }

    protected Map<String, ProductVariant> makeVariants(Map<String, ProductVariant> variants) {
        Map<String, GenericValue> sizeTypeRecordMap;
        try {
            sizeTypeRecordMap = ProductImageWorker.getVariantProductContentDataResourceRecordsBySizeType(getDelegator(),
                    getProductId(), getProductContentTypeId(), UtilDateTime.nowTimestamp(), false, isUseEntityCache());
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            sizeTypeRecordMap = Collections.emptyMap(); // this can sometimes proceed, sometimes not
        }
        for(ImageVariantConfig.VariantInfo config : getVariantConfig().getVariantList()) {
            String sizeType = config.getName();
            ProductVariant variant;
            GenericValue variantRecord = sizeTypeRecordMap.get(sizeType);
            if (variantRecord != null) {
                variant = makeVariant(config, variantRecord);
            } else {
                // TODO: REVIEW: NOT ALL SIZE TYPES POSSESS ProductContent records, and may not be on Product either,
                //  so we have to infer from Product fields and then filenames...
                String pctId = ProductImageWorker.getImageSizeTypeProductContentTypeId(getProductContentTypeId(), sizeType);
                variant = makeInlineVariant(config, pctId);
            }
            if (!isVariantSourceCheck() || variant.hasSource()) {
                variants.put(config.getName(), variant);
            }
        }
        return variants;
    }

    protected ProductVariant makeVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
        return new ProductVariant(config, record);
    }

    public class ProductVariant extends Variant {
        protected final GenericValue record;
        protected String staticImageUrl;
        protected String mimeTypeId;

        protected ProductVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
            super(config);
            this.record = record;
        }

        @Override
        public String getContentId() {
            return getRecord().getString("contentId");
        }

        @Override
        public String getAssocId() {
            return getRecord().getString("productContentTypeId");
        }

        @Override
        public GenericValue getRecord() {
            return record;
        }

        @Override
        public String getImageUrl(Map<String, Object> context, Map<String, Object> args) {
            if (args == null) {
                args = Collections.emptyMap();
            }
            String uri = getStaticImageUrl();
            if (uri == null) {
                return null;
            }
            try {
                return ContentUrlDirective.getInstance().makeLinkForContext(context, args, uri, null);
            } catch(Exception e) {
                Debug.logError("Error building URL for image variant content [" + getContentId() + "]", module);
                return null;
            }
        }

        @Override
        public String getPlainImageUrl(Map<String, Object> context, Map<String, Object> args) {
            if (args == null) {
                args = Collections.emptyMap();
            }
            String uri = getStaticImageUrl();
            if (uri == null) {
                return null;
            }
            return appendUrlParams(uri, UtilGenerics.cast(args.get("params")), (String) args.get("paramDelim"));
        }

        @Override
        public String getStaticImageUrl() {
            String staticImageUrl = this.staticImageUrl;
            if (staticImageUrl == null) {
                staticImageUrl = readStaticImageUrl();
                if (staticImageUrl == null) {
                    Debug.logWarning("Could not determine an imageUrl for product [" + getProductId()
                            + "] productContentTypeId [" + ProductImageVariants.this.getProductContentTypeId()
                            + "] variant [" + getName () + "]", module);
                    staticImageUrl = "";
                }
                this.staticImageUrl = staticImageUrl;
            }
            return staticImageUrl.isEmpty() ? null : staticImageUrl;
        }

        protected String readStaticImageUrl() {
            return getStoredImageUrl();
        }

        public String getStoredImageUrl() {
            return getRecordImageUrl();
        }

        public String getRecordImageUrl() {
            String dataResourceTypeId = getRecord().getString("drDataResourceTypeId");
            if ("SHORT_TEXT".equals(dataResourceTypeId)) {
                return getRecord().getString("drObjectInfo");
            } else if ("ELECTRONIC_TEXT".equals(dataResourceTypeId)) {
                String dataResourceId = getRecord().getString("dataResourceId");
                GenericValue elecText = null;
                try {
                    elecText = getDelegator().findOne("ElectronicText", UtilMisc.toMap("dataResourceId", dataResourceId), isUseEntityCache());
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                }
                if (elecText != null) {
                    return elecText.getString("textData");
                }
            }
            return null;
        }

        /** Gets the Product xxxImageUrl field: originalImageUrl, detailImageUrl, largeImageUrl, mediumImageUrl, smallImageUrl, ... */
        public String getFieldImageUrl() {
            // Based on ProductContentWrapper
            String candidateFieldName = ModelUtil.dbNameToVarName(getAssocId());
            ModelEntity productModel = getDelegator().getModelEntity("Product");
            if (productModel.isField(candidateFieldName)) {
                String candidateValue = getProduct().getString(candidateFieldName);
                if (UtilValidate.isNotEmpty(candidateValue)) {
                    return candidateValue;
                } else if (isUseParents() && "Y".equals(getProduct().getString("isVariant"))) {
                    GenericValue parent = ProductWorker.getParentProduct(getProductId(), getDelegator(), isUseEntityCache());
                    if (parent != null) {
                        candidateValue = parent.getString(candidateFieldName);
                        if (UtilValidate.isNotEmpty(candidateValue)) {
                            return candidateValue;
                        }
                    }
                }
            }
            return null;
        }

        /**
         * Returns the variant's file location as expected to be as if uploading a new file, or in other words
         * its default location, typically under /images.
         */
        public String getDefaultFileImageUrl() {
            ProductImageLocationInfo.VariantLocation loc = getDefaultVariantLocations().get(getName());
            return (loc != null) ? loc.getUrl() : null;
        }

        /**
         * Attempts to heuristically determine an image url from one of the other size image urls.
         * Original image url is consulted last because sometimes it was uploaded at a different location than its variants.
         * @deprecated using {@link #getDefaultFileImageUrl()} is currently safer.
         */
        @Deprecated
        public String getSubstitutedImageUrl() {
            for(ProductVariant variant : getVariantList()) {
                if (variant != this && !"original".equals(variant.getName())) { // NOTE: original should already be excluded, just in case
                    String imageUrl = substituteVariantInImageUrl(variant.getStoredImageUrl());
                    if (imageUrl != null) {
                        return imageUrl;
                    }
                }
            }
            return substituteVariantInImageUrl(getOriginal().getStoredImageUrl());
        }

        protected String substituteVariantInImageUrl(String imageUrl) {
            if (UtilValidate.isEmpty(imageUrl)) {
                return null;
            }
            int lastDot = imageUrl.lastIndexOf('.');
            int lastSlash = imageUrl.lastIndexOf('/');
            if (lastDot > 0 && lastSlash >= 0 && lastDot < (imageUrl.length() - 1) && lastSlash < (lastDot - 1)) {
                String path = imageUrl.substring(0, lastSlash + 1);
                String ext = imageUrl.substring(lastDot + 1);
                if (UtilValidate.isNotEmpty(getConfig().getFormat())) {
                    ext = getConfig().getFormat();
                }
                return path + getName().toLowerCase() + "." + ext; // NOTE: should already be lowercase
            } else {
                Debug.logWarning("Invalid url for image for product [" + getProductId() + "] productContentTypeId ["
                        + ProductImageVariants.this.getProductContentTypeId() + "] variant [" + getName()
                        + "]; bad slash or extension separator: " + imageUrl, module);
            }
            return null;
        }

        @Override
        public Integer getImageWidth() {
            Number width = getRecord().getNumber("drScpWidth");
            return (width != null) ? width.intValue() : null;
        }

        @Override
        public Integer getImageHeight() {
            Number height = getRecord().getNumber("drScpHeight");
            return (height != null) ? height.intValue() : null;
        }

        @Override
        public String getMimeTypeId() {
            String mimeTypeId = this.mimeTypeId;
            if (mimeTypeId == null) {
                mimeTypeId = readMimeTypeId();
                if (mimeTypeId == null) {
                    mimeTypeId = "";
                }
                this.mimeTypeId = mimeTypeId;
            }
            return mimeTypeId.isEmpty() ? null : mimeTypeId;
        }

        protected String readMimeTypeId() {
            String mimeTypeId = getExplicitMimeTypeId();
            if (mimeTypeId == null) {
                mimeTypeId = getMimeTypeIdFromExtension();
            }
            return mimeTypeId;
        }

        @Override
        public String getExplicitMimeTypeId() {
            // NOTE: drMimeTypeId is usually text/html due to stock design
            return getRecord().getString("drSrcMimeTypeId");
        }

        public String getMimeTypeIdFromExtension() {
            return getMimeTypeIdFromExtension(getStaticImageUrl());
        }

        protected String getMimeTypeIdFromExtension(String path) {
            if (UtilValidate.isEmpty(path)) {
                Debug.logWarning("Could not determine mimeTypeId for product [" + getProductId() + "] productContentTypeId ["
                        + ProductImageVariants.this.getProductContentTypeId() + "] variant [" + getName() + "]; no image path", module);
                return null;
            }
            int lastDot = path.lastIndexOf('.');
            if (lastDot < 0 || lastDot >= (path.length() - 1)) {
                Debug.logWarning("Could not determine mimeTypeId for product [" + getProductId() + "] productContentTypeId ["
                        + ProductImageVariants.this.getProductContentTypeId() + "] variant [" + getName() + "]; no extension in path [" + path + "]", module);
                return null;
            }
            String ext = path.substring(lastDot + 1);
            GenericValue fileExt = getDelegator().from("FileExtension").where("fileExtensionId", ext).cache().queryOneSafe();
            if (fileExt == null) {
                Debug.logWarning("Could not determine mimeTypeId for product [" + getProductId() + "] productContentTypeId ["
                        + ProductImageVariants.this.getProductContentTypeId() + "] variant [" + getName() + "]; unrecognized file extension [" + path + "]", module);
                return null;
            }
            return fileExt.getString("mimeTypeId");
        }

        @Override
        public boolean hasSource() { // explicit record is considered as having source (even if file were missing)
            return true;
        }
    }

    protected InlineProductVariant makeInlineVariant(ImageVariantConfig.VariantInfo config, String productContentTypeId) {
        return new InlineProductVariant(config, productContentTypeId);
    }

    public class InlineProductVariant extends ProductVariant {
        protected final String productContentTypeId;

        protected InlineProductVariant(ImageVariantConfig.VariantInfo config, String productContentTypeId) {
            super(config, null);
            this.productContentTypeId = productContentTypeId;
        }

        @Override
        public String getContentId() {
            return null;
        }

        @Override
        public String getAssocId() {
            return productContentTypeId;
        }

        @Override
        public GenericValue getRecord() {
            return getProduct();
        }

        @Override
        protected String readStaticImageUrl() {
            String baseImageUrl = getStoredImageUrl();
            if (baseImageUrl == null) {
                // heuristic for incomplete stock data (this shouldn't happen if productImageAutoRescale is fully called on all images)
                baseImageUrl = getDefaultFileImageUrl();
            }
            return baseImageUrl;
        }

        @Override
        public String getStoredImageUrl() {
            return getFieldImageUrl();
        }

        @Override
        public String getRecordImageUrl() {
            return null;
        }

        @Override
        public Integer getImageWidth() {
            Long width = getProductMediaDetailsFieldValue("width");
            return (width != null) ? width.intValue() : null;
        }

        @Override
        public Integer getImageHeight() {
            Long height = getProductMediaDetailsFieldValue("height");
            return (height != null) ? height.intValue() : null;
        }

        protected <T> T getProductMediaDetailsFieldValue(String suffixField) {
            GenericValue pmd = getProductMediaDetails();
            if (pmd == null) {
                return null;
            }
            String prefix = ProductImageWorker.getProductInlineImageFieldPrefix(getDelegator(), getAssocId());
            if (prefix == null) {
                return null;
            }
            return UtilGenerics.cast(pmd.get(prefix + suffixField.substring(0, 1).toUpperCase() + suffixField.substring(1)));
        }

        @Override
        public String getExplicitMimeTypeId() {
            return getProductMediaDetailsFieldValue("mimeTypeId");
        }

        @Override
        public boolean hasSource() {
            String imageUrl = getStoredImageUrl(); // if there's an explicit filename, assume it exists
            if (imageUrl != null) {
                return false;
            }
            ProductImageLocationInfo.VariantLocation loc = getDefaultVariantLocations().get(getName());
            return (loc != null) ? loc.hasSourceFile() : false;
        }
    }

    @Override
    public Map<String, Object> getResponsiveVariantMap(String targetMimeTypeId, Map<String, ? extends Number> sizeDefs,
                                                       Map<String, Object> context, Map<String, Object> urlArgs) {
        // TODO
        return Collections.emptyMap();
    }


    public static void clearCaches(Delegator delegator) {
        CACHE.clear();
    }

    public static Map<String, Object> clearCaches(ServiceContext ctx) {
        clearCaches((Delegator) null);

        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.newMap(); // UtilMisc.toMap("type", ctx.attr("type"));
                dcc.runDistributedService("productImageVariantsDistributedClearCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccess();
    }
}
