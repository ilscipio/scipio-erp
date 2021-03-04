package com.ilscipio.scipio.product.image;

import com.ilscipio.scipio.content.image.ImageVariants;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.ftl.ContentUrlDirective;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
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
    private static final List<String> VARIANT_PRIO_LIST = UtilMisc.unmodifiableArrayList("original", "detail", "large", "medium", "small");
    private static final List<String> PCTID_PRIO_LIST = UtilMisc.unmodifiableArrayList("ORIGINAL_IMAGE_URL", "DETAIL_IMAGE_URL", "LARGE_IMAGE_URL", "MEDIUM_IMAGE_URL", "SMALL_IMAGE_URL");

    protected final GenericValue product;
    protected final GenericValue productMediaDetails;
    protected final ProductImageViewType origImageViewType;
    protected final GenericValue productContentView;
    protected final ProductVariant original;
    protected final boolean variantSourceCheck;
    protected final boolean useParents;
    protected Map<String, ProductVariant> variantMap;
    protected List<ProductVariant> variantList;
    protected List<GenericValue> variantRecords;
    //protected ProductImageLocationInfo defaultLocationInfo; // TODO: REVIEW: may lead to over-caching
    protected transient Map<String, ProductImageLocationInfo.VariantLocation> defaultVariantLocations;
    protected transient String defaultStoredImageUrl;
    protected Timestamp queryMoment;

    protected ProductImageVariants(GenericValue product, String productContentTypeId, GenericValue productContentView, GenericValue productMediaDetails, boolean useParents,
                                   Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache, Map<String, Object> options) throws GeneralException {
        super(delegator, dispatcher, locale, useEntityCache, options);
        if (options == null) {
            options = Collections.emptyMap();
        }
        this.product = product;
        this.origImageViewType = ProductImageViewType.from(delegator, productContentTypeId, false, true);
        this.productContentView = productContentView;
        this.productMediaDetails = productMediaDetails;
        if (productContentView != null) {
            Long imageWidth = productContentView.getLong("drScpWidth");
            Long imageHeight = productContentView.getLong("drScpHeight");
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            this.original = makeContentVariant(variantInfo, this.origImageViewType, productContentView);
        } else {
            Long imageWidth = (productMediaDetails != null) ? productMediaDetails.getLong("originalImageWidth") : null;
            Long imageHeight = (productMediaDetails != null) ? productMediaDetails.getLong("originalImageHeight") : null;
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            this.original = makeInlineVariant(variantInfo, this.origImageViewType, getProductCandidateFieldName(productContentTypeId));
        }
        this.useParents = useParents;
        Boolean variantSourceCheck = (Boolean) options.get("variantSourceCheck");
        this.variantSourceCheck = (variantSourceCheck != null) ? variantSourceCheck : VARIANT_SOURCE_CHECK;
    }

    protected ProductImageVariants() {
        this.product = null;
        this.origImageViewType = null;
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
        } catch (Exception e) {
            Debug.logError(e, module);
            return null;
        }
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
        return getOrigImageViewType().getProductContentTypeId();
    }

    public ProductImageViewType getOrigImageViewType() {
        return origImageViewType;
    }

    public boolean isUseParents() {
        return useParents;
    }

    public Timestamp getQueryMoment() {
        Timestamp queryMoment = this.queryMoment;
        if (queryMoment == null) {
            queryMoment = UtilDateTime.nowTimestamp();
            this.queryMoment = queryMoment;
        }
        return queryMoment;
    }

    protected String getDefaultStoredImageUrl() {
        String defaultStoredImageUrl = this.defaultStoredImageUrl;
        if (defaultStoredImageUrl == null) {
            // TODO: REVIEW: this does manual queries due to circular loops
            defaultStoredImageUrl = getOriginal().getStoredImageUrl();
            if (defaultStoredImageUrl == null) {
                GenericValue product = getProduct();
                for (String pctId : PCTID_PRIO_LIST) {
                    String fieldName = getProductCandidateFieldName(pctId);
                    if (product.hasModelField(fieldName)) {
                        defaultStoredImageUrl = product.getString(fieldName);
                        if (defaultStoredImageUrl != null) {
                            break;
                        }
                    }
                }
                if (defaultStoredImageUrl == null) {
                    // TODO: reuse makeVariants query - circular loop issue currently
                    List<GenericValue> pcdrList = getDelegator().from("ProductContentAndDataResource")
                            .where(EntityCondition.makeCondition("productId", getProductId()),
                                    EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, "%_IMAGE_URL"))
                            .orderBy("-fromDate").filterByDate(getQueryMoment()).cache(false).queryListSafe();
                    if (pcdrList != null) {
                        for(String pctId : PCTID_PRIO_LIST) {
                            for(GenericValue pcdr : pcdrList) {
                                if (pctId.equals(pcdr.getString("productContentTypeId"))) {
                                    defaultStoredImageUrl = getRecordImageUrl(pcdr);
                                    break;
                                }
                            }
                            if (defaultStoredImageUrl != null) {
                                break;
                            }
                        }
                    }
                }
            }
            if (defaultStoredImageUrl == null) {
                Debug.logWarning("No default image URL for product [" + getProductId() + "]", module);
            }
            this.defaultStoredImageUrl = defaultStoredImageUrl;
        }
        return defaultStoredImageUrl;
    }

    public ProductImageLocationInfo getDefaultLocationInfo() {
        // TODO: REVIEW: caching appropriate when?
        ProductImageLocationInfo locationInfo = null;
        try {
            String imageUrl = getDefaultStoredImageUrl();
            if (imageUrl == null) {
                throw new IllegalStateException("No stored default image URL found for product/productContentTypeId");
            }
            locationInfo = ProductImageLocationInfo.from(getDctx(), getProductId(), getOrigImageViewType(), getProfile(),
                    imageUrl, null, false, true, null);
        } catch (Exception e) {
            Debug.logError(e, "Error getting product image location info for product [" + getProductId()
                    + "] productContentTypeId [" + getProductContentTypeId() + "]", module);
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
                        + "] productContentTypeId [" + getProductContentTypeId() + "]", module);
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

    protected boolean isProductField(String fieldName) {
        return getDelegator().getModelEntity("Product").isField(fieldName);
    }

    protected String getProductCandidateFieldName(String productContentTypeId) {
        return ModelUtil.dbNameToVarName(productContentTypeId);
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

    @Override
    public Map<String, ProductVariant> getOriginalAndVariantMap() {
        return UtilGenerics.cast(super.getOriginalAndVariantMap());
    }

    @Override
    public List<ProductVariant> getOriginalAndVariantList() {
        return UtilGenerics.cast(super.getOriginalAndVariantList());
    }

    protected Map<String, ProductVariant> makeVariants(Map<String, ProductVariant> variants) {
        Map<String, GenericValue> sizeTypeRecordMap;
        try {
            sizeTypeRecordMap = ProductImageWorker.getVariantProductContentDataResourceRecordsByViewSize(getDelegator(),
                    getProductId(), getOrigImageViewType(), getQueryMoment(), false, isUseEntityCache());
        } catch (GeneralException e) {
            Debug.logError(e, "makeVariants: " + e.toString(), module);
            sizeTypeRecordMap = Collections.emptyMap(); // this can sometimes proceed, sometimes not
        }

        for(ImageVariantConfig.VariantInfo config : getVariantConfig().getVariantList()) {
            String sizeType = config.getName();
            ProductVariant variant;
            GenericValue variantRecord = sizeTypeRecordMap.get(sizeType);

            ProductImageViewType variantImageViewType;
            try {
                variantImageViewType = ProductImageViewType.from(getDelegator(),
                        getOrigImageViewType().getViewType(), getOrigImageViewType().getViewNumber(), sizeType, false, true);
            } catch (Exception e) {
                Debug.logError("Unable to derive variantImageViewType for sizeType [" + sizeType +
                        "] for productId [" + getProductId() + "] imageViewType " + getOrigImageViewType() + ": " + e.toString(), module);
                continue;
            }

            if (variantRecord != null) {
                variant = makeContentVariant(config, variantImageViewType, variantRecord);
            } else {
                String fieldName = getProductCandidateFieldName(variantImageViewType.getProductContentTypeId());
                if (isProductField(fieldName)) {
                    variant = makeInlineVariant(config, variantImageViewType, fieldName);
                } else {
                    variant = makeDefaultVariant(config, variantImageViewType);
                }
            }

            if (!isVariantSourceCheck() || variant.hasSource()) {
                variants.put(config.getName(), variant);
            }
        }
        return variants;
    }

    public abstract class ProductVariant extends Variant {
        protected final ProductImageViewType variantImageViewType;
        protected String staticImageUrl;
        protected String mimeTypeId;

        protected ProductVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType) {
            super(config);
            this.variantImageViewType = variantImageViewType;
        }

        public ProductImageViewType getVariantImageViewType() {
            return variantImageViewType;
        }

        @Override
        public String getAssocId() {
            return getVariantImageViewType().getProductContentTypeId();
        }

        @Override
        public String getImageUrl(Map<String, Object> context, Map<String, Object> args) {
            String uri = getStaticImageUrl();
            if (uri == null) {
                return null;
            }
            if (args == null) {
                args = Collections.emptyMap();
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
            String uri = getStaticImageUrl();
            if (uri == null) {
                return null;
            }
            if (args == null) {
                args = Collections.emptyMap();
            }
            return appendUrlParams(uri, UtilGenerics.cast(args.get("params")), (String) args.get("paramDelim"));
        }

        @Override
        public String getStaticImageUrl() {
            String staticImageUrl = this.staticImageUrl;
            if (staticImageUrl == null) {
                staticImageUrl = readStaticImageUrl();
                if (staticImageUrl == null) {
                    if (Debug.verboseOn()) {
                        Debug.logInfo("Could not determine an image url for product [" + getProductId()
                                + "] productContentTypeId [" + ProductImageVariants.this.getProductContentTypeId()
                                + "] variant [" + getName() + "]", module);
                    }
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
            return ProductImageVariants.this.getRecordImageUrl(getRecord());
        }

        /** Gets the Product xxxImageUrl field: originalImageUrl, detailImageUrl, largeImageUrl, mediumImageUrl, smallImageUrl, ... */
        public String getFieldImageUrl() {
            // Based on ProductContentWrapper
            String fieldName = getProductUrlFieldName();
            if (fieldName == null || !isProductField(fieldName)) {
                return null;
            }
            String candidateValue = getProduct().getString(fieldName);
            if (UtilValidate.isNotEmpty(candidateValue)) {
                return candidateValue;
            } else if (isUseParents() && "Y".equals(getProduct().getString("isVariant"))) {
                GenericValue parent = ProductWorker.getParentProduct(getProductId(), getDelegator(), isUseEntityCache());
                if (parent != null) {
                    candidateValue = parent.getString(fieldName);
                    if (UtilValidate.isNotEmpty(candidateValue)) {
                        return candidateValue;
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
        public boolean hasSource() {
            // explicit record is considered as having source (even if file were missing)
            return (getStaticImageUrl() != null);
        }

        protected String getProductUrlFieldName() {
            return getProductCandidateFieldName(getAssocId());
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
    }

    protected ContentProductVariant makeContentVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType, GenericValue record) {
        return new ContentProductVariant(config, variantImageViewType, record);
    }

    public class ContentProductVariant extends ProductVariant {
        protected final GenericValue record;

        protected ContentProductVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType, GenericValue record) {
            super(config, variantImageViewType);
            this.record = record;
        }

        @Override
        public String getContentId() {
            return getRecord().getString("contentId");
        }

        @Override
        public GenericValue getRecord() {
            return record;
        }
    }

    protected InlineProductVariant makeInlineVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType, String fieldName) {
        return new InlineProductVariant(config, variantImageViewType, fieldName);
    }

    /**
     * InlineProductVariant.
     * TODO: REVIEW: Currently this is also created for variants missing records and not having any inline fields.
     */
    public class InlineProductVariant extends ProductVariant {
        protected final String fieldName;

        protected InlineProductVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType, String fieldName) {
            super(config, variantImageViewType);
            this.fieldName = fieldName;
        }

        @Override
        public String getContentId() {
            return null;
        }

        @Override
        public GenericValue getRecord() {
            return getProduct();
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
        @Override
        public String getExplicitMimeTypeId() {
            return getProductMediaDetailsFieldValue("mimeTypeId");
        }

        @Override
        public boolean hasSource() {
            return super.hasSource();
        }
    }

    protected DefaultProductVariant makeDefaultVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType) {
        return new DefaultProductVariant(config, variantImageViewType);
    }

    public class DefaultProductVariant extends ProductVariant {

        protected DefaultProductVariant(ImageVariantConfig.VariantInfo config, ProductImageViewType variantImageViewType) {
            super(config, variantImageViewType);
        }

        @Override
        public String getContentId() {
            return null;
        }

        @Override
        public String getAssocId() {
            return getVariantImageViewType().getProductContentTypeId();
        }

        @Override
        public GenericValue getRecord() {
            return getProduct();
        }

        @Override
        protected String readStaticImageUrl() {
            return getDefaultFileImageUrl();
        }

        @Override
        public String getStoredImageUrl() {
            return null;
        }

        @Override
        public String getRecordImageUrl() {
            return null;
        }

        @Override
        public Integer getImageWidth() {
            return null;
        }

        @Override
        public Integer getImageHeight() {
            return null;
        }

        @Override
        public String getExplicitMimeTypeId() {
            return null;
        }

        @Override
        public boolean hasSource() {
            ProductImageLocationInfo.VariantLocation loc = getDefaultVariantLocations().get(getName());
            return loc != null && loc.hasSourceFile();
        }
    }

    @Override
    public Map<String, Object> getResponsiveVariantMap(String targetMimeTypeId, Map<String, ? extends Number> sizeDefs,
                                                       Map<String, Object> context, Map<String, Object> urlArgs) {
        // TODO: REVIEW: from ContentImageVariants#getResponsiveVariantMap
        /* Example:
        <#local sizeMap = getImageVariants("product", productId)!false/><#-- now accepts empty contentId, will return null/false -->
        <#if !sizeMap?is_boolean>
          <#local responsiveMap = sizeMap.getResponsiveVariantMap("image/webp", sizeDefs, context, {})/>
        </#if>
         */
        List<? extends Variant> variantList = getVariantList();
        if (UtilValidate.isEmpty(variantList) || UtilValidate.isEmpty(sizeDefs)) {
            return Collections.emptyMap();
        }

        Map<String, Object> srcset = new LinkedHashMap<>();
        Map<String, Object> srcsetTarget = new LinkedHashMap<>();
        Map<String, Object> srcsetSize = new LinkedHashMap<>();
        Map<String, Object> srcsetSizeTarget = new LinkedHashMap<>();

        variantList = new ArrayList<>(variantList);
        variantList.sort(new Comparator<Variant>() {
            @Override
            public int compare(Variant o1, Variant o2) {
                // NOTE: imageWidth is physical width, it *could* be missing for old data (and non-updated product images),
                // so may want configWidth instead...
                Integer width1 = o1.getImageWidth();
                Integer width2 = o2.getImageWidth();
                if (width1 == null || width2 == null) {
                    return 0;
                }
                return Integer.compare(width2, width1); // reversed
            }
        });

        for(Map.Entry<String, ? extends Number> sizeDefEntry : sizeDefs.entrySet()) {
            Variant bestSize = null;
            Variant bestSizeTarget = null;

            String sizeDefKey = sizeDefEntry.getKey();
            Number sizeDefNum = sizeDefEntry.getValue();
            if (UtilValidate.isNotEmpty(sizeDefKey) && sizeDefNum != null) {
                for (Variant variant : variantList) {
                    if (variant.getImageWidth() != null && variant.getImageWidth() >= sizeDefNum.intValue()) {
                        if (targetMimeTypeId.equals(variant.getMimeTypeId())) {
                            bestSizeTarget = variant;
                        } else {
                            bestSize = variant;
                        }
                    }
                }
            }

            if (bestSize != null) {
                String url = bestSize.getImageUrl(context, urlArgs);
                if (UtilValidate.isNotEmpty(url)) {
                    srcset.put(url, bestSize.getImageWidth());
                    srcsetSize.put(url, sizeDefKey);
                }
            }
            if (bestSizeTarget != null) {
                String url = bestSizeTarget.getImageUrl(context, urlArgs);
                if (UtilValidate.isNotEmpty(url)) {
                    srcsetTarget.put(url, bestSizeTarget.getImageWidth());
                    srcsetSizeTarget.put(url, sizeDefKey);
                }
            }
        }

        return UtilMisc.toMap("srcset", srcset, "srcsetTarget", srcsetTarget,
                "srcsetSize", srcsetSize, "srcsetSizeTarget", srcsetSizeTarget);
    }

    public static Map<String, Object> clearCaches(ServiceContext ctx) {
        String productId = ctx.attr("productId");
        String productContentTypeId = ctx.attr("productContentTypeId");

        if (UtilValidate.isNotEmpty(productId)) {
            String prefix = UtilValidate.isNotEmpty(productContentTypeId) ? productId + "::" + productContentTypeId + "::" : productId + "::";
            clearCachesByPrefix(ctx.delegator(), prefix);
        } else {
            CACHE.clear();
        }

        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.toMap("productId", productId, "productContentTypeId", productContentTypeId);
                dcc.runDistributedService("productImageVariantsDistributedClearCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccess();
    }

    public static void clearCachesByPrefix(Delegator delegator, String prefix) { // SCIPIO
        CACHE.removeByFilter((k, v) -> k.startsWith(prefix));
        ProductContentWrapper.clearCachesByPrefix(delegator, prefix);
    }

    protected String getRecordImageUrl(GenericValue drRecord) {
        String dataResourceTypeId = drRecord.getString("drDataResourceTypeId");
        if ("SHORT_TEXT".equals(dataResourceTypeId)) {
            return drRecord.getString("drObjectInfo");
        } else if ("ELECTRONIC_TEXT".equals(dataResourceTypeId)) {
            String dataResourceId = drRecord.getString("dataResourceId");
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
}
