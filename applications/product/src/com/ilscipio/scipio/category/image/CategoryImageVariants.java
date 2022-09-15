package com.ilscipio.scipio.category.image;

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
import org.ofbiz.product.category.CategoryContentWrapper;
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
 * ProductCategory image and variants accessor and dedicated cache (SCIPIO).
 */
public class CategoryImageVariants extends ImageVariants {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final CategoryImageVariants NULL = new CategoryImageVariants();
    private static final UtilCache<String, CategoryImageVariants> CACHE = UtilCache.createUtilCache("category.image.variants", true);
    private static final boolean VARIANT_SOURCE_CHECK = UtilProperties.getPropertyAsBoolean("cache", "category.image.variants.sourceCheck", true);
    private static final List<String> VARIANT_PRIO_LIST = UtilMisc.unmodifiableArrayList("original", "detail", "large", "medium", "small");
    private static final List<String> PCCTID_PRIO_LIST = UtilMisc.unmodifiableArrayList("ORIGINAL_IMAGE_URL", "DETAIL_IMAGE_URL", "LARGE_IMAGE_URL", "MEDIUM_IMAGE_URL", "SMALL_IMAGE_URL");

    protected final GenericValue productCategory;
    protected final GenericValue productCategoryMediaDetails;
    protected final CategoryImageViewType origImageViewType;
    protected final GenericValue productContentView;
    protected final CategoryVariant original;
    protected final boolean variantSourceCheck;
    protected final boolean useParents;
    protected Map<String, CategoryVariant> variantMap;
    protected List<CategoryVariant> variantList;
    protected List<GenericValue> variantRecords;
    protected transient Map<String, CategoryImageLocationInfo.VariantLocation> defaultVariantLocations;
    protected transient String defaultStoredImageUrl;
    protected Timestamp queryMoment;

    protected CategoryImageVariants(GenericValue productCategory, String prodCatContentTypeId, GenericValue productCategoryContentView, GenericValue productCategoryMediaDetails, boolean useParents,
                                    Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache, Map<String, Object> options) throws GeneralException {
        super(delegator, dispatcher, locale, useEntityCache, options);
        if (options == null) {
            options = Collections.emptyMap();
        }
        this.productCategory = productCategory;
        this.origImageViewType = CategoryImageViewType.from(delegator, prodCatContentTypeId, false, true);
        this.productContentView = productCategoryContentView;
        this.productCategoryMediaDetails = productCategoryMediaDetails;
        if (productCategoryContentView != null) {
            Long imageWidth = productCategoryContentView.getLong("drScpWidth");
            Long imageHeight = productCategoryContentView.getLong("drScpHeight");
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            this.original = makeContentVariant(variantInfo, this.origImageViewType, productCategoryContentView);
        } else {
            Long imageWidth = (productCategoryMediaDetails != null) ? productCategoryMediaDetails.getLong("originalImageWidth") : null;
            Long imageHeight = (productCategoryMediaDetails != null) ? productCategoryMediaDetails.getLong("originalImageHeight") : null;
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            this.original = makeInlineVariant(variantInfo, this.origImageViewType, getProductCategoryCandidateFieldName(prodCatContentTypeId));
        }
        this.useParents = useParents;
        Boolean variantSourceCheck = (Boolean) options.get("variantSourceCheck");
        this.variantSourceCheck = (variantSourceCheck != null) ? variantSourceCheck : VARIANT_SOURCE_CHECK;
    }

    protected CategoryImageVariants() {
        this.productCategory = null;
        this.origImageViewType = null;
        this.productContentView = null;
        this.original = null;
        this.variantSourceCheck = VARIANT_SOURCE_CHECK;
        this.useParents = false;
        this.productCategoryMediaDetails = null;
    }

    /** Main factory method with util cache support. */
    public static CategoryImageVariants from(String productCategoryId, String prodCatContentTypeId, boolean useParents, Delegator delegator, LocalDispatcher dispatcher,
                                             Locale locale, boolean useUtilCache, Map<String, Object> options)  {
        if (UtilValidate.isEmpty(productCategoryId) || UtilValidate.isEmpty(prodCatContentTypeId)) {
            return null;
        }
        CategoryImageVariants civ;
        String cacheKey = null;
        if (useUtilCache) {
            cacheKey = productCategoryId + "::" + prodCatContentTypeId + "::" + useParents + "::" + delegator.getDelegatorName() + "::" + locale;
            civ = CACHE.get(cacheKey);
            if (civ != null) {
                return civ != NULL ? civ : null;
            }
        }
        civ = from(productCategoryId, prodCatContentTypeId, useParents, delegator, dispatcher, locale, options);
        if (useUtilCache) {
            CACHE.put(cacheKey, civ != null ? civ : NULL);
        }
        //if (civ == null) {
        //    Debug.logWarning("Could not load product category image records for category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "]", module);
        //}
        return civ;
    }

    /** Uncached factory method. */
    public static CategoryImageVariants from(String productCategoryId, String prodCatContentTypeId, boolean useParents, Delegator delegator, LocalDispatcher dispatcher,
                                             Locale locale, Map<String, Object> options) {
        GenericValue productCategory;
        GenericValue productCategoryContentView;
        GenericValue productCategoryMediaDetails;
        try {
            productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", productCategoryId), false);
            if (productCategory == null) {
                Debug.logWarning("Could not find category [" + productCategoryId + "]", module);
                return null;
            }
            productCategoryContentView = delegator.from("ProductCategoryContentAndInfo").where("productCategoryId", productCategoryId, "prodCatContentTypeId", prodCatContentTypeId)
                    .orderBy("-fromDate").filterByDate().queryFirst();
            productCategoryMediaDetails = delegator.findOne("CategoryMediaDetails", UtilMisc.toMap("productCategoryId", productCategoryId), false);

            if (productCategoryContentView == null && !"ORIGINAL_IMAGE_URL".equals(prodCatContentTypeId)) {
                return null;
            }

            CategoryImageVariants imageVariants = new CategoryImageVariants(productCategory, prodCatContentTypeId, productCategoryContentView, productCategoryMediaDetails, useParents,
                    delegator, dispatcher, locale, false, options);

            String originalImageUrl = imageVariants.getOriginal().getStaticImageUrl();
            if (UtilValidate.isNotEmpty(originalImageUrl)) {
                CategoryImageWorker.ensureCategoryImage(dispatcher.getDispatchContext(), locale, productCategory, prodCatContentTypeId, originalImageUrl, true, true);
            }

            return imageVariants;
        } catch (Exception e) {
            Debug.logError(e, module);
            return null;
        }
    }

    @Override
    public String getType() {
        return "category";
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
        return CategoryImageWorker.getDefaultCategoryImageProfileName(getDelegator(), getProdCatContentTypeId());
    }

    @Override
    public String getExplicitProfileName() {
        GenericValue productContentView = getProductCategoryContentView();
        if (productContentView != null) {
            return productContentView.getString("mediaProfile");
        } else {
            return getProductCategory().getString("imageProfile");
        }
    }

    @Override
    public CategoryVariant getOriginal() {
        return original;
    }

    // confusing
    //public GenericValue getRecord() {
    //    return getOriginal().getRecord();
    //}

    public GenericValue getProductCategory() {
        return productCategory;
    }

    public GenericValue getProductCategoryMediaDetails() {
        return productCategoryMediaDetails;
    }

    public String getProductCategoryId() {
        return productCategory.getString("productCategoryId");
    }

    public GenericValue getProductCategoryContentView() {
        return productContentView;
    }

    public String getProdCatContentTypeId() {
        return getOrigImageViewType().getContentTypeId();
    }

    public CategoryImageViewType getOrigImageViewType() {
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
                GenericValue productCategory = getProductCategory();
                for (String pcctId : PCCTID_PRIO_LIST) {
                    String fieldName = getProductCategoryCandidateFieldName(pcctId);
                    if (productCategory.hasModelField(fieldName)) {
                        defaultStoredImageUrl = productCategory.getString(fieldName);
                        if (defaultStoredImageUrl != null) {
                            break;
                        }
                    }
                }
                if (defaultStoredImageUrl == null) {
                    // TODO: reuse makeVariants query - circular loop issue currently
                    List<GenericValue> pccdrList = getDelegator().from("ProductCategoryContentAndInfo")
                            .where(EntityCondition.makeCondition("productCategoryId", getProductCategoryId()),
                                    EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.LIKE, "%_IMAGE_URL"))
                            .orderBy("-fromDate").filterByDate(getQueryMoment()).cache(false).queryListSafe();
                    if (pccdrList != null) {
                        for(String pcctId : PCCTID_PRIO_LIST) {
                            for(GenericValue pccdr : pccdrList) {
                                if (pcctId.equals(pccdr.getString("prodCatContentTypeId"))) {
                                    defaultStoredImageUrl = getRecordImageUrl(pccdr);
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
                Debug.logWarning("No default image URL for category [" + getProductCategoryId() + "]", module);
            }
            this.defaultStoredImageUrl = defaultStoredImageUrl;
        }
        return defaultStoredImageUrl;
    }

    public CategoryImageLocationInfo getDefaultLocationInfo() {
        // TODO: REVIEW: caching appropriate when?
        CategoryImageLocationInfo locationInfo = null;
        try {
            String imageUrl = getDefaultStoredImageUrl();
            if (imageUrl == null) {
                throw new IllegalStateException("No stored default image URL found for category/prodCatContentTypeId");
            }
            locationInfo = CategoryImageLocationInfo.from(getDctx(), getProductCategoryId(), getOrigImageViewType(), getProfile(),
                    imageUrl, null, false, true, null);
        } catch (Exception e) {
            Debug.logError(e, "Error getting category image location info for category [" + getProductCategoryId()
                    + "] prodCatContentTypeId [" + getProdCatContentTypeId() + "]", module);
        }
        return locationInfo;
    }

    public Map<String, CategoryImageLocationInfo.VariantLocation> getDefaultVariantLocations() {
        Map<String, CategoryImageLocationInfo.VariantLocation> defaultVariantLocations = this.defaultVariantLocations;
        if (defaultVariantLocations == null) {
            try {
                defaultVariantLocations = getDefaultLocationInfo().getVariantLocations();
            } catch (Exception e) {
                Debug.logError(e, "Error getting category image location info for category [" + getProductCategoryId()
                        + "] prodCatContentTypeId [" + getProdCatContentTypeId() + "]", module);
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

    protected boolean isProductCategoryField(String fieldName) {
        return getDelegator().getModelEntity("ProductCategory").isField(fieldName);
    }

    protected String getProductCategoryCandidateFieldName(String productContentTypeId) {
        return ModelUtil.dbNameToVarName(productContentTypeId);
    }

    @Override
    public Map<String, CategoryVariant> getVariantMap() {
        Map<String, CategoryVariant> variants = this.variantMap;
        if (variants == null) {
            variants = new LinkedHashMap<>();
            variants = makeVariants(variants);
            variants = variants.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(variants);
            this.variantMap = variants;
        }
        return variants;
    }

    @Override
    public List<CategoryVariant> getVariantList() {
        List<CategoryVariant> variantList = this.variantList;
        if (variantList == null) {
            Map<String, CategoryVariant> variants = getVariantMap();
            variantList = variants.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(variants.values()));
            this.variantList = variantList;
        }
        return variantList;
    }

    @Override
    public Map<String, CategoryVariant> getOriginalAndVariantMap() {
        return UtilGenerics.cast(super.getOriginalAndVariantMap());
    }

    @Override
    public List<CategoryVariant> getOriginalAndVariantList() {
        return UtilGenerics.cast(super.getOriginalAndVariantList());
    }

    protected Map<String, CategoryVariant> makeVariants(Map<String, CategoryVariant> variants) {
        Map<String, GenericValue> sizeTypeRecordMap;
        try {
            sizeTypeRecordMap = CategoryImageWorker.getVariantProductCategoryContentDataResourceRecordsByViewSize(getDelegator(),
                    getProductCategoryId(), getOrigImageViewType(), getQueryMoment(), false, isUseEntityCache());
        } catch (GeneralException e) {
            Debug.logError(e, "makeVariants: " + e.toString(), module);
            sizeTypeRecordMap = Collections.emptyMap(); // this can sometimes proceed, sometimes not
        }

        for(ImageVariantConfig.VariantInfo config : getVariantConfig().getVariantList()) {
            String sizeType = config.getName();
            CategoryVariant variant;
            GenericValue variantRecord = sizeTypeRecordMap.get(sizeType);

            CategoryImageViewType variantImageViewType;
            try {
                variantImageViewType = CategoryImageViewType.from(getDelegator(),
                        getOrigImageViewType().getViewType(), getOrigImageViewType().getViewNumber(), sizeType,
                        false, true, true);
            } catch (Exception e) {
                Debug.logError("Unable to derive variantImageViewType for sizeType [" + sizeType +
                        "] for productCategoryId [" + getProductCategoryId() + "] imageViewType " + getOrigImageViewType() + ": " + e.toString(), module);
                continue;
            }

            if (variantRecord != null) {
                variant = makeContentVariant(config, variantImageViewType, variantRecord);
            } else {
                String fieldName = getProductCategoryCandidateFieldName(variantImageViewType.getContentTypeId());
                if (isProductCategoryField(fieldName)) {
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

    public abstract class CategoryVariant extends Variant {
        protected final CategoryImageViewType variantImageViewType;
        protected String staticImageUrl;
        protected String mimeTypeId;

        protected CategoryVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType) {
            super(config);
            this.variantImageViewType = variantImageViewType;
        }

        public CategoryImageViewType getVariantImageViewType() {
            return variantImageViewType;
        }

        @Override
        public String getAssocId() {
            return getVariantImageViewType().getContentTypeId();
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
                        Debug.logInfo("Could not determine an image url for category [" + getProductCategoryId()
                                + "] productContentTypeId [" + CategoryImageVariants.this.getProdCatContentTypeId()
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
            return CategoryImageVariants.this.getRecordImageUrl(getRecord());
        }

        /** Gets the Product xxxImageUrl field: originalImageUrl, detailImageUrl, largeImageUrl, mediumImageUrl, smallImageUrl, ... */
        public String getFieldImageUrl() {
            // Based on ProductContentWrapper
            String fieldName = getProductCategoryUrlFieldName();
            if (fieldName == null || !isProductCategoryField(fieldName)) {
                return null;
            }
            String candidateValue = getProductCategory().getString(fieldName);
            if (UtilValidate.isNotEmpty(candidateValue)) {
                return candidateValue;
            } else if (isUseParents() && "Y".equals(getProductCategory().getString("isVariant"))) {
                GenericValue parent = ProductWorker.getParentProduct(getProductCategoryId(), getDelegator(), isUseEntityCache());
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
            CategoryImageLocationInfo.VariantLocation loc = getDefaultVariantLocations().get(getName());
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
                Debug.logWarning("Invalid url for image for category [" + getProductCategoryId() + "] prodCatContentTypeId ["
                        + CategoryImageVariants.this.getProdCatContentTypeId() + "] variant [" + getName()
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
                Debug.logWarning("Could not determine mimeTypeId for category [" + getProductCategoryId() + "] prodCatContentTypeId ["
                        + CategoryImageVariants.this.getProdCatContentTypeId() + "] variant [" + getName() + "]; no image path", module);
                return null;
            }
            int lastDot = path.lastIndexOf('.');
            if (lastDot < 0 || lastDot >= (path.length() - 1)) {
                Debug.logWarning("Could not determine mimeTypeId for category [" + getProductCategoryId() + "] prodCatContentTypeId ["
                        + CategoryImageVariants.this.getProdCatContentTypeId() + "] variant [" + getName() + "]; no extension in path [" + path + "]", module);
                return null;
            }
            String ext = path.substring(lastDot + 1);
            GenericValue fileExt = getDelegator().from("FileExtension").where("fileExtensionId", ext).cache().queryOneSafe();
            if (fileExt == null) {
                Debug.logWarning("Could not determine mimeTypeId for category [" + getProductCategoryId() + "] prodCatContentTypeId ["
                        + CategoryImageVariants.this.getProdCatContentTypeId() + "] variant [" + getName() + "]; unrecognized file extension [" + path + "]", module);
                return null;
            }
            return fileExt.getString("mimeTypeId");
        }

        @Override
        public boolean hasSource() {
            // explicit record is considered as having source (even if file were missing)
            return (getStaticImageUrl() != null);
        }

        protected String getProductCategoryUrlFieldName() {
            return getProductCategoryCandidateFieldName(getAssocId());
        }

        protected <T> T getProductCategoryMediaDetailsFieldValue(String suffixField) {
            GenericValue pmd = getProductCategoryMediaDetails();
            if (pmd == null) {
                return null;
            }
            String prefix = CategoryImageWorker.getProductCategoryInlineImageFieldPrefix(getDelegator(), getAssocId());
            if (prefix == null) {
                return null;
            }
            return UtilGenerics.cast(pmd.get(prefix + suffixField.substring(0, 1).toUpperCase() + suffixField.substring(1)));
        }
    }

    protected ContentProductCategoryVariant makeContentVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType, GenericValue record) {
        return new ContentProductCategoryVariant(config, variantImageViewType, record);
    }

    public class ContentProductCategoryVariant extends CategoryVariant {
        protected final GenericValue record;

        protected ContentProductCategoryVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType, GenericValue record) {
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

    protected InlineProductCategoryVariant makeInlineVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType, String fieldName) {
        return new InlineProductCategoryVariant(config, variantImageViewType, fieldName);
    }

    /**
     * InlineProductVariant.
     * TODO: REVIEW: Currently this is also created for variants missing records and not having any inline fields.
     */
    public class InlineProductCategoryVariant extends CategoryVariant {
        protected final String fieldName;

        protected InlineProductCategoryVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType, String fieldName) {
            super(config, variantImageViewType);
            this.fieldName = fieldName;
        }

        @Override
        public String getContentId() {
            return null;
        }

        @Override
        public GenericValue getRecord() {
            return getProductCategory();
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
            Long width = getProductCategoryMediaDetailsFieldValue("width");
            return (width != null) ? width.intValue() : null;
        }

        @Override
        public Integer getImageHeight() {
            Long height = getProductCategoryMediaDetailsFieldValue("height");
            return (height != null) ? height.intValue() : null;
        }
        @Override
        public String getExplicitMimeTypeId() {
            return getProductCategoryMediaDetailsFieldValue("mimeTypeId");
        }

        @Override
        public boolean hasSource() {
            return super.hasSource();
        }
    }

    protected DefaultProductCategoryVariant makeDefaultVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType) {
        return new DefaultProductCategoryVariant(config, variantImageViewType);
    }

    public class DefaultProductCategoryVariant extends CategoryVariant {

        protected DefaultProductCategoryVariant(ImageVariantConfig.VariantInfo config, CategoryImageViewType variantImageViewType) {
            super(config, variantImageViewType);
        }

        @Override
        public String getContentId() {
            return null;
        }

        @Override
        public String getAssocId() {
            return getVariantImageViewType().getContentTypeId();
        }

        @Override
        public GenericValue getRecord() {
            return getProductCategory();
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
            CategoryImageLocationInfo.VariantLocation loc = getDefaultVariantLocations().get(getName());
            return loc != null && loc.hasSourceFile();
        }
    }

    @Override
    public Map<String, Object> getResponsiveVariantMap(String targetMimeTypeId, Map<String, ? extends Number> sizeDefs,
                                                       Map<String, Object> context, Map<String, Object> urlArgs) {
        // TODO: REVIEW: from ContentImageVariants#getResponsiveVariantMap
        /* Example:
        <#local sizeMap = getImageVariants("category", productId)!false/><#-- now accepts empty contentId, will return null/false -->
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
                // NOTE: imageWidth is physical width, it *could* be missing for old data (and non-updated category images),
                // so may want configWidth instead...
                Integer width1 = o1.getImageWidth();
                if (width1 == null) { width1 = o1.getConfigWidth(); }
                Integer width2 = o2.getImageWidth();
                if (width2 == null) { width2 = o2.getConfigWidth(); }
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
                    Integer width = variant.getImageWidth();
                    if (width == null) { width = variant.getConfigWidth(); }
                    if (width != null && width >= sizeDefNum.intValue()) {
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
                    Integer width = bestSize.getImageWidth();
                    if (width == null) { width = bestSize.getConfigWidth(); }
                    srcset.put(url, width);
                    srcsetSize.put(url, sizeDefKey);
                }
            }
            if (bestSizeTarget != null) {
                String url = bestSizeTarget.getImageUrl(context, urlArgs);
                if (UtilValidate.isNotEmpty(url)) {
                    Integer width = bestSizeTarget.getImageWidth();
                    if (width == null) { width = bestSizeTarget.getConfigWidth(); }
                    srcsetTarget.put(url, width);
                    srcsetSizeTarget.put(url, sizeDefKey);
                }
            }
        }

        return UtilMisc.toMap("srcset", srcset, "srcsetTarget", srcsetTarget,
                "srcsetSize", srcsetSize, "srcsetSizeTarget", srcsetSizeTarget);
    }

    public static Map<String, Object> clearCaches(ServiceContext ctx) {
        String productCategoryId = ctx.attr("productCategoryId");
        String prodCatContentTypeId = ctx.attr("prodCatContentTypeId");

        if (UtilValidate.isNotEmpty(productCategoryId)) {
            String prefix = UtilValidate.isNotEmpty(prodCatContentTypeId) ? productCategoryId + "::" + prodCatContentTypeId + "::" : productCategoryId + "::";
            clearCachesByPrefix(ctx.delegator(), prefix);
        } else {
            CACHE.clear();
        }

        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.toMap("productCategoryId", productCategoryId, "prodCatContentTypeId", prodCatContentTypeId);
                dcc.runDistributedService("categoryImageVariantsDistributedClearCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccess();
    }

    public static void clearCachesByPrefix(Delegator delegator, String prefix) { // SCIPIO
        CACHE.removeByFilter((k, v) -> k.startsWith(prefix));
        CategoryContentWrapper.clearCachesByPrefix(delegator, prefix);
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
