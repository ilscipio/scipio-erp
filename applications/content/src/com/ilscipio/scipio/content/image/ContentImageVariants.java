package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.RequestHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Content/CMS media image and variants accessor and dedicated cache (SCIPIO).
 */
public class ContentImageVariants extends ImageVariants {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final ContentImageVariants NULL = new ContentImageVariants();
    private static final UtilCache<String, ContentImageVariants> CACHE = UtilCache.createUtilCache("content.image.variants", true);

    protected final GenericValue contentView;
    protected String mediaProfileName;
    protected ContentVariant original;
    protected Map<String, ContentVariant> variantMap;
    protected List<ContentVariant> variantList;
    protected List<GenericValue> variantRecords;

    protected ContentImageVariants(GenericValue contentView, Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache) {
        super(delegator, dispatcher, locale, useEntityCache);
        this.contentView = contentView;
    }

    protected ContentImageVariants() {
        this.contentView = null;
    }

    /** Main factory method with util cache support. */
    public static ContentImageVariants from(String contentId, Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useUtilCache) {
        if (UtilValidate.isEmpty(contentId)) {
            return null;
        }
        ContentImageVariants civ;
        String cacheKey = null;
        if (useUtilCache) {
            cacheKey = contentId + "::" + delegator.getDelegatorName() + "::" + locale;
            civ = CACHE.get(cacheKey);
            if (civ != null) {
                return civ != NULL ? civ : null;
            }
        }
        civ = from(contentId, delegator, dispatcher, locale);
        if (useUtilCache) {
            CACHE.put(cacheKey, civ != null ? civ : NULL);
        }
        //if (civ == null) {
        //    Debug.logWarning("Could not load content image record [" + contentId + "]", module);
        //}
        return civ;
    }

    /** Uncached factory method. */
    public static ContentImageVariants from(String contentId, Delegator delegator, LocalDispatcher dispatcher, Locale locale) {
        GenericValue record;
        try {
            record = EntityUtil.getFirst(delegator.findByAnd("ContentDataResourceRequiredView",
                    UtilMisc.toMap("contentId", contentId), null, false));
        } catch (GeneralException e) {
            Debug.logError(e, module);
            return null;
        }
        if (record == null) {
            Debug.logWarning("Could not find content image [" + contentId + "]", module);
            return null;
        }
        return new ContentImageVariants(record, delegator, dispatcher, locale, false);
    }

    @Override
    public String getType() {
        return "content";
    }

    @Override
    public String getContentId() {
        return getContentView().getString("contentId");
    }

    @Override
    public String getProfileName() {
        String mediaProfileName = this.mediaProfileName;
        if (mediaProfileName == null) {
            try {
                mediaProfileName = ContentImageWorker.getContentImageMediaProfileOrDefault(getRecord(), isUseEntityCache());
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            if (mediaProfileName == null) {
                mediaProfileName = "";
            }
            this.mediaProfileName = mediaProfileName;
        }
        return mediaProfileName.isEmpty() ? null : mediaProfileName;
    }

    @Override
    public String getExplicitProfileName() {
        return getRecord().getString("mediaProfile");
    }

    public GenericValue getRecord() {
        return getContentView();
    }

    public GenericValue getContentView() {
        return contentView;
    }

    @Override
    public ContentVariant getOriginal() {
        ContentVariant original = this.original;
        if (original == null) {
            Long imageWidth = getRecord().getLong("drScpWidth");
            Long imageHeight = getRecord().getLong("drScpHeight");
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            original = makeOriginalVariant(variantInfo, getRecord());
            this.original = original;
        }
        return original;
    }

    @Override
    public Map<String, ContentVariant> getVariantMap() {
        Map<String, ContentVariant> variants = this.variantMap;
        if (variants == null) {
            variants = new LinkedHashMap<>();
            variants = makeVariants(variants, getVariantRecords());
            variants = variants.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(variants);
            this.variantMap = variants;
        }
        return variants;
    }

    @Override
    public List<ContentVariant> getVariantList() {
        List<ContentVariant> variantList = this.variantList;
        if (variantList == null) {
            Map<String, ContentVariant> variants = getVariantMap();
            variantList = variants.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(variants.values()));
            this.variantList = variantList;
        }
        return variantList;
    }

    protected Map<String, ContentVariant> makeVariants(Map<String, ContentVariant> variants, List<GenericValue> variantRecords) {
        Map<String, ContentVariant> recordVariants = new LinkedHashMap<>();
        for (GenericValue variantRecord : variantRecords) {
            String name = variantRecord.getString("caMapKey");
            if (name == null) {
                Debug.logError("Variant record [" + variantRecord.get("contentId") + "] missing mapKey/sizeType", module);
                continue;
            }
            ImageVariantConfig.VariantInfo variantConfig = getVariantConfig().getVariant(name);
            if (variantConfig == null) {
                Debug.logError("Variant record [" + variantRecord.get("contentId") + "] mapKey/sizeType ["
                        + name + "] not found in variant config [" + variantConfig.getName() + "]", module);
                continue;
            }
            ContentVariant variant = makeVariant(variantConfig, variantRecord);
            recordVariants.put(variant.getName(), variant);
        }
        for(ImageVariantConfig.VariantInfo config : getVariantConfig().getVariantList()) {
            ContentVariant variant = recordVariants.remove(config.getName());
            if (variant != null) {
                variants.put(variant.getName(), variant);
            }
        }
        // TODO: REVIEW: it's possible we don't want this putAll because the variant list can temporarily or erroneously
        //  differ, but for now just put everything we found until this behavior is clarified...
        variants.putAll(recordVariants);
        return variants;
    }

    public List<GenericValue> getVariantRecords() {
        List<GenericValue> variantRecords = this.variantRecords;
        if (variantRecords == null) {
            try {
                variantRecords = ContentImageWorker.getResizedImageContentAssocDataResourceRecords(getDelegator(), getContentId(), isUseEntityCache());
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            if (variantRecords == null) {
                variantRecords = Collections.emptyList(); // keeps failures under control
            }
            this.variantRecords = variantRecords;
        }
        return variantRecords;
    }

    protected ContentVariant makeVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
        return new ContentVariant(config, record);
    }

    public class ContentVariant extends Variant {
        protected final GenericValue record;

        protected ContentVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
            super(config);
            this.record = record;
        }

        @Override
        public String getContentId() {
            return getRecord().getString("contentId");
        }

        @Override
        public String getAssocId() {
            return getRecord().getString("caContentAssocTypeId");
        }

        @Override
        public GenericValue getRecord() {
            return record;
        }

        @Override
        public String getImageUrl(Map<String, Object> context, Map<String, Object> args) {
            // FIXME: INCOMPLETE/HARDCODED
            HttpServletRequest request = UtilGenerics.cast(context.get("request"));
            HttpServletResponse response = UtilGenerics.cast(context.get("response"));
            Boolean fullPath = (Boolean) args.get("fullPath");
            Boolean secure = (Boolean) args.get("secure");
            Boolean encode = (Boolean) args.get("encode");
            String uri = appendUrlParams("media"+ getBaseImageUrl(), UtilGenerics.cast(args.get("params")));
            String url = null;
            try {
                url = RequestHandler.makeLinkAuto(request, response, uri, false, false, null,
                        false, fullPath, secure, encode);
            } catch(Exception e) {
                Debug.logError("Error building URL for image variant content [" + getContentId() + "] uri [" + uri + "]", module);
            }
            return url;
        }

        @Override
        public String getBaseImageUrl() {
            return "?content=" + UtilCodec.getUrlEncoder().encode(getContentId());
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
            return getExplicitMimeTypeId();
        }

        @Override
        public String getExplicitMimeTypeId() {
            return getRecord().getString("drMimeTypeId");
        }
    }

    protected OriginalContentVariant makeOriginalVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
        return new OriginalContentVariant(config, record);
    }

    public class OriginalContentVariant extends ContentVariant {
        protected OriginalContentVariant(ImageVariantConfig.VariantInfo config, GenericValue record) {
            super(config, record);
        }

        @Override
        public String getAssocId() {
            return null;
        }
    }
}
