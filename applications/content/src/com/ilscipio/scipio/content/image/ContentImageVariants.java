package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.ftl.WebappUrlDirective;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
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
    protected String originalContentId;
    protected Map<String, ContentVariant> variantMap;
    protected List<ContentVariant> variantList;
    protected List<GenericValue> variantRecords;

    protected ContentImageVariants(GenericValue contentView, Delegator delegator, LocalDispatcher dispatcher, Locale locale,
                                   boolean useEntityCache, Map<String, Object> options) {
        super(delegator, dispatcher, locale, useEntityCache, options);
        this.contentView = contentView;
        this.originalContentId = contentView.getString("contentId");
    }

    protected ContentImageVariants() {
        this.contentView = null;
    }

    /** Main factory method with util cache support. */
    public static ContentImageVariants from(String contentId, Delegator delegator, LocalDispatcher dispatcher, Locale locale,
                                            boolean useUtilCache, Map<String, Object> options) {
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
        civ = from(contentId, delegator, dispatcher, locale, options);
        if (useUtilCache) {
            CACHE.put(cacheKey, civ != null ? civ : NULL);
        }
        //if (civ == null) {
        //    Debug.logWarning("Could not load content image record [" + contentId + "]", module);
        //}
        return civ;
    }

    /** Uncached factory method. */
    public static ContentImageVariants from(String contentId, Delegator delegator, LocalDispatcher dispatcher, Locale locale, Map<String, Object> options) {
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
        return new ContentImageVariants(record, delegator, dispatcher, locale, false, options);
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
            ImageProfile imageProfile = ContentImageWorker.getContentImageProfileOrDefault(getDelegator(), getContentView(), isUseEntityCache(), false);
            if (imageProfile != null) {
                mediaProfileName = imageProfile.getName();
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
        return getContentView().getString("mediaProfile");
    }

    // confusing
    //public GenericValue getRecord() {
    //    return getContentView();
    //}

    public GenericValue getContentView() {
        return contentView;
    }

    @Override
    public ContentVariant getOriginal() {
        ContentVariant original = this.original;
        if (original == null) {
            Long imageWidth = getContentView().getLong("drScpWidth");
            Long imageHeight = getContentView().getLong("drScpHeight");
            ImageVariantConfig.VariantInfo variantInfo = new ImageVariantConfig.VariantInfo("original",
                    (imageWidth != null) ? imageWidth : -1, (imageHeight != null) ? imageHeight : -1, null, null);
            original = makeOriginalVariant(variantInfo, getContentView());
            this.original = original;
        }
        return original;
    }

    public String getOriginalContentId() {
        return originalContentId;
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

    @Override
    public Map<String, ContentVariant> getOriginalAndVariantMap() {
        return UtilGenerics.cast(super.getOriginalAndVariantMap());
    }

    @Override
    public List<ContentVariant> getOriginalAndVariantList() {
        return UtilGenerics.cast(super.getOriginalAndVariantList());
    }

    protected Map<String, ContentVariant> makeVariants(Map<String, ContentVariant> variants, List<GenericValue> variantRecords) {
        Map<String, ContentVariant> recordVariants = new LinkedHashMap<>();
        for (GenericValue variantRecord : variantRecords) {
            String name = variantRecord.getString("caMapKey");
            if (name == null) {
                Debug.logError("Variant record [" + variantRecord.get("contentId") + "] missing sizeType name (caMapKey, ContentAssoc.mapKey)", module);
                continue;
            }
            ImageVariantConfig.VariantInfo variantConfig = getVariantConfig().getVariant(name);
            if (variantConfig == null) {
                Debug.logWarning("Variant record [" + variantRecord.get("contentId") + "] sizeType/caMapKey [" +
                        name + "] not found in variant config [" + name + "]; discarding variant; data may be out of date" +
                        " with image properties config (contentImageAutoRescale/productImageAutoRescale needed?)", module);
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
            try {
                // TODO: REVIEW: may not 100% coincide with RequestHandler.makeLink yet, close enough for now
                if (args == null) {
                    args = Collections.emptyMap();
                }
                String webSiteId = (String) args.get("webSiteId");
                FullWebappInfo targetWebappInfo = getUrlTargetWebapp(context, args, webSiteId);
                String uri = getMediaServletPath(targetWebappInfo) + getMediaImageBaseParams(context, args);
                return WebappUrlDirective.AppUrlDirective.getInstance().makeLinkForContext(context, args, uri, webSiteId, null);
            } catch(Exception e) {
                Debug.logError("Error building URL for image variant content [" + getContentId() + "]", module);
                return null;
            }
        }

        @Override
        public String getPlainImageUrl(Map<String, Object> context, Map<String, Object> args) {
            try {
                // TODO: REVIEW: may not 100% coincide with RequestHandler.makeLink yet, close enough for now
                if (args == null) {
                    args = Collections.emptyMap();
                }
                String webSiteId = (String) args.get("webSiteId");
                FullWebappInfo targetWebappInfo = getUrlTargetWebapp(context, args, webSiteId);
                String uri = getMediaServletPath(targetWebappInfo) + getMediaImageBaseParams(context, args);
                args = new HashMap<>(args);
                args.putIfAbsent("fullPath", false);
                args.putIfAbsent("secure", false);
                args.putIfAbsent("encode", false);
                return WebappUrlDirective.AppUrlDirective.getInstance().makeLinkForContext(context, args, uri, webSiteId, null);
            } catch(Exception e) {
                Debug.logError("Error building URL for image variant content [" + getContentId() + "]", module);
                return null;
            }
        }

        protected String getMediaImageBaseParams(Map<String, Object> context, Map<String, Object> args) {
            String paramStr = getStaticImageUrl(); // contentId
            if (isOriginal()) {
                return paramStr;
            }
            Map<String, Object> params = UtilGenerics.cast(args.get("params"));
            if (params == null) {
                params = Collections.emptyMap();
            }
            if (params.containsKey("autoVariant") || params.containsKey("variant")) {
                // don't add variant
                return paramStr;
            }
            paramStr += (paramStr.contains("?") ? "&" : "?") + "variant=" + UtilCodec.getUrlEncoder().encode(getName());
            return paramStr;
        }

        protected String getMediaServletPath(FullWebappInfo targetWebappInfo) {
            String mediaPath = (targetWebappInfo != null) ? targetWebappInfo.getServletMappingMountPoint("CmsMediaServlet") : null;
            return (mediaPath != null) ? mediaPath : "/media";
        }

        @Override
        public String getStaticImageUrl() {
            String contentPath = getContentView().getString("contentPath");
            if (contentPath != null) {
                return "/" + contentPath;
            }
            return "?contentId=" + UtilCodec.getUrlEncoder().encode(getOriginalContentId());
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

    @Override
    public Map<String, Object> getResponsiveVariantMap(String targetMimeTypeId, Map<String, ? extends Number> sizeDefs,
                                                       Map<String, Object> context, Map<String, Object> urlArgs) {
        /* Example:
        <#local sizeMap = getImageVariants("content", contentId)!false/><#-- now accepts empty contentId, will return null/false -->
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

    public static void clearCaches(Delegator delegator) {
        CACHE.clear();
    }

    public static Map<String, Object> clearCaches(ServiceContext ctx) {
        clearCaches((Delegator) null);

        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.newMap(); // UtilMisc.toMap("type", ctx.attr("type"));
                dcc.runDistributedService("contentImageVariantsDistributedClearCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccess();
    }
}
