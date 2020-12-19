package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.ftl.WebappUrlDirective;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Image variants records lookup class, for use in frontend or anywhere else (SCIPIO).
 * Similar to CommonContentWrapper.
 * See ContentImageVariants, ProductImageVariants for implementations.
 * NOTE: This class can be considered thread-safe but is optimized without volatile where possible.
 */
public abstract class ImageVariants implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // Basic variables (CommonContentWrapper)
    private transient Delegator delegator;
    private final String delegatorName;
    private transient LocalDispatcher dispatcher;
    private final String dispatcherName;
    private final Locale locale;
    private final boolean useEntityCache;

    // Common image variables
    protected transient ImageProfile profile;
    protected transient ImageProfile explicitProfile;

    protected ImageVariants(Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache, Map<String, Object> options) {
        this.delegator = delegator;
        this.delegatorName = delegator.getDelegatorName();
        this.dispatcher = dispatcher;
        this.dispatcherName = dispatcher.getName();
        this.locale = locale;
        this.useEntityCache = useEntityCache;
    }

    protected ImageVariants() {
        this.delegatorName = null;
        this.dispatcherName = null;
        this.locale = null;
        this.useEntityCache = false;
    }

    public abstract String getType();

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

    public Locale getLocale() {
        return locale;
    }

    public boolean isUseEntityCache() {
        return useEntityCache;
    }

    /** Returns the contentId associated with the image or null if the image is an inlined entity field such as Product.originalImageUrl. */
    public abstract String getContentId();

    /** Returns the ImageProfile name or default for the image. For the profile directly associated with the image, use {@link #getExplicitProfileName()}. */
    public abstract String getProfileName();

    /** Returns the explicit ImageProfile name directly associated with the image, or null if none. */
    public abstract String getExplicitProfileName();

    /** Returns the ImageProfile or default for the image. For the profile directly associated with the image, use {@link #getExplicitProfile()}. */
    public ImageProfile getProfile() {
        ImageProfile profile = this.profile;
        if (profile == null) {
            profile = ImageProfile.getImageProfile(getDelegator(), getProfileName(), false);
            this.profile = profile;
        }
        return profile;
    }

    /** Returns the explicit ImageProfile directly associated with the image, or null if none. */
    public ImageProfile getExplicitProfile() {
        ImageProfile explicitProfile = this.explicitProfile;
        if (explicitProfile == null) {
            explicitProfile = ImageProfile.getImageProfile(getDelegator(), getExplicitProfileName(), false);
            this.explicitProfile = explicitProfile;
        }
        return explicitProfile;
    }

    /** Returns the static variant configuration (ImageProperties.xml or ImageSizePreset/CMS) */
    public ImageVariantConfig getVariantConfig() {
        ImageProfile profile = getProfile();
        return (profile != null) ? profile.getVariantConfig() : null;
    }

    public ImageVariantConfig getExplicitVariantConfig() {
        ImageProfile explicitProfile = getExplicitProfile();
        return (explicitProfile != null) ? explicitProfile.getVariantConfig() : null;
    }

    public abstract Variant getOriginal();

    /**
     * Returns map of image variants by sizeType ("small", "800x600", ...), excluding "original" ({@link #getOriginal()}.
     * NOTE: If cache.properties#product.image.variants.sourceCheck=true (product images only), this returns empty if no physically defined
     * variants are detected.
     */
    public abstract Map<String, ? extends Variant> getVariantMap();

    /** Returns list of image variants, excluding "original" ({@link #getOriginal()}. */
    public abstract List<? extends Variant> getVariantList();

    /**
     * Returns the source check setting, which controls whether Variant instances with or without checking for source
     * (file) presence. Currently this may only be false for cache.properties#product.image.variants.sourceCheck=false
     * for product images only.
     */
    public boolean isVariantSourceCheck() {
        return true;
    }

    /**
     * Checks if any Variant instances are defined.
     * NOTE: If cache.properties#product.image.variants.sourceCheck=true (product images only), this returns false if no physically defined
     * variants are detected.
     */
    public boolean hasVariants() {
        return !getVariantMap().isEmpty();
    }

    /**
     * Performs a DEEP (file-based) check to see if the image currently has physically available variants.
     * IGNORES the cache.properties#product.image.variants.sourceCheck setting. Depends on implementation.
     * See also {@link Variant#hasSource()}.
     */
    public boolean hasSourcedVariants() {
        for(Map.Entry<String, ? extends Variant> entry : getVariantMap().entrySet()) {
            if (entry.getValue().hasSource()) {
                return true;
            }
        }
        return false;
    }

    public Variant getVariant(String name) {
        if ("original".equals(name)) {
            return getOriginal();
        }
        return getVariantMap().get(name);
    }

    /**
     * Represents an image variant, for the original image represented by {@link #getOriginal()} including itself.
     *
     * <p>NOTE: The presence of an instance does not necessarily guarantee that a file is immediately available.
     * This is controlled by the setting cache.properties#product.image.variants.sourceCheck (currently only
     * meaningful for product images). When sourceCheck=true, instances
     * are returned only when the source/file is immediately available. It can be set to false so that instances are only
     * created when source/file available. It can also be left to false and {@link #hasSource()} manually checked in code instead.</p>
     */
    public abstract class Variant implements Serializable {
        protected final String name;
        protected ImageVariantConfig.VariantInfo config; // TODO: REVIEW: did not put this transient because some callers

        protected Variant(String name) {
            this.name = name;
        }

        protected Variant(ImageVariantConfig.VariantInfo config) {
            this.name = config.getName();
            this.config = config;
        }

        public String getName() {
            return name;
        }

        public boolean isOriginal() {
            return this == getOriginal();
        }

        /** Returns the associated contentId for the image variant or null if not applicable (implementation). */
        public abstract String getContentId();

        /** Returns contentAssocTypeId, productContentTypeId, other or null (implementation). */
        public abstract String getAssocId();

        /** Returns the associated record for the image variant or null if not applicable (implementation). */
        public abstract GenericValue getRecord();

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...).
         * This produces a URL equivalent to calling the @contentUrl or @pageUrl Freemarker utilities.ftl macros.
         * <p>context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).</p>
         * @see #getPlainImageUrl(Map, Map)
         */
        public abstract String getImageUrl(Map<String, Object> context, Map<String, Object> args);

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...).
         * This produces a URL equivalent to calling the @contentUrl or @pageUrl Freemarker utilities.ftl macros.
         * <p>context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).</p>
         * @see #getImageUrl(Map, Map)
         */
        public String getImageUrl(Map<String, Object> context) {
            return getImageUrl(context, Collections.emptyMap());
        }

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...)
         * without URL encoding or any extras other than URL parameters.
         * <p>URLs returned from this method are not encoded with @contentUrl or @pageUrl and may need an extra call for URL encoding,
         * but in such case it's preferable to call {@link #getImageUrl(Map, Map)} since the URL encoding method usually depends
         * on the type of link (content, product, etc.).</p>
         * <p>context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).</p>
         * @see #getImageUrl(Map, Map)
         */
        public abstract String getPlainImageUrl(Map<String, Object> context, Map<String, Object> args);

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...)
         * without URL encoding or any extras other than URL parameters.
         * <p>URLs returned from this method are not encoded with @contentUrl or @pageUrl and may need an extra call for URL encoding,
         * but in such case it's preferable to call {@link #getImageUrl(Map, Map)} since the URL encoding method usually depends
         * on the type of link (content, product, etc.).</p>
         * <p>context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).</p>
         * @see #getPlainImageUrl(Map, Map)
         * @see #getImageUrl(Map, Map)
         */
        public String getPlainImageUrl(Map<String, Object> context) {
            return getPlainImageUrl(context, Collections.emptyMap());
        }

        /** Returns the stored image URL of the original (non-resized) image or null if cannot be determined without parameters or context. */
        public abstract String getStaticImageUrl();

        /** Returns the physical variant image width, or null if missing on the record. */
        public abstract Integer getImageWidth();

        /** Returns the physical variant image height, or null if missing on the record. */
        public abstract Integer getImageHeight();

        /** Returns the mimeTypeId for the image (stored as-is or looked up). */
        public abstract String getMimeTypeId();

        /** Returns the explicit mimeTypeId stored for the image or null if not applicable. */
        public abstract String getExplicitMimeTypeId();

        /** Returns the configured preset variant image width, from ImageProperties.xml (file) or ImageSizePreset/ImageSizeDimension (CMS), or null if not configured (?). */
        public Integer getConfigWidth() {
            return getConfig().getWidth();
        }

        /** Returns the configured preset variant image height, from ImageProperties.xml (file) or ImageSizePreset/ImageSizeDimension (CMS), or null if not configured (?). */
        public Integer getConfigHeight() {
            return getConfig().getHeight();
        }

        public ImageVariantConfig.VariantInfo getConfig() {
            ImageVariantConfig.VariantInfo config = this.config;
            if (config == null) { // NOTE: implementations may also set the config explicitly
                ImageVariantConfig variantConfig = getVariantConfig();
                if (variantConfig != null) {
                    config = variantConfig.getVariant(getName());
                    this.config = config;
                } else {
                    Debug.logError("No VariantInfo defined for variant [" + getName() + "] in variant config ["
                            + variantConfig.getName() + "]; will return null values for this iteration (error or cache?)", module);
                    config = ImageVariantConfig.VariantInfo.NULL;
                    this.config = config;
                }
            }
            return config;
        }

        protected Variant getPrevious() {
            Variant lastVariant = null;
            for(Variant variant : getVariantList()) {
                if (variant == this) {
                    break;
                }
                lastVariant = variant;
            }
            return lastVariant;
        }

        protected Variant getNext() {
            boolean found = false;
            for(Variant variant : getVariantList()) {
                if (found) {
                    return variant;
                }
                if (variant == this) {
                    found = true;
                }
            }
            return null;
        }

        /**
         * Performs a DEEP (file-based) check to see if this variant has a physically available variant source file.
         * IGNORES the cache.properties#product.image.variants.sourceCheck setting. Depends on implementation.
         * NOTE: When sourceCheck=true, this ImageVariant instance will not be available to begin with as
         * it will be omitted from {@link #getVariantMap()}. See also {@link #hasSourcedVariants()}.
         */
        public boolean hasSource() {
            return true;
        }
    }

    /** Abstracted function for FTL templates. See MimeType entity for list of target mimeTypeIds. e.g.: image/jpeg, image/png, image/webp. */
    public abstract Map<String, Object> getResponsiveVariantMap(String targetMimeTypeId, Map<String, ? extends Number> sizeDefs,
                                                                Map<String, Object> context, Map<String, Object> urlArgs);

    protected String appendUrlParams(String url, Map<String, Object> params, String paramDelim) {
        return WebappUrlDirective.appendUrlParams(url, params, paramDelim);
    }

    protected FullWebappInfo getUrlTargetWebapp(Map<String, Object> context, Map<String, Object> args, String webSiteId) {
        if (webSiteId == null) {
            webSiteId = (String) args.get("webSiteId");
        }
        // TODO: REVIEW: may not 100% coincide with RequestHandler.makeLink yet, close enough for now
        if (UtilValidate.isNotEmpty(webSiteId)) {
            HttpServletRequest request = (HttpServletRequest) context.get("request");
            if (request != null) {
                return FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), request);
            } else {
                return FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), context);
            }
        } else {
            HttpServletRequest request = (HttpServletRequest) context.get("request");
            if (request != null) {
                return FullWebappInfo.fromRequest(request);
            } else {
                return FullWebappInfo.fromContext(context);
            }
        }
    }
}
