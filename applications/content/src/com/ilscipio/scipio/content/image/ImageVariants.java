package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.webapp.control.RequestHandler;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;
import java.util.Collection;
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

    protected ImageVariants(Delegator delegator, LocalDispatcher dispatcher, Locale locale, boolean useEntityCache) {
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
            profile = ImageProfile.getImageProfile(getDelegator(), getProfileName());
            this.profile = profile;
        }
        return profile;
    }

    /** Returns the explicit ImageProfile directly associated with the image, or null if none. */
    public ImageProfile getExplicitProfile() {
        ImageProfile explicitProfile = this.explicitProfile;
        if (explicitProfile == null) {
            explicitProfile = ImageProfile.getImageProfile(getDelegator(), getExplicitProfileName());
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

    /** Returns map of image variants by sizeType ("small", "800x600", ...), excluding "original" ({@link #getOriginal()}. */
    public abstract Map<String, ? extends Variant> getVariantMap();

    /** Returns list of image variants, excluding "original" ({@link #getOriginal()}. */
    public abstract List<? extends Variant> getVariantList();


    public Variant getVariant(String name) {
        if ("original".equals(name)) {
            return getOriginal();
        }
        return getVariantMap().get(name);
    }

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

        /** Returns the associated contentId for the image variant or null if not applicable (implementation). */
        public abstract String getContentId();

        /** Returns contentAssocTypeId, productContentTypeId, other or null (implementation). */
        public abstract String getAssocId();

        /** Returns the associated record for the image variant or null if not applicable (implementation). */
        public abstract GenericValue getRecord();

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...).
         * context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).
         */
        public abstract String getImageUrl(Map<String, Object> context, Map<String, Object> args);

        /**
         * Returns the image URL of the original (non-resized) image, from context root (/images/..., /shop/media?contentId=xxxx, ...).
         * context refers to the screen context or a wrapper context and typically should contain request, response and locale.
         * For product images this returns the stored image url plus given params.
         * Param values are url-encoded but the url is not html-encoded (screen concern).
         */
        public String getImageUrl(Map<String, Object> context) {
            return getImageUrl(context, Collections.emptyMap());
        }

        /** Returns the stored image URL of the original (non-resized) image or null if cannot be determined without parameters or context. */
        public abstract String getBaseImageUrl();

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
    }

    protected String makeUrl(String uri, Map<String, Object> context, Map<String, Object> args) {
        // TODO
        throw new UnsupportedOperationException();
    }

    protected String appendUrlParams(String url, Map<String, Object> params) {
        if (UtilValidate.isEmpty(params)) {
            return url;
        }
        // TODO: util
        StringBuilder sb = new StringBuilder(url);
        String delim = url.contains("?") ? "&" : "?";
        for(Map.Entry<String, Object> entry : params.entrySet()) {
            String name = entry.getKey();
            if (entry.getValue() instanceof Collection) {
                for(Object value : UtilGenerics.<Collection<?>>cast(entry.getValue())) {
                    sb.append(delim);
                    sb.append(UtilCodec.getUrlEncoder().encode(name));
                    sb.append('=');
                    if (value != null) {
                        sb.append(UtilCodec.getUrlEncoder().encode(value.toString()));
                    }
                    delim = "&";
                }
            } else {
                Object value = entry.getValue();
                sb.append(delim);
                sb.append(UtilCodec.getUrlEncoder().encode(name));
                sb.append('=');
                if (value != null) {
                    sb.append(UtilCodec.getUrlEncoder().encode(value.toString()));
                }
                delim = "&";
            }
        }
        return sb.toString();
    }
}
