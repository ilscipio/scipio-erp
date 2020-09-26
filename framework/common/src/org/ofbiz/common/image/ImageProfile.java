package org.ofbiz.common.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

/**
 * Image specialization of MediaProfile (SCIPIO).
 */
public class ImageProfile extends MediaProfile {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String TYPE = "IMAGE_OBJECT";

    protected volatile ImageVariantConfig variantConfig;

    protected ImageProfile(Delegator delegator, String name, Map<String, Object> properties) {
        super(delegator, name, properties);
    }

    /**
     * Gets a media profile by name or null if not specifically defined, no default fallbacks, as defined in mediaprofiles.properties.
     * NOTE: This overload does not require the type as by convention the profile names imply it ("IMAGE_DEFAULT", "IMAGE_PRODUCT", etc.).
     */
    public static <M extends ImageProfile> M getImageProfile(Delegator delegator, String name) {
        return getMediaProfile(delegator, name, TYPE);
    }

    /**
     * Gets a media profile by name and type or null if not specifically defined, or default profile for the type, as defined in mediaprofiles.properties.
     */
    public static <M extends ImageProfile> M getImageProfileOrDefault(Delegator delegator, String name) {
        return getMediaProfileOrDefault(delegator, name, TYPE);
    }

    /**
     * Gets a default media profile by type, as defined in mediaprofiles.properties.
     */
    public static <M extends ImageProfile> M getDefaultImageProfile(Delegator delegator) {
        return getDefaultMediaProfile(delegator, TYPE);
    }

    public static Collection<String> getImageProfileNames(Delegator delegator, String type) {
        return getMediaProfileNames(delegator, TYPE);
    }

    @Override
    public String getType() {
        return TYPE;
    }

    public ImageVariantConfig getVariantConfig() {
        ImageVariantConfig variantConfig = this.variantConfig;
        if (variantConfig == null) {
            // TODO: ImageSizePreset support here
            try {
                variantConfig = ImageVariantConfig.fromImagePropertiesXml(getLocation());
            } catch (IOException e) {
                Debug.logError(e, module);
            }
            this.variantConfig = variantConfig;
        }
        return variantConfig;
    }

    public static ImageVariantConfig getVariantConfig(ImageProfile imageProfile) {
        return (imageProfile != null) ? imageProfile.getVariantConfig() : null;
    }
}
