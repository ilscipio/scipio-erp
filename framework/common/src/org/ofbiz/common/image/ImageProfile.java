package org.ofbiz.common.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Image specialization of MediaProfile (SCIPIO).
 */
public class ImageProfile extends MediaProfile {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String TYPE = "IMAGE_OBJECT";

    protected volatile ImageVariantConfig variantConfig;

    protected ImageProfile(Delegator delegator, String name, String parentProfile, Map<String, Object> properties, ImageVariantConfig variantConfig) {
        super(delegator, name, parentProfile, properties);
        this.variantConfig = variantConfig;
    }

    public static ImageProfile createImageProfile(Delegator delegator, String name, Map<String, Object> properties) {
        return new ImageProfile(delegator, name, null, properties, null);
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

    public static Collection<String> getImageProfileNames(Delegator delegator) {
        return getMediaProfileNames(delegator, TYPE);
    }

    public static List<String> getImageProfileNameList(Delegator delegator) {
        return getMediaProfileNameList(delegator, TYPE);
    }

    static List<ImageProfile> loadStoredImageProfiles(Delegator delegator) {
        List<ImageProfile> imageProfiles = new ArrayList<>();
        List<GenericValue> imageSizePresets = delegator.from("ImageSizePreset").queryListSafe();
        if (UtilValidate.isNotEmpty(imageSizePresets)) {
            for(GenericValue isp : imageSizePresets) {
                ImageProfile imageProfile = createImageProfile(delegator, isp.getString("presetId"),
                        UtilMisc.toMap("parentProfile", isp.getString("parentProfile")));
                imageProfiles.add(imageProfile);
            }
        }
        return imageProfiles;
    }

    @Override
    public String getType() {
        return TYPE;
    }

    public ImageVariantConfig getVariantConfig() {
        ImageVariantConfig variantConfig = this.variantConfig;
        if (variantConfig == null) {
            try {
                variantConfig = ImageVariantConfig.fromImageSizePreset(getDelegator(), getName(), false);
                if (variantConfig == null) {
                    variantConfig = ImageVariantConfig.fromImagePropertiesXml(getVariantConfigLocation());
                    if (variantConfig == null) {
                        Debug.logError("Could not find ImageSizePreset with presetId [" + getName() +
                                "] or ImageVariantConfig for mediaProfile [" + getName() + "]", module);
                    }
                }
            } catch (Exception e) {
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
