package org.ofbiz.common.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Image specialization of MediaProfile (SCIPIO).
 */
public class ImageProfile extends MediaProfile {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String TYPE = "IMAGE_OBJECT";

    protected volatile ImageVariantConfig variantConfig;

    protected ImageProfile(Delegator delegator, String name, String description, Map<String, Object> properties, ImageVariantConfig variantConfig, Boolean stored) {
        super(delegator, name, description, properties, stored);
        this.variantConfig = variantConfig;
        this.stored = stored;
    }

    public static ImageProfile createImageProfile(Delegator delegator, String name, String description, Map<String, Object> properties, Boolean stored) {
        return new ImageProfile(delegator, name, description, properties, null, stored);
    }

    public static ImageProfile createImageProfile(Delegator delegator, String name, Map<String, Object> properties) {
        return createImageProfile(delegator, name, null, properties, false);
    }

    public static ImageProfile createImageProfileFromPreset(GenericValue imageSizePreset) {
        return new ImageProfile(imageSizePreset.getDelegator(), imageSizePreset.getString("presetId"), imageSizePreset.getString("presetName"),
                imageSizePreset, null, true);
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

    public static Map<String, ImageProfile> getImageProfileMap(Delegator delegator) {
        return getMediaProfileTypeMap(delegator, TYPE);
    }

    static List<ImageProfile> loadStoredImageProfiles(Delegator delegator) {
        List<ImageProfile> imageProfiles = new ArrayList<>();
        List<GenericValue> imageSizePresets = delegator.from("ImageSizePreset").queryListSafe();
        if (UtilValidate.isNotEmpty(imageSizePresets)) {
            for(GenericValue imageSizePreset : imageSizePresets) {
                imageProfiles.add(createImageProfileFromPreset(imageSizePreset));
            }
        }
        return imageProfiles;
    }

    @Override
    public String getType() {
        return TYPE;
    }

    @Override
    public boolean isStored() {
        Boolean stored = this.stored;
        if (stored != null) {
            return stored;
        }
        return (getDelegator().from("ImageSizePreset").where("presetId", getName()).cache().queryCountSafe() > 0);
    }

    public ImageVariantConfig getVariantConfig() {
        ImageVariantConfig variantConfig = this.variantConfig;
        if (variantConfig == null) {
            try {
                ImageProfile effProfile = getResolvedVariantConfigProfile();
                variantConfig = ImageVariantConfig.fromImageSizePreset(getDelegator(), effProfile.getName(), false);
                if (variantConfig == null) {
                    String variantConfigLocation = effProfile.getVariantConfigLocation();
                    if (variantConfigLocation != null) {
                        variantConfig = ImageVariantConfig.fromImagePropertiesXml(variantConfigLocation);
                        if (variantConfig == null) {
                            Debug.logError("Could not find ImageSizePreset with presetId [" + getName() +
                                    "] or ImageVariantConfig for mediaProfile [" + effProfile.getName() + "]", module);
                        }
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, module);
            }
            if (variantConfig == null) {
                variantConfig = ImageVariantConfig.NULL;
            }
            this.variantConfig = variantConfig;
        }
        return variantConfig.isNull() ? null : variantConfig;
    }

    public static ImageVariantConfig getVariantConfig(ImageProfile imageProfile) {
        return (imageProfile != null) ? imageProfile.getVariantConfig() : null;
    }

    public ImageProfile getResolvedVariantConfigProfile() {
        return (ImageProfile) super.getResolvedVariantConfigProfile();
    }

}
