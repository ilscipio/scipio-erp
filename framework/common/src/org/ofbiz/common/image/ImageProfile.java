package org.ofbiz.common.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
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

    public static ImageProfile createImageProfileFromPreset(Delegator delegator, String name) {
        try {
            GenericValue imageSizePreset = delegator.findOne("ImageSizePreset", UtilMisc.toMap("presetId", name), false);
            if (imageSizePreset != null) {
                return createImageProfileFromPreset(imageSizePreset);
            }
        } catch (Exception e) {
            Debug.logError(e.toString(), module);
        }
        return null;
    }

    public static ImageProfile createImageProfileFromPreset(GenericValue imageSizePreset) {
        String name = imageSizePreset.getString("presetId"); // NOTE: this is NOT presetName
        String parentProfile = imageSizePreset.getString("parentProfile");
        if (parentProfile == null) {
            parentProfile = "IMAGE_MEDIA";
        }
        return new ImageProfile(imageSizePreset.getDelegator(), name, imageSizePreset.getString("presetName"),
                UtilMisc.toMap("parentProfile", parentProfile,
                        "variantConfigProfile", imageSizePreset.get("variantConfigProfile"),
                        "variantConfigLocation", imageSizePreset.get("variantConfigLocation")), null, true);
    }

    /**
     * Gets a media profile by name or null if not specifically defined, no default fallbacks, as defined in mediaprofiles.properties.
     * NOTE: This overload does not require the type as by convention the profile names imply it ("IMAGE_DEFAULT", "IMAGE_PRODUCT", etc.).
     */
    public static <M extends ImageProfile> M getImageProfile(Delegator delegator, String name, boolean useUtilCache) {
        return getMediaProfile(delegator, name, useUtilCache);
    }

    public static Collection<String> getImageProfileNames(Delegator delegator) {
        return getMediaProfileNames(delegator, TYPE);
    }

    public static List<String> getImageProfileNameList(Delegator delegator) {
        return getMediaProfileNameList(delegator, TYPE);
    }

    public static Map<String, ImageProfile> getImageProfileMap(Delegator delegator) {
        return getMediaProfileMap(delegator, TYPE);
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

    /** Gets the cached variant config associated with the image profile, either ImageProperties.xml or ImageSizePreset. */
    public ImageVariantConfig getVariantConfig() {
        ImageVariantConfig variantConfig = this.variantConfig;
        if (variantConfig == null) {
            variantConfig = readVariantConfig();
            if (variantConfig == null) {
                variantConfig = ImageVariantConfig.NULL;
            }
            this.variantConfig = variantConfig;
        }
        return variantConfig.isNull() ? null : variantConfig;
    }

    /**
     * Reads (non-cached) the variant config associated with the image profile, either ImageProperties.xml or ImageSizePreset.
     * <p>NOTE: Currently this updates the variant config cached on ImageProfife so that backend resize calls automatically
     * update the cache, but this may be changed to another pattern in the future.</p>
     */
    public ImageVariantConfig readVariantConfig() {
        ImageVariantConfig variantConfig = null;
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
            if (variantConfig != null) {
                // SPECIAL: Always update the cache here so that frontend responds whenever this is re-read by a non-caching backend service
                this.variantConfig = variantConfig;
            }
        } catch (Exception e) {
            Debug.logError(e, module);
        }
        return variantConfig;
    }

    public ImageProfile getResolvedVariantConfigProfile() {
        return (ImageProfile) super.getResolvedVariantConfigProfile();
    }

    public static List<ImageProfile> readImageProfilesFromPresets(Delegator delegator) { // FIXME: called circular from MediaProfile; unhardcode with a factory property
        List<ImageProfile> imageProfiles = new ArrayList<>();
        List<GenericValue> imageSizePresets = delegator.from("ImageSizePreset").orderBy("presetId").queryListSafe();
        if (UtilValidate.isNotEmpty(imageSizePresets)) {
            for(GenericValue imageSizePreset : imageSizePresets) {
                imageProfiles.add(createImageProfileFromPreset(imageSizePreset));
            }
        }
        return imageProfiles;
    }

    public static Map<String, ImageProfile> readImageProfileMapFromPresets(Delegator delegator) { // FIXME: called circular from MediaProfile; unhardcode with a factory property
        Map<String, ImageProfile> imageProfiles = new LinkedHashMap<>();
        List<GenericValue> imageSizePresets = delegator.from("ImageSizePreset").orderBy("presetId").queryListSafe();
        if (UtilValidate.isNotEmpty(imageSizePresets)) {
            for(GenericValue imageSizePreset : imageSizePresets) {
                ImageProfile profile = createImageProfileFromPreset(imageSizePreset);
                imageProfiles.put(profile.getName(), profile);
            }
        }
        return imageProfiles;
    }
}
