package org.ofbiz.common.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;

/**
 * Represents media profile definitions, as defined in mediaprofiles.properties.
 * <p>TODO: ImageSizePreset is currently directly used as entity storage, but it only makes sense for images,
 *  so may want a dedicated MediaProfile (and even ImageProfile) entity eventually.</p>
 */
public abstract class MediaProfile implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, MediaProfile> NAME_CACHE = UtilCache.createUtilCache("scipio.mediaProfile.name"); // key: name + delegator name
    private static final UtilCache<String, Profiles> STATIC_CACHE = UtilCache.createUtilCache("scipio.mediaProfile.static"); // key: delegator name

    private static class Profiles implements Serializable { // for static properties (never change after load)
        final Map<String, MediaProfile> nameMap;
        final Map<String, Map<String, MediaProfile>> typeMap;

        Profiles(Map<String, MediaProfile> nameMap, Map<String, Map<String, MediaProfile>> typeMap) {
            this.nameMap = Collections.unmodifiableMap(nameMap);
            this.typeMap = Collections.unmodifiableMap(typeMap);
        }
    }

    protected final String name;
    protected final String description;
    protected final String parentProfile;
    protected final String variantConfigProfile;
    protected final String variantConfigLocation;
    protected final Map<String, Object> properties;
    protected volatile Boolean stored; // NOTE: this only reflects load time, may change until cache clear

    protected final String delegatorName;
    protected transient Delegator delegator;
    protected Set<String> ancestorProfiles;

    protected MediaProfile(Delegator delegator, String name, String description, Map<String, Object> properties, Boolean stored) {
        this.name = name;
        this.description = description;
        this.properties = UtilValidate.isNotEmpty(properties) ? Collections.unmodifiableMap(new LinkedHashMap<>(properties)) : Collections.emptyMap();
        this.parentProfile = UtilValidate.nullIfEmpty((String) properties.get("parentProfile"));
        this.variantConfigProfile = UtilValidate.nullIfEmpty((String) properties.get("variantConfigProfile"));
        this.variantConfigLocation = UtilValidate.nullIfEmpty((String) properties.get("variantConfigLocation"));
        this.delegatorName = delegator.getDelegatorName();
        this.delegator = delegator;
        this.stored = stored;
    }

    public static <M extends MediaProfile> M create(Delegator delegator, String name, String type, Map<String, Object> properties) {
        if ("IMAGE_OBJECT".equals(type)) {
            return UtilGenerics.cast(ImageProfile.createImageProfile(delegator, name, properties));
        } else {
            throw new UnsupportedOperationException("Unsupported media profile type: " + type);
        }
    }

    public static <M extends MediaProfile> M create(Delegator delegator, String name, Map<String, Object> properties) {
        return create(delegator, name, (String) properties.get("type"), properties);
    }

    /**
     * Gets a media profile by name or null if not specifically defined, no default fallbacks, as defined in mediaprofiles.properties.
     * <p>NOTE: This overload does not require the type as by convention the profile names imply it ("IMAGE_DEFAULT", "IMAGE_PRODUCT", etc.).</p>
     */
    public static <M extends MediaProfile> M getMediaProfile(Delegator delegator, String name, boolean useUtilCache) {
        if (UtilValidate.isEmpty(name)) {
            return null;
        }
        MediaProfile profile = null;
        String cacheKey = name + "::" + delegator.getDelegatorName();
        if (useUtilCache) {
            profile = NAME_CACHE.get(cacheKey);
            if (profile != null) {
                return UtilGenerics.cast(profile);
            }
        }
        if (name.startsWith("IMAGE_")) {
            profile = ImageProfile.createImageProfileFromPreset(delegator, name);
        }
        if (profile == null) {
            Profiles staticProfiles = getStaticProfiles(delegator);
            profile = staticProfiles.nameMap.get(name);
        }
        if (profile != null) {
            NAME_CACHE.put(cacheKey, profile); // NOTE: Always update the cache
        }
        return UtilGenerics.cast(profile);
    }

    /**
     * Gets a media profile by name and type or null if not specifically defined, no default fallbacks, as defined in mediaprofiles.properties.
     */
    public static <M extends MediaProfile> M getMediaProfile(Delegator delegator, String name, String type, boolean useUtilCache) {
        MediaProfile profile = getMediaProfile(delegator, name, useUtilCache);
        return (profile != null && type != null && type.equals(profile.getType())) ? UtilGenerics.cast(profile) : null;
    }

    public static <M extends MediaProfile> Map<String, M> getMediaProfileMap(Delegator delegator, String type) {
        Profiles profiles = getStaticProfiles(delegator);
        Map<String, MediaProfile> map = (type != null) ? profiles.typeMap.get(type) : profiles.nameMap;
        if (type == null || "IMAGE_OBJECT".equals(type)) {
            map = new TreeMap<>(map);
            map.putAll(ImageProfile.readImageProfileMapFromPresets(delegator));
        }
        if (UtilValidate.isEmpty(map)) {
            return Collections.emptyMap();
        }
        return UtilGenerics.cast(map);
    }

    public static Collection<String> getMediaProfileNames(Delegator delegator, String type) {
        Map<String, MediaProfile> map = getMediaProfileMap(delegator, type);
        return (map != null) ? map.keySet() : Collections.emptySet();
    }

    public static List<String> getMediaProfileNameList(Delegator delegator, String type) {
        return new ArrayList<>(getMediaProfileNames(delegator, type));
    }

    public abstract String getType();

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getParentProfile() {
        return parentProfile;
    }

    public String getVariantConfigProfile() {
        return variantConfigProfile;
    }

    /** NOTE: uncached */
    public MediaProfile getResolvedVariantConfigProfile() {
        String variantConfigProfile = getVariantConfigProfile();
        if (variantConfigProfile != null) {
            MediaProfile nextProfile = MediaProfile.getMediaProfile(getDelegator(), variantConfigProfile, false);
            if (nextProfile == null) {
                Debug.logError("Could not resolve mediaProfile [" + variantConfigProfile + "] referenced from profile [" + getName() + "]", module);
            } else {
                return nextProfile.getResolvedVariantConfigProfile();
            }
        }
        return this;
    }

    /** Returns component:// variant config location for ImageProperties.xml or equivalent, or null if ImageSizePreset or other. */
    public String getVariantConfigLocation() {
        return variantConfigLocation;
    }

    /** Returns component:// variant config location for ImageProperties.xml or equivalent, or null if ImageSizePreset or other. */
    public String getResolvedVariantConfigLocation() {
        return getResolvedVariantConfigProfile().getVariantConfigLocation();
    }

    public Set<String> getAncestorProfiles() {
        Set<String> ancestorProfiles = this.ancestorProfiles;
        if (ancestorProfiles == null) {
            ancestorProfiles = new LinkedHashSet<>();
            ancestorProfiles.add(getName());
            MediaProfile profile = this;
            String profileName;
            while((profileName = profile.getParentProfile()) != null) {
                ancestorProfiles.add(profileName);
                profile = getMediaProfile(getDelegator(), profileName, false);
                if (profile == null) {
                    Debug.logError("Could not find media profile [" + profileName + "]", module);
                    break;
                }
            }
            this.ancestorProfiles = Collections.unmodifiableSet(ancestorProfiles);
        }
        return ancestorProfiles;
    }

    public boolean isProfileOrChild(String name) {
        return getAncestorProfiles().contains(name);
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    protected Delegator getDelegator() {
        Delegator delegator = this.delegator;
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator(this.delegatorName);
            this.delegator = delegator;
        }
        return delegator;
    }

    public boolean isStored() { // may be overridden
        Boolean stored = this.stored;
        return (stored != null) ? stored : false;
    }

    public static void clearCaches(Delegator delegator) {
        if (delegator != null) {
            NAME_CACHE.remove(delegator.getDelegatorName());
        } else {
            NAME_CACHE.clear();
        }
    }

    public static Map<String, Object> clearCaches(ServiceContext ctx) {
        if (Boolean.TRUE.equals(ctx.attr("tenantOnly"))) {
            clearCaches(ctx.delegator());
        } else {
            clearCaches((Delegator) null);
        }
        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.toMap("type", ctx.attr("type"));
                dcc.runDistributedService("cmsDistributedClearMappingCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccess();
    }

    private static Profiles getStaticProfiles(Delegator delegator) {
        Profiles profiles = STATIC_CACHE.get(delegator.getDelegatorName());
        if (profiles == null) {
            profiles = readStaticProfiles(delegator);
            STATIC_CACHE.put(delegator.getDelegatorName(), profiles);
        }
        return profiles;
    }

    private static Profiles readStaticProfiles(Delegator delegator) {
        Map<String, MediaProfile> nameMap = new LinkedHashMap<>();
        Map<String, Map<String, MediaProfile>> typeMap = new LinkedHashMap<>();
        List<MediaProfile> mediaProfiles = new ArrayList<>();
        mediaProfiles.addAll(readMediaProfileMapFromStatic(delegator, UtilProperties.getMergedPropertiesFromAllComponents("mediaprofiles")).values());
        for(MediaProfile mediaProfile : mediaProfiles) {
            nameMap.put(mediaProfile.getName(), mediaProfile);
            Map<String, MediaProfile> typeProfiles = typeMap.get(mediaProfile.getType());
            if (typeProfiles == null) {
                typeProfiles = new TreeMap<>();
                typeMap.put(mediaProfile.getType(), typeProfiles);
            }
            typeProfiles.put(mediaProfile.getName(), mediaProfile);
        }
        for(Map.Entry<String, Map<String, MediaProfile>> entry : typeMap.entrySet()) {
            entry.setValue(Collections.unmodifiableMap(new LinkedHashMap<>(entry.getValue())));
        }
        return new Profiles(nameMap, typeMap);
    }

    private static Map<String, MediaProfile> readMediaProfileMapFromStatic(Delegator delegator, Properties mediaProfilesProps) {
        Map<String, MediaProfile> mpp = new LinkedHashMap<>();
        Map<String, Map<String, Object>> profiles = UtilProperties.extractPropertiesWithPrefixAndId(new LinkedHashMap<>(), mediaProfilesProps, "mediaProfile.");
        for(Map.Entry<String, Map<String, Object>> entry : profiles.entrySet()) {
            String name = entry.getKey();
            String type = (String) entry.getValue().get("type");
            try {
                MediaProfile mediaProfile = create(delegator, name, type, entry.getValue());
                mpp.put(name, mediaProfile);
            } catch(Exception e) {
                Debug.logError("Error initializing mediaprofile.properties profile [" + name + "] definition " + entry.getValue() + ": " + e.toString(), module);
            }
        }
        return mpp;
    }
}
