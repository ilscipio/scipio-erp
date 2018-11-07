package org.ofbiz.common.qrcode;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.codec.digest.DigestUtils;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;

/**
 * SCIPIO: Maps random generated IDs to qrcode component:// filenames.
 * <p>
 * TODO: refactor reusable parts of the cache into a util.
 */
public final class QRCodeLogoRegistry {

    // Auto-generated ID cache
    private static final UtilCache<String, String> autoLocationToIdCache = UtilCache.createUtilCache("qrcode.logo.locationToId");
    private static final UtilCache<String, String> autoIdToLocationCache = UtilCache.createUtilCache("qrcode.logo.idToLocation");

    // Manual ID cache
    private static final Map<String, String> manualLocationToIdCache;
    private static final Map<String, String> manualIdToLocationCache;
    static {
        Map<String, String> map = UtilProperties.getPropertiesWithPrefix(UtilProperties.getProperties("qrcode"), "qrcode.logoimage.");
        map.putAll(UtilProperties.getPropertiesWithPrefix(UtilProperties.getMergedPropertiesFromAllComponents("qrcode-custom"), "qrcode.logoimage."));
        manualIdToLocationCache = map;
        Map<String, String> reverseMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            reverseMap.put(entry.getValue(), entry.getKey());
        }
        manualLocationToIdCache = reverseMap;
    }

    private QRCodeLogoRegistry() {
    }

    /**
     * Returns the public ID for the given logo file location; if none exists in system, this registers it.
     * <p>
     * This is called automatically by the <code>@qrcode</code> macro.
     * <p>
     * NOTE: You can also call this at any time in a store in server-side code to ensure location is available in
     * case you are not using <code>@qrcode</code> macro.
     */
    public static String getIdForLocation(Delegator delegator, String location) {
        String id = manualLocationToIdCache.get(location);
        if (id == null) {
            id = autoLocationToIdCache.get(location);
            if (id == null) {
                synchronized(QRCodeLogoRegistry.class) {
                    id = autoLocationToIdCache.get(location);
                    if (id == null) {
                        id = registerIdForLocation(delegator, location, generateId(location));
                    }
                }
            }
        }
        return id;
    }

    private static String generateId(String location) {
        // TODO: REVIEW: using predictable hash for now in case of server up/down/balancing issues
        return DigestUtils.sha1Hex(location);
    }

    /**
     * The the given public ID for the given logo file location.
     * <p>
     * Generally this is only needed in special case where you need to access qrcode request WITHOUT <code>@qrcode</code> macro.
     * <p>
     * Generally, this should only be used with a fixed id string.
     */
    public static String registerIdForLocation(Delegator delegator, String location, String id) {
        if (id == null) {
            throw new NullPointerException();
        }
        synchronized(QRCodeLogoRegistry.class) {
            autoLocationToIdCache.put(location, id);
            autoIdToLocationCache.put(id, location);
        }
        return id;
    }

    public static String getLocationForId(Delegator delegator, String id) {
        String location = manualIdToLocationCache.get(id);
        if (location == null) {
            location = autoIdToLocationCache.get(id);
        }
        return location;
    }
}
