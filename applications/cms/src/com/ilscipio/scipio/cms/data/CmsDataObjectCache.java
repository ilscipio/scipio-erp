package com.ilscipio.scipio.cms.data;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.cache.UtilCache;

public class CmsDataObjectCache<T extends CmsDataObject> {
    public static final int EXPIRATION_TIME_DEFAULT = 30;
    private UtilCache<String, T> cache;

    public CmsDataObjectCache(int expireTime) {
        cache = UtilCache.<String, T> createUtilCache();
        cache.setExpireTime(expireTime * 1000);
    }

    public CmsDataObjectCache() {
        this(getDefaultExpiration());
    }

    public T get(String key) {
        return cache.get(key);
    }

    public void get(String key, T value) {
        cache.put(key, value);
    }

    public void clear() {
        cache.clear();
    }

    public void put(String key, T value) {
        cache.put(key, value);
    }

    public static int getDefaultExpiration() {
        return UtilProperties.getPropertyAsInteger("cms.properties",
                "cache.expiration", EXPIRATION_TIME_DEFAULT);
    }
}
