package org.ofbiz.base.util.template;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.cache.UtilCache;

import freemarker.template.Configuration;
import freemarker.template.Template;

/**
 * SCIPIO: New FTL cache registry that maps Configurations to UtilCache instances
 * for specific types (location, inline-named, inline-self).
 * <p>
 * The caches registered here must be considered to be safe for use with the
 * <code>FreeMarkerWorker.getTemplate</code> methods.
 * Only "well-known" caches should be published here, and the types map
 * the "main" ones.
 * <p>
 * The purpose is to be able to cache templates in a standard way for
 * any Configuration in effect.
 */
public class FtlCacheRegistry {

    /**
     * The main file location cache used by the configuration.
     */
    public static final String MAIN_LOCATION_CACHE = "main-loc";
    /**
     * The main general name cache used by the configuration. Should accept inline templates
     * with a generated name.
     */
    public static final String MAIN_NAME_CACHE = "main-name";
    /**
     * The main body cached used by the configuration.
     * Designates cache whose key is the template body itself (usually for short templates).
     */
    public static final String MAIN_BODY_CACHE = "main-body";

    private static final FtlCacheRegistry registry = new FtlCacheRegistry();
    
    private final Object syncObj = new Object();
    /**
     * NOTE: the Map is immutable and uses double-locking idiom.
     */
    private Map<CacheKey, UtilCache<String, Template>> caches = Collections.emptyMap();
    
    protected FtlCacheRegistry() {
    }

    public static FtlCacheRegistry getDefault() {
        return registry;
    }
    
    public void registerCache(String cacheType, Configuration config, UtilCache<String, Template> cache) {
        synchronized(syncObj) {
            Map<CacheKey, UtilCache<String, Template>> newCaches = new HashMap<>(this.caches);
            newCaches.put(new CacheKey(cacheType, config), cache);
            this.caches = Collections.unmodifiableMap(newCaches);
        }
    }
    
    public UtilCache<String, Template> getCache(String cacheType, Configuration config) {
        Map<CacheKey, UtilCache<String, Template>> caches = this.caches;
        CacheKey key = new CacheKey(cacheType, config);
        return caches.get(key);
    }
    
    private static class CacheKey {
        private final String cacheType;
        private final Configuration config;
        
        public CacheKey(String cacheType, Configuration config) {
            this.cacheType = cacheType;
            this.config = config;
        }

        // TODO: REVIEW hashCode and equals

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((cacheType == null) ? 0 : cacheType.hashCode());
            result = prime * result + ((config == null) ? 0 : config.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (cacheType == null) {
                if (other.cacheType != null)
                    return false;
            } else if (!cacheType.equals(other.cacheType))
                return false;
            if (config == null) {
                if (other.config != null)
                    return false;
            } else if (!config.equals(other.config))
                return false;
            return true;
        }

    }

}
