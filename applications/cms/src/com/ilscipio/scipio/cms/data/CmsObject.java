package com.ilscipio.scipio.cms.data;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.cms.CmsException;

/**
 * Base CMS object, generalization of CmsDataObject (originally needed for "objects" that
 * have no actual presence in DB but share same role).
 * <p>
 * This is mainly meant to represent objects or elements that a user could recognize,
 * like a request, view, mapping, etc.
 * It will probably not make much sense to make internal classes such as
 * workers or renderers extend this.
 */
public abstract class CmsObject implements Serializable, Preloadable {

    private static final long serialVersionUID = 8922531894662530516L;
    
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    // Cache flags. Note: In general if one or our methods is called with useCache false, that means don't
    // use any kind of cache. Basically you have a generic "useCache" flag passed around
    // and then you have some specific caches (global object, local object, db) which will be
    // toggled based on that flag and another setting.

    // 2016: this flag is better if it's hardcoded in methods.
    //private static final boolean USE_CACHE_DEFAULT = true;
    
    // 2016: NOTE: Flags below are all TRUE by default - they're negative flags so they
    // will only disable caching but never enable
    // NOTE: the 
    
    static final boolean ALLOW_DB_CACHE = UtilProperties.getPropertyAsBoolean("cms.properties", "cache.db.allow", true);
    static final boolean ALLOW_LOCAL_OBJ_CACHE = UtilProperties.getPropertyAsBoolean("cms.properties", "cache.obj.local.allow", true);
    static final boolean ALLOW_GLOBAL_OBJ_CACHE = UtilProperties.getPropertyAsBoolean("cms.properties", "cache.obj.global.allow", true);

// 2016: currently no occurrences of these. 
//    protected boolean isUseCache() {
//        // Currently, we don't indicate this on the object itself; caller must specify per method call
//        // Could also check object member...
//        return isUseCacheStatic();
//    }
//    
//    protected static boolean isUseCacheStatic() {
//        return USE_CACHE_DEFAULT;
//    }
    
    /**
     * Logic to determine if the entity delegator cache should be enabled, based on passed generic
     * useCache bool.
     */
    protected static boolean isUseDbCacheStatic(boolean useCache) {
        return useCache && ALLOW_DB_CACHE;
    }
    
    protected boolean isUseLocalObjCache(boolean useCache) {
        return useCache && ALLOW_LOCAL_OBJ_CACHE;
    }

    /**
     * Logic to determine if the UtilCache CmsObject cache should be enabled, based on passed generic
     * useCache bool.
     */
    protected static boolean isUseGlobalObjCacheStatic(boolean useCache) {
        return useCache && ALLOW_GLOBAL_OBJ_CACHE;
    }
    
    /**
     * Returns the delegator associated to this instance, or a default one if somehow not available.
     */
    public abstract Delegator getDelegator();
    
    /**
     * 2016: Loads ALL this object's content into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY
     * (EVEN if the instance is not physically made immutable!).
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
    }
    
    /**
     * Throws IllegalStateException if the object is marked immutable.
     * <p>
     * The purpose of this is to guarantee we don't accidentally call methods from the frontend
     * that were not written to be thread-safe. This needs to be explicit to prevent accidental
     * create of extremely difficult to find threading bugs.
     */
    protected void preventIfImmutable() throws IllegalStateException {
        if (this.isImmutable()) {
            throw new IllegalStateException("CMS: Fatal: Immutable CMS object subjected to code"
                    + " involving instance modification or otherwise inappropriate for immutable object"
                    + " (usually this is due to running editing-mode-only methods on a CMS object cached for live render)");
        }
    }
    
    /**
     * Gets a full Map representation of this object.
     * <p>
     * NOTE: should never be called from live renders (various reasons, including terrible performance).
     * <p>
     * NOTE: 2017: We should remove use of this in all cases except AJAX - it is not necessary for 
     * Freemarker and just complicates the screens.
     */
    public Map<String, Object> getDescriptor(Locale locale) {
        preventIfImmutable(); // don't allow in live renders
        
        return new HashMap<>();
    }
    
    public boolean isMajorEntity() {
        return (this instanceof CmsMajorObject);
    }

    public static abstract class ObjectWorker<T extends CmsObject> {
        
        /*
         * Cache operations
         */
        
        public void clearMemoryCaches() {
        }
        
        public void clearEntityCaches(Delegator delegator) throws CmsException {
        }
        
    }

    
}
