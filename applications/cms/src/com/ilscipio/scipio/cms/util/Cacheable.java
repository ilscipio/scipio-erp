package com.ilscipio.scipio.cms.util;


/**
 * Interface for classes whose instances can be identified by unique string in a cache.
 */
public interface Cacheable {
    
    /**
     * Returns a unique string representation of this instance that must be appropriate
     * for use as a cache key (i.e., simple characters if possible).
     * 
     * @return  the cache key string
     */
    String toCacheKey();
    
}
    
