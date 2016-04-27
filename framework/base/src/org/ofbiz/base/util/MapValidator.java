package org.ofbiz.base.util;

import java.util.Map;

/**
 * Cato: Interface for extra validation code.
 */
public interface MapValidator {
    
    /**
     * Validates a map and for each entry that does not validate, adds a corresponding
     * message in the errorMessages map.
     * 
     * @param map
     * @param errorMessages
     */
    public void validate(Map<String, Object> map, Map<String, String> errorMessages);
}