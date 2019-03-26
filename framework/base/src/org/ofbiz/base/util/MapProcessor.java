package org.ofbiz.base.util;

import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Basic interface for a map processor.
 * <p>
 * Generalization of the logical minilang SimpleMapProcessor interface; processors may be written in any language.
 * <p>
 * This goes further and supports a map of lists to associate error messages with param names in addition to
 * non-specific error message lists.
 */
public interface MapProcessor {

    /**
     * Validates a map and for each entry that does not validate, adds a corresponding
     * message in the entryErrorMessages/generalErrorMessages map for the field.
     * <p>
     * May associate errors with fields using entryErrorMessages or (if not supported) specify generic errorMessages list.
     */
    public void process(Map<String, Object> inMap, Map<String, Object> results, Map<String, List<String>> entryErrorMessages,
            List<String> generalErrorMessages, Locale locale) throws GeneralException;

}