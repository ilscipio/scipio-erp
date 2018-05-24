package com.ilscipio.scipio.ce.webapp.ftl.template.standard;

import java.util.List;
import java.util.Map;

/**
 * SCIPIO: Standard template markup FTL utils.
 * <p>
 * These are utilities used to assist the markup implementations found under:
 * <code>component://framework/common/webcommon/includes/scipio/lib/standard</code>
 * <p>
 * These are theme-, styling-framework- and platform-aware.
 * <p>
 * DEV NOTE: these could be further divided but I doubt there will be many.
 */
public abstract class StdTemplateFtlUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected StdTemplateFtlUtil() {
    }
    
    
    /**
     * Heuristically calculates container grid size factors from lists of parent container sizes.
     * <p>
     * TODO: Not implemented
     */
    public static Map<String, Object> evalAbsContainerSizeFactors(List<Map<String, Object>> sizesList, Object maxSizes,
            List<Map<String, Object>> cachedFactorsList) {
        // TODO
        throw new UnsupportedOperationException("Not implemented");
        /*
        Map<String, Object> res = new HashMap<String, Object>();
        res.put("large", 1F);
        res.put("medium", 1F);
        res.put("small", 1F);
        return res; */
    }
    
}
