package com.ilscipio.cato.ce.webapp.ftl.template.standard;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Cato: Standard template markup FTL utils.
 * <p>
 * These are utilities used to assist the markup implementations found under:
 * <code>component://framework/common/webcommon/includes/cato/lib/standard</code>
 * <p>
 * These are theme-, styling-framework- and platform-aware.
 * <p>
 * DEV NOTE: these could be further divided but I doubt there will be many.
 */
public abstract class StdTemplateFtlUtil {

    public static final String module = StdTemplateFtlUtil.class.getName();
    
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
