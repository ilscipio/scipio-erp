package org.ofbiz.base;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.string.FlexibleStringExpander;

/**
 * General/base system config - some configurations in general.properties - facade class to help prevent dependency issues (SCIPIO).
 * NOTE: This class does not have access to most applications, intentionally.
 */
public abstract class GeneralConfig {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String instanceId = FlexibleStringExpander.expandString( // SCIPIO: Added simple expansion support + moved from JobManager
            UtilProperties.getPropertyValue("general", "unique.instanceId", "scipio0"),
            UtilMisc.toMap());

    /**
     * Returns the instanceId configured in general.properties.
     */
    public static String getInstanceId() {
        return instanceId;
    }

}
