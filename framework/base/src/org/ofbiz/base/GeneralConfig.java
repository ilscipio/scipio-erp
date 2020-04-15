package org.ofbiz.base;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import java.net.InetAddress;

/**
 * General/base system config - some configurations in general.properties - facade class to help prevent dependency issues (SCIPIO).
 * NOTE: This class does not have access to most applications, intentionally.
 */
public abstract class GeneralConfig {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String instanceId = FlexibleStringExpander.expandString( // SCIPIO: Added simple expansion support + moved from JobManager
            UtilProperties.getPropertyValue("general", "unique.instanceId", "scipio0"),
            UtilMisc.toMap());

    private static final InetAddress inetAddress; // moved from VisitHandler
    static {
        InetAddress tmpAddress = null;
        try {
            tmpAddress = InetAddress.getLocalHost();
        } catch (java.net.UnknownHostException e) {
            Debug.logError("Unable to get server's internet address (system may crash): " + e.toString(), module);
        }
        inetAddress = tmpAddress;
    }

    /**
     * Returns the instanceId configured in general.properties.
     */
    public static String getInstanceId() {
        return instanceId;
    }

    /**
     * Returns the current server's localhost address and hostname.
     */
    public static InetAddress getLocalhostAddress() { return inetAddress; }
}
