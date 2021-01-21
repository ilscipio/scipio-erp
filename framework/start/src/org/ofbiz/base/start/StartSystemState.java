package org.ofbiz.base.start;

/**
 * Start package implementation of org.ofbiz.base.SystemState (SCIPIO).
 */
public class StartSystemState {

    public static boolean isShutdownAfterLoad() {
        return Start.getInstance().getConfig().isShutdownAfterLoad();
    }

}
