package com.ilscipio.scipio.ce.webapp.ftl.doc;

import org.ofbiz.base.util.Debug;

/**
 * Message adapter for Ofbiz Debug class.
 * <p>
 * IMPORTANT: Keep separate from the other classes for now.
 */
public class OfbizDebugMsgHandler implements MsgHandler {

    protected final String targetModule;

    public OfbizDebugMsgHandler(String targetModule) {
        this.targetModule = targetModule;
    }

    @Override
    public void logInfo(String msg) {
        Debug.logInfo(msg, targetModule);
    }

    @Override
    public void logError(String msg) {
        Debug.logError(msg, targetModule);
    }

    @Override
    public void logDebug(String msg) {
        if (Debug.verboseOn()) {
            Debug.logVerbose(msg, targetModule);
        }
    }

    @Override
    public void logWarn(String msg) {
        Debug.logWarning(msg, targetModule);
    }

}
