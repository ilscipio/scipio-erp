package com.ilscipio.scipio.setup;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: Setup/wizard events (new).
 */
public abstract class SetupEvents {

    public static final String module = SetupEvents.class.getName();
    
    protected SetupEvents() {
    }
    
    public static String getSubmittedSetupStep(HttpServletRequest request, HttpServletResponse response) {
        try {
            return SetupWorker.getWorker(request).getSubmittedSetupStep();
        } catch(Exception e) {
            final String enErrMsg = "Error determining submitted setup step";
            Debug.logError(e, "Setup: " + enErrMsg + ": " + e.getMessage(), module);
            request.setAttribute("_ERROR_MESSAGE_", enErrMsg + ": " + e.getMessage());// TODO: localize
            return "error";
        }
    }
    
    public static String setSubmittedSetupStep(HttpServletRequest request, HttpServletResponse response) {
        SetupWorker worker = SetupWorker.getWorker(request);
        
        worker.setEffectiveSetupStep(SetupWorker.ERROR_STEP); // in case fail
        
        String thisRequestUri = (String) request.getAttribute("thisRequestUri");
        if (thisRequestUri == null || !thisRequestUri.startsWith("setup")) {
            Debug.logError("setSubmittedSetupStep: thisRequestUri is not in setup* name format", module);
            request.setAttribute("_ERROR_MESSAGE_", "INTERNAL ERROR: please contact developers"); // shouldn't happen (TODO: localize)
            return "error";
        }
        
        String setupStep = thisRequestUri.substring("setup".length()).toLowerCase();
        if (worker.getAllSetupStepValuesSet().contains(setupStep)) {
            Debug.logError("setSubmittedSetupStep: thisRequestUri setup* name does not designate a valid setup step", module);
            request.setAttribute("_ERROR_MESSAGE_", "INTERNAL ERROR: please contact developers"); // shouldn't happen (TODO: localize)
            return "error";
        }
        
        worker.setEffectiveSetupStep(setupStep);
        return "success";
    }
    
    
    public static String getSetupStep(HttpServletRequest request, HttpServletResponse response) {
        SetupWorker worker = SetupWorker.getWorker(request);
        try {
            // NOTE: this code will throw exception if REQ_SETUP_STEP_ATTR in params is invalid;
            // will not fallback to valid value because that might be even more confusing (e.g. URL
            // doesn't match screen shown) - we could also do a 302 redirect instead (TODO: REVIEW)...
            return worker.getSetupStep(Boolean.TRUE, Boolean.TRUE);
        } catch(Exception e) {
            final String enErrMsg = "Error determining next setup step";
            Debug.logError(e, "Setup: " + enErrMsg + ": " + e.getMessage(), module);
            request.setAttribute("_ERROR_MESSAGE_", enErrMsg + ": " + e.getMessage());// TODO: localize
            return "error";
        }

    }

}
