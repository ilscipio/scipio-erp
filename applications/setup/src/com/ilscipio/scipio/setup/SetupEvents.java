package com.ilscipio.scipio.setup;

import java.util.Collection;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;

/**
 * SCIPIO: Setup/wizard events (new).
 */
public abstract class SetupEvents {

    public static final String module = SetupEvents.class.getName();
    
    protected SetupEvents() {
    }
    
    /**
     * Returns the next setup step AND clears the cached setup state/worker in request attributes.
     */
    public static String getNextSetupStep(HttpServletRequest request, HttpServletResponse response) {
        savePreviousEventErrors(request, response); // SPECIAL: needed for worker to know if event success or fail
        
        SetupWorker worker = null;
        try {
            SetupWorker.clearCached(request); // in case previous events cached stuff for a different step
            worker = SetupWorker.getWorker(request);
            // NOTE: this code will throw exception if REQ_SETUP_STEP_ATTR in params is invalid;
            // will not fallback to valid value in that case because that might be even more confusing (e.g. URL
            // doesn't match screen shown) - but we could also do a 302 redirect instead (TODO: REVIEW)...
            return worker.determineStepAndUpdate();
        } catch(Exception e) {
            final String enErrMsg = "Error determining next setup step";
            Debug.logError(e, "Setup: " + enErrMsg + ": " + e.getMessage(), module);
            request.setAttribute("_ERROR_MESSAGE_", enErrMsg + ": " + e.getMessage());// TODO: localize
            return "error";
        }
    }
    
    public static String getSubmittedSetupStep(HttpServletRequest request, HttpServletResponse response) {
        try {
            return SetupWorker.getWorker(request).getSubmittedStep();
        } catch(Exception e) {
            final String enErrMsg = "Error determining submitted setup step";
            Debug.logError(e, "Setup: " + enErrMsg + ": " + e.getMessage(), module);
            request.setAttribute("_ERROR_MESSAGE_", enErrMsg + ": " + e.getMessage());// TODO: localize
            return "error";
        }
    }
    
    /**
     * This uses a hack to force the effective step to the name of the request uri, in the form:
     * setup[stepname] which gets lowercased.
     */
    public static String setEffectiveSetupStep(HttpServletRequest request, HttpServletResponse response) {
        savePreviousEventErrors(request, response); // SPECIAL: needed for worker to know if event success or fail
        
        SetupWorker worker = SetupWorker.getWorker(request);
        
        worker.setEffectiveStep(SetupWorker.ERROR_STEP); // in case fail
        
        String thisRequestUri = (String) request.getAttribute("thisRequestUri");
        if (thisRequestUri == null || !thisRequestUri.startsWith("setup")) {
            Debug.logError("Setup: setSubmittedSetupStep: controller error: thisRequestUri is not in \"setup[Step]\" name format"
                    + " (valid steps: " + worker.getSteps() + ")", module);
            request.setAttribute("_ERROR_MESSAGE_", "INTERNAL ERROR: please contact developers"); // shouldn't happen (TODO: localize)
            return "error";
        }
        
        String setupStep = thisRequestUri.substring("setup".length()).toLowerCase();
        if (!worker.getAllStepValues().contains(setupStep)) {
            Debug.logError("Setup: setSubmittedSetupStep: controller error: thisRequestUri \"setup[Step]\" name does not designate a valid setup step "
                    + " (valid steps: " + worker.getSteps() + ")", module);
            request.setAttribute("_ERROR_MESSAGE_", "INTERNAL ERROR: please contact developers"); // shouldn't happen (TODO: localize)
            return "error";
        }
        
        worker.setEffectiveStep(setupStep);
        return "success";
    }
    
    /**
     * Saves the previous event errors in special request attributes, because they get removed
     * and this messes up the SetupWorker.
     */
    public static String savePreviousEventErrors(HttpServletRequest request, HttpServletResponse response) {
        request.setAttribute("_SETUP_ERROR_MESSAGE_LIST_", request.getAttribute("_ERROR_MESSAGE_LIST_"));
        request.setAttribute("_SETUP_ERROR_MESSAGE_", request.getAttribute("_ERROR_MESSAGE_"));
        return "success";
    }
    
    public static boolean isPreviousEventSavedError(Map<String, Object> params) {
        Object errListObj = params.get("_SETUP_ERROR_MESSAGE_LIST_");
        return (errListObj instanceof Collection && UtilValidate.isNotEmpty(UtilGenerics.<String>checkCollection(errListObj))) ||
                UtilValidate.isNotEmpty((String)params.get("_SETUP_ERROR_MESSAGE_"));
    }
    
}
