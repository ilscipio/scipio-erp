/**
 * SCIPIO: setup wizard common actions.
 * uses include guard to run only once.
 */

import org.ofbiz.base.util.*;
import com.ilscipio.scipio.setup.*;
final module = "SetupWizardCommonActions.groovy";

if (context.setupWizardActionsRun != true) {
    context.DEBUG = true;
    try {
        setupWorker = SetupWorker.getWorker(request);
        context.setupWorker = setupWorker;
        context.setupStepStates = setupWorker.getStepStatePrimitiveMap();
        if (context.DEBUG) Debug.logInfo("Setup: Step states: " + context.setupStepStates, module);
    } catch(Exception e) {
        Debug.logError("Setup: Error reading setup step information: " + e.getMessage(), module);
        errorMessageList = context.errorMessageList;
        if (errorMessageList == null) {
            errorMessageList = [];
            context.errorMessageList = errorMessageList;
        }
        // TODO: LOCALIZE
        errorMessageList.add("Setup error: " + e.getMessage());
    }
    context.setupWizardActionsRun = true;
}