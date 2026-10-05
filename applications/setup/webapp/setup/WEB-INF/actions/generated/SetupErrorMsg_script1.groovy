import org.ofbiz.base.util.*;
                    final module = "SetupError.groovy";
                    
                    // Log this because caller might not have (WARN: setupErrorMsg is already localized; oh well)
                    if (context.logSetupErrorMsg != false) {
                        Debug.logError("Setup: Error occurred (displaying): " + (context.setupErrorMsg ?: "(message missing)"), module);
                    }
                    
                    // this is best-effort to get a 
                    context.autoNextSetupStep = null;
                    try {
                        context.autoNextSetupStep = context.setupWorker?.determineStepAuto();
                    } catch(Exception e) {
                        Debug.logError(e, "Setup: Error trying to display error message: " + e.getMessage(), "SetupErrorMsg.groovy");
                    }