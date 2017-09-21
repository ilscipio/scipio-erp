/**
 * SCIPIO: setup wizard common actions.
 * uses include guard to run only once.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupWizardCommonActions.groovy";

if (context.setupWizardActionsRun != true) {
    if (context.debugMode == null) {
        context.debugMode = UtilMisc.booleanValueVersatile(parameters.debugMode, false);
    }
    try {
        def setupWorker = SetupWorker.getWorker(request);
        context.setupWorker = setupWorker;
        context.setupStepStates = setupWorker.getStepStatePrimitiveMap();
        
        partyId = setupWorker.getOrgPartyId();
        context.partyId = partyId;
        parameters.partyId = partyId;
        
        def party = null;
        def partyGroup = null;
        if (partyId) {
            party = delegator.findOne("Party", ["partyId": partyId], false);
            partyGroup = delegator.findOne("PartyGroup", ["partyId": partyId], false);
        }
        context.party = party;
        context.partyGroup = partyGroup;
        
        if (context.debugMode) Debug.logInfo("Setup: Step states: " + context.setupStepStates, module);
    } catch(Exception e) {
        Debug.logError("Setup: Error reading setup step information: " + e.getMessage(), module);
        errorMessageList = context.errorMessageList;
        if (errorMessageList == null) {
            errorMessageList = [];
            context.errorMessageList = errorMessageList;
        }
        final errorMsgPrefix = UtilProperties.getMessage("ScipioSetupUiLabels", "SetupError", context.locale);
        errorMessageList.add(errorMsgPrefix + ": " + e.getMessage());
    }
    
    context.defaultCountryGeoId = EntityUtilProperties.getPropertyValue("general", "country.geo.id.default", "USA", delegator);

    context.setupWizardActionsRun = true;
}
