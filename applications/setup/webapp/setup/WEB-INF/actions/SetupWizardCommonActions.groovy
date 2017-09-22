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
    // compat
    context.partyId = null
    
    try {
        def setupStep = context.setupStep;
        
        def setupWorker = SetupWorker.getWorker(request);
        context.setupWorker = setupWorker;
        context.setupStepStates = setupWorker.getStepStatePrimitiveMap();
        
        def partyId = null;
        if (setupWorker.isValidStepOrFinished(setupStep) ) {
            partyId = setupWorker.getOrgPartyId();
        }
        context.partyId = partyId;
        parameters.partyId = partyId;
        // ALSO SET orgParty for convenience/standard
        context.orgPartyId = partyId;
        parameters.orgPartyId = partyId;
        
        def party = null;
        def partyGroup = null;
        if (partyId) {
            // TODO? could store in worker
            party = delegator.findOne("Party", ["partyId": partyId], false);
            partyGroup = delegator.findOne("PartyGroup", ["partyId": partyId], false);
        }
        context.party = party;
        context.partyGroup = partyGroup;
        // ALSO SET orgParty for convenience/standard
        context.orgParty = party;
        context.orgPartyGroup = partyGroup;
        
        def productStoreId = null;
        def productStore = null;
        if (setupWorker.isValidStep(setupStep) && setupWorker.getStepState(setupStep).getStepParamInfo().getSupported().contains("productStoreId")) {
            productStoreId = setupWorker.getProductStoreId();
            productStore = setupWorker.getProductStore();
        }
        context.productStoreId = productStoreId;
        parameters.productStoreId = productStoreId;
        context.productStore = productStore;
        
        if (context.debugMode) Debug.logInfo("Setup: Step states: " + context.setupStepStates, module);
    } catch(Exception e) {
        Debug.logError(e, "Setup: Error reading setup step information: " + e.getMessage(), module);
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
