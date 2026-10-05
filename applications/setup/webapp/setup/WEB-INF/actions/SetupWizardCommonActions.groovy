/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
    
    GroovyUtil.runScriptAtLocation("component://setup/webapp/setup/WEB-INF/actions/SetupWizardStaticProperties.groovy", null, context);
    
    def setupStepList = SetupWorker.getStepsStatic(); // (excludes "finished")

    def setupStepDisabledMap = [:]; // special map for Menus.xml, true defaults needed in case crash
    for(step in setupStepList) {
        setupStepDisabledMap[step] = true;
    }
    
    // variables for context
    def setupStep = context.setupStep;
    SetupWorker setupWorker = null;
    
    def setupStepStates = null;
    def setupStepState = null;
    def setupStepData = null;
    
    def partyId = null;
    def party = null;
    def partyGroup = null;
    
    def productStoreId = null;
    def productStore = null;
    
    def setupStepSkippable = false;
    def nextSetupStep = null;
    def nextAvailSetupStep = null;
    def isSetupEventError = false;
    
    try {
        setupWorker = SetupWorker.getWorker(request);
        setupStepStates = setupWorker.getStepStatePrimitiveMap();
        
        setupStepState = setupWorker.getStepState(setupStep);
        setupStepData = setupWorker.getStepState(setupStep)?.getStepData();
        
        for(name in setupStepDisabledMap.keySet()) {
            disabled = setupStepStates[name]?.disabled;
            if (disabled != null) {
                setupStepDisabledMap[name] = disabled;
            }
        }
        
        if (setupWorker.isValidStepOrFinished(setupStep) ) {
            partyId = setupWorker.getOrgPartyId();
        }

        if (partyId) {
            party = setupWorker.getOrgParty();
            partyGroup = setupWorker.getOrgPartyGroup();
        }

        if (SetupWorker.FINISHED_STEP.equals(setupStep) || setupStepState?.getStepParamInfo().getSupported().contains("productStoreId")) {
            productStoreId = setupWorker.getProductStoreId();
            productStore = setupWorker.getProductStore();
        }

        if (setupWorker.isValidStep(setupStep)) {
            nextSetupStep = setupWorker.getStepAfter(setupStep);
            nextAvailSetupStep = setupStepState.getNextAccessibleStep();
            setupStepSkippable = setupStepState.isSkippableEffective();
        }
        
        isSetupEventError = setupWorker.isSetupEventError();
        
        if (context.debugMode) Debug.logInfo("Setup: Step states: " + context.setupStepStates, module);
    } catch(Exception e) {
        Debug.logError(e, "Setup: Error reading setup step information: " + e.getMessage(), module);
        errorMessageList = context.errorMessageList;
        if (errorMessageList == null) {
            errorMessageList = [];
            context.errorMessageList = errorMessageList;
        }
        final errorMsgPrefix = UtilProperties.getMessage("ScipioSetupErrorUiLabels", "SetupError", context.locale);
        errorMessageList.add(errorMsgPrefix + ": " + e.getMessage());
    }
    
    // assign all after try block so context/parameters clean even if fail
    
    context.setupStepList = setupStepList;
    context.setupStepDisabledMap = setupStepDisabledMap;
    
    context.setupWorker = setupWorker;
    context.setupStepStates = setupStepStates;
    //context.setupStepData = setupStepData;
    
    // set both partyId and orgPartyId to same (aliases)
    context.partyId = partyId;
    parameters.partyId = partyId;
    context.orgPartyId = partyId;
    parameters.orgPartyId = partyId;
    
    context.party = party;
    context.partyGroup = partyGroup;
    context.orgParty = party;
    context.orgPartyGroup = partyGroup;
    
    context.productStoreId = productStoreId;
    parameters.productStoreId = productStoreId;
    context.productStore = productStore;

    context.defaultWebSiteId = setupStepStates?.store?.stepData?.defaultWebSite?.webSiteId; // may be null

    context.setupStepSkippable = setupStepSkippable;
    context.nextSetupStep = nextSetupStep;
    context.nextAvailSetupStep = nextAvailSetupStep;
    
    context.defaultCountryGeoId = EntityUtilProperties.getPropertyValue("general", "country.geo.id.default", "USA", delegator);
    context.defaultSystemCurrencyUomId = EntityUtilProperties.getPropertyValue("general", "currency.uom.id.default", "USD", delegator);
    
    context.isSetupEventError = isSetupEventError;
    
    context.setupWizardActionsRun = true;
}
