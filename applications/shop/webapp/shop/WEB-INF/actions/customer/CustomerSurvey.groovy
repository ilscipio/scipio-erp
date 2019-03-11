/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreSurveyWrapper;

final module = "CustomerSurvey.groovy";

def getParamSafe(name) { // SCIPIO
    def value = context[name];
    if (value != null) {
        return value;
    }
    value = request.getAttribute(name);
    if (value != null) {
        return value;
    }
    if ("POST" == context.requestMethod) {
        // SCIPIO: TODO: VERIFY: security: We need to get surveyAction/others from params, but for security reasons,
        // allow this param only if it's a POST. However, this could cause problems if the survey screen
        // is invoked through a "view-last" (will not be "post" in that case); for that case, we currently
        // count of the behavior where the parameters will actually have been dumped as request attributes,
        // so will already have been caught by request.getAttribute(name)...
        value = parameters[name];
        if (value != null) {
            return value;
        }
    }
    return null;
}

partyId = userLogin?.partyId; // SCIPIO: prevent crash on missing userLogin
paramMap = UtilHttp.getParameterMap(request);

productStoreSurveyId = parameters.productStoreSurveyId;

/* SCIPIO: 2019-03-11: Don't do this for now, re-load the wrapper, because it may be modified below (thread safety concern)
//SCIPIO: If surveyWrapper is already set in request attributes, reuse it...
def wrapper = context.surveyWrapper;
if (wrapper == null) {
    wrapper = request.getAttribute("surveyWrapper");
}
if (!(wrapper instanceof ProductStoreSurveyWrapper)) { // SCIPIO: kust in case
    // TODO?: Can remove this log line later when sure no problems from this...
    Debug.logInfo("Note: Received non-ProductStoreSurveyWrapper surveyWrapper from context/request; discarding", module);
    wrapper = null;
}
if (wrapper != null) {
    productStoreSurveyId = wrapper?.getProductStoreSurveyAppl()?.productStoreSurveyId;
}
*/
def wrapper = null;

// SCIPIO: 2019-03-06: The event may specify a specific action to take
surveyAction = getParamSafe("surveyAction")?.toString();
if (surveyAction) {
    context.surveyAction = surveyAction;
}

surveyAppl = wrapper?.getProductStoreSurveyAppl() ?: // SCIPIO: 2019-03-06: Reuse the record from the survey wrapper, if set
    from("ProductStoreSurveyAppl").where("productStoreSurveyId", productStoreSurveyId).queryOne();
if (surveyAppl) {
    survey = surveyAppl.getRelatedOne("Survey", false);
    context.survey = survey;

    if (wrapper == null) { // SCIPIO: 2019-03-06: Don't recreate unless
        // SCIPIO: 2019-03-11: This _ERROR_MESSAGE_ is both undesirable and never worked in the first place (always empty in screens).
        //if (!parameters._ERROR_MESSAGE_) {
        def origParamMapId = getParamSafe("_ORIG_PARAM_MAP_ID_")?.toString();
        if (origParamMapId) {
            paramMap = [productStoreSurveyId: productStoreSurveyId, _ORIG_PARAM_MAP_ID_: origParamMapId];
        } else {
            paramMap = [productStoreSurveyId: productStoreSurveyId];
        }
        //}
        wrapper = new ProductStoreSurveyWrapper(surveyAppl, partyId, paramMap);
        if (Debug.infoOn()) {
            Debug.logInfo("Creating ProductStoreSurveyWrapper for productStoreSurveyId '" + productStoreSurveyId + "'", module);
        }
    } else {
        if (Debug.infoOn()) {
            Debug.logInfo("Reusing ProductStoreSurveyWrapper for productStoreSurveyId '" + productStoreSurveyId + "'", module);
        }
    }
    context.surveyWrapper = wrapper;

    surveyResp = parameters.surveyResponseId;
    if (surveyResp) {
        wrapper.setThisResponseId(surveyResp);
        wrapper.callResult(true);
    }
}

if (Debug.verboseOn()) { // SCIPIO
    Debug.logVerbose("CustomerSurvey info: [productStoreSurveyId=" + productStoreSurveyId
        + ", parameters.surveyResponseId=" + parameters.surveyResponseId
        + ", surveyAction=" + surveyAction
        + ", surveyAppl=" + surveyAppl + "]", module);
}
