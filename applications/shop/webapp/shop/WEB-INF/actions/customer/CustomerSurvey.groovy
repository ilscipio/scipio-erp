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

partyId = userLogin?.partyId; // SCIPIO: prevent crash on missing userLogin
paramMap = UtilHttp.getParameterMap(request);

productStoreSurveyId = parameters.productStoreSurveyId;

// SCIPIO: 2019-03-06: If surveyWrapper is already set in request attributes, reuse it...
wrapper = context.surveyWrapper;
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

// SCIPIO: 2019-03-06: The event may specify a specific action to take
surveyAction = context.surveyAction;
if (surveyAction == null) {
    surveyAction = request.getAttribute("surveyAction");
}
if (surveyAction) {
    context.surveyAction = surveyAction;
}

surveyAppl = wrapper?.getProductStoreSurveyAppl() ?: // SCIPIO: 2019-03-06: Reuse the record from the survey wrapper
    from("ProductStoreSurveyAppl").where("productStoreSurveyId", productStoreSurveyId).queryOne();
if (surveyAppl) {
    survey = surveyAppl.getRelatedOne("Survey", false);
    context.survey = survey;

    if (!parameters._ERROR_MESSAGE_) {
        paramMap = [productStoreSurveyId : productStoreSurveyId];
    }
    if (wrapper == null) { // SCIPIO: 2019-03-06: Don't recreate unless
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
