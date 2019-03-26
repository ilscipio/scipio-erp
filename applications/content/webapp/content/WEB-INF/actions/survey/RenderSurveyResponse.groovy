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

/**
 * SCIPIO: Renders a SurveyResponse using the template specified by the "surveyTmplLoc" context field
 * or, if surveyTmplLoc is empty, performs the data preparation but without rendering.
 * Based on EditSurveyResponse.groovy.
 */

import org.ofbiz.content.survey.*
import org.ofbiz.base.util.*

final module = "RenderSurveyResponse.groovy";

def surveyTmplLoc = context.surveyTmplLoc;

def surveyResponse = context.surveyResponse;
def surveyResponseId = surveyResponse?.surveyResponseId ?: context.surveyResponseId ?: parameters.surveyResponseId;
if (!surveyResponse) {
    if (surveyResponseId) {
        surveyResponse = from("SurveyResponse").where("surveyResponseId", surveyResponseId).queryOne();
    }
    if (!surveyResponse) {
        Debug.logError("SurveyResponse not found [surveyResponseId=" + surveyResponseId + "]", module);
        return
    }
}
def partyId = surveyResponse.partyId;
context.surveyPartyId = partyId;
def surveyId = surveyResponse.surveyId;
context.surveyId = surveyId;

def surveyString = null;
def surveyWrapper = new SurveyWrapper(delegator, surveyResponseId, partyId, surveyId, null);
surveyWrapper.setEdit(false);
if (surveyTmplLoc) {
    try {
        surveyString = surveyWrapper.render(surveyTmplLoc, context);
        if (!surveyString) {
            Debug.logWarning("SurveyResponse '" + surveyResponseId + "' render produced no output", module);
        }
    } catch(Exception e) {
        Debug.logError("Error rendering SurveyResponse '" + surveyResponseId + "': " + e.toString() + " [surveyResponse=" + surveyResponse +"]", module)
    }
}
context.surveyWrapper = surveyWrapper;
context.surveyString = surveyString;
