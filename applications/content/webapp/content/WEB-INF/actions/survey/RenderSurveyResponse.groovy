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
