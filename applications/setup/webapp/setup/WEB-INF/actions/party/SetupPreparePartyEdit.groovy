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
 * SCIPIO: Special best-effort heuristics to get editcontactmech (and others) 
 * to display in setup wizard, based on parameters.partyId and session scpSetupOrgPartyId.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupPreparePartyEdit.groovy";

orgPartyId = parameters.orgPartyId ?: session.getAttribute("scpSetupOrgPartyId");
partyId = parameters.partyId;

setupStep = null;
if (partyId) {
    if (partyId == orgPartyId) {
        Debug.logInfo("Setup: Editing info for last-viewed organization '" + orgPartyId + "'", module);
        setupStep = "organization";
    } else {
        partyIsOrg = from("PartyRole").where("partyId", partyId, "roleTypeId", "INTERNAL_ORGANIZATIO").queryOne();
        if (partyIsOrg) {
            Debug.logInfo("Setup: Editing info for organization '" + partyId +
                "'; is not last-viewed organization", module);
            // TODO: REVIEW: partyIsOrg is special case to make up for session/tab issues...
            // But this may cause issues if trying to add a user that is also an INTERNAL_ORGANIZATIO?
            // In that case, we'd have to comment partyIsOrg out here or add extra check for a relation...
            setupStep = "organization";
            orgPartyId = partyId;
        } else {
            setupStep = "user";
            // There should be a relation to the org from session...
            // (NOTE: not constraining to roleTypeIdFrom INTERNAL_ORGANIZATIO here, this is just for display, can be lenient...)
            if (from("PartyRelationship").where("partyIdFrom", orgPartyId, "partyIdTo", partyId).filterByDate().queryList()) {
                Debug.logInfo("Setup: Editing info for user '" + partyId +
                    "' directly related to last-viewed organization (" + orgPartyId + ")", module);
            } else {
                orgRel = SetupDataUtil.findBestDisplayOrganizationRelationForParty(delegator, partyId, false);
                if (orgRel) {
                    orgPartyId = orgRel.partyIdFrom;
                    Debug.logInfo("Setup: Editing info for user '" + partyId +
                        "' not related to last-viewed organization (" + orgPartyId +
                        "); showing its related organization '" + orgPartyId +
                        "' instead (roleTypeIdFrom: " + orgRel.roleTypeIdFrom + ")", module);
                } else {
                    // in this case, we can't show an org... will be ugly and miss info, but functional
                    orgPartyId = null;
                    Debug.logWarning("Setup: Editing info for user '" + partyId +
                            "' not related to last-viewed organization (" + orgPartyId +
                            "); cannot find any related organization; display may be incomplete", module);
                }
            }
            // Set userPartyId so user step menu item links back to us
            request.setAttribute("userPartyId", partyId);
            parameters.userPartyId = partyId;
            context.userPartyId = partyId;
        }
    }
    // Set the organization in request as if it was set over req params... SetupWorker will see req attribs...
    if (orgPartyId) {
        request.setAttribute("orgPartyId", orgPartyId);
        parameters.orgPartyId = orgPartyId;
        context.orgPartyId = orgPartyId;
    }
}
context.setupStep = setupStep;
