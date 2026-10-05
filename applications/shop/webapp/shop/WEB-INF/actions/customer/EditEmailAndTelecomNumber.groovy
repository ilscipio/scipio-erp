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

import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.party.contact.ContactHelper;

if (userLogin) {
    party = userLogin.getRelatedOne("Party", false);
    if (party == null) {
        return; // SCIPIO
    }

    contactMech = EntityUtil.getFirst(ContactHelper.getContactMech(party, "PRIMARY_EMAIL", "EMAIL_ADDRESS", false));
    if (contactMech) {
        context.emailContactMechId = contactMech.contactMechId;
        context.emailAddress = contactMech.infoString;
    }

    contactMech = EntityUtil.getFirst(ContactHelper.getContactMech(party, "PRIMARY_PHONE", "TELECOM_NUMBER", false));
    if (contactMech) {
        partyContactMech = from("PartyContactMech").where("partyId", party.partyId, "contactMechId", contactMech.contactMechId).filterByDate().queryFirst();
        if (partyContactMech) {
            telecomNumber = partyContactMech.getRelatedOne("TelecomNumber", false);
            context.phoneContactMechId = telecomNumber.contactMechId;
            context.countryCode = telecomNumber.countryCode;
            context.areaCode = telecomNumber.areaCode;
            context.contactNumber = telecomNumber.contactNumber;
            context.extension = partyContactMech.extension;
        }
    }
}
