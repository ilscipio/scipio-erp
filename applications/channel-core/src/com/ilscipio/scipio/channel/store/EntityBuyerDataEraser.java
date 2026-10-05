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
package com.ilscipio.scipio.channel.store;

import java.util.LinkedHashSet;
import java.util.Set;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.channel.core.RetentionRun;

/**
 * Erases the buyer data of one channel order (W1-08 decision 2). The order, its items, amounts and adjustments stay.
 *
 * <p>Steps: (1) the postal addresses of the order lose name, street lines, city, postal code and phone; the country and the
 * state stay because tax needs the place of supply. (2) The order notes lose their text. (3) The e-mail and phone of the buyer party are cleared. (4) The buyer party (one party
 * for each channel order, see EntityOrderCreator) is anonymized with the service anonymizePartyPersonalData of the compliance
 * component. When that service is not loaded, the erase throws and the order stays due. Its contact data that an invoice needs stays for the tax retention period.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08). Not covered by a unit test: it needs a running store database.</p>
 */
public final class EntityBuyerDataEraser implements RetentionRun.BuyerDataEraser {
    private static final String[] PII_FIELDS = {"toName", "attnName", "address1", "address2", "address3", "city", "postalCode",
            "postalCodeExt", "directions"};

    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final GenericValue userLogin;

    public EntityBuyerDataEraser(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin) {
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.userLogin = userLogin;
    }

    @Override
    public void erase(String orderId) throws GenericEntityException, GenericServiceException {
        Set<String> contactMechIds = new LinkedHashSet<>();
        for (GenericValue v : EntityQuery.use(delegator).from("OrderContactMech").where("orderId", orderId).queryList()) {
            contactMechIds.add(v.getString("contactMechId"));
        }
        for (GenericValue v : EntityQuery.use(delegator).from("OrderItemShipGroup").where("orderId", orderId).queryList()) {
            if (v.getString("contactMechId") != null) {
                contactMechIds.add(v.getString("contactMechId"));
            }
        }
        for (String cmId : contactMechIds) {
            GenericValue address = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", cmId).queryOne();
            if (address == null) {
                continue;
            }
            for (String f : PII_FIELDS) {
                address.set(f, null);
            }
            address.set("address1", "[erased]");
            address.store();
        }

        // notes of the order (the buyer note, notes from the channel)
        for (GenericValue link : EntityQuery.use(delegator).from("OrderHeaderNote").where("orderId", orderId).queryList()) {
            GenericValue note = EntityQuery.use(delegator).from("NoteData").where("noteId", link.getString("noteId")).queryOne();
            if (note != null) {
                note.set("noteInfo", "[erased]");
                note.set("noteName", null);
                note.store();
            }
        }

        GenericValue role = EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId, "roleTypeId", "PLACING_CUSTOMER")
                .queryFirst();
        if (role == null) {
            role = EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId, "roleTypeId", "BILL_TO_CUSTOMER").queryFirst();
        }
        if (role == null) {
            return;
        }
        String partyId = role.getString("partyId");
        // e-mail and phone of the buyer party (one party for each channel order)
        for (GenericValue pcm : EntityQuery.use(delegator).from("PartyContactMech").where("partyId", partyId).queryList()) {
            GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", pcm.getString("contactMechId")).queryOne();
            if (cm != null && ("EMAIL_ADDRESS".equals(cm.getString("contactMechTypeId")))) {
                cm.set("infoString", "[erased]");
                cm.store();
            }
            if (cm != null && "TELECOM_NUMBER".equals(cm.getString("contactMechTypeId"))) {
                GenericValue tn = EntityQuery.use(delegator).from("TelecomNumber").where("contactMechId", cm.getString("contactMechId")).queryOne();
                if (tn != null) {
                    tn.set("contactNumber", "[erased]");
                    tn.set("areaCode", null);
                    tn.set("countryCode", null);
                    tn.store();
                }
            }
        }
        // the party itself: name and the rest. Without this service the erase is not complete: throw, so the order stays due.
        if (!serviceExists("anonymizePartyPersonalData")) {
            throw new GenericServiceException("The service anonymizePartyPersonalData is not loaded (compliance component). "
                    + "The buyer data of order " + orderId + " is not erased.");
        }
        java.util.Map<String, Object> res = dispatcher.runSync("anonymizePartyPersonalData",
                UtilMisc.<String, Object>toMap("partyId", partyId, "userLogin", userLogin));
        if (ServiceUtil.isError(res)) {
            throw new GenericServiceException(ServiceUtil.getErrorMessage(res));
        }
    }

    private boolean serviceExists(String name) {
        try {
            return dispatcher.getDispatchContext().getModelService(name) != null;
        } catch (GenericServiceException e) {
            return false;
        }
    }
}
