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

import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.party.contact.*;
import org.ofbiz.product.catalog.*;

final module = "ShipSettings.groovy";

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
partyId = cart.getPartyId();
context.cart = cart;

// nuke the event messages
request.removeAttribute("_EVENT_MESSAGE_");

if (partyId && !partyId.equals("_NA_")) {
    party = from("Party").where("partyId", partyId).queryOne();
    person = party.getRelatedOne("Person", false);
    context.party = party;
    context.person = person;
}

if (cart?.getShippingContactMechId()) {
    shippingContactMechId = cart.getShippingContactMechId();
    shippingPartyContactDetail = from("PartyContactDetailByPurpose").where("partyId", partyId, "contactMechId", shippingContactMechId).filterByDate().queryFirst();
    // SCIPIO: NOTE: Null checks added to all below
    if (!shippingPartyContactDetail) {
        Debug.logError("Scipio: Missing shipping party contact detail for partyId '" + partyId + "' and contactMechId '" + shippingContactMechId + "'", module);
    }
    
    parameters.shippingContactMechId = shippingPartyContactDetail?.contactMechId;
    context.callSubmitForm = true;

    fullAddressBuf = new StringBuffer();
    if (shippingPartyContactDetail) { // SCIPIO: null check
        fullAddressBuf.append(shippingPartyContactDetail.address1);
        if (shippingPartyContactDetail.address2) {
            fullAddressBuf.append(", ");
            fullAddressBuf.append(shippingPartyContactDetail.address2);
        }
        fullAddressBuf.append(", ");
        fullAddressBuf.append(shippingPartyContactDetail.city);
        fullAddressBuf.append(", ");
        fullAddressBuf.append(shippingPartyContactDetail.postalCode);
    }
    parameters.fullAddress = fullAddressBuf.toString();

    // NOTE: these parameters are a special case because they might be filled in by the address lookup service, so if they are there we won't fill in over them...
    if (!parameters.postalCode) {
        parameters.attnName = shippingPartyContactDetail?.attnName;
        parameters.address1 = shippingPartyContactDetail?.address1;
        parameters.address2 = shippingPartyContactDetail?.address2;
        parameters.city = shippingPartyContactDetail?.city;
        parameters.postalCode = shippingPartyContactDetail?.postalCode;
        parameters.stateProvinceGeoId = shippingPartyContactDetail?.stateProvinceGeoId;
        parameters.countryGeoId = shippingPartyContactDetail?.countryGeoId;
        parameters.allowSolicitation = shippingPartyContactDetail?.allowSolicitation;
    }

    parameters.yearsAtAddress = shippingPartyContactDetail?.yearsWithContactMech;
    parameters.monthsAtAddress = shippingPartyContactDetail?.monthsWithContactMech;
} else {
    context.postalFields = UtilHttp.getParameterMap(request);
}
