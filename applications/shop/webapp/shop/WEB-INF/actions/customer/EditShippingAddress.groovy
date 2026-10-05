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
import org.ofbiz.order.shoppingcart.*;

if (userLogin) {
    party = userLogin.getRelatedOne("Party", false);
    if (party == null) {
        return; // SCIPIO
    }
    context.partyId = party.partyId
    if ("PERSON".equals(party.partyTypeId)) {
        person = from("Person").where("partyId", party.partyId).queryOne();
        context.firstName = person.firstName;
        context.lastName = person.lastName;
    } else {
        group = from("PartyGroup").where("partyId", party.partyId).queryOne();
        context.firstName = group.groupName;
        context.lastName = "";    
    }

    contactMech = EntityUtil.getFirst(ContactHelper.getContactMech(party, "SHIPPING_LOCATION", "POSTAL_ADDRESS", false));
    if (contactMech) {
        postalAddress = contactMech.getRelatedOne("PostalAddress", false);
        context.shipToContactMechId = postalAddress.contactMechId;

        context.shipToName = postalAddress.toName;
        context.shipToAttnName = postalAddress.attnName;
        context.shipToAddress1 = postalAddress.address1;
        context.shipToAddress2 = postalAddress.address2;
        context.shipToCity = postalAddress.city;
        context.shipToPostalCode = postalAddress.postalCode;
        context.shipToStateProvinceGeoId = postalAddress.stateProvinceGeoId;
        context.shipToCountryGeoId = postalAddress.countryGeoId;
        shipToStateProvinceGeo = from("Geo").where("geoId", postalAddress.stateProvinceGeoId).queryOne();
        if (shipToStateProvinceGeo) {
            context.shipToStateProvinceGeo =  shipToStateProvinceGeo.geoName;
        }
        shipToCountryProvinceGeo = from("Geo").where("geoId", postalAddress.countryGeoId).queryOne();
        if (shipToCountryProvinceGeo) {
            context.shipToCountryProvinceGeo =  shipToCountryProvinceGeo.geoName;
        }
    } else {
        context.shipToContactMechId = null;
    }

    shipToContactMechList = ContactHelper.getContactMech(party, "PHONE_SHIPPING", "TELECOM_NUMBER", false)
    if (shipToContactMechList) {
        shipToTelecomNumber = (EntityUtil.getFirst(shipToContactMechList)).getRelatedOne("TelecomNumber", false);
        pcm = EntityUtil.getFirst(shipToTelecomNumber.getRelated("PartyContactMech", null, null, false));
        context.shipToTelecomNumber = shipToTelecomNumber;
        context.shipToExtension = pcm.extension;
    }

    shipToFaxNumberList = ContactHelper.getContactMech(party, "FAX_SHIPPING", "TELECOM_NUMBER", false)
    if (shipToFaxNumberList) {
        shipToFaxNumber = (EntityUtil.getFirst(shipToFaxNumberList)).getRelatedOne("TelecomNumber", false);
        faxPartyContactMech = EntityUtil.getFirst(shipToFaxNumber.getRelated("PartyContactMech", null, null, false));
        context.shipToFaxNumber = shipToFaxNumber;
        context.shipToFaxExtension = faxPartyContactMech.extension;
    }
    
    CartUpdate cartUpdate = CartUpdate.updateSection(request);
    try { // SCIPIO
        ShoppingCart cart = cartUpdate.getCartForUpdate();

        cart.setAllShippingContactMechId(context.shipToContactMechId); // SCIPIO

        cart = cartUpdate.commit(cart); // SCIPIO
    } finally {
        cartUpdate.close();
    }
}
