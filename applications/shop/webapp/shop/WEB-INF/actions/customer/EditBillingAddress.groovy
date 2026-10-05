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
import org.ofbiz.entity.condition.EntityCondition;

if (userLogin) {
    party = userLogin.getRelatedOne("Party", false);
    if (party == null) {
        return; // SCIPIO
    }
    contactMech = EntityUtil.getFirst(ContactHelper.getContactMech(party, "BILLING_LOCATION", "POSTAL_ADDRESS", false));
    if (contactMech) {
        postalAddress = contactMech.getRelatedOne("PostalAddress", false);
        billToContactMechId = postalAddress.contactMechId;
        context.billToContactMechId = billToContactMechId;
        context.billToName = postalAddress.toName;
        context.billToAttnName = postalAddress.attnName; 
        context.billToAddress1 = postalAddress.address1;
        context.billToAddress2 = postalAddress.address2;
        context.billToCity = postalAddress.city;
        context.billToPostalCode = postalAddress.postalCode;
        context.billToStateProvinceGeoId = postalAddress.stateProvinceGeoId;
        context.billToCountryGeoId = postalAddress.countryGeoId;
        billToStateProvinceGeo = from("Geo").where("geoId", postalAddress.stateProvinceGeoId).queryOne();
        if (billToStateProvinceGeo) {
            context.billToStateProvinceGeo = billToStateProvinceGeo.geoName;
        }
        billToCountryProvinceGeo = from("Geo").where("geoId", postalAddress.countryGeoId).queryOne();
        if (billToCountryProvinceGeo) {
            context.billToCountryProvinceGeo = billToCountryProvinceGeo.geoName;
        }

        creditCards = [];
        paymentMethod = from("PaymentMethod").where("partyId", party.partyId, "paymentMethodTypeId", "CREDIT_CARD").orderBy("fromDate").filterByDate().queryFirst();
        if (paymentMethod) {
            creditCard = paymentMethod.getRelatedOne("CreditCard", false);
            context.paymentMethodTypeId = "CREDIT_CARD";
            context.cardNumber = creditCard.cardNumber;
            context.cardType = creditCard.cardType;
            context.paymentMethodId = creditCard.paymentMethodId;
            context.firstNameOnCard = creditCard.firstNameOnCard;
            context.lastNameOnCard = creditCard.lastNameOnCard;
            context.expMonth = (creditCard.expireDate).substring(0, 2);
            context.expYear = (creditCard.expireDate).substring(3);
        }
        if (shipToContactMechId) {
            if (billToContactMechId && billToContactMechId.equals(shipToContactMechId)) {
                context.useShippingAddressForBilling = "Y";
            }
        }
    }

    billToContactMechList = ContactHelper.getContactMech(party, "PHONE_BILLING", "TELECOM_NUMBER", false)
    if (billToContactMechList) {
        billToTelecomNumber = (EntityUtil.getFirst(billToContactMechList)).getRelatedOne("TelecomNumber", false);
        pcm = EntityUtil.getFirst(billToTelecomNumber.getRelated("PartyContactMech", null, null, false));
        context.billToTelecomNumber = billToTelecomNumber;
        context.billToExtension = pcm.extension;
    }

    billToFaxNumberList = ContactHelper.getContactMech(party, "FAX_BILLING", "TELECOM_NUMBER", false)
    if (billToFaxNumberList) {
        billToFaxNumber = (EntityUtil.getFirst(billToFaxNumberList)).getRelatedOne("TelecomNumber", false);
        faxPartyContactMech = EntityUtil.getFirst(billToFaxNumber.getRelated("PartyContactMech", null, null, false));
        context.billToFaxNumber = billToFaxNumber;
        context.billToFaxExtension = faxPartyContactMech.extension;
    }
}
