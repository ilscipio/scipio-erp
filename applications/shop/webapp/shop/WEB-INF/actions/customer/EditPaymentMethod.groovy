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

import java.util.HashMap;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.accounting.payment.PaymentWorker;
import org.ofbiz.party.contact.ContactMechWorker;

// SCIPIO: prevent crash on missing userLogin

paymentResults = PaymentWorker.getPaymentMethodAndRelated(request, userLogin?.partyId);
//returns the following: "paymentMethod", "creditCard", "giftCard", "eftAccount", "paymentMethodId", "curContactMechId", "donePage", "tryEntity"
context.putAll(paymentResults);

curPostalAddressResults = ContactMechWorker.getCurrentPostalAddress(request, userLogin?.partyId, paymentResults.curContactMechId);
//returns the following: "curPartyContactMech", "curContactMech", "curPostalAddress", "curPartyContactMechPurposes"
context.putAll(curPostalAddressResults);

postalAddressInfos = ContactMechWorker.getPartyPostalAddresses(request, userLogin?.partyId, paymentResults.curContactMechId);
context.put("postalAddressInfos", postalAddressInfos);

//prepare "Data" maps for filling form input boxes
tryEntity = paymentResults.tryEntity;

creditCardData = paymentResults.creditCard;
if (!tryEntity) creditCardData = parameters;
if (!creditCardData) creditCardData = [:];
if (creditCardData) context.creditCardData = creditCardData;

giftCardData = paymentResults.giftCard;
if (!tryEntity) giftCardData = parameters;
if (!giftCardData) giftCardData = [:];
if (giftCardData) context.giftCardData = giftCardData;

eftAccountData = paymentResults.eftAccount;
if (!tryEntity) eftAccountData = parameters;
if (!eftAccountData) eftAccountData = [:];
if (eftAccountData) context.eftAccountData = eftAccountData;

paymentMethodData = paymentResults.paymentMethod;
if (!tryEntity) paymentMethodData = parameters;
if (!paymentMethodData) paymentMethodData = [:];
if (paymentMethodData) context.paymentMethodData = paymentMethodData;

//prepare security flag
if (!security.hasEntityPermission("PARTYMGR", "_VIEW", request) && (context.creditCard || context.giftCard || context.eftAccount) && context.paymentMethod && (!userLogin?.partyId || !userLogin.partyId.equals(context.paymentMethod.partyId))) {
    context.canNotView = true;
} else {
    context.canNotView = false;
}

// SCIPIO: for double-inclusion detection
context.editPaymentMethodDataPrepared = true;


