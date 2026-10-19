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
/*
 * E-invoice settings of a party: the PartyEInvoicePref record, the format and channel options, and the EFT accounts
 * of the party (for an organization: the account printed on the invoice).
 */
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.party.party.PartyHelper

partyId = parameters.partyId
if (!partyId) {
    return
}
context.partyId = partyId
context.partyName = PartyHelper.getPartyName(delegator, partyId, false)
context.einvPref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", partyId).queryOne()
context.einvFormats = EntityQuery.use(delegator).from("Enumeration").where("enumTypeId", "EINV_FORMAT").orderBy("sequenceId").cache().queryList()
context.einvChannels = EntityQuery.use(delegator).from("Enumeration").where("enumTypeId", "EINV_DELIVERY").orderBy("sequenceId").cache().queryList()
eftAccounts = []
EntityQuery.use(delegator).from("PaymentMethod").where("partyId", partyId, "paymentMethodTypeId", "EFT_ACCOUNT").filterByDate().queryList().each { pm ->
    eft = EntityQuery.use(delegator).from("EftAccount").where("paymentMethodId", pm.paymentMethodId).queryOne()
    if (eft) {
        eftAccounts.add([paymentMethodId: pm.paymentMethodId, label: (eft.bankName ?: "") + " " + (eft.ibanNumber ?: eft.accountNumber ?: "")])
    }
}
context.einvEftAccounts = eftAccounts
