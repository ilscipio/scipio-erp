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
 * SCIPIO: SETUP Accounting Preferences data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditAcctgPreferences.groovy";

orgPartyId = context.orgPartyId;

String acctgPreferencesActionUrl = "setupCreateAccountingPreferences";

partyAcctgPreferences = context.acctgPreferences;
if (partyAcctgPreferences) {
    GenericValue invoiceCustomMethod = partyAcctgPreferences.getRelatedOne("InvoiceCustomMethod", true);
    GenericValue quoteCustomMethod = partyAcctgPreferences.getRelatedOne("QuoteCustomMethod", true);
    GenericValue orderCustomMethod = partyAcctgPreferences.getRelatedOne("OrderCustomMethod", true);
    context.invoiceCustomMethod = invoiceCustomMethod;
    context.quoteCustomMethod = quoteCustomMethod;
    context.orderCustomMethod = orderCustomMethod;
    
    acctgPreferencesActionUrl = "setupUpdateAccountingPreferences";
}

context.acctgPreferencesActionUrl = acctgPreferencesActionUrl;

cogsMethods = EntityQuery.use(delegator).from("Enumeration").where(["enumTypeId": "COGS_METHODS"]).orderBy("description").cache().queryList();
context.cogsMethods = cogsMethods;

taxForms = EntityQuery.use(delegator).from("Enumeration").where(["enumTypeId": "TAX_FORMS"]).orderBy("description").cache().queryList();
context.taxForms = taxForms;

currencyUoms = EntityQuery.use(delegator).from("Uom").where(["uomTypeId": "CURRENCY_MEASURE"]).orderBy("description").cache().queryList();
context.currencyUoms = currencyUoms;

invoiceCustomMethods = EntityQuery.use(delegator).from("CustomMethod").where(["customMethodTypeId": "INVOICE_HOOK"]).orderBy("description").cache().queryList();
context.invoiceCustomMethods = invoiceCustomMethods;

orderCustomMethods = EntityQuery.use(delegator).from("CustomMethod").where(["customMethodTypeId": "ORDER_HOOK"]).orderBy("description").cache().queryList();
context.orderCustomMethods = orderCustomMethods;

quoteCustomMethods = EntityQuery.use(delegator).from("CustomMethod").where(["customMethodTypeId": "QUOTE_HOOK"]).orderBy("description").cache().queryList();
context.quoteCustomMethods = quoteCustomMethods;

long acctgTransCount = EntityQuery.use(delegator).from("AcctgTrans").queryCount();
long orderCount = EntityQuery.use(delegator).from("OrderHeader").queryCount();
long invoiceCount = EntityQuery.use(delegator).from("Invoice").queryCount();
long quoteCount = EntityQuery.use(delegator).from("Quote").queryCount();
context.acctgTransCount = acctgTransCount; 
context.orderCount = orderCount;
context.invoiceCount = invoiceCount;
context.quoteCount = quoteCount;
