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

invoiceCustomMethods = EntityQuery.use(delegator).from("Enumeration").where(["enumTypeId": "INVOICE_SEQMD"]).orderBy("description").cache().queryList();
context.invoiceCustomMethods = invoiceCustomMethods;

orderCustomMethods = EntityQuery.use(delegator).from("Enumeration").where(["enumTypeId": "ORDER_SEQMD"]).orderBy("description").cache().queryList();
context.orderCustomMethods = orderCustomMethods;

quoteCustomMethods = EntityQuery.use(delegator).from("Enumeration").where(["enumTypeId": "QUOTE_SEQMD"]).orderBy("description").cache().queryList();
context.quoteCustomMethods = quoteCustomMethods;

