/**
 * SCIPIO: SETUP Accounting Preferences data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditAcctgPreferences.groovy";

orgPartyId = context.orgPartyId;

partyAcctgPreferences = context.acctgPreferences;
if (partyAcctgPreferences) {
    GenericValue invoiceCustomMethod = partyAcctgPreferences.getRelatedOne("InvoiceCustomMethod", true);
    GenericValue quoteCustomMethod = partyAcctgPreferences.getRelatedOne("QuoteCustomMethod", true);
    GenericValue orderCustomMethod = partyAcctgPreferences.getRelatedOne("OrderCustomMethod", true);
    context.invoiceCustomMethod = invoiceCustomMethod;
    context.quoteCustomMethod = quoteCustomMethod;
    context.orderCustomMethod = orderCustomMethod;
}

invoiceCustomMethods = EntityQuery.use(delegator).from("CustomMethod").where(["customMethodTypeId": "INVOICE_HOOK"]).orderBy("customMethodName").cache().queryList();


context.invoiceCustomMethods = invoiceCustomMethods;