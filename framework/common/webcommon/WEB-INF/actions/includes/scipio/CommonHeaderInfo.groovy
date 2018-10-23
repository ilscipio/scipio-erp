import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.party.contact.*;
import org.ofbiz.order.order.OrderReadHelper;
import java.sql.Timestamp;
import org.ofbiz.party.content.PartyContentWrapper;


orderHeader = parameters.orderHeader;
orderId = parameters.orderId;
invoice = parameters.invoice;
invoiceId = parameters.invoiceId;
shipmentId = parameters.shipmentId;
returnHeader = parameters.returnHeader;
returnId = parameters.returnId;
quote = null;
quoteId = parameters.quoteId;
fromPartyId = parameters.fromPartyId;

if (!orderHeader && orderId) {
    orderHeader = from("OrderHeader").where("orderId", orderId).queryOne();
    try {
        if (parameters.facilityId) {
            response.setHeader("Content-Disposition","attachment; filename=\"PickSheet" + orderId + ".pdf" + "\";");
        } else {
            response.setHeader("Content-Disposition","attachment; filename=\"" + orderId + ".pdf" + "\";");
        }
    } catch (MissingPropertyException e) {
        // This hack for OFBIZ-6792 to avoid "groovy.lang.MissingPropertyException: No such property: response for class: CompanyHeader" when response does not exist (in sendOrderConfirmation service)
    }
} else if (shipmentId) {
    shipment = from("Shipment").where("shipmentId", shipmentId).queryOne();
    orderHeader = shipment.getRelatedOne("PrimaryOrderHeader", false);
}

if (!invoice && invoiceId)    {
    invoice = from("Invoice").where("invoiceId", invoiceId).queryOne();
    try {
        response.setHeader("Content-Disposition","attachment; filename=\"" + invoiceId + ".pdf" + "\";");
    } catch (MissingPropertyException e) {
        // This hack for OFBIZ-6792 to avoid "groovy.lang.MissingPropertyException: No such property: response for class: CompanyHeader" when response does not exist (in sendOrderConfirmation service)
    }
}

if (!returnHeader && returnId) {
    returnHeader = from("ReturnHeader").where("returnId", returnId).queryOne();
}

if (quoteId) {
    quote = from("Quote").where("quoteId", quoteId).queryOne();
}

// defaults:
def logoImageUrl = null; // the default value, "/images/ofbiz_powered.gif", is set in the screen decorators
def partyId = null;

// get the logo partyId from order or invoice - note that it is better to do comparisons this way in case the there are null values
if (orderHeader) {
    orh = new OrderReadHelper(orderHeader);
    // for sales order, the logo party is the "BILL_FROM_VENDOR" of the order.  If that's not available, we'll use the OrderHeader's ProductStore's payToPartyId
    if ("SALES_ORDER".equals(orderHeader.orderTypeId)) {
        if (orh.getBillFromParty()) {
            partyId = orh.getBillFromParty().partyId;
        } else {
            productStore = orderHeader.getRelatedOne("ProductStore", false);
            if (orderHeader.orderTypeId.equals("SALES_ORDER") && productStore?.payToPartyId) {
                partyId = productStore.payToPartyId;
            }
        }
    // purchase orders - use the BILL_TO_CUSTOMER of the order
    } else if ("PURCHASE_ORDER".equals(orderHeader.orderTypeId)) {
        def billToParty = orh.getBillToParty();
        if (billToParty) {
            partyId = billToParty.partyId;
        } else {
            def billToCustomer = EntityUtil.getFirst(orderHeader.getRelated("OrderRole", [roleTypeId : "BILL_TO_CUSTOMER"], null, false));
            if (billToCustomer) {
                partyId = billToCustomer.partyId;
            }
        }
    }
} else if (invoice) {
    if ("SALES_INVOICE".equals(invoice.invoiceTypeId) && invoice.partyIdFrom) {
        partyId = invoice.partyIdFrom;
    }
    if ("PURCHASE_INVOICE".equals(invoice.invoiceTypeId) || "CUST_RTN_INVOICE".equals(invoice.invoiceTypeId) && invoice.partyId) {
        partyId = invoice.partyId;
    }
} else if (returnHeader) {
    if ("CUSTOMER_RETURN".equals(returnHeader.returnHeaderTypeId) && returnHeader.toPartyId) {
        partyId = returnHeader.toPartyId;
    } else if ("VENDOR_RETURN".equals(returnHeader.returnHeaderTypeId) && returnHeader.fromPartyId) {
        partyId = returnHeader.fromPartyId;
    }
} else if (quote) {
    productStore = quote.getRelatedOne("ProductStore", false);
    if (productStore?.payToPartyId) {
        partyId = productStore.payToPartyId;
    }
}

// if partyId wasn't found use fromPartyId-parameter
if (!partyId) {
    if (fromPartyId) {
        partyId = fromPartyId;
    } else {
        partyId = EntityUtilProperties.getPropertyValue("general.properties", "ORGANIZATION_PARTY", delegator);
    }
}

// the logo
partyGroup = from("PartyGroup").where("partyId", partyId).queryOne();
if (partyGroup) {
    partyContentWrapper = new PartyContentWrapper(dispatcher, partyGroup, locale, "text/html");
    partyContent = partyContentWrapper.getFirstPartyContentByType(partyGroup.partyId , partyGroup, "LGOIMGURL", delegator);
    if (partyContent) {
        logoImageUrl = "/content/control/stream?contentId="+partyContent.contentId;
    } else {
        if (partyGroup?.logoImageUrl) {
            logoImageUrl = partyGroup.logoImageUrl;
        }
    }
}
//If logoImageUrl not null then only set it to context else it will override the default value "/images/ofbiz_powered.gif"
if (logoImageUrl) {
    context.logoImageUrl = logoImageUrl;
}