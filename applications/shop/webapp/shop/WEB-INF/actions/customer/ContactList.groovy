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

import java.lang.*;
import java.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.party.contact.ContactMechWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.webapp.website.WebSiteWorker;
import org.ofbiz.accounting.payment.PaymentWorker;

/*publicEmailContactLists = delegator.findByAnd("ContactList", [isPublic : "Y", contactMechTypeId : "EMAIL_ADDRESS"], ["contactListName"], false);
context.publicEmailContactLists = publicEmailContactLists;*/

webSiteId = WebSiteWorker.getWebSiteId(request);
exprList = [];
exprListThruDate = [];
exprList.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId));
exprListThruDate.add(EntityCondition.makeCondition("thruDate", EntityOperator.EQUALS, null));
exprListThruDate.add(EntityCondition.makeCondition("thruDate", EntityOperator.GREATER_THAN_EQUAL_TO, UtilDateTime.nowTimestamp()));
orCond = EntityCondition.makeCondition(exprListThruDate, EntityOperator.OR);
exprList.add(orCond);
webSiteContactList = from("WebSiteContactList").where(exprList).queryList();

publicEmailContactLists = [];
webSiteContactList.each { webSiteContactList ->
    contactList = webSiteContactList.getRelatedOne("ContactList", false);
    contactListType = contactList.getRelatedOne("ContactListType", false);
    temp = [:];
    temp.contactList = contactList;
    temp.contactListType = contactListType;
    publicEmailContactLists.add(temp);
}
context.publicEmailContactLists = publicEmailContactLists;

if (userLogin) {
    partyAndContactMechList = from("PartyAndContactMech").where("partyId", partyId, "contactMechTypeId", "EMAIL_ADDRESS").orderBy("-fromDate").filterByDate().queryList();
    context.partyAndContactMechList = partyAndContactMechList;
}


