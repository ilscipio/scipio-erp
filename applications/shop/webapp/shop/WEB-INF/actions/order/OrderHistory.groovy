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

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.condition.*;

// SCIPIO: Some fixes to prevent crash on missing userLogin

partyRole = from("PartyRole").where("partyId", userLogin?.partyId, "roleTypeId", "SUPPLIER").queryOne();
if (partyRole) {
    if ("SUPPLIER".equals(partyRole.roleTypeId)) {
        /** drop shipper or supplier **/
        porderRoleCollection = from("OrderRole").where("partyId", userLogin?.partyId, "roleTypeId", "SUPPLIER_AGENT").queryList();
        porderHeaderList = EntityUtil.orderBy(EntityUtil.filterByAnd(EntityUtil.getRelated("OrderHeader", null, porderRoleCollection, false),
                [EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "ORDER_REJECTED"),
                 EntityCondition.makeCondition("orderTypeId", EntityOperator.EQUALS, "PURCHASE_ORDER")]),
                 ["orderDate DESC"]);
        context.porderHeaderList = porderHeaderList;
    }
}
orderRoleCollection = from("OrderRole").where("partyId", userLogin?.partyId, "roleTypeId", "PLACING_CUSTOMER").queryList();
orderHeaderList = EntityUtil.orderBy(EntityUtil.filterByAnd(EntityUtil.getRelated("OrderHeader", null, orderRoleCollection, false),
        [EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "ORDER_REJECTED")]), ["orderDate DESC"]);
context.orderHeaderList = orderHeaderList;

// SCIPIO: order by ProductContent.sequenceNum
downloadOrderRoleAndProductContentInfoList = from("OrderRoleAndProductContentInfo").where("partyId", userLogin?.partyId, "roleTypeId", "PLACING_CUSTOMER", "productContentTypeId", "DIGITAL_DOWNLOAD", "statusId", "ITEM_COMPLETED").orderBy("sequenceNum ASC").queryList();
context.downloadOrderRoleAndProductContentInfoList = downloadOrderRoleAndProductContentInfoList;

// SCIPIO: Flag
context.hasOrderDownloads = downloadOrderRoleAndProductContentInfoList ? true : false;
