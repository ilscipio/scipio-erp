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
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.util.EntityQuery

acctgTypeConds = UtilMisc.toList(
        EntityCondition.makeCondition("parentTypeId", EntityJoinOperator.EQUALS, null),
        EntityCondition.makeCondition("parentTypeId", EntityJoinOperator.EQUALS, ""));
    
context.acctgParentTransTypes = EntityQuery.use(delegator).from("AcctgTransType").cache(true).where(acctgTypeConds, EntityJoinOperator.OR).queryList();
context.acctgParentEntryTransTypes = EntityQuery.use(delegator).from("AcctgTransEntryType").cache(true).where(acctgTypeConds, EntityJoinOperator.OR).queryList();

context.datevDataCategories = EntityQuery.use(delegator).from("DatevDataCategory").cache(true).queryList();