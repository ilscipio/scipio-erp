/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */

// SCIPIO: Resources screen (machines and workers) for a production run.
// The machine list/forms are rendered directly through the ProductionRunTaskFixedAssets/AddProductionRunTaskFixedAsset
// forms (no screen-local FTL needed), one flat "records" list spanning all tasks of the run.

productionRunId = parameters.productionRunId ?: parameters.workEffortId;

tasks = from("WorkEffort").where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK").orderBy("workEffortId").queryList();

records = [];
tasks.each { task ->
    records.addAll(task.getRelated("WorkEffortFixedAssetAssign", null, null, false));
}
context.records = records;

// SCIPIO: the people assigned to the run or its tasks (WorkEffortPartyAssignment).
tasksByWorkEffortId = [:];
tasks.each { task -> tasksByWorkEffortId[task.workEffortId] = task };
workEffortIds = tasks.collect { it.workEffortId };
workEffortIds.add(productionRunId);

workers = [];
workEffortIds.each { weId ->
    assigns = from("WorkEffortPartyAssignment").where("workEffortId", weId).filterByDate().queryList();
    assigns.each { assign ->
        partyNameView = from("PartyNameView").where("partyId", assign.partyId).queryOne();
        partyName = assign.partyId;
        if (partyNameView) {
            partyName = partyNameView.groupName ?: [partyNameView.firstName, partyNameView.lastName].findAll { it }.join(" ");
        }
        roleType = from("RoleType").where("roleTypeId", assign.roleTypeId).cache(true).queryOne();
        task = tasksByWorkEffortId[weId];
        taskLabel = task ? "${task.workEffortName} [${task.workEffortId}]" : "${productionRun?.workEffortName} [${weId}]";
        workers.add([
            partyId        : assign.partyId,
            partyName      : partyName,
            roleTypeId     : assign.roleTypeId,
            roleDescription: roleType?.description ?: assign.roleTypeId,
            workEffortId   : weId,
            taskLabel      : taskLabel,
            fromDate       : assign.fromDate
        ]);
    }
}
context.workers = workers;
context.tasks = tasks;
context.productionRunId = productionRunId;
