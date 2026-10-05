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

// SCIPIO: Groups CostComponent rows by base cost type (EST_STD_/ACTUAL_ prefix stripped) so the template
// can show a Standard/Actual/Variance table per task and one for the whole run.

productionRunId = parameters.productionRunId ?: parameters.workEffortId;

baseCostTypeLabels = [
    "MAT_COST"  : "ManufacturingCostTypeMaterial",
    "ROUTE_COST": "ManufacturingCostTypeRouting",
    "LABOR_COST": "ManufacturingCostTypeLabor",
    "GEN_COST"  : "ManufacturingCostTypeOverheadGeneral",
    "IND_COST"  : "ManufacturingCostTypeOverheadIndirect",
    "OTHER_COST": "ManufacturingCostTypeOther"
];
baseCostTypeOrder = ["MAT_COST", "ROUTE_COST", "LABOR_COST", "GEN_COST", "IND_COST", "OTHER_COST"];

def newRow(baseType, costUomId) {
    return [baseType: baseType, labelKey: baseCostTypeLabels[baseType], standard: BigDecimal.ZERO, actual: BigDecimal.ZERO, currencyUomId: costUomId];
}

def orderRows(grouped) {
    rows = [];
    baseCostTypeOrder.each { bt -> if (grouped[bt] != null) rows.add(grouped[bt]); }
    grouped.each { bt, row -> if (!baseCostTypeOrder.contains(bt)) rows.add(row); }
    rows.each { row -> row.variance = row.actual.subtract(row.standard); }
    return rows;
}

def groupCosts(costs) {
    grouped = [:];
    costs.each { cc ->
        typeId = cc.costComponentTypeId ?: "";
        isActual = typeId.startsWith("ACTUAL_");
        baseType = typeId.startsWith("EST_STD_") ? typeId.substring(8) : (isActual ? typeId.substring(7) : typeId);
        row = grouped[baseType];
        if (row == null) {
            row = newRow(baseType, cc.costUomId);
            grouped[baseType] = row;
        }
        cost = cc.cost ?: BigDecimal.ZERO;
        if (isActual) {
            row.actual = row.actual.add(cost);
        } else {
            row.standard = row.standard.add(cost);
        }
        if (!row.currencyUomId) {
            row.currencyUomId = cc.costUomId;
        }
    }
    return orderRows(grouped);
}

def mergeInto(totalsMap, rows) {
    rows.each { row ->
        total = totalsMap[row.baseType];
        if (total == null) {
            total = newRow(row.baseType, row.currencyUomId);
            totalsMap[row.baseType] = total;
        }
        total.standard = total.standard.add(row.standard);
        total.actual = total.actual.add(row.actual);
        if (!total.currencyUomId) {
            total.currencyUomId = row.currencyUomId;
        }
    }
}

taskCosts = [];
runTotals = [:];
tasks = from("WorkEffort").where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK").orderBy("workEffortId").queryList();
tasks.each { task ->
    costs = from("CostComponent").where("workEffortId", task.workEffortId).filterByDate().queryList();
    rows = groupCosts(costs);
    taskCosts.add([task: task, rows: rows]);
    mergeInto(runTotals, rows);
}

// get the costs directly associated to the production run (e.g. overhead costs)
productionRun = from("WorkEffort").where("workEffortId", productionRunId).cache(true).queryOne();
runLevelCosts = from("CostComponent").where("workEffortId", productionRunId).filterByDate().queryList();
runLevelRows = groupCosts(runLevelCosts);
if (runLevelRows) {
    taskCosts.add([task: productionRun, rows: runLevelRows]);
    mergeInto(runTotals, runLevelRows);
}

context.taskCosts = taskCosts;
context.runTotalRows = orderRows(runTotals);
