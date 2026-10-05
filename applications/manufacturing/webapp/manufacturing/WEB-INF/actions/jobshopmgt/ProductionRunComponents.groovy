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

import org.ofbiz.widget.renderer.html.HtmlFormWrapper
import org.ofbiz.service.ServiceUtil

FORMS_CLASS = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml";

productionRunId = parameters.productionRunId ?: parameters.workEffortId;
runFacilityId = productionRun ? productionRun.facilityId : null;

// SCIPIO: delivProducts must always be set in context, since CreateRoutingTaskDelivProduct
// (included further down in the screen) tests "${groovy:delivProducts.size()>0}" unconditionally.
context.delivProducts = [];

tasks = from("WorkEffort").where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK").orderBy("workEffortId").queryList();

taskInfos = [];
outputs = [];

tasks.each { task ->
    // Inputs (consumed): WorkEffortGoodStandard rows of type PRUNT_PROD_NEEDED, enriched with
    // issued quantity and, for the run facility, quantity-on-hand / available-to-promise, plus
    // a shortage mark when the outstanding (not-yet-issued) quantity exceeds what is available.
    inputRows = [];
    materials = from("WorkEffortGoodStandard").where("workEffortId", task.workEffortId, "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED").queryList();
    materials.each { material ->
        Map row = material.getAllFields();

        issuances = from("WorkEffortAndInventoryAssign").where("workEffortId", material.workEffortId, "productId", material.productId).queryList();
        totalIssued = 0.0;
        issuances.each { issuance ->
            if (issuance.quantity) {
                totalIssued += issuance.quantity;
            }
        }
        row.issuedQuantity = totalIssued;

        qoh = 0.0;
        atp = 0.0;
        if (runFacilityId && material.productId) {
            try {
                availResult = dispatcher.runSync("getInventoryAvailableByFacility",
                        [productId: material.productId, facilityId: runFacilityId, userLogin: userLogin]);
                if (!ServiceUtil.isError(availResult)) {
                    qoh = availResult.quantityOnHandTotal ?: 0.0;
                    atp = availResult.availableToPromiseTotal ?: 0.0;
                }
            } catch (Exception e) {
                // leave qoh/atp at 0.0; not fatal for rendering the screen
            }
        }
        row.qoh = qoh;
        row.atp = atp;

        outstanding = (material.estimatedQuantity ?: 0.0) - totalIssued;
        row.shortage = (outstanding > atp) ? "Y" : "N";

        inputRows.add(row);
    }
    HtmlFormWrapper taskForm = null;
    HtmlFormWrapper replaceForm = null;
    try {
        taskForm = new HtmlFormWrapper(FORMS_CLASS, "ProductionRunTaskComponents", request, response);
        taskForm.putInContext("records", inputRows);
        replaceForm = new HtmlFormWrapper(FORMS_CLASS, "ReplaceProductionRunComponent", request, response);
        replaceForm.putInContext("records", inputRows);
    } catch (Exception e) {
        // SCIPIO: don't let a form-resolution problem take down the whole screen; the Inputs table
        // for this task is simply skipped and the rest of the page (Outputs, other tasks) still renders.
        Debug.logError(e, "Could not build component forms for task " + task.workEffortId, "ProductionRunComponents.groovy");
    }
    taskInfos.add([task : task, taskForm : taskForm, replaceForm : replaceForm, inputRows : inputRows]);

    // Outputs (produced): WorkEffortGoodStandard rows of type PRUNT_PROD_DELIV for this task.
    taskOutputs = from("WorkEffortGoodStandard").where("workEffortId", task.workEffortId, "workEffortGoodStdTypeId", "PRUNT_PROD_DELIV").queryList();
    taskOutputs.each { out ->
        Map row = out.getAllFields();
        product = out.getRelatedOne("Product", false);
        row.internalName = product ? product.internalName : null;
        row.taskName = task.workEffortName ?: task.workEffortId;
        row.plannedQuantity = out.estimatedQuantity;

        produced = from("WorkEffortAndInventoryProduced").where("workEffortId", task.workEffortId, "productId", out.productId).queryList();
        totalProduced = 0.0;
        produced.each { p ->
            detail = from("InventoryItemDetail").where("inventoryItemId", p.inventoryItemId).orderBy("inventoryItemDetailSeqId").queryFirst();
            if (detail && detail.quantityOnHandDiff) {
                totalProduced += detail.quantityOnHandDiff;
            }
        }
        row.producedQuantity = totalProduced;
        outputs.add(row);
    }
}
context.taskInfos = taskInfos;

// Outputs (produced): the run-level deliverable, WorkEffortGoodStandard of type PRUN_PROD_DELIV.
if (productionRun) {
    runOutput = from("WorkEffortGoodStandard").where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").orderBy("-fromDate").queryFirst();
    if (runOutput) {
        Map row = runOutput.getAllFields();
        product = runOutput.getRelatedOne("Product", false);
        row.internalName = product ? product.internalName : null;
        row.taskName = null; // rendered as the production run itself
        row.plannedQuantity = productionRun.quantityToProduce;
        row.producedQuantity = productionRun.quantityProduced;
        outputs.add(0, row);
    }
}
context.outputs = outputs;

// SCIPIO: lot reservations of the run's tasks, plus the "Reserved lots" list form and "Reserve a lot" form.
reservations = [];
if (productionRunId) {
    try {
        reservationsResult = dispatcher.runSync("getProductionRunReservations", [productionRunId: productionRunId, userLogin: userLogin]);
        if (!ServiceUtil.isError(reservationsResult)) {
            reservations = reservationsResult.reservations ?: [];
        }
    } catch (Exception e) {
        Debug.logError(e, "Could not look up lot reservations for production run " + productionRunId, "ProductionRunComponents.groovy");
    }
}
context.reservations = reservations;

reservationsForm = null;
reserveLotForm = null;
try {
    reservationsForm = new HtmlFormWrapper(FORMS_CLASS, "ProductionRunLotReservations", request, response);
    reservationsForm.putInContext("reservations", reservations);
    reserveLotForm = new HtmlFormWrapper(FORMS_CLASS, "ReserveProductionRunLot", request, response);
} catch (Exception e) {
    // SCIPIO: same defensive pattern as the task/replace forms above; the FTL falls back to a plain table
    Debug.logError(e, "Could not build reservation forms for production run " + productionRunId, "ProductionRunComponents.groovy");
}
context.reservationsForm = reservationsForm;
context.reserveLotForm = reserveLotForm;
