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

// SCIPIO: Loads the production run header, its tasks, and the produced product for the
// ProductionRunLabels label sheet (one A6 label per task, plus one for the run).

productionRunId = parameters.productionRunId;
context.productionRunId = productionRunId;

if (productionRunId) {
    productionRun = from("WorkEffort").where("workEffortId", productionRunId).queryOne();
    context.productionRun = productionRun;

    if (productionRun) {
        context.runScanCode = "PRUN:" + productionRunId;

        producedGood = from("WorkEffortGoodStandard")
                .where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV")
                .queryFirst();
        if (producedGood) {
            context.productId = producedGood.productId;
            context.product = from("Product").where("productId", producedGood.productId).queryOne();
        }

        tasks = from("WorkEffort")
                .where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK")
                .orderBy("priority")
                .queryList();
        context.tasks = tasks;

        workCenterNames = [:];
        tasks.each { task ->
            fixedAssetId = task.fixedAssetId;
            if (fixedAssetId && !workCenterNames.containsKey(fixedAssetId)) {
                fixedAsset = from("FixedAsset").where("fixedAssetId", fixedAssetId).queryOne();
                workCenterNames[fixedAssetId] = fixedAsset ? fixedAsset.fixedAssetName : null;
            }
        }
        context.workCenterNames = workCenterNames;
    }
}
