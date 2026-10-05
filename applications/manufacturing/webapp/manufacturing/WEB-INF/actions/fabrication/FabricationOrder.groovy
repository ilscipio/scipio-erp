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
/**
 * SCIPIO: Loads a fabrication order header (if a fabricationOrderId was given) along with its
 * production runs and totals (via the getFabricationOrder service), for the EditFabricationOrder screen.
 * The screen stays usable without a fabricationOrderId (shows the create form instead).
 */
import org.ofbiz.service.ServiceUtil

fabricationOrderId = parameters.fabricationOrderId;
context.fabricationOrderId = fabricationOrderId;

fabOrder = null;
if (fabricationOrderId) {
    fabOrder = from("WorkEffort").where("workEffortId", fabricationOrderId, "workEffortTypeId", "FAB_ORDER").queryOne();
}
context.fabOrder = fabOrder;

if (fabOrder) {
    result = dispatcher.runSync("getFabricationOrder", [fabricationOrderId: fabricationOrderId, userLogin: userLogin]);
    if (!ServiceUtil.isError(result)) {
        context.fabOrderRuns = result.runs;
        context.fabOrderTotals = result.totals;
    } else {
        context.errorMessage = ServiceUtil.getErrorMessage(result);
    }
}
