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

// SCIPIO: Loads the outcome of the last scan (if any) and the recent scan history of its
// production run, for the ScanTask screen.

scanCode = request.getAttribute("scanCode") ?: parameters.scanCode;
context.scanCode = scanCode;
scanAction = request.getAttribute("scanAction") ?: parameters.scanAction ?: "INFO";
context.scanAction = scanAction;

scanResult = request.getAttribute("scanResult");
productionRunId = null;
if (scanResult) {
    context.scanResult = scanResult;
    productionRunId = scanResult.productionRunId;
}
if (!productionRunId) {
    productionRunId = parameters.productionRunId;
}

if (productionRunId) {
    scansOut = runService('getProductionRunScans', [productionRunId: productionRunId, limit: 20]);
    context.lastScans = scansOut.scans;
}
