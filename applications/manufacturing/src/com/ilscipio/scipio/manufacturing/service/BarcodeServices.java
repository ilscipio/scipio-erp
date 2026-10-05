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
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Barcode/QR scan service definitions: records one shop floor scan and performs the requested
 * task action (start, complete, produce, or info only), and lists recent scans.
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class BarcodeServices {

    /**
     * Parses a scanned code (PRUN:productionRunId:workEffortId, PRUN:productionRunId, or a bare
     * task work effort id), resolves the task/run, performs the requested action, and always
     * records the scan (including a failed action, with the error in comments).
     */
    @Service(
        name = "recordProductionRunScan",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.barcode.BarcodeServices",
        invoke = "recordProductionRunScan",
        description = "Records one barcode/QR scan and performs the requested action on the scanned production run task",
        auth = "true",
        attributes = {
            @Attribute(name = "scanCode", type = "String", mode = "IN"),
            @Attribute(name = "scanAction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scanId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "productionRunId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "taskName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "productName", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "message", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RecordProductionRunScan {}

    /**
     * Returns the most recent barcode/QR scans for a production run and/or a task, newest first.
     */
    @Service(
        name = "getProductionRunScans",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.barcode.BarcodeServices",
        invoke = "getProductionRunScans",
        description = "Returns the most recent barcode/QR scans for a production run and/or task",
        auth = "true",
        attributes = {
            @Attribute(name = "productionRunId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "limit", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "scans", type = "List", mode = "OUT")
        }
    )
    public interface GetProductionRunScans {}

}
