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
package com.ilscipio.scipio.manufacturing.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Controller definitions for the manufacturing barcode/QR scan feature: the scan screen and the
 * production run label sheet PDF.
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class BarcodeControllerDef {

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ScanTask",
        type = "screen",
        page = "component://manufacturing/widget/manufacturing/BarcodeScreens.xml#ScanTask",
        controller = "manufacturing"
    )
    public static final String VIEW_SCANTASK = "ScanTask";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ProductionRunLabelsPdf",
        type = "screenfop",
        page = "component://manufacturing/widget/manufacturing/BarcodeScreens.xml#ProductionRunLabels",
        contentType = "application/pdf",
        encoding = "none",
        controller = "manufacturing"
    )
    public static final String VIEW_PRODUCTIONRUNLABELSPDF = "ProductionRunLabelsPdf";

    @Request(
        uri = "ScanTask",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ScanTask")
    public interface ScanTask {}

    @Request(
        uri = "scanTaskCode",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ScanTask")
    @Response(name = "error", type = "view", value = "ScanTask")
    @Event(type = "java", path = "com.ilscipio.scipio.manufacturing.barcode.BarcodeEvents", invoke = "scanTaskCode")
    public static String scanTaskCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "ProductionRunLabelsPdf",
        controller = "manufacturing",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "ProductionRunLabelsPdf")
    public interface ProductionRunLabelsPdf {}
}
