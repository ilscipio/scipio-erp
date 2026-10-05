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
package com.ilscipio.scipio.manufacturing.widget;

import com.ilscipio.scipio.widget.def.form.*;

/**
 * Form definitions for the manufacturing barcode/QR scan feature.
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class ManufacturingBarcodeForms {

    @Form(
        name = "ScanTaskForm",
        location = "component://manufacturing/widget/manufacturing/BarcodeForms.xml",
        target = "scanTaskCode",
        focusFieldName = "scanCode",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "scanCode", title = "${uiLabelMap.ManufacturingScanCode}", requiredField = true,
                text = @TextField(size = 30)),
            @FormField(name = "scanAction", title = "${uiLabelMap.ManufacturingScanAction}",
                radio = @RadioField(options = {
                    @Option(key = "INFO", description = "${uiLabelMap.ManufacturingScanActionInfo}"),
                    @Option(key = "START", description = "${uiLabelMap.ManufacturingScanActionStart}"),
                    @Option(key = "COMPLETE", description = "${uiLabelMap.ManufacturingScanActionComplete}"),
                    @Option(key = "PRODUCE", description = "${uiLabelMap.ManufacturingScanActionProduce}")
                })),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField(size = 8)),
            @FormField(name = "lotId", title = "${uiLabelMap.ManufacturingLot}", text = @TextField(size = 15)),
            @FormField(name = "comments", title = "${uiLabelMap.ManufacturingComments}", textarea = @TextareaField(cols = 40, rows = 2)),
            @FormField(name = "submitAction", title = "${uiLabelMap.ManufacturingScan}", widgetStyle = "${styles.link_run_sys} ${styles.action_run_local}", submit = @SubmitField)
        }
    )
    public interface ScanTaskForm {}

}
