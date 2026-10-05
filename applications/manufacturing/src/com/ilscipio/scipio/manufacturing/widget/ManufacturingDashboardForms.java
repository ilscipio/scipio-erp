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
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Manufacturing capacity planning dashboard form definitions.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingDashboardForms {

    @Form(
        name = "FacilitySelect",
        location = "component://manufacturing/widget/manufacturing/DashboardForms.xml",
        target = "Dashboard",
        method = "get",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", submit = @SubmitField)
        }
    )
    public interface FacilitySelect {}

    @Form(
        name = "WorkCenterLoadForm",
        location = "component://manufacturing/widget/manufacturing/DashboardForms.xml",
        target = "WorkCenterLoad",
        method = "get",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.ManufacturingWorkCenter}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]"))),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "timestamp")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "timestamp")),
            @FormField(name = "includeClosed", title = "${uiLabelMap.ManufacturingIncludeClosed}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", submit = @SubmitField)
        }
    )
    public interface WorkCenterLoadForm {}

}
