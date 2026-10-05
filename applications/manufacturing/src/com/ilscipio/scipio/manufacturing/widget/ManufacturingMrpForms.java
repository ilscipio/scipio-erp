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
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ManufacturingMrpForms {

    @Form(
        name = "RunMrp",
        location = "component://manufacturing/widget/manufacturing/MrpForms.xml",
        target = "runMrpGo",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "mrpName", title = "${uiLabelMap.ManufacturingMrpName}", text = @TextField(size = 20)),
            @FormField(name = "facilityGroupId", title = "${uiLabelMap.ProductFacilityGroup}", tooltip = "${uiLabelMap.ManufacturingRunMrpFacilityTooltip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FacilityGroup", description = "${facilityGroupName} [${facilityGroupId}]"))),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", tooltip = "${uiLabelMap.ManufacturingRunMrpFacilityTooltip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "manufacturingFacilityId", title = "${uiLabelMap.ManufacturingManufacturingFacility}", tooltip = "${uiLabelMap.ManufacturingManufacturingFacilityTooltip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "defaultYearsOffset", title = "${uiLabelMap.ManufacturingDefaultYearsOffset}", tooltip = "${uiLabelMap.ManufacturingDefaultYearsOffsetTooltip}", text = @TextField(size = 5, defaultValue = "1")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", submit = @SubmitField)
        }
    )
    public interface RunMrp {}

    @Form(
        name = "ListRunningMrpJobs",
        location = "component://manufacturing/widget/manufacturing/MrpForms.xml",
        type = FormType.LIST,
        listName = "mrpActiveJobs",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobSandbox", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "poolId", hidden = @HiddenField),
            @FormField(name = "parentJobId", hidden = @HiddenField),
            @FormField(name = "previousJobId", hidden = @HiddenField),
            @FormField(name = "loaderName", hidden = @HiddenField),
            @FormField(name = "runAsUser", hidden = @HiddenField),
            @FormField(name = "runByInstanceId", hidden = @HiddenField),
            @FormField(name = "runtimeDataId", hidden = @HiddenField),
            @FormField(name = "recurrenceInfoId", hidden = @HiddenField),
            @FormField(name = "serviceName", hidden = @HiddenField),
            @FormField(name = "startDateTime", hidden = @HiddenField),
            @FormField(name = "finishDateTime", hidden = @HiddenField),
            @FormField(name = "cancelDateTime", hidden = @HiddenField)
        }
    )
    public interface ListRunningMrpJobs {}

    @Form(
        name = "ListFinishedMrpJobs",
        location = "component://manufacturing/widget/manufacturing/MrpForms.xml",
        type = FormType.LIST,
        listName = "lastFinishedJobs",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobSandbox", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "poolId", hidden = @HiddenField),
            @FormField(name = "parentJobId", hidden = @HiddenField),
            @FormField(name = "previousJobId", hidden = @HiddenField),
            @FormField(name = "loaderName", hidden = @HiddenField),
            @FormField(name = "runAsUser", hidden = @HiddenField),
            @FormField(name = "runByInstanceId", hidden = @HiddenField),
            @FormField(name = "runtimeDataId", hidden = @HiddenField),
            @FormField(name = "recurrenceInfoId", hidden = @HiddenField),
            @FormField(name = "serviceName", hidden = @HiddenField)
        }
    )
    public interface ListFinishedMrpJobs {}

}
