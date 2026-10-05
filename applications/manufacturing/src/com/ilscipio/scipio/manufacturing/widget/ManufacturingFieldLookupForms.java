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
public class ManufacturingFieldLookupForms {

    @Form(
        name = "lookupRouting",
        location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml",
        target = "LookupRouting",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "lookupRoutingTask", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", textFind = @TextFindField),
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "ROUTING")),
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fixedAssetId_op", hidden = @HiddenField(value = "equals")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupRouting {}

    @Form(
        name = "listLookupRouting",
        location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupRouting",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${workEffortId}')", urlMode = UrlMode.PLAIN, description = "${workEffortId}", alsoHidden = false)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingRoutingName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "quantityToProduce", title = "${uiLabelMap.ManufacturingQuantityMinimum}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupRouting {}

    @Form(
        name = "lookupRoutingTask",
        location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml",
        target = "LookupRoutingTask",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "lookupRoutingTask", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "ROU_TASK")),
            @FormField(name = "fixedAssetId", dropDown = @DropDownField(options = {@Option()}, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName}", constraints = {@EntityConstraint(name = "fixedAssetTypeId", value = "GROUP_EQUIPMENT")}))),
            @FormField(name = "fixedAssetId_op", hidden = @HiddenField(value = "equals")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupRoutingTask {}

    @Form(
        name = "listLookupRoutingTask",
        location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupRoutingTask",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${workEffortId}')", urlMode = UrlMode.PLAIN, description = "${workEffortId}", alsoHidden = false)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.ManufacturingTaskName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "estimatedSetupMillis", title = "${uiLabelMap.ManufacturingTaskEstimatedSetupMillis}", display = @DisplayField),
            @FormField(name = "estimatedMilliSeconds", title = "${uiLabelMap.ManufacturingTaskEstimatedMilliSeconds}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupRoutingTask {}

}
