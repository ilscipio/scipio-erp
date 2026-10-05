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
package com.ilscipio.scipio.workeffort.widget;

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
public class WorkEffortRequirementForms {

    @Form(
        name = "ListWorkEffortRequirements",
        location = "component://workeffort/widget/WorkEffortRequirementForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/EditRequirement", urlMode = UrlMode.INTER_APP, description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")})),
            @FormField(name = "workReqFulfTypeDescription", display = @DisplayField),
            @FormField(name = "statusItemDescription", display = @DisplayField),
            @FormField(name = "requirementDescription", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortRequirement", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "requirementId")}))
        }
    )
    public interface ListWorkEffortRequirements {}

    @Form(
        name = "AddWorkEffortRequirement",
        location = "component://workeffort/widget/WorkEffortRequirementForms.xml",
        target = "createWorkEffortRequirement",
        defaultMapName = "workRequirementFulfillment",
        extendsForm = "EditRequirement",
        extendsResource = "component://order/widget/ordermgr/RequirementForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "requirementId", lookup = @LookupField(targetFormName = "LookupRequirement")),
            @FormField(name = "workReqFulfTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkReqFulfType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "requirementId"), @SortField(name = "workReqFulfTypeId")})
    )
    public interface AddWorkEffortRequirement {}

}
