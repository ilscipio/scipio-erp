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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrQuoteWorkEffortForms {

    @Form(
        name = "ListQuoteWorkEfforts",
        location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml",
        type = FormType.LIST,
        target = "ListQuoteWorkEfforts",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "workEffortId", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditQuoteWorkEffort", description = "${workEffortName} [${workEffortId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "quoteId")})),
            @FormField(name = "workEffortTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortType", description = "${description}")),
            @FormField(name = "statusItemDescription", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "workEffortPurposeTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortPurposeType", description = "${description}")),
            @FormField(name = "actualStartDate", display = @DisplayField),
            @FormField(name = "actualEndDate", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteQuoteWorkEffort", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteWorkEfforts {}

    @Form(
        name = "AddQuoteWorkEffort",
        location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml",
        target = "/ordermgr/control/createQuoteWorkEffort",
        extendsForm = "EditWorkEffort",
        extendsResource = "component://workeffort/widget/WorkEffortForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", mapName = "parameters", display = @DisplayField),
            @FormField(name = "workEffortId", useWhen = "workEffort==null&&workEffortId!=null", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "quoteId"), @SortField(name = "workEffortId")})
    )
    public interface AddQuoteWorkEffort {}

    @Form(
        name = "EditQuoteWorkEffort",
        location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml",
        target = "updateQuoteWorkEffort",
        extendsForm = "EditWorkEffort",
        extendsResource = "component://workeffort/widget/WorkEffortForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", mapName = "parameters", fieldName = "quoteId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditQuoteWorkEffort {}

}
