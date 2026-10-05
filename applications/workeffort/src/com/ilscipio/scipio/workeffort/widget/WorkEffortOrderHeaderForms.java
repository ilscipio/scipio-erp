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
public class WorkEffortOrderHeaderForms {

    @Form(
        name = "ListWorkEffortOrderHeaders",
        location = "component://workeffort/widget/WorkEffortOrderHeaderForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId"), @ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "statusItemDescription", display = @DisplayField),
            @FormField(name = "orderTypeDescription", display = @DisplayField),
            @FormField(name = "orderDate", display = @DisplayField),
            @FormField(name = "grandTotal", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortOrderHeader", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "orderId")}))
        }
    )
    public interface ListWorkEffortOrderHeaders {}

    @Form(
        name = "AddWorkEffortOrderHeader",
        location = "component://workeffort/widget/WorkEffortOrderHeaderForms.xml",
        target = "createWorkEffortOrderHeader",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddWorkEffortOrderHeader {}

}
