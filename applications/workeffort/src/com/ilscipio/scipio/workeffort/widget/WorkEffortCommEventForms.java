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
public class WorkEffortCommEventForms {

    @Form(
        name = "ListWorkEffortCommEvents",
        location = "component://workeffort/widget/WorkEffortCommEventForms.xml",
        type = FormType.LIST,
        target = "updateCommunicationEventWorkEff",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/EditCommunicationEvent", urlMode = UrlMode.INTER_APP, description = "${communicationEventId}", parameters = {@ParameterDef(paramName = "communicationEventId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "description", text = @TextField(size = 40)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCommunicationEventWorkEff", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "communicationEventId")}))
        }
    )
    public interface ListWorkEffortCommEvents {}

    @Form(
        name = "AddWorkEffortCommEvent",
        location = "component://workeffort/widget/WorkEffortCommEventForms.xml",
        target = "createWorkEffortCommEvent",
        defaultMapName = "communicationEvent",
        extendsForm = "EditCommEvent",
        extendsResource = "component://party/widget/partymgr/CommunicationEventForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "communicationEventId", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        },
        actions = @FormActions(set = {@SetAction(field = "communicationEvent")}),
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortId"), @SortField(name = "communicationEventId"), @SortField(name = "description")})
    )
    public interface AddWorkEffortCommEvent {}

}
