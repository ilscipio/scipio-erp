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
package com.ilscipio.scipio.marketing.widget;

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
public class FormsEventForms {

    @Form(
        name = "MyTasks",
        location = "component://marketing/widget/sfa/forms/EventForms.xml",
        listName = "myTasks",
        extendsForm = "ListWorkEfforts",
        extendsResource = "component://workeffort/widget/WorkEffortForms.xml",
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField),
            @FormField(name = "deleteAction", hidden = @HiddenField),
            @FormField(name = "currentStatusId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditEvent", description = "${workEffortName} [${workEffortId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "assignedByUserLoginId", title = "${uiLabelMap.SfaAssignedBy}", widgetStyle = "${styles.link_nav_info_idname_long} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${assignByPartyName.firstName} ${assignByPartyName.middleName} ${assignByPartyName.lastName} ${assignByPartyName.groupName} [${assignByPartyName.partyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "assignByPartyName.partyId")})),
            @FormField(name = "cancel", title = "${uiLabelMap.CommonCancel}", widgetStyle = "${styles.link_run_sys} ${styles.action_cancel}", hyperlink = @HyperlinkField(target = "updateEventWorkEffortReturn", description = "${uiLabelMap.CommonCancel}", linkType = "hidden-form", parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "currentStatusId", value = "CAL_CANCELLED")})),
            @FormField(name = "complete", title = "${uiLabelMap.SfaComplete}", widgetStyle = "${styles.link_run_sys} ${styles.action_complete}", hyperlink = @HyperlinkField(target = "updateEventWorkEffortReturn", description = "${uiLabelMap.SfaComplete}", linkType = "hidden-form", parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "currentStatusId", value = "CAL_COMPLETED")})),
            @FormField(name = "viewCalendarAction", title = "${uiLabelMap.CommonCalendar}", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "Calendar", description = "${uiLabelMap.CommonCalendar}", alsoHidden = false, parameters = {@ParameterDef(paramName = "period", value = "month"), @ParameterDef(paramName = "startDate", fromField = "targetPeriodStart")})),
            @FormField(name = "viewDetailedAction", title = "${uiLabelMap.WorkEffortWorkEffort}", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.WorkEffortWorkEffort}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")}))
        },
        rowActions = @RowActions(script = {@ScriptAction(location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/MyTasks_script1.groovy")}, entityOne = {@EntityOneAction(entityName = "UserLogin", valueField = "assignedByUserLogin"), @EntityOneAction(entityName = "PartyNameView", valueField = "assignByPartyName")})
    )
    public interface MyTasks {}

    @Form(
        name = "TasksAssignedByMe",
        location = "component://marketing/widget/sfa/forms/EventForms.xml",
        listName = "tasksAssignedByMe",
        extendsForm = "MyTasks",
        fields = {
            @FormField(name = "assignedByUserLoginId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_toPartyId}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${toPartyName.firstName} ${toPartyName.middleName} ${toPartyName.lastName} ${toPartyName.groupName} [${toPartyName.partyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "toPartyName.partyId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "toPartyName")}),
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortId"), @SortField(name = "workEffortPurposeTypeId"), @SortField(name = "description"), @SortField(name = "priority"), @SortField(name = "estimatedStartDate"), @SortField(name = "estimatedCompletionDate"), @SortField(name = "actualStartDate"), @SortField(name = "actualCompletionDate"), @SortField(name = "partyId"), @SortField(name = "cancel"), @SortField(name = "complete")})
    )
    public interface TasksAssignedByMe {}

    @Form(
        name = "EditEvent",
        location = "component://marketing/widget/sfa/forms/EventForms.xml",
        extendsForm = "editCalEvent",
        extendsResource = "component://workeffort/widget/CalendarForms.xml",
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "TASK")),
            @FormField(name = "statusId", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_ACCEPTED")),
            @FormField(name = "scopeEnumId", hidden = @HiddenField),
            @FormField(name = "actualStartDate", hidden = @HiddenField),
            @FormField(name = "actualCompletionDate", hidden = @HiddenField),
            @FormField(name = "priority", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "1", description = "${uiLabelMap.WorkEffortPriorityOne}"), @Option(key = "2", description = "${uiLabelMap.WorkEffortPriorityTwo}"), @Option(key = "3", description = "${uiLabelMap.WorkEffortPriorityThree}"), @Option(key = "4", description = "${uiLabelMap.WorkEffortPriorityFour}"), @Option(key = "5", description = "${uiLabelMap.WorkEffortPriorityFive}"), @Option(key = "6", description = "${uiLabelMap.WorkEffortPrioritySix}"), @Option(key = "7", description = "${uiLabelMap.WorkEffortPrioritySeventh}"), @Option(key = "8", description = "${uiLabelMap.WorkEffortPriorityEight}"), @Option(key = "9", description = "${uiLabelMap.WorkEffortPriorityNine}")})),
            @FormField(name = "estimatedStartDate", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "estimatedCompletionDate", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_toPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${userLogin.partyId}")),
            @FormField(name = "completeAction", useWhen = "workEffort!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_complete}", combinePrevious = true, hyperlink = @HyperlinkField(target = "javascript:document.getElementById('${completeEventFormId}').submit();", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.SfaComplete}"))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortId"), @SortField(name = "partyId"), @SortField(name = "fixedAssetId"), @SortField(name = "roleTypeId"), @SortField(name = "statusId"), @SortField(name = "workEffortTypeId"), @SortField(name = "currentStatusId"), @SortField(name = "scopeEnumId"), @SortField(name = "actualStartDate"), @SortField(name = "actualCompletionDate"), @SortField(name = "workEffortName"), @SortField(name = "description"), @SortField(name = "priority"), @SortField(name = "estimatedStartDate"), @SortField(name = "estimatedCompletionDate"), @SortField(name = "partyId"), @SortField(name = "addAction"), @SortField(name = "updateAction"), @SortField(name = "cancelAction"), @SortField(name = "completeAction")})
    )
    public interface EditEvent {}

    @Form(
        name = "cancelEventHidden",
        location = "component://marketing/widget/sfa/forms/EventForms.xml",
        target = "updateEventWorkEffortReturn",
        id = "${cancelEventFormId}",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_CANCELLED"))
        }
    )
    public interface cancelEventHidden {}

    @Form(
        name = "completeEventHidden",
        location = "component://marketing/widget/sfa/forms/EventForms.xml",
        target = "updateEventWorkEffortReturn",
        id = "${completeEventFormId}",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_COMPLETED"))
        }
    )
    public interface completeEventHidden {}

}
