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
public class CalendarForms {

    @Form(
        name = "FilterCalendarEvents",
        location = "component://workeffort/widget/CalendarForms.xml",
        target = "calendar",
        fields = {
            @FormField(name = "calendarType", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CALENDAR_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName", size = 16)),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.PartyEventType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "facilityId", title = "${uiLabelMap.Facility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAsset}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetName}", orderBy = {@EntityOrderBy(fieldName = "fixedAssetId")}))),
            @FormField(name = "hideEvents", check = @CheckField),
            @FormField(name = "viewAction", title = "${uiLabelMap.CommonView}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField),
            @FormField(name = "form", hidden = @HiddenField(value = "${parameters.form}")),
            @FormField(name = "period", hidden = @HiddenField(value = "${parameters.period}"))
        }
    )
    public interface FilterCalendarEvents {}

    @Form(
        name = "EditCalendar",
        location = "component://workeffort/widget/CalendarForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "initialView", dropDown = @DropDownField(options = {@Option(key = "day", description = "${uiLabelMap.WorkEffortDayView}"), @Option(key = "week", description = "${uiLabelMap.WorkEffortWeekView}"), @Option(key = "month", description = "${uiLabelMap.WorkEffortMonthView}")})),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCalendar {}

    @Form(
        name = "editCalEvent",
        location = "component://workeffort/widget/CalendarForms.xml",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortId", useWhen = "workEffort!=null", hidden = @HiddenField),
            @FormField(name = "start", hidden = @HiddenField(value = "${parameters.start}")),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.userLogin.partyId}")),
            @FormField(name = "fixedAssetId", hidden = @HiddenField(value = "${parameters.fixedAssetId}")),
            @FormField(name = "roleTypeId", useWhen = "workEffort==null", hidden = @HiddenField(value = "CAL_OWNER")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "workEffort==null", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.WorkEffortEventName}", requiredField = true, text = @TextField),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", useWhen = "statusItemList.size() > 1", dropDown = @DropDownField(listOptions = @ListOptions(listName = "statusItemList", keyName = "statusId", description = "${description}"))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", useWhen = "statusItemList.size() <= 1", display = @DisplayField(description = "${currentStatusItem.description}")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.PartyEventType}", useWhen = "parentTypeId!=void", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", envName = "parentTypeId")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.PartyEventType}", useWhen = "parentTypeId==void", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "scopeEnumId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORK_EFF_SCOPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "estimatedStartDate", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "estimatedCompletionDate", dateTime = @DateTimeField),
            @FormField(name = "actualStartDate", useWhen = "workEffort!=null", dateTime = @DateTimeField),
            @FormField(name = "actualCompletionDate", useWhen = "workEffort!=null", dateTime = @DateTimeField),
            @FormField(name = "addAction", useWhen = "workEffort==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", useWhen = "workEffort!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", useWhen = "calEventMayCancel", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", combinePrevious = true, hyperlink = @HyperlinkField(target = "javascript:document.getElementById('${cancelEventFormId}').submit();", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonCancel}")),
            @FormField(name = "form", hidden = @HiddenField(value = "${calEventFormAction}")),
            @FormField(name = "period", hidden = @HiddenField(value = "${calEventFormPeriod}")),
            @FormField(name = "calEventEdited", hidden = @HiddenField(value = "Y")),
            @FormField(name = "calViewParams_partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "calViewParams_fixedAssetId", hidden = @HiddenField(value = "${parameters.fixedAssetId}")),
            @FormField(name = "calViewParams_workEffortTypeId", hidden = @HiddenField(value = "${parameters.workEffortTypeId}")),
            @FormField(name = "calViewParams_calendarType", hidden = @HiddenField(value = "${parameters.calendarType}")),
            @FormField(name = "calViewParams_facilityId", hidden = @HiddenField(value = "${parameters.facilityId}")),
            @FormField(name = "calViewParams_hideEvents", hidden = @HiddenField(value = "${parameters.hideEvents}"))
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort==null", target = "${createCalEventUrl}"),
            @AltTarget(useWhen = "workEffort!=null", target = "${updateCalEventUrl}")
        },
        actions = @FormActions(set = {@SetAction(field = "statusTypeIds[]", value = "EVENT_STATUS"), @SetAction(field = "statusTypeIds[]", value = "CALENDAR_STATUS"), @SetAction(field = "statusTypeIds[]", value = "TASK_STATUS"), @SetAction(field = "createCalEventUrl", fromField = "createCalEventUrl", defaultValue = "createWorkEffortAndPartyAssign"), @SetAction(field = "updateCalEventUrl", fromField = "updateCalEventUrl", defaultValue = "updateWorkEffort")}, script = {@ScriptAction(location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/EditCalEventForm.groovy")})
    )
    public interface editCalEvent {}

    @Form(
        name = "cancelEvent",
        location = "component://workeffort/widget/CalendarForms.xml",
        target = "updateWorkEffort",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_CANCELLED")),
            @FormField(name = "cancel", title = "${uiLabelMap.WorkEffortCancelCalendarEvent}", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", submit = @SubmitField),
            @FormField(name = "form", hidden = @HiddenField(value = "${calEventFormAction}")),
            @FormField(name = "period", hidden = @HiddenField(value = "${calEventFormPeriod}")),
            @FormField(name = "calEventEdited", hidden = @HiddenField(value = "Y")),
            @FormField(name = "calViewParams_partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "calViewParams_fixedAssetId", hidden = @HiddenField(value = "${parameters.fixedAssetId}")),
            @FormField(name = "calViewParams_workEffortTypeId", hidden = @HiddenField(value = "${parameters.workEffortTypeId}")),
            @FormField(name = "calViewParams_calendarType", hidden = @HiddenField(value = "${parameters.calendarType}")),
            @FormField(name = "calViewParams_facilityId", hidden = @HiddenField(value = "${parameters.facilityId}")),
            @FormField(name = "calViewParams_hideEvents", hidden = @HiddenField(value = "${parameters.hideEvents}"))
        }
    )
    public interface cancelEvent {}

    @Form(
        name = "cancelEventHidden",
        location = "component://workeffort/widget/CalendarForms.xml",
        id = "${cancelEventFormId}",
        extendsForm = "cancelEvent",
        fields = {
            @FormField(name = "cancel", ignored = @IgnoredField)
        }
    )
    public interface cancelEventHidden {}

    @Form(
        name = "showCalEvent",
        location = "component://workeffort/widget/CalendarForms.xml",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortName", title = "${uiLabelMap.WorkEffortEventName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.PartyEventType}", displayEntity = @DisplayEntityField(entityName = "WorkEffortType", description = "${description}")),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "scopeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "estimatedStartDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "estimatedCompletionDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "actualStartDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "actualCompletionDate", display = @DisplayField(type = "date-time"))
        }
    )
    public interface showCalEvent {}

    @Form(
        name = "showCalEventRoles",
        location = "component://workeffort/widget/CalendarForms.xml",
        type = FormType.LIST,
        listName = "roles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "workEffort!=null", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}"))
        }
    )
    public interface showCalEventRoles {}

    @Form(
        name = "showCalEventRolesDel",
        location = "component://workeffort/widget/CalendarForms.xml",
        type = FormType.LIST,
        target = "deleteWorkEffortPartyAssign",
        extendsForm = "showCalEventRoles",
        fields = {
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface showCalEventRolesDel {}

    @Form(
        name = "addCalEventRole",
        location = "component://workeffort/widget/CalendarForms.xml",
        target = "createWorkEffortPartyAssign",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "partyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName", size = 10)),
            @FormField(name = "roleTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "CALENDAR_ROLE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "form", hidden = @HiddenField(value = "${calEventFormAction}")),
            @FormField(name = "period", hidden = @HiddenField(value = "${calEventFormPeriod}")),
            @FormField(name = "calEventEdited", hidden = @HiddenField(value = "Y")),
            @FormField(name = "calViewParams_partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "calViewParams_fixedAssetId", hidden = @HiddenField(value = "${parameters.fixedAssetId}")),
            @FormField(name = "calViewParams_workEffortTypeId", hidden = @HiddenField(value = "${parameters.workEffortTypeId}")),
            @FormField(name = "calViewParams_calendarType", hidden = @HiddenField(value = "${parameters.calendarType}")),
            @FormField(name = "calViewParams_facilityId", hidden = @HiddenField(value = "${parameters.facilityId}")),
            @FormField(name = "calViewParams_hideEvents", hidden = @HiddenField(value = "${parameters.hideEvents}"))
        }
    )
    public interface addCalEventRole {}

}
