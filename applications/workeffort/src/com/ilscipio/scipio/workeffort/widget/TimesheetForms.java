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
public class TimesheetForms {

    @Form(
        name = "ListMyTimesheets",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        listName = "timesheets",
        paginate = "true",
        paginateTarget = "MyTimesheets",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Timesheet", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditTimesheet", description = "${timesheetId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "timesheetId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "clientPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} [${partyId}]"))
        }
    )
    public interface ListMyTimesheets {}

    @Form(
        name = "ListMyRates",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        listName = "partyRates",
        paginate = "false",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyRate", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", displayEntity = @DisplayEntityField(entityName = "RateType")),
            @FormField(name = "partyId", hidden = @HiddenField)
        }
    )
    public interface ListMyRates {}

    @Form(
        name = "QuickCreateTimeEntry",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "createQuickTimeEntry",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "timesheetId", mapName = "currentTimesheet", hidden = @HiddenField),
            @FormField(name = "partyId", mapName = "userLogin", hidden = @HiddenField),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "hours", text = @TextField(size = 5)),
            @FormField(name = "comments", text = @TextField(size = 40)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface QuickCreateTimeEntry {}

    @Form(
        name = "FindTimesheet",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "FindTimesheet",
        defaultMapName = "timesheet",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Timesheet", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindTimesheet {}

    @Form(
        name = "ListFindTimesheet",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "true",
        paginateTarget = "FindTimesheet",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Timesheet", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditTimesheet", description = "${timesheetId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "timesheetId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")})))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Timesheet"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFindTimesheet {}

    @Form(
        name = "EditTimesheet",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "updateTimesheet",
        defaultMapName = "timesheet",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTimesheet")
        },
        fields = {
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "timesheet!=null", display = @DisplayField),
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", useWhen = "timesheet==null&&timesheetId==null", ignored = @IgnoredField),
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${timesheetId}]", useWhen = "timesheet==null&&timesheetId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}*", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "clientPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}*", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "TIMESHEET_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "timesheet==null", target = "createTimesheet")
        }
    )
    public interface EditTimesheet {}

    @Form(
        name = "DisplayTimesheetEntries",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        listName = "timesheetEntries",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "TimeEntry", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "timeEntryId", hidden = @HiddenField),
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}"),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", displayEntity = @DisplayEntityField(entityName = "RateType")),
            @FormField(name = "workEffortId", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "WorkEffortSummary", description = "${workEffortId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId")}))),
            @FormField(name = "invoiceId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/accounting/control/invoiceOverview", urlMode = UrlMode.INTER_APP, description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")}))
        }
    )
    public interface DisplayTimesheetEntries {}

    @Form(
        name = "AddTimesheetToInvoice",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "addTimesheetToInvoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "invoiceId", lookup = @LookupField(targetFormName = "LookupInvoice")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PageTitleAddWorkEffortTimeToInvoice}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTimesheetToInvoice {}

    @Form(
        name = "AddTimesheetToNewInvoice",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "addTimesheetToNewInvoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.WorkEffortTimeBillFromParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.WorkEffortTimeBillToParty}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${timesheet.clientPartyId}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PageTitleAddWorkEffortTimeToNewInvoice}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTimesheetToNewInvoice {}

    @Form(
        name = "ListTimesheetRoles",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        listName = "timesheetRoles",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRole}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTimesheetRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "timesheetId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListTimesheetRoles {}

    @Form(
        name = "AddTimesheetRole",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "createTimesheetRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTimesheetRole")
        },
        fields = {
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRole}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTimesheetRole {}

    @Form(
        name = "ListTimesheetEntries",
        location = "component://workeffort/widget/TimesheetForms.xml",
        type = FormType.LIST,
        target = "updateTimesheetEntry",
        listName = "timesheetEntries",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTimeEntry")
        },
        fields = {
            @FormField(name = "timeEntryId", hidden = @HiddenField),
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 12, defaultValue = "${timesheet.partyId}")),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 10)),
            @FormField(name = "invoiceId", ignored = @IgnoredField),
            @FormField(name = "invoiceItemSeqId", ignored = @IgnoredField),
            @FormField(name = "comments", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTimesheetEntry", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "timesheetId"), @ParameterDef(paramName = "timeEntryId")}))
        }
    )
    public interface ListTimesheetEntries {}

    @Form(
        name = "AddTimesheetEntry",
        location = "component://workeffort/widget/TimesheetForms.xml",
        target = "createTimesheetEntry",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTimeEntry")
        },
        fields = {
            @FormField(name = "timeEntryId", ignored = @IgnoredField),
            @FormField(name = "timesheetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${timesheet.partyId}")),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "invoiceId", ignored = @IgnoredField),
            @FormField(name = "invoiceItemSeqId", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTimesheetEntry {}

}
