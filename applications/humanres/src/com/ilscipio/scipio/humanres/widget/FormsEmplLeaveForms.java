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
package com.ilscipio.scipio.humanres.widget;

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
public class FormsEmplLeaveForms {

    @Form(
        name = "FindEmplLeaves",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        target = "FindEmplLeaves",
        oddRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplLeave", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "leaveTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveType", description = "${description}", keyFieldName = "leaveTypeId"))),
            @FormField(name = "emplLeaveReasonTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveReasonType", description = "${description}", keyFieldName = "emplLeaveReasonTypeId"))),
            @FormField(name = "leaveStatus", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "LEAVE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approverPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "description", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindEmplLeaves {}

    @Form(
        name = "ListEmplLeaves",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindEmplLeaves",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplLeave", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "leaveTypeId", displayEntity = @DisplayEntityField(entityName = "EmplLeaveType")),
            @FormField(name = "emplLeaveReasonTypeId", displayEntity = @DisplayEntityField(entityName = "EmplLeaveReasonType")),
            @FormField(name = "approverPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverPartyId")}))),
            @FormField(name = "leaveStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "updateLeave", title = "${uiLabelMap.CommonEdit}", useWhen = "hasAdminPermission", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditEmplLeave", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "leaveTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", useWhen = "hasAdminPermission", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplLeave", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "leaveTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "description", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "EmplLeave")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmplLeaves {}

    @Form(
        name = "EditEmplLeave",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        target = "updateEmplLeaveExt",
        defaultMapName = "leaveApp",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeave", mapName = "leaveApp")
        },
        fields = {
            @FormField(name = "partyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "approverPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "leaveTypeId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveType", description = "${description}", keyFieldName = "leaveTypeId"))),
            @FormField(name = "emplLeaveReasonTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveReasonType", description = "${description}", keyFieldName = "emplLeaveReasonTypeId"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "leaveStatus", hidden = @HiddenField(value = "LEAVE_CREATED")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "leaveApp==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "leaveApp!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "leaveApp==null", target = "createEmplLeaveExt")
        }
    )
    public interface EditEmplLeave {}

    @Form(
        name = "FindLeaveApprovals",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        target = "FindLeaveApprovals",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplLeave", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "leaveStatus", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "LEAVE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", hidden = @HiddenField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindLeaveApprovals {}

    @Form(
        name = "ListLeaveApprovals",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplLeave", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", fieldName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "leaveTypeId", displayEntity = @DisplayEntityField(entityName = "EmplLeaveType")),
            @FormField(name = "emplLeaveReasonTypeId", displayEntity = @DisplayEntityField(entityName = "EmplLeaveReasonType")),
            @FormField(name = "approverPartyId", fieldName = "approverPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverPartyId")}))),
            @FormField(name = "leaveStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "UpdateStatus", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditEmplLeaveStatus", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "leaveTypeId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "EmplLeave")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName")})})
    )
    public interface ListLeaveApprovals {}

    @Form(
        name = "EditEmplLeaveStatus",
        location = "component://humanres/widget/forms/EmplLeaveForms.xml",
        target = "updateEmplLeaveStatus",
        defaultMapName = "leaveApp",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeaveStatus", mapName = "leaveApp")
        },
        fields = {
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "approverPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "leaveTypeId", display = @DisplayField),
            @FormField(name = "emplLeaveReasonTypeId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "leaveStatus", title = "${uiLabelMap.HumanResLeaveStatus}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "LEAVE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "leaveStatus", useWhen = "leaveApp!=null&&leaveApp.getString(\"leaveStatus\").equals(\"LEAVE_REJECTED\")", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditEmplLeaveStatus {}

}
