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
public class FormsPersonTrainingForms {

    @Form(
        name = "showTrainingCalendar",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "applyTraining",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${loginPartyId}")),
            @FormField(name = "approverId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTraining} ${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "approvalStatus", hidden = @HiddenField(value = "TRAINING_APPLIED")),
            @FormField(name = "workEffortTypeId", hidden = @HiddenField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date-time")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface showTrainingCalendar {}

    @Form(
        name = "editTrainingCalendar",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "createTrainingCalendar",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortId", useWhen = "workEffort!=null", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.userLogin.partyId}")),
            @FormField(name = "roleTypeId", useWhen = "workEffort==null", hidden = @HiddenField(value = "CAL_OWNER")),
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "TRAINING")),
            @FormField(name = "statusId", useWhen = "workEffort==null", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_TENTATIVE")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.HumanResTrainings} ${uiLabelMap.CommonName}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrainingClassType", description = "${description}", keyFieldName = "trainingClassTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "scopeEnumId", hidden = @HiddenField(value = "WES_PUBLIC")),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort!=null", target = "updateTrainingCalendar")
        }
    )
    public interface editTrainingCalendar {}

    @Form(
        name = "AssignTraining",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "assignTraining",
        fields = {
            @FormField(name = "approverId", hidden = @HiddenField(value = "${loginPartyId}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", hidden = @HiddenField),
            @FormField(name = "trainingClassTypeId", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "approvalStatus", hidden = @HiddenField(value = "TRAINING_ASSIGNED")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName", size = 10)),
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "CAL_ATTENDEE")),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AssignTraining {}

    @Form(
        name = "ListTrainingParticipants",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}", widgetStyle = "${styles.link_nav_info_name_long}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "approvalStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "trainingRequestId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "trainingClassTypeId", displayEntity = @DisplayEntityField(entityName = "TrainingClassType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PersonTraining"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTrainingParticipants {}

    @Form(
        name = "FindTrainingApprovals",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "FindTrainingApprovals",
        defaultTitleStyle = "tableheadtext",
        defaultWidgetStyle = "inputBox",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TrainingClassType", description = "${description}", keyFieldName = "trainingClassTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approverId", useWhen = "!hasAdminPermission", hidden = @HiddenField(value = "${loginPartyId}")),
            @FormField(name = "reason", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindTrainingApprovals {}

    @Form(
        name = "ListTrainingApprovals",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        type = FormType.LIST,
        target = "updateTrainingStatus",
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}", widgetStyle = "${styles.link_nav_info_name_long}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "approverId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverId")}))),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}"),
            @FormField(name = "trainingRequestId", widgetStyle = "${styles.link_nav_info_id}"),
            @FormField(name = "UpdateStatus", title = "${uiLabelMap.CommonUpdate}", display = @DisplayField(description = "Update")),
            @FormField(name = "UpdateStatus", title = "${uiLabelMap.CommonUpdate}", useWhen = "(\"${approvalStatus}\".equals(\"TRAINING_APPLIED\"))||(\"${approvalStatus}\".equals(\"TRAINING_APPROVED\"))", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditTrainingApprovals", description = "${uiLabelMap.CommonUpdate}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "trainingClassTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "approvalStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "trainingClassTypeId", displayEntity = @DisplayEntityField(entityName = "TrainingClassType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PersonTraining"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTrainingApprovals {}

    @Form(
        name = "EditTrainingApprovals",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "updateTrainingStatus",
        defaultMapName = "personTraining",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTrainingStatus", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}"),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}"),
            @FormField(name = "approvalStatus", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "TRAINING_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approvalStatus", useWhen = "personTraining!=null&&personTraining.getString(\"approvalStatus\").equals(\"TRAINING_REJECTED\")", display = @DisplayField),
            @FormField(name = "reason", requiredField = true, text = @TextField),
            @FormField(name = "reason", useWhen = "personTraining!=null&&personTraining.getString(\"approvalStatus\").equals(\"TRAINING_REJECTED\")", display = @DisplayField),
            @FormField(name = "submitAction", title = "Update", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditTrainingApprovals {}

    @Form(
        name = "FindTrainingStatus",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        target = "FindTrainingStatus",
        defaultTitleStyle = "tableheadtext",
        defaultWidgetStyle = "inputBox",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${loginPartyId}")),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TrainingClassType", description = "${description}", keyFieldName = "trainingClassTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approverId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "approvalStatus", textFind = @TextFindField),
            @FormField(name = "reason", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSearch}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindTrainingStatus {}

    @Form(
        name = "ListTrainingStatus",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}", widgetStyle = "${styles.link_nav_info_id}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "approverId", widgetStyle = "${styles.link_nav_info_id}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverId")}))),
            @FormField(name = "trainingRequestId", widgetStyle = "${styles.link_nav_info_id}"),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}"),
            @FormField(name = "approvalStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "trainingClassTypeId", displayEntity = @DisplayEntityField(entityName = "TrainingClassType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PersonTraining"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTrainingStatus {}

    @Form(
        name = "simpleListTrainingStatus",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date-time")),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}"),
            @FormField(name = "approvalStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "trainingClassTypeId", displayEntity = @DisplayEntityField(entityName = "TrainingClassType")),
            @FormField(name = "approverId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PersonTraining"), @FieldMap(fieldName = "orderBy", value = "fromDate"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface simpleListTrainingStatus {}

    @Form(
        name = "ListEmplTrainings",
        location = "component://humanres/widget/forms/PersonTrainingForms.xml",
        type = FormType.LIST,
        target = "updateEmplLeave",
        listName = "listIt",
        paginateTarget = "FindEmplLeaves",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PersonTraining", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_employeePartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "approverId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "trainingRequestId"),
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}"),
            @FormField(name = "approvalStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "trainingClassTypeId", displayEntity = @DisplayEntityField(entityName = "TrainingClassType"))
        }
    )
    public interface ListEmplTrainings {}

}
