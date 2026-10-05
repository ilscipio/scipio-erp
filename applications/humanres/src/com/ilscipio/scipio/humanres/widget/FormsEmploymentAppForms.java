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
public class FormsEmploymentAppForms {

    @Form(
        name = "ListEmploymentApps",
        location = "component://humanres/widget/forms/EmploymentAppForms.xml",
        type = FormType.MULTI,
        target = "updateEmploymentAppExt?partyId=${partyId}&&referredByPartyId=${partyId}",
        listName = "listIt",
        paginateTarget = "FindEmploymentApps",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "approverPartyId", hidden = @HiddenField),
            @FormField(name = "applicationId", title = "${uiLabelMap.HumanResApplicationId}", hyperlink = @HyperlinkField(target = "ViewEmploymentApp", description = "${applicationId}", parameters = {@ParameterDef(paramName = "applicationId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.FormFieldTitle_emplPositionId}", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "jobRequisitionId", title = "${uiLabelMap.FormFieldTitle_jobRequisitionId}", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "employmentAppSourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmploymentAppSourceType", description = "${employmentAppSourceTypeId}", keyFieldName = "employmentAppSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "employmentAppSourceTypeId")}))),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "applyingPartyId", title = "${uiLabelMap.FormFieldTitle_applyingPartyId}", hyperlink = @HyperlinkField(target = "EmployeeProfile", description = "${applyingPartyId}", parameters = {@ParameterDef(paramName = "partyId", fromField = "applyingPartyId")})),
            @FormField(name = "referredByPartyId", title = "${uiLabelMap.FormFieldTitle_referredByPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "applicationDate", title = "${uiLabelMap.FormFieldTitle_applicationDate}", dateTime = @DateTimeField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmploymentApp", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "applicationId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "referredByPartyId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee==null", target = "updateEmploymentApp")
        },
        actions = @FormActions(set = {@SetAction(field = "partyId", fromField = "parameters.partyId"), @SetAction(field = "referredByPartyId", fromField = "parameters.partyId"), @SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "employmentAppCtx"), @FieldMap(fieldName = "entityName", value = "EmploymentApp"), @FieldMap(fieldName = "orderBy", value = "applicationId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}, entityOne = {@EntityOneAction(entityName = "EmploymentApp")}),
        rowActions = @RowActions(set = {@SetAction(field = "applicationId", fromField = "applicationId")})
    )
    public interface ListEmploymentApps {}

    @Form(
        name = "FindEmploymentApps",
        location = "component://humanres/widget/forms/EmploymentAppForms.xml",
        target = "FindEmploymentApps",
        defaultMapName = "employmentApp",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "approverPartyId", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "applicationId", lookup = @LookupField(targetFormName = "LookupEmploymentApp")),
            @FormField(name = "emplPositionId", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "jobRequisitionId", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "applyingPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "referredByPartyId", useWhen = "referredByPartyId==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "employmentAppSourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmploymentAppSourceType", description = "${description}", keyFieldName = "employmentAppSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "applicationDate", title = "${uiLabelMap.FormFieldTitle_applicationDate}", dateFind = @DateFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "referredByPartyId", fromField = "parameters.partyId")})
    )
    public interface FindEmploymentApps {}

    @Form(
        name = "AddEmploymentApp",
        location = "component://humanres/widget/forms/EmploymentAppForms.xml",
        target = "createEmploymentApp",
        defaultEntityName = "EmploymentApp",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "approverPartyId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${referredByPartyId}")),
            @FormField(name = "applicationId", tooltip = "${uiLabelMap.CommonLeaveEmptyAutoValue}", text = @TextField),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.FormFieldTitle_emplPositionId} **", tooltip = "${uiLabelMap.HumanResEitherPositionOrRequisitionMustBeSpecified}", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "jobRequisitionId", title = "${uiLabelMap.FormFieldTitle_jobRequisitionId} **", tooltip = "${uiLabelMap.HumanResEitherPositionOrRequisitionMustBeSpecified}", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "applyingPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "referredByPartyId", useWhen = "employmentApp==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "referredByPartyId", useWhen = "employmentApp!=null", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmploymentAppSourceType", description = "${description}", keyFieldName = "employmentAppSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "employmentAppSourceTypeId")}))),
            @FormField(name = "applicationDate", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee != null", target = "createEmploymentAppExt")
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")})
    )
    public interface AddEmploymentApp {}

    @Form(
        name = "EditEmploymentApp",
        location = "component://humanres/widget/forms/EmploymentAppForms.xml",
        target = "updateEmploymentAppSingle",
        defaultMapName = "employmentApp",
        defaultEntityName = "EmploymentApp",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "approverPartyId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${referredByPartyId}")),
            @FormField(name = "applicationId", display = @DisplayField),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.FormFieldTitle_emplPositionId} **", tooltip = "${uiLabelMap.HumanResEitherPositionOrRequisitionMustBeSpecified}", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "jobRequisitionId", title = "${uiLabelMap.FormFieldTitle_jobRequisitionId} **", tooltip = "${uiLabelMap.HumanResEitherPositionOrRequisitionMustBeSpecified}", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "applyingPartyId", useWhen = "employmentApp==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "applyingPartyId", useWhen = "employmentApp!=null", requiredField = true, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile", description = "${employmentApp.applyingPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "employmentApp.applyingPartyId")}))),
            @FormField(name = "referredByPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "employmentAppSourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmploymentAppSourceType", description = "${description}", keyFieldName = "employmentAppSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "employmentAppSourceTypeId")}))),
            @FormField(name = "applicationDate", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee != null", target = "updateEmploymentAppExtSingle")
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")})
    )
    public interface EditEmploymentApp {}

    @Form(
        name = "ViewEmploymentApp",
        location = "component://humanres/widget/forms/EmploymentAppForms.xml",
        defaultMapName = "employmentApp",
        defaultEntityName = "EmploymentApp",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "applicationId", display = @DisplayField),
            @FormField(name = "emplPositionId", useWhen = "emplPositionId!=null", hyperlink = @HyperlinkField(target = "emplPositionView", description = "${employmentApp.emplPositionId}", parameters = {@ParameterDef(paramName = "emplPositionId", fromField = "employmentApp.emplPositionId")})),
            @FormField(name = "jobRequisitionId", useWhen = "jobRequisitionId!=null", hyperlink = @HyperlinkField(target = "EditJobRequisition", description = "${employmentApp.jobRequisitionId}", parameters = {@ParameterDef(paramName = "jobRequisitionId", fromField = "employmentApp.jobRequisitionId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "applyingPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile", description = "${employmentApp.applyingPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "employmentApp.applyingPartyId")}))),
            @FormField(name = "referredByPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${employmentApp.referredByPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "employmentApp.referredByPartyId")}))),
            @FormField(name = "employmentAppSourceTypeId", displayEntity = @DisplayEntityField(entityName = "EmploymentAppSourceType", description = "${description}")),
            @FormField(name = "applicationDate", display = @DisplayField(defaultValue = "${nowTimestamp}", type = "date-time"))
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")})
    )
    public interface ViewEmploymentApp {}

}
