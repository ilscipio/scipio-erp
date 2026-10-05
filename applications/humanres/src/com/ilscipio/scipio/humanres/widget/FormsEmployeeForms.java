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
public class FormsEmployeeForms {

    @Form(
        name = "NewEmployee",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        target = "createEmployee",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "salutation", title = "${uiLabelMap.CommonTitle}", text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "firstName", requiredField = true, text = @TextField),
            @FormField(name = "middleName", title = "${uiLabelMap.PartyMiddleInitial}", text = @TextField(size = 4, maxlength = 4)),
            @FormField(name = "lastName", requiredField = true, text = @TextField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.OrderOrderEntryInternalOrganization}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleNameDetail", description = "${groupName}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "permanentAddress", title = "${uiLabelMap.OrderAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "postalAddContactMechPurpTypeId", hidden = @HiddenField(value = "PRIMARY_LOCATION")),
            @FormField(name = "address1", title = "${uiLabelMap.CommonAddress1}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "address2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "city", title = "${uiLabelMap.CommonCity}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "postalCode", title = "${uiLabelMap.CommonZipPostalCode}", requiredField = true, text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "countryGeoId", title = "${uiLabelMap.CommonCountry}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName} - ${geoId}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoName")}))),
            @FormField(name = "stateProvinceGeoId", title = "${uiLabelMap.CommonState}", dropDown = @DropDownField(allowEmpty = true)),
            @FormField(name = "phoneTitle", title = "${uiLabelMap.PartyPrimaryPhone}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "countryCode", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "areaCode", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "contactNumber", title = "${uiLabelMap.PartyPhoneNumber}", requiredField = true, text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "extension", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "emailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface NewEmployee {}

    @Form(
        name = "AddEmployeeSkills",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        target = "createEmployeeSkill",
        defaultMapName = "partySkill",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "skillTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "yearsExperience", text = @TextField),
            @FormField(name = "rating", text = @TextField),
            @FormField(name = "skillLevel", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmployeeSkills {}

    @Form(
        name = "ListEmployeeSkills",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        type = FormType.LIST,
        target = "updateEmployeeSkill",
        listName = "listIt",
        paginateTarget = "FindPartySkills",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartySkill", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "skillTypeId", displayEntity = @DisplayEntityField(entityName = "SkillType", description = "${description}")),
            @FormField(name = "yearsExperience", text = @TextField),
            @FormField(name = "rating", text = @TextField),
            @FormField(name = "skillLevel", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmployeeSkill", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "skillTypeId"), @ParameterDef(paramName = "partyId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListEmployeeSkills {}

    @Form(
        name = "AddEmployeeQualification",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        target = "createEmployeeQualification",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyQual")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "partyQualTypeId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyQualType", description = "${description}", keyFieldName = "partyQualTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_INV_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "verifStatusId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTYQUAL_VERIFY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmployeeQualification {}

    @Form(
        name = "ListEmployeeQualification",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        type = FormType.LIST,
        target = "updateEmployeeQualification",
        listName = "listIt",
        paginateTarget = "FindPartyQuals",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyQual")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "partyQualTypeId", displayEntity = @DisplayEntityField(entityName = "PartyQualType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "qualificationDesc", text = @TextField),
            @FormField(name = "title", text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_INV_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "verifStatusId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTYQUAL_VERIFY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmployeeQualification", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "partyQualTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface ListEmployeeQualification {}

    @Form(
        name = "AddEmployeeTraining",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        target = "createEmployeeTraining",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "trainingClassTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TrainingClassType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmployeeTraining {}

    @Form(
        name = "AddEmplLeave",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        target = "createEmplLeave",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeave", mapName = "leaveApp")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "leaveTypeId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveType", description = "${description}", keyFieldName = "leaveTypeId"))),
            @FormField(name = "emplLeaveReasonTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplLeaveReasonType", description = "${description}", keyFieldName = "emplLeaveReasonTypeId"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "leaveStatus", hidden = @HiddenField(value = "LEAVE_CREATED")),
            @FormField(name = "approverPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmplLeave {}

    @Form(
        name = "ListEmplLeaves",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        type = FormType.LIST,
        target = "updateEmplLeave",
        listName = "listIt",
        paginateTarget = "FindEmplLeaves",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeave")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "approverPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "leaveStatus", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "emplLeaveReasonTypeId", display = @DisplayField),
            @FormField(name = "leaveTypeId", displayEntity = @DisplayEntityField(entityName = "EmplLeaveType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListEmplLeaves {}

    @Form(
        name = "CurrentEmploymentData",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        defaultMapName = "employmentData",
        fields = {
            @FormField(name = "company", entryName = "employment.partyIdFrom", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${employmentData.employment.partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "employmentData.employment.partyIdFrom")}))),
            @FormField(name = "position", entryName = "emplPositionType.emplPositionTypeId", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "emplPositionView", description = "${employmentData.emplPositionType.description} [${employmentData.emplPosition.emplPositionId}]", parameters = {@ParameterDef(paramName = "emplPositionId", fromField = "employmentData.emplPosition.emplPositionId")})),
            @FormField(name = "salary", entryName = "emplPositionRateAmount.rateAmount", display = @DisplayField(type = "currency"))
        }
    )
    public interface CurrentEmploymentData {}

    @Form(
        name = "PayrollHistoryList",
        location = "component://humanres/widget/forms/EmployeeForms.xml",
        type = FormType.LIST,
        listName = "payroll",
        extendsForm = "ListInvoices",
        extendsResource = "component://accounting/widget/invoice/InvoiceForms.xml",
        paginateTarget = "PayrollHistory",
        fields = {
            @FormField(name = "invoiceId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "/accounting/control/invoiceOverview", urlMode = UrlMode.INTER_APP, description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")}))
        }
    )
    public interface PayrollHistoryList {}

}
