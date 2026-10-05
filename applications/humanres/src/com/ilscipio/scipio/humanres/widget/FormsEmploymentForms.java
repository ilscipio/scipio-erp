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
public class FormsEmploymentForms {

    @Form(
        name = "FindEmployments",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "FindEmployments",
        oddRowStyle = "header-row",
        fields = {
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField(value = "INTERNAL_ORGANIZATIO")),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField(value = "EMPLOYEE")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", tooltip = "${uiLabelMap.HumanResEmploymentPartyIdFromToolTip}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName}", keyFieldName = "partyId", orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "firstName", textFind = @TextFindField),
            @FormField(name = "lastName", position = 2, textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField),
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", lookup = @LookupField(targetFormName = "LookupTerminationReason")),
            @FormField(name = "terminationTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TerminationType", description = "${description}", keyFieldName = "terminationTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindEmployments {}

    @Form(
        name = "ListEmploymentsPerson",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "EmploymentAndPerson",
        paginateTarget = "FindEmployments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", display = @DisplayField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EmployeeProfile", description = "${partyIdTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")})),
            @FormField(name = "firstName", display = @DisplayField),
            @FormField(name = "lastName", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", display = @DisplayField),
            @FormField(name = "terminationTypeId", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditEmployment", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "roleTypeIdTo")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "employmentCtx"), @FieldMap(fieldName = "entityName", value = "EmploymentAndPerson"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmploymentsPerson {}

    @Form(
        name = "ListEmployments",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "EmploymentAndPerson",
        paginateTarget = "FindEmployments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", display = @DisplayField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyIdTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", display = @DisplayField),
            @FormField(name = "terminationTypeId", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditEmployment", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "roleTypeIdTo")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "employmentCtx"), @FieldMap(fieldName = "entityName", value = "Employment"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmployments {}

    @Form(
        name = "EditEmployment",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "updateEmployment",
        defaultMapName = "employment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmployment", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField(value = "INTERNAL_ORGANIZATIO")),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField(value = "EMPLOYEE")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", useWhen = "employment==null", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", useWhen = "employment!=null", hidden = @HiddenField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", useWhen = "employment==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", useWhen = "employment!=null", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "employment==null", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "employment!=null", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", useWhen = "employment!=null", position = 2, dateTime = @DateTimeField),
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", useWhen = "employment!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TerminationReason", description = "${description}", keyFieldName = "terminationReasonId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "terminationTypeId", useWhen = "employment!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TerminationType", description = "${description}", keyFieldName = "terminationTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "employment==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "employment!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "employment==null", target = "createEmployment")
        }
    )
    public interface EditEmployment {}

    @Form(
        name = "ListPayHistories",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.LIST,
        target = "updatePayHistory",
        paginateTarget = "findPayHistories",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePayHistory")
        },
        fields = {
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "amount", hidden = @HiddenField),
            @FormField(name = "comments", hidden = @HiddenField),
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResSalaryStepSeqId}", lookup = @LookupField(targetFormName = "LookupSalaryStep", size = 20)),
            @FormField(name = "payGradeId", title = "${uiLabelMap.HumanResPayGradeID}", lookup = @LookupField(targetFormName = "LookupPayGrade", size = 20)),
            @FormField(name = "periodTypeId", title = "${uiLabelMap.FormFieldTitle_periodTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", keyFieldName = "periodTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePayHistory", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPayHistories {}

    @Form(
        name = "ListPartyBenefits",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.MULTI,
        target = "updatePartyBenefit?benefitTypeId=${benefitTypeId}&roleTypeIdFrom=${roleTypeIdFrom}&roleTypeIdTo=${roleTypeIdTo}&partyIdFrom=${partyIdFrom}&partyIdTo=${partyIdTo}&fromDate=${fromDate}",
        paginateTarget = "findPartyBenefits",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyBenefit")
        },
        fields = {
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "benefitTypeId", displayEntity = @DisplayEntityField(entityName = "BenefitType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "periodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", keyFieldName = "periodTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyBenefit", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "benefitTypeId"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyBenefits {}

    @Form(
        name = "AddPartyBenefit",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "createPartyBenefit",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyBenefit")
        },
        fields = {
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "benefitTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BenefitType", description = "${description}", keyFieldName = "benefitTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true),
            @FormField(name = "periodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", keyFieldName = "periodTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyBenefit {}

    @Form(
        name = "ListPayrollPreferences",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.MULTI,
        target = "updatePayrollPreference?partyId=${partyId}&roleTypeId=${roleTypeId}&payrollPreferenceSeqId=${payrollPreferenceSeqId}",
        paginateTarget = "findPayRollPreferences",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePayrollPreference", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "payrollPreferenceSeqId", title = "${uiLabelMap.HumanResPayrollPreferenceSeqId}", display = @DisplayField),
            @FormField(name = "paymentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", description = "${description}")),
            @FormField(name = "periodTypeId", displayEntity = @DisplayEntityField(entityName = "PeriodType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePayrollPreference", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "payrollPreferenceSeqId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListPayrollPreferences {}

    @Form(
        name = "AddPayrollPreference",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "createPayrollPreference",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPayrollPreference")
        },
        fields = {
            @FormField(name = "payrollPreferenceSeqId", title = "${uiLabelMap.HumanResPayrollPreferenceSeqId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPayRollPreference")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "deductionTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DeductionType", description = "${description}", keyFieldName = "deductionTypeId", orderBy = {@EntityOrderBy(fieldName = "deductionTypeId")}))),
            @FormField(name = "paymentMethodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", keyFieldName = "paymentMethodTypeId", orderBy = {@EntityOrderBy(fieldName = "paymentMethodTypeId")}))),
            @FormField(name = "periodTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", keyFieldName = "periodTypeId", orderBy = {@EntityOrderBy(fieldName = "periodTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPayrollPreference {}

    @Form(
        name = "ListPerformanceNotes",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.LIST,
        target = "updatePerformanceNote",
        paginateTarget = "findPerformanceNotes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePerformanceNote", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")})))
        }
    )
    public interface ListPerformanceNotes {}

    @Form(
        name = "AddPerformanceNote",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "createPerformanceNote",
        defaultMapName = "performanceNote",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPerformanceNote", mapName = "performanceNote")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPerformanceNote {}

    @Form(
        name = "ListUnemploymentClaims",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.MULTI,
        target = "updateUnemploymentClaim?unemploymentClaimId=${unemploymentClaimId}&roleTypeIdFrom=${roleTypeIdFrom}&roleTypeIdTo=${roleTypeIdTo}&partyIdFrom=${partyIdFrom}&partyIdTo=${partyIdTo}&fromDate=${fromDate}",
        paginateTarget = "FindUnemploymentClaim",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "unemploymentClaimId", display = @DisplayField),
            @FormField(name = "partyIdFrom", display = @DisplayField),
            @FormField(name = "partyIdTo", display = @DisplayField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "description", text = @TextField(size = 12)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteUnemploymentClaim", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "unemploymentClaimId"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListUnemploymentClaims {}

    @Form(
        name = "AddUnemploymentClaim",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "createUnemploymentClaim",
        defaultMapName = "unemploymentClaim",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createUnemploymentClaim")
        },
        fields = {
            @FormField(name = "unemploymentClaimId", requiredField = true, lookup = @LookupField(targetFormName = "LookupUnemploymentClaim")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", hidden = @HiddenField),
            @FormField(name = "partyIdTo", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddUnemploymentClaim {}

    @Form(
        name = "ListAgreementEmploymentAppls",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        type = FormType.MULTI,
        target = "updateAgreementEmploymentAppl?agreementId=${agreementId}&partyIdTo=${partyIdTo}&partyIdFrom=${partyIdFrom}&roleTypeIdFrom=${roleTypeIdFrom}&roleTypeIdTo=${roleTypeIdTo}&fromDate=${fromDate}&agreementItemSeqId=${agreementItemSeqId}",
        paginateTarget = "EditAgreementEmploymentAppls",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", display = @DisplayField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResPartyIdFrom}", display = @DisplayField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResPartyIdTo}", display = @DisplayField),
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "agreementDate", dateTime = @DateTimeField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAgreementEmploymentAppl", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListAgreementEmploymentAppls {}

    @Form(
        name = "AddAgreementEmploymentAppl",
        location = "component://humanres/widget/forms/EmploymentForms.xml",
        target = "createAgreementEmploymentAppl",
        defaultMapName = "agreementEmploymentAppl",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createAgreementEmploymentAppl", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupAgreement")),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupAgreementEmploymentAppl")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.HumanResPartyIdFrom}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.HumanResPartyIdTo}", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeIdFrom", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", position = 2, requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "agreementDate", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddAgreementEmploymentAppl {}

}
