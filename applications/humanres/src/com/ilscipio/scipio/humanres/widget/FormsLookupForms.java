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
public class FormsLookupForms {

    @Form(
        name = "LookupBudget",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupBudget",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Budget", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "budgetId", title = "${uiLabelMap.HumanResBudgetID}", textFind = @TextFindField),
            @FormField(name = "budgetTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "BudgetType", description = "${description}", keyFieldName = "budgetTypeId"))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupBudget {}

    @Form(
        name = "ListBudgets",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupBudget",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Budget", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "budgetId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${budgetId}')", urlMode = UrlMode.PLAIN, description = "${budgetId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Budget"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListBudgets {}

    @Form(
        name = "LookupBudgetItem",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupBudgetItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "BudgetItem", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "budgetItemSeqId", title = "${uiLabelMap.HumanResBudgetItemSeqId}", textFind = @TextFindField),
            @FormField(name = "budgetItemTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "BudgetItemType", description = "${description}", keyFieldName = "budgetItemTypeId"))),
            @FormField(name = "budgetId", lookup = @LookupField(targetFormName = "LookupBudget")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupBudgetItem {}

    @Form(
        name = "ListBudgetItems",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupBudgetItem",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "BudgetItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "budgetItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${budgetItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${budgetItemSeqId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "BudgetItem"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListBudgetItems {}

    @Form(
        name = "LookupEmplPosition",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupEmplPosition",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplPosition", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmplPositionId}", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId"))),
            @FormField(name = "emplPositionTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplPositionType", description = "${description}", keyFieldName = "emplPositionTypeId"))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "budgetId", lookup = @LookupField(targetFormName = "LookupBudget")),
            @FormField(name = "budgetItemSeqId", lookup = @LookupField(targetFormName = "LookupBudgetItem")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupEmplPosition {}

    @Form(
        name = "ListEmplPositions",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupEmplPosition",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "emplPositionId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${emplPositionId}')", urlMode = UrlMode.PLAIN, description = "${emplPositionId}", alsoHidden = false)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", display = @DisplayField),
            @FormField(name = "budgetId", display = @DisplayField),
            @FormField(name = "budgetItemSeqId", display = @DisplayField),
            @FormField(name = "emplPositionTypeId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "EmplPosition"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmplPositions {}

    @Form(
        name = "LookupTerminationReason",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupTerminationReason",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "TerminationReason", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupTerminationReason {}

    @Form(
        name = "ListTerminationReasons",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupEmplPosition",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "terminationReasonId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${terminationReasonId}')", urlMode = UrlMode.PLAIN, description = "${terminationReasonId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "TerminationReason"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListTerminationReasons {}

    @Form(
        name = "LookupSalaryStep",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupSalaryStep",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalaryStep", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResLookupSalaryStepSeqId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupSalaryStep {}

    @Form(
        name = "ListSalarySteps",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSalaryStep",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResLookupSalaryStepSeqId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${salaryStepSeqId}')", urlMode = UrlMode.PLAIN, description = "${salaryStepSeqId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SalaryStep"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListSalarySteps {}

    @Form(
        name = "LookupPayGrade",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupPayGrade",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalaryStep", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "payGradeId", title = "${uiLabelMap.HumanResLookupPayGradeId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPayGrade {}

    @Form(
        name = "ListPayGrades",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPayGrade",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "payGradeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${payGradeId}')", urlMode = UrlMode.PLAIN, description = "${payGradeId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PayGrade"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPayGrades {}

    @Form(
        name = "LookupPayRollPreference",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupPayRollPreference",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PayrollPreference", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "payrollPreferenceSeqId", title = "${uiLabelMap.HumanResLookupPayrollPreferenceSeqId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPayRollPreference {}

    @Form(
        name = "ListPayRollPreferences",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPayRollPreference",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "payrollPreferenceSeqId", title = "${uiLabelMap.HumanResLookupPayrollPreferenceSeqId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${payrollPreferenceSeqId}')", urlMode = UrlMode.PLAIN, description = "${payrollPreferenceSeqId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PayrollPreference"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPayRollPreferences {}

    @Form(
        name = "LookupUnemploymentClaim",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupUnemploymentClaim",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "UnemploymentClaim", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "unemploymentClaimId", title = "${uiLabelMap.HumanResLookupUnemploymentClaimId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupUnemploymentClaim {}

    @Form(
        name = "ListUnemploymentClaims",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupUnemploymentClaim",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "unemploymentClaimId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${unemploymentClaimId}')", urlMode = UrlMode.PLAIN, description = "${unemploymentClaimId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "UnemploymentClaim"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListUnemploymentClaims {}

    @Form(
        name = "LookupAgreementEmploymentAppl",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupAgreementEmploymentAppl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementEmploymentAppl", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.HumanResLookupAgreementItemSeqId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupAgreementEmploymentAppl {}

    @Form(
        name = "ListAgreementEmploymentAppls",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupAgreementEmploymentAppl",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${agreementItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${agreementItemSeqId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "AgreementEmploymentAppl"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListAgreementEmploymentAppls {}

    @Form(
        name = "LookupPerfReview",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupPerfReview",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PerfReview", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "perfReviewId", title = "${uiLabelMap.HumanResLookupPerfReviewId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPerfReview {}

    @Form(
        name = "ListPerfReviews",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPerfReview",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "perfReviewId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${perfReviewId}')", urlMode = UrlMode.PLAIN, description = "${perfReviewId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PerfReview"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPerfReviews {}

    @Form(
        name = "LookupPartyResume",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupPartyResume",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyResume", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "resumeId", title = "${uiLabelMap.HumanResLookupPartyResume}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPartyResume {}

    @Form(
        name = "ListPartyResumes",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPartyResume",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "resumeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${resumeId}')", urlMode = UrlMode.PLAIN, description = "${resumeId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyResume"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPartyResumes {}

    @Form(
        name = "LookupEmploymentApp",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupEmploymentApp",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmploymentApp", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "applicationId", title = "${uiLabelMap.HumanResLookupApplicationId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupEmploymentApp {}

    @Form(
        name = "ListEmploymentApps",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupEmploymentApp",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "applicationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${applicationId}')", urlMode = UrlMode.PLAIN, description = "${applicationId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "EmploymentApp"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmploymentApps {}

    @Form(
        name = "LookupJobRequisition",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupJobRequisition",
        fields = {
            @FormField(name = "jobRequisitionId", textFind = @TextFindField),
            @FormField(name = "qualification", textFind = @TextFindField),
            @FormField(name = "location", textFind = @TextFindField),
            @FormField(name = "skillTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobPostingTypeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "JOB_POSTING")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupJobRequisition {}

    @Form(
        name = "ListLookupJobRequisition",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupJobRequisition",
        fields = {
            @FormField(name = "jobRequisitionId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${jobRequisitionId}')", urlMode = UrlMode.PLAIN, description = "${jobRequisitionId}", alsoHidden = false)),
            @FormField(name = "jobPostingTypeEnumId", display = @DisplayField),
            @FormField(name = "qualification", display = @DisplayField),
            @FormField(name = "skillTypeId", display = @DisplayField),
            @FormField(name = "location", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "JobRequisition"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupJobRequisition {}

    @Form(
        name = "LookupTraining",
        location = "component://humanres/widget/forms/LookupForms.xml",
        target = "LookupTraining",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", textFind = @TextFindField),
            @FormField(name = "parentTypeId", title = "${uiLabelMap.HumanResPreRequisiteSkill}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupTraining {}

    @Form(
        name = "ListLookupTraining",
        location = "component://humanres/widget/forms/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${trainingClassTypeId}')", urlMode = UrlMode.PLAIN, description = "${trainingClassTypeId}", alsoHidden = false)),
            @FormField(name = "parentTypeId", title = "${uiLabelMap.HumanResPreRequisiteSkill}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "TrainingClassType"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupTraining {}

}
