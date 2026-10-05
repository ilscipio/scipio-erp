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
public class FormsGlobalHRSettingForms {

    @Form(
        name = "ListSkillTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateSkillType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSkillType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "skillTypeId", title = "${uiLabelMap.HumanResSkillTypeId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSkillType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "skillTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListSkillTypes {}

    @Form(
        name = "AddSkillType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createSkillType",
        defaultMapName = "skillType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSkillType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "skillTypeId", title = "${uiLabelMap.HumanResSkillTypeId}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddSkillType {}

    @Form(
        name = "ListResponsibilityTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateResponsibilityType",
        listName = "responsibilityTypes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateResponsibilityType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "responsibilityTypeId", title = "${uiLabelMap.HumanResResponsibilityTypeId}", display = @DisplayField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteResponsibilityType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "responsibilityTypeId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListResponsibilityTypes {}

    @Form(
        name = "AddResponsibilityType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createResponsibilityType",
        defaultMapName = "responsibilityType",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createResponsibilityType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "responsibilityTypeId", title = "${uiLabelMap.HumanResResponsibilityTypeId}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddResponsibilityType {}

    @Form(
        name = "ListTerminationTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateTerminationType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTerminationType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "terminationTypeId", title = "${uiLabelMap.HumanResTerminationTypeId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTerminationType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "terminationTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListTerminationTypes {}

    @Form(
        name = "AddTerminationType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createTerminationType",
        defaultMapName = "TerminationType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTerminationType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "terminationTypeId", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddTerminationType {}

    @Form(
        name = "FindEmplPositionTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplPositionType", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "emplPositionTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "EmplPositionType", description = "${description}", keyFieldName = "emplPositionTypeId"))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindEmplPositionTypes {}

    @Form(
        name = "ListEmplPositionTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.LIST,
        target = "updateEmplPositionType",
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "emplPositionTypeId", title = "${uiLabelMap.HumanResEmplPositionType}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditEmplPositionTypes", description = "${emplPositionTypeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionTypeId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplPositionType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionTypeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "emplPositionTypeCtx"), @FieldMap(fieldName = "entityName", value = "EmplPositionType"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmplPositionTypes {}

    @Form(
        name = "EditEmplPositionTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "updateEmplPositionType",
        defaultMapName = "emplPositionType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionType")
        },
        fields = {
            @FormField(name = "emplPositionTypeId", useWhen = "emplPositionType==null", text = @TextField),
            @FormField(name = "emplPositionTypeId", useWhen = "emplPositionType!=null", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "emplPositionType!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "emplPositionType==null", target = "createEmplPositionType")
        }
    )
    public interface EditEmplPositionTypes {}

    @Form(
        name = "ListEmplPositionTypeRates",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.LIST,
        target = "deleteEmplPositionTypeRate",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "rateCurrencyUomId", hidden = @HiddenField),
            @FormField(name = "emplPositionTypeId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "rateAmountFromDate", hidden = @HiddenField),
            @FormField(name = "rateTypeId", displayEntity = @DisplayEntityField(entityName = "RateType", description = "${description}")),
            @FormField(name = "periodTypeId", displayEntity = @DisplayEntityField(entityName = "PeriodType", description = "${description}")),
            @FormField(name = "payGradeId", displayEntity = @DisplayEntityField(entityName = "PayGrade", description = "${description}")),
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResSalaryStepSeqId}", display = @DisplayField),
            @FormField(name = "rateAmount", display = @DisplayField(type = "currency")),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "nowDate", value = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDateString(\"yyyy-MM-dd HH:mm:ss.S\")}", type = "String")})
    )
    public interface ListEmplPositionTypeRates {}

    @Form(
        name = "AddEmplPositionTypeRate",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "updateEmplPositionTypeRate",
        defaultMapName = "emplPositionTypeRate",
        paginateTarget = "EditEmplPositionTypeRates",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "emplPositionTypeId", hidden = @HiddenField(value = "${parameters.emplPositionTypeId}")),
            @FormField(name = "rateTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", keyFieldName = "rateTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "payGradeId", position = 2, lookup = @LookupField(targetFormName = "LookupPayGrade")),
            @FormField(name = "periodTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", keyFieldName = "periodTypeId", orderBy = {@EntityOrderBy(fieldName = "periodTypeId")}))),
            @FormField(name = "salaryStepSeqId", title = "${uiLabelMap.HumanResSalaryStepSeqId}", position = 2, lookup = @LookupField(targetFormName = "LookupSalaryStep")),
            @FormField(name = "rateAmount", requiredField = true, text = @TextField),
            @FormField(name = "rateCurrencyUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddEmplPositionTypeRate {}

    @Form(
        name = "ListTerminationReasons",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateTerminationReason",
        paginateTarget = "EditTerminationReasons",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTerminationReason", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "terminationReasonId", title = "${uiLabelMap.HumanResTerminationReasonId}", display = @DisplayField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTerminationReason", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "terminationReasonId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y"))
        }
    )
    public interface ListTerminationReasons {}

    @Form(
        name = "AddTerminationReason",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createTerminationReason",
        defaultMapName = "terminationReason",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTerminationReason")
        },
        fields = {
            @FormField(name = "terminationReasonId", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddTerminationReason {}

    @Form(
        name = "ListJobInterviewType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateJobInterviewType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "jobInterviewTypeId", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteJobInterviewType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobInterviewTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListJobInterviewType {}

    @Form(
        name = "AddJobInterviewType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createJobInterviewType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "jobInterviewTypeId", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddJobInterviewType {}

    @Form(
        name = "ListTrainingTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateTrainingTypes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", display = @DisplayField),
            @FormField(name = "parentTypeId", title = "${uiLabelMap.HumanResPreRequisiteSkill}", widgetStyle = "${styles.link_nav_info_id}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteTrainingTypes", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trainingClassTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListTrainingTypes {}

    @Form(
        name = "AddTrainingTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createTrainingTypes",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "trainingClassTypeId", title = "${uiLabelMap.HumanResTrainingClassType}", requiredField = true, text = @TextField),
            @FormField(name = "parentTypeId", title = "${uiLabelMap.HumanResPreRequisiteSkill}", lookup = @LookupField(targetFormName = "LookupTraining")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddTrainingTypes {}

    @Form(
        name = "AddEmplLeaveType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createEmplLeaveType",
        defaultMapName = "emplLeaveType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplLeaveType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "leaveTypeId", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddEmplLeaveType {}

    @Form(
        name = "ListEmplLeaveTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateEmplLeaveType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeaveType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "leaveTypeId", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplLeaveType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "leaveTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListEmplLeaveTypes {}

    @Form(
        name = "AddEmplLeaveReasonType",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createEmplLeaveReasonType",
        defaultMapName = "emplLeaveReasonType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplLeaveReasonType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "emplLeaveReasonTypeId", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddEmplLeaveReasonType {}

    @Form(
        name = "ListEmplLeaveReasonTypes",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.MULTI,
        target = "updateEmplLeaveReasonType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplLeaveReasonType", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "emplLeaveReasonTypeId", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplLeaveReasonType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplLeaveReasonTypeId")})),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListEmplLeaveReasonTypes {}

    @Form(
        name = "AddPublicHoliday",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        target = "createPublicHoliday",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortId", useWhen = "workEffort!=null", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.userLogin.partyId}")),
            @FormField(name = "roleTypeId", useWhen = "workEffort==null", hidden = @HiddenField(value = "CAL_OWNER")),
            @FormField(name = "statusId", useWhen = "workEffort==null", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "PUBLIC_HOLIDAY")),
            @FormField(name = "currentStatusId", useWhen = "workEffort==null", hidden = @HiddenField(value = "CAL_TENTATIVE")),
            @FormField(name = "scopeEnumId", hidden = @HiddenField(value = "WES_PUBLIC")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.HumanHolidayName}", requiredField = true, text = @TextField(size = 50)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textarea = @TextareaField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.CommonTo}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonSubmit}", useWhen = "workEffort==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "workEffort!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort!=null", target = "updatePublicHoliday")
        }
    )
    public interface AddPublicHoliday {}

    @Form(
        name = "ListPublicHoliday",
        location = "component://humanres/widget/forms/GlobalHRSettingForms.xml",
        type = FormType.LIST,
        target = "PublicHoliday",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "PUBLIC_HOLIDAY")),
            @FormField(name = "scopeEnumId", hidden = @HiddenField(value = "WES_PUBLIC")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.HumanHolidayName}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "estimatedStartDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "estimatedCompletionDate", title = "${uiLabelMap.CommonTo}", display = @DisplayField(type = "date")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonBy}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${lastName}, ${firstName} ${middleName}")),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePublicHoliday", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "partyId", fromField = "assignmentList[0].partyId")})
    )
    public interface ListPublicHoliday {}

}
