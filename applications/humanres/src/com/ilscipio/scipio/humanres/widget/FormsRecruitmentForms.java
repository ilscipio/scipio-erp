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
public class FormsRecruitmentForms {

    @Form(
        name = "FindJobRequisitions",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "FindJobRequisitions",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobRequisition", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "jobRequisitionId", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "skillTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobPostingTypeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "JOB_POSTING")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobLocation", textFind = @TextFindField),
            @FormField(name = "examTypeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "EXAM_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "gender", hidden = @HiddenField),
            @FormField(name = "age", hidden = @HiddenField),
            @FormField(name = "durationMonths", hidden = @HiddenField),
            @FormField(name = "noOfResources", hidden = @HiddenField),
            @FormField(name = "jobRequisitionDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "requiredOnDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "qualification", hidden = @HiddenField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "jobRequisitionId", fromField = "parameters.jobRequisitionId")})
    )
    public interface FindJobRequisitions {}

    @Form(
        name = "ListJobRequisitions",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindJobRequisitions",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "jobRequisitionId", useWhen = "hasAdminPermission", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditJobRequisition", description = "${jobRequisitionId}", parameters = {@ParameterDef(paramName = "jobRequisitionId")})),
            @FormField(name = "jobRequisitionId", useWhen = "!hasAdminPermission", display = @DisplayField),
            @FormField(name = "skillTypeId", displayEntity = @DisplayEntityField(entityName = "SkillType", description = "${description}")),
            @FormField(name = "jobPostingTypeEnumId", display = @DisplayField),
            @FormField(name = "examTypeEnumId", display = @DisplayField),
            @FormField(name = "qualification", display = @DisplayField),
            @FormField(name = "jobLocation", display = @DisplayField),
            @FormField(name = "experienceYears", display = @DisplayField),
            @FormField(name = "experienceMonths", display = @DisplayField),
            @FormField(name = "jobRequisitionDate", display = @DisplayField),
            @FormField(name = "requiredOnDate", display = @DisplayField),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditInternalJobPosting", description = "${uiLabelMap.CommonApply}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobRequisitionId")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", useWhen = "hasAdminPermission", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteJobRequisition", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobRequisitionId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "JobRequisition")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListJobRequisitions {}

    @Form(
        name = "EditJobRequisition",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "updateJobRequisition",
        defaultMapName = "jobRequisition",
        fields = {
            @FormField(name = "jobRequisitionId", useWhen = "jobRequisition==null", ignored = @IgnoredField),
            @FormField(name = "jobRequisitionId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "jobRequisition!=null", display = @DisplayField),
            @FormField(name = "jobDescription", title = "${uiLabelMap.CommonDescription}", titleAreaStyle = "group-label", text = @TextField),
            @FormField(name = "jobRequisitionDate", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}", type = "date")),
            @FormField(name = "requiredOnDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "jobLocation", requiredField = true, text = @TextField),
            @FormField(name = "age", text = @TextField),
            @FormField(name = "noOfResources", requiredField = true, text = @TextField),
            @FormField(name = "gender", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "M", description = "${uiLabelMap.CommonMale}"), @Option(key = "F", description = "${uiLabelMap.CommonFemale}")})),
            @FormField(name = "durationMonths", text = @TextField),
            @FormField(name = "qualification", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyQualType", description = "${description}", keyFieldName = "partyQualTypeId", constraints = {@EntityConstraint(name = "parentTypeId", value = "DEGREE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "examTypeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "EXAM_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "skillCriteria", title = "${uiLabelMap.HumanResSkills} ${uiLabelMap.CommonRequired}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "skillTypeId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "experienceMonths", requiredField = true, text = @TextField),
            @FormField(name = "experienceYears", requiredField = true, text = @TextField),
            @FormField(name = "jobPostingTypeEnumId", hidden = @HiddenField(value = "JOB_POSTING_INTR")),
            @FormField(name = "emplPositionId", requiredField = true, lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "submitAction", title = "Create", useWhen = "jobRequisition==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "Update", useWhen = "jobRequisition!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "jobRequisition==null", target = "createJobRequisition")
        }
    )
    public interface EditJobRequisition {}

    @Form(
        name = "FindInternalJobPosting",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "FindInternalJobPosting",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmploymentApp", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "applicationId", lookup = @LookupField(targetFormName = "LookupEmploymentApp")),
            @FormField(name = "applyingPartyId", useWhen = "hasAdminPermission", hidden = @HiddenField),
            @FormField(name = "applyingPartyId", useWhen = "!hasAdminPermission", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "statusId", title = "${uiLabelMap.HumanResInternalJobPosting} ${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "IJP_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "jobRequisitionId", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "approverPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindInternalJobPosting {}

    @Form(
        name = "ListInternalJobPosting",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindInternalJobPosting",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmploymentApp", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "applicationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInternalJobPosting", description = "${applicationId}", parameters = {@ParameterDef(paramName = "applicationId")})),
            @FormField(name = "approverPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverPartyId")}))),
            @FormField(name = "applyingPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${applyingPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "applyingPartyId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.HumanResIJPStatus}", display = @DisplayField),
            @FormField(name = "jobRequisitionId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditJobRequisition", description = "${jobRequisitionId}", parameters = {@ParameterDef(paramName = "jobRequisitionId")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", useWhen = "hasAdminPermission", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteInternalJobPosting", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "applicationId")})),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "EmploymentApp")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListInternalJobPosting {}

    @Form(
        name = "EditInternalJobPosting",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "updateInternalJobPosting",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateInternalJobPosting", mapName = "employmentApp")
        },
        fields = {
            @FormField(name = "applicationId", useWhen = "employmentApp==null", hidden = @HiddenField),
            @FormField(name = "applicationId", useWhen = "employmentApp!=null", display = @DisplayField),
            @FormField(name = "applyingPartyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "approverPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "jobRequisitionId", requiredField = true, lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "statusId", title = "${uiLabelMap.HumanResIJPStatus}", hidden = @HiddenField(value = "IJP_APPLIED")),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "Create", useWhen = "employmentApp==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "Update", useWhen = "employmentApp!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "employmentApp==null", target = "createInternalJobPosting")
        }
    )
    public interface EditInternalJobPosting {}

    @Form(
        name = "FindJobInterview",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "FindJobInterview",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobInterview", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "jobIntervieweePartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "jobRequisitionId", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "jobInterviewerPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "jobInterviewTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "JobInterviewType", description = "${jobInterviewTypeId}", keyFieldName = "jobInterviewTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobInterviewResult", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Pass", description = "Pass"), @Option(key = "Fail", description = "Fail")})),
            @FormField(name = "gradeSecuredEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "INTR_RATNG")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindJobInterview {}

    @Form(
        name = "ListInterview",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindJobInterview",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobInterview", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "jobInterviewId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditJobInterview", description = "${jobInterviewId}", parameters = {@ParameterDef(paramName = "jobInterviewId")})),
            @FormField(name = "jobIntervieweePartyId", fieldName = "jobIntervieweePartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${jobIntervieweePartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "jobIntervieweePartyId")}))),
            @FormField(name = "jobIntervieweePartyId", fieldName = "jobIntervieweePartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${jobInterviewerPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "jobInterviewerPartyId")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteJobInterview", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "jobInterviewId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "JobInterview")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListInterview {}

    @Form(
        name = "EditJobInterview",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "updateJobInterview",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateJobInterview", mapName = "JobInterview")
        },
        fields = {
            @FormField(name = "jobInterviewId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "JobInterview!=null", display = @DisplayField),
            @FormField(name = "jobInterviewId", useWhen = "JobInterview==null", ignored = @IgnoredField),
            @FormField(name = "jobIntervieweePartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "jobRequisitionId", requiredField = true, lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "jobInterviewerPartyId", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "gradeSecuredEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "INTR_RATNG")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobInterviewTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "JobInterviewType", description = "${jobInterviewTypeId}", orderBy = {@EntityOrderBy(fieldName = "jobInterviewTypeId")}))),
            @FormField(name = "jobInterviewResult", dropDown = @DropDownField(options = {@Option(key = "Pass", description = "Pass"), @Option(key = "Fail", description = "Fail")})),
            @FormField(name = "submitAction", title = "Create", useWhen = "JobInterview==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "Update", useWhen = "JobInterview!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "JobInterview==null", target = "createJobInterview")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "JobInterview", valueField = "JobInterview")})
    )
    public interface EditJobInterview {}

    @Form(
        name = "FindApprovals",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "FindApprovals",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmploymentApp", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "applicationId", lookup = @LookupField(targetFormName = "LookupEmploymentApp")),
            @FormField(name = "approverPartyId", useWhen = "!hasAdminPermission", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "applyingPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.HumanResInternalJobPosting} ${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "IJP_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "jobRequisitionId", lookup = @LookupField(targetFormName = "LookupJobRequisition")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindApprovals {}

    @Form(
        name = "ListApprovals",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmploymentApp", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "applyingPartyId", fieldName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${applyingPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "applyingPartyId")}))),
            @FormField(name = "approverPartyId", fieldName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${approverPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "approverPartyId")}))),
            @FormField(name = "UpdateStatus", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditApprovalStatus", description = "${uiLabelMap.CommonUpdate}", parameters = {@ParameterDef(paramName = "applicationId")})),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "EmploymentApp")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListApprovals {}

    @Form(
        name = "EditApprovalStatus",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "updateApprovalStatus",
        defaultMapName = "employmentApp",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateApprovalStatus", mapName = "employmentApp")
        },
        fields = {
            @FormField(name = "applicationId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", display = @DisplayField),
            @FormField(name = "applicationId", display = @DisplayField),
            @FormField(name = "applyingPartyId", display = @DisplayField),
            @FormField(name = "jobRequisitionId", display = @DisplayField),
            @FormField(name = "approverPartyId", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.HumanResInternalJobPosting} ${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "IJP_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", useWhen = "employmentApp!=null&&employmentApp.getString(\"statusId\").equals(\"IJP_REJECTED\")", display = @DisplayField),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "employmentAppSourceTypeId", hidden = @HiddenField),
            @FormField(name = "referredByPartyId", hidden = @HiddenField),
            @FormField(name = "applicationDate", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "Update", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditApprovalStatus {}

    @Form(
        name = "FindRelocation",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        target = "FindRelocation",
        oddRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.HumanResEmployeePartyIdTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "emplPositionId", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "emplPositionIdReportingTo", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "internalOrganisation", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}))),
            @FormField(name = "reportingDate", dateFind = @DateFindField),
            @FormField(name = "location", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindRelocation {}

    @Form(
        name = "ListRelocation",
        location = "component://humanres/widget/forms/RecruitmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "Employee Name", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyId")}))),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmployeePositionId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "emplPositionView", description = "${emplPositionId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId")})),
            @FormField(name = "emplPositionIdReportingTo", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "emplPositionView", description = "${emplPositionIdReportingTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId", fromField = "emplPositionIdReportingTo")})),
            @FormField(name = "internalOrganisation", display = @DisplayField),
            @FormField(name = "reportingDate", display = @DisplayField),
            @FormField(name = "location", display = @DisplayField(description = "${groovy:                 import org.ofbiz.entity.GenericValue;                 import org.ofbiz.base.util.UtilMisc;                 import org.ofbiz.entity.util.EntityUtil;                 GenericValue partyAndPostalAddress = EntityUtil.getFirst(delegator.findByAnd(\"PartyAndPostalAddress\",UtilMisc.toMap(\"partyId\",internalOrganisation), null, false));                 if(partyAndPostalAddress==null) return ;                 if(partyAndPostalAddress!=null) city = partyAndPostalAddress.getString(\"city\");                 return city;                 }"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "EmplPositionFulfillmentAndReportingStruct"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListRelocation {}

}
