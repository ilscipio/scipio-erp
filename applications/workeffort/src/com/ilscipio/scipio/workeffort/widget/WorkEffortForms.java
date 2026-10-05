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
public class WorkEffortForms {

    @Form(
        name = "FilterUserJobs",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "SERVICE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FilterUserJobs {}

    @Form(
        name = "UserJobsList",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "userJobs",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "JobSandbox", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "poolId", hidden = @HiddenField),
            @FormField(name = "parentJobId", hidden = @HiddenField),
            @FormField(name = "previousJobId", hidden = @HiddenField),
            @FormField(name = "loaderName", hidden = @HiddenField),
            @FormField(name = "runAsUser", hidden = @HiddenField),
            @FormField(name = "authUserLoginId", hidden = @HiddenField),
            @FormField(name = "runByInstanceId", hidden = @HiddenField),
            @FormField(name = "runtimeDataId", hidden = @HiddenField),
            @FormField(name = "recurrenceInfoId", hidden = @HiddenField)
        }
    )
    public interface UserJobsList {}

    @Form(
        name = "EditWorkEffort",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffort",
        defaultMapName = "workEffort",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffort")
        },
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "workEffort!=null", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", useWhen = "workEffort==null&&workEffortId==null", ignored = @IgnoredField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${workEffortId}]", useWhen = "workEffort==null&&workEffortId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.CommonName}*"),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.CommonType}*", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", keyFieldName = "workEffortTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.CommonPurpose}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortPurposeType", description = "${description}", keyFieldName = "workEffortPurposeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}*", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "[${uiLabelMap.WorkEffortGeneral}] ${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CALENDAR_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "percentComplete", position = 2),
            @FormField(name = "priority", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "1", description = "${uiLabelMap.WorkEffortPriorityOne}"), @Option(key = "2", description = "${uiLabelMap.WorkEffortPriorityTwo}"), @Option(key = "3", description = "${uiLabelMap.WorkEffortPriorityThree}"), @Option(key = "4", description = "${uiLabelMap.WorkEffortPriorityFour}"), @Option(key = "5", description = "${uiLabelMap.WorkEffortPriorityFive}"), @Option(key = "6", description = "${uiLabelMap.WorkEffortPrioritySix}"), @Option(key = "7", description = "${uiLabelMap.WorkEffortPrioritySeventh}"), @Option(key = "8", description = "${uiLabelMap.WorkEffortPriorityEight}"), @Option(key = "9", description = "${uiLabelMap.WorkEffortPriorityNine}")})),
            @FormField(name = "scopeEnumId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORK_EFF_SCOPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "estimatedStartDate"),
            @FormField(name = "estimatedCompletionDate", position = 2),
            @FormField(name = "actualStartDate"),
            @FormField(name = "actualCompletionDate", position = 2),
            @FormField(name = "tempExprId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TemporalExpression", description = "${tempExprId}", keyFieldName = "tempExprId"))),
            @FormField(name = "facilityId", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "moneyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${abbreviation} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "abbreviation")}))),
            @FormField(name = "estimatedMilliSeconds", lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "estimatedSetupMillis", position = 2, lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "actualMilliSeconds", lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "actualSetupMillis", position = 2, lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "totalMilliSecondsAllowed", lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "totalMoneyAllowed", position = 2),
            @FormField(name = "quantityToProduce"),
            @FormField(name = "quantityProduced"),
            @FormField(name = "quantityRejected", position = 2),
            @FormField(name = "reservPersons"),
            @FormField(name = "reserv2ndPPPerc"),
            @FormField(name = "reservNthPPPerc", position = 2),
            @FormField(name = "quickAssignPartyId", useWhen = "workEffort==null", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${userLogin.partyId}")),
            @FormField(name = "requirementId", useWhen = "workEffort==null", lookup = @LookupField(targetFormName = "LookupRequirement")),
            @FormField(name = "communicationEventId", mapName = "context", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "revisionNumber", useWhen = "workEffort!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "workflowPackageId", ignored = @IgnoredField),
            @FormField(name = "workflowPackageVersion", ignored = @IgnoredField),
            @FormField(name = "workflowProcessId", ignored = @IgnoredField),
            @FormField(name = "workflowProcessVersion", ignored = @IgnoredField),
            @FormField(name = "workflowActivityId", ignored = @IgnoredField),
            @FormField(name = "recurrenceInfoId", ignored = @IgnoredField),
            @FormField(name = "runtimeDataId", ignored = @IgnoredField),
            @FormField(name = "noteId", ignored = @IgnoredField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort==null", target = "createWorkEffort")
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "quickAssignPartyId"), @SortField(name = "workEffortId"), @SortField(name = "workEffortName"), @SortField(name = "description"), @SortField(name = "workEffortTypeId"), @SortField(name = "workEffortPurposeTypeId"), @SortField(name = "currentStatusId"), @SortField(name = "percentComplete"), @SortField(name = "priority"), @SortField(name = "scopeEnumId"), @SortField(name = "estimatedStartDate"), @SortField(name = "estimatedCompletionDate"), @SortField(name = "actualStartDate"), @SortField(name = "actualCompletionDate")})
    )
    public interface EditWorkEffort {}

    @Form(
        name = "FindWorkEffort",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "FindWorkEffort",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", textFind = @TextFindField),
            @FormField(name = "workEffortParentId", position = 2, textFind = @TextFindField),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", keyFieldName = "workEffortTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.CommonPurpose}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortPurposeType", description = "${description}", keyFieldName = "workEffortPurposeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "[${uiLabelMap.WorkEffortGeneral}] ${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CALENDAR_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "priority", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "9"), @Option(key = "8"), @Option(key = "7"), @Option(key = "6"), @Option(key = "5"), @Option(key = "4"), @Option(key = "3"), @Option(key = "2"), @Option(key = "1")})),
            @FormField(name = "workEffortName", textFind = @TextFindField),
            @FormField(name = "description", position = 2, textFind = @TextFindField),
            @FormField(name = "facilityId", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "fixedAssetId", position = 2, lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "scopeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORK_EFF_SCOPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "moneyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${abbreviation} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "abbreviation")}))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "workflowPackageId", ignored = @IgnoredField),
            @FormField(name = "workflowPackageVersion", ignored = @IgnoredField),
            @FormField(name = "workflowProcessId", ignored = @IgnoredField),
            @FormField(name = "workflowProcessVersion", ignored = @IgnoredField),
            @FormField(name = "workflowActivityId", ignored = @IgnoredField),
            @FormField(name = "recurrenceInfoId", ignored = @IgnoredField),
            @FormField(name = "runtimeDataId", ignored = @IgnoredField),
            @FormField(name = "noteId", ignored = @IgnoredField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindWorkEffort {}

    @Form(
        name = "ListLookupWorkEffort",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "javascript:set_value('${workEffortId}')", urlMode = UrlMode.PLAIN, description = "${workEffortName}[${workEffortId}]", alsoHidden = false)),
            @FormField(name = "workEffortTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortType")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "marketingCampaignId", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "WorkEffort"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupWorkEffort {}

    @Form(
        name = "AddWorkEffortAndAssoc",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortAndAssoc",
        defaultMapName = "workEffort",
        extendsForm = "EditWorkEffort",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortIdFrom", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "workEffortIdTo", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "workEffortAssocTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortIdFrom"), @SortField(name = "workEffortAssocTypeId"), @SortField(name = "sequenceNum"), @SortField(name = "fromDate"), @SortField(name = "thruDate"), @SortField(name = "workEffortIdTo"), @SortField(name = "workEffortName"), @SortField(name = "description")})
    )
    public interface AddWorkEffortAndAssoc {}

    @Form(
        name = "AddWorkEffortAssoc",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortIdFrom", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName} [${workEffortId}]")),
            @FormField(name = "workEffortIdTo", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "workEffortAssocTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        }
    )
    public interface AddWorkEffortAssoc {}

    @Form(
        name = "EditWorkEffortAssoc",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortIdFrom", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "EditWorkEffort", description = "${workEffortIdFrom}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdFrom")}))),
            @FormField(name = "workEffortIdTo", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "EditWorkEffort", description = "${workEffortIdTo}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdTo")}))),
            @FormField(name = "workEffortAssocTypeId", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffortAssocType")),
            @FormField(name = "sequenceNum", mapName = "workEffortAssoc", fieldName = "sequenceNum", text = @TextField),
            @FormField(name = "fromDate", mapName = "workEffortAssoc", fieldName = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", mapName = "workEffortAssoc", fieldName = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        }
    )
    public interface EditWorkEffortAssoc {}

    @Form(
        name = "EditWorkEffortAndAssoc",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortAndAssoc",
        defaultMapName = "workEffort",
        extendsForm = "EditWorkEffort",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortIdFrom", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "EditWorkEffort", description = "${workEffortIdFrom}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdFrom")}))),
            @FormField(name = "workEffortIdTo", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "EditWorkEffort", description = "${workEffortIdTo}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId", fromField = "workEffortIdTo")}))),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "workEffortAssocTypeId", mapName = "workEffortAssoc", displayEntity = @DisplayEntityField(entityName = "WorkEffortAssocType")),
            @FormField(name = "sequenceNum", mapName = "workEffortAssoc", fieldName = "sequenceNum", text = @TextField),
            @FormField(name = "fromDate", mapName = "workEffortAssoc", fieldName = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", mapName = "workEffortAssoc", fieldName = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffortAssoc==null", target = "createWorkEffortAndAssoc")
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "workEffortIdFrom"), @SortField(name = "workEffortAssocTypeId"), @SortField(name = "sequenceNum"), @SortField(name = "fromDate"), @SortField(name = "thruDate"), @SortField(name = "workEffortIdTo"), @SortField(name = "workEffortName"), @SortField(name = "description")})
    )
    public interface EditWorkEffortAndAssoc {}

    @Form(
        name = "ListWorkEfforts",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditWorkEffort", description = "${workEffortName} [${workEffortId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "WorkEffortType")),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "WorkEffortPurposeType")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "priority", display = @DisplayField),
            @FormField(name = "estimatedStartDate", display = @DisplayField(type = "date")),
            @FormField(name = "estimatedCompletionDate", display = @DisplayField(type = "date")),
            @FormField(name = "actualStartDate", display = @DisplayField(type = "date")),
            @FormField(name = "actualCompletionDate", display = @DisplayField(type = "date")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffort", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "fieldList", value = "${groovy:['workEffortId','workEffortTypeId','currentStatusId','workEffortPurposeTypeId','description','priority','lastModifiedDate','estimatedStartDate','estimatedCompletionDate','actualStartDate','actualCompletionDate']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "WorkEffortFindView"), @FieldMap(fieldName = "fieldList", fromField = "fieldList"), @FieldMap(fieldName = "orderBy", value = "lastModifiedDate DESC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface ListWorkEfforts {}

    @Form(
        name = "FoundWorkEfforts",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        extendsForm = "ListWorkEfforts",
        paginateTarget = "FindWorkEffort",
        headerRowStyle = "header-row-2"
    )
    public interface FoundWorkEfforts {}

    @Form(
        name = "WorkEffortTreeLine",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "ListWorkEfforts",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        hideHeader = true,
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditWorkEffort", description = "${workEffortName} [${workEffortId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortType")),
            @FormField(name = "workEffortPurposeTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortPurposeType")),
            @FormField(name = "detailAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ListChildWorkEffort", description = "${uiLabelMap.CommonDetail}", alsoHidden = false, parameters = {@ParameterDef(paramName = "trail", fromField = "workEffortId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeWorkEffort", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")}))
        }
    )
    public interface WorkEffortTreeLine {}

    @Form(
        name = "EditWorkEffortParty",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortParty",
        defaultMapName = "workEffortParty",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "preferredContactMechId", lookup = @LookupField(targetFormName = "LookupPreferredContactMech")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "workEffortParty==null", target = "createWorkEffortParty")
        }
    )
    public interface EditWorkEffortParty {}

    @Form(
        name = "FindWorkEffortParties",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "ListWorkEffortParties",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", textFind = @TextFindField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "description", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "preferredContactMechId", lookup = @LookupField(targetFormName = "LookupPreferredContactMech")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindWorkEffortParties {}

    @Form(
        name = "ListWorkEffortParties",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "preferredContactMechId", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditWorkEffortParty", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListWorkEffortParties {}

    @Form(
        name = "DisplayWorkEffortPartyAssigns",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "partyAssignments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.AccountingRoleType}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "assignmentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "expectationEnumId", title = "${uiLabelMap.WorkEffortExpectation}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId"))
        }
    )
    public interface DisplayWorkEffortPartyAssigns {}

    @Form(
        name = "EditWorkEffortCommEvent",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortCommEvent",
        defaultMapName = "workEffortCommEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", useWhen = "communicationEvent!=null", display = @DisplayField),
            @FormField(name = "communicationEventTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CommunicationEventType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "communicationEvent==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "communicationEvent!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${communicationEvent.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "contactMechTypeId", useWhen = "commEventContactMechType!=null&&parentCommEventContactMechType==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechTypeId", useWhen = "commEventContactMechType==null&&parentCommEventContactMechType!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechTypeId", useWhen = "commEventContactMechType==null&&parentCommEventContactMechType==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdFrom", useWhen = "commEventRoleTypeIdFrom!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", useWhen = "commEventRoleTypeIdTo!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateTime = @DateTimeField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", dateTime = @DateTimeField),
            @FormField(name = "subject", text = @TextField(size = 30)),
            @FormField(name = "note", title = "${uiLabelMap.CommonNote}", textarea = @TextareaField(rows = 3)),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent}", textarea = @TextareaField(rows = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "communicationEvent==null", target = "createWorkEffortCommEvent")
        }
    )
    public interface EditWorkEffortCommEvent {}

    @Form(
        name = "FindWorkEffortCommEvents",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "ListWorkEffortCommEvents",
        defaultMapName = "workEffortCommEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "commEventId", title = "${uiLabelMap.FormFieldTitle_communicationEventId}", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindWorkEffortCommEvents {}

    @Form(
        name = "ListWorkEffortCommEvents",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultMapName = "workEffortCommEvent",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditWorkEffortCommEvent", description = "${communicationEventId}", parameters = {@ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "DONE_PAGE", fromField = "donePage")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonFrom}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "custRequestId", text = @TextField(size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField),
            @FormField(name = "subject", mapName = "subjectMap", text = @TextField(size = 30))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListWorkEffortCommEvents {}

    @Form(
        name = "ListPreferredContactMech",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactMechId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactMechId}')", urlMode = UrlMode.PLAIN, description = "${contactMechId}", alsoHidden = false)),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType"))
        }
    )
    public interface ListPreferredContactMech {}

    @Form(
        name = "LookupCommEvent",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "LookupCommEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.WorkEffortCommEventId}", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "contactMechTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdFrom", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "custRequestId", lookup = @LookupField(targetFormName = "LookupCustRequest", size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateFind = @DateFindField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", dateFind = @DateFindField),
            @FormField(name = "subject", mapName = "subjectMap", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupCommEvent {}

    @Form(
        name = "ListLookupCommEvent",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.WorkEffortCommEventId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${communicationEventId}')", urlMode = UrlMode.PLAIN, description = "${communicationEventId}", alsoHidden = false)),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonFrom}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyId} ${uiLabelMap.CommonTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "custRequestId", text = @TextField(size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupCommEvent {}

    @Form(
        name = "ListWorkEffortRates",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "deleteWorkEffortRate",
        listName = "workEffortRates",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "rateCurrencyUomId", hidden = @HiddenField),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", displayEntity = @DisplayEntityField(entityName = "RateType")),
            @FormField(name = "rateAmount", display = @DisplayField(type = "currency")),
            @FormField(name = "periodTypeId", displayEntity = @DisplayEntityField(entityName = "PeriodType", description = "${description}")),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListWorkEffortRates {}

    @Form(
        name = "AddWorkEffortRate",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortRate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "periodTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", constraints = {@EntityConstraint(name = "periodTypeId", value = "RATE_%", operator = "like")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "rateAmount", tooltip = "${uiLabelMap.WorkEffortOverrideDefaultRateAmount}", text = @TextField),
            @FormField(name = "rateCurrencyUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${abbreviation} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "abbreviation")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "periodTypeId", value = "RATE_HOUR")})
    )
    public interface AddWorkEffortRate {}

    @Form(
        name = "ListWorkEffortTimeEntries",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortTimeEntry",
        listName = "timesheetEntries",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "timeEntryId", hidden = @HiddenField),
            @FormField(name = "timesheetId", lookup = @LookupField(targetFormName = "LookupTimesheet", size = 10)),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 12, defaultValue = "${timesheet.partyId}")),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "invoiceId", ignored = @IgnoredField),
            @FormField(name = "invoiceItemSeqId", ignored = @IgnoredField),
            @FormField(name = "invoiceInfo", display = @DisplayField(description = "${invoiceId}:${invoiceItemSeqId}", alsoHidden = false)),
            @FormField(name = "comments", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortTimeEntry", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "timeEntryId")}))
        }
    )
    public interface ListWorkEffortTimeEntries {}

    @Form(
        name = "AddWorkEffortTimeEntry",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortTimeEntry",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createTimeEntry")
        },
        fields = {
            @FormField(name = "timeEntryId", ignored = @IgnoredField),
            @FormField(name = "timesheetId", lookup = @LookupField(targetFormName = "LookupTimesheet", size = 10)),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${timesheet.partyId}")),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "invoiceId", ignored = @IgnoredField),
            @FormField(name = "invoiceItemSeqId", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortTimeEntry {}

    @Form(
        name = "AddWorkEffortTimeToInvoice",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "addWorkEffortTimeToInvoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "invoiceId", lookup = @LookupField(targetFormName = "LookupInvoice")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PageTitleAddWorkEffortTimeToInvoice}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortTimeToInvoice {}

    @Form(
        name = "AddWorkEffortTimeToNewInvoice",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "addWorkEffortTimeToNewInvoice",
        fields = {
            @FormField(name = "combineInvoiceItem", hidden = @HiddenField(value = "Y")),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.WorkEffortTimeBillFromParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.WorkEffortTimeBillToParty}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${timesheet.clientPartyId}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PageTitleAddWorkEffortTimeToNewInvoice}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortTimeToNewInvoice {}

    @Form(
        name = "ListWorkEffortNotes",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "workEffortNotes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "noteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditWorkEffortNotes", description = "${noteId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "noteId")})),
            @FormField(name = "workEffortId", entityName = "WorkEffort", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditWorkEffort", description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "noteInfo", title = "${uiLabelMap.CommonNote}", display = @DisplayField),
            @FormField(name = "noteParty", title = "${uiLabelMap.CommonBy}", display = @DisplayField(description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, noteParty, true)} at ${noteDateTime}")),
            @FormField(name = "internalNote", title = "${uiLabelMap.WorkEffortPrivatePublic}", useWhen = "\"N\".equals(internalNote)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updateWorkEffortNote", description = "${uiLabelMap.OrderNotesPrivate}", parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "noteId"), @ParameterDef(paramName = "internalNote", value = "Y")})),
            @FormField(name = "internalNote", title = "${uiLabelMap.WorkEffortPrivatePublic}", useWhen = "\"Y\".equals(internalNote)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updateWorkEffortNote", description = "${uiLabelMap.OrderNotesPublic}", parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "noteId"), @ParameterDef(paramName = "internalNote", value = "N")}))
        }
    )
    public interface ListWorkEffortNotes {}

    @Form(
        name = "AddWorkEffortNote",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateWorkEffortNote",
        defaultMapName = "workEffortNoteAndData",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortNote")
        },
        fields = {
            @FormField(name = "noteId", useWhen = "noteId != null", hidden = @HiddenField),
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "noteInfo", title = "${uiLabelMap.CommonNote}", textarea = @TextareaField(cols = 70, rows = 5)),
            @FormField(name = "internalNote", title = "${uiLabelMap.WorkEffortInternalNote}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "noteParty", hidden = @HiddenField),
            @FormField(name = "noteName", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "noteId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "noteId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "noteId == null", target = "createWorkEffortNote")
        }
    )
    public interface AddWorkEffortNote {}

    @Form(
        name = "AddWorkEffortContent",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortContent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortContent")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "contentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "workEffortContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortContentType", description = "${description}", keyFieldName = "workEffortContentTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortContent {}

    @Form(
        name = "ListWorkEffortContents",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortContent",
        listName = "workEffortContents",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortContent", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "contentId", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "/content/control/editContent", description = "${contentId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "workEffortContentTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortContentType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortContent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortContentTypeId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "contentId")}))
        }
    )
    public interface ListWorkEffortContents {}

    @Form(
        name = "AddWorkEffortGoodStandard",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortGoodStandard",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortGoodStandard")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "workEffortGoodStdTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortGoodStandardType", description = "${description}", keyFieldName = "workEffortGoodStdTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFG_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortGoodStandard {}

    @Form(
        name = "ListWorkEffortGoodStandards",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortGoodStandard",
        listName = "workEffortGoodStandards",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortGoodStandard", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "workEffortGoodStdTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortGoodStandardType", description = "${description}")),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productName}", subHyperlink = @SubHyperlink(target = "/catalog/control/ViewProduct", description = "${productId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFG_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeWorkEffortGoodStandard", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortGoodStdTypeId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortId")}))
        }
    )
    public interface ListWorkEffortGoodStandards {}

    @Form(
        name = "AddWorkEffortReview",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortReview",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortReview")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "userLoginId", lookup = @LookupField(targetFormName = "LookupPartyAndUserLoginAndPerson", defaultValue = "${defaultUserLoginId}")),
            @FormField(name = "reviewDate", dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}*", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFF_REVIEW_STTS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "reviewText", textarea = @TextareaField(rows = 5)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortReview {}

    @Form(
        name = "ListWorkEffortReviews",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortReview",
        listName = "workEffortReviews",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortReview", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "userLoginId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${userLoginId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "reviewDate", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}*", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFF_REVIEW_STTS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortReview", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "userLoginId"), @ParameterDef(paramName = "reviewDate")}))
        }
    )
    public interface ListWorkEffortReviews {}

    @Form(
        name = "AddWorkEffortKeyword",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortKeyword",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortKeyword")
        },
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "keyword", title = "${uiLabelMap.WorkEffortKeyword}*", text = @TextField(size = 10)),
            @FormField(name = "relevancyWeight", title = "${uiLabelMap.ProductWeight}", text = @TextField(size = 5)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortKeyword {}

    @Form(
        name = "ListWorkEffortKeywords",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "workEffortkeywords",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "keyword", title = "${uiLabelMap.WorkEffortKeywords}", display = @DisplayField),
            @FormField(name = "relevancyWeight", display = @DisplayField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.ProductDeleteAllKeywords}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortKeyword", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "keyword")}))
        }
    )
    public interface ListWorkEffortKeywords {}

    @Form(
        name = "AddWorkEffortContactMech",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortContactMech",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "contactMechId", lookup = @LookupField(targetFormName = "LookupContactMech")),
            @FormField(name = "comments", text = @TextField(size = 50)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortContactMech {}

    @Form(
        name = "ListWorkEffortContactMechs",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "workEffortContactMechs",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "contactMechId", title = "${uiLabelMap.CommonDescription}", useWhen = "telecomNumber != null", display = @DisplayField(description = "${telecomNumber.contryCode} ${telecomNumber.areaCode} ${telecomNumber.contactNumber} ${telecomNumber.askForName}")),
            @FormField(name = "contactMechId", title = "${uiLabelMap.CommonDescription}", useWhen = "postalAddress != null", display = @DisplayField(description = "${postalAddress.address1} ${postalAddress.address2} ${postalAddress.city} ${postalAddress.stateProvinceGeoId} ${postalAddress.postalCode} ${postalAddress.countryGeoId} ")),
            @FormField(name = "contactMechId", title = "${uiLabelMap.CommonDescription}", useWhen = "telecomNumber == null && postalAddress == null", display = @DisplayField(description = "${contactMech.infoString}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortContactMech", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "contactMechId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "contactMechTypeId", fromField = "contactMech.contactMechTypeId")}, entityOne = {@EntityOneAction(entityName = "ContactMech", valueField = "contactMech"), @EntityOneAction(entityName = "PostalAddress", valueField = "postalAddress"), @EntityOneAction(entityName = "TelecomNumber", valueField = "telecomNumber")})
    )
    public interface ListWorkEffortContactMechs {}

    @Form(
        name = "AddAgreementWorkEffortApplic",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createAgreementWorkEffortApplic",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${parameters.workEffortId}")),
            @FormField(name = "agreementId", lookup = @LookupField(targetFormName = "LookupAgreement")),
            @FormField(name = "agreementItemSeqId", lookup = @LookupField(targetFormName = "LookupAgreementItem")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddAgreementWorkEffortApplic {}

    @Form(
        name = "ListAgreementWorkEffortApplics",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateAgreementWorkEffortApplic",
        listName = "agreementWorkEffortApplics",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", displayEntity = @DisplayEntityField(entityName = "Agreement", keyFieldName = "agreementId", subHyperlink = @SubHyperlink(target = "/accounting/control/FindAgreement", description = "${agreementId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "agreementId")}))),
            @FormField(name = "agreementItemSeqId", display = @DisplayField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAgreementWorkEffortApplic", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "workEffortId")}))
        }
    )
    public interface ListAgreementWorkEffortApplics {}

    @Form(
        name = "ListWorkEffortFixedAssetAssigns",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortFixedAssetAssign",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAsset}", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}", subHyperlink = @SubHyperlink(target = "/accounting/control/EditFixedAsset", description = "${fixedAssetId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "fixedAssetId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FA_ASGN_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "availabilityStatusId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFA_AVAILABILITY")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "allocatedCost", text = @TextField),
            @FormField(name = "comments", text = @TextField(size = 60, maxlength = 255)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortFixedAssetAssign", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListWorkEffortFixedAssetAssigns {}

    @Form(
        name = "DisplayWorkEffortFixedAssetAssigns",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "fixedAssetAssignments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAsset}", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}", subHyperlink = @SubHyperlink(target = "/accounting/control/EditFixedAsset", description = "${fixedAssetId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "fixedAssetId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "statusId", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "availabilityStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "allocatedCost", display = @DisplayField),
            @FormField(name = "comments", display = @DisplayField)
        }
    )
    public interface DisplayWorkEffortFixedAssetAssigns {}

    @Form(
        name = "EditWorkEffortFixedAssetAssign",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortFixedAssetAssign",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "fixedAssetId", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FA_ASGN_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "availabilityStatusId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFA_AVAILABILITY")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "allocatedCost", text = @TextField),
            @FormField(name = "comments", text = @TextField(size = 60, maxlength = 255)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditWorkEffortFixedAssetAssign {}

    @Form(
        name = "ListWorkEffortEventReminders",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortEventReminder",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "sequenceId", hidden = @HiddenField),
            @FormField(name = "contactMechId", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "reminderDateTime", dateTime = @DateTimeField),
            @FormField(name = "repeatCount", text = @TextField),
            @FormField(name = "repeatInterval", text = @TextField),
            @FormField(name = "reminderOffset", lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortEventReminder", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "contactMechId"), @ParameterDef(paramName = "sequenceId")}))
        }
    )
    public interface ListWorkEffortEventReminders {}

    @Form(
        name = "EditWorkEffortEventReminder",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createWorkEffortEventReminder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "localeId", hidden = @HiddenField(value = "${locale}")),
            @FormField(name = "timeZoneId", hidden = @HiddenField(value = "${timeZone}")),
            @FormField(name = "contactMechId", lookup = @LookupField(targetFormName = "LookupContactMech")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "reminderDateTime", dateTime = @DateTimeField),
            @FormField(name = "repeatCount", text = @TextField),
            @FormField(name = "repeatInterval", text = @TextField),
            @FormField(name = "reminderOffset", lookup = @LookupField(targetFormName = "LookupTimeDuration", presentation = "window")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditWorkEffortEventReminder {}

    @Form(
        name = "EditICalendar",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateICalendar",
        defaultMapName = "workEffort",
        fields = {
            @FormField(name = "workEffortTypeId", hidden = @HiddenField(value = "PUBLISH_PROPS")),
            @FormField(name = "currentStatusId", hidden = @HiddenField(value = "CAL_CANCELLED")),
            @FormField(name = "quickAssignPartyId", title = "${uiLabelMap.WorkEffortICalendarOwner}", useWhen = "workEffort==null @and workEffortId==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortICalendarUrl}", useWhen = "workEffort!=null", hyperlink = @HyperlinkField(target = "${serverRootUrl}/iCalendar/${workEffortId}/", urlMode = UrlMode.PLAIN, description = "${serverRootUrl}/iCalendar/${workEffortId}/")),
            @FormField(name = "workEffortName", title = "${uiLabelMap.WorkEffortICalendarName}", requiredField = true, text = @TextField),
            @FormField(name = "scopeEnumId", title = "${uiLabelMap.WorkEffortICalendarVisibility}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORK_EFF_SCOPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "actualStartDate", title = "${uiLabelMap.CommonFrom}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "actualCompletionDate", title = "${uiLabelMap.CommonTo}", position = 3, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort==null", target = "createICalendar")
        },
        actions = @FormActions(set = {@SetAction(field = "serverRootUrl", value = "${groovy: org.ofbiz.base.util.UtilHttp.getServerRootUrl(request)}")})
    )
    public interface EditICalendar {}

    @Form(
        name = "EditICalendarData",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateICalendarData",
        defaultMapName = "iCalData",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField(value = "${workEffortId}")),
            @FormField(name = "icalData", title = "${uiLabelMap.WorkEffortICalendarData}", textarea = @TextareaField(rows = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "iCalData==null", target = "createICalendarData")
        }
    )
    public interface EditICalendarData {}

    @Form(
        name = "EditICalendarPartyAssign",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createICalendarPartyAssign",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField(value = "PRTYASGN_ASSIGNED")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "roleList", keyName = "roleTypeId", description = "${description}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "comments", text = @TextField(size = 60, maxlength = 255)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.partyId"), @SetAction(field = "parameters.roleTypeId"), @SetAction(field = "parameters.fromDate"), @SetAction(field = "parameters.thruDate"), @SetAction(field = "parameters.comments")})
    )
    public interface EditICalendarPartyAssign {}

    @Form(
        name = "ListICalendarPartyAssigns",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateICalendarPartyAssign",
        extendsForm = "ListWorkEffortPartyAssigns",
        extendsResource = "component://workeffort/widget/WorkEffortPartyAssignForms.xml",
        headerRowStyle = "header-row-2",
        fields = {
            @FormField(name = "statusDateTime", ignored = @IgnoredField),
            @FormField(name = "availabilityStatusId", ignored = @IgnoredField),
            @FormField(name = "expectationEnumId", ignored = @IgnoredField),
            @FormField(name = "delegateReasonEnumId", ignored = @IgnoredField),
            @FormField(name = "facilityId", ignored = @IgnoredField),
            @FormField(name = "mustRsvp", ignored = @IgnoredField)
        }
    )
    public interface ListICalendarPartyAssigns {}

    @Form(
        name = "ListIcalendars",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditICalendar", urlMode = UrlMode.PLAIN, description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "iCalendarUrl", title = "${uiLabelMap.WorkEffortICalendarUrl}", widgetStyle = "${styles.link_nav_info_uri} ${styles.action_view}", hyperlink = @HyperlinkField(target = "${serverRootUrl}/iCalendar/${workEffortId}/", urlMode = UrlMode.PLAIN, description = "${serverRootUrl}/iCalendar/${workEffortId}/", alsoHidden = false)),
            @FormField(name = "workEffortName", title = "${uiLabelMap.WorkEffortICalendarName}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "serverRootUrl", value = "${groovy: org.ofbiz.base.util.UtilHttp.getServerRootUrl(request)}")})
    )
    public interface ListIcalendars {}

    @Form(
        name = "EditICalendarFixedAssetAssign",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "createICalendarFixedAssetAssign",
        extendsForm = "EditWorkEffortFixedAssetAssign",
        fields = {
            @FormField(name = "statusId", ignored = @IgnoredField),
            @FormField(name = "availabilityStatusId", ignored = @IgnoredField),
            @FormField(name = "allocatedCost", ignored = @IgnoredField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.fixedAssetId"), @SetAction(field = "parameters.fromDate"), @SetAction(field = "parameters.thruDate"), @SetAction(field = "parameters.comments")})
    )
    public interface EditICalendarFixedAssetAssign {}

    @Form(
        name = "ListICalendarFixedAssetAssigns",
        location = "component://workeffort/widget/WorkEffortForms.xml",
        target = "updateICalendarFixedAssetAssign",
        extendsForm = "ListWorkEffortFixedAssetAssigns",
        fields = {
            @FormField(name = "statusId", ignored = @IgnoredField),
            @FormField(name = "availabilityStatusId", ignored = @IgnoredField),
            @FormField(name = "allocatedCost", ignored = @IgnoredField),
            @FormField(name = "deleteAction", ignored = @IgnoredField)
        }
    )
    public interface ListICalendarFixedAssetAssigns {}

}
