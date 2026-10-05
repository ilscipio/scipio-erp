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
public class LookupForms {

    @Form(
        name = "lookupWorkEffort",
        location = "component://workeffort/widget/LookupForms.xml",
        target = "LookupWorkEffort",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffort", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", textFind = @TextFindField),
            @FormField(name = "workEffortParentId", textFind = @TextFindField),
            @FormField(name = "workEffortName", textFind = @TextFindField),
            @FormField(name = "workEffortTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", keyFieldName = "workEffortTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortPurposeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortPurposeType", description = "${description}", keyFieldName = "workEffortPurposeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "[General] ${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CALENDAR_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortParentId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "facilityId", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "fixedAssetId", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "scopeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "WORK_EFF_SCOPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "moneyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${abbreviation} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "abbreviation")}))),
            @FormField(name = "workflowPackageId", ignored = @IgnoredField),
            @FormField(name = "workflowPackageVersion", ignored = @IgnoredField),
            @FormField(name = "workflowProcessId", ignored = @IgnoredField),
            @FormField(name = "workflowProcessVersion", ignored = @IgnoredField),
            @FormField(name = "workflowActivityId", ignored = @IgnoredField),
            @FormField(name = "recurrenceInfoId", ignored = @IgnoredField),
            @FormField(name = "runtimeDataId", ignored = @IgnoredField),
            @FormField(name = "noteId", ignored = @IgnoredField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupWorkEffort {}

    @Form(
        name = "lookupWorkEffortShort",
        location = "component://workeffort/widget/LookupForms.xml",
        target = "LookupWorkEffortShort",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", textFind = @TextFindField),
            @FormField(name = "workEffortParentId", textFind = @TextFindField),
            @FormField(name = "workEffortName", textFind = @TextFindField),
            @FormField(name = "workEffortTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortType", description = "${description}", keyFieldName = "workEffortTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortPurposeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WorkEffortPurposeType", description = "${description}", keyFieldName = "workEffortPurposeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "[General] ${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CALENDAR_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "workEffortParentId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "facilityId", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "fixedAssetId", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface lookupWorkEffortShort {}

    @Form(
        name = "listLookupWorkEffort",
        location = "component://workeffort/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupWorkEffort",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffortWorkEffortId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${workEffortId}')", urlMode = UrlMode.PLAIN, description = "${workEffortId}", alsoHidden = false)),
            @FormField(name = "workEffortName", display = @DisplayField),
            @FormField(name = "workEffortTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortType")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "marketingCampaignId", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "WorkEffort"), @FieldMap(fieldName = "orderBy", value = "workEffortId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupWorkEffort {}

    @Form(
        name = "lookupTimesheet",
        location = "component://workeffort/widget/LookupForms.xml",
        target = "LookupTimesheet",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Timesheet", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", textFind = @TextFindField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "clientPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupTimesheet {}

    @Form(
        name = "listLookupTimesheet",
        location = "component://workeffort/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPerson",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "timesheetId", title = "${uiLabelMap.WorkEffortTimesheetTimesheetId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${timesheetId}')", urlMode = UrlMode.PLAIN, description = "${timesheetId}", alsoHidden = false)),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "clientPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} [${partyId}]"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Timesheet"), @FieldMap(fieldName = "orderBy", value = "timesheetId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupTimesheet {}

}
