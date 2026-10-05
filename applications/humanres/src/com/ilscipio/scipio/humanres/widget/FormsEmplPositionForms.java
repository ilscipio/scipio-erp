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
public class FormsEmplPositionForms {

    @Form(
        name = "ListEmplPositions",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "EmplPosition",
        paginate = "true",
        paginateTarget = "FindEmplPositions",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplPosition", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmployeePositionId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "emplPositionView", description = "${emplPositionId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "emplPositionTypeId", displayEntity = @DisplayEntityField(entityName = "EmplPositionType", description = "${description}")),
            @FormField(name = "applyAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_add}", hyperlink = @HyperlinkField(target = "EditEmploymentApp", description = "${uiLabelMap.HumanResNewEmploymentApp}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId", fromField = "emplPositionId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "EmplPosition"), @FieldMap(fieldName = "orderBy", value = "emplPositionId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListEmplPositions {}

    @Form(
        name = "ListEmplPositionsParty",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        listName = "ListEmplPositions",
        extendsForm = "ListEmplPositions",
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        }
    )
    public interface ListEmplPositionsParty {}

    @Form(
        name = "EditEmplPosition",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "updateEmplPosition",
        defaultMapName = "emplPosition",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplPosition")
        },
        fields = {
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmplPositionId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "emplPosition!=null", display = @DisplayField),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmplPositionId}", useWhen = "emplPosition==null&&emplPositionId==null", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.HumanResEmplPositionId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${emplPositionId}]", useWhen = "emplPosition==null&&emplPositionId!=null", display = @DisplayField),
            @FormField(name = "partyId", parameterName = "partyId", title = "${uiLabelMap.HumanResEmploymentPartyIdFrom}", tooltip = "${uiLabelMap.HumanResEmploymentPartyIdFromToolTip}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "EMPL_POSITION_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "budgetId", lookup = @LookupField(targetFormName = "LookupBudget")),
            @FormField(name = "budgetItemSeqId", lookup = @LookupField(targetFormName = "LookupBudgetItem")),
            @FormField(name = "emplPositionTypeId", title = "${uiLabelMap.HumanResEmployeePositionTypeId}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "EmplPositionType", description = "${description}", keyFieldName = "emplPositionTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "emplPosition==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "emplPosition!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "emplPosition==null", target = "createEmplPosition")
        }
    )
    public interface EditEmplPosition {}

    @Form(
        name = "ListEmplPositionFulfillments",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        target = "updateEmplPositionFulfillment",
        paginateTarget = "EditEmplPositionFulfillments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionFulfillment")
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplPositionFulfillment", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListEmplPositionFulfillments {}

    @Form(
        name = "AddEmplPositionFulfillment",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createEmplPositionFulfillment",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplPositionFulfillment")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmplPositionFulfillment {}

    @Form(
        name = "ListReportsToEmplPositionReportingStructs",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        target = "updateEmplPositionReportingStruct",
        paginateTarget = "EditReportsToEmplPositionReportingStruct",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionReportingStruct")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "emplPositionIdReportingTo", display = @DisplayField),
            @FormField(name = "emplPositionIdManagedBy", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplPositionReportingStruct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionIdReportingTo"), @ParameterDef(paramName = "emplPositionIdManagedBy"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "emplPositionId", fromField = "parameters.emplPositionId")}))
        }
    )
    public interface ListReportsToEmplPositionReportingStructs {}

    @Form(
        name = "AddReportsToEmplPositionReportingStruct",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createEmplPositionReportingStruct",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplPositionReportingStruct")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "emplPositionIdReportingTo", requiredField = true, lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "emplPositionIdManagedBy", requiredField = true, hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddReportsToEmplPositionReportingStruct {}

    @Form(
        name = "ListReportedToEmplPositionReportingStructs",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        target = "updateEmplPositionReportingStruct",
        listName = "emplPositionReportingStructList",
        paginateTarget = "EditReportedToEmplPositionReportingStruct",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionReportingStruct")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "emplPositionIdManagedBy", display = @DisplayField),
            @FormField(name = "emplPositionIdReportingTo", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplPositionReportingStruct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionIdReportingTo"), @ParameterDef(paramName = "emplPositionIdManagedBy"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "emplPositionId", fromField = "parameters.emplPositionId")}))
        }
    )
    public interface ListReportedToEmplPositionReportingStructs {}

    @Form(
        name = "AddReportedToEmplPositionReportingStruct",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createEmplPositionReportingStruct",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplPositionReportingStruct")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "emplPositionIdReportingTo", hidden = @HiddenField(value = "${parameters.emplPositionId}")),
            @FormField(name = "emplPositionIdManagedBy", requiredField = true, lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddReportedToEmplPositionReportingStruct {}

    @Form(
        name = "ListEmplPositionResponsibilities",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        target = "updateEmplPositionResponsibility",
        paginateTarget = "EditEmplPositionResponsibilities",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionResponsibility")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "responsibilityTypeId", title = "${uiLabelMap.HumanResResponsibilityTypeId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteEmplPositionResponsibility", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionId"), @ParameterDef(paramName = "responsibilityTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListEmplPositionResponsibilities {}

    @Form(
        name = "AddEmplPositionResponsibility",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createEmplPositionResponsibility",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createEmplPositionResponsibility")
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField),
            @FormField(name = "responsibilityTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ResponsibilityType", description = " ${description}", keyFieldName = "responsibilityTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddEmplPositionResponsibility {}

    @Form(
        name = "ListValidResponsibilities",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        target = "updateValidResponsibility",
        paginateTarget = "findValidResponsibilities",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateValidResponsibility")
        },
        fields = {
            @FormField(name = "emplPositionTypeId", title = "${uiLabelMap.HumanResEmployeePositionTypeId}", display = @DisplayField),
            @FormField(name = "responsibilityTypeId", title = "${uiLabelMap.HumanResResponsibilityTypeId}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteValidResponsibility", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "emplPositionTypeId", fromField = "emplPositionId"), @ParameterDef(paramName = "responsibilityTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListValidResponsibilities {}

    @Form(
        name = "AddValidResponsibility",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createValidResponsibility",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createValidResponsibility")
        },
        fields = {
            @FormField(name = "emplPositionTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "EmplPositionType", description = " ${description}", keyFieldName = "emplPositionTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "responsibilityTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ResponsibilityType", description = " ${description}", keyFieldName = "responsibilityTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddValidResponsibility {}

    @Form(
        name = "FindEmplPositions",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "FindEmplPositions",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "EmplPosition", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "salaryFlag", dropDown = @DropDownField(current = "selected", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "exemptFlag", dropDown = @DropDownField(current = "selected", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "fulltimeFlag", dropDown = @DropDownField(current = "selected", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "temporaryFlag", dropDown = @DropDownField(current = "selected", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindEmplPositions {}

    @Form(
        name = "EmplPositionInfo",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        defaultMapName = "emplPosition",
        paginateTarget = "FindEmplPositions",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPosition", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyGroup", description = "${groupName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile", description = "[${emplPosition.partyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "emplPosition.partyId")}))),
            @FormField(name = "emplPositionTypeId", displayEntity = @DisplayEntityField(entityName = "EmplPositionType", description = "${description}", subHyperlink = @SubHyperlink(target = "EditEmplPositionTypes", description = "[${emplPosition.emplPositionTypeId}]", parameters = {@ParameterDef(paramName = "emplPositionTypeId", fromField = "emplPosition.emplPositionTypeId")})))
        }
    )
    public interface EmplPositionInfo {}

    @Form(
        name = "ListEmplPositionFulfilmentInfo",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        listName = "emplPositionFulfillments",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionFulfillment", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField)
        }
    )
    public interface ListEmplPositionFulfilmentInfo {}

    @Form(
        name = "ListEmplPositionResponsibilityInfo",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        listName = "emplPositionResponsibilities",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionResponsibility", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "emplPositionId", hidden = @HiddenField)
        }
    )
    public interface ListEmplPositionResponsibilityInfo {}

    @Form(
        name = "ListEmplPositionReportsToInfo",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        listName = "emplPositionReportingStructs",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionReportingStruct", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "emplPositionIdManagedBy", hidden = @HiddenField)
        }
    )
    public interface ListEmplPositionReportsToInfo {}

    @Form(
        name = "ListEmplPositionReportedToInfo",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        type = FormType.LIST,
        listName = "emplPositionReportingStructs",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateEmplPositionReportingStruct", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "emplPositionIdReportingTo", hidden = @HiddenField)
        }
    )
    public interface ListEmplPositionReportedToInfo {}

    @Form(
        name = "ListInternalOrg",
        location = "component://humanres/widget/forms/EmplPositionForms.xml",
        target = "createInternalOrg",
        defaultMapName = "partyRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "headpartyId", parameterName = "headpartyId", hidden = @HiddenField),
            @FormField(name = "partyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "dummy1", title = " ", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListInternalOrg {}

}
