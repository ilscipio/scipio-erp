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
package com.ilscipio.scipio.accounting.widget;

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
public class ControllingBudgetForms {

    @Form(
        name = "ListBudgets",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListBudgets",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "budgetId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditBudget", description = "${budgetId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "budgetId")})),
            @FormField(name = "budgetTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "BudgetType")),
            @FormField(name = "customTimePeriodId", title = "${uiLabelMap.CommonPeriod}", sortField = true, displayEntity = @DisplayEntityField(entityName = "CustomTimePeriod", description = "${customTimePeriodId}: ${fromDate} - ${thruDate}")),
            @FormField(name = "comments", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Budget"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "BudgetSearchResults")
        }
    )
    public interface ListBudgets {}

    @Form(
        name = "FindBudgetOptions",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        target = "ListBudgets",
        extendsForm = "lookupBudget",
        extendsResource = "component://accounting/widget/FieldLookupForms.xml",
        fields = {
            @FormField(name = "searchOptions_collapsed", hidden = @HiddenField(value = "true")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindBudgetOptions {}

    @Form(
        name = "EditBudget",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        target = "updateBudget",
        defaultMapName = "budget",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "budgetId", useWhen = "budget != null", display = @DisplayField),
            @FormField(name = "budgetTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BudgetType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "customTimePeriodId", title = "${uiLabelMap.CommonPeriod}", position = 2, lookup = @LookupField(targetFormName = "LookupCustomTimePeriod")),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "budget == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "budget != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "budget == null", target = "createBudget")
        }
    )
    public interface EditBudget {}

    @Form(
        name = "BudgetHeader",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        title = "Budget header information",
        defaultMapName = "budget",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "budgetTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "BudgetType")),
            @FormField(name = "customTimePeriodId", title = "${uiLabelMap.CommonPeriod}", position = 2, displayEntity = @DisplayEntityField(entityName = "CustomTimePeriod", description = "${customTimePeriodId}: ${fromDate} - ${thruDate}")),
            @FormField(name = "comments", display = @DisplayField)
        }
    )
    public interface BudgetHeader {}

    @Form(
        name = "BudgetStatus",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetStatuses",
        paginateTarget = "budgetOverview",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "statusDate", display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}"))
        }
    )
    public interface BudgetStatus {}

    @Form(
        name = "BudgetRoles",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetRoles",
        paginateTarget = "BudgetOverview",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "InvoiceRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "name", entryName = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false))
        }
    )
    public interface BudgetRoles {}

    @Form(
        name = "BudgetItems",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetItems",
        paginateTarget = "BudgetOverview",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "BudgetItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "budgetItemSeqId", display = @DisplayField),
            @FormField(name = "budgetItemTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "BudgetItemType")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "purpose", display = @DisplayField),
            @FormField(name = "justification", display = @DisplayField)
        }
    )
    public interface BudgetItems {}

    @Form(
        name = "BudgetReviews",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetReviews",
        paginateTarget = "BudgetOverview",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "budgetReviewId", display = @DisplayField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "name", entryName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "budgetReviewResultTypeId", displayEntity = @DisplayEntityField(entityName = "BudgetReviewResultType", alsoHidden = false)),
            @FormField(name = "reviewDate", display = @DisplayField)
        }
    )
    public interface BudgetReviews {}

    @Form(
        name = "EditBudgetItems",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.MULTI,
        target = "updateBudgetItem?budgetId=${budgetId}",
        title = "Edit Budget Items",
        listName = "budgetItems",
        defaultEntityName = "BudgetItem",
        paginateTarget = "EditBudgetItems",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "viewSize", hidden = @HiddenField(value = "${viewSize}")),
            @FormField(name = "viewIndex", hidden = @HiddenField(value = "${viewIndex}")),
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "budgetItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditBudgetItems", description = "${budgetItemSeqId}", parameters = {@ParameterDef(paramName = "budgetId"), @ParameterDef(paramName = "budgetItemSeqId")})),
            @FormField(name = "budgetItemTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BudgetItemType", description = "${description}", keyFieldName = "budgetItemTypeId"))),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField(size = 10)),
            @FormField(name = "purpose", text = @TextField(size = 50)),
            @FormField(name = "justification", text = @TextField(size = 50)),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeBudgetItem", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "budgetId"), @ParameterDef(paramName = "budgetItemSeqId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        }
    )
    public interface EditBudgetItems {}

    @Form(
        name = "EditBudgetItem",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        target = "createBudgetItem",
        defaultMapName = "budgetItem",
        defaultEntityName = "BudgetItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "budgetItemTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BudgetItemType", description = "${description}", keyFieldName = "budgetItemTypeId"))),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField(size = 10)),
            @FormField(name = "purpose", text = @TextField(size = 50)),
            @FormField(name = "justification", text = @TextField(size = 50)),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", useWhen = "invoiceItem==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonAdd}", useWhen = "invoiceItem!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditBudgetItem {}

    @Form(
        name = "EditBudgetRole",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        target = "createBudgetRole",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "BudgetRole")
        },
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditBudgetRole {}

    @Form(
        name = "ListBudgetRoles",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetRoles",
        paginateTarget = "BudgetRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "name", entryName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false)),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeBudgetRole", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "budgetId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        }
    )
    public interface ListBudgetRoles {}

    @Form(
        name = "EditBudgetReview",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        target = "createBudgetReview",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "budgetReviewResultTypeId", title = "${uiLabelMap.AccountingBudgetReviewResult}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BudgetReviewResultType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "reviewDate", dateTime = @DateTimeField),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditBudgetReview {}

    @Form(
        name = "ListBudgetReviews",
        location = "component://accounting/widget/controlling/BudgetForms.xml",
        type = FormType.LIST,
        listName = "budgetReviews",
        paginateTarget = "BudgetReviews",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "budgetId", hidden = @HiddenField),
            @FormField(name = "budgetReviewId", display = @DisplayField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "name", entryName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "budgetReviewResultTypeId", title = "${uiLabelMap.AccountingBudgetReviewResult}", displayEntity = @DisplayEntityField(entityName = "BudgetReviewResultType", alsoHidden = false)),
            @FormField(name = "reviewDate", display = @DisplayField),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeBudgetReview", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "budgetId"), @ParameterDef(paramName = "budgetReviewId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "budgetReviewResultTypeId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        }
    )
    public interface ListBudgetReviews {}

}
