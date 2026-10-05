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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllingBudgetScreens {

    @Screen(name = "ListBudgets", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingFindBudgets")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListBudgets")
    @DecoratorScreen(
        name = "CommonControllingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(condition = @Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "CREATE"
                    }), widgets = @WidgetsLeaf(containers = {
                        @ContainerLeaf(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingNewBudget}", style = "${styles.link_nav} ${styles.action_add}", target = "EditBudget"
                        )})}))})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindBudgetOptions", location = "component://accounting/widget/controlling/BudgetForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "BudgetSearchResults"
                        )}))})})
        }
    )
    public interface ListBudgets {}

    @Screen(name = "BudgetSearchResults", location = "component://accounting/widget/controlling/BudgetScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"fixedAssetPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListBudgets", location = "component://accounting/widget/controlling/BudgetForms.xml")}))
    public interface BudgetSearchResults {}

    @Screen(name = "EditBudget", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBudget")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "parameters.budgetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.budget ? 'PageTitleEditBudget' : 'AccountingNewBudget'}")
    @DecoratorScreen(
        name = "CommonBudgetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditBudget", location = "component://accounting/widget/controlling/BudgetForms.xml"
                )})})
        }
    )
    public interface EditBudget {}

    @Screen(name = "BudgetOverview", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleBudgetOverview")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BudgetOverview")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "parameters.budgetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetRole", list = "budgetRoles", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "parameters.budgetId")}, orderBy = {"partyId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetStatus", list = "budgetStatuses", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "parameters.budgetId")}, orderBy = {"statusId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetItem", list = "budgetItems", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "parameters.budgetId")}, orderBy = {"budgetItemSeqId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetReview", list = "budgetReviews", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "budgetId")}, orderBy = {"budgetReviewId"})
    @DecoratorScreen(
        name = "CommonBudgetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.CONTAINER, style = "clear"),
                @Widget(type = WidgetType.CONTAINER, style = "clear")}, containers = {
                    @Container(style = "${styles.grid_large}6", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.AccountingBudgetStatus}", navigationFormName = "BudgetStatus", includeForms = {
                    @IncludeForm(name = "BudgetStatus", location = "component://accounting/widget/controlling/BudgetForms.xml"
                
                    )})}, position = 1),
                    @Container(style = "${styles.grid_large}6", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.AccountingBudgetRoles}", navigationFormName = "BudgetRoles", includeForms = {
                    @IncludeForm(name = "BudgetRoles", location = "component://accounting/widget/controlling/BudgetForms.xml"
                
                    )})}, position = 2)}, screenlets = {
                        @Screenlet(title = "${uiLabelMap.AccountingBudgetHeader}", includeForms = {
                            @IncludeForm(name = "BudgetHeader", location = "component://accounting/widget/controlling/BudgetForms.xml"
                        )}, position = 0),
                        @Screenlet(title = "${uiLabelMap.AccountingBudgetItems}", includeForms = {
                            @IncludeForm(name = "BudgetItems", location = "component://accounting/widget/controlling/BudgetForms.xml"
                        )}, position = 4),
                        @Screenlet(title = "${uiLabelMap.AccountingBudgetReviews}", includeForms = {
                            @IncludeForm(name = "BudgetReviews", location = "component://accounting/widget/controlling/BudgetForms.xml"
                        )}, position = 6)})
        }
    )
    public interface BudgetOverview {}

    @Screen(name = "EditBudgetItems", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingEntityLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.viewIndex")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.viewSize")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListBudget")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BudgetItem")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "parameters.budgetId")
    @Action(type = ActionType.SET, field = "budgetItemSeqId", fromField = "parameters.budgetItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.ENTITY_ONE, entityName = "BudgetItem", valueField = "budgetItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetItem", list = "budgetItems", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "parameters.budgetId")}, orderBy = {"budgetItemSeqId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "BudgetItemType", list = "budgetItemTypes")
    @DecoratorScreen(
        name = "CommonBudgetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingBudgetItemsAdd}", includeForms = {
                    @IncludeForm(name = "EditBudgetItem", location = "component://accounting/widget/controlling/BudgetForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.AccountingBudgetItems}", includeForms = {
                    @IncludeForm(name = "EditBudgetItems", location = "component://accounting/widget/controlling/BudgetForms.xml"
                )})})
        }
    )
    public interface EditBudgetItems {}

    @Screen(name = "BudgetRoles", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListBudgetRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BudgetRoles")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "parameters.budgetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetRole", list = "budgetRoles", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "budgetId")}, orderBy = {"partyId"})
    @DecoratorScreen(
        name = "CommonBudgetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListBudgetRoles", location = "component://accounting/widget/controlling/BudgetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingPartyRoleAdd}", name = "PartyBudgetRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditBudgetRole", location = "component://accounting/widget/controlling/BudgetForms.xml"
                )}, position = 0)})
        }
    )
    public interface BudgetRoles {}

    @Screen(name = "BudgetReviews", location = "component://accounting/widget/controlling/BudgetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListBudgetReviews")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "BudgetReviews")
    @Action(type = ActionType.SET, field = "budgetId", fromField = "parameters.budgetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Budget", valueField = "budget")
    @Action(type = ActionType.ENTITY_AND, entityName = "BudgetReview", list = "budgetReviews", fieldMaps = {@FieldMap(fieldName = "budgetId", fromField = "budgetId")}, orderBy = {"budgetReviewId"})
    @DecoratorScreen(
        name = "CommonBudgetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListBudgetReviews", location = "component://accounting/widget/controlling/BudgetForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingBudgetReviewAdd}", name = "BudgetReviewPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditBudgetReview", location = "component://accounting/widget/controlling/BudgetForms.xml"
                )}, position = 0)})
        }
    )
    public interface BudgetReviews {}

}
