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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrQuoteWorkEffortScreens {

    @Screen(name = "CommonQuoteDecorator", location = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "quote")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "[${quote.quoteId}] ${quote.description}", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"quote"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "QuoteTabBar", location = "component://order/widget/ordermgr/OrderMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface CommonQuoteDecorator {}

    @Screen(name = "AddQuoteWorkEffort", location = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderCreateOrderQuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleAddQuoteWorkEffort")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/workeffort/control/ListWorkEfforts")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteWorkEffort", valueField = "quoteWorkEffort")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddQuoteWorkEffort", location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml"
                )})})
        }
    )
    public interface AddQuoteWorkEffort {}

    @Screen(name = "EditQuoteWorkEffort", location = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditQuoteWorkEffort")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteWorkEffort", location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml"
                )})})
        }
    )
    public interface EditQuoteWorkEffort {}

    @Screen(name = "ListQuoteWorkEfforts", location = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListQuoteWorkEfforts")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/workeffort/control/ListWorkEfforts")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteWorkEfforts", location = "component://order/widget/ordermgr/QuoteWorkEffortForms.xml"
                )})})
        }
    )
    public interface ListQuoteWorkEfforts {}

}
