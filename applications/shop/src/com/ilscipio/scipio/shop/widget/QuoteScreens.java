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
package com.ilscipio.scipio.shop.widget;

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
public class QuoteScreens {

    @Screen(name = "ListQuotes", location = "component://shop/widget/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListQuotes")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Quote", list = "quoteList", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "userLogin.partyId"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "QUO_CREATED")}, orderBy = {"-validFromDate"})
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/quote/QuoteList.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ListQuotes {}

    @Screen(name = "ViewQuote", location = "component://shop/widget/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewQuote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "QuoteType", toValueField = "quoteType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Uom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteItem", list = "quoteItems")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteAdjustment", list = "quoteAdjustments")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteRole", list = "quoteRoles")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = CompareField.class, params = {"quote.partyId", "equals", "userLogin.partyId"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/quote/CreateOrderQuote.ftl"
                        ),
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewQuoteTemplate", location = "component://order/widget/ordermgr/QuoteScreens.xml"
                    )}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoQuoteFound}", style = "common-msg-error"
                    )}))}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ViewQuote {}

}
