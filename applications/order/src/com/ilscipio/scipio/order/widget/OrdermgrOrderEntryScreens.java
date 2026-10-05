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
public class OrdermgrOrderEntryScreens {

    @Screen(name = "CheckInits", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleOrderInits")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckInits.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryBaseDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/checkinits.ftl"
            )})
        }
    )
    public interface CheckInits {}

    @Screen(name = "OrderAgreements", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleOrderAgreements")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/OrderAgreements.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryBaseDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/orderagreements.ftl"
            )})
        }
    )
    public interface OrderAgreements {}

    @Screen(name = "RequirementsForSupplier", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderFindRequirementsForSupplier}")
    @Action(type = ActionType.SET, field = "entityName", value = "Requirement")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRequirements", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.showList"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderRequirementsList}", style = "heading"
                ),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "RequirementsList", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )}))})
        }
    )
    public interface RequirementsForSupplier {}

    @Screen(name = "FindQuoteForCart", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderFindQuotes}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "requestParameters.statusId", defaultValue = "QUO_APPROVED")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @Action(type = ActionType.SET, field = "requestParameters.currencyUomId", fromField = "currencyUomId")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindQuotes", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListQuotes", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )})
        }
    )
    public interface FindQuoteForCart {}

    @Screen(name = "ViewShoppingLists", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleShoppingList}")
    @Action(type = ActionType.SET, field = "partyId", fromField = "requestParameters.partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ShoppingList", list = "customershoppinglists", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ViewShoppingLists", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )})
        }
    )
    public interface ViewShoppingLists {}

    @Screen(name = "AddFromShoppingList", location = "component://order/widget/ordermgr/OrderEntryScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleShoppingListItem}")
    @Action(type = ActionType.SET, field = "shoppingListId", fromField = "requestParameters.shoppingListId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ShoppingListItem", list = "shoppinglistitems", fieldMaps = {@FieldMap(fieldName = "shoppingListId", fromField = "shoppingListId")})
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddFromShoppingList", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "AddFromShoppingListAll", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )})
        }
    )
    public interface AddFromShoppingList {}

}
