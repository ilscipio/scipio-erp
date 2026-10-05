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
public class OrdermgrOrderEntryCartScreens {

    @Screen(name = "minicart", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "hidetoplinks", value = "Y")
    @Action(type = ActionType.SET, field = "hidebottomlinks", value = "Y")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/minicart.ftl")}))
    public interface minicart {}

    @Screen(name = "promoUseDetailsInline", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promoUseDetailsInline.ftl")}))
    public interface promoUseDetailsInline {}

    @Screen(name = "orderHeaderInfo", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/orderHeaderInfo.ftl")}))
    public interface orderHeaderInfo {}

    @Screen(name = "orderShortcuts", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShoppingList.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/orderShortcuts.ftl")}))
    public interface orderShortcuts {}

    @Screen(name = "ShowCart", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleOrderShowCart")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "giftEnable", resource = "order", property = "orderPreference.giftEnable", defaultValue = "Y")
    @Action(type = ActionType.SET, field = "promoUseDetailsInlineScreen", value = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#promoUseDetailsInline")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowPromoText.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStorePromoAndAppl", list = "allProductPromos", filterByDate = true, conditions = {@ConditionExpr(fieldName = "manualOnly", value = "Y"), @ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"productPromoId"})
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/showcartitems.ftl"
            )}, containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}9 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/cart/javascript.ftl"
                    ),
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/cart/showcart.ftl"
                ),
                @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/cart/addItemsToShoppingList.ftl"
            )})}, position = 0)}),
            @DecoratorSection(name = "right-column", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promoCodes.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/manualPromotions.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promoText.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/associatedProducts.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promotionsApplied.ftl"
            )})
        }
    )
    public interface ShowCart {}

    @Screen(name = "survey", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleOrderShowCart")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "promoUseDetailsInlineScreen", value = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#promoUseDetailsInline")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowPromoText.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/survey.ftl"
            )})
        }
    )
    public interface survey {}

    @Screen(name = "showAllPromotions", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAllPromotions")
    @Action(type = ActionType.SET, field = "promoUseDetailsInlineScreen", value = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#promoUseDetailsInline")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShowPromoText.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/showAllPromotions.ftl"
            )})
        }
    )
    public interface showAllPromotions {}

    @Screen(name = "showPromotionDetails", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShowPromotionDetails")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/cart/ShowPromotionDetails.groovy")
    @DecoratorScreen(
        name = "CommonOrderEntryDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "promotion", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"
            )})
        }
    )
    public interface showPromotionDetails {}

    @Screen(name = "promotion", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productPromo"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderErrorNoPromotionFoundWithID} [${productPromoId}]", style = "common-msg-error")}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promotiondetails.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promotioncategories.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promotionproducts.ftl")}))
    public interface promotion {}

    @Screen(name = "AddGiftCertificate", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderAddGiftCertificate")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/AddGiftCertificates.groovy")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/GiftCertificates.ftl"
            )})
        }
    )
    public interface AddGiftCertificate {}

    @Screen(name = "LookupAssociatedProducts", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderAssociatedProducts")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/lookup/LookupAssociatedProducts.groovy")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupAssociatedProducts", location = "component://order/widget/ordermgr/OrderEntryForms.xml"
            )})
        }
    )
    public interface LookupAssociatedProducts {}

}
