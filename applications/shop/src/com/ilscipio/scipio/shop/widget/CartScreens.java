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
public class CartScreens {

    @Screen(name = "microcart", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SET, field = "initialLocaleComplete", value = "${groovy:parameters?.userLogin?.lastLocale}", valueType = "String", defaultValue = "${groovy:locale.toString()}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/cart/microcart.ftl")}))
    public interface microcart {}

    @Screen(name = "minicart", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SET, field = "hidetoplinks", value = "Y")
    @Action(type = ActionType.SET, field = "hidebottomlinks", value = "N")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/minicart.ftl")}))
    public interface minicart {}

    @Screen(name = "minipromotext", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowPromoText.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/cart/minipromotext.ftl")}))
    public interface minipromotext {}

    @Screen(name = "promoUseDetailsInline", location = "component://shop/widget/CartScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/promoUseDetailsInline.ftl")}))
    public interface promoUseDetailsInline {}

    @Screen(name = "showcart", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShoppingCart")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "Shopping Cart")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "giftEnable", resource = "order", property = "orderPreference.giftEnable", defaultValue = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowPromoText.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/cart/showcart.ftl"
            )})
        }
    )
    public interface showcart {}

    @Screen(name = "showAllPromotions", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SET, field = "promoUseDetailsInlineScreen", value = "component://shop/widget/CartScreens.xml#promoUseDetailsInline")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAllPromotions")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowPromoText.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/cart/showAllPromotions.ftl"
            )})
        }
    )
    public interface showAllPromotions {}

    @Screen(name = "showPromotionDetails", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShowPromotionDetails")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/cart/ShowPromotionDetails.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "promotion", location = "component://order/widget/ordermgr/OrderEntryCartScreens.xml"
            )})
        }
    )
    public interface showPromotionDetails {}

    @Screen(name = "UpdateCart", location = "component://shop/widget/CartScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/ShowCart.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/cart/UpdateCart.ftl")}))
    public interface UpdateCart {}

}
