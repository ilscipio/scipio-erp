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
public class EmailOrderScreens {

    @Screen(name = "OrderConfirmNoticePdf", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderConfirmationNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}")
    @Action(type = ActionType.SET, field = "title", value = "Order")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/OrderView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/CompanyHeader.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportContactMechs.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportHeaderInfo.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportBody.fo.ftl", platform = "xsl-fo"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/orderReportConditions.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "footer", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/pdf/ScipioOrderFooter.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface OrderConfirmNoticePdf {}

    @Screen(name = "OrderConfirmNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderConfirmationNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}")
    @Action(type = ActionType.SET, field = "allowAnonymousView", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/OrderNoticeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface OrderConfirmNotice {}

    @Screen(name = "OrderCompleteNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderCompleteNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/OrderNoticeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface OrderCompleteNotice {}

    @Screen(name = "BackorderNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderBackorderNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SET, field = "allowAnonymousView", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/OrderNoticeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface BackorderNotice {}

    @Screen(name = "OrderChangeNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderChangeNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SET, field = "allowAnonymousView", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/OrderNoticeEmail.ftl", platform = "email"
            )})
        }
    )
    public interface OrderChangeNotice {}

    @Screen(name = "PaymentRetryNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderPaymentRetryNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/emailpayretry.ftl", platform = "email"
            )})
        }
    )
    public interface PaymentRetryNotice {}

    @Screen(name = "PaymentChangeNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderPaymentRetryNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/emailpaychange.ftl", platform = "email"
            )})
        }
    )
    public interface PaymentChangeNotice {}

    @Screen(name = "PaymentCompletedNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleOrderPaymentRetryNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/emailpaycompleted.ftl", platform = "email"
            )})
        }
    )
    public interface PaymentCompletedNotice {}

    @Screen(name = "RecoverAbandonedCart", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleCartRecoveryReminder}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/cart/PrepareCartRecovery.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/RecoverAbandonedCart.ftl", platform = "email"
            )})
        }
    )
    public interface RecoverAbandonedCart {}

    @Screen(name = "ShipmentSentNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleShipmentSentNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/ShipmentStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ShipmentNotificationEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ShipmentSentNotice {}

    @Screen(name = "ShipmentCompleteNotice", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleShipmentCompleteNotice}")
    @Action(type = ActionType.SET, field = "baseEcommerceSecureUrl", value = "${baseSecureUrl}${baseWebappPath}/control")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/ShipmentStatus.groovy")
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/templates/email/ShipmentNotificationEmail.ftl", platform = "email"
            )})
        }
    )
    public interface ShipmentCompleteNotice {}

    @Screen(name = "orderheader", location = "component://shop/widget/EmailOrderScreens.xml")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/order/orderheader.ftl", platform = "email")}))
    public interface orderheader {}

    @Screen(name = "orderitems", location = "component://shop/widget/EmailOrderScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderItems.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://shop/webapp/shop/order/orderitems.ftl", platform = "email")}))
    public interface orderitems {}

    @Screen(name = "emailcart", location = "component://shop/widget/EmailOrderScreens.xml")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://shop/templates/email/emailcart.ftl", platform = "email")}))
    public interface emailcart {}

}
