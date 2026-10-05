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
package com.ilscipio.scipio.product.widget;

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
public class CatalogShippingScreens {

    @Screen(name = "ListQuantityBreaks", location = "component://product/widget/catalog/ShippingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListQuantityBreaks")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuantityBreaks")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductQuantityBreaks")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "QuantityBreak", list = "quantityBreaks", orderBy = {"quantityBreakId"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuantityBreak", valueField = "quantityBreak")
    @DecoratorScreen(
        name = "CommonCarrierDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuantityBreaks", location = "component://product/widget/catalog/ShippingForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditQuantityBreaks}", includeForms = {
                    @IncludeForm(name = "EditQuantityBreak", location = "component://product/widget/catalog/ShippingForms.xml"
                )})})
        }
    )
    public interface ListQuantityBreaks {}

    @Screen(name = "ListShipmentMethodTypes", location = "component://product/widget/catalog/ShippingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListShipmentMethodTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListShipmentMethodTypes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductShipmentMethodTypes")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ShipmentMethodType", list = "shipmentMethodTypes", orderBy = {"sequenceNum", "description"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentMethodType", valueField = "shipmentMethodType")
    @DecoratorScreen(
        name = "CommonShippingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListShipmentMethodTypes", location = "component://product/widget/catalog/ShippingForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditShipmentMethodTypes}", includeForms = {
                    @IncludeForm(name = "EditShipmentMethodType", location = "component://product/widget/catalog/ShippingForms.xml"
                )})})
        }
    )
    public interface ListShipmentMethodTypes {}

    @Screen(name = "ListCarrierShipmentMethods", location = "component://product/widget/catalog/ShippingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCarrierShipmentMethods")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListCarrierShipmentMethods")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCarrierShipmentMethods")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CarrierShipmentMethod", list = "carrierShipmentMethods", orderBy = {"sequenceNumber"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "CarrierShipmentMethod", valueField = "carrierShipmentMethod")
    @DecoratorScreen(
        name = "CommonCarrierDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CarrierSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListCarrierShipmentMethods", location = "component://product/widget/catalog/ShippingForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditCarrierShipmentMethods}", includeForms = {
                    @IncludeForm(name = "EditCarrierShipmentMethod", location = "component://product/widget/catalog/ShippingForms.xml"
                )})})
        }
    )
    public interface ListCarrierShipmentMethods {}

    @Screen(name = "NewCarrier", location = "component://product/widget/catalog/ShippingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNewCarrier")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListCarrierShipmentMethods")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductNewCarrier")
    @DecoratorScreen(
        name = "CommonCarrierDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewCarrier", location = "component://product/widget/catalog/ShippingForms.xml"
                )})})
        }
    )
    public interface NewCarrier {}

}
