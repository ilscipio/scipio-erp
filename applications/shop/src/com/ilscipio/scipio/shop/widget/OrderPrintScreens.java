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
public class OrderPrintScreens {

    @Screen(name = "OrderPDF", location = "component://shop/widget/OrderPrintScreens.xml")
    @Action(type = ActionType.SET, field = "permChecksSetGlobal", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"userIsKnown"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "OrderPDF", location = "component://order/widget/ordermgr/OrderPrintScreens.xml")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}")}))
    public interface OrderPDF {}

    @Screen(name = "InvoicePDF", location = "component://shop/widget/OrderPrintScreens.xml")
    @Action(type = ActionType.SET, field = "permChecksSetGlobal", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"userIsKnown"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "InvoicePDF", location = "component://accounting/widget/AccountingPrintScreens.xml")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}")}))
    public interface InvoicePDF {}

}
