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
public class CatalogPriceScreens {

    @Screen(name = "FindProductPriceRule", location = "component://product/widget/catalog/PriceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPriceRule")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonPriceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductGlobalPriceRules}", includeForms = {
                    @IncludeForm(name = "FindProductPriceRules", location = "component://product/widget/catalog/PriceForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductAddPriceRule}", includeForms = {
                    @IncludeForm(name = "AddPriceRules", location = "component://product/widget/catalog/PriceForms.xml"
                )})})
        }
    )
    public interface FindProductPriceRule {}

    @Screen(name = "EditProductPriceRules", location = "component://product/widget/catalog/PriceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductPriceRule")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/price/EditProductPriceRules.groovy")
    @DecoratorScreen(
        name = "CommonPriceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/price/setPriceRulesCondEventJs.ftl"
            ),
            @Widget(type = WidgetType.INCLUDE_MENU, name = "PriceRulesButtonBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductPriceRule", location = "component://product/widget/catalog/PriceForms.xml", position = 2
                )}, labels = {
                    @Label(text = "${uiLabelMap.ProductConditionsActionsRemoveBefore}", style = "heading+4", position = 0
                ),
                @Label(text = "${uiLabelMap.ProductConditionsThenActions}", style = "heading+4", position = 4
            )}, screenlets = {
                @ScreenletNested(title = "${uiLabelMap.ProductConditions}", includeForms = {
                    @IncludeForm(name = "EditProductPriceRulesCond", location = "component://product/widget/catalog/PriceForms.xml", position = 0
                
            ),
                @IncludeForm(name = "AddProductPriceRulesCond", location = "component://product/widget/catalog/PriceForms.xml", position = 2
                
            )}, widgets = {
                    @Widget(type = WidgetType.HORIZONTAL_SEPARATOR, position = 1
            )
                }, position = 6),
            @ScreenletNested(title = "${uiLabelMap.ProductActions}", includeForms = {
                    @IncludeForm(name = "EditProductPriceRulesAction", location = "component://product/widget/catalog/PriceForms.xml", position = 0
                
            ),
                @IncludeForm(name = "AddProductPriceRulesAction", location = "component://product/widget/catalog/PriceForms.xml", position = 2
                
            )}, widgets = {
                    @Widget(type = WidgetType.HORIZONTAL_SEPARATOR, position = 1
            )
                }, position = 7)}, widgets = {
                @Widget(type = WidgetType.HORIZONTAL_SEPARATOR, position = 1),
                @Widget(type = WidgetType.HORIZONTAL_SEPARATOR, position = 3),
                @Widget(type = WidgetType.HORIZONTAL_SEPARATOR, position = 5)
            })})
        }
    )
    public interface EditProductPriceRules {}

}
