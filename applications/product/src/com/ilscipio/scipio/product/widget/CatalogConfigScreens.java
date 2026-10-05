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
public class CatalogConfigScreens {

    @Screen(name = "FindProductConfigItems", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindConfigItems")
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProductConfigItems", location = "component://product/widget/catalog/ConfigForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductConfigItems", location = "component://product/widget/catalog/ConfigForms.xml"
                    )}))})})
        }
    )
    public interface FindProductConfigItems {}

    @Screen(name = "EditProductConfigItem", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditConfigItem")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigItem")
    @Action(type = ActionType.SET, field = "configItemId", fromField = "parameters.configItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigItem", valueField = "configItem")
    @Action(type = ActionType.SET, field = "isCreateConfigItem", value = "${groovy: !(context.configItem || (parameters.configItemId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateConfigItem ? 'ProductNewConfigItem' : 'ProductConfigItem'}")
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductConfigItem", location = "component://product/widget/catalog/ConfigForms.xml"
                )})})
        }
    )
    public interface EditProductConfigItem {}

    @Screen(name = "EditProductConfigOptions", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditConfigOptions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigOptions")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductConfigOptions")
    @Action(type = ActionType.SET, field = "configItemId", fromField = "parameters.configItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigItem", valueField = "configItem")
    @Action(type = ActionType.SET, field = "configOptionId", fromField = "parameters.configOptionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigOption", valueField = "configOption")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigProduct", valueField = "productConfigProduct")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductConfigOption", list = "configOptionList", conditions = {@ConditionExpr(fieldName = "configItemId", fromField = "configItemId")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductConfigProduct", list = "configProducts", conditions = {@ConditionExpr(fieldName = "configItemId", fromField = "configItemId"), @ConditionExpr(fieldName = "configOptionId", fromField = "configOptionId")}, orderBy = {"sequenceNum"})
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "configOptions", location = "component://product/widget/catalog/ConfigScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "configComponent", location = "component://product/widget/catalog/ConfigScreens.xml"
            )})
        }
    )
    public interface EditProductConfigOptions {}

    @Screen(name = "configOptions", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductConfigOptionList}", includeForms = {@IncludeForm(name = "ProductConfigOptionList", location = "component://product/widget/catalog/ConfigForms.xml")}, position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"configOptionId"})}), widgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.PageTitleEditConfigOptions}", includeForms = {
                    @IncludeForm(name = "CreateConfigOption", location = "component://product/widget/catalog/ConfigForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductCreateNewConfigOptions}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductConfigOptions"
                )})}), failWidgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.ProductCreateNewConfigOptions}", includeForms = {
                    @IncludeForm(name = "CreateConfigOption", location = "component://product/widget/catalog/ConfigForms.xml"
                )})}), position = 0)}))
    public interface configOptions {}

    @Screen(name = "configComponent", location = "component://product/widget/catalog/ConfigScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"configOptionId"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductComponents} - ${configOption.configOptionId} - ${configOption.description}", includeForms = {@IncludeForm(name = "ProductConfigList", location = "component://product/widget/catalog/ConfigForms.xml")}, position = 0)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productId"})}), widgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.AddProductComponent}", includeForms = {
                    @IncludeForm(name = "CreateProductConfigProduct", location = "component://product/widget/catalog/ConfigForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AddProductComponent}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductConfigOptions"
                )})}), failWidgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${uiLabelMap.AddProductComponent}", includeForms = {
                    @IncludeForm(name = "CreateProductConfigProduct", location = "component://product/widget/catalog/ConfigForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AddProductComponent}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductConfigOptions"
                )})}), position = 1)}))
    public interface configComponent {}

    @Screen(name = "EditProductConfigItemContent", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductConfigItemContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigItemContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductConfigItemContent")
    @Action(type = ActionType.SET, field = "configItemId", fromField = "parameters.configItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigItem", valueField = "configItem")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/config/EditProductConfigItemContent.groovy")
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/config/EditProductConfigItemContent.ftl"
            )})
        }
    )
    public interface EditProductConfigItemContent {}

    @Screen(name = "EditProductConfigItemContentContent", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductConfigItemContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigItemContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductConfigItemContent")
    @Action(type = ActionType.SET, field = "configItemId", fromField = "parameters.configItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigItem", valueField = "configItem")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "requetParameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/config/EditProductConfigItemContentContent.groovy")
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/config/EditProductConfigItemContentContent.ftl"
            )})
        }
    )
    public interface EditProductConfigItemContentContent {}

    @Screen(name = "ProductConfigItemArticle", location = "component://product/widget/catalog/ConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductConfigItemContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ProductConfigItemArticle")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductConfigItemContent")
    @Action(type = ActionType.SET, field = "configItemId", fromField = "parameters.configItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductConfigItem", valueField = "configItem")
    @DecoratorScreen(
        name = "CommonConfigDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListProductConfigItem", location = "component://product/widget/catalog/ConfigForms.xml"
                )})})
        }
    )
    public interface ProductConfigItemArticle {}

}
