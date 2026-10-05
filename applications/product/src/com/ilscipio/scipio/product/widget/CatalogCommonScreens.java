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
public class CatalogCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.ProductCatalogCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.ProductCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "catalogmgr", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "CatalogAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://product/widget/catalog/CatalogMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.ProductCatalog}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @Action(type = ActionType.SET, field = "showMainRegularBar", fromField = "showMainRegularBar", defaultValue = "true")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", fromField = "showMainExtendedBar", defaultValue = "false")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )})),
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"showMainExtendedBar"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "leftbar")})
                )}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonCatalogAppDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonCatalogAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifHasPermission = {@IfHasPermission(permission = "CATALOG", action = "_ADMIN"), @IfHasPermission(permission = "CATALOG", action = "_CREATE"), @IfHasPermission(permission = "CATALOG", action = "_UPDATE"), @IfHasPermission(permission = "CATALOG", action = "_VIEW")})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonCatalogAppSideBarMenu", location = "component://product/widget/catalog/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonCatalogAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalogViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonCatalogAppDecorator {}

    @Screen(name = "CommonCatalogDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"isCreateProdCatalog"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = False.class, params = {"isSpecificCatalog"})})}), then = @Actions(value = {@Action(order = 1, type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${prodCatalogId}")}, ifs = {@IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"prodCatalog"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "prodCatalogId", fromField = "parameters.prodCatalogId", global = true), @Action(type = ActionType.ENTITY_ONE, entityName = "ProdCatalog", valueField = "prodCatalog")}), elseActions = @Actions2(value = {@Action(type = ActionType.SET, field = "prodCatalogId", fromField = "prodCatalog.prodCatalogId", global = true)}))}))
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"prodCatalog"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Catalog")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "TOP"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "catalogs")}))
    @Action(order = 3, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 4, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 5, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${titleElemName} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Or.class, tree = {
                        @ConditionNode(not = true, type = True.class, params = {"isCreateProdCatalog"
                    })})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "CatalogSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface CommonCatalogDecorator {}

    @Screen(name = "CommonCategoryDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"isCreateCategory"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = False.class, params = {"isSpecificCategory"})})}), then = @Actions(value = {@Action(order = 1, type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${productCategoryId}")}, ifs = {@IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"productCategory"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "productCategoryId", fromField = "parameters.productCategoryId"), @Action(type = ActionType.ENTITY_ONE, entityName = "ProductCategory", valueField = "productCategory")}), elseActions = @Actions2(value = {@Action(type = ActionType.SET, field = "productCategoryId", fromField = "productCategory.productCategoryId")}))}))
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productCategory"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Category")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "TOP"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "categories")}))
    @Action(order = 3, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 4, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 5, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${titleElemName} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"productCategory"
                }),
                @Condition(type = NotEmpty.class, params = {"TabBarName"})}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "${TabBarName}", location = "component://product/widget/catalog/CatalogMenus.xml"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "CategorySubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                )}), position = 0)})
        }
    )
    public interface CommonCategoryDecorator {}

    @Screen(name = "CommonProductDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 1, type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 2, type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"isCreateProduct"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = False.class, params = {"isSpecificProduct"})})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId", global = true), @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product"), @Action(type = ActionType.SET, field = "product", fromField = "product", global = true), @Action(type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${productId}")}))
    @IfAction(order = 4, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"product"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Product")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "TOP"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "products")}))
    @Action(order = 5, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 6, type = ActionType.SET, field = "qualifiedLabel", value = "\\${finalTitle}")
    @Action(order = 7, type = ActionType.SET, field = "prefixedQualifiedLabel", value = "${uiLabelMap.ProductProductQualifiedLabel}")
    @Action(order = 8, type = ActionType.SET, field = "titleFormatPrefix", value = "${groovy: (context.labelTitlePropertyFull == true || !context.labelTitleProperty) ? context.qualifiedLabel : context.prefixedQualifiedLabel}")
    @Action(order = 9, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 10, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "${titleFormatPrefix} ${titleElemName} ${${extraFunctionName}}")
    @Action(order = 11, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/SetProductLastProduct.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Or.class, tree = {
                        @ConditionNode(not = true, type = True.class, params = {"isCreateProduct"
                    })})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/menu/ProductSubTabBar.ftl"
                    )}), position = 0)})
        }
    )
    public interface CommonProductDecorator {}

    @Screen(name = "CommonConfigDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"configItem"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#ConfigItem")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "component://product/widget/catalog/CatalogMenus.xml#Product"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigs")}))
    @Action(order = 1, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 2, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 3, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.configItemId} ${${extraFunctionName}}")
    @Action(order = 4, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/GetProductLastProduct.groovy")
    @Action(order = 5, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/SetProductLastProduct.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ConfigItemSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonConfigDecorator {}

    @Screen(name = "CommonFeatureDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productId"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "component://product/widget/catalog/CatalogMenus.xml#Product")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Features")}))
    @Action(order = 2, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 3, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/GetProductLastProduct.groovy")
    @Action(order = 4, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/SetProductLastProduct.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFeatureDecorator {}

    @Screen(name = "CommonSpecificFeatureDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.productFeatureId} ${${extraFunctionName}}")
    @Action(type = ActionType.SET, field = "productFeatureId", fromField = "productFeatureId", defaultValue = "${parameters.productFeatureId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductFeature", valueField = "productFeature")
    @DecoratorScreen(
        name = "CommonFeatureDecorator",
        location = "component://product/widget/catalog/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FeaturesSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonSpecificFeatureDecorator {}

    @Screen(name = "CommonProductFeaturesDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Product")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "component://product/widget/catalog/CommonScreens.xml"
    )
    public interface CommonProductFeaturesDecorator {}

    @Screen(name = "CommonPromoDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Promo")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.productPromoId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "PromoSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonPromoDecorator {}

    @Screen(name = "CommonPromoCodeDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Promo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "FindProductPromoCode")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "promoCodeTitleFormat", value = "\\${finalTitle} ${parameters.productPromoCodeId} ${${extraFunctionName}}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "${groovy: (context.usePromoCodeTitle == true) ? context.promoCodeTitleFormat : ''}")
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://product/widget/catalog/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonPromoCodeDecorator {}

    @Screen(name = "CommonProductStoreDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 1, type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 2, type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"isCreateProductStore"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = False.class, params = {"isSpecificProductStore"})})}), then = @Actions(value = {@Action(order = 1, type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${productStoreId}")}, ifs = {@IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"productStore"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "productStoreId", fromField = "productStoreId", defaultValue = "${parameters.productStoreId}"), @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "productStoreId", fromField = "productStoreId")})}), elseActions = @Actions2(value = {@Action(type = ActionType.SET, field = "productStoreId", fromField = "productStore.productStoreId")}))}))
    @IfAction(order = 4, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productStore"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#ProductStore")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "TOP"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "store")}))
    @Action(order = 5, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 6, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 7, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${titleElemName} ${${extraFunctionName}}")
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ProductStoreSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonProductStoreDecorator {}

    @Screen(name = "CommonProductStoreGroupDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @IfAction(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.productStoreGroupId"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#ProductStore"), @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "EditProductStoreGroups")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", value = "component://product/widget/catalog/CatalogMenus.xml#ProductStore"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreGroups")}))
    @Action(order = 2, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 3, type = ActionType.SET, field = "productStoreId", fromField = "productStoreId", defaultValue = "${parameters.productStoreId}")
    @Action(order = 4, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 5, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${productStoreId} ${${extraFunctionName}}")
    @Action(order = 6, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/GetStoreLastProductStore.groovy")
    @Action(order = 7, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonProductStoreGroupDecorator {}

    @Screen(name = "CommonShippingDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Shipping")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "ListShipmentMethodTypes")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "productStoreId", defaultValue = "${parameters.productStoreId}")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${productStoreId} ${${extraFunctionName}}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/GetStoreLastProductStore.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonShippingDecorator {}

    @Screen(name = "CommonPriceDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "TOP")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "pricerules")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.productPriceRuleId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonPriceDecorator {}

    @Screen(name = "CommonCarrierDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Carriers")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "ListCarrierShipmentMethods")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonCarrierDecorator {}

    @Screen(name = "CommonWebSiteDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 1, type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 2, type = ActionType.SET, field = "productStoreId", fromField = "productStoreId", defaultValue = "${parameters.productStoreId}")
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"isCreateWebSite"})}), then = @Actions(value = {@Action(order = 1, type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${webSiteId}")}, ifs = {@IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"webSite"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "webSiteId", fromField = "webSiteId", defaultValue = "${parameters.webSiteId}"), @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")}), elseActions = @Actions2(value = {@Action(type = ActionType.SET, field = "webSiteId", fromField = "webSite.webSiteId")}))}))
    @Action(order = 4, type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#WebSite")
    @Action(order = 5, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 6, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${titleElemName} ${${extraFunctionName}}")
    @Action(order = 7, type = ActionType.SET, field = "getStoreLastProductStore.overrideProductStoreId", fromField = "webSite.productStoreId")
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/GetStoreLastProductStore.groovy")
    @Action(order = 9, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @Action(order = 10, type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, containers = {
                    @Container(includeMenus = {
                        @IncludeMenu(name = "websiteMenu", location = "component://content/widget/content/ContentMenus.xml"
                    )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ContentViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CommonWebSiteDecorator {}

    @Screen(name = "CommonWebAnalyticsDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 1, type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 2, type = ActionType.SET, field = "productStoreId", fromField = "productStoreId", defaultValue = "${parameters.productStoreId}")
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"webSite"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "webSiteId", fromField = "webSiteId", defaultValue = "${parameters.webSiteId}"), @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")}))
    @Action(order = 4, type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#WebSite")
    @Action(order = 5, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 6, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.webSiteId} ${${extraFunctionName}}")
    @Action(order = 7, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/GetStoreLastProductStore.groovy")
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @Action(order = 9, type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, containers = {
                    @Container(style = "button-bar", includeMenus = {
                        @IncludeMenu(name = "WebAnalyticsConfigButtonBar", location = "component://content/widget/content/ContentMenus.xml"
                    )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ContentViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CommonWebAnalyticsDecorator {}

    @Screen(name = "CommonContentDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#Content")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "component://content/widget/CommonScreens.xml"
    )
    public interface CommonContentDecorator {}

    @Screen(name = "leftbar", location = "component://product/widget/catalog/CommonScreens.xml")
    public interface leftbar {}

    @Screen(name = "keywordsearchbox", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/keywordsearchbox.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductSearchProducts}", name = "ProductKeywordsPanel", collapsible = true, htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/find/keywordsearchbox.ftl")})}))
    public interface keywordsearchbox {}

    @Screen(name = "sidecatalogs", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/sidecatalogs.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductBrowseCatalogs}", name = "ProductBrowseCatalogsPanel", collapsible = true, htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/find/sidecatalogs.ftl")})}))
    public interface sidecatalogs {}

    @Screen(name = "sidedeepcategory", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/sidedeepcategory.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductBrowseCategories}", name = "ProductBrowseCategoriesPanel", collapsible = true, htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/find/sidedeepcategory.ftl")})}))
    public interface sidedeepcategory {}

    @Screen(name = "miniproductlist", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/miniproductlist.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductCategoryProducts}", name = "ProductCategoryProductsPanel", collapsible = true, containers = {@Container(id = "miniproductlist", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/find/miniproductlist.ftl")})})}))
    public interface miniproductlist {}

    @Screen(name = "ChooseTopCategory", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleChooseTopCategory")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/ChooseTopCategory.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "leftbar", location = "component://product/widget/catalog/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewCategory}", style = "${styles.link_nav} ${styles.action_add}", target = "EditCategory"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.ProductCategoryWithNoParent}", includeForms = {
                        @IncludeForm(name = "ListTopCategory", location = "component://product/widget/catalog/CategoryForms.xml"
                    )}, position = 1)})
        }
    )
    public interface ChooseTopCategory {}

    @Screen(name = "FastLoadCache", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFastLoadCatalogIntoCache")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/FastLoadCache.groovy")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "leftbar", location = "component://product/widget/catalog/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/FastLoadCache.ftl"
            )})
        }
    )
    public interface FastLoadCache {}

    @Screen(name = "categorytree", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/category/CategoryTree.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductBrowseCatalogeAndCategories}", name = "ProductBrowseCategoriesPanel", collapsible = true, htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/category/CategoryTree.ftl", position = 1)}, containers = {@Container(id = "EditDocumentTree", position = 0)})}))
    public interface categorytree {}

    @Screen(name = "ProductStoreGroupTree", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreGroup", valueField = "parentProductStoreGroup")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreGroup", list = "parentProductStoreGroups", conditions = {@ConditionExpr(fieldName = "primaryParentGroupId", fromField = "nullField")})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductParentProductStoreGroups}", name = "ProductStoreGroupPanel", collapsible = true, htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/store/ProductStoreGroupTree.ftl", position = 1)}, containers = {@Container(id = "EditDocumentTree", position = 0)})}))
    public interface ProductStoreGroupTree {}

    @Screen(name = "main", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductCatalogManager")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStore", list = "productStores")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/ViewProdCatalogs.ftl"
            )}, containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioRecentProductsAdded", location = "component://product/widget/catalog/CommonScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}"
                )})})
        }
    )
    public interface main {}

    @Screen(name = "ImageManagementDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#ImageManagement")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface ImageManagementDecorator {}

    @Screen(name = "CommonReviewDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "TOP")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "productReviews")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonReviewDecorator {}

    @Screen(name = "CommonThesaurusDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "TOP")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "thesaurus")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonThesaurusDecorator {}

    @Screen(name = "CommonSubscriptionDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"isCreateSubscription"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = False.class, params = {"isSpecificSubscription"})})}), then = @Actions(value = {@Action(order = 1, type = ActionType.SET, field = "titleElemName", fromField = "titleElemName", defaultValue = "${subscriptionId}")}, ifs = {@IfAction2(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"subscription"})}), then = @Actions2(value = {@Action(type = ActionType.SET, field = "subscriptionId", fromField = "parameters.subscriptionId"), @Action(type = ActionType.ENTITY_ONE, entityName = "Subscription", valueField = "subscription")}), elseActions = @Actions2(value = {@Action(type = ActionType.SET, field = "subscriptionId", fromField = "subscription.subscriptionId")}))}))
    @Action(order = 1, type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Subscriptions")
    @Action(order = 2, type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Subscriptions")
    @Action(order = 3, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 4, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 5, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${titleElemName} ${${extraFunctionName}}")
    @Action(order = 6, type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"subscription"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "EditSubscription", location = "component://product/widget/catalog/SubscriptionMenus.xml"
                        )}, containers = {
                            @Container2(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewSubscription}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSubscription"
                            )})}), failWidgets = @WidgetsForContainer(sections = {
                                @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "EditSubscription"
                                })}), widgets = @WidgetsForContainer2(containers = {
                                    @Container3(style = "button-bar", widgets = {
                                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewSubscription}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSubscription"
                                    )})}))}), position = 0)}), failWidgets = @InlineWidgets(value = {
                                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductSubscriptionViewPermissionError}", style = "common-msg-error-perm"
                                    )}))})
        }
    )
    public interface CommonSubscriptionDecorator {}

    @Screen(name = "CommonSubscriptionResourceDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"subscriptionResource"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "subscriptionResourceId", fromField = "parameters.subscriptionResourceId"), @Action(type = ActionType.ENTITY_ONE, entityName = "SubscriptionResource", valueField = "subscriptionResource")}))
    @Action(order = 1, type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Subscription")
    @Action(order = 2, type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @Action(order = 3, type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(order = 4, type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.subscriptionResourceId} ${${extraFunctionName}}")
    @Action(order = 5, type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"subscriptionResource"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "EditSubscriptionResource", location = "component://product/widget/catalog/SubscriptionMenus.xml"
                    )}, containers = {
                        @Container2(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewSubscriptionResource}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSubscriptionResource"
                        ),
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductFindResourceSubscriptions}", style = "${styles.link_nav} ${styles.action_find}", target = "FindSubscription"
                    )})}), position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductSubscriptionResourceViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CommonSubscriptionResourceDecorator {}

    @Screen(name = "CommonFindDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonFindDecorator {}

    @Screen(name = "listMiniproduct", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/miniproductlist.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/miniproductlist.ftl")}))
    public interface listMiniproduct {}

    @Screen(name = "ScipioViewCatalogTree", location = "component://product/widget/catalog/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioViewCatalogTree", location = "component://product/widget/catalog/CatalogScreens.xml")}))
    public interface ScipioViewCatalogTree {}

    @Screen(name = "ScipioStoreCatalogTree", location = "component://product/widget/catalog/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioViewCatalogTree", location = "component://product/widget/catalog/CatalogScreens.xml")}))
    public interface ScipioStoreCatalogTree {}

    @Screen(name = "ScipioRecentProductsAdded", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/script/com/ilscipio/product/dashboard/NewProducts.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"products"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductRecentProductsAdded}", includeForms = {@IncludeForm(name = "newProducts", location = "component://product/widget/catalog/CatalogForms.xml")})}))
    public interface ScipioRecentProductsAdded {}

    @Screen(name = "ScipioPromotionsList", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "userEntered", fromField = "parameters.userEntered")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromo", list = "productPromos", conditions = {@ConditionExpr(fieldName = "userEntered", fromField = "userEntered", ignoreIfEmpty = true)}, orderBy = {"-createdDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productPromos"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductProductPromotionsList}", includeForms = {@IncludeForm(name = "ListProductPromos", location = "component://product/widget/catalog/PromoForms.xml")})}))
    public interface ScipioPromotionsList {}

    @Screen(name = "ScpCatalogCommon.js", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getUserPreferenceGroup", resultMapName = "prefResult", fieldMaps = {@FieldMap(fieldName = "userPrefGroupTypeId", value = "GLOBAL_PREFERENCES")})
    @Action(type = ActionType.SET, field = "userPreferences", fromField = "prefResult.userPrefMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/static/ScpCatalogCommon.js.ftl")}))
    public interface ScpCatalogCommon_js {}

    @Screen(name = "MainSideBarMenu", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://product/widget/catalog/CatalogMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "CatalogAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://product/widget/catalog/CatalogMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonCatalogAppSideBarMenu", location = "component://product/widget/catalog/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonCatalogAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifHasPermission = {@IfHasPermission(permission = "CATALOG", action = "_ADMIN"), @IfHasPermission(permission = "CATALOG", action = "_CREATE"), @IfHasPermission(permission = "CATALOG", action = "_UPDATE"), @IfHasPermission(permission = "CATALOG", action = "_VIEW")})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonCatalogAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonCatalogAppSideBarMenu {}

}
