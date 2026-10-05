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
public class CatalogCatalogScreens {

    @Screen(name = "FindCatalog", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCatalog")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindCatalog")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindCatalog")
    @Action(type = ActionType.SET, field = "isSpecificCatalog", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCatalogDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindCatalog", location = "component://product/widget/catalog/CatalogForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCatalog", location = "component://product/widget/catalog/CatalogForms.xml"
                    )}))})})
        }
    )
    public interface FindCatalog {}

    @Screen(name = "EditProdCatalog", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ProductCatalog")
    @Action(type = ActionType.SET, field = "prodCatalogId", fromField = "parameters.prodCatalogId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProdCatalog", valueField = "prodCatalog")
    @Action(type = ActionType.SET, field = "isCreateProdCatalog", value = "${groovy: !(context.prodCatalog || (parameters.prodCatalogId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateProdCatalog ? 'ProductNewCatalog' : 'ProductCatalog'}")
    @DecoratorScreen(
        name = "CommonCatalogDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioEditProdCatalog", location = "component://product/widget/catalog/CatalogScreens.xml"
                )})})
        }
    )
    public interface EditProdCatalog {}

    @Screen(name = "ScipioEditProdCatalog", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/EditProdCatalog.ftl")}))
    public interface ScipioEditProdCatalog {}

    @Screen(name = "EditProdCatalogSection", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCatalog")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ProductCatalog")
    @Action(type = ActionType.SET, field = "prodCatalogId", fromField = "parameters.prodCatalogId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProdCatalog", valueField = "prodCatalog")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCatalog")
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProdCatalog", location = "component://product/widget/catalog/CatalogForms.xml"
                )})})
        }
    )
    public interface EditProdCatalogSection {}

    @Screen(name = "EditProdCatalogCategories", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ProductCategories")
    @Action(type = ActionType.SET, field = "prodCatalogId", fromField = "parameters.prodCatalogId")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCatalogCategories")
    @DecoratorScreen(
        name = "CommonCatalogDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioAddProductCategoryToProdCatalog", location = "component://product/widget/catalog/CatalogScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductCatalogCategoryList}", includeScreens = {
                    @IncludeScreen(name = "ScipioProdCatalogCategoryList", location = "component://product/widget/catalog/CatalogScreens.xml"
                )})})
        }
    )
    public interface EditProdCatalogCategories {}

    @Screen(name = "ScipioAddProductCategoryToProdCatalog", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProdCatalogCategoryType", list = "prodCatalogCategoryTypes")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/AddProductCategoryToProdCatalog.ftl")}))
    public interface ScipioAddProductCategoryToProdCatalog {}

    @Screen(name = "ScipioProdCatalogCategoryList", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProdCatalogCategory", list = "prodCatalogCategories", fieldMaps = {@FieldMap(fieldName = "prodCatalogId", fromField = "parameters.prodCatalogId")}, orderBy = {"prodCatalogCategoryTypeId", "sequenceNum", "productCategoryId"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/ProdCatalogCategoryList.ftl")}))
    public interface ScipioProdCatalogCategoryList {}

    @Screen(name = "EditProdCatalogParties", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PartyParties")
    @Action(type = ActionType.SET, field = "prodCatalogId", fromField = "parameters.prodCatalogId", global = true)
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCatalogParties")
    @DecoratorScreen(
        name = "CommonCatalogDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioAddProdCatalogToParty", location = "component://product/widget/catalog/CatalogScreens.xml"
                )}),
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioProdCatalogPartyList", location = "component://product/widget/catalog/CatalogScreens.xml"
                )})})
        }
    )
    public interface EditProdCatalogParties {}

    @Screen(name = "ScipioAddProdCatalogToParty", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "RoleType", list = "roleTypes", orderBy = {"description"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/AddProdCatalogToParty.ftl")}))
    public interface ScipioAddProdCatalogToParty {}

    @Screen(name = "ScipioProdCatalogPartyList", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProdCatalogRole", list = "prodCatalogRoleList", fieldMaps = {@FieldMap(fieldName = "prodCatalogId", fromField = "parameters.prodCatalogId")}, orderBy = {"sequenceNum", "partyId"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"prodCatalogRoleList"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductCatalogPartyList}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/catalog/ProdCatalogPartyList.ftl")})}))
    public interface ScipioProdCatalogPartyList {}

    @Screen(name = "ScipioAddProdCatalogToStore", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStore", list = "productStoreList", orderBy = {"storeName"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/AddProdCatalogToStore.ftl")}))
    public interface ScipioAddProdCatalogToStore {}

    @Screen(name = "ScipioProdCatalogStoreList", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductStoreCatalog", list = "productStoreCatalogList", fieldMaps = {@FieldMap(fieldName = "prodCatalogId", fromField = "parameters.prodCatalogId")}, orderBy = {"sequenceNum", "productStoreId"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/catalog/ProdCatalogStoreList.ftl")}))
    public interface ScipioProdCatalogStoreList {}

    @Screen(name = "ScipioEditCatalogTreePage", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "productStoreId")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/content/images/ScpContentCommon.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/product/WEB-INF/actions/generated/ScipioEditCatalogTreePage_script1.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/catalog/ScpCatalogCommon.js?t=${ScpEgltCommon}", global = true)
    @DecoratorScreen(
        name = "CommonCatalogDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioEditCatalogTree", location = "component://product/widget/catalog/CatalogScreens.xml"
            )})
        }
    )
    public interface ScipioEditCatalogTreePage {}

    @Screen(name = "ScipioEditCatalogTree", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/catalog/tree/EditCatalogTree.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productStoreId"}), @Condition(type = NotEmpty.class, params = {"treeMenuData"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${productStore.storeName}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/catalog/tree/EditCatalogTree.ftl")})}))
    public interface ScipioEditCatalogTree {}

    @Screen(name = "ScipioViewCatalogTree", location = "component://product/widget/catalog/CatalogScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/catalog/tree/ViewCatalogTree.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productStoreId"}), @Condition(type = NotEmpty.class, params = {"treeMenuData"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${productStore.storeName}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/catalog/tree/ViewCatalogTree.ftl")})}))
    public interface ScipioViewCatalogTree {}

}
