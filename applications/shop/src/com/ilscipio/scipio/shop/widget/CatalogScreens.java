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
public class CatalogScreens {

    @Screen(name = "choosecatalog", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/choosecatalog.ftl")}))
    public interface choosecatalog {}

    @Screen(name = "keywordsearchbox", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/KeywordSearchOptions.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/keywordsearchbox.ftl")}))
    public interface keywordsearchbox {}

    @Screen(name = "sidedeepcategory", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/SideDeepCategory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/sidedeepcategory.ftl")}))
    public interface sidedeepcategory {}

    @Screen(name = "minireorderprods", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/catalog/MiniReorderProds.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/minireorderprods.ftl")}))
    public interface minireorderprods {}

    @Screen(name = "miniassocprods", location = "component://shop/widget/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/miniassocprods.ftl")}))
    public interface miniassocprods {}

    @Screen(name = "minilastviewedcategories", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/SideDeepCategory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/minilastviewedcategories.ftl")}))
    public interface minilastviewedcategories {}

    @Screen(name = "minilastviewedproducts", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/catalog/MiniProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/minilastviewedproducts.ftl")}))
    public interface minilastviewedproducts {}

    @Screen(name = "minilastproductsearches", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/minilastproductsearches.ftl")}))
    public interface minilastproductsearches {}

    @Screen(name = "miniproductsummary", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/catalog/MiniProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/miniproductsummary.ftl")}))
    public interface miniproductsummary {}

    @Screen(name = "productsummary", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/ProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/productsummary.ftl")}))
    public interface productsummary {}

    @Screen(name = "breadcrumbs", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/SideDeepCategory.groovy")
    @Action(type = ActionType.SET, field = "useBreadcrumbsTitleFallback", value = "false", valueType = "Boolean")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/breadcrumbs.ftl")}))
    public interface breadcrumbs {}

    @Screen(name = "category", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Category.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${categoryTitle}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "LookupProductCategories")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs", location = "component://shop/widget/CatalogScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "category-include", location = "component://shop/widget/CatalogScreens.xml"
            )})
        }
    )
    public interface category {}

    @Screen(name = "category-include", location = "component://shop/widget/CatalogScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productCategory"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${detailScreen}")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCategoryNotFoundForCategoryID} ${productCategoryId}!", style = "head2")}))
    public interface category_include {}

    @Screen(name = "categorydetail", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "productCategoryLinkScreen", value = "component://shop/widget/CatalogScreens.xml#ProductCategoryLink")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/CategoryDetail.groovy")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductCategoryLink", list = "productCategoryLinks", useCache = true, filterByDate = true, fieldMaps = {@FieldMap(fieldName = "productCategoryId", fromField = "productCategoryId")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.SET, field = "paginateEcommerceStyle", value = "Y")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/categorydetail.ftl")}))
    public interface categorydetail {}

    @Screen(name = "categorydetailmatrix", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "numCol", value = "3")
    @Action(type = ActionType.SET, field = "searchInCategory", value = "N")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", defaultValue = "9")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "categorydetail")}))
    public interface categorydetailmatrix {}

    @Screen(name = "ProductCategoryLink", location = "component://shop/widget/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/ProductCategoryLink.ftl")}))
    public interface ProductCategoryLink {}

    @Screen(name = "product", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/shop/images/productAdditionalView.js", global = true)
    @Action(type = ActionType.SET, field = "configproductdetailScreen", value = "component://shop/widget/CatalogScreens.xml#configproductdetail")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Product.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${productTitle}")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"product"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "${detailScreen}"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductProductNotFound} ${productId}!", style = "head2"
                    )}))})
        }
    )
    public interface product {}

    @Screen(name = "productdetail", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductDetail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/productdetail.ftl")}))
    public interface productdetail {}

    @Screen(name = "inlineProductDetail", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/InlineProductDetail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/inlineProductDetail.ftl")}))
    public interface inlineProductDetail {}

    @Screen(name = "configproductdetail", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "inlineProductDetailScreen", value = "component://shop/widget/CatalogScreens.xml#inlineProductDetail")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductDetail.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/PrepareConfigForm.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/configproductdetail.ftl")}))
    public interface configproductdetail {}

    @Screen(name = "productreview", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductReview")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "inlineproductreview", location = "component://shop/widget/CatalogScreens.xml"
            )})
        }
    )
    public interface productreview {}

    @Screen(name = "inlineproductreview", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/ProductReview.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/productreview.ftl")}))
    public interface inlineproductreview {}

    @Screen(name = "lastviewedproducts", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLastViewProducts")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/lastviewedproducts.ftl"
            )})
        }
    )
    public interface lastviewedproducts {}

    @Screen(name = "tellafriend", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/tellafriend.ftl")}))
    public interface tellafriend {}

    @Screen(name = "quickadd", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "quickaddsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#quickaddsummary")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleQuickAdd")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/QuickAdd.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/quickadd.ftl"
            )})
        }
    )
    public interface quickadd {}

    @Screen(name = "quickaddsummary", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/ProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/quickaddsummary.ftl")}))
    public interface quickaddsummary {}

    @Screen(name = "keywordsearch", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", defaultValue = "10")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/KeywordSearch.groovy")
    @Action(type = ActionType.SET, field = "resetSearch", value = "false", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/CommonSearchOptions.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/keywordsearch.ftl"
            )})
        }
    )
    public interface keywordsearch {}

    @Screen(name = "advancedsearch", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAdvancedSearch")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "Advanced Search")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/AdvancedSearchOptions.groovy")
    @Action(type = ActionType.SET, field = "searchApplyDefaults", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/CommonSearchOptions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/GetCatalogCategoryTreeForSelect.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/advancedsearch.ftl"
            )})
        }
    )
    public interface advancedsearch {}

    @Screen(name = "LayeredNavBar", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/LayeredNavBar.ftl")}))
    public interface LayeredNavBar {}

    @Screen(name = "LayeredCategoryDetail", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://shop/widget/CatalogScreens.xml#productsummary")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/LayeredCategoryDetail.ftl")}))
    public interface LayeredCategoryDetail {}

    @Screen(name = "bestSellingCategory", location = "component://shop/widget/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "showBestSellingCategory")}))
    public interface bestSellingCategory {}

    @Screen(name = "showBestSellingCategory", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/catalog/BestSellingCategory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/ShowBestSellingCategory.ftl")}))
    public interface showBestSellingCategory {}

    @Screen(name = "productCategories", location = "component://shop/widget/CatalogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "LookupProductCategories")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/catalog/ProductCategories.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/catalog/ProductCategories.ftl")}))
    public interface productCategories {}

    @Screen(name = "productCategoryList", location = "component://shop/widget/CatalogScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.mainSubmitted"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "category")}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/shop/Category.groovy")
    @Action(type = ActionType.SET, field = "fromSetSessionLocale", value = "${groovy: return request.getAttribute('fromSetSessionLocale');}")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"fromSetSessionLocale", "equals", "true"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "category")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs", shareScope = true), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "category-include", shareScope = true)}))
    public interface productCategoryList {}

    @Screen(name = "compareProducts", location = "component://shop/widget/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "compareProducts", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")}))
    public interface compareProducts {}

    @Screen(name = "ProductUomDropDownOnly", location = "component://shop/widget/CatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ProductUomDropDownOnly", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")}))
    public interface ProductUomDropDownOnly {}

}
