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
public class OrdermgrOrderEntryCatalogScreens {

    @Screen(name = "choosecatalog", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ChooseCatalog.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/choosecatalog.ftl")}))
    public interface choosecatalog {}

    @Screen(name = "keywordsearchbox", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/KeywordSearchOptions.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/keywordsearchbox.ftl")}))
    public interface keywordsearchbox {}

    @Screen(name = "sidedeepcategory", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/SideDeepCategory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/sidedeepcategory.ftl")}))
    public interface sidedeepcategory {}

    @Screen(name = "productsummary", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "backendPath", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/productsummary.ftl")}))
    public interface productsummary {}

    @Screen(name = "breadcrumbs", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/SideDeepCategory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/breadcrumbs.ftl")}))
    public interface breadcrumbs {}

    @Screen(name = "compareproductslist", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/compareproductslist.ftl")}))
    public interface compareproductslist {}

    @Screen(name = "category", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/Category.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonCategory}: ${categoryTitle}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/SetTrailFromCategory.groovy")
    @Action(type = ActionType.SET, field = "showCategoryLinks", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "category-include", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, content = "<@alert type=\"info\">${uiLabelMap.OrderCategoryNonRecursiveProductsOnlyInfo}</@alert>"
            )})
        }
    )
    public interface category {}

    @Screen(name = "category-include", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"detailScreen"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/Category.groovy")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productCategory"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${detailScreen}")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCategoryNotFoundForCategoryID} ${productCategoryId}!", style = "common-msg-error")}))
    public interface category_include {}

    @Screen(name = "categorydetail", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/CategoryDetail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/categorydetail.ftl")}))
    public interface categorydetail {}

    @Screen(name = "product", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "configproductdetailScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#configproductdetail")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/Product.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonProduct}: ${productTitle}")
    @Action(type = ActionType.SET, field = "showProductLinks", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/SetTrailFromProduct.groovy")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"product"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "${detailScreen}"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductProductNotFound} ${productId}!", style = "common-msg-error"
                    )}))})
        }
    )
    public interface product {}

    @Screen(name = "productdetail", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductDetail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/productdetail.ftl")}))
    public interface productdetail {}

    @Screen(name = "configproductdetail", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SET, field = "inlineProductDetailScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#inlineProductDetail")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductDetail.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/PrepareConfigForm.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "breadcrumbs"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/configproductdetail.ftl")}))
    public interface configproductdetail {}

    @Screen(name = "inlineProductDetail", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/InlineProductDetail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/inlineProductDetail.ftl")}))
    public interface inlineProductDetail {}

    @Screen(name = "keywordsearch", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SET, field = "productsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#productsummary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/KeywordSearch.groovy")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/keywordsearch.ftl"
            )})
        }
    )
    public interface keywordsearch {}

    @Screen(name = "advancedsearch", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAdvancedSearch")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/AdvancedSearchOptions.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRoleAndPartyDetail", list = "supplerPartyRoleAndPartyDetails", conditions = {@ConditionExpr(fieldName = "roleTypeId", value = "SUPPLIER")}, orderBy = {"groupName", "lastName", "firstName"})
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/advancedsearch.ftl"
            )})
        }
    )
    public interface advancedsearch {}

    @Screen(name = "quickadd", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleQuickAdd")
    @Action(type = ActionType.SET, field = "quickaddsummaryScreen", value = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#quickaddsummary")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/QuickAdd.groovy")
    @DecoratorScreen(
        name = "CommonOrderCatalogDecorator",
        location = "component://order/widget/ordermgr/OrderEntryCommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/quickadd.ftl"
            )})
        }
    )
    public interface quickadd {}

    @Screen(name = "quickaddsummary", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductSummary.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/quickaddsummary.ftl")}))
    public interface quickaddsummary {}

    @Screen(name = "compareProducts", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductCompareProducts")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", fromField = "uiLabelMap.ProductCompareProducts")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/CompareProducts.groovy")
    @DecoratorScreen(
        name = "CommonPopUpDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/compareproducts.ftl"
            )})
        }
    )
    public interface compareProducts {}

    @Screen(name = "ProductUomDropDownOnly", location = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/ProductUomDropDownOnly.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"product"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/catalog/ProductUomDropDownOnly.ftl")}))
    public interface ProductUomDropDownOnly {}

}
