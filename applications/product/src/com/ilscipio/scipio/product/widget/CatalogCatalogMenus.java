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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class CatalogCatalogMenus {

    @Menu(
        name = "CatalogAppBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        title = "${uiLabelMap.ProductCatalog}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "catalogs", title = "${uiLabelMap.ProductCatalogs}", link = @MenuLink(target = "FindCatalog", parameters = {@MenuParameter(paramName = "find", value = "true")})),
            @MenuItem(name = "categories", title = "${uiLabelMap.ProductCategories}", link = @MenuLink(target = "FindCategory")),
            @MenuItem(name = "products", title = "${uiLabelMap.ProductProducts}", link = @MenuLink(target = "FindProduct")),
            @MenuItem(name = "productReviews", title = "${uiLabelMap.ProductReviews}", link = @MenuLink(target = "FindReviews")),
            @MenuItem(name = "store", title = "${uiLabelMap.ProductStores}", link = @MenuLink(target = "FindProductStore")),
            @MenuItem(name = "pricerules", title = "${uiLabelMap.ProductPrices}", link = @MenuLink(target = "FindProductPriceRules")),
            @MenuItem(name = "promos", title = "${uiLabelMap.ProductPromotions}", link = @MenuLink(target = "FindProductPromo")),
            @MenuItem(name = "subscriptions", title = "${uiLabelMap.ProductSubscriptions}", link = @MenuLink(target = "FindSubscription")),
            @MenuItem(name = "carriers", title = "${uiLabelMap.ProductCarriers}", link = @MenuLink(target = "ListCarrierShipmentMethods")),
            @MenuItem(name = "features", title = "${uiLabelMap.ProductFeatures}", link = @MenuLink(target = "ListFeatures"))
        }
    )
    public interface CatalogAppBar {}

    @Menu(
        name = "CatalogAppSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        title = "${uiLabelMap.ProductCatalog}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CatalogAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "catalogs", subMenus = {@SubMenu(name = "Catalog", include = "component://product/widget/catalog/CatalogMenus.xml#CatalogSideBar")}),
            @MenuItem(name = "categories", subMenus = {@SubMenu(name = "Category", include = "component://product/widget/catalog/CatalogMenus.xml#CategorySideBar")}),
            @MenuItem(name = "products", subMenus = {@SubMenu(name = "Product", include = "component://product/widget/catalog/CatalogMenus.xml#ProductSideBar")}),
            @MenuItem(name = "store", subMenus = {@SubMenu(name = "ProductStore", include = "component://product/widget/catalog/CatalogMenus.xml#ProductStoreSideBar")}),
            @MenuItem(name = "promos", subMenus = {@SubMenu(name = "Promo", include = "component://product/widget/catalog/CatalogMenus.xml#PromoSideBar")}),
            @MenuItem(name = "subscriptions", subMenus = {@SubMenu(name = "Subscriptions", include = "component://product/widget/catalog/CatalogMenus.xml#SubscriptionSideBar")}),
            @MenuItem(name = "carriers", subMenus = {@SubMenu(name = "Carriers", include = "component://product/widget/catalog/CatalogMenus.xml#CarrierSideBar")}),
            @MenuItem(name = "features", subMenus = {@SubMenu(name = "Features", include = "component://product/widget/catalog/CatalogMenus.xml#FeaturesSideBar")})
        }
    )
    public interface CatalogAppSideBar {}

    @Menu(
        name = "CatalogTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ProductCatalog", title = "${uiLabelMap.ProductCatalog}", link = @MenuLink(target = "EditProdCatalog", parameters = {@MenuParameter(paramName = "prodCatalogId", fromField = "prodCatalogId")})),
            @MenuItem(name = "PartyParties", title = "${uiLabelMap.ProductCatalogParties}", link = @MenuLink(target = "EditProdCatalogParties", parameters = {@MenuParameter(paramName = "prodCatalogId", fromField = "prodCatalogId")})),
            @MenuItem(name = "ProductCategories", title = "${uiLabelMap.ProductCatalogCategories}", link = @MenuLink(target = "EditProdCatalogCategories", parameters = {@MenuParameter(paramName = "prodCatalogId", fromField = "prodCatalogId")}))
        }
    )
    public interface CatalogTabBar {}

    @Menu(
        name = "CatalogSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CatalogTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface CatalogSideBar {}

    @Menu(
        name = "CatalogSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductCatalog", title = "${uiLabelMap.ProductNewProdCatalog}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = True.class, params = {"isCreateProdCatalog"})}), link = @MenuLink(target = "EditProdCatalog"))
        }
    )
    public interface CatalogSubTabBar {}

    @Menu(
        name = "ProductFeaturesSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Feature", title = "${uiLabelMap.ProductFeature}", link = @MenuLink(target = "EditProductFeatures", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")}))
        }
    )
    public interface ProductFeaturesSubTabBar {}

    @Menu(
        name = "ProductFeaturesSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductFeaturesSubTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ProductFeaturesSideBar {}

    @Menu(
        name = "FeaturesTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListFeatures", title = "${uiLabelMap.ProductFeature}", link = @MenuLink(target = "ListFeatures")),
            @MenuItem(name = "FeatureType", title = "${uiLabelMap.ProductFeatureType}", link = @MenuLink(target = "EditFeatureTypes")),
            @MenuItem(name = "FeatureCategory", title = "${uiLabelMap.ProductFeatureCategory}", link = @MenuLink(target = "EditFeatureCategories")),
            @MenuItem(name = "FeatureGroup", title = "${uiLabelMap.ProductFeatureGroup}", link = @MenuLink(target = "EditFeatureGroups")),
            @MenuItem(name = "FeatureInterAction", title = "${uiLabelMap.ProductFeatureInteraction}", link = @MenuLink(target = "EditFeatureInterActions"))
        }
    )
    public interface FeaturesTabBar {}

    @Menu(
        name = "FeaturesSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FeaturesTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface FeaturesSideBar {}

    @Menu(
        name = "FeaturesSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditFeature", title = "${uiLabelMap.ProductNewFeature}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateFeature", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditFeature")),
            @MenuItem(name = "EditFeatureCategoryFeatures", title = "${uiLabelMap.ProductGoToFeatureCategory} ${productFeature.productFeatureCategoryId}", widgetStyle = "+${styles.action_nav}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"productFeature"}), @Condition(type = NotEmpty.class, params = {"productFeature.productFeatureCategoryId"})}), link = @MenuLink(target = "EditFeatureCategoryFeatures", parameters = {@MenuParameter(paramName = "productFeatureCategoryId", fromField = "productFeature.productFeatureCategoryId")}))
        }
    )
    public interface FeaturesSubTabBar {}

    @Menu(
        name = "FeatureTypeSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditFeatureType", title = "${uiLabelMap.ProductNewFeatureType}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateFeatureType", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditFeatureType"))
        }
    )
    public interface FeatureTypeSubTabBar {}

    @Menu(
        name = "FeatureInterActionSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditFeatureInterAction", title = "${uiLabelMap.ProductNewFeatureInterAction}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateFeatureInterAction", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditFeatureInterAction"))
        }
    )
    public interface FeatureInterActionSubTabBar {}

    @Menu(
        name = "ShippingTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListShipmentMethodTypes", title = "${uiLabelMap.ProductShipmentMethodTypes}", link = @MenuLink(target = "ListShipmentMethodTypes")),
            @MenuItem(name = "EditProductStoreShipSetup", title = "${uiLabelMap.ProductStoreShippingSetup}", link = @MenuLink(target = "EditProductStoreShipSetup", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreShipmentCostEstimates", title = "${uiLabelMap.ProductViewEstimates}", link = @MenuLink(target = "EditProductStoreShipmentCostEstimates", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")}))
        }
    )
    public interface ShippingTabBar {}

    @Menu(
        name = "ShippingSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ShippingTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ShippingSideBar {}

    @Menu(
        name = "CarrierSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "CreateCarrier", title = "${uiLabelMap.ProductNewCarrier}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "addCarrier"))
        }
    )
    public interface CarrierSubTabBar {}

    @Menu(
        name = "CarrierTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListQuantityBreaks", title = "${uiLabelMap.ProductQuantityBreaks}", link = @MenuLink(target = "ListQuantityBreaks")),
            @MenuItem(name = "ListCarrierShipmentMethods", title = "${uiLabelMap.ProductCarrierShipmentMethods}", link = @MenuLink(target = "ListCarrierShipmentMethods"))
        }
    )
    public interface CarrierTabBar {}

    @Menu(
        name = "CarrierSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CarrierTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface CarrierSideBar {}

    @Menu(
        name = "CategoryTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditCategory", title = "${uiLabelMap.ProductCategory}", link = @MenuLink(target = "EditCategory", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "EditCategoryContent", title = "${uiLabelMap.ProductCategoryContent}", link = @MenuLink(target = "EditCategoryContent", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "EditCategoryRollup", title = "${uiLabelMap.ProductCategoryAssociations}", link = @MenuLink(target = "EditCategoryRollup", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "EditCategoryProducts", title = "${uiLabelMap.ProductCategoryProds}", link = @MenuLink(target = "EditCategoryProducts", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "EditCategoryAttributes", title = "${uiLabelMap.ProductCategoryAttributes}", link = @MenuLink(target = "EditCategoryAttributes", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")}))
        }
    )
    public interface CategoryTabBar {}

    @Menu(
        name = "CategorySideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CategoryTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface CategorySideBar {}

    @Menu(
        name = "CategorySubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(propertyToField = {@PropertyToFieldAction(resource = "catalog", property = "shop.default.link.category.prefix", field = "categoryPageUrl")}),
        items = {
            @MenuItem(name = "EditCategory", title = "${uiLabelMap.ProductNewCategory}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateCategory", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditCategory")),
            @MenuItem(name = "createProductInCategoryStart", title = "${uiLabelMap.ProductCreateProductInCategory}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "createProductInCategoryStart", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "DuplicateCategory", title = "${uiLabelMap.ProductDuplicateCategory}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "EditCategory", parameters = {@MenuParameter(paramName = "duplicateCategory", value = "Y"), @MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "AdvancedSearch", title = "${uiLabelMap.ProductSearchInCategory}", widgetStyle = "+search", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "advancedsearch", parameters = {@MenuParameter(paramName = "SEARCH_CATEGORY_ID", fromField = "productCategoryId")})),
            @MenuItem(name = "ProductCategoryPage", title = "${uiLabelMap.CommonShopPage}", widgetStyle = "+website", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"productCategory"}), @Condition(type = NotEmpty.class, params = {"categoryPageUrl"})}), link = @MenuLink(target = "${categoryPageUrl}${productCategory.productCategoryId}", targetWindow = "_blank", urlMode = UrlMode.PLAIN))
        }
    )
    public interface CategorySubTabBar {}

    @Menu(
        name = "CategoryContentSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewContentInCategory", title = "${uiLabelMap.ContentNewContent}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "EditCategoryContent", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId")})),
            @MenuItem(name = "AddExistingContentInCategory", title = "${uiLabelMap.ProductAddExistingContentInCategory}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "addExistingContentInCategory", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId"), @MenuParameter(paramName = "addExistingContent", value = "Y")}))
        }
    )
    public interface CategoryContentSubTabBar {}

    @Menu(
        name = "CategoryProductSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditCategoryProduct", title = "${uiLabelMap.ProductCopyCategoryProductsToCategory}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "EditCategoryProducts", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId"), @MenuParameter(paramName = "copyProductToCategory", value = "Y")})),
            @MenuItem(name = "ExpireAllCategoryProducts", title = "${uiLabelMap.ProductExpireAllCategoryProducts}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "EditCategoryProducts", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId"), @MenuParameter(paramName = "expireAllCategoryProducts", value = "Y")})),
            @MenuItem(name = "RemoveExpiredCategoryProducts", title = "${uiLabelMap.ProductRemoveExpiredCategoryProducts}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productCategory"})}), link = @MenuLink(target = "EditCategoryProducts", parameters = {@MenuParameter(paramName = "productCategoryId", fromField = "productCategoryId"), @MenuParameter(paramName = "removeExpiredCategoryProducts", value = "Y")}))
        }
    )
    public interface CategoryProductSubTabBar {}

    @Menu(
        name = "ProductStoreTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductStore", title = "${uiLabelMap.ProductStore}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStore", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "FindProductStoreRoles", title = "${uiLabelMap.PartyRoles}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "FindProductStoreRoles", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStorePromos", title = "${uiLabelMap.ProductPromos}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStorePromos", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreCatalogs", title = "${uiLabelMap.ProductCatalogs}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreCatalogs", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreWebSites", title = "${uiLabelMap.ProductWebSites}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreWebSites", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "ListShipmentMethodTypes", title = "${uiLabelMap.ProductShipping}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "ListShipmentMethodTypes", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStorePaySetup", title = "${uiLabelMap.ProductPaymentMethods}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStorePaySetup", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreFinAccountSettings", title = "${uiLabelMap.CommonFinAccounts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreFinAccountSettings", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreFacilities", title = "${uiLabelMap.ProductFacility}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "ProductStoreFacilities", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreEmails", title = "${uiLabelMap.CommonEmails}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreEmails", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreSurveys", title = "${uiLabelMap.CommonSurveys}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreSurveys", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreKeywordOvrd", title = "${uiLabelMap.FormFieldTitle_keyword}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "editProductStoreKeywordOvrd", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "ViewProductStoreSegments", title = "${uiLabelMap.ProductSegments}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "ViewProductStoreSegments", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreVendorPayments", title = "${uiLabelMap.ProductVendorPayments}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreVendorPayments", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreVendorShipments", title = "${uiLabelMap.ProductVendorShipments}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productStore"})}), link = @MenuLink(target = "EditProductStoreVendorShipments", parameters = {@MenuParameter(paramName = "productStoreId", fromField = "productStoreId")})),
            @MenuItem(name = "EditProductStoreGroups", title = "${uiLabelMap.ProductProductStoreGroup}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"productStore"}), @Condition(type = NotEmpty.class, params = {"_non_existent_variable_"})}), link = @MenuLink(target = "ListParentProductStoreGroup", parameters = {@MenuParameter(paramName = "productStoreGroupId", fromField = "productStoreGroupId")}))
        }
    )
    public interface ProductStoreTabBar {}

    @Menu(
        name = "ProductStoreSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductStoreTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "EditProductStoreWebSites", subMenus = {@SubMenu(name = "WebSite", include = "component://content/widget/content/ContentMenus.xml#WebSiteSideBar")}),
            @MenuItem(name = "ListShipmentMethodTypes", subMenus = {@SubMenu(name = "Shipping", include = "component://product/widget/catalog/CatalogMenus.xml#ShippingSideBar")})
        }
    )
    public interface ProductStoreSideBar {}

    @Menu(
        name = "ProductStoreSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductStore", title = "${uiLabelMap.ProductNewProductStore}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateProductStore", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditProductStore"))
        }
    )
    public interface ProductStoreSubTabBar {}

    @Menu(
        name = "ProductStoreGroupButtonBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "editstoregroup", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditProductStoreGroup", text = "${uiLabelMap.ProductNewGroup}"))
        }
    )
    public interface ProductStoreGroupButtonBar {}

    @Menu(
        name = "ProductStoreFacility",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml"
    )
    public interface ProductStoreFacility {}

    @Menu(
        name = "PriceRulesButtonBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "FindRules", title = "${uiLabelMap.CommonAdd}/${uiLabelMap.ProductFindRule}", link = @MenuLink(target = "FindProductPriceRules"))
        }
    )
    public interface PriceRulesButtonBar {}

    @Menu(
        name = "PromoTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductPromo", title = "${uiLabelMap.ProductPromotion}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productPromoId"})}), link = @MenuLink(target = "EditProductPromo", parameters = {@MenuParameter(paramName = "productPromoId", fromField = "productPromoId")})),
            @MenuItem(name = "FindProductPromo", title = "${uiLabelMap.ProductPromotion}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"productPromoId"})}), link = @MenuLink(target = "FindProductPromo")),
            @MenuItem(name = "EditProductPromoRules", title = "${uiLabelMap.ProductRules}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productPromoId"})}), link = @MenuLink(target = "EditProductPromoRules", parameters = {@MenuParameter(paramName = "productPromoId", fromField = "productPromoId")})),
            @MenuItem(name = "EditProductPromoStores", title = "${uiLabelMap.ProductStores}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productPromoId"})}), link = @MenuLink(target = "EditProductPromoStores", parameters = {@MenuParameter(paramName = "productPromoId", fromField = "productPromoId")})),
            @MenuItem(name = "FindProductPromoCode", title = "${uiLabelMap.ProductPromotionCode}", link = @MenuLink(target = "FindProductPromoCode", parameters = {@MenuParameter(paramName = "productPromoId", fromField = "productPromoId")})),
            @MenuItem(name = "EditProductPromoContent", title = "${uiLabelMap.CommonContent}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productPromoId"})}), link = @MenuLink(target = "EditProductPromoContent", parameters = {@MenuParameter(paramName = "productPromoId", fromField = "productPromoId")}))
        }
    )
    public interface PromoTabBar {}

    @Menu(
        name = "PromoSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PromoTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PromoSideBar {}

    @Menu(
        name = "PromoSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductPromo", title = "${uiLabelMap.ProductNewPromotion}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateProductPromo", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditProductPromo"))
        }
    )
    public interface PromoSubTabBar {}

    @Menu(
        name = "PromoCodeSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductPromoCode", title = "${uiLabelMap.ProductNewPromotionCode}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateProductPromoCode", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditProductPromoCode"))
        }
    )
    public interface PromoCodeSubTabBar {}

    @Menu(
        name = "ProductTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProduct", title = "${uiLabelMap.ProductProduct}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"CATALOG", "_UPDATE"}), @Condition(type = NotEmpty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProduct", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "ViewProduct", title = "${uiLabelMap.ProductProductOverview}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "ViewProduct", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductPrices", title = "${uiLabelMap.ProductPrices}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductPrices", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductContent", title = "${uiLabelMap.ProductContent}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductContent", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductGeos", title = "${uiLabelMap.CommonGeos}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductGeos", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductGoodIdentifications", title = "${uiLabelMap.CommonIds}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductGoodIdentifications", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductCategories", title = "${uiLabelMap.ProductCategories}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductCategories", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductAssoc", title = "${uiLabelMap.ProductAssociations}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductAssoc", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductCosts", title = "${uiLabelMap.ProductCosts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductCosts", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductAttributes", title = "${uiLabelMap.ProductAttributes}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductAttributes", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductFeatures", title = "${uiLabelMap.ProductFeatures}", disabled = "${(empty context.productId) and (not currentMenuRenderState.itemState.conditionResult)}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"productId"})}), link = @MenuLink(target = "EditProductFeatures", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductFacilities", title = "${uiLabelMap.ProductFacilities}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductFacilities", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditSupplierProduct", title = "${uiLabelMap.ProductSuppliers}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductSuppliers", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductPaymentMethodTypes", title = "${uiLabelMap.ProductPaymentTypes}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductPaymentMethodTypes", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductSubscriptionResources", title = "${uiLabelMap.ProductSubscriptionResources}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductSubscriptionResources", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditVendorProduct", title = "${uiLabelMap.PartyVendor}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditVendorProduct", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "QuickAddVariants", title = "${uiLabelMap.ProductVariants}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"product.isVirtual", "equals", "Y"}), @Condition(type = NotEmpty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "QuickAddVariants", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductConfigs", title = "${uiLabelMap.ProductConfigs}", disabled = "${(empty context.productId) and (not currentMenuRenderState.itemState.conditionResult)}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "product.productTypeId", operator = "equals", value = "AGGREGATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "product.productTypeId", operator = "equals", value = "AGGREGATED_SERVICE")})}, conditions = {@Condition(type = NotEmpty.class, params = {"productId"})}), link = @MenuLink(target = "EditProductConfigs", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")})),
            @MenuItem(name = "EditProductAssetUsage", title = "${uiLabelMap.ProductAssetUsage}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "product.productTypeId", operator = "equals", value = "ASSET_USAGE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "product.productTypeId", operator = "equals", value = "ASSET_USAGE_OUT_IN")})}, conditions = {@Condition(type = NotEmpty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductAssetUsage", parameters = {@MenuParameter(paramName = "productId", fromField = "productId")}))
        }
    )
    public interface ProductTabBar {}

    @Menu(
        name = "ProductSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "EditProductConfigs", subMenus = {@SubMenu(name = "ConfigItem", include = "component://product/widget/catalog/CatalogMenus.xml#ConfigItemSideBar")})
        }
    )
    public interface ProductSideBar {}

    @Menu(
        name = "ProductContentSectionSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewProductContent", title = "${uiLabelMap.ContentNewContent}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"product.productId"})}), link = @MenuLink(target = "EditProductContent", parameters = {@MenuParameter(paramName = "productId", fromField = "product.productId")}))
        }
    )
    public interface ProductContentSectionSubTabBar {}

    @Menu(
        name = "ProductReviewTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "FindReviews", title = "${uiLabelMap.ProductReviews}", link = @MenuLink(target = "FindReviews", parameters = {@MenuParameter(paramName = "productId", fromField = "product.productId")})),
            @MenuItem(name = "updateProductReview", title = "${uiLabelMap.ProductConfigOptions}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "updateProductReview", parameters = {@MenuParameter(paramName = "productId", fromField = "product.productId")}))
        }
    )
    public interface ProductReviewTabBar {}

    @Menu(
        name = "ProductReviewSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductReviewTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ProductReviewSideBar {}

    @Menu(
        name = "ConfigItemTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductConfigItem", title = "${uiLabelMap.ProductConfigItem}", link = @MenuLink(target = "EditProductConfigItem", parameters = {@MenuParameter(paramName = "configItemId", fromField = "configItemId")})),
            @MenuItem(name = "EditProductConfigOptions", title = "${uiLabelMap.ProductConfigOptions}", link = @MenuLink(target = "EditProductConfigOptions", parameters = {@MenuParameter(paramName = "configItemId", fromField = "configItemId")})),
            @MenuItem(name = "EditProductConfigItemContent", title = "${uiLabelMap.ProductContent}", link = @MenuLink(target = "EditProductConfigItemContent", parameters = {@MenuParameter(paramName = "configItemId", fromField = "configItemId")}))
        }
    )
    public interface ConfigItemTabBar {}

    @Menu(
        name = "ConfigItemSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ConfigItemTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ConfigItemSideBar {}

    @Menu(
        name = "ConfigItemSubTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditProductConfigItem", title = "${uiLabelMap.ProductNewConfigItem}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Compare.class, params = {"isCreateConfigItem", "equals", "true", "Boolean"})}), link = @MenuLink(target = "EditProductConfigItem"))
        }
    )
    public interface ConfigItemSubTabBar {}

    @Menu(
        name = "SubscriptionSideBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SubscriptionTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface SubscriptionSideBar {}

    @Menu(
        name = "SubscriptionTabBar",
        location = "component://product/widget/catalog/CatalogMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml"
    )
    public interface SubscriptionTabBar {}

}
