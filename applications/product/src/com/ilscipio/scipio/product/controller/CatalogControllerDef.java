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
package com.ilscipio.scipio.product.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import com.ilscipio.scipio.product.category.CategoryEvents;
import org.ofbiz.product.store.ProductStoreEvents;
import org.ofbiz.product.product.ProductSearchEvents;
import org.ofbiz.webapp.event.TestEvent;
import org.ofbiz.product.product.VariantEvents;
import org.ofbiz.product.product.ProductEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://product/widget/catalog/CommonScreens.xml#main",
        controller = "catalog"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ChooseTopCategory",
        type = "screen",
        page = "component://product/widget/catalog/CommonScreens.xml#ChooseTopCategory",
        controller = "catalog"
    )
    public static final String VIEW_CHOOSETOPCATEGORY = "ChooseTopCategory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FastLoadCache",
        type = "screen",
        page = "component://product/widget/catalog/CommonScreens.xml#FastLoadCache",
        controller = "catalog"
    )
    public static final String VIEW_FASTLOADCACHE = "FastLoadCache";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "listMiniproduct",
        type = "screen",
        page = "component://product/widget/catalog/CommonScreens.xml#listMiniproduct",
        controller = "catalog"
    )
    public static final String VIEW_LISTMINIPRODUCT = "listMiniproduct";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "advancedsearch",
        type = "screen",
        page = "component://product/widget/catalog/FindScreens.xml#advancedsearch",
        controller = "catalog"
    )
    public static final String VIEW_ADVANCEDSEARCH = "advancedsearch";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "keywordsearch",
        type = "screen",
        page = "component://product/widget/catalog/FindScreens.xml#keywordsearch",
        controller = "catalog"
    )
    public static final String VIEW_KEYWORDSEARCH = "keywordsearch";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "exportproducts",
        type = "screen",
        page = "component://product/widget/catalog/FindScreens.xml#exportproducts",
        controller = "catalog"
    )
    public static final String VIEW_EXPORTPRODUCTS = "exportproducts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindProductById",
        type = "screen",
        page = "component://product/widget/catalog/FindScreens.xml#FindProductById",
        controller = "catalog"
    )
    public static final String VIEW_FINDPRODUCTBYID = "FindProductById";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindCatalog",
        type = "screen",
        page = "component://product/widget/catalog/CatalogScreens.xml#FindCatalog",
        controller = "catalog"
    )
    public static final String VIEW_FINDCATALOG = "FindCatalog";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProdCatalog",
        type = "screen",
        page = "component://product/widget/catalog/CatalogScreens.xml#EditProdCatalog",
        controller = "catalog"
    )
    public static final String VIEW_EDITPRODCATALOG = "EditProdCatalog";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProdCatalogCategories",
        type = "screen",
        page = "component://product/widget/catalog/CatalogScreens.xml#EditProdCatalogCategories",
        controller = "catalog"
    )
    public static final String VIEW_EDITPRODCATALOGCATEGORIES = "EditProdCatalogCategories";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProdCatalogParties",
        type = "screen",
        page = "component://product/widget/catalog/CatalogScreens.xml#EditProdCatalogParties",
        controller = "catalog"
    )
    public static final String VIEW_EDITPRODCATALOGPARTIES = "EditProdCatalogParties";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProdCatalogSection",
        type = "screen",
        page = "component://product/widget/catalog/CatalogScreens.xml#EditProdCatalogSection",
        controller = "catalog"
    )
    public static final String VIEW_EDITPRODCATALOGSECTION = "EditProdCatalogSection";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindCategory",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#FindCategory",
        controller = "catalog"
    )
    public static final String VIEW_FINDCATEGORY = "FindCategory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategory",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategory",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORY = "EditCategory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategoryContent",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategoryContent",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORYCONTENT = "EditCategoryContent";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategoryAttributes",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategoryAttributes",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORYATTRIBUTES = "EditCategoryAttributes";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategoryContentContent",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategoryContentContent",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORYCONTENTCONTENT = "EditCategoryContentContent";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategoryRollup",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategoryRollup",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORYROLLUP = "EditCategoryRollup";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategoryProducts",
        type = "screen",
        page = "component://product/widget/catalog/CategoryScreens.xml#EditCategoryProducts",
        controller = "catalog"
    )
    public static final String VIEW_EDITCATEGORYPRODUCTS = "EditCategoryProducts";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCategorySection",
            type = "screen",
            page = "component://product/widget/catalog/CategoryScreens.xml#EditCategorySection",
            controller = "catalog"
        )
        public static final String VIEW_EDITCATEGORYSECTION = "EditCategorySection";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "createProductInCategoryStart",
            type = "screen",
            page = "component://product/widget/catalog/CategoryScreens.xml#createProductInCategoryStart",
            controller = "catalog"
        )
        public static final String VIEW_CREATEPRODUCTINCATEGORYSTART = "createProductInCategoryStart";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateProductInCategoryCheckExisting",
            type = "screen",
            page = "component://product/widget/catalog/CategoryScreens.xml#CreateProductInCategoryCheckExisting",
            controller = "catalog"
        )
        public static final String VIEW_CREATEPRODUCTINCATEGORYCHECKEXISTING = "CreateProductInCategoryCheckExisting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#FindProduct",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCT = "FindProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ViewProduct",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCT = "ViewProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProduct",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCT = "EditProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPrices",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductPrices",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPRICES = "EditProductPrices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductAssetUsage",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductAssetUsage",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTASSETUSAGE = "EditProductAssetUsage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductParties",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductParties",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPARTIES = "EditProductParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showFixedAssetProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#showFixedAssetProduct",
            controller = "catalog"
        )
        public static final String VIEW_SHOWFIXEDASSETPRODUCT = "showFixedAssetProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "newFixedAssetProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#newFixedAssetProduct",
            controller = "catalog"
        )
        public static final String VIEW_NEWFIXEDASSETPRODUCT = "newFixedAssetProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductPriceHistory",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ProductPriceHistory",
            controller = "catalog"
        )
        public static final String VIEW_PRODUCTPRICEHISTORY = "ProductPriceHistory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductBarCode.pdf",
            type = "screenfop",
            page = "component://product/widget/catalog/ProductScreens.xml#ProductBarCode.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "catalog"
        )
        public static final String VIEW_PRODUCTBARCODE_PDF = "ProductBarCode.pdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductContent",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductContent",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONTENT = "EditProductContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductGeos",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductGeos",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTGEOS = "EditProductGeos";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductGoodIdentifications",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductGoodIdentifications",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTGOODIDENTIFICATIONS = "EditProductGoodIdentifications";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductCategories",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductCategories",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCATEGORIES = "EditProductCategories";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductKeyword",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductKeyword",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTKEYWORD = "EditProductKeyword";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductAssoc",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductAssoc",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTASSOC = "EditProductAssoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductManufacturing",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ViewProductManufacturing",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCTMANUFACTURING = "ViewProductManufacturing";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductAgreements",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ViewProductAgreements",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCTAGREEMENTS = "ViewProductAgreements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductCosts",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductCosts",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCOSTS = "EditProductCosts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductAttributes",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductAttributes",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTATTRIBUTES = "EditProductAttributes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductFeatures",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductFeatures",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTFEATURES = "EditProductFeatures";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApplyFeaturesFromCategory",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ApplyFeaturesFromCategory",
            controller = "catalog"
        )
        public static final String VIEW_APPLYFEATURESFROMCATEGORY = "ApplyFeaturesFromCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductFacilities",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductFacilities",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTFACILITIES = "EditProductFacilities";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductFacilityLocations",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductFacilityLocations",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTFACILITYLOCATIONS = "EditProductFacilityLocations";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductQuickAdmin",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductQuickAdmin",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTQUICKADMIN = "EditProductQuickAdmin";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductInventoryItems",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductInventoryItems",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTINVENTORYITEMS = "EditProductInventoryItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductGlAccounts",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductGlAccounts",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTGLACCOUNTS = "EditProductGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPaymentMethodTypes",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductPaymentMethodTypes",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPAYMENTMETHODTYPES = "EditProductPaymentMethodTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductContentContent",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductContentContent",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONTENTCONTENT = "EditProductContentContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSupplierProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditSupplierProduct",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUPPLIERPRODUCT = "EditSupplierProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductConfigs",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductConfigs",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONFIGS = "EditProductConfigs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "QuickAddVariants",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#QuickAddVariants",
            controller = "catalog"
        )
        public static final String VIEW_QUICKADDVARIANTS = "QuickAddVariants";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateVirtualWithVariantsForm",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#CreateVirtualWithVariantsForm",
            controller = "catalog"
        )
        public static final String VIEW_CREATEVIRTUALWITHVARIANTSFORM = "CreateVirtualWithVariantsForm";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductMaints",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductMaints",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTMAINTS = "EditProductMaints";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductMeters",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductMeters",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTMETERS = "EditProductMeters";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductSubscriptionResources",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductSubscriptionResources",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSUBSCRIPTIONRESOURCES = "EditProductSubscriptionResources";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSubscription",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#FindSubscription",
            controller = "catalog"
        )
        public static final String VIEW_FINDSUBSCRIPTION = "FindSubscription";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSubscription",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#EditSubscription",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUBSCRIPTION = "EditSubscription";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSubscriptionAttributes",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#EditSubscriptionAttributes",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUBSCRIPTIONATTRIBUTES = "EditSubscriptionAttributes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSubscriptionResource",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#FindSubscriptionResource",
            controller = "catalog"
        )
        public static final String VIEW_FINDSUBSCRIPTIONRESOURCE = "FindSubscriptionResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSubscriptionResource",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#EditSubscriptionResource",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUBSCRIPTIONRESOURCE = "EditSubscriptionResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSubscriptionResourceProducts",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#EditSubscriptionResourceProducts",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUBSCRIPTIONRESOURCEPRODUCTS = "EditSubscriptionResourceProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSubscriptionCommEvent",
            type = "screen",
            page = "component://product/widget/catalog/SubscriptionScreens.xml#EditSubscriptionCommEvent",
            controller = "catalog"
        )
        public static final String VIEW_EDITSUBSCRIPTIONCOMMEVENT = "EditSubscriptionCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFeatures",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#ListFeatures",
            controller = "catalog"
        )
        public static final String VIEW_LISTFEATURES = "ListFeatures";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeature",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeature",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATURE = "EditFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureCategories",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureCategories",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATURECATEGORIES = "EditFeatureCategories";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureCategoryFeatures",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureCategoryFeatures",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATURECATEGORYFEATURES = "EditFeatureCategoryFeatures";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureGroups",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureGroups",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATUREGROUPS = "EditFeatureGroups";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureGroup",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureGroup",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATUREGROUP = "EditFeatureGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureGroupAppls",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureGroupAppls",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATUREGROUPAPPLS = "EditFeatureGroupAppls";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureTypes",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureTypes",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATURETYPES = "EditFeatureTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureType",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureType",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATURETYPE = "EditFeatureType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureInterActions",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureInterActions",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATUREINTERACTIONS = "EditFeatureInterActions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFeatureInterAction",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#EditFeatureInterAction",
            controller = "catalog"
        )
        public static final String VIEW_EDITFEATUREINTERACTION = "EditFeatureInterAction";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "QuickAddProductFeatures",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#QuickAddProductFeatures",
            controller = "catalog"
        )
        public static final String VIEW_QUICKADDPRODUCTFEATURES = "QuickAddProductFeatures";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#CreateProductFeature",
            controller = "catalog"
        )
        public static final String VIEW_CREATEPRODUCTFEATURE = "CreateProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFeaturePrice",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#ListFeaturePrice",
            controller = "catalog"
        )
        public static final String VIEW_LISTFEATUREPRICE = "ListFeaturePrice";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateFeature",
            type = "screen",
            page = "component://product/widget/catalog/FeatureScreens.xml#CreateFeature",
            controller = "catalog"
        )
        public static final String VIEW_CREATEFEATURE = "CreateFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromo",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#FindProductPromo",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCTPROMO = "FindProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromo",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromo",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPROMO = "EditProductPromo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoRules",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoRules",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPROMORULES = "EditProductPromoRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoStores",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoStores",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPROMOSTORES = "EditProductPromoStores";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductPromoCode",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#FindProductPromoCode",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCTPROMOCODE = "FindProductPromoCode";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoCode",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoCode",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPROMOCODE = "EditProductPromoCode";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPromoContent",
            type = "screen",
            page = "component://product/widget/catalog/PromoScreens.xml#EditProductPromoContent",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPROMOCONTENT = "EditProductPromoContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPriceRules",
            type = "screen",
            page = "component://product/widget/catalog/PriceScreens.xml#FindProductPriceRule",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRICERULES = "FindPriceRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductPriceRules",
            type = "screen",
            page = "component://product/widget/catalog/PriceScreens.xml#EditProductPriceRules",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTPRICERULES = "EditProductPriceRules";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductStore",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#FindProductStore",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCTSTORE = "FindProductStore";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStore",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStore",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTORE = "EditProductStore";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductStoreRoles",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#FindProductStoreRoles",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCTSTOREROLES = "FindProductStoreRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreEmails",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreEmails",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREEMAILS = "EditProductStoreEmails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStorePromos",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStorePromos",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREPROMOS = "EditProductStorePromos";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreCatalogs",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreCatalogs",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTORECATALOGS = "EditProductStoreCatalogs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreShipSetup",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreShipSetup",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTORESHIPSETUP = "EditProductStoreShipSetup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreShipmentCostEstimates",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreShipmentCostEstimates",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTORESHIPMENTCOSTESTIMATES = "EditProductStoreShipmentCostEstimates";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreSurveys",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreSurveys",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTORESURVEYS = "EditProductStoreSurveys";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStorePaySetup",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStorePaySetup",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREPAYSETUP = "EditProductStorePaySetup";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreWebSites",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreWebSites",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREWEBSITES = "EditProductStoreWebSites";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreKeywordOvrd",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreKeywordOvrd",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREKEYWORDOVRD = "EditProductStoreKeywordOvrd";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductStoreSegments",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#ViewProductStoreSegments",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCTSTORESEGMENTS = "ViewProductStoreSegments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreFinAccountSettings",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreFinAccountSettings",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREFINACCOUNTSETTINGS = "EditProductStoreFinAccountSettings";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreVendorPayments",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreVendorPayments",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREVENDORPAYMENTS = "EditProductStoreVendorPayments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreVendorShipments",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreVendorShipments",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREVENDORSHIPMENTS = "EditProductStoreVendorShipments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductStoreFacilities",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#ProductStoreFacilities",
            controller = "catalog"
        )
        public static final String VIEW_PRODUCTSTOREFACILITIES = "ProductStoreFacilities";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListProductStoreFacility",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#ListProductStoreFacility",
            controller = "catalog"
        )
        public static final String VIEW_LISTPRODUCTSTOREFACILITY = "ListProductStoreFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreFacility",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreFacility",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREFACILITY = "EditProductStoreFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditVendorProduct",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditVendorProduct",
            controller = "catalog"
        )
        public static final String VIEW_EDITVENDORPRODUCT = "EditVendorProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditKeywordThesaurus",
            type = "screen",
            page = "component://product/widget/catalog/ThesaurusScreens.xml#EditKeywordThesaurus",
            controller = "catalog"
        )
        public static final String VIEW_EDITKEYWORDTHESAURUS = "EditKeywordThesaurus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListParentProductStoreGroup",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#ListParentProductStoreGroup",
            controller = "catalog"
        )
        public static final String VIEW_LISTPARENTPRODUCTSTOREGROUP = "ListParentProductStoreGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreGroup",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreGroup",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREGROUP = "EditProductStoreGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductStoreGroupAndAssoc",
            type = "screen",
            page = "component://product/widget/catalog/StoreScreens.xml#EditProductStoreGroupAndAssoc",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTSTOREGROUPANDASSOC = "EditProductStoreGroupAndAssoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindReviews",
            type = "screen",
            page = "component://product/widget/catalog/ReviewScreens.xml#FindReviews",
            controller = "catalog"
        )
        public static final String VIEW_FINDREVIEWS = "FindReviews";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductOrder",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ViewProductOrder",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCTORDER = "ViewProductOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductCommunicationEvents",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductCommunicationEvents",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCOMMUNICATIONEVENTS = "EditProductCommunicationEvents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCommunicationEvent",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditCommunicationEvent",
            controller = "catalog"
        )
        public static final String VIEW_EDITCOMMUNICATIONEVENT = "EditCommunicationEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductConfigItemArticle",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#ProductConfigItemArticle",
            controller = "catalog"
        )
        public static final String VIEW_PRODUCTCONFIGITEMARTICLE = "ProductConfigItemArticle";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindProductConfigItems",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#FindProductConfigItems",
            controller = "catalog"
        )
        public static final String VIEW_FINDPRODUCTCONFIGITEMS = "FindProductConfigItems";

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductConfigItem",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#EditProductConfigItem",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONFIGITEM = "EditProductConfigItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductConfigOptions",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#EditProductConfigOptions",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONFIGOPTIONS = "EditProductConfigOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductConfigItemContent",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#EditProductConfigItemContent",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONFIGITEMCONTENT = "EditProductConfigItemContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductConfigItemContentContent",
            type = "screen",
            page = "component://product/widget/catalog/ConfigScreens.xml#EditProductConfigItemContentContent",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTCONFIGITEMCONTENTCONTENT = "EditProductConfigItemContentContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductWorkEfforts",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductWorkEfforts",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTWORKEFFORTS = "EditProductWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewProductGroupOrder",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#ViewProductGroupOrder",
            controller = "catalog"
        )
        public static final String VIEW_VIEWPRODUCTGROUPORDER = "ViewProductGroupOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductGroupOrder",
            type = "screen",
            page = "component://product/widget/catalog/ProductScreens.xml#EditProductGroupOrder",
            controller = "catalog"
        )
        public static final String VIEW_EDITPRODUCTGROUPORDER = "EditProductGroupOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuantityBreaks",
            type = "screen",
            page = "component://product/widget/catalog/ShippingScreens.xml#ListQuantityBreaks",
            controller = "catalog"
        )
        public static final String VIEW_LISTQUANTITYBREAKS = "ListQuantityBreaks";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListShipmentMethodTypes",
            type = "screen",
            page = "component://product/widget/catalog/ShippingScreens.xml#ListShipmentMethodTypes",
            controller = "catalog"
        )
        public static final String VIEW_LISTSHIPMENTMETHODTYPES = "ListShipmentMethodTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListCarrierShipmentMethods",
            type = "screen",
            page = "component://product/widget/catalog/ShippingScreens.xml#ListCarrierShipmentMethods",
            controller = "catalog"
        )
        public static final String VIEW_LISTCARRIERSHIPMENTMETHODS = "ListCarrierShipmentMethods";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewCarrier",
            type = "screen",
            page = "component://product/widget/catalog/ShippingScreens.xml#NewCarrier",
            controller = "catalog"
        )
        public static final String VIEW_NEWCARRIER = "NewCarrier";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupUserLoginAndPartyDetails",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPUSERLOGINANDPARTYDETAILS = "LookupUserLoginAndPartyDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFixedAsset",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupFixedAsset",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPFIXEDASSET = "LookupFixedAsset";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCommEvent",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCommEvent",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPCOMMEVENT = "LookupCommEvent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSupplierProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupSupplierProduct",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPSUPPLIERPRODUCT = "LookupSupplierProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVirtualProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVirtualProduct",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPVIRTUALPRODUCT = "LookupVirtualProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductCategory",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductStore",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductStore",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPRODUCTSTORE = "LookupProductStore";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacilityLocation",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacilityLocation",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPFACILITYLOCATION = "LookupFacilityLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCostComponentCalc",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupCostComponentCalc",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPCOSTCOMPONENTCALC = "LookupCostComponentCalc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupDataResource",
            type = "screen",
            page = "component://content/widget/content/DataResourceScreens.xml#LookupDataResource",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPDATARESOURCE = "LookupDataResource";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPreferredContactMech",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupPreferredContactMech",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPPREFERREDCONTACTMECH = "LookupPreferredContactMech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContactList",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupContactList",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPCONTACTLIST = "LookupContactList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupMediaImage",
            type = "screen",
            page = "component://cms/widget/LookupScreens.xml#LookupMediaImage",
            controller = "catalog"
        )
        public static final String VIEW_LOOKUPMEDIAIMAGE = "LookupMediaImage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ScpCatalogCommon.js",
            type = "screen",
            page = "component://product/widget/catalog/CommonScreens.xml#ScpCatalogCommon.js",
            contentType = "application/javascript",
            controller = "catalog"
        )
        public static final String VIEW_SCPCATALOGCOMMON_JS = "ScpCatalogCommon.js";

        @Request(
            uri = "view",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface ViewDef {}

        @Request(
            uri = "chain",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "/view")
        @Response(name = "error", type = "view", value = "error")
        public static String chain(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.TestEvent.test
            return TestEvent.test(request, response);
        }

        @Request(
            uri = "main",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "FastLoadCache",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FastLoadCache")
        public interface FastLoadCache {}

        @Request(
            uri = "advancedsearch",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        public interface Advancedsearch {}

        @Request(
            uri = "search",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        public interface Search {}

        @Request(
            uri = "keywordsearch",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "search")
        public interface Keywordsearch {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "searchRemoveFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String searchRemoveFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchRemoveFromCategory
            return ProductSearchEvents.searchRemoveFromCategory(request, response);
        }

        @Request(
            uri = "searchExpireFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String searchExpireFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchExpireFromCategory
            return ProductSearchEvents.searchExpireFromCategory(request, response);
        }

        @Request(
            uri = "searchAddToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String searchAddToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchAddToCategory
            return ProductSearchEvents.searchAddToCategory(request, response);
        }

        @Request(
            uri = "searchAddFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String searchAddFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchAddFeature
            return ProductSearchEvents.searchAddFeature(request, response);
        }

        @Request(
            uri = "searchRemoveFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String searchRemoveFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchRemoveFeature
            return ProductSearchEvents.searchRemoveFeature(request, response);
        }

        @Request(
            uri = "searchExportProductList",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "exportproducts")
        @Response(name = "error", type = "view", value = "exportproducts")
        public static String searchExportProductList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchEvents.searchExportProductList
            return ProductSearchEvents.searchExportProductList(request, response);
        }

        @Request(
            uri = "FindProductById",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductById")
        public interface FindProductById {}

        @Request(
            uri = "ChooseTopCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ChooseTopCategory")
        public interface ChooseTopCategory {}

        @Request(
            uri = "FindCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCategory")
        public interface FindCategory {}

        @Request(
            uri = "EditCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        public interface EditCategory {}

        @Request(
            uri = "UploadCategoryImage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        public interface UploadCategoryImage {}

        @Request(
            uri = "EditCategoryAjax",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategorySection")
        public interface EditCategoryAjax {}

        @Request(
            uri = "createProductCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        @Response(name = "error", type = "view", value = "EditCategory")
        @Event(type = "service", invoke = "createProductCategory")
        public static String createProductCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        @Response(name = "error", type = "view", value = "EditCategory")
        @Event(type = "service", invoke = "updateProductCategory")
        public static String updateProductCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "DuplicateProductCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        @Response(name = "error", type = "view", value = "EditCategory")
        @Event(type = "service", invoke = "duplicateProductCategory")
        public static String duplicateProductCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupMediaImage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupMediaImage")
        public interface LookupMediaImage {}

        @Request(
            uri = "EditCategoryRollup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryRollup")
        public interface EditCategoryRollup {}

        @Request(
            uri = "addProductCategoryToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryRollup")
        @Response(name = "error", type = "view", value = "EditCategoryRollup")
        @Event(type = "service", invoke = "safeAddProductCategoryToCategory")
        public static String addProductCategoryToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategoryToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditCategoryRollup")
        @Response(name = "error", type = "view", value = "EditCategoryRollup")
        @Event(type = "service-multi", invoke = "updateProductCategoryToCategory")
        public static String updateProductCategoryToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductCategoryFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryRollup")
        @Response(name = "error", type = "view", value = "EditCategoryRollup")
        @Event(type = "service", invoke = "removeProductCategoryFromCategory")
        public static String removeProductCategoryFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "EditCategoryProducts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        public interface EditCategoryProducts {}

        @Request(
            uri = "addCategoryProductMember",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "safeAddProductToCategory")
        public static String addCategoryProductMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCategoryProductMember",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service-multi", invoke = "updateProductToCategory")
        public static String updateCategoryProductMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeCategoryProductMember",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "removeProductFromCategory")
        public static String removeCategoryProductMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "copyCategoryProductMembers",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "copyCategoryProductMembers")
        public static String copyCategoryProductMembers(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expireAllCategoryProductMembers",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "expireAllCategoryProductMembers")
        public static String expireAllCategoryProductMembers(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeExpiredCategoryProductMembers",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "removeExpiredCategoryProductMembers")
        public static String removeExpiredCategoryProductMembers(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductInCategoryStart",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "createProductInCategoryStart")
        public interface CreateProductInCategoryStart {}

        @Request(
            uri = "CreateProductInCategoryCheckExisting",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateProductInCategoryCheckExisting")
        public interface CreateProductInCategoryCheckExisting {}

        @Request(
            uri = "createProductInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryProducts")
        @Response(name = "error", type = "view", value = "EditCategoryProducts")
        @Event(type = "service", invoke = "createProductInCategory")
        public static String createProductInCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CreateProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateProductFeature")
        public interface CreateProductFeature {}

        @Request(
            uri = "EditCategoryAttributes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryAttributes")
        public interface EditCategoryAttributes {}

        @Request(
            uri = "createProductCategoryAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryAttributes")
        @Response(name = "error", type = "view", value = "EditCategoryAttributes")
        @Event(type = "service", invoke = "createProductCategoryAttribute")
        public static String createProductCategoryAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategoryAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryAttributes")
        @Response(name = "error", type = "view", value = "EditCategoryAttributes")
        @Event(type = "service", invoke = "updateProductCategoryAttribute")
        public static String updateProductCategoryAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductCategoryAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryAttributes")
        @Response(name = "error", type = "view", value = "EditCategoryAttributes")
        @Event(type = "service", invoke = "deleteProductCategoryAttribute")
        public static String deleteProductCategoryAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProduct")
        public interface FindProduct {}

        @Request(
            uri = "ViewProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProduct")
        public interface ViewProduct {}

        @Request(
            uri = "EditProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        public interface EditProduct {}

        @Request(
            uri = "createProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "service", invoke = "createProduct")
        public static String createProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "service", invoke = "updateProduct")
        public static String updateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "DuplicateProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "service", invoke = "duplicateProduct")
        public static String duplicateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateProductVariants",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "service", invoke = "copyToProductVariants")
        public static String updateProductVariants(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductBarCode.pdf",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductBarCode.pdf")
        public interface ProductBarCodePdf {}

        @Request(
            uri = "EditProductParties",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductParties")
        public interface EditProductParties {}

        @Request(
            uri = "addPartyToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductParties")
        @Response(name = "error", type = "view", value = "EditProductParties")
        @Event(type = "service", invoke = "addPartyToProduct")
        public static String addPartyToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductParties")
        @Response(name = "error", type = "view", value = "EditProductParties")
        @Event(type = "service", invoke = "updatePartyToProduct")
        public static String updatePartyToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePartyFromProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductParties")
        @Response(name = "error", type = "view", value = "EditProductParties")
        @Event(type = "service", invoke = "removePartyFromProduct")
        public static String removePartyFromProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductAssetUsage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssetUsage")
        public interface EditProductAssetUsage {}

        @Request(
            uri = "updateProductAssetUsage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssetUsage")
        @Response(name = "error", type = "view", value = "EditProductAssetUsage")
        @Event(type = "service", invoke = "updateProduct")
        public static String updateProductAssetUsage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "showFixedAssetProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showFixedAssetProduct")
        public interface ShowFixedAssetProduct {}

        @Request(
            uri = "newFixedAssetProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "newFixedAssetProduct")
        public interface NewFixedAssetProduct {}

        @Request(
            uri = "addFixedAssetProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssetUsage")
        @Response(name = "error", type = "view", value = "newFixedAssetProduct")
        @Event(type = "service", path = "org.ofbiz.accounting.fixedasset.FixedAssetServices.xml", invoke = "addFixedAssetProduct")
        public static String addFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updFixedAssetProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showFixedAssetProduct")
        @Response(name = "error", type = "view", value = "showFixedAssetProduct")
        @Event(type = "service", path = "org.ofbiz.accounting.fixedasset.FixedAssetServices.xml", invoke = "updateFixedAssetProduct")
        public static String updFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFixedAssetProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssetUsage")
        @Response(name = "error", type = "view", value = "EditProductAssetUsage")
        @Event(type = "service", path = "org.ofbiz.accounting.fixedasset.FixedAssetServices.xml", invoke = "removeFixedAssetProduct")
        public static String removeFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPrices",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPrices")
        public interface EditProductPrices {}

        @Request(
            uri = "createProductPrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProductPrices")
        @Response(name = "error", type = "view", value = "EditProductPrices")
        @Event(type = "service", invoke = "createProductPrice")
        public static String createProductPrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProductPrices")
        @Response(name = "error", type = "view", value = "EditProductPrices")
        @Event(type = "service", invoke = "updateProductPrice")
        public static String updateProductPrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductPriceHistory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductPriceHistory")
        public interface ProductPriceHistory {}

        @Request(
            uri = "deleteProductPrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProductPrices")
        @Response(name = "error", type = "view", value = "EditProductPrices")
        @Event(type = "service", invoke = "deleteProductPrice")
        public static String deleteProductPrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCategoryContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        public interface EditCategoryContent {}

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "updateCategoryContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateProductCategory")
        public static String updateCategoryContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addExistingContentInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        public interface AddExistingContentInCategory {}

        @Request(
            uri = "EditCategoryContentContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContentContent")
        public interface EditCategoryContentContent {}

        @Request(
            uri = "prepareAddContentToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContentContent")
        public interface PrepareAddContentToCategory {}

        @Request(
            uri = "addAdditionalImagesForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "uploadCategoryAdditionalViewImages")
        public static String addAdditionalImagesForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addContentToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "createCategoryContent")
        public static String addContentToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateCategoryContent")
        public static String updateContentToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "removeCategoryContent")
        public static String removeContentFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSimpleTextContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateSimpleTextContentForCategory")
        public static String updateSimpleTextContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSimpleTextContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "createSimpleTextContentForCategory")
        public static String createSimpleTextContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentSEOForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateContentSEOForCategory")
        public static String updateContentSEOForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createRelatedUrlContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "createRelatedUrlContentForCategory")
        public static String createRelatedUrlContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRelatedUrlContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateRelatedUrlContentForCategory")
        public static String updateRelatedUrlContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDownloadContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "updateDownloadContentForCategory")
        public static String updateDownloadContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createDownloadContentForCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "createDownloadContentForCategory")
        public static String createDownloadContentForCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        public interface EditProductContent {}

        @Request(
            uri = "updateProductContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "updateProduct")
        public static String updateProductContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UploadProductImage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        public interface UploadProductImage {}

        @Request(
            uri = "updateContentSEOForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "updateContentSEOForProduct")
        public static String updateContentSEOForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSimpleTextContentForAlternateLocaleInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "createSimpleTextContentForAlternateLocale")
        public static String createSimpleTextContentForAlternateLocaleInCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "updateSimpleTextContentForAlternateLocaleInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service-multi", invoke = "updateSimpleTextContentForAlternateLocale")
        public static String updateSimpleTextContentForAlternateLocaleInCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSimpleTextContentForAlternateLocaleInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "deleteSimpleTextContentForAlternateLocale")
        public static String deleteSimpleTextContentForAlternateLocaleInCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategoryContentStcLocFields",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategoryContent")
        @Response(name = "error", type = "view", value = "EditCategoryContent")
        @Event(type = "service", invoke = "replaceProductCategoryContentLocalizedSimpleTexts")
        public static String updateProductCategoryContentStcLocFields(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductContentStcLocFields",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "replaceProductContentLocalizedSimpleTexts")
        public static String updateProductContentStcLocFields(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductContentContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        public interface EditProductContentContent {}

        @Request(
            uri = "prepareAddContentToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        public interface PrepareAddContentToProduct {}

        @Request(
            uri = "addAdditionalImagesForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "uploadProductAdditionalViewImages")
        public static String addAdditionalImagesForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addContentToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "createProductContent")
        public static String addContentToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "updateProductContent")
        public static String updateContentToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentFromProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContent")
        @Event(type = "service", invoke = "removeProductContent")
        public static String removeContentFromProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmailContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "updateEmailContentForProduct")
        public static String updateEmailContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEmailContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "createEmailContentForProduct")
        public static String createEmailContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateExternalContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "updateProductContent")
        public static String updateExternalContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createExternalContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "createProductContent")
        public static String createExternalContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDownloadContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "updateDownloadContentForProduct")
        public static String updateDownloadContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createDownloadContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "createDownloadContentForProduct")
        public static String createDownloadContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSimpleTextContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "updateSimpleTextContentForProduct")
        public static String updateSimpleTextContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSimpleTextContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "createSimpleTextContentForProduct")
        public static String createSimpleTextContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSimpleTextContentForAlternateLocale",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "createSimpleTextContentForAlternateLocale")
        public static String createSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSimpleTextContentForAlternateLocale",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service-multi", invoke = "updateSimpleTextContentForAlternateLocale")
        public static String updateSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "deleteSimpleTextContentForAlternateLocale",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContentContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "deleteSimpleTextContentForAlternateLocale")
        public static String deleteSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addAdditionalImageContentForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductContent")
        @Response(name = "error", type = "view", value = "EditProductContentContent")
        @Event(type = "service", invoke = "addAdditionalViewForProduct")
        public static String addAdditionalImageContentForProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductGoodIdentifications",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGoodIdentifications")
        public interface EditProductGoodIdentifications {}

        @Request(
            uri = "createGoodIdentification",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGoodIdentifications")
        @Response(name = "error", type = "view", value = "EditProductGoodIdentifications")
        @Event(type = "service", invoke = "createGoodIdentification")
        public static String createGoodIdentification(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGoodIdentification",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGoodIdentifications")
        @Response(name = "error", type = "view", value = "EditProductGoodIdentifications")
        @Event(type = "service", invoke = "updateGoodIdentification")
        public static String updateGoodIdentification(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteGoodIdentification",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGoodIdentifications")
        @Response(name = "error", type = "view", value = "EditProductGoodIdentifications")
        @Event(type = "service", invoke = "deleteGoodIdentification")
        public static String deleteGoodIdentification(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductCategories",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategories")
        public interface EditProductCategories {}

        @Request(
            uri = "addProductToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategories")
        @Response(name = "error", type = "view", value = "EditProductCategories")
        @Event(type = "service", invoke = "safeAddProductToCategory")
        public static String addProductToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductToCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategories")
        @Response(name = "error", type = "view", value = "EditProductCategories")
        @Event(type = "service-multi", invoke = "updateProductToCategory")
        public static String updateProductToCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategories")
        @Response(name = "error", type = "view", value = "EditProductCategories")
        @Event(type = "service", invoke = "removeProductFromCategory")
        public static String removeProductFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductKeyword",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        public interface EditProductKeyword {}

        @Request(
            uri = "UpdateAllKeywords",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String updateAllKeywords(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateAllKeywords
            return ProductEvents.updateAllKeywords(request, response);
        }

        @Request(
            uri = "updateProductKeyword",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "updateProductKeyword")
        public static String updateProductKeyword(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductKeyword",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "createProductKeyword")
        public static String createProductKeyword(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductKeyword",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "updateProductKeyword")
        public static String updateProductKeyword_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductKeyword",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "deleteProductKeyword")
        public static String deleteProductKeyword(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductKeywords",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "deleteProductKeywords")
        public static String deleteProductKeywords(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "forceIndexProductKeywords",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductKeyword")
        @Response(name = "error", type = "view", value = "EditProductKeyword")
        @Event(type = "service", invoke = "forceIndexProductKeywords")
        public static String forceIndexProductKeywords(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductAssoc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssoc")
        public interface EditProductAssoc {}

        @Request(
            uri = "UpdateProductAssoc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProductAssoc")
        @Response(name = "error", type = "view", value = "EditProductAssoc")
        public static String updateProductAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateProductAssoc
            return ProductEvents.updateProductAssoc(request, response);
        }

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "ViewProductManufacturing",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductManufacturing")
        public interface ViewProductManufacturing {}

        @Request(
            uri = "ViewProductAgreements",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductAgreements")
        public interface ViewProductAgreements {}

        @Request(
            uri = "EditProductCosts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        public interface EditProductCosts {}

        @Request(
            uri = "createCostComponent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "createCostComponent")
        public static String createCostComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCostComponent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "updateCostComponent")
        public static String updateCostComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCostComponent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "deleteCostComponent")
        public static String deleteCostComponent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductCostComponentCalc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "createProductCostComponentCalc")
        public static String createProductCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCostComponentCalc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "updateProductCostComponentCalc")
        public static String updateProductCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductCostComponentCalc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "deleteProductCostComponentCalc")
        public static String deleteProductCostComponentCalc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "calculateProductCosts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCosts")
        @Response(name = "error", type = "view", value = "EditProductCosts")
        @Event(type = "service", invoke = "calculateProductCosts")
        public static String calculateProductCosts(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductAttributes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAttributes")
        public interface EditProductAttributes {}

        @Request(
            uri = "createProductAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAttributes")
        @Response(name = "error", type = "view", value = "EditProductAttributes")
        @Event(type = "service", invoke = "createProductAttribute")
        public static String createProductAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAttributes")
        @Response(name = "error", type = "view", value = "EditProductAttributes")
        @Event(type = "service", invoke = "updateProductAttribute")
        public static String updateProductAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAttributes")
        @Response(name = "error", type = "view", value = "EditProductAttributes")
        @Event(type = "service", invoke = "deleteProductAttribute")
        public static String deleteProductAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductFacilities",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilities")
        public interface EditProductFacilities {}

        @Request(
            uri = "createProductFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilities")
        @Response(name = "error", type = "view", value = "EditProductFacilities")
        @Event(type = "service", invoke = "createProductFacility")
        public static String createProductFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilities")
        @Response(name = "error", type = "view", value = "EditProductFacilities")
        @Event(type = "service", invoke = "updateProductFacility")
        public static String updateProductFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilities")
        @Response(name = "error", type = "view", value = "EditProductFacilities")
        @Event(type = "service", invoke = "deleteProductFacility")
        public static String deleteProductFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductFacilityLocations",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilityLocations")
        public interface EditProductFacilityLocations {}

        @Request(
            uri = "createProductFacilityLocation",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilityLocations")
        @Response(name = "error", type = "view", value = "EditProductFacilityLocations")
        @Event(type = "service", invoke = "createProductFacilityLocation")
        public static String createProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "updateProductFacilityLocation",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilityLocations")
        @Response(name = "error", type = "view", value = "EditProductFacilityLocations")
        @Event(type = "service", invoke = "updateProductFacilityLocation")
        public static String updateProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductFacilityLocation",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFacilityLocations")
        @Response(name = "error", type = "view", value = "EditProductFacilityLocations")
        @Event(type = "service", invoke = "deleteProductFacilityLocation")
        public static String deleteProductFacilityLocation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductQuickAdmin",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        public interface EditProductQuickAdmin {}

        @Request(
            uri = "updateProductQuickAdminName",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        @Event(type = "service", invoke = "updateProductQuickAdminName")
        public static String updateProductQuickAdminName(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductQuickAdminShipping",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String updateProductQuickAdminShipping(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateProductQuickAdminShipping
            return ProductEvents.updateProductQuickAdminShipping(request, response);
        }

        @Request(
            uri = "updateProductQuickAdminSelFeat",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String updateProductQuickAdminSelFeat(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateProductQuickAdminSelFeat
            return ProductEvents.updateProductQuickAdminSelFeat(request, response);
        }

        @Request(
            uri = "updateProductQuickAdminDelFeatureTypes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String updateProductQuickAdminDelFeatureTypes(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.removeFeatureApplsByFeatureTypeId
            return ProductEvents.removeFeatureApplsByFeatureTypeId(request, response);
        }

        @Request(
            uri = "quickAdminUpdateProductAssoc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String quickAdminUpdateProductAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateProductAssoc
            return ProductEvents.updateProductAssoc(request, response);
        }

        @Request(
            uri = "quickAdminRemoveProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String quickAdminRemoveProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.removeProductFeatureAppl
            return ProductEvents.removeProductFeatureAppl(request, response);
        }

        @Request(
            uri = "quickAdminAddCategories",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String quickAdminAddCategories(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.addProductToCategories
            return ProductEvents.addProductToCategories(request, response);
        }

        @Request(
            uri = "quickAdminRemoveProductFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        @Event(type = "service", invoke = "removeProductFromCategory")
        public static String quickAdminRemoveProductFromCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAdminUnPublish",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String quickAdminUnPublish(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.updateProductCategoryMember
            return ProductEvents.updateProductCategoryMember(request, response);
        }

        @Request(
            uri = "quickAdminApplyFeatureToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        public static String quickAdminApplyFeatureToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.addProductFeatures
            return ProductEvents.addProductFeatures(request, response);
        }

        @Request(
            uri = "quickAdminRemoveFeatureFromProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductQuickAdmin")
        @Response(name = "error", type = "view", value = "EditProductQuickAdmin")
        @Event(type = "service", invoke = "removeFeatureFromProduct")
        public static String quickAdminRemoveFeatureFromProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductInventoryItems",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductInventoryItems")
        public interface EditProductInventoryItems {}

        @Request(
            uri = "EditProductGlAccounts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        public interface EditProductGlAccounts {}

        @Request(
            uri = "createProductGlAccount",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "createProductGlAccount")
        public static String createProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductGlAccount",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "updateProductGlAccount")
        public static String updateProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductGlAccount",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "deleteProductGlAccount")
        public static String deleteProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPaymentMethodTypes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPaymentMethodTypes")
        public interface EditProductPaymentMethodTypes {}

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "createProductPaymentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPaymentMethodTypes")
        @Response(name = "error", type = "view", value = "EditProductPaymentMethodTypes")
        @Event(type = "service", invoke = "createProductPaymentMethodType")
        public static String createProductPaymentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPaymentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPaymentMethodTypes")
        @Response(name = "error", type = "view", value = "EditProductPaymentMethodTypes")
        @Event(type = "service", invoke = "updateProductPaymentMethodType")
        public static String updateProductPaymentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPaymentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPaymentMethodTypes")
        @Response(name = "error", type = "view", value = "EditProductPaymentMethodTypes")
        @Event(type = "service", invoke = "deleteProductPaymentMethodType")
        public static String deleteProductPaymentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFeatureCategories",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureCategories")
        public interface EditFeatureCategories {}

        @Request(
            uri = "CreateFeatureCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureCategoryFeatures")
        @Response(name = "error", type = "view", value = "EditFeatureCategories")
        @Event(type = "service", invoke = "createProductFeatureCategory")
        public static String createFeatureCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateFeatureCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "EditFeatureCategories")
        @Event(type = "service", invoke = "updateProductFeatureCategory")
        public static String updateFeatureCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFeatureCategoryFeatures",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureCategoryFeatures")
        public interface EditFeatureCategoryFeatures {}

        @Request(
            uri = "UpdateProductFeatureInCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureCategoryFeatures")
        @Response(name = "error", type = "view", value = "EditFeatureCategoryFeatures")
        @Event(type = "service-multi", invoke = "updateProductFeature")
        public static String updateProductFeatureInCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BulkAddProductFeatures",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureCategoryFeatures")
        @Response(name = "error", type = "view", value = "QuickAddProductFeatures")
        @Event(type = "service-multi", invoke = "createProductFeature")
        public static String bulkAddProductFeatures(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "QuickAddProductFeatures",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickAddProductFeatures")
        public interface QuickAddProductFeatures {}

        @Request(
            uri = "ListFeatures",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFeatures")
        public interface ListFeatures {}

        @Request(
            uri = "EditFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        public interface EditFeature {}

        @Request(
            uri = "CreateFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateFeature")
        public interface CreateFeature {}

        @Request(
            uri = "createProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "createProductFeature")
        public static String createProductFeature_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "updateProductFeature")
        public static String updateProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFeatureGroups",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureGroups")
        public interface EditFeatureGroups {}

        @Request(
            uri = "EditProductFeatureGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureGroup")
        public interface EditProductFeatureGroup {}

        @Request(
            uri = "CreateProductFeatureGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureGroups")
        @Response(name = "error", type = "view", value = "EditFeatureGroups")
        @Event(type = "service", invoke = "createProductFeatureGroup")
        public static String createProductFeatureGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateProductFeatureGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureGroups")
        @Response(name = "error", type = "view", value = "EditFeatureGroups")
        @Event(type = "service", invoke = "updateProductFeatureGroup")
        public static String updateProductFeatureGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFeatureGroupAppls",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureGroupAppls")
        public interface EditFeatureGroupAppls {}

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "CreateProductFeatureGroupAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditFeatureGroupAppls")
        @Response(name = "error", type = "view", value = "EditFeatureGroupAppls")
        @Event(type = "service", invoke = "createProductFeatureGroupAppl")
        public static String createProductFeatureGroupAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateProductFeatureGroupAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditFeatureGroupAppls")
        @Response(name = "error", type = "view", value = "EditFeatureGroupAppls")
        @Event(type = "service-multi", invoke = "updateProductFeatureGroupAppl")
        public static String updateProductFeatureGroupAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApplyFeaturesFromCategoryToGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditFeatureGroupAppls")
        @Response(name = "error", type = "view", value = "EditFeatureGroupAppls")
        @Event(type = "service-multi", invoke = "createProductFeatureGroupAppl")
        public static String applyFeaturesFromCategoryToGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveProductFeatureGroupAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditFeatureGroupAppls")
        @Response(name = "error", type = "view", value = "EditFeatureGroupAppls")
        @Event(type = "service", invoke = "removeProductFeatureGroupAppl")
        public static String removeProductFeatureGroupAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFeatureTypes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureTypes")
        public interface EditFeatureTypes {}

        @Request(
            uri = "EditFeatureType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureType")
        public interface EditFeatureType {}

        @Request(
            uri = "EditFeatureInterActions",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureInterActions")
        public interface EditFeatureInterActions {}

        @Request(
            uri = "EditFeatureInterAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureInterAction")
        public interface EditFeatureInterAction {}

        @Request(
            uri = "createProductFeatureIactn",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureInterAction")
        @Response(name = "error", type = "view", value = "EditFeatureInterAction")
        @Event(type = "service", invoke = "createProductFeatureIactn")
        public static String createProductFeatureIactn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductFeatureIactn",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "EditFeatureInterActions")
        @Response(name = "error", type = "view", value = "EditFeatureInterAction")
        @Event(type = "service", invoke = "removeProductFeatureIactn")
        public static String removeProductFeatureIactn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddProductFeatureIactn",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "createProductFeatureIactn")
        public static String addProductFeatureIactn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFeatureIactn",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "removeProductFeatureIactn")
        public static String removeFeatureIactn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductFeatureType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureTypes")
        @Response(name = "error", type = "view", value = "EditFeatureType")
        @Event(type = "service", invoke = "createProductFeatureType")
        public static String createProductFeatureType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductFeatureType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureType")
        @Response(name = "error", type = "view", value = "EditFeatureType")
        @Event(type = "service", invoke = "updateProductFeatureType")
        public static String updateProductFeatureType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductFeatureType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeatureTypes")
        @Response(name = "error", type = "view", value = "EditFeatureType")
        @Event(type = "service", invoke = "removeProductFeatureType")
        public static String removeProductFeatureType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListFeaturePrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        public interface ListFeaturePrice {}

        @Request(
            uri = "createFeaturePrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "createFeaturePrice")
        public static String createFeaturePrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFeaturePrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "updateFeaturePrice")
        public static String updateFeaturePrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFeaturePrice",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "deleteFeaturePrice")
        public static String deleteFeaturePrice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductFeatures",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        public interface EditProductFeatures {}

    }

    // Auto-generated split (Part 19)
    public static class Part19 {
        @Request(
            uri = "ApplyFeatureToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "applyFeatureToProduct")
        public static String applyFeatureToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApplyFeaturesToProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service-multi", invoke = "applyFeatureToProduct")
        public static String applyFeaturesToProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApplyFeaturesFromCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApplyFeaturesFromCategory")
        public interface ApplyFeaturesFromCategory {}

        @Request(
            uri = "UpdateFeatureToProductApplication",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service-multi", invoke = "updateFeatureToProductApplication")
        public static String updateFeatureToProductApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveFeatureFromProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "removeFeatureFromProduct")
        public static String removeFeatureFromProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApplyFeatureToProductFromTypeAndCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "applyFeatureToProductFromTypeAndCode")
        public static String applyFeatureToProductFromTypeAndCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductFeatureTypesAndCodes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String findProductFeatureTypesAndCodes(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.findProductFeatureTypesAndCodes
            return ProductEvents.findProductFeatureTypesAndCodes(request, response);
        }

        @Request(
            uri = "createProductFeatureApplAttr",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "createProductFeatureApplAttr")
        public static String createProductFeatureApplAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductFeatureApplAttr",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductFeatures")
        @Response(name = "error", type = "view", value = "EditProductFeatures")
        @Event(type = "service", invoke = "removeProductFeatureApplAttr")
        public static String deleteProductFeatureApplAttr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CreateVirtualWithVariantsForm",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateVirtualWithVariantsForm")
        public interface CreateVirtualWithVariantsForm {}

        @Request(
            uri = "quickCreateVirtualWithVariants",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "CreateVirtualWithVariantsForm")
        @Event(type = "service", invoke = "quickCreateVirtualWithVariants")
        public static String quickCreateVirtualWithVariants(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addVariantsToVirtual",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductAssoc")
        @Response(name = "error", type = "view", value = "QuickAddVariants")
        @Event(type = "service", invoke = "quickCreateVirtualWithVariants")
        public static String addVariantsToVirtual(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "QuickAddVariants",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickAddVariants")
        public interface QuickAddVariants {}

        @Request(
            uri = "QuickAddChosenVariant",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickAddVariants")
        @Response(name = "error", type = "view", value = "QuickAddVariants")
        public static String quickAddChosenVariant(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.VariantEvents.quickAddChosenVariant
            return VariantEvents.quickAddChosenVariant(request, response);
        }

        @Request(
            uri = "QuickAddChosenVariants",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuickAddVariants")
        @Response(name = "error", type = "view", value = "QuickAddVariants")
        @Event(type = "service-multi", invoke = "quickAddVariant")
        public static String quickAddChosenVariants(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindCatalog")
        public interface FindCatalog {}

        @Request(
            uri = "EditProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalog")
        public interface EditProdCatalog {}

        @Request(
            uri = "CreateSeoProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditProdCatalog")
        @Event(type = "service", invoke = "createMissingCategoryAndProductAltUrls")
        public static String createSeoProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalog")
        @Response(name = "error", type = "view", value = "EditProdCatalog")
        @Event(type = "service", invoke = "createProdCatalog")
        public static String createProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalog")
        @Response(name = "error", type = "view", value = "EditProdCatalog")
        @Event(type = "service", invoke = "updateProdCatalog")
        public static String updateProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 20)
    public static class Part20 {
        @Request(
            uri = "EditProdCatalogAjax",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogSection")
        public interface EditProdCatalogAjax {}

        @Request(
            uri = "EditProdCatalogCategories",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogCategories")
        public interface EditProdCatalogCategories {}

        @Request(
            uri = "addProductCategoryToProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogCategories")
        @Response(name = "error", type = "view", value = "EditProdCatalogCategories")
        @Event(type = "service", invoke = "addProductCategoryToProdCatalog")
        public static String addProductCategoryToProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategoryToProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogCategories")
        @Response(name = "error", type = "view", value = "EditProdCatalogCategories")
        @Event(type = "service-multi", invoke = "updateProductCategoryToProdCatalog")
        public static String updateProductCategoryToProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductCategoryFromProdCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogCategories")
        @Response(name = "error", type = "view", value = "EditProdCatalogCategories")
        @Event(type = "service", invoke = "removeProductCategoryFromProdCatalog")
        public static String removeProductCategoryFromProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListParentProductStoreGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListParentProductStoreGroup")
        public interface ListParentProductStoreGroup {}

        @Request(
            uri = "EditProductStoreGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreGroup")
        public interface EditProductStoreGroup {}

        @Request(
            uri = "EditProductStoreGroupAndAssoc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreGroupAndAssoc")
        public interface EditProductStoreGroupAndAssoc {}

        @Request(
            uri = "createProductStoreGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListParentProductStoreGroup")
        @Response(name = "error", type = "view", value = "EditProductStoreGroup")
        @Event(type = "service", invoke = "createProductStoreGroup")
        public static String createProductStoreGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListParentProductStoreGroup")
        @Response(name = "error", type = "view", value = "EditProductStoreGroup")
        @Event(type = "service", invoke = "updateProductStoreGroup")
        public static String updateProductStoreGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getProductStoreGroupRollupHierarchy",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getProductStoreGroupRollupHierarchy(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.store.ProductStoreEvents.getChildProductStoreGroupTree
            return ProductStoreEvents.getChildProductStoreGroupTree(request, response);
        }

        @Request(
            uri = "AddProductStoreToGroup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createProductStoreGroupMember")
        public static String addProductStoreToGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreGroupRollup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListParentProductStoreGroup")
        @Response(name = "error", type = "view", value = "EditProductStoreGroup")
        @Event(type = "service", invoke = "updateProductStoreGroupRollup")
        public static String updateProductStoreGroupRollup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProdCatalogParties",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogParties")
        public interface EditProdCatalogParties {}

        @Request(
            uri = "addProdCatalogToParty",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogParties")
        @Response(name = "error", type = "view", value = "EditProdCatalogParties")
        @Event(type = "service", invoke = "addProdCatalogToParty")
        public static String addProdCatalogToParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProdCatalogToParty",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogParties")
        @Response(name = "error", type = "view", value = "EditProdCatalogParties")
        @Event(type = "service-multi", invoke = "updateProdCatalogToParty")
        public static String updateProdCatalogToParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProdCatalogFromParty",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProdCatalogParties")
        @Response(name = "error", type = "view", value = "EditProdCatalogParties")
        @Event(type = "service", invoke = "removeProdCatalogFromParty")
        public static String removeProdCatalogFromParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromo")
        public interface FindProductPromo {}

        @Request(
            uri = "EditProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        public interface EditProductPromo {}

        @Request(
            uri = "createProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        @Response(name = "error", type = "view", value = "EditProductPromo")
        @Event(type = "service", invoke = "createProductPromo")
        public static String createProductPromo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 21)
    public static class Part21 {
        @Request(
            uri = "updateProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromo")
        @Response(name = "error", type = "view", value = "EditProductPromo")
        @Event(type = "service", invoke = "updateProductPromo")
        public static String updateProductPromo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoStores",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        public interface EditProductPromoStores {}

        @Request(
            uri = "promo_createProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "createProductStorePromoAppl")
        public static String promoCreateProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "promo_updateProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "updateProductStorePromoAppl")
        public static String promoUpdateProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "promo_deleteProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoStores")
        @Response(name = "error", type = "view", value = "EditProductPromoStores")
        @Event(type = "service", invoke = "deleteProductStorePromoAppl")
        public static String promoDeleteProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductMaints",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMaints")
        public interface EditProductMaints {}

        @Request(
            uri = "createProductMaint",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMaints")
        @Response(name = "error", type = "view", value = "EditProductMaints")
        @Event(type = "service", invoke = "createProductMaint")
        public static String createProductMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductMaint",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMaints")
        @Response(name = "error", type = "view", value = "EditProductMaints")
        @Event(type = "service", invoke = "updateProductMaint")
        public static String updateProductMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductMaint",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMaints")
        @Response(name = "error", type = "view", value = "EditProductMaints")
        @Event(type = "service", invoke = "deleteProductMaint")
        public static String deleteProductMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductMeters",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMeters")
        public interface EditProductMeters {}

        @Request(
            uri = "createProductMeter",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMeters")
        @Response(name = "error", type = "view", value = "EditProductMeters")
        @Event(type = "service", invoke = "createProductMeter")
        public static String createProductMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductMeter",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMeters")
        @Response(name = "error", type = "view", value = "EditProductMeters")
        @Event(type = "service", invoke = "updateProductMeter")
        public static String updateProductMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductMeter",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductMeters")
        @Response(name = "error", type = "view", value = "EditProductMeters")
        @Event(type = "service", invoke = "deleteProductMeter")
        public static String deleteProductMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductGeos",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGeos")
        public interface EditProductGeos {}

        @Request(
            uri = "createProductGeo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGeos")
        @Response(name = "error", type = "view", value = "EditProductGeos")
        @Event(type = "service", invoke = "createProductGeo")
        public static String createProductGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductGeo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGeos")
        @Response(name = "error", type = "view", value = "EditProductGeos")
        @Event(type = "service", invoke = "updateProductGeo")
        public static String updateProductGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductGeo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGeos")
        @Response(name = "error", type = "view", value = "EditProductGeos")
        @Event(type = "service", invoke = "deleteProductGeo")
        public static String deleteProductGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductSubscriptionResources",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductSubscriptionResources")
        public interface EditProductSubscriptionResources {}

        @Request(
            uri = "createProductSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductSubscriptionResources")
        @Response(name = "error", type = "view", value = "EditProductSubscriptionResources")
        @Event(type = "service", invoke = "createProductSubscriptionResource")
        public static String createProductSubscriptionResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductSubscriptionResources")
        @Response(name = "error", type = "view", value = "EditProductSubscriptionResources")
        @Event(type = "service", invoke = "updateProductSubscriptionResource")
        public static String updateProductSubscriptionResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 22)
    public static class Part22 {
        @Request(
            uri = "deleteProductSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductSubscriptionResources")
        @Response(name = "error", type = "view", value = "EditProductSubscriptionResources")
        @Event(type = "service", invoke = "deleteProductSubscriptionResource")
        public static String deleteProductSubscriptionResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindSubscription",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSubscription")
        public interface FindSubscription {}

        @Request(
            uri = "EditSubscription",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscription")
        public interface EditSubscription {}

        @Request(
            uri = "createSubscription",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscription")
        @Response(name = "error", type = "view", value = "EditSubscription")
        @Event(type = "service", invoke = "createSubscription")
        public static String createSubscription(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSubscription",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscription")
        @Response(name = "error", type = "view", value = "EditSubscription")
        @Event(type = "service", invoke = "updateSubscription")
        public static String updateSubscription(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSubscriptionResource")
        public interface FindSubscriptionResource {}

        @Request(
            uri = "EditSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResource")
        public interface EditSubscriptionResource {}

        @Request(
            uri = "createSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResource")
        @Response(name = "error", type = "view", value = "EditSubscriptionResource")
        @Event(type = "service", invoke = "createSubscriptionResource")
        public static String createSubscriptionResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSubscriptionResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResource")
        @Response(name = "error", type = "view", value = "EditSubscriptionResource")
        @Event(type = "service", invoke = "updateSubscriptionResource")
        public static String updateSubscriptionResource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSubscriptionResourceProducts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResourceProducts")
        public interface EditSubscriptionResourceProducts {}

        @Request(
            uri = "createProductSubscriptionResourceSr",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResourceProducts")
        @Response(name = "error", type = "view", value = "EditSubscriptionResourceProducts")
        @Event(type = "service", invoke = "createProductSubscriptionResource")
        public static String createProductSubscriptionResourceSr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductSubscriptionResourceSr",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResourceProducts")
        @Response(name = "error", type = "view", value = "EditSubscriptionResourceProducts")
        @Event(type = "service", invoke = "updateProductSubscriptionResource")
        public static String updateProductSubscriptionResourceSr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductSubscriptionResourceSr",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionResourceProducts")
        @Response(name = "error", type = "view", value = "EditSubscriptionResourceProducts")
        @Event(type = "service", invoke = "deleteProductSubscriptionResource")
        public static String deleteProductSubscriptionResourceSr(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSubscriptionAttributes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionAttributes")
        public interface EditSubscriptionAttributes {}

        @Request(
            uri = "UpdateSubscriptionAttribute",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionAttributes")
        @Event(type = "service", invoke = "updateSubscriptionAttribute")
        public static String updateSubscriptionAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSubscriptionCommEvent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionCommEvent")
        public interface EditSubscriptionCommEvent {}

        @Request(
            uri = "createSubscriptionCommEvent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionCommEvent")
        @Response(name = "error", type = "view", value = "EditSubscriptionCommEvent")
        @Event(type = "service", invoke = "createSubscriptionCommEvent")
        public static String createSubscriptionCommEvent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSubscriptionCommEvent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSubscriptionCommEvent")
        @Response(name = "error", type = "view", value = "EditSubscriptionCommEvent")
        @Event(type = "service", invoke = "removeSubscriptionCommEvent")
        public static String removeSubscriptionCommEvent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoRules",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        public interface EditProductPromoRules {}

        @Request(
            uri = "createProductPromoRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoRule")
        public static String createProductPromoRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 23)
    public static class Part23 {
        @Request(
            uri = "updateProductPromoRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoRule")
        public static String updateProductPromoRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoRule")
        public static String deleteProductPromoRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoCond")
        public static String createProductPromoCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoCond")
        public static String updateProductPromoCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupUserLoginAndPartyDetails",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUserLoginAndPartyDetails")
        public interface LookupUserLoginAndPartyDetails {}

        @Request(
            uri = "deleteProductPromoCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoCond")
        public static String deleteProductPromoCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoAction")
        public static String createProductPromoAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoAction")
        public static String updateProductPromoAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoAction")
        public static String deleteProductPromoAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoCategory")
        public static String createProductPromoCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoCategory")
        public static String updateProductPromoCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoCategory")
        public static String deleteProductPromoCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "createProductPromoProduct")
        public static String createProductPromoProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "updateProductPromoProduct")
        public static String updateProductPromoProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoRules")
        @Response(name = "error", type = "view", value = "EditProductPromoRules")
        @Event(type = "service", invoke = "deleteProductPromoProduct")
        public static String deleteProductPromoProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductPriceRules",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPriceRules")
        public interface FindProductPriceRules {}

        @Request(
            uri = "EditProductPriceRules",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        public interface EditProductPriceRules {}

        @Request(
            uri = "createProductPriceRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "FindPriceRules")
        @Event(type = "service", invoke = "createProductPriceRule")
        public static String createProductPriceRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPriceRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "updateProductPriceRule")
        public static String updateProductPriceRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPriceRule",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "deleteProductPriceRule")
        public static String deleteProductPriceRule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 24)
    public static class Part24 {
        @Request(
            uri = "createProductPriceCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "createProductPriceCond")
        public static String createProductPriceCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPriceCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "updateProductPriceCond")
        public static String updateProductPriceCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPriceCond",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "deleteProductPriceCond")
        public static String deleteProductPriceCond(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPriceAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "createProductPriceAction")
        public static String createProductPriceAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPriceAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "updateProductPriceAction")
        public static String updateProductPriceAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPriceAction",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPriceRules")
        @Response(name = "error", type = "view", value = "EditProductPriceRules")
        @Event(type = "service", invoke = "deleteProductPriceAction")
        public static String deleteProductPriceAction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getAssociatedPriceRulesConds",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getAssociatedPriceRulesConds")
        public static String getAssociatedPriceRulesConds(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        public interface FindProductPromoCode {}

        @Request(
            uri = "deleteProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCode")
        public static String deleteProductPromoCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        public interface EditProductPromoCode {}

        @Request(
            uri = "createProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCode")
        public static String createProductPromoCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "updateProductPromoCode")
        public static String updateProductPromoCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCodeEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeEmail")
        public static String createProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCodeEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCodeEmail")
        public static String deleteProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBulkProductPromoCodeEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createBulkProductPromoCodeEmail")
        public static String createBulkProductPromoCodeEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCodeParty",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeParty")
        public static String createProductPromoCodeParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductPromoCodeParty",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoCode")
        @Response(name = "error", type = "view", value = "EditProductPromoCode")
        @Event(type = "service", invoke = "deleteProductPromoCodeParty")
        public static String deleteProductPromoCodeParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductPromoCodeSet",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "createProductPromoCodeSet")
        public static String createProductPromoCodeSet(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBulkProductPromoCode",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductPromoCode")
        @Response(name = "error", type = "view", value = "FindProductPromoCode")
        @Event(type = "service", invoke = "createBulkProductPromoCode")
        public static String createBulkProductPromoCode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductStore",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductStore")
        public interface FindProductStore {}

    }

    // Auto-generated split (Part 25)
    public static class Part25 {
        @Request(
            uri = "EditProductStore",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStore")
        public interface EditProductStore {}

        @Request(
            uri = "createProductStore",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStore")
        @Response(name = "error", type = "view", value = "EditProductStore")
        @Event(type = "service", invoke = "createProductStore")
        public static String createProductStore(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStore",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStore")
        @Response(name = "error", type = "view", value = "EditProductStore")
        @Event(type = "service", invoke = "updateProductStore")
        public static String updateProductStore(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreWebSites",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreWebSites")
        public interface EditProductStoreWebSites {}

        @Request(
            uri = "storeUpdateWebSite",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreWebSites")
        @Response(name = "error", type = "view", value = "EditProductStoreWebSites")
        @Event(type = "service", invoke = "updateWebSite")
        public static String storeUpdateWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setProductStoreDefaultWebSite",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreWebSites")
        @Response(name = "error", type = "view", value = "EditProductStoreWebSites")
        @Event(type = "service", invoke = "setProductStoreDefaultWebSite")
        public static String setProductStoreDefaultWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductStoreRoles",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductStoreRoles")
        public interface FindProductStoreRoles {}

        @Request(
            uri = "storeCreateRole",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductStoreRoles")
        @Response(name = "error", type = "view", value = "FindProductStoreRoles")
        @Event(type = "service", invoke = "createProductStoreRole")
        public static String storeCreateRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeUpdateRole",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductStoreRoles")
        @Response(name = "error", type = "view", value = "FindProductStoreRoles")
        @Event(type = "service", invoke = "updateProductStoreRole")
        public static String storeUpdateRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeRemoveRole",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductStoreRoles")
        @Response(name = "error", type = "view", value = "FindProductStoreRoles")
        @Event(type = "service", invoke = "removeProductStoreRole")
        public static String storeRemoveRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStorePaySetup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePaySetup")
        public interface EditProductStorePaySetup {}

        @Request(
            uri = "storeCreatePaySetting",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePaySetup")
        @Response(name = "error", type = "view", value = "EditProductStorePaySetup")
        @Event(type = "service", invoke = "createProductStorePaymentSetting")
        public static String storeCreatePaySetting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeUpdatePaySetting",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePaySetup")
        @Response(name = "error", type = "view", value = "EditProductStorePaySetup")
        @Event(type = "service", invoke = "updateProductStorePaymentSetting")
        public static String storeUpdatePaySetting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeRemovePaySetting",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePaySetup")
        @Response(name = "error", type = "view", value = "EditProductStorePaySetup")
        @Event(type = "service", invoke = "deleteProductStorePaymentSetting")
        public static String storeRemovePaySetting(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreShipSetup",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipSetup")
        public interface EditProductStoreShipSetup {}

        @Request(
            uri = "EditProductStoreShipmentCostEstimates",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipmentCostEstimates")
        public interface EditProductStoreShipmentCostEstimates {}

        @Request(
            uri = "storeCreateShipRate",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipmentCostEstimates")
        @Response(name = "error", type = "view", value = "EditProductStoreShipmentCostEstimates")
        @Event(type = "service", invoke = "createShipmentEstimate")
        public static String storeCreateShipRate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeRemoveShipRate",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipmentCostEstimates")
        @Response(name = "error", type = "view", value = "EditProductStoreShipmentCostEstimates")
        @Event(type = "service", invoke = "removeShipmentEstimate")
        public static String storeRemoveShipRate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "prepareCreateShipMeth",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipSetup")
        @Response(name = "error", type = "view", value = "EditProductStoreShipSetup")
        @Event(type = "groovy", path = "component://product/webapp/catalog/store/prepareCreateShipMeth.groovy")
        public static String prepareCreateShipMeth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeCreateShipMeth",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipSetup")
        @Response(name = "error", type = "view", value = "EditProductStoreShipSetup")
        @Event(type = "service", invoke = "createProductStoreShipMeth")
        public static String storeCreateShipMeth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 26)
    public static class Part26 {
        @Request(
            uri = "storeUpdateShipMeth",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipSetup")
        @Response(name = "error", type = "view", value = "EditProductStoreShipSetup")
        @Event(type = "service", invoke = "updateProductStoreShipMeth")
        public static String storeUpdateShipMeth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "storeRemoveShipMeth",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreShipSetup")
        @Response(name = "error", type = "view", value = "EditProductStoreShipSetup")
        @Event(type = "service", invoke = "removeProductStoreShipMeth")
        public static String storeRemoveShipMeth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListQuantityBreaks",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuantityBreaks")
        public interface ListQuantityBreaks {}

        @Request(
            uri = "createQuantityBreak",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuantityBreaks")
        @Response(name = "error", type = "view", value = "ListQuantityBreaks")
        @Event(type = "service", invoke = "createQuantityBreak")
        public static String createQuantityBreak(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuantityBreak",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuantityBreaks")
        @Response(name = "error", type = "view", value = "ListQuantityBreaks")
        @Event(type = "service", invoke = "updateQuantityBreak")
        public static String updateQuantityBreak(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteQuantityBreak",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuantityBreaks")
        @Response(name = "error", type = "view", value = "ListQuantityBreaks")
        @Event(type = "service", invoke = "deleteQuantityBreak")
        public static String deleteQuantityBreak(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListShipmentMethodTypes",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListShipmentMethodTypes")
        public interface ListShipmentMethodTypes {}

        @Request(
            uri = "createShipmentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListShipmentMethodTypes")
        @Response(name = "error", type = "view", value = "ListShipmentMethodTypes")
        @Event(type = "service", invoke = "createShipmentMethodType")
        public static String createShipmentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipmentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListShipmentMethodTypes")
        @Response(name = "error", type = "view", value = "ListShipmentMethodTypes")
        @Event(type = "service", invoke = "updateShipmentMethodType")
        public static String updateShipmentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteShipmentMethodType",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListShipmentMethodTypes")
        @Response(name = "error", type = "view", value = "ListShipmentMethodTypes")
        @Event(type = "service", invoke = "deleteShipmentMethodType")
        public static String deleteShipmentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListCarrierShipmentMethods",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCarrierShipmentMethods")
        public interface ListCarrierShipmentMethods {}

        @Request(
            uri = "createCarrierShipmentMethod",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCarrierShipmentMethods")
        @Response(name = "error", type = "view", value = "ListCarrierShipmentMethods")
        @Event(type = "service", invoke = "createCarrierShipmentMethod")
        public static String createCarrierShipmentMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCarrierShipmentMethod",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCarrierShipmentMethods")
        @Response(name = "error", type = "view", value = "ListCarrierShipmentMethods")
        @Event(type = "service", invoke = "updateCarrierShipmentMethod")
        public static String updateCarrierShipmentMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCarrierShipmentMethod",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCarrierShipmentMethods")
        @Response(name = "error", type = "view", value = "ListCarrierShipmentMethods")
        @Event(type = "service", invoke = "deleteCarrierShipmentMethod")
        public static String deleteCarrierShipmentMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addCarrier",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewCarrier")
        @Response(name = "error", type = "view", value = "ListCarrierShipmentMethods")
        public interface AddCarrier {}

        @Request(
            uri = "createCarrier",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCarrierShipmentMethods")
        @Response(name = "error", type = "view", value = "ListCarrierShipmentMethods")
        @Event(type = "service", invoke = "createCarrier")
        public static String createCarrier(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreSurveys",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreSurveys")
        public interface EditProductStoreSurveys {}

        @Request(
            uri = "createProductStoreSurveyAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreSurveys")
        @Event(type = "service", invoke = "createProductStoreSurveyAppl")
        public static String createProductStoreSurveyAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreSurveyAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreSurveys")
        @Event(type = "service", invoke = "deleteProductStoreSurveyAppl")
        public static String deleteProductStoreSurveyAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStorePromos",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePromos")
        public interface EditProductStorePromos {}

    }

    // Auto-generated split (Part 27)
    public static class Part27 {
        @Request(
            uri = "createProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePromos")
        @Response(name = "error", type = "view", value = "EditProductStorePromos")
        @Event(type = "service", invoke = "createProductStorePromoAppl")
        public static String createProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePromos")
        @Response(name = "error", type = "view", value = "EditProductStorePromos")
        @Event(type = "service", invoke = "updateProductStorePromoAppl")
        public static String updateProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStorePromoAppl",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStorePromos")
        @Response(name = "error", type = "view", value = "EditProductStorePromos")
        @Event(type = "service", invoke = "deleteProductStorePromoAppl")
        public static String deleteProductStorePromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreCatalogs",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreCatalogs")
        public interface EditProductStoreCatalogs {}

        @Request(
            uri = "createProductStoreCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreCatalogs")
        @Response(name = "error", type = "view", value = "EditProductStoreCatalogs")
        @Event(type = "service", invoke = "createProductStoreCatalog")
        public static String createProductStoreCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreCatalogs")
        @Response(name = "error", type = "view", value = "EditProductStoreCatalogs")
        @Event(type = "service", invoke = "updateProductStoreCatalog")
        public static String updateProductStoreCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreCatalog",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreCatalogs")
        @Response(name = "error", type = "view", value = "EditProductStoreCatalogs")
        @Event(type = "service", invoke = "deleteProductStoreCatalog")
        public static String deleteProductStoreCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreEmails",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreEmails")
        public interface EditProductStoreEmails {}

        @Request(
            uri = "createProductStoreEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreEmails")
        @Response(name = "error", type = "view", value = "EditProductStoreEmails")
        @Event(type = "service", invoke = "createProductStoreEmailSetting")
        public static String createProductStoreEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreEmails")
        @Response(name = "error", type = "view", value = "EditProductStoreEmails")
        @Event(type = "service", invoke = "updateProductStoreEmailSetting")
        public static String updateProductStoreEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeProductStoreEmail",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreEmails")
        @Response(name = "error", type = "view", value = "EditProductStoreEmails")
        @Event(type = "service", invoke = "removeProductStoreEmailSetting")
        public static String removeProductStoreEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editProductStoreKeywordOvrd",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreKeywordOvrd")
        public interface EditProductStoreKeywordOvrd {}

        @Request(
            uri = "createProductStoreKeywordOvrd",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreKeywordOvrd")
        @Response(name = "error", type = "view", value = "EditProductStoreKeywordOvrd")
        @Event(type = "service", invoke = "createProductStoreKeywordOvrd")
        public static String createProductStoreKeywordOvrd(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreKeywordOvrd",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreKeywordOvrd")
        @Response(name = "error", type = "view", value = "EditProductStoreKeywordOvrd")
        @Event(type = "service", invoke = "updateProductStoreKeywordOvrd")
        public static String updateProductStoreKeywordOvrd(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreKeywordOvrd",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreKeywordOvrd")
        @Response(name = "error", type = "view", value = "EditProductStoreKeywordOvrd")
        @Event(type = "service", invoke = "deleteProductStoreKeywordOvrd")
        public static String deleteProductStoreKeywordOvrd(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewProductStoreSegments",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductStoreSegments")
        public interface ViewProductStoreSegments {}

        @Request(
            uri = "EditProductStoreFinAccountSettings",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreFinAccountSettings")
        public interface EditProductStoreFinAccountSettings {}

        @Request(
            uri = "CreateProductStoreFinAccountSettings",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreFinAccountSettings")
        @Response(name = "error", type = "view", value = "EditProductStoreFinAccountSettings")
        @Event(type = "service", invoke = "createProductStoreFinActSetting")
        public static String createProductStoreFinAccountSettings(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateProductStoreFinAccountSettings",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreFinAccountSettings")
        @Response(name = "error", type = "view", value = "EditProductStoreFinAccountSettings")
        @Event(type = "service", invoke = "updateProductStoreFinActSetting")
        public static String updateProductStoreFinAccountSettings(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RemoveProductStoreFinAccountSettings",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreFinAccountSettings")
        @Response(name = "error", type = "view", value = "EditProductStoreFinAccountSettings")
        @Event(type = "service", invoke = "removeProductStoreFinActSetting")
        public static String removeProductStoreFinAccountSettings(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 28)
    public static class Part28 {
        @Request(
            uri = "EditProductStoreVendorPayments",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorPayments")
        public interface EditProductStoreVendorPayments {}

        @Request(
            uri = "createProductStoreVendorPayment",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorPayments")
        @Response(name = "error", type = "view", value = "EditProductStoreVendorPayments")
        @Event(type = "service", invoke = "createProductStoreVendorPayment")
        public static String createProductStoreVendorPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreVendorPayment",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorPayments")
        @Response(name = "error", type = "view", value = "EditProductStoreVendorPayments")
        @Event(type = "service", invoke = "deleteProductStoreVendorPayment")
        public static String deleteProductStoreVendorPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductStoreVendorShipments",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorShipments")
        public interface EditProductStoreVendorShipments {}

        @Request(
            uri = "createProductStoreVendorShipment",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorShipments")
        @Response(name = "error", type = "view", value = "EditProductStoreVendorShipments")
        @Event(type = "service", invoke = "createProductStoreVendorShipment")
        public static String createProductStoreVendorShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreVendorShipment",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreVendorShipments")
        @Response(name = "error", type = "view", value = "EditProductStoreVendorShipments")
        @Event(type = "service", invoke = "deleteProductStoreVendorShipment")
        public static String deleteProductStoreVendorShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ProductStoreFacilities",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductStoreFacilities")
        public interface ProductStoreFacilities {}

        @Request(
            uri = "ListProductStoreFacilityFormOnly",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListProductStoreFacility")
        public interface ListProductStoreFacilityFormOnly {}

        @Request(
            uri = "editProductStoreFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStoreFacility")
        public interface EditProductStoreFacility {}

        @Request(
            uri = "addProductStoreFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductStoreFacilities")
        @Response(name = "error", type = "view", value = "ProductStoreFacilities")
        @Event(type = "service", invoke = "createProductStoreFacility")
        public static String addProductStoreFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStoreFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductStoreFacilities")
        @Response(name = "error", type = "view", value = "ProductStoreFacilities")
        @Event(type = "service", invoke = "updateProductStoreFacility")
        public static String updateProductStoreFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductStoreFacility",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductStoreFacilities")
        @Response(name = "error", type = "view", value = "ProductStoreFacilities")
        @Event(type = "service", invoke = "deleteProductStoreFacility")
        public static String deleteProductStoreFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editKeywordThesaurus",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditKeywordThesaurus")
        public interface EditKeywordThesaurus {}

        @Request(
            uri = "createKeywordThesaurus",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditKeywordThesaurus")
        @Response(name = "error", type = "view", value = "EditKeywordThesaurus")
        @Event(type = "service", invoke = "createKeywordThesaurus")
        public static String createKeywordThesaurus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteKeywordThesaurus",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditKeywordThesaurus")
        @Response(name = "error", type = "view", value = "EditKeywordThesaurus")
        @Event(type = "service", invoke = "deleteKeywordThesaurus")
        public static String deleteKeywordThesaurus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductReview",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "FindReviews")
        @Response(name = "error", type = "view", value = "FindReviews")
        @Event(type = "service", invoke = "updateProductReview")
        public static String updateProductReview(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindReviews",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindReviews")
        public interface FindReviews {}

        @Request(
            uri = "updateProductReviewStatus",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindReviews")
        @Response(name = "error", type = "view", value = "FindReviews")
        @Event(type = "service", invoke = "setProductReviewStatus")
        public static String updateProductReviewStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductSuppliers",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSupplierProduct")
        public interface EditProductSuppliers {}

        @Request(
            uri = "createSupplierProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSupplierProduct")
        @Response(name = "error", type = "view", value = "EditSupplierProduct")
        @Event(type = "service", invoke = "createSupplierProduct")
        public static String createSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 29)
    public static class Part29 {
        @Request(
            uri = "updateSupplierProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSupplierProduct")
        @Response(name = "error", type = "view", value = "EditSupplierProduct")
        @Event(type = "service", invoke = "updateSupplierProduct")
        public static String updateSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSupplierProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSupplierProduct")
        @Response(name = "error", type = "view", value = "EditSupplierProduct")
        @Event(type = "service", invoke = "removeSupplierProduct")
        public static String removeSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSupplierProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "createSupplierProductFeature")
        public static String createSupplierProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSupplierProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "updateSupplierProductFeature")
        public static String updateSupplierProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSupplierProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFeature")
        @Response(name = "error", type = "view", value = "EditFeature")
        @Event(type = "service", invoke = "removeSupplierProductFeature")
        public static String removeSupplierProductFeature(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductConfigs",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigs")
        public interface EditProductConfigs {}

        @Request(
            uri = "ProductConfigItemArticle",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductConfigItemArticle")
        public interface ProductConfigItemArticle {}

        @Request(
            uri = "createProductConfig",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigs")
        @Response(name = "error", type = "view", value = "EditProductConfigs")
        @Event(type = "service", invoke = "createProductConfig")
        public static String createProductConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductConfig",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigs")
        @Response(name = "error", type = "view", value = "EditProductConfigs")
        @Event(type = "service", invoke = "updateProductConfig")
        public static String updateProductConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductConfig",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigs")
        @Response(name = "error", type = "view", value = "EditProductConfigs")
        @Event(type = "service", invoke = "deleteProductConfig")
        public static String deleteProductConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindProductConfigItems",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindProductConfigItems")
        public interface FindProductConfigItems {}

        @Request(
            uri = "EditProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItem")
        public interface EditProductConfigItem {}

        @Request(
            uri = "createProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItem")
        @Response(name = "error", type = "view", value = "EditProductConfigItem")
        @Event(type = "service", invoke = "createProductConfigItem")
        public static String createProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItem")
        @Response(name = "error", type = "view", value = "EditProductConfigItem")
        @Event(type = "service", invoke = "updateProductConfigItem")
        public static String updateProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItem")
        @Response(name = "error", type = "view", value = "EditProductConfigItem")
        @Event(type = "service", invoke = "deleteProductConfigItem")
        public static String deleteProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductConfigOptions",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        public interface EditProductConfigOptions {}

        @Request(
            uri = "createProductConfigOption",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "createProductConfigOption")
        public static String createProductConfigOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductConfigOption",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "updateProductConfigOption")
        public static String updateProductConfigOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductConfigOption",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "deleteProductConfigOption")
        public static String deleteProductConfigOption(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createProductConfigProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "createProductConfigProduct")
        public static String createProductConfigProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 30)
    public static class Part30 {
        @Request(
            uri = "updateProductConfigProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "updateProductConfigProduct")
        public static String updateProductConfigProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductConfigProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigOptions")
        @Response(name = "error", type = "view", value = "EditProductConfigOptions")
        @Event(type = "service", invoke = "deleteProductConfigProduct")
        public static String deleteProductConfigProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductConfigItemContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        public interface EditProductConfigItemContent {}

        @Request(
            uri = "updateProductConfigItemContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContent")
        @Event(type = "service", invoke = "updateProductConfigItem")
        public static String updateProductConfigItemContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UploadProductConfigItemImage",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        public interface UploadProductConfigItemImage {}

        @Request(
            uri = "EditProductConfigItemContentContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContentContent")
        public interface EditProductConfigItemContentContent {}

        @Request(
            uri = "prepareAddContentToProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContentContent")
        public interface PrepareAddContentToProductConfigItem {}

        @Request(
            uri = "addContentToProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContent")
        @Event(type = "service", invoke = "createProductConfigItemContent")
        public static String addContentToProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContentToProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContent")
        @Event(type = "service", invoke = "updateProductConfigItemContent")
        public static String updateContentToProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeContentFromProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContent")
        @Event(type = "service", invoke = "removeProductConfigItemContent")
        public static String removeContentFromProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSimpleTextContentForProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContentContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContentContent")
        @Event(type = "service", invoke = "updateSimpleTextContentForProductConfigItem")
        public static String updateSimpleTextContentForProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createSimpleTextContentForProductConfigItem",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductConfigItemContent")
        @Response(name = "error", type = "view", value = "EditProductConfigItemContentContent")
        @Event(type = "service", invoke = "createSimpleTextContentForProductConfigItem")
        public static String createSimpleTextContentForProductConfigItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductWorkEfforts",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductWorkEfforts")
        public interface EditProductWorkEfforts {}

        @Request(
            uri = "createWorkEffortGoodStandard",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductWorkEfforts")
        @Response(name = "error", type = "view", value = "EditProductWorkEfforts")
        @Event(type = "service", invoke = "createWorkEffortGoodStandard")
        public static String createWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWorkEffortGoodStandard",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductWorkEfforts")
        @Response(name = "error", type = "view", value = "EditProductWorkEfforts")
        @Event(type = "service", invoke = "updateWorkEffortGoodStandard")
        public static String updateWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeWorkEffortGoodStandard",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductWorkEfforts")
        @Response(name = "error", type = "view", value = "EditProductWorkEfforts")
        @Event(type = "service", invoke = "removeWorkEffortGoodStandard")
        public static String removeWorkEffortGoodStandard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewProductOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductOrder")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "service", invoke = "findOrders")
        public static String viewProductOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductCommunicationEvents",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCommunicationEvents")
        public interface EditProductCommunicationEvents {}

        @Request(
            uri = "AddCommEventForProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCommunicationEvent")
        public interface AddCommEventForProduct {}

        @Request(
            uri = "createCommunicationEvent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCommunicationEvents")
        @Response(name = "error", type = "view", value = "EditProductCommunicationEvents")
        @Event(type = "service", invoke = "createCommunicationEvent")
        public static String createCommunicationEvent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 31)
    public static class Part31 {
        @Request(
            uri = "LookupContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}

        @Request(
            uri = "LookupFixedAsset",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFixedAsset")
        public interface LookupFixedAsset {}

        @Request(
            uri = "LookupPartyName",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

        @Request(
            uri = "LookupCommEvent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCommEvent")
        public interface LookupCommEvent {}

        @Request(
            uri = "LookupProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupSupplierProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSupplierProduct")
        public interface LookupSupplierProduct {}

        @Request(
            uri = "LookupVariantProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupVirtualProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVirtualProduct")
        public interface LookupVirtualProduct {}

        @Request(
            uri = "LookupProductCategory",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductCategory")
        public interface LookupProductCategory {}

        @Request(
            uri = "LookupProductFeature",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "LookupProductStore",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductStore")
        public interface LookupProductStore {}

        @Request(
            uri = "LookupFacilityLocation",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacilityLocation")
        public interface LookupFacilityLocation {}

        @Request(
            uri = "LookupWorkEffort",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "LookupCostComponentCalc",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCostComponentCalc")
        public interface LookupCostComponentCalc {}

        @Request(
            uri = "LookupDataResource",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupDataResource")
        public interface LookupDataResource {}

        @Request(
            uri = "LookupPerson",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupPreferredContactMech",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPreferredContactMech")
        public interface LookupPreferredContactMech {}

        @Request(
            uri = "LookupContactList",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContactList")
        public interface LookupContactList {}

        @Request(
            uri = "EditVendorProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVendorProduct")
        public interface EditVendorProduct {}

    }

    // Auto-generated split (Part 32)
    public static class Part32 {
        @Request(
            uri = "createVendorProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVendorProduct")
        @Response(name = "error", type = "view", value = "EditVendorProduct")
        @Event(type = "service", invoke = "createVendorProduct")
        public static String createVendorProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteVendorProduct",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVendorProduct")
        @Response(name = "error", type = "view", value = "EditVendorProduct")
        @Event(type = "service", invoke = "deleteVendorProduct")
        public static String deleteVendorProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProductPromoContent",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        public interface EditProductPromoContent {}

        @Request(
            uri = "removeContentFromProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        @Event(type = "service", invoke = "removeProductPromoContent")
        public static String removeContentFromProductPromo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addImageContentForProductPromo",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductPromoContent")
        @Response(name = "error", type = "view", value = "EditProductPromoContent")
        @Event(type = "service", invoke = "addImageForProductPromo")
        public static String addImageContentForProductPromo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getChild",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getChild(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: com.ilscipio.scipio.product.category.CategoryEvents.getChildCategoryTree
            return CategoryEvents.getChildCategoryTree(request, response);
        }

        @Request(
            uri = "listMiniproduct",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "listMiniproduct")
        public interface ListMiniproduct {}

        @Request(
            uri = "ViewProductGroupOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductGroupOrder")
        public interface ViewProductGroupOrder {}

        @Request(
            uri = "EditProductGroupOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGroupOrder")
        public interface EditProductGroupOrder {}

        @Request(
            uri = "createProductGroupOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductGroupOrder")
        @Response(name = "error", type = "view", value = "ViewProductGroupOrder")
        @Event(type = "service", invoke = "createProductGroupOrder")
        public static String createProductGroupOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductGroupOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductGroupOrder")
        @Response(name = "error", type = "view", value = "EditProductGroupOrder")
        @Event(type = "service", invoke = "updateProductGroupOrder")
        public static String updateProductGroupOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductGroupOrder",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewProductGroupOrder")
        @Response(name = "error", type = "view", value = "ViewProductGroupOrder")
        @Event(type = "service", invoke = "deleteProductGroupOrder")
        public static String deleteProductGroupOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getProductCategoryContentLocalizedSimpleTextViews",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getProductCategoryContentLocalizedSimpleTextViews")
        public static String getProductCategoryContentLocalizedSimpleTextViews(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getProductContentLocalizedSimpleTextViews",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getProductContentLocalizedSimpleTextViews")
        public static String getProductContentLocalizedSimpleTextViews(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getProductCategoryExtendedData",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getProductCategoryExtendedDataVersatile")
        public static String getProductCategoryExtendedData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getProductExtendedData",
            controller = "catalog",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getProductExtendedDataVersatile")
        public static String getProductExtendedData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ScpCatalogCommon.js",
            controller = "catalog",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ScpCatalogCommon.js")
        public interface ScpCatalogCommonJs {}


    }
}
