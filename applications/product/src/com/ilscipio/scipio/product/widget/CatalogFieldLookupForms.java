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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogFieldLookupForms {

    @Form(
        name = "lookupProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", textFind = @TextFindField),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", textFind = @TextFindField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", textFind = @TextFindField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "primaryProductCategoryId", title = "${uiLabelMap.ProductPrimaryCategory}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductCategory", description = "${description}", keyFieldName = "productCategoryId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupProduct {}

    @Form(
        name = "listLookupProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupProduct",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productId}')", urlMode = UrlMode.PLAIN, description = "${productId}", alsoHidden = false)),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", display = @DisplayField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", display = @DisplayField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", displayEntity = @DisplayEntityField(entityName = "ProductType")),
            @FormField(name = "searchAction", title = " ", useWhen = "hasVariants", widgetStyle = "${styles.link_nav} ${styles.action_find}", hyperlink = @HyperlinkField(target = "LookupVariantProduct", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.ProductVariants}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Product"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "hasVariants", value = "${groovy: org.ofbiz.entity.util.EntityUtil.filterByDate(delegator.findByAnd('ProductAssoc', org.ofbiz.base.util.UtilMisc.toMap('productId', productId, 'productAssocTypeId', 'PRODUCT_VARIANT'), null, true)).size() > 0}", type = "Boolean")})
    )
    public interface listLookupProduct {}

    @Form(
        name = "lookupSupplierProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupSupplierProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", textFind = @TextFindField),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", textFind = @TextFindField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", textFind = @TextFindField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupSupplierProduct {}

    @Form(
        name = "listLookupSupplierProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSupplierProduct",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productId}')", urlMode = UrlMode.PLAIN, description = "${productId}", alsoHidden = false)),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", display = @DisplayField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", display = @DisplayField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", displayEntity = @DisplayEntityField(entityName = "ProductType")),
            @FormField(name = "supplierProductId", display = @DisplayField),
            @FormField(name = "minimumOrderQuantity", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "fieldList", value = "${groovy:['productId','partyId','lastPrice','shippingPrice','currencyUomId','supplierProductName','supplierProductId','canDropShip']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SupplierProductAndProduct"), @FieldMap(fieldName = "orderBy", value = "productId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "distinct", fromField = "searchDistinct")})})
    )
    public interface listLookupSupplierProduct {}

    @Form(
        name = "lookupVirtualProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupVirtualProduct",
        defaultMapName = "inputFields",
        extendsForm = "lookupProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "isVirtual", hidden = @HiddenField(value = "Y"))
        }
    )
    public interface lookupVirtualProduct {}

    @Form(
        name = "listLookupVirtualProduct",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        extendsForm = "listLookupProduct",
        paginateTarget = "LookupVirtualProduct",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productId}')", urlMode = UrlMode.PLAIN, description = "${productId}", alsoHidden = false)),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", display = @DisplayField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", display = @DisplayField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", displayEntity = @DisplayEntityField(entityName = "ProductType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Product"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupVirtualProduct {}

    @Form(
        name = "lookupProductAndPrice",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupProductAndPrice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", textFind = @TextFindField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", textFind = @TextFindField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "primaryProductCategoryId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductCategory", description = "${description}", keyFieldName = "productCategoryId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.ProductPriceType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPriceType", description = "${description}", keyFieldName = "productPriceTypeId"))),
            @FormField(name = "productPricePurposeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPricePurpose", description = "${description}", keyFieldName = "productPricePurposeId"))),
            @FormField(name = "price", title = "${uiLabelMap.ProductPrice}", rangeFind = @RangeFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "filterByDate", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupProductAndPrice {}

    @Form(
        name = "listLookupProductAndPrice",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupProductAndPrice",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productId}')", urlMode = UrlMode.PLAIN, description = "${productId}", alsoHidden = false)),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", display = @DisplayField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", displayEntity = @DisplayEntityField(entityName = "ProductType")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "price", title = "${uiLabelMap.ProductPrice}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "searchAction", title = " ", useWhen = "hasVariants", widgetStyle = "${styles.link_nav} ${styles.action_find}", hyperlink = @HyperlinkField(target = "LookupVariantProduct", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.ProductVariants}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductAndPriceView"), @FieldMap(fieldName = "orderBy", value = "productId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "hasVariants", value = "${groovy: delegator.findByAnd('ProductAssoc', org.ofbiz.base.util.UtilMisc.toMap('productId', productId, 'productAssocTypeId', 'PRODUCT_VARIANT'), null, true).size() > 0}", type = "Boolean")})
    )
    public interface listLookupProductAndPrice {}

    @Form(
        name = "lookupProductCategory",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupProductCategory",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategory", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "productCategoryTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductCategoryType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "showInSelect", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        sortOrder = @SortOrder()
    )
    public interface lookupProductCategory {}

    @Form(
        name = "listLookupProductCategory",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupProductCategory",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategory", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productCategoryTypeId", displayEntity = @DisplayEntityField(entityName = "ProductCategoryType")),
            @FormField(name = "productCategoryId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productCategoryId}')", urlMode = UrlMode.PLAIN, description = "${productCategoryId}", alsoHidden = false)),
            @FormField(name = "longDescription", ignored = @IgnoredField),
            @FormField(name = "categoryImageUrl", ignored = @IgnoredField),
            @FormField(name = "linkOneImageUrl", ignored = @IgnoredField),
            @FormField(name = "linkTwoImageUrl", ignored = @IgnoredField),
            @FormField(name = "detailScreen", ignored = @IgnoredField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductCategory"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupProductCategory {}

    @Form(
        name = "lookupProductFeature",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupProductFeature",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeature", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "productFeatureTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureCategoryId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureCategory", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupProductFeature {}

    @Form(
        name = "listLookupProductFeature",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupProductFeature",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeature", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productFeatureId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productFeatureId}')", urlMode = UrlMode.PLAIN, description = "${productFeatureId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductFeature"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupProductFeature {}

    @Form(
        name = "LookupProductStore",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupProductStore",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", textFind = @TextFindField),
            @FormField(name = "storeName", textFind = @TextFindField),
            @FormField(name = "companyName", textFind = @TextFindField),
            @FormField(name = "payToPartyId", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupProductStore {}

    @Form(
        name = "listLookupProductStore",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupProductStore",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "productStoreId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productStoreId}')", urlMode = UrlMode.PLAIN, description = "${productStoreId}", alsoHidden = false)),
            @FormField(name = "primaryStoreGroupId", display = @DisplayField),
            @FormField(name = "storeName", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductStore"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupProductStore {}

    @Form(
        name = "lookupCostComponentCalc",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        target = "LookupCostComponentCalc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "costComponentCalcId", title = "${uiLabelMap.ProductCostComponentCalcId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "costGlAccountTypeId", title = "${uiLabelMap.ProductCostGlAccountTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "offsettingGlAccountTypeId", title = "${uiLabelMap.ProductOffsettingGlAccountTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCostComponentCalc {}

    @Form(
        name = "listLookupCostComponentCalc",
        location = "component://product/widget/catalog/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCostComponentCalc",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "costComponentCalcId", title = "${uiLabelMap.ProductCostComponentCalcId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${costComponentCalcId}')", urlMode = UrlMode.PLAIN, description = "${costComponentCalcId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "costGlAccountTypeId", title = "${uiLabelMap.ProductCostGlAccountTypeId}", display = @DisplayField),
            @FormField(name = "offsettingGlAccountTypeId", title = "${uiLabelMap.ProductOffsettingGlAccountTypeId}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CostComponentCalc"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupCostComponentCalc {}

}
