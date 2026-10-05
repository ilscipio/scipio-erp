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
public class CatalogProductForms {

    @Form(
        name = "FindProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "FindProduct",
        defaultMapName = "product",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productName", position = 2, lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productName_op", hidden = @HiddenField(value = "contains")),
            @FormField(name = "productId", title = "${uiLabelMap.CommonId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "internalName_op", hidden = @HiddenField(value = "contains")),
            @FormField(name = "productTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindProduct {}

    @Form(
        name = "ListProducts",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindProduct",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "ViewProduct", description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "internalName", title = "${uiLabelMap.CommonName}", sortField = true, display = @DisplayField),
            @FormField(name = "productName", sortField = true, display = @DisplayField),
            @FormField(name = "brandName", sortField = true, display = @DisplayField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "ProductType", description = "${description}")),
            @FormField(name = "description", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "Product")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListProducts {}

    @Form(
        name = "EditProductDup",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "DuplicateProduct",
        defaultMapName = "product",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "oldProductId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "productId", mapName = "dupProduct", title = "${uiLabelMap.ProductDuplicateRemoveSelectedWithNewId}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "newInternalName", title = "${uiLabelMap.ProductInternalName}", text = @TextField(size = 30, maxlength = 255)),
            @FormField(name = "newProductName", title = "${uiLabelMap.ProductProductName}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "newDescription", title = "${uiLabelMap.ProductProductDescription}", widgetStyle = "textAreaBox", textarea = @TextareaField(rows = 2)),
            @FormField(name = "newLongDescription", title = "${uiLabelMap.ProductLongDescription}", widgetStyle = "textAreaBox dojo-ResizableTextArea", textarea = @TextareaField(rows = 7)),
            @FormField(name = "duplicateTitle", mapName = "dummy", title = "${uiLabelMap.CommonDuplicate}", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "duplicatePrices", title = "${uiLabelMap.ProductPrices}", check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateIDs", title = "${uiLabelMap.CommonId}", position = 2, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateContent", title = "${uiLabelMap.ProductContent}", position = 3, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateCategoryMembers", title = "${uiLabelMap.ProductCategoryMembers}", position = 4, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateAssocs", title = "${uiLabelMap.ProductAssocs}", check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateAttributes", title = "${uiLabelMap.ProductAttributes}", position = 2, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateFeatureAppls", title = "${uiLabelMap.ProductFeatureAppls}", position = 3, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateInventoryItems", title = "${uiLabelMap.ProductInventoryItems}", position = 4, check = @CheckField),
            @FormField(name = "removeTitle", mapName = "dummy", title = "${uiLabelMap.CommonRemove}", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "removePrices", title = "${uiLabelMap.ProductPrices}", check = @CheckField),
            @FormField(name = "removeIDs", title = "${uiLabelMap.CommonId}", position = 2, check = @CheckField),
            @FormField(name = "removeContent", title = "${uiLabelMap.ProductContent}", position = 3, check = @CheckField),
            @FormField(name = "removeCategoryMembers", title = "${uiLabelMap.ProductCategoryMembers}", position = 4, check = @CheckField),
            @FormField(name = "removeAssocs", title = "${uiLabelMap.ProductAssocs}", check = @CheckField),
            @FormField(name = "removeAttributes", title = "${uiLabelMap.ProductAttributes}", position = 2, check = @CheckField),
            @FormField(name = "removeFeatureAppls", title = "${uiLabelMap.ProductFeatureAppls}", position = 3, check = @CheckField),
            @FormField(name = "removeInventoryItems", title = "${uiLabelMap.ProductInventoryItems}", position = 4, check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonDuplicate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductDup {}

    @Form(
        name = "UpdateProductVariants",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "UpdateProductVariants?productId=${productId}",
        defaultMapName = "product",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "virtualProductId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "removeBefore", title = "${uiLabelMap.ProductRemoveBefore}", check = @CheckField),
            @FormField(name = "duplicatePrices", title = "${uiLabelMap.ProductPrices}", position = 2, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateIDs", title = "${uiLabelMap.CommonId}", position = 3, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateContent", title = "${uiLabelMap.ProductContent}", position = 4, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateCategoryMembers", title = "${uiLabelMap.ProductCategoryMembers}", check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateAttributes", title = "${uiLabelMap.ProductAttributes}", position = 2, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateFacilities", title = "${uiLabelMap.ProductFacilities}", position = 3, check = @CheckField(allChecked = true)),
            @FormField(name = "duplicateLocations", title = "${uiLabelMap.ProductLocations}", position = 4, check = @CheckField(allChecked = true)),
            @FormField(name = "commonGoAction", title = "${uiLabelMap.CommonGo}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface UpdateProductVariants {}

    @Form(
        name = "AddProductPrice",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductPrice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "price", requiredField = true, text = @TextField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productPricePurposeId", title = "${uiLabelMap.CommonPurpose}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPricePurpose", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPriceType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthCombinedId", title = "${uiLabelMap.AccountingTaxAuthority}", widgetStyle = "+AddProductPrice_taxAuthCombinedId_field", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "taxAuthorityInfoList", keyName = "taxAuthCombinedId", description = "${party.groupName} ${party.firstName} ${party.lastName} [${taxAuthPartyId}] / ${geo.geoName} [${taxAuthGeoId}]"))),
            @FormField(name = "taxInPrice", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "taxPercentage", title = "${uiLabelMap.CommonTax} ${uiLabelMap.CommonPercentage}", text = @TextField),
            @FormField(name = "productStoreGroupId", title = "${uiLabelMap.CommonStore} ${uiLabelMap.CommonGroup}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStoreGroup", description = "${productStoreGroupName}", orderBy = {@EntityOrderBy(fieldName = "productStoreGroupName")}))),
            @FormField(name = "termUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "${typeDescription}: ${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "typeDescription"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "customPriceCalcService", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "PRICE_FORMULA")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/tax/GetTaxAuthorityListForDisplay.groovy")})
    )
    public interface AddProductPrice {}

    @Form(
        name = "UpdateProductPrice",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductPrice",
        listName = "productPrices",
        paginateTarget = "EditProductPrices",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productPricePurposeId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "ProductPricePurpose")),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductPriceType")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${uomId}")),
            @FormField(name = "productStoreGroupId", title = "${uiLabelMap.CommonStore} ${uiLabelMap.CommonGroup}", displayEntity = @DisplayEntityField(entityName = "ProductStoreGroup", description = "${productStoreGroupName}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "price", requiredField = true, text = @TextField),
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthCombinedId", entryName = "'${taxAuthGeoId}::${taxAuthPartyId}'", title = "${uiLabelMap.AccountingTaxAuthority}", widgetStyle = "+UpdateProductPrice_taxAuthCombinedId_field", dropDown = @DropDownField(allowEmpty = true, current = "selected", listOptions = @ListOptions(listName = "taxAuthorityInfoList", keyName = "taxAuthCombinedId", description = "${party.groupName} ${party.firstName} ${party.lastName} [${taxAuthPartyId}] / ${geo.geoName} [${taxAuthGeoId}]"))),
            @FormField(name = "taxInPrice", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "taxPercentage", title = "${uiLabelMap.CommonTax} ${uiLabelMap.CommonPercentage}", text = @TextField),
            @FormField(name = "termUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "${typeDescription}: ${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "typeDescription"), @EntityOrderBy(fieldName = "uomId")}))),
            @FormField(name = "customPriceCalcService", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "PRICE_FORMULA")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "viewHistoryAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ProductPriceHistory", description = "${uiLabelMap.ProductHistory}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productPriceTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPrice", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productPriceTypeId"), @ParameterDef(paramName = "productPricePurposeId"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "productStoreGroupId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/tax/GetTaxAuthorityListForDisplay.groovy")})
    )
    public interface UpdateProductPrice {}

    @Form(
        name = "ListProductPriceHistory",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "productPricesChanges",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productPricePurposeId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "ProductPricePurpose")),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductPriceType")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "price", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "oldPrice", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "productStoreGroupId", title = "${uiLabelMap.CommonStore} ${uiLabelMap.CommonGroup}", displayEntity = @DisplayEntityField(entityName = "ProductStoreGroup", description = "${productStoreGroupName}")),
            @FormField(name = "changedByUserLogin", title = "${uiLabelMap.ProductLastModifiedBy}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}")),
            @FormField(name = "changedDate", display = @DisplayField)
        }
    )
    public interface ListProductPriceHistory {}

    @Form(
        name = "AddProductPaymentMethodType",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductPaymentMethodType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productPricePurposeId", title = "${uiLabelMap.CommonPurpose}", widgetStyle = "+smallSelect", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPricePurpose", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonType}", widgetStyle = "+smallSelect", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductPaymentMethodType {}

    @Form(
        name = "UpdateProductPaymentMethodType",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductPaymentMethodType",
        listName = "productPaymentMethodTypes",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productPricePurposeId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "ProductPricePurpose")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPaymentMethodType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productPricePurposeId"), @ParameterDef(paramName = "paymentMethodTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductPaymentMethodType {}

    @Form(
        name = "AddProductCategoryMember",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "addProductToCategory",
        title = "${uiLabelMap.ProductAddProductCategoryMemberFromDate}:",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", mapName = "product", title = "${uiLabelMap.ProductProductId}", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonCategory}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.ProductSequenceNum}", text = @TextField),
            @FormField(name = "quantity", title = "${uiLabelMap.ProductQuantity}", position = 2, text = @TextField),
            @FormField(name = "comments", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductCategoryMember {}

    @Form(
        name = "UpdateProductCategoryMember",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductToCategory",
        listName = "productCategoryMembers",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonCategory}", displayEntity = @DisplayEntityField(entityName = "ProductCategory", description = "${categoryName}", subHyperlink = @SubHyperlink(target = "EditCategory", description = "${productCategoryId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productCategoryId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.ProductSequenceNum}", text = @TextField),
            @FormField(name = "quantity", title = "${uiLabelMap.ProductQuantity}", text = @TextField),
            @FormField(name = "comments", title = "${uiLabelMap.ProductComments}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductFromCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductCategoryMember {}

    @Form(
        name = "ListProductContentInfos",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "productContent",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "editProductContentInfo", title = "${uiLabelMap.ProductContent}", widgetStyle = "${styles.link_nav_info_desc}", hyperlink = @HyperlinkField(target = "EditProductContentContent", description = "${description} [${contentId}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "productContentTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "productContentTypeId", title = "${uiLabelMap.ProductType}", displayEntity = @DisplayEntityField(entityName = "ProductContentType", description = "${description}", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "purchaseFromDate", display = @DisplayField(type = "date")),
            @FormField(name = "purchaseThruDate", display = @DisplayField(type = "date")),
            @FormField(name = "useCountLimit", display = @DisplayField),
            @FormField(name = "useTime", display = @DisplayField),
            @FormField(name = "useTimeUomId", display = @DisplayField),
            @FormField(name = "useRoleTypeId", display = @DisplayField),
            @FormField(name = "sequenceNum", display = @DisplayField),
            @FormField(name = "editContentAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "/content/control/EditContent", urlMode = UrlMode.INTER_APP, description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "removeContent", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentFromProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "productContentTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductContentInfos {}

    @Form(
        name = "AddProductContentAssoc",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "addContentToProduct",
        title = "${uiLabelMap.ProductAddProductContentFromDate}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent")
        },
        fields = {
            @FormField(name = "productId", mapName = "product", title = "${uiLabelMap.ProductProductId}", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "productContentTypeId", title = "${uiLabelMap.ProductProductContentTypeId}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "purchaseFromDate", title = "${uiLabelMap.ProductPurchaseFromDate}"),
            @FormField(name = "purchaseThruDate", title = "${uiLabelMap.ProductPurchaseThruDate}", position = 2),
            @FormField(name = "useCountLimit", title = "${uiLabelMap.ProductUseCountLimit}"),
            @FormField(name = "useTime", title = "${uiLabelMap.ProductUseTime}"),
            @FormField(name = "useTimeUomId", title = "${uiLabelMap.ProductUseTimeUom}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", title = "${uiLabelMap.ProductUseRole}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductContentAssoc {}

    @Form(
        name = "PrepareAddProductContentAssoc",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "prepareAddContentToProduct",
        title = "${uiLabelMap.ProductAddProductContentFromDate}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", mapName = "product", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", title = "${uiLabelMap.ProductProductContentTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductPrepareCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface PrepareAddProductContentAssoc {}

    @Form(
        name = "UpdateProductContentAssoc",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateContentToProduct",
        listName = "productContentDatas",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductContent", mapName = "productContent")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContent_Id}", widgetStyle = "${styles.link_nav_info_desc}", hyperlink = @HyperlinkField(target = "EditProductContentContent", description = "${content.description} [${productContent.contentId}]", parameters = {@ParameterDef(paramName = "productId", fromField = "productContent.productId"), @ParameterDef(paramName = "contentId", fromField = "productContent.contentId")})),
            @FormField(name = "productContentTypeId", title = "${uiLabelMap.ProductProductContentTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentFromProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId", fromField = "productContent.productId"), @ParameterDef(paramName = "contentId", fromField = "productContent.contentId"), @ParameterDef(paramName = "productContentTypeId", fromField = "productContent.productContentTypeId"), @ParameterDef(paramName = "fromDate", fromField = "productContent.fromDate")}))
        }
    )
    public interface UpdateProductContentAssoc {}

    @Form(
        name = "ListAssociatedContentInfos",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "contentInfos",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "caContentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType", keyFieldName = "contentAssocTypeId", description = "${description}", alsoHidden = false)),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType", description = "${description}", alsoHidden = false)),
            @FormField(name = "localeString", display = @DisplayField),
            @FormField(name = "drDataResourceTypeId", title = "${uiLabelMap.FormFieldTitle_dataResourceTypeId}", displayEntity = @DisplayEntityField(entityName = "DataResourceType", keyFieldName = "dataResourceTypeId", description = "${description}", alsoHidden = false)),
            @FormField(name = "editDataResourceAction", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "/content/control/EditDataResource", urlMode = UrlMode.INTER_APP, description = "${dataResourceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceId")})),
            @FormField(name = "editContentAction", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "/content/control/EditContent", urlMode = UrlMode.INTER_APP, description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId")}))
        }
    )
    public interface ListAssociatedContentInfos {}

    @Form(
        name = "CreateSimpleTextContentForAlternateLocale",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createSimpleTextContentForAlternateLocale",
        title = "${uiLabelMap.ProductCreateSimpleTextContentForAlternateLocale}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "mainContentId", entryName = "contentId", hidden = @HiddenField),
            @FormField(name = "localeString", text = @TextField),
            @FormField(name = "text", text = @TextField),
            @FormField(name = "fromDate", mapName = "productContent", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", mapName = "productContent", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateSimpleTextContentForAlternateLocale {}

    @Form(
        name = "ListSimpleTextContentForAlternateLocaleCore",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.MULTI,
        listName = "contentAssocList",
        listEntryName = "contentAssoc",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "mainContentId", mapName = "contentAssoc", entryName = "contentId", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "contentAssoc", entryName = "contentIdTo", hidden = @HiddenField),
            @FormField(name = "localeString", mapName = "content", display = @DisplayField(description = "${content.localeString}")),
            @FormField(name = "text", mapName = "electronicText", entryName = "textData", text = @TextField)
        },
        actions = @FormActions(set = {@SetAction(field = "useRequestParameters", value = "false", type = "Boolean")}),
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Content", valueField = "content"), @EntityOneAction(entityName = "ElectronicText", valueField = "electronicText")})
    )
    public interface ListSimpleTextContentForAlternateLocaleCore {}

    @Form(
        name = "ListSimpleTextContentForAlternateLocale",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateSimpleTextContentForAlternateLocale?productId=${productContent.productId}&productContentTypeId=${productContent.productContentTypeId}&fromDate=${productContent.fromDate}&contentId=${contentId}",
        extendsForm = "ListSimpleTextContentForAlternateLocaleCore",
        fields = {
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSimpleTextContentForAlternateLocale", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, linkType = "hidden-form", parameters = {@ParameterDef(paramName = "mainContentId", fromField = "contentAssoc.contentId"), @ParameterDef(paramName = "contentId", fromField = "contentAssoc.contentIdTo"), @ParameterDef(paramName = "productId", fromField = "productContent.productId"), @ParameterDef(paramName = "productContentTypeId", fromField = "productContent.productContentTypeId"), @ParameterDef(paramName = "fromDate", fromField = "productContent.fromDate")}))
        }
    )
    public interface ListSimpleTextContentForAlternateLocale {}

    @Form(
        name = "EditProductContentEmail",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateEmailContentForProduct",
        title = "${uiLabelMap.ProductUpdateEmailContentProduct}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 3),
            @FormField(name = "purchaseFromDate", title = "${uiLabelMap.ProductPurchaseFromDate}"),
            @FormField(name = "purchaseThruDate", title = "${uiLabelMap.ProductPurchaseThruDate}", position = 3),
            @FormField(name = "useCountLimit", title = "${uiLabelMap.ProductUseCountLimit}"),
            @FormField(name = "useTime", title = "${uiLabelMap.ProductUseTime}"),
            @FormField(name = "useTimeUomId", title = "${uiLabelMap.ProductUseTimeUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", title = "${uiLabelMap.ProductUseRole}"),
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "subject", mapName = "emailData", title = "${uiLabelMap.ProductSubject}", text = @TextField(size = 40)),
            @FormField(name = "plainBody", mapName = "emailData", title = "${uiLabelMap.ProductContentPlainBody}", textarea = @TextareaField(rows = 7)),
            @FormField(name = "htmlBody", mapName = "emailData", title = "${uiLabelMap.ProductContentHtmlBody}", textarea = @TextareaField(rows = 7)),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductOptional}", useWhen = "contentId == null", text = @TextField(maxlength = 20)),
            @FormField(name = "contentId", mapName = "productContentData", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "subjectDataResourceId", mapName = "emailData", hidden = @HiddenField),
            @FormField(name = "plainBodyDataResourceId", mapName = "emailData", hidden = @HiddenField),
            @FormField(name = "htmlBodyDataResourceId", mapName = "emailData", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createEmailContentForProduct")
        }
    )
    public interface EditProductContentEmail {}

    @Form(
        name = "EditProductContentDownload",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.UPLOAD,
        target = "updateDownloadContentForProduct",
        title = "${uiLabelMap.ProductUpdateDownloadContentProduct}",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "purchaseFromDate", title = "${uiLabelMap.ProductPurchaseFromDate}"),
            @FormField(name = "purchaseThruDate", title = "${uiLabelMap.ProductPurchaseThruDate}"),
            @FormField(name = "useCountLimit", title = "${uiLabelMap.ProductUseCountLimit}"),
            @FormField(name = "useTime", title = "${uiLabelMap.ProductUseTime}"),
            @FormField(name = "useTimeUomId", title = "${uiLabelMap.ProductUseTimeUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", title = "${uiLabelMap.ProductUseRole}"),
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductOptional}", useWhen = "contentId == null", text = @TextField(maxlength = 20)),
            @FormField(name = "contentId", mapName = "productContentData", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/content/control/editContent", urlMode = UrlMode.INTER_APP, description = "${contentId} ${contentName}", parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "imageData", title = "${uiLabelMap.ProductFile}", file = @FileField),
            @FormField(name = "fileDataResourceId", mapName = "downloadData", hidden = @HiddenField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createDownloadContentForProduct")
        }
    )
    public interface EditProductContentDownload {}

    @Form(
        name = "EditProductContentExternal",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateExternalContentForProduct",
        title = "${uiLabelMap.ProductUpdateExternalContentProduct}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "serviceName", mapName = "content", title = "${uiLabelMap.ProductServiceName}", text = @TextField(size = 40)),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductOptional}", useWhen = "contentId == null", text = @TextField(maxlength = 20)),
            @FormField(name = "contentId", mapName = "productContentData", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createExternalContentForProduct")
        }
    )
    public interface EditProductContentExternal {}

    @Form(
        name = "EditProductContentSimpleText",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateSimpleTextContentForProduct",
        title = "${uiLabelMap.ProductUpdateSimpleTextContentProduct}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId==null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "localeString", mapName = "content", title = "${uiLabelMap.ProductLocaleString}", text = @TextField(size = 40)),
            @FormField(name = "contentId", useWhen = "contentId == null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "productContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "text", mapName = "textData", title = "${uiLabelMap.ProductText}*", widgetStyle = "dojo-ResizableTextArea", textarea = @TextareaField(cols = 80, rows = 20)),
            @FormField(name = "textDataResourceId", mapName = "textData", title = "${uiLabelMap.ProductTextDataResourceId}", hidden = @HiddenField),
            @FormField(name = "useTime", hidden = @HiddenField),
            @FormField(name = "useTimeUomId", hidden = @HiddenField),
            @FormField(name = "useRoleTypeId", hidden = @HiddenField),
            @FormField(name = "useCountLimit", hidden = @HiddenField),
            @FormField(name = "purchaseThruDate", hidden = @HiddenField),
            @FormField(name = "purchaseFromDate", hidden = @HiddenField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", displayEntity = @DisplayEntityField(entityName = "ProductContentType")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createSimpleTextContentForProduct")
        }
    )
    public interface EditProductContentSimpleText {}

    @Form(
        name = "EditProductContentImage",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.UPLOAD,
        target = "addAdditionalImageContentForProduct",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductContent", mapName = "productContentData")
        },
        fields = {
            @FormField(name = "productContentTypeId", displayEntity = @DisplayEntityField(entityName = "ProductContentType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId==null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "contentId", useWhen = "contentId == null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "productContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "useTime", hidden = @HiddenField),
            @FormField(name = "useTimeUomId", hidden = @HiddenField),
            @FormField(name = "useRoleTypeId", hidden = @HiddenField),
            @FormField(name = "useCountLimit", hidden = @HiddenField),
            @FormField(name = "purchaseThruDate", hidden = @HiddenField),
            @FormField(name = "purchaseFromDate", hidden = @HiddenField),
            @FormField(name = "uploadedFile", title = "${uiLabelMap.ProductFile}", file = @FileField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductContentImage {}

    @Form(
        name = "EditProductContentSEO",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateContentSEOForProduct",
        title = "${uiLabelMap.PageTitleEditProductContent}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "title", title = "${uiLabelMap.PageTitle}", text = @TextField(size = 40)),
            @FormField(name = "metaKeyword", title = "${uiLabelMap.MetaKeywords}", textarea = @TextareaField(rows = 5)),
            @FormField(name = "metaDescription", title = "${uiLabelMap.MetaDescription}", textarea = @TextareaField(rows = 5)),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductContentSEO {}

    @Form(
        name = "AddSupplierProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateSupplierProduct",
        defaultMapName = "supplierProduct",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSupplierProduct")
        },
        fields = {
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", useWhen = "supplierProduct!=null", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", useWhen = "supplierProduct==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} ${firstName} ${lastName} [${partyId}]", constraints = {@EntityConstraint(name = "roleTypeId", value = "SUPPLIER")}, orderBy = {@EntityOrderBy(fieldName = "groupName"), @EntityOrderBy(fieldName = "firstName")}))),
            @FormField(name = "supplierProductId", requiredField = true, text = @TextField),
            @FormField(name = "lastPrice", requiredField = true, text = @TextField),
            @FormField(name = "availableFromDate", useWhen = "supplierProduct==null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "availableFromDate", useWhen = "supplierProduct!=null", display = @DisplayField(type = "date")),
            @FormField(name = "minimumOrderQuantity", useWhen = "supplierProduct==null", text = @TextField(size = 5, defaultValue = "0")),
            @FormField(name = "minimumOrderQuantity", useWhen = "supplierProduct!=null", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", useWhen = "supplierProduct==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", useWhen = "supplierProduct!=null", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId")),
            @FormField(name = "supplierPrefOrderId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SupplierPrefOrder", description = "${description}", keyFieldName = "supplierPrefOrderId", orderBy = {@EntityOrderBy(fieldName = "supplierPrefOrderId")}))),
            @FormField(name = "supplierRatingTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SupplierRatingType", description = "${description}", keyFieldName = "supplierRatingTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.ProductQuantityUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "${typeDescription}: ${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "typeDescription"), @EntityOrderBy(fieldName = "uomId")}))),
            @FormField(name = "canDropShip", title = "${uiLabelMap.ProductSupplierCanDropShip}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "supplierProduct == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "supplierProduct != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "supplierProduct==null", target = "createSupplierProduct")
        }
    )
    public interface AddSupplierProduct {}

    @Form(
        name = "ListSupplierProducts",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateSupplierProduct",
        listName = "productSuppliers",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "supplierProductId", display = @DisplayField),
            @FormField(name = "minimumOrderQuantity", title = "${uiLabelMap.ProductMinimumOrderQuantity}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", sortField = true, display = @DisplayField),
            @FormField(name = "orderQtyIncrements", title = "${uiLabelMap.ProductOrderQtyIncrements}", sortField = true, display = @DisplayField),
            @FormField(name = "supplierPrefOrderId", displayEntity = @DisplayEntityField(entityName = "SupplierPrefOrder")),
            @FormField(name = "availableFromDate", title = "${uiLabelMap.ProductAvailableFromDate}", redWhen = "after-now", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "availableThruDate", title = "${uiLabelMap.ProductAvailableThruDate}", redWhen = "before-now", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "quantityUomId", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId")),
            @FormField(name = "lastPrice", titleAreaStyle = "align-right", widgetAreaStyle = "amount", sortField = true, display = @DisplayField(type = "currency")),
            @FormField(name = "shippingPrice", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductSuppliers", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "minimumOrderQuantity"), @ParameterDef(paramName = "availableFromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeSupplierProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "minimumOrderQuantity"), @ParameterDef(paramName = "availableFromDate")}))
        }
    )
    public interface ListSupplierProducts {}

    @Form(
        name = "AddProductConfig",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductConfig",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonItem}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductConfigItem", description = "${configItemName} [${configItemId}] (${description})", orderBy = {@EntityOrderBy(fieldName = "configItemName")}))),
            @FormField(name = "configTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(options = {@Option(key = "STANDARD", description = "${uiLabelMap.ProductStandard}"), @Option(key = "QUESTION", description = "${uiLabelMap.ProductQuestion}")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "isMandatory", title = "${uiLabelMap.CommonMandatory}", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "defaultConfigOptionId", text = @TextField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSeqNum}", position = 2, text = @TextField),
            @FormField(name = "longDescription", title = "${uiLabelMap.CommonLongDescription}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductConfig {}

    @Form(
        name = "UpdateProductConfig",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductConfig",
        listName = "productConfigs",
        paginateTarget = "ViewProductManufacturing",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "configItemId", title = "${uiLabelMap.CommonItem}", displayEntity = @DisplayEntityField(entityName = "ProductConfigItem", keyFieldName = "configItemId", description = "${configItemName} : (${description})", subHyperlink = @SubHyperlink(target = "EditProductConfigItem", description = "[ ${configItemId} ]", parameters = {@ParameterDef(paramName = "configItemId")}))),
            @FormField(name = "configTypeId", title = "${uiLabelMap.ProductType}", dropDown = @DropDownField(options = {@Option(key = "STANDARD", description = "${uiLabelMap.ProductStandard}"), @Option(key = "QUESTION", description = "${uiLabelMap.ProductQuestion}")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "isMandatory", title = "${uiLabelMap.CommonMandatory}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "defaultConfigOptionId", text = @TextField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSeqNum}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "longDescription", title = "${uiLabelMap.CommonLongDescription}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductConfig", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "configItemId"), @ParameterDef(paramName = "sequenceNum"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductConfig {}

    @Form(
        name = "EditProductAssetUsage",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateProductAssetUsage",
        defaultMapName = "product",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "reservMaxPersons", title = "${uiLabelMap.ProductReservMaxPersons}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "reserv2ndPPPerc", title = "${uiLabelMap.ProductReserv2ndPPPerc}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "reservNthPPPerc", title = "${uiLabelMap.ProductReservNthPPPerc}", position = 2, text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductUpdateProduct}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "product==null", target = "createProduct")
        }
    )
    public interface EditProductAssetUsage {}

    @Form(
        name = "ListProductFixedAssets",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetProduct",
        listName = "fixedAssetProducts",
        paginateTarget = "ViewProductManufacturing",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAsset} [${uiLabelMap.AccountingFixedAssetId}]", widgetStyle = "${styles.link_nav_info_desc}", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName} [${fixedAssetId}]")),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.AccountingFixedAssetProductType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetProductType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "showFixedAssetProduct", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fixedAssetProductTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFixedAssetProduct", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fixedAssetProductTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductFixedAssets {}

    @Form(
        name = "addFixedAssetProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "addFixedAssetProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAssetId}", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.AccountingFixedAssetProductType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAssetProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}", text = @TextField(size = 30, maxlength = 30)),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequence}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.CommonUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface addFixedAssetProduct {}

    @Form(
        name = "showFixedAssetProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updFixedAssetProduct",
        defaultMapName = "fixedAssetProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAssetId}", useWhen = "fixedAssetId!=null", displayEntity = @DisplayEntityField(entityName = "FixedAsset", description = "${fixedAssetName}[${fixedAssetId}]")),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAssetId}", useWhen = "fixedAssetId==null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.AccountingFixedAssetProductType}", useWhen = "fixedAssetId!=null", displayEntity = @DisplayEntityField(entityName = "FixedAssetProductType")),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.AccountingFixedAssetProductType}", useWhen = "fixedAssetId==null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAssetProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "fixedAssetId!=null", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "fixedAssetId==null", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}", text = @TextField(size = 30, maxlength = 30)),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequence}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.CommonUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "fixedAssetId==null", target = "addFixedAssetProduct")
        }
    )
    public interface showFixedAssetProduct {}

    @Form(
        name = "AddProductAssoc",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "UpdateProductAssoc",
        fields = {
            @FormField(name = "UPDATE_MODE", hidden = @HiddenField(value = "CREATE")),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "productIdTo", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productAssocTypeId", title = "${uiLabelMap.CommonType}", position = 2, requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSeqNum}", text = @TextField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", position = 2, text = @TextField),
            @FormField(name = "instruction", textarea = @TextareaField),
            @FormField(name = "reason", position = 2, textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductAssoc {}

    @Form(
        name = "EditProductAssoc",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "UpdateProductAssoc",
        fields = {
            @FormField(name = "UPDATE_MODE", hidden = @HiddenField(value = "UPDATE")),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "productIdTo", position = 2, displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${description} [${productIdTo}]")),
            @FormField(name = "productAssocTypeId", title = "${uiLabelMap.CommonType}", position = 2, displayEntity = @DisplayEntityField(entityName = "ProductAssocType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSeqNum}", text = @TextField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", position = 2, text = @TextField),
            @FormField(name = "instruction", textarea = @TextareaField),
            @FormField(name = "reason", position = 2, textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductAssoc {}

    @Form(
        name = "ListProductAssocs",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "assocFromProducts",
        paginateTarget = "EditProductAssoc",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productIdTo", title = "${uiLabelMap.CommonProduct}", widgetStyle = "${styles.link_nav_info_name}", hyperlink = @HyperlinkField(target = "ViewProduct", description = "${productAssoc.internalName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId", fromField = "productIdTo")})),
            @FormField(name = "productAssocTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductAssocType")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSeqNum}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQty}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditTaxAuthority", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productIdTo"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItem", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productIdTo"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "productAssoc")})
    )
    public interface ListProductAssocs {}

    @Form(
        name = "ListProductAssocsTo",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "assocToProduct",
        paginateTarget = "EditProductAssoc",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productIdTo", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewProduct", description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")}))
        }
    )
    public interface ListProductAssocsTo {}

    @Form(
        name = "ListProductComponents",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "components",
        paginateTarget = "ViewProductManufacturing",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductAssoc", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productAssocTypeId", hidden = @HiddenField),
            @FormField(name = "reason", hidden = @HiddenField),
            @FormField(name = "instruction", hidden = @HiddenField),
            @FormField(name = "routingWorkEffortId", hidden = @HiddenField),
            @FormField(name = "productIdTo", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewProductManufacturing", description = "${productIdTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId", fromField = "productIdTo")})),
            @FormField(name = "productName", entryName = "productIdTo", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}"))
        }
    )
    public interface ListProductComponents {}

    @Form(
        name = "ListProductParents",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "parents",
        paginateTarget = "ViewProductManufacturing",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductAssoc", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productIdTo", hidden = @HiddenField),
            @FormField(name = "productAssocTypeId", hidden = @HiddenField),
            @FormField(name = "sequenceNum", hidden = @HiddenField),
            @FormField(name = "reason", hidden = @HiddenField),
            @FormField(name = "instruction", hidden = @HiddenField),
            @FormField(name = "routingWorkEffortId", hidden = @HiddenField),
            @FormField(name = "productAssocTypeId", hidden = @HiddenField),
            @FormField(name = "scrapFactor", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewProductManufacturing", description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productName", entryName = "productId", title = "${uiLabelMap.ProductProductName}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}"))
        }
    )
    public interface ListProductParents {}

    @Form(
        name = "ListProductRoutings",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "routings",
        paginateTarget = "ViewProductManufacturing",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WorkEffortGoodStandard", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "workEffortGoodStdTypeId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "workEffortName", entryName = "workEffortId", title = "${uiLabelMap.ProductWorkEffortName}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/manufacturing/control/EditRoutingProductAction", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")}))
        }
    )
    public interface ListProductRoutings {}

    @Form(
        name = "FilterCostComponents",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "EditProductCosts",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "costComponentTypePrefix", dropDown = @DropDownField(options = {@Option(key = "EST_STD", description = "${uiLabelMap.ProductEstimatedCosts}")})),
            @FormField(name = "costUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} [${uomId}]", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FilterCostComponents {}

    @Form(
        name = "ListCostComponents",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "EditProductCosts",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CostComponent", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "cost", display = @DisplayField),
            @FormField(name = "costComponentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductCosts", description = "${costComponentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productCostComponentId", fromField = "costComponentId")})),
            @FormField(name = "productId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "costComponentTypeId", displayEntity = @DisplayEntityField(entityName = "CostComponentType")),
            @FormField(name = "deleteAction", entryName = "costComponentId", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCostComponent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "costComponentId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "InParam.productId", fromField = "requestParameters.productId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "InParam"), @FieldMap(fieldName = "entityName", value = "CostComponent"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "orderBy", value = "-fromDate")})})
    )
    public interface ListCostComponents {}

    @Form(
        name = "ListProductCostComponentCalcs",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "productCostComponentCalcs",
        paginateTarget = "EditProductCosts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "costComponentTypeId", displayEntity = @DisplayEntityField(entityName = "CostComponentType")),
            @FormField(name = "costComponentCalcId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/manufacturing/control/EditCostCalcs", urlMode = UrlMode.INTER_APP, description = "${costComponentCalcId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "costComponentCalcId")})),
            @FormField(name = "costComponentCalc", entryName = "costComponentCalcId", displayEntity = @DisplayEntityField(entityName = "CostComponentCalc", keyFieldName = "costComponentCalcId")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "deleteAction", entryName = "productId", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductCostComponentCalc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "costComponentTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductCostComponentCalcs {}

    @Form(
        name = "AddProductCostComponentCalc",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductCostComponentCalc",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductCostComponentCalc")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${product.productId}")),
            @FormField(name = "costComponentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CostComponentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", envName = "nullField")}))),
            @FormField(name = "costComponentCalcId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CostComponentCalc", description = "${description}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductCostComponentCalc {}

    @Form(
        name = "EditCostComponent",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createCostComponent",
        defaultMapName = "costComponent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCostComponent")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "costComponentId", useWhen = "costComponent!=null", display = @DisplayField),
            @FormField(name = "costComponentId", useWhen = "costComponent==null", hidden = @HiddenField),
            @FormField(name = "productFeatureId", lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "geoId", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "fixedAssetId", lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "costComponentCalcId", lookup = @LookupField(targetFormName = "LookupCostComponentCalc")),
            @FormField(name = "costComponentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CostComponentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "costUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "costComponent!=null", target = "updateCostComponent")
        }
    )
    public interface EditCostComponent {}

    @Form(
        name = "CalculateProductCosts",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "calculateProductCosts",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "calculateProductCosts")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "costComponentTypePrefix", dropDown = @DropDownField(options = {@Option(key = "EST_STD", description = "${uiLabelMap.ProductEstimatedCosts}")})),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} [${uomId}]", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface CalculateProductCosts {}

    @Form(
        name = "OutstandingPurchaseOrders",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "purchaseOrders",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderDate", display = @DisplayField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderItemSeqId", display = @DisplayField),
            @FormField(name = "quantity", display = @DisplayField),
            @FormField(name = "cancelQuantity", display = @DisplayField),
            @FormField(name = "itemStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "estimatedShipDate", display = @DisplayField),
            @FormField(name = "estimatedDeliveryDate", display = @DisplayField),
            @FormField(name = "shipBeforeDate", display = @DisplayField),
            @FormField(name = "shipAfterDate", display = @DisplayField)
        }
    )
    public interface OutstandingPurchaseOrders {}

    @Form(
        name = "AddProductMaint",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductMaint",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productMaintSeqId", ignored = @IgnoredField),
            @FormField(name = "maintName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "productMaintTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductMaintType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "maintTemplateWorkEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "intervalQuantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField),
            @FormField(name = "intervalUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "intervalMeterTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMeterType", description = "${description}", keyFieldName = "productMeterTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "repeatCount", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductMaint {}

    @Form(
        name = "ListProductMaints",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductMaint",
        listName = "productMaints",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductMaint")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productMaintSeqId", hidden = @HiddenField),
            @FormField(name = "productMaintTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMaintType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "maintName", title = "${uiLabelMap.CommonName}", text = @TextField(size = 20)),
            @FormField(name = "maintTemplateWorkEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "intervalUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "intervalMeterTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMeterType", description = "${description}", keyFieldName = "productMeterTypeId", orderBy = {@EntityOrderBy(fieldName = "productMeterTypeId")}))),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductMaint", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productMaintSeqId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductMaints {}

    @Form(
        name = "AddProductMeter",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductMeter",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductMeter")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productMeterTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductMeterType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "meterUomId", title = "${uiLabelMap.CommonUom}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "meterName", title = "${uiLabelMap.CommonName}", text = @TextField(size = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductMeter {}

    @Form(
        name = "ListProductMeters",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductMeter",
        listName = "productMeters",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductMeter")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productMeterTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductMeterType")),
            @FormField(name = "meterUomId", title = "${uiLabelMap.CommoUom}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "meterName", title = "${uiLabelMap.CommonName}", text = @TextField(size = 20)),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductMeter", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productMeterTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductMeters {}

    @Form(
        name = "AddProductGeo",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductGeo",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductGeo")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeo}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", orderBy = {@EntityOrderBy(fieldName = "geoTypeId"), @EntityOrderBy(fieldName = "geoName")}))),
            @FormField(name = "productGeoEnumId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_GEO")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductGeo {}

    @Form(
        name = "ListProductGeos",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductGeo",
        listName = "productGeos",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductGeo")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", description = "${geoName}")),
            @FormField(name = "productGeoEnumId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_GEO")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductGeo", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "geoId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductGeos {}

    @Form(
        name = "ListFeatureInteractions",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "featureInteractions",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", displayEntity = @DisplayEntityField(entityName = "ProductFeature", description = "${description}", subHyperlink = @SubHyperlink(target = "EditFeature", description = "[${productFeatureId}]", parameters = {@ParameterDef(paramName = "productFeatureId")}))),
            @FormField(name = "productFeatureIdTo", displayEntity = @DisplayEntityField(entityName = "ProductFeature", keyFieldName = "productFeatureId", description = "${description}", subHyperlink = @SubHyperlink(target = "EditFeature", description = "[${productFeatureIdTo}]", parameters = {@ParameterDef(paramName = "productFeatureId", fromField = "productFeatureIdTo")}))),
            @FormField(name = "productFeatureIactnTypeId", displayEntity = @DisplayEntityField(entityName = "ProductFeatureIactnType")),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFeatureIactn", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "productFeatureIdTo"), @ParameterDef(paramName = "productId")}))
        }
    )
    public interface ListFeatureInteractions {}

    @Form(
        name = "AddFeatureInteraction",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "AddProductFeatureIactn",
        defaultMapName = "productFeatureIactn",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${parameters.productId}")),
            @FormField(name = "productFeatureId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureAndAppl", description = "${description} [${productFeatureId}]", constraints = {@EntityConstraint(name = "productId", envName = "parameters.productId")}, orderBy = {@EntityOrderBy(fieldName = "productFeatureTypeId"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureIdTo", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureAndAppl", description = "${description} [${productFeatureId}]", keyFieldName = "productFeatureId", constraints = {@EntityConstraint(name = "productId", envName = "parameters.productId")}, orderBy = {@EntityOrderBy(fieldName = "productFeatureTypeId"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureIactnTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductFeatureIactnType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFeatureInteraction {}

    @Form(
        name = "AddProductFeatureApplAttr",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductFeatureApplAttr",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductFeatureApplAttr")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductFeatureAndAppl", description = "${description} [${productFeatureId}]", keyFieldName = "productFeatureId", constraints = {@EntityConstraint(name = "productId", envName = "productId")}, orderBy = {@EntityOrderBy(fieldName = "sequenceNum"), @EntityOrderBy(fieldName = "defaultSequenceNum"), @EntityOrderBy(fieldName = "fromDate")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductFeatureApplAttr {}

    @Form(
        name = "ListProductFeatureApplAttrs",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "EditProductFeatureAppl",
        listName = "productFeatureApplAttrs",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeatureApplAttr", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", displayEntity = @DisplayEntityField(entityName = "ProductFeature", description = "${description} [${productFeatureId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductFeatureApplAttr", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "attrName"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductFeatureApplAttrs {}

    @Form(
        name = "ListProductSubscriptionResources",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductSubscriptionResource",
        listName = "productSubscriptionResources",
        paginateTarget = "EditProductSubscriptionResources",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductSubscriptionResource")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "subscriptionResourceId", displayEntity = @DisplayEntityField(entityName = "SubscriptionResource", description = "${description}", subHyperlink = @SubHyperlink(target = "EditSubscriptionResource", description = "${subscriptionResourceId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "subscriptionResourceId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "useTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "canclAutmExtTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductSubscriptionResource", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "subscriptionResourceId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductSubscriptionResources {}

    @Form(
        name = "AddProductSubscriptionResource",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductSubscriptionResource",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductSubscriptionResource")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "subscriptionResourceId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SubscriptionResource", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "useRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "canclAutmExtTimeUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductSubscriptionResource {}

    @Form(
        name = "ListSupplierProductAgreements",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "supplierProductAgreements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/accounting/control/EditAgreement", urlMode = UrlMode.INTER_APP, description = "${agreementId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSupplier}", display = @DisplayField),
            @FormField(name = "agreementText", display = @DisplayField(description = "${agreement.agreementText}")),
            @FormField(name = "description", display = @DisplayField(description = "${agreement.description}")),
            @FormField(name = "availableFromDate", display = @DisplayField),
            @FormField(name = "availableThruDate", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField),
            @FormField(name = "lastPrice", display = @DisplayField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Agreement", valueField = "agreement")})
    )
    public interface ListSupplierProductAgreements {}

    @Form(
        name = "ListSalesAgreements",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "salesAgreements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "/accounting/control/EditAgreementItemProduct", urlMode = UrlMode.INTER_APP, description = "${agreementId}/${agreementItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "productId")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.ProductPartyCustomer}", display = @DisplayField),
            @FormField(name = "agreementText", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField),
            @FormField(name = "price", display = @DisplayField)
        }
    )
    public interface ListSalesAgreements {}

    @Form(
        name = "ListProductAgreements",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "productAgreements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "/accounting/control/EditAgreementItemProduct", urlMode = UrlMode.INTER_APP, description = "${agreementId}/${agreementItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "productId")})),
            @FormField(name = "partyIdTo", display = @DisplayField),
            @FormField(name = "agreementText", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField),
            @FormField(name = "price", display = @DisplayField)
        }
    )
    public interface ListProductAgreements {}

    @Form(
        name = "ListCommissionAgreements",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "commissionAgreements",
        oddRowStyle = "alternate-row",
        defaultTableStyle = "basic-table",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "/accounting/control/EditAgreementItemProduct", urlMode = UrlMode.INTER_APP, description = "${agreementId}/${agreementItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "productId")})),
            @FormField(name = "partyIdTo", display = @DisplayField),
            @FormField(name = "agreementText", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "fromDate", display = @DisplayField),
            @FormField(name = "thruDate", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField),
            @FormField(name = "price", display = @DisplayField)
        }
    )
    public interface ListCommissionAgreements {}

    @Form(
        name = "AddProductWorkEffort",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createWorkEffortGoodStandard",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField(value = "${parameters.productId}")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "workEffortGoodStdTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortGoodStandardType", description = "${description}", keyFieldName = "workEffortGoodStdTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFG_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "estimatedQuantity", text = @TextField),
            @FormField(name = "estimatedCost", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductWorkEffort {}

    @Form(
        name = "ListProductWorkEfforts",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortGoodStandard",
        listName = "productWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "/workeffort/control/EditWorkEffort", description = "${workEffortId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId")}))),
            @FormField(name = "workEffortGoodStdTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "WorkEffortGoodStandardType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "WEFG_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "estimatedQuantity", text = @TextField),
            @FormField(name = "estimatedCost", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeWorkEffortGoodStandard", description = "[${uiLabelMap.CommonDelete}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "workEffortGoodStdTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListProductWorkEfforts {}

    @Form(
        name = "UpdateProductFacilities",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductFacility",
        listName = "productFacilities",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductFacility")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "facilityId", title = "${uiLabelMap.Facility}", displayEntity = @DisplayEntityField(entityName = "Facility", description = "${facilityName} [${facilityId}]")),
            @FormField(name = "lastInventoryCount", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductFacility", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "facilityId")}))
        }
    )
    public interface UpdateProductFacilities {}

    @Form(
        name = "AddProductFacility",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductFacility",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "facilityId", title = "${uiLabelMap.Facility}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "minimumStock", position = 2, text = @TextField),
            @FormField(name = "reorderQuantity", text = @TextField),
            @FormField(name = "daysToShip", position = 2, text = @TextField),
            @FormField(name = "lastInventoryCount", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductFacility {}

    @Form(
        name = "UpdateProductFacilityLocations",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductFacilityLocation",
        listName = "productFacilityLocations",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "facilityName", entryName = "facilityId", title = "${uiLabelMap.Facility}", useWhen = "showPosition1", displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName} [${facilityId}]", alsoHidden = false)),
            @FormField(name = "locationSeqId", title = "${uiLabelMap.CommonLocation}", display = @DisplayField(description = "${facilityLocation.areaId} ${facilityLocation.aisleId} ${facilityLocation.sectionId} ${facilityLocation.levelId} ${facilityLocation.positionId} [${locationSeqId}] (${locationType.description})")),
            @FormField(name = "minimumStock", text = @TextField),
            @FormField(name = "moveQuantity", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductFacilityLocation", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "facilityId"), @ParameterDef(paramName = "locationSeqId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "showPosition1", value = "${groovy:String prev=(String)previousItem.get(\"facilityId\");return new Boolean(!(prev!=null&&prev.equals(facilityId)));}", type = "Boolean")}, entityOne = {@EntityOneAction(entityName = "FacilityLocation", valueField = "facilityLocation"), @EntityOneAction(entityName = "Enumeration", valueField = "locationType")})
    )
    public interface UpdateProductFacilityLocations {}

    @Form(
        name = "AddProductFacilityLocation",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductFacilityLocation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductFacilityLocation")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "facilityId", title = "${uiLabelMap.Facility}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "locationSeqId", title = "${uiLabelMap.CommonLocation}", position = 2, lookup = @LookupField(targetFormName = "LookupFacilityLocation")),
            @FormField(name = "minimumStock", text = @TextField),
            @FormField(name = "moveQuantity", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductFacilityLocation {}

    @Form(
        name = "AddProductGoodIdentification",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createGoodIdentification",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createGoodIdentification")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "goodIdentificationTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GoodIdentificationType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "idValue", title = "${uiLabelMap.CommonValue}", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductGoodIdentification {}

    @Form(
        name = "UpdateProductGoodIdentifications",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateGoodIdentification",
        listName = "goodIdentifications",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "goodIdentificationTypeId", title = "${uiLabelMap.ProductIdType}", displayEntity = @DisplayEntityField(entityName = "GoodIdentificationType")),
            @FormField(name = "idValue", title = "${uiLabelMap.CommonValue}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteGoodIdentification", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "goodIdentificationTypeId")}))
        }
    )
    public interface UpdateProductGoodIdentifications {}

    @Form(
        name = "AddProductKeyword",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductKeyword",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "keyword", text = @TextField),
            @FormField(name = "keywordTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "KEYWORD_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "relevancyWeight", text = @TextField(defaultValue = "1")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "KEYWORD_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductKeyword {}

    @Form(
        name = "UpdateProductKeyword",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductKeyword",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "keyword", display = @DisplayField),
            @FormField(name = "keywordTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "relevancyWeight", text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "KEYWORD_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductKeyword", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "keyword"), @ParameterDef(paramName = "keywordTypeId")}))
        }
    )
    public interface UpdateProductKeyword {}

    @Form(
        name = "AddProductAttribute",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductAttribute",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "attrName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "attrType", title = "${uiLabelMap.CommonType}", position = 2, text = @TextField),
            @FormField(name = "attrValue", title = "${uiLabelMap.CommonValue}", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductAttribute {}

    @Form(
        name = "UpdateProductAttribute",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductAttribute",
        listName = "productAttributes",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "attrName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "attrType", title = "${uiLabelMap.CommonType}", text = @TextField),
            @FormField(name = "attrValue", title = "${uiLabelMap.CommonValue}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductAttribute", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "attrName")}))
        }
    )
    public interface UpdateProductAttribute {}

    @Form(
        name = "ListProductGlAccounts",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updateProductGlAccount",
        listName = "productGlAccounts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType", keyFieldName = "glAccountTypeId", description = "${description}")),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonOrganisation}", display = @DisplayField(description = "${groovy: org.ofbiz.party.party.PartyHelper.getPartyName(delegator, organizationPartyId, true);} [${organizationPartyId}]")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "organizationGlAccounts", keyName = "accountCode", description = "${accountCode} ${accountName}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductGlAccount", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "glAccountTypeId")}))
        }
    )
    public interface ListProductGlAccounts {}

    @Form(
        name = "AddProductGlAccount",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductGlAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "productGlAccountTypes", keyName = "glAccountTypeId", description = "${description}"))),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.CommonOrganisation}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "glAccounts", keyName = "accountCode", description = "${accountCode} ${accountName}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductGlAccount {}

    @Form(
        name = "ListVendorProducts",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "deleteVendorProduct",
        listName = "vendorProductList",
        paginateTarget = "EditVendorProduct",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "vendorPartyId", title = "${uiLabelMap.CommonParty}", display = @DisplayField),
            @FormField(name = "productStoreGroupId", title = "${uiLabelMap.CommonStore} ${uiLabelMap.CommonGroup}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListVendorProducts {}

    @Form(
        name = "EditVendorProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createVendorProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "vendorPartyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "productStoreGroupId", title = "${uiLabelMap.CommonStore} ${uiLabelMap.CommonGroup}", position = 2, dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "ProductStoreGroup", description = "${description}", keyFieldName = "productStoreGroupId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditVendorProduct {}

    @Form(
        name = "ListBestProduct",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "bestSellingProducts",
        paginate = "false",
        headerRowStyle = "header-row",
        oddRowStyle = "alternate-row",
        viewSize = 5,
        fields = {
            @FormField(name = "productName", title = "${uiLabelMap.ProductName}", display = @DisplayField(description = "${productName}")),
            @FormField(name = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "qtyOrdered", title = "${uiLabelMap.OrderQtyOrdered}", display = @DisplayField)
        }
    )
    public interface ListBestProduct {}

    @Form(
        name = "ListCommEvents",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "communicationEvents",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/EditCommunicationEvent?communicationEventId=${communicationEventId}", urlMode = UrlMode.INTER_APP, description = "${communicationEventId}")),
            @FormField(name = "subject", display = @DisplayField),
            @FormField(name = "communicationEventTypeId", displayEntity = @DisplayEntityField(entityName = "CommunicationEventType", keyFieldName = "communicationEventTypeId", description = "${description}")),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "lastUpdatedStampCE", title = "Email Sent On", display = @DisplayField),
            @FormField(name = "contactMechIdTo", title = "To Email Address", displayEntity = @DisplayEntityField(entityName = "ContactMech", keyFieldName = "contactMechId", description = "${infoString}")),
            @FormField(name = "content", display = @DisplayField),
            @FormField(name = "subject", mapName = "subjectMap", display = @DisplayField)
        }
    )
    public interface ListCommEvents {}

    @Form(
        name = "EditCommEvent",
        location = "component://product/widget/catalog/ProductForms.xml",
        extendsForm = "EditCommEvent",
        extendsResource = "component://party/widget/partymgr/CommunicationEventForms.xml",
        fields = {
            @FormField(name = "productId", mapName = "parameters", hidden = @HiddenField)
        }
    )
    public interface EditCommEvent {}

    @Form(
        name = "UpdateProductRole",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        target = "updatePartyToProduct",
        listName = "productRoles",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyToProduct")
        },
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "sequenceNum", text = @TextField(size = 5)),
            @FormField(name = "comments", text = @TextField(size = 30)),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${personalTitle} ${firstName} ${middleName} ${lastName} ${suffix} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "party_id", fromField = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", redWhen = "after-now", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", redWhen = "before-now", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePartyFromProduct", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductRole {}

    @Form(
        name = "AddProductRole",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "addPartyToProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "comments", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductRole {}

    @Form(
        name = "ListProductGroupOrder",
        location = "component://product/widget/catalog/ProductForms.xml",
        type = FormType.LIST,
        listName = "productGroupOrders",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "groupOrderId", title = "${uiLabelMap.CommonId}", display = @DisplayField),
            @FormField(name = "reqOrderQty", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "soldOrderQty", title = "${uiLabelMap.CommonSold}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "editAction", title = " ", useWhen = "${groovy: return reqOrderQty.compareTo(soldOrderQty)!= 0;}&&${groovy: return thruDate.compareTo(org.ofbiz.base.util.UtilDateTime.nowTimestamp()) == 1}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductGroupOrder", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "groupOrderId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductGroupOrder", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "groupOrderId")}))
        }
    )
    public interface ListProductGroupOrder {}

    @Form(
        name = "CreateProductGroupOrder",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "createProductGroupOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField(value = "GO_CREATED")),
            @FormField(name = "soldOrderQty", hidden = @HiddenField(value = "0")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "reqOrderQty", title = "${uiLabelMap.CommonQuantity}", position = 2, requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductGroupOrder {}

    @Form(
        name = "EditProductGroupOrder",
        location = "component://product/widget/catalog/ProductForms.xml",
        target = "updateProductGroupOrder",
        defaultMapName = "productGroupOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "groupOrderId", hidden = @HiddenField),
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "reqOrderQty", title = "${uiLabelMap.CommonQuantity}", position = 2, requiredField = true, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProductGroupOrder {}

}
