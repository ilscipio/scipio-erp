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
public class CatalogFeatureForms {

    @Form(
        name = "EditProductFeature",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "updateProductFeature",
        defaultMapName = "productFeature",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductFeature")
        },
        fields = {
            @FormField(name = "isCreate", useWhen = "productFeature==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.ProductFeatureId}", tooltip = "${uiLabelMap.ProductChangeWithoutProductCatalog}", useWhen = "productFeature!=null", display = @DisplayField),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.ProductFeatureId}", tooltip = "${uiLabelMap.ProductCouldNotFindProductConfigItem} [${productFeatureId}]", useWhen = "productFeature==null&&productFeatureId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.ProductFeatureId}", useWhen = "productFeature==null&&productFeatureId==null", ignored = @IgnoredField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.ProductFeatureType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductFeatureType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.ProductFeatureCategory}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductFeatureCategory", description = "${description} [${productFeatureCategoryId}]", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true),
            @FormField(name = "uomId", title = "${uiLabelMap.ProductUnitOfMeasureId}"),
            @FormField(name = "numberSpecified", title = "${uiLabelMap.ProductNumberQuantity}"),
            @FormField(name = "defaultAmount", title = "${uiLabelMap.ProductDefaultAmount}"),
            @FormField(name = "defaultSequenceNum", title = "${uiLabelMap.ProductDefaultSequenceNumber}"),
            @FormField(name = "idCode", title = "${uiLabelMap.ProductIdCode}"),
            @FormField(name = "abbrev", title = "${uiLabelMap.ProductAbbreviation}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "productFeature!=null&&productFeatureId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "productFeature==null&&productFeatureId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productFeature==null", target = "createProductFeature")
        }
    )
    public interface EditProductFeature {}

    @Form(
        name = "CreateSupplierProductFeature",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "createSupplierProductFeature",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSupplierProductFeature", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSuppliers}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} ${firstName} ${lastName} [${partyId}]", constraints = {@EntityConstraint(name = "roleTypeId", value = "SUPPLIER")}, orderBy = {@EntityOrderBy(fieldName = "groupName"), @EntityOrderBy(fieldName = "lastName")}))),
            @FormField(name = "uomId", title = "${uiLabelMap.ProductCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId}"))),
            @FormField(name = "idCode", title = "${uiLabelMap.ProductIdCode}"),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "submitForm", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateSupplierProductFeature {}

    @Form(
        name = "EditSupplierProductFeatures",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        target = "updateSupplierProductFeature",
        listName = "supplierProductFeatures",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSupplierProductFeature", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.ProductSuppliers}", displayEntity = @DisplayEntityField(entityName = "PartyGroup", description = "${groupName}")),
            @FormField(name = "description", title = "${uiLabelMap.ProductProductDescription}", text = @TextField(size = 40)),
            @FormField(name = "idCode", title = "${uiLabelMap.ProductIdCode}", text = @TextField(size = 5)),
            @FormField(name = "uomId", title = "${uiLabelMap.ProductCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeSupplierProductFeature", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "partyId")}))
        }
    )
    public interface EditSupplierProductFeatures {}

    @Form(
        name = "FindFeatureType",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "EditFeatureTypes",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureTypeId", textFind = @TextFindField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFeatureType {}

    @Form(
        name = "ListFeatureTypes",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "EditFeatureTypes",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productFeatureTypeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFeatureType", description = "${productFeatureTypeId}", parameters = {@ParameterDef(paramName = "productFeatureTypeId")})),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "parentTypeId", displayEntity = @DisplayEntityField(entityName = "ProductFeatureType", keyFieldName = "productFeatureTypeId", description = "${description}", subHyperlink = @SubHyperlink(target = "EditFeatureType", description = "${parentTypeId}", parameters = {@ParameterDef(paramName = "productFeatureTypeId", fromField = "parentTypeId")}))),
            @FormField(name = "removeFeatureTypeAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductFeatureType", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "productFeatureTypeId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductFeatureType"), @FieldMap(fieldName = "orderBy", value = "productFeatureTypeId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFeatureTypes {}

    @Form(
        name = "EditFeatureType",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "updateProductFeatureType",
        defaultMapName = "productFeatureType",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeatureType")
        },
        fields = {
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.ProductFeatureType}", useWhen = "productFeatureType!=null", display = @DisplayField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.ProductFeatureType}", tooltip = "${uiLabelMap.ProductCouldNotFindProductFeatureType} [${productFeatureTypeId}]", useWhen = "productFeatureType==null&&productFeatureTypeId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "isCreate", useWhen = "productFeatureType==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "parentTypeId", title = "${uiLabelMap.ProductParentType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureType", description = "${description}", keyFieldName = "productFeatureTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "hasTable", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productFeatureType==null", target = "createProductFeatureType")
        }
    )
    public interface EditFeatureType {}

    @Form(
        name = "FindFeatureInterAction",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "EditFeatureInterActions",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureId", textFind = @TextFindField),
            @FormField(name = "productFeatureIdTo", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFeatureInterAction {}

    @Form(
        name = "ListFeatureInterActions",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "EditFeatureInterAction",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productFeatureId", displayEntity = @DisplayEntityField(entityName = "ProductFeature", description = "${description}", subHyperlink = @SubHyperlink(target = "EditFeature", description = "[${productFeatureId}]", parameters = {@ParameterDef(paramName = "productFeatureId")}))),
            @FormField(name = "productFeatureIdTo", displayEntity = @DisplayEntityField(entityName = "ProductFeature", keyFieldName = "productFeatureId", description = "${description}", subHyperlink = @SubHyperlink(target = "EditFeature", description = "[${productFeatureIdTo}]", parameters = {@ParameterDef(paramName = "productFeatureId", fromField = "productFeatureIdTo")}))),
            @FormField(name = "productFeatureIactnTypeId", displayEntity = @DisplayEntityField(entityName = "ProductFeatureIactnType")),
            @FormField(name = "removeFeatureInterAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductFeatureIactn", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "productFeatureIdTo")}))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductFeatureIactn"), @FieldMap(fieldName = "orderBy", value = "productFeatureId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFeatureInterActions {}

    @Form(
        name = "EditFeatureInterAction",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "createProductFeatureIactn",
        defaultMapName = "productFeatureIactn",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "isCreate", hidden = @HiddenField(value = "true")),
            @FormField(name = "productFeatureId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeature", description = "${description} [${productFeatureId}]", orderBy = {@EntityOrderBy(fieldName = "productFeatureTypeId"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureIdTo", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeature", description = "${description} [${productFeatureId}]", keyFieldName = "productFeatureId", orderBy = {@EntityOrderBy(fieldName = "productFeatureTypeId"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureIactnTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductFeatureIactnType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditFeatureInterAction {}

    @Form(
        name = "FindFeatureGroup",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "EditFeatureGroups",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureGroupId", textFind = @TextFindField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFeatureGroup {}

    @Form(
        name = "CreateFeatureGroup",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "UpdateProductFeatureGroup",
        defaultMapName = "productFeatureGroup",
        defaultEntityName = "ProductFeatureGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureGroupId", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "productFeatureGroup!=null&&productFeatureGroupId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "productFeatureGroup==null&&productFeatureGroupId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productFeatureGroup==null", target = "CreateProductFeatureGroup")
        }
    )
    public interface CreateFeatureGroup {}

    @Form(
        name = "ListFeatureGroupAppls",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.MULTI,
        target = "UpdateProductFeatureGroupAppl?productFeatureGroupId=${productFeatureGroupId}",
        listName = "productFeatureGroupAndAppls",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productFeatureGroupId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", hidden = @HiddenField),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.CommonId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.ProductFeature}", display = @DisplayField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductFeatureType")),
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.ProductFeatureCategory}", displayEntity = @DisplayEntityField(entityName = "ProductFeatureCategory")),
            @FormField(name = "sequenceNum", text = @TextField(size = 3)),
            @FormField(name = "removeFeatureGroupApplAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveProductFeatureGroupAppl", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "productFeatureGroupId"), @ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListFeatureGroupAppls {}

    @Form(
        name = "QuickApplyFeatureToGroup",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "CreateProductFeatureGroupAppl",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureGroupId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.ProductFeatureId}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface QuickApplyFeatureToGroup {}

    @Form(
        name = "ApplyFeatureCategoryToGroup",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "EditFeatureGroupAppls",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureGroupId", hidden = @HiddenField),
            @FormField(name = "productFeatureCategoryId", dropDown = @DropDownField(listOptions = @ListOptions(listName = "productFeatureCategories", keyName = "productFeatureCategoryId", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonContinue}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ApplyFeatureCategoryToGroup {}

    @Form(
        name = "ApplyFeaturesFromCategoryToGroup",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.MULTI,
        target = "ApplyFeaturesFromCategoryToGroup?productFeatureGroupId=${productFeatureGroupId}",
        listName = "productFeatures",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productFeatureGroupId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.CommonId}", display = @DisplayField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductFeatureType")),
            @FormField(name = "description", title = "${uiLabelMap.ProductFeature}", display = @DisplayField),
            @FormField(name = "idCode", title = "${uiLabelMap.CommonIdCode}", display = @DisplayField),
            @FormField(name = "abbrev", title = "${uiLabelMap.ProductAbbrev}", display = @DisplayField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ApplyFeaturesFromCategoryToGroup {}

    @Form(
        name = "FindProductFeatureCategory",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "EditFeatureCategories",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField(size = 20, maxlength = 255)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindProductFeatureCategory {}

    @Form(
        name = "ListProductFeatureCategory",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        target = "UpdateFeatureCategory",
        listName = "listIt",
        paginateTarget = "EditFeatureCategories",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFeatureCategoryFeatures", description = "${productFeatureCategoryId}", parameters = {@ParameterDef(paramName = "productFeatureCategoryId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "update", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductFeatureCategory"), @FieldMap(fieldName = "orderBy", value = "productFeatureCategoryId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListProductFeatureCategory {}

    @Form(
        name = "CreateProductFeature",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "CreateFeatureCategory",
        fields = {
            @FormField(name = "isCreate", hidden = @HiddenField(value = "true")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}*", text = @TextField),
            @FormField(name = "parentCategory", title = "${uiLabelMap.ProductParentCategory}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureCategory", description = "${description}", keyFieldName = "description"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductFeature {}

    @Form(
        name = "ListFeaturePrice",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        target = "updateFeaturePrice",
        listName = "productFeaturePrice",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.ProductPriceType}", displayEntity = @DisplayEntityField(entityName = "ProductPriceType")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.ProductCurrency}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description} [${uomId}]")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(defaultValue = "${thruDate}")),
            @FormField(name = "price", title = "${uiLabelMap.ProductPrice}", requiredField = true, text = @TextField(defaultValue = "${price}")),
            @FormField(name = "updateFeaturePrice", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "lastModifiedBy", title = "${uiLabelMap.ProductLastModifiedBy}", display = @DisplayField(description = "[${lastModifiedByUserLogin}] ${uiLabelMap.CommonOn} ${lastModifiedDate}")),
            @FormField(name = "deleteFeaturePriceAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFeaturePrice", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productFeatureId"), @ParameterDef(paramName = "productPriceTypeId"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListFeaturePrice {}

    @Form(
        name = "CreateFeaturePrice",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "createFeaturePrice",
        fields = {
            @FormField(name = "isCreate", hidden = @HiddenField(value = "true")),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "productPriceTypeId", title = "${uiLabelMap.ProductPriceType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPriceType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "price", title = "${uiLabelMap.ProductPrice}", requiredField = true, text = @TextField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.ProductCurrency}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateFeaturePrice {}

    @Form(
        name = "FindProductFeature",
        location = "component://product/widget/catalog/FeatureForms.xml",
        target = "ListFeatures",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.ProductFeatureType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.ProductFeatureCategory}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureCategory", description = "${description} [${productFeatureCategoryId}]", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "uomId", title = "${uiLabelMap.ProductUnitOfMeasureId}", textFind = @TextFindField),
            @FormField(name = "idCode", title = "${uiLabelMap.ProductIdCode}", textFind = @TextFindField),
            @FormField(name = "abbrev", title = "${uiLabelMap.ProductAbbreviation}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindProductFeature {}

    @Form(
        name = "ListProductFeature",
        location = "component://product/widget/catalog/FeatureForms.xml",
        type = FormType.LIST,
        target = "updateProductFeature",
        listName = "listIt",
        paginateTarget = "ListFeatures",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productFeatureId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFeature", description = "${productFeatureId}", parameters = {@ParameterDef(paramName = "productFeatureId")})),
            @FormField(name = "productFeatureCategoryId", title = "${uiLabelMap.ProductFeatureCategory}", hyperlink = @HyperlinkField(target = "EditFeatureCategoryFeatures", description = "${featureCategory.description}", parameters = {@ParameterDef(paramName = "productFeatureCategoryId", fromField = "featureCategory.productFeatureCategoryId")})),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.ProductFeatureType}", hyperlink = @HyperlinkField(target = "EditFeatureType", description = "${featureType.description}", parameters = {@ParameterDef(paramName = "productFeatureTypeId", fromField = "featureType.productFeatureTypeId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "uomId", display = @DisplayField),
            @FormField(name = "numberSpecified", display = @DisplayField),
            @FormField(name = "abrev", display = @DisplayField),
            @FormField(name = "idCode", display = @DisplayField),
            @FormField(name = "update", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.noConditionFind", value = "Y")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductFeature"), @FieldMap(fieldName = "orderBy", value = "productFeatureId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "ProductFeatureCategory", valueField = "featureCategory"), @EntityOneAction(entityName = "ProductFeatureType", valueField = "featureType")})
    )
    public interface ListProductFeature {}

}
