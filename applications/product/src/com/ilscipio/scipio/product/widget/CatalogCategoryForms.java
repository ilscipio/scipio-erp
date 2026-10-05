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
public class CatalogCategoryForms {

    @Form(
        name = "FindCategory",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "FindCategory",
        defaultMapName = "category",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "categoryName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCategory {}

    @Form(
        name = "ListCategory",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindCategory",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productCategoryId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditCategory", description = "${productCategoryId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId")})),
            @FormField(name = "categoryName", title = "${uiLabelMap.CommonName}", sortField = true, display = @DisplayField),
            @FormField(name = "productCategoryTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "ProductCategoryType", description = "${description}")),
            @FormField(name = "primaryParentCategoryId", title = "${uiLabelMap.CommonParent}", sortField = true, display = @DisplayField),
            @FormField(name = "description", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "ProductCategory")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "noConditionFind", value = "Y")})})
    )
    public interface ListCategory {}

    @Form(
        name = "CreateProductCategoryAttribute",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "createProductCategoryAttribute",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductCategoryAttribute", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "productCategoryId", display = @DisplayField),
            @FormField(name = "submitForm", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductCategoryAttribute {}

    @Form(
        name = "AddCategoryCatalog",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "category_addProductCategoryToProdCatalog",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalog}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdCatalog", description = "${catalogName}", orderBy = {@EntityOrderBy(fieldName = "catalogName")}))),
            @FormField(name = "prodCatalogCategoryTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdCatalogCategoryType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCategoryCatalog {}

    @Form(
        name = "AddCategoryChild",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "addProductCategoryToCategory",
        fields = {
            @FormField(name = "showProductCategoryId", hidden = @HiddenField),
            @FormField(name = "parentProductCategoryId", hidden = @HiddenField(value = "${showProductCategoryId}")),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategory}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProductCategory", defaultValue = " ")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCategoryChild {}

    @Form(
        name = "AddCategoryParent",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "addProductCategoryToCategory",
        fields = {
            @FormField(name = "showProductCategoryId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "parentProductCategoryId", title = "${uiLabelMap.ProductCategory}", requiredField = true, lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCategoryParent {}

    @Form(
        name = "AddCategoryParty",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "addPartyToCategory",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "comment", title = "${uiLabelMap.CommonComment}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCategoryParty {}

    @Form(
        name = "UpdateCategoryCatalogs",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "category_updateProductCategoryToProdCatalog",
        listName = "prodCatalogCategories",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalog}", displayEntity = @DisplayEntityField(entityName = "ProdCatalog", description = " ", cache = true, subHyperlink = @SubHyperlink(target = "EditProdCatalog", description = "${categoryCatalog.catalogName}", linkStyle = "${styles.link_nav_info_name}", parameters = {@ParameterDef(paramName = "prodCatalogId")}))),
            @FormField(name = "prodCatalogCategoryTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProdCatalogCategoryType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "category_removeProductCategoryFromProdCatalog", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "prodCatalogCategoryTypeId"), @ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "ProdCatalog", valueField = "categoryCatalog")})
    )
    public interface UpdateCategoryCatalogs {}

    @Form(
        name = "UpdateCategoryChild",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryToCategory",
        listName = "parentProductCategoryRollups",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "showProductCategoryId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategory}", displayEntity = @DisplayEntityField(entityName = "ProductCategory", description = " ", cache = true, subHyperlink = @SubHyperlink(target = "EditCategory", description = "${category.categoryName} [${productCategoryId}]", linkStyle = "${styles.link_nav_info_idname}", parameters = {@ParameterDef(paramName = "productCategoryId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "comment", title = "${uiLabelMap.CommonComment}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductCategoryFromCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "showproductCategoryId"), @ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "parentProductCategoryId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "productCategoryId", fromField = "productCategoryId")}, entityOne = {@EntityOneAction(entityName = "ProductCategory", valueField = "category")})
    )
    public interface UpdateCategoryChild {}

    @Form(
        name = "UpdateCategoryParent",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryToCategory",
        listName = "currentProductCategoryRollups",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "showProductCategoryId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategory}", displayEntity = @DisplayEntityField(entityName = "ProductCategory", description = " ", cache = true, subHyperlink = @SubHyperlink(target = "EditCategory", description = "${category.categoryName} [${productCategoryId}]", linkStyle = "${styles.link_nav_info_idname}", parameters = {@ParameterDef(paramName = "productCategoryId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "comment", title = "${uiLabelMap.CommonComment}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductCategoryFromCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "showProductCategoryId"), @ParameterDef(paramName = "productCategoryId", value = "${showProductCategoryId}"), @ParameterDef(paramName = "parentProductCategoryId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "productCategoryId", fromField = "parentProductCategoryId")}, entityOne = {@EntityOneAction(entityName = "ProductCategory", valueField = "category")})
    )
    public interface UpdateCategoryParent {}

    @Form(
        name = "UpdateCategoryParties",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updatePartyToCategory",
        listName = "productCategoryRoles",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = " ", cache = true, subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${categoryParty.groupName}${categoryParty.firstName} ${categoryParty.middleInitial} ${categoryParty.lastName}", linkStyle = "${styles.link_nav_info_name_long}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "comment", title = "${uiLabelMap.CommonComment}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "category_removeProductCategoryFromProdCatalog", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "prodCatalogCategoryTypeId"), @ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "categoryParty")})
    )
    public interface UpdateCategoryParties {}

    @Form(
        name = "EditProductCategoryAttributes",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryAttribute",
        listName = "categoryAttributes",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductCategoryAttribute", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "attrValue", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductCategoryAttribute", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "attrName")}))
        }
    )
    public interface EditProductCategoryAttributes {}

    @Form(
        name = "AddCategoryContentAssoc",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "addContentToCategory",
        title = "${uiLabelMap.ProductAddProductCategoryContentFromDate}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategoryContent")
        },
        fields = {
            @FormField(name = "productCategoryId", mapName = "productCategory", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "prodCatContentTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductCategoryContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "purchaseFromDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "purchaseThruDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "useCountLimit", text = @TextField),
            @FormField(name = "useDaysLimit", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCategoryContentAssoc {}

    @Form(
        name = "PrepareAddCategoryContentAssoc",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "prepareAddContentToCategory",
        title = "${uiLabelMap.ProductAddProductCategoryContentFromDate}",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", mapName = "productCategory", hidden = @HiddenField),
            @FormField(name = "prodCatContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductCategoryContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductPrepareCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface PrepareAddCategoryContentAssoc {}

    @Form(
        name = "UpdateCategoryContentAssoc",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updateContentToCategory",
        listName = "productCategoryContentList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContent}", displayEntity = @DisplayEntityField(entityName = "Content", description = "${description}", subHyperlink = @SubHyperlink(target = "EditCategoryContentContent", description = "${contentId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "prodCatContentTypeId"), @ParameterDef(paramName = "fromDate")}))),
            @FormField(name = "prodCatContentTypeId", title = "${uiLabelMap.ProductType}", displayEntity = @DisplayEntityField(entityName = "ProductCategoryContentType", description = "${description}", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "editAction", title = "${uiLabelMap.ProductEditContent}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "/content/control/EditContent", urlMode = UrlMode.INTER_APP, description = "${contentId}", parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentFromCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "prodCatContentTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateCategoryContentAssoc {}

    @Form(
        name = "EditCategoryContentSimpleText",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "updateSimpleTextContentForCategory",
        title = "${uiLabelMap.ProductUpdateSimpleTextContentCategory}",
        defaultMapName = "categoryContent",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategoryContent")
        },
        fields = {
            @FormField(name = "contentId", useWhen = "content==null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "categoryContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "content!=null", display = @DisplayField),
            @FormField(name = "prodCatContentTypeId", title = "${uiLabelMap.CommonType}", position = 2, displayEntity = @DisplayEntityField(entityName = "ProductCategoryContentType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId==null", requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "description", mapName = "content", textarea = @TextareaField),
            @FormField(name = "localeString", mapName = "content", text = @TextField(size = 40)),
            @FormField(name = "text", mapName = "textDataMap", textarea = @TextareaField(cols = 80, rows = 10)),
            @FormField(name = "textDataResourceId", mapName = "textDataMap", hidden = @HiddenField),
            @FormField(name = "useCountLimit", hidden = @HiddenField),
            @FormField(name = "useDaysLimit", hidden = @HiddenField),
            @FormField(name = "purchaseThruDate", hidden = @HiddenField),
            @FormField(name = "purchaseFromDate", hidden = @HiddenField),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "content == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "content != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "content==null", target = "createSimpleTextContentForCategory")
        }
    )
    public interface EditCategoryContentSimpleText {}

    @Form(
        name = "CreateSimpleTextContentForAlternateLocale",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "createSimpleTextContentForAlternateLocaleInCategory",
        title = "${uiLabelMap.ProductCreateSimpleTextContentForAlternateLocale}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "mainContentId", entryName = "contentId", hidden = @HiddenField),
            @FormField(name = "localeString", text = @TextField),
            @FormField(name = "text", text = @TextField),
            @FormField(name = "prodCatContentTypeId", mapName = "productCategoryContent", hidden = @HiddenField),
            @FormField(name = "fromDate", mapName = "productCategoryContent", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateSimpleTextContentForAlternateLocale {}

    @Form(
        name = "ListSimpleTextContentForAlternateLocale",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "updateSimpleTextContentForAlternateLocaleInCategory?productCategoryId=${productCategoryContent.productCategoryId}&prodCatContentTypeId=${productCategoryContent.prodCatContentTypeId}&fromDate=${productCategoryContent.fromDate}&contentId=${contentId}",
        extendsForm = "ListSimpleTextContentForAlternateLocaleCore",
        extendsResource = "component://product/widget/catalog/ProductForms.xml",
        fields = {
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSimpleTextContentForAlternateLocaleInCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, linkType = "hidden-form", parameters = {@ParameterDef(paramName = "mainContentId", fromField = "contentAssoc.contentId"), @ParameterDef(paramName = "contentId", fromField = "contentAssoc.contentIdTo"), @ParameterDef(paramName = "productCategoryId", fromField = "productCategoryContent.productCategoryId"), @ParameterDef(paramName = "prodCatContentTypeId", fromField = "productCategoryContent.prodCatContentTypeId"), @ParameterDef(paramName = "fromDate", fromField = "productCategoryContent.fromDate")}))
        }
    )
    public interface ListSimpleTextContentForAlternateLocale {}

    @Form(
        name = "ListProductCategoryLinks",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryLink",
        listName = "productCategoryLinks",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductCategoryLink")
        },
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "linkSeqId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "titleText", text = @TextField(size = 20)),
            @FormField(name = "comments", text = @TextField(size = 20)),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequence}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "linkTypeEnumId", ignored = @IgnoredField),
            @FormField(name = "detailText", ignored = @IgnoredField),
            @FormField(name = "linkInfo", ignored = @IgnoredField),
            @FormField(name = "detailSubScreen", ignored = @IgnoredField),
            @FormField(name = "imageUrl", ignored = @IgnoredField),
            @FormField(name = "imageTwoUrl", ignored = @IgnoredField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductCategoryAction", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "linkSeqId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductCategoryLinks {}

    @Form(
        name = "AddProductCategoryLink",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "createProductCategoryLink",
        defaultMapName = "productCategoryAction",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductCategoryLink")
        },
        fields = {
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "linkSeqId", useWhen = "productCategoryLink==null", ignored = @IgnoredField),
            @FormField(name = "linkSeqId", useWhen = "productCategoryLink!=null", display = @DisplayField),
            @FormField(name = "fromDate", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "productCategoryLink != null", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "productCategoryLink == null", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequence}", text = @TextField(size = 5, maxlength = 5)),
            @FormField(name = "imageUrl", tooltip = "${uiLabelMap.ProductImageUrlTooltip}", text = @TextField(size = 60, maxlength = 255)),
            @FormField(name = "linkTypeEnumId", title = "${uiLabelMap.ProductLinkTypeEnumId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${groovy: uiLabelMap.get(\"ProductCategoryLinkType.description.\"+enumId)}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PCAT_LINK_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "detailSubScreen", tooltip = "${uiLabelMap.ProductDetailSubScreenTooltip}", text = @TextField(size = 60, maxlength = 255)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "productCategoryLink==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "productCategoryLink!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "clearFormAction", title = " ", useWhen = "productCategoryLink!=null", widgetStyle = "${styles.link_run_local} ${styles.action_clear}", hyperlink = @HyperlinkField(target = "EditProductCategoryLinks", description = "${uiLabelMap.CommonClear}", parameters = {@ParameterDef(paramName = "productCategoryId")}))
        },
        altTargets = {
            @AltTarget(useWhen = "productCategoryLink != null", target = "updateProductCategoryAction")
        }
    )
    public interface AddProductCategoryLink {}

    @Form(
        name = "ListTopCategory",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.LIST,
        listName = "noParentCategories",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productCategoryId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditCategory", description = "${productCategoryId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "CATALOG_TOP_CATEGORY", value = "${productCategoryId}"), @ParameterDef(paramName = "productCategoryId", value = "${productCategoryId}")})),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface ListTopCategory {}

    @Form(
        name = "EditCategoryContentSEO",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "updateContentSEOForCategory",
        title = "${uiLabelMap.ProductUpdateSEOContentCategory}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "title", title = "${uiLabelMap.PageTitle}", text = @TextField(size = 40)),
            @FormField(name = "metaKeyword", title = "${uiLabelMap.MetaKeywords}", textarea = @TextareaField(rows = 5)),
            @FormField(name = "metaDescription", title = "${uiLabelMap.MetaDescription}", textarea = @TextareaField(rows = 5)),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "prodCatContentTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCategoryContentSEO {}

    @Form(
        name = "EditCategoryContentRelatedUrl",
        location = "component://product/widget/catalog/CategoryForms.xml",
        target = "updateRelatedUrlContentForCategory",
        title = "${uiLabelMap.ProductUpdateRelatedURLContentCategory}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategoryContent")
        },
        fields = {
            @FormField(name = "contentId", useWhen = "content==null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "categoryContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "content!=null", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}*", useWhen = "contentId==null", dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField),
            @FormField(name = "dataResourceId", hidden = @HiddenField),
            @FormField(name = "prodCatContentTypeId", displayEntity = @DisplayEntityField(entityName = "ProductCategoryContentType")),
            @FormField(name = "title", title = "${uiLabelMap.CommonTitle}", text = @TextField(size = 50)),
            @FormField(name = "description", title = "${uiLabelMap.FormFieldTitle_description}", textarea = @TextareaField(cols = 50, rows = 2)),
            @FormField(name = "url", textarea = @TextareaField(cols = 50, rows = 2)),
            @FormField(name = "localeString", lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "purchaseFromDate", hidden = @HiddenField),
            @FormField(name = "purchaseThruDate", hidden = @HiddenField),
            @FormField(name = "useCountLimit", hidden = @HiddenField),
            @FormField(name = "useDaysLimit", hidden = @HiddenField),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "content == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "content != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "content==null", target = "createRelatedUrlContentForCategory")
        }
    )
    public interface EditCategoryContentRelatedUrl {}

    @Form(
        name = "EditCategoryContentDownload",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.UPLOAD,
        target = "updateDownloadContentForCategory",
        title = "${uiLabelMap.ProductUpdateDownloadContentCategory}",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategoryContent", mapName = "productCategoryContentData")
        },
        fields = {
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}*", useWhen = "contentId==null", dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "purchaseFromDate", title = "${uiLabelMap.ProductPurchaseFromDate}"),
            @FormField(name = "purchaseThruDate", title = "${uiLabelMap.ProductPurchaseThruDate}"),
            @FormField(name = "useCountLimit", title = "${uiLabelMap.ProductUseCountLimit}"),
            @FormField(name = "useDaysLimit", title = "${uiLabelMap.ProductUseTime}"),
            @FormField(name = "description", mapName = "content", title = "${uiLabelMap.ProductCategoryDescription}", text = @TextField(size = 40)),
            @FormField(name = "contentId", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductOptional}", useWhen = "contentId == null", text = @TextField(maxlength = 20)),
            @FormField(name = "contentId", mapName = "productContentData", title = "${uiLabelMap.ProductContentId}", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/content/control/editContent", urlMode = UrlMode.INTER_APP, description = "${contentId} ${contentName}", parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "imageData", title = "${uiLabelMap.ProductFile}", file = @FileField),
            @FormField(name = "fileDataResourceId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "prodCatContentTypeId", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId==null", target = "createDownloadContentForCategory")
        }
    )
    public interface EditCategoryContentDownload {}

    @Form(
        name = "EditCategoryContentImage",
        location = "component://product/widget/catalog/CategoryForms.xml",
        type = FormType.UPLOAD,
        target = "addAdditionalImageContentForProduct",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductCategoryContent", mapName = "productCategoryContentData")
        },
        fields = {
            @FormField(name = "prodCatContentTypeId", displayEntity = @DisplayEntityField(entityName = "ProductCategoryContentType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId==null", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "contentId!=null", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2),
            @FormField(name = "contentId", useWhen = "contentId == null", ignored = @IgnoredField),
            @FormField(name = "contentId", mapName = "productCategoryContentData", tooltip = "${uiLabelMap.ProductNotModificationRecrationProductContentAssociation}", useWhen = "contentId != null", display = @DisplayField),
            @FormField(name = "useTime", hidden = @HiddenField),
            @FormField(name = "useTimeUomId", hidden = @HiddenField),
            @FormField(name = "useRoleTypeId", hidden = @HiddenField),
            @FormField(name = "useCountLimit", hidden = @HiddenField),
            @FormField(name = "useDaysLimit", hidden = @HiddenField),
            @FormField(name = "purchaseThruDate", hidden = @HiddenField),
            @FormField(name = "purchaseFromDate", hidden = @HiddenField),
            @FormField(name = "uploadedFile", title = "${uiLabelMap.ProductFile}", file = @FileField),
            @FormField(name = "productCategoryId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "contentId == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "contentId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCategoryContentImage {}

}
