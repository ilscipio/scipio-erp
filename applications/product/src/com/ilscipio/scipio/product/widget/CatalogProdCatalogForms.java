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
public class CatalogProdCatalogForms {

    @Form(
        name = "FindCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        target = "FindCatalog",
        defaultMapName = "catalog",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProdCatalogId}", textFind = @TextFindField),
            @FormField(name = "catalogName", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCatalog {}

    @Form(
        name = "ListCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindCatalog",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "prodCatalogId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditCatalog", description = "${prodCatalogId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "prodCatalogId")})),
            @FormField(name = "prodCatalogId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditProdCatalog?prodCatalogId=${prodCatalogId}", description = "${prodCatalogId}")),
            @FormField(name = "catalogName", sortField = true, display = @DisplayField),
            @FormField(name = "useQuickAdd", title = "${uiLabelMap.ProductUseQuickAdd}", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "ProdCatalog"), @SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "+catalogName")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "noConditionFind", value = "Y")})})
    )
    public interface ListCatalog {}

    @Form(
        name = "EditProdCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        target = "updateProdCatalog",
        defaultMapName = "prodCatalog",
        defaultEntityName = "ProdCatalog",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalogId}", tooltip = "${uiLabelMap.ProductNotModificationRecreatingProductCatalog}.", useWhen = "prodCatalog!=null", display = @DisplayField),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalogId}", tooltip = "${uiLabelMap.ProductCouldNotFindProductCatalogWithId} [${prodCatalogId}]", useWhen = "prodCatalog==null&&prodCatalogId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalogId}", useWhen = "prodCatalog==null&&prodCatalogId==null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "catalogName", position = 2, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "styleSheet", title = "${uiLabelMap.ProductStyleSheet}", text = @TextField(size = 60)),
            @FormField(name = "headerLogo", title = "${uiLabelMap.ProductHeaderLogo}", text = @TextField(size = 60)),
            @FormField(name = "contentPathPrefix", title = "${uiLabelMap.ProductContentPathPrefix}", tooltip = "${uiLabelMap.ProductPrependedImageContentPaths}", text = @TextField(size = 60)),
            @FormField(name = "templatePathPrefix", title = "${uiLabelMap.ProductTemplatePathPrefix}", tooltip = "${uiLabelMap.ProductPrependedTemplatePaths}", text = @TextField(size = 60)),
            @FormField(name = "useQuickAdd", title = "${uiLabelMap.ProductUseQuickAdd}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "viewAllowPermReqd", title = "${uiLabelMap.ProductCategoryViewAllowPermReqd}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "purchaseAllowPermReqd", title = "${uiLabelMap.ProductCategoryPurchaseAllowPermReqd}", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "prodCatalog==null", target = "createProdCatalog")
        }
    )
    public interface EditProdCatalog {}

    @Form(
        name = "AddProdCatalogToParty",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        target = "addProdCatalogToParty",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addProdCatalogToParty")
        },
        fields = {
            @FormField(name = "prodCatalogId", mapName = "prodCatalog", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRole}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProdCatalogToParty {}

    @Form(
        name = "UpdateProdCatalogToParty",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        type = FormType.LIST,
        target = "updateProdCatalogToParty",
        listName = "prodCatalogRoleList",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProdCatalogToParty")
        },
        fields = {
            @FormField(name = "prodCatalogId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${personalTitle} ${firstName} ${middleName} ${lastName} ${suffix} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "party_id", fromField = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRole}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProdCatalogFromParty", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProdCatalogToParty {}

    @Form(
        name = "CreateProductStoreCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        target = "createProdCatalogStore",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductStoreCatalog")
        },
        fields = {
            @FormField(name = "prodCatalogId", mapName = "prodCatalog", hidden = @HiddenField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStore}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductStoreCatalog {}

    @Form(
        name = "UpdateProductStoreCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        type = FormType.LIST,
        target = "updateProdCatalogStore",
        listName = "productStoreCatalogList",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreCatalog")
        },
        fields = {
            @FormField(name = "prodCatalogId", hidden = @HiddenField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", displayEntity = @DisplayEntityField(entityName = "ProductStore", description = "${storeName}", cache = true, subHyperlink = @SubHyperlink(target = "EditProductStore", description = "${productStoreId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productStoreId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProdCatalogStore", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductStoreCatalog {}

    @Form(
        name = "EditProdCatalogCategories",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        type = FormType.LIST,
        target = "updateProductCategoryToProdCatalog",
        listName = "prodCatalogCategories",
        paginateTarget = "EditProdCatalogCategories",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductCategoryToProdCatalog")
        },
        fields = {
            @FormField(name = "prodCatalogId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategoryId}", displayEntity = @DisplayEntityField(entityName = "ProductCategory", description = "${description}", cache = true, subHyperlink = @SubHyperlink(target = "EditCategory", description = "${productCategoryId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productCategoryId")}))),
            @FormField(name = "prodCatalogCategoryTypeId", title = "${uiLabelMap.ProductCatalogCategoryType}", displayEntity = @DisplayEntityField(entityName = "ProdCatalogCategoryType", cache = true)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}"),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductCategoryFromProdCatalog", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "productCategoryId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "prodCatalogCategoryTypeId")})),
            @FormField(name = "makeTopAction", title = " ", widgetStyle = "${styles.link_run_session} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditCategory", description = "${uiLabelMap.ProductMakeTop}", alsoHidden = false, parameters = {@ParameterDef(paramName = "CATALOG_TOP_CATEGORY", fromField = "productCategoryId"), @ParameterDef(paramName = "productCategoryId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditProdCatalogCategories {}

    @Form(
        name = "addProductCategoryToProdCatalog",
        location = "component://product/widget/catalog/ProdCatalogForms.xml",
        target = "addProductCategoryToProdCatalog",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addProductCategoryToProdCatalog")
        },
        fields = {
            @FormField(name = "prodCatalogId", hidden = @HiddenField),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductCategoryId}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "prodCatalogCategoryTypeId", title = "${uiLabelMap.ProductCatalogCategoryType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdCatalogCategoryType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}"),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface addProductCategoryToProdCatalog {}

}
