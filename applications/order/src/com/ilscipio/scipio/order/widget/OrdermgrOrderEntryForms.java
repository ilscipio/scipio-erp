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
public class OrdermgrOrderEntryForms {

    @Form(
        name = "FindRequirements",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        target = "RequirementsForSupplier",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "showList", hidden = @HiddenField(value = "Y")),
            @FormField(name = "requirementId", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartySupplier}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "requirementByDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindRequirements {}

    @Form(
        name = "RequirementsList",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        type = FormType.MULTI,
        target = "addRequirementsToCart",
        listName = "requirementsForSupplier",
        paginateTarget = "RequirementsForSupplier",
        defaultTitleStyle = "tableheadtext",
        useRowSubmit = true,
        fields = {
            @FormField(name = "requirementId", display = @DisplayField),
            @FormField(name = "productId", hidden = @HiddenField(value = "${productId}")),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "requiredByDate", display = @DisplayField),
            @FormField(name = "quantity", text = @TextField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "prepareFind", resultMapName = "resultConditions", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName")}), @ServiceAction(serviceName = "getRequirementsForSupplier", resultMapName = "result", resultMapList = "requirementsForSupplier", fieldMaps = {@FieldMap(fieldName = "requirementConditions", fromField = "resultConditions.entityConditionList"), @FieldMap(fieldName = "partyId", fromField = "parameters.partyId")})})
    )
    public interface RequirementsList {}

    @Form(
        name = "FindQuotes",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        target = "FindQuoteForCart",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Quote", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.OrderOrderQuoteId}"),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.OrderOrderQuoteTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuoteType", description = "${description}", keyFieldName = "quoteTypeId"))),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}"),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", hidden = @HiddenField),
            @FormField(name = "validFromDate", hidden = @HiddenField),
            @FormField(name = "validThruDate", hidden = @HiddenField),
            @FormField(name = "description", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindQuotes {}

    @Form(
        name = "ListQuotes",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindQuoteForCart",
        defaultTitleStyle = "tableheadtext",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Quote", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.OrderOrderQuoteId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "loadCartFromQuote", description = "${quoteId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId")})),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.OrderOrderQuoteTypeId}", displayEntity = @DisplayEntityField(entityName = "QuoteType")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}"),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}"),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}"),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}"),
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "Quote")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListQuotes {}

    @Form(
        name = "ViewShoppingLists",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        type = FormType.LIST,
        listName = "customershoppinglists",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "listName", title = "${uiLabelMap.PageTitleShoppingList}", display = @DisplayField),
            @FormField(name = "shoppingListTypeId", title = "${uiLabelMap.OrderListType}", displayEntity = @DisplayEntityField(entityName = "ShoppingListType")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "addFromListAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "addFromShoppingList", description = "${uiLabelMap.OrderToAddSelectedItemsToShoppingList}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shoppingListId")})),
            @FormField(name = "addAllFromList", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "addAllFromShoppingList", description = "${uiLabelMap.OrderQuickAdd}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shoppingListId")}))
        }
    )
    public interface ViewShoppingLists {}

    @Form(
        name = "AddFromShoppingList",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        type = FormType.LIST,
        listName = "shoppinglistitems",
        defaultTitleStyle = "tableheadtext",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShoppingListItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProduct}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${productId} - ${description}")),
            @FormField(name = "addToCart", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "additem/editShoppingList", description = "${uiLabelMap.CommonAdd} ${quantity} ${uiLabelMap.OrderAddQntToOrder}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shoppingListId"), @ParameterDef(paramName = "shoppingListItemSeqId"), @ParameterDef(paramName = "add_product_id", fromField = "productId"), @ParameterDef(paramName = "quantity"), @ParameterDef(paramName = "configId")}))
        }
    )
    public interface AddFromShoppingList {}

    @Form(
        name = "AddFromShoppingListAll",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        fields = {
            @FormField(name = "addAllFromList", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "addAllFromShoppingList", description = "${uiLabelMap.OrderQuickAdd}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shoppingListId")})),
            @FormField(name = "returnToOrderEntry", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "orderentry", description = "${uiLabelMap.OrderOrderReturn}", alsoHidden = false))
        }
    )
    public interface AddFromShoppingListAll {}

    @Form(
        name = "LookupAssociatedProducts",
        location = "component://order/widget/ordermgr/OrderEntryForms.xml",
        type = FormType.MULTI,
        target = "BulkAddProducts",
        listName = "productList",
        paginateTarget = "LookupAssociatedProducts",
        defaultTitleStyle = "tableheadtext",
        defaultWidgetStyle = "inputBox",
        useRowSubmit = true,
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductInventoryItems?productId=${productId}", urlMode = UrlMode.INTER_APP, description = "${productId}")),
            @FormField(name = "brandName", title = "${uiLabelMap.ProductBrandName}", display = @DisplayField),
            @FormField(name = "internalName", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.OrderQuantity}", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "amount", title = "${uiLabelMap.OrderAmount}", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "itemDesiredDeliveryDate", title = "${uiLabelMap.OrderDesiredDeliveryDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.OrderAddToOrder}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface LookupAssociatedProducts {}

}
