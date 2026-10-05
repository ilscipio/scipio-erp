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
public class OrdermgrQuoteForms {

    @Form(
        name = "FindQuotes",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "FindQuote",
        headerRowStyle = "header-row",
        defaultPositionSpan = 1,
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuoteType", description = "${description}", keyFieldName = "quoteTypeId"))),
            @FormField(name = "quoteName", title = "${uiLabelMap.OrderOrderQuoteName}", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "QUOTE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}", position = 2, dateFind = @DateFindField(type = "date")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", hidden = @HiddenField),
            @FormField(name = "validFromDate", hidden = @HiddenField),
            @FormField(name = "validThruDate", hidden = @HiddenField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductProductStore}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y"))
        }
    )
    public interface FindQuotes {}

    @Form(
        name = "ListQuotes",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindQuote",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ViewQuote", description = "${quoteId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId")})),
            @FormField(name = "issueDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false)),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "QuoteType")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName}${firstName} ${middleInitial} ${lastName}")),
            @FormField(name = "quoteName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStore}", displayEntity = @DisplayEntityField(entityName = "ProductStore", description = "${storeName}")),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}", display = @DisplayField(type = "date")),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}", display = @DisplayField(type = "date"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Quote"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListQuotes {}

    @Form(
        name = "EditQuote",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuote",
        defaultMapName = "quote",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.CommonId}", useWhen = "quote!=null", display = @DisplayField),
            @FormField(name = "quoteName", title = "${uiLabelMap.OrderOrderQuoteName}", text = @TextField),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuoteType", description = "${description}", keyFieldName = "quoteTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${quote.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStore}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textarea = @TextareaField(maxlength = 255)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quote==null", target = "createQuote")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditQuote {}

    @Form(
        name = "ListQuoteRoles",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteRoles",
        paginateTarget = "ListQuoteRoles",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false)),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeQuoteRole", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteRoles {}

    @Form(
        name = "EditQuoteRole",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "createQuoteRole",
        defaultMapName = "quoteRole",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteRole", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditQuoteRole {}

    @Form(
        name = "EditQuoteItem",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteItem",
        defaultMapName = "quoteItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "isPromo", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", lookup = @LookupField(targetFormName = "LookupProductAndPrice")),
            @FormField(name = "productFeatureId", title = "${uiLabelMap.ProductFeatures}", position = 2, lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "configId", text = @TextField),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "uomId")}))),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField),
            @FormField(name = "selectedAmount", text = @TextField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderPrice}", position = 2, text = @TextField),
            @FormField(name = "estimatedDeliveryDate", title = "${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "custRequestId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CustRequest", description = "${custRequestId}"))),
            @FormField(name = "custRequestItemSeqId", position = 2, text = @TextField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", text = @TextField),
            @FormField(name = "reservPersons", position = 2, text = @TextField),
            @FormField(name = "reservStart", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "reservLength", position = 2, text = @TextField),
            @FormField(name = "skillTypeId", title = "${uiLabelMap.HumanResSkill}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", keyFieldName = "skillTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "deliverableTypeId", title = "${uiLabelMap.WorkEffortDeliverableType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DeliverableType", description = "${description}", keyFieldName = "deliverableTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}", position = 2, textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteItem==null", target = "createQuoteItem")
        },
        sortOrder = @SortOrder()
    )
    public interface EditQuoteItem {}

    @Form(
        name = "ListQuoteItems",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteItems",
        paginateTarget = "ListQuoteItems",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.CommonSeqNum}", useWhen = "${groovy:isPromo==null}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteItem", description = "${quoteItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.CommonSeqNum}", useWhen = "${groovy: 'N'.equals(isPromo)}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteItem", description = "${quoteItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.CommonSeqNum}", useWhen = "${groovy: 'Y'.equals(isPromo)}", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderPrice}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "estimatedDeliveryDate", title = "${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}", display = @DisplayField(type = "date")),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "skillTypeId", hidden = @HiddenField),
            @FormField(name = "deliverableTypeId", hidden = @HiddenField),
            @FormField(name = "comments", hidden = @HiddenField),
            @FormField(name = "uomId", hidden = @HiddenField),
            @FormField(name = "custRequestId", title = "${uiLabelMap.OrderRequest}", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "requestitem", description = "${custRequestId} ${custRequestItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeQuoteItem", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteItemSeqId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteItems {}

    @Form(
        name = "EditQuoteAttribute",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteAttribute",
        defaultMapName = "quoteAttribute",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteAttribute", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "attrName", title = "${uiLabelMap.OrderOrderQuoteAttributeName}", useWhen = "quoteAttribute==null", text = @TextField),
            @FormField(name = "attrName", title = "${uiLabelMap.OrderOrderQuoteAttributeName}", useWhen = "quoteAttribute!=null", display = @DisplayField),
            @FormField(name = "attrValue", title = "${uiLabelMap.OrderOrderQuoteAttributeValue}"),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteAttribute==null", target = "createQuoteAttribute")
        }
    )
    public interface EditQuoteAttribute {}

    @Form(
        name = "ListQuoteAttributes",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteAttributes",
        paginateTarget = "ListQuoteAttributes",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteAttribute", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "attrName", title = "${uiLabelMap.OrderOrderQuoteAttributeName}", widgetStyle = "${styles.link_nav_info_name}", hyperlink = @HyperlinkField(target = "EditQuoteAttribute", description = "${attrName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "attrName")})),
            @FormField(name = "attrValue", title = "${uiLabelMap.OrderOrderQuoteAttributeValue}"),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeQuoteAttribute", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "attrName"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteAttributes {}

    @Form(
        name = "ListQuoteCoefficients",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteCoefficients",
        paginateTarget = "ListQuoteCoefficients",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteCoefficient", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "coeffName", title = "${uiLabelMap.OrderOrderQuoteCoeffName}", widgetStyle = "${styles.link_nav_info_name}", hyperlink = @HyperlinkField(target = "EditQuoteCoefficient", description = "${coeffName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "coeffName")})),
            @FormField(name = "coeffValue", title = "${uiLabelMap.OrderOrderQuoteCoeffValue}"),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeQuoteCoefficient", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "coeffName"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteCoefficients {}

    @Form(
        name = "EditQuoteCoefficient",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteCoefficient",
        defaultMapName = "quoteCoefficient",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteCoefficient", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "coeffName", title = "${uiLabelMap.OrderOrderQuoteCoeffName}", useWhen = "quoteCoefficient==null", text = @TextField),
            @FormField(name = "coeffName", title = "${uiLabelMap.OrderOrderQuoteCoeffName}", useWhen = "quoteCoefficient!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteCoefficient==null", target = "createQuoteCoefficient")
        }
    )
    public interface EditQuoteCoefficient {}

    @Form(
        name = "ManageQuotePrices",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.MULTI,
        target = "autoUpdateQuotePrices?quoteId=${quoteId}",
        listName = "quoteItemAndCostInfos",
        paginateTarget = "ManageQuotePrices",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "custRequestId", title = "${uiLabelMap.OrderRequest}", useWhen = "custRequestId!=null && custRequestItemSeqId!=null", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "requestitem", description = "${custRequestId}-${custRequestItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteItem", description = "${quoteItemSeqId}", parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "averageCost", title = "${uiLabelMap.OrderOrderQuoteAverageCost}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "costToPriceMult", title = "${uiLabelMap.OrderOrderQuoteCostToPrice}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "defaultQuoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteDefaultUnitPrice}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "manualQuoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteManualUnitPrice}", text = @TextField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelected}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ManageQuotePrices {}

    @Form(
        name = "ListQuoteAdjustments",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteAdjustments",
        paginateTarget = "ListQuoteAdjustments",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteAdjustmentId", title = "${uiLabelMap.CommonId}", useWhen = "${groovy:productPromoId==null}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteAdjustment", description = "${quoteAdjustmentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteAdjustmentId")})),
            @FormField(name = "quoteAdjustmentId", title = "${uiLabelMap.CommonId}", useWhen = "${groovy:productPromoId!=null}", display = @DisplayField),
            @FormField(name = "quoteAdjustmentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "OrderAdjustmentType", keyFieldName = "orderAdjustmentTypeId")),
            @FormField(name = "correspondingProductId", display = @DisplayField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.ProductPromo}", display = @DisplayField),
            @FormField(name = "productPromoRuleId", title = "${uiLabelMap.ProductPromoRule}", display = @DisplayField),
            @FormField(name = "productPromoActionSeqId", title = "${uiLabelMap.ProductPromoAction}", display = @DisplayField),
            @FormField(name = "amount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeQuoteAdjustment", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteAdjustmentId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteAdjustments {}

    @Form(
        name = "EditQuoteAdjustment",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteAdjustment",
        defaultMapName = "quoteAdjustment",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteAdjustment", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "productPromoId", hidden = @HiddenField),
            @FormField(name = "productPromoRuleId", hidden = @HiddenField),
            @FormField(name = "productPromoActionSeqId", hidden = @HiddenField),
            @FormField(name = "quoteAdjustmentId", display = @DisplayField),
            @FormField(name = "comments", hidden = @HiddenField),
            @FormField(name = "primaryGeoId", hidden = @HiddenField),
            @FormField(name = "secondaryGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", hidden = @HiddenField),
            @FormField(name = "taxAuthPartyId", hidden = @HiddenField),
            @FormField(name = "sourceReferenceId", hidden = @HiddenField),
            @FormField(name = "customerReferenceId", hidden = @HiddenField),
            @FormField(name = "overrideGlAccountId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "createdDate", hidden = @HiddenField),
            @FormField(name = "createdByUserLogin", hidden = @HiddenField),
            @FormField(name = "quoteAdjustmentTypeId", title = "${uiLabelMap.OrderOrderQuoteAdjustmentType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "OrderAdjustmentType", description = "${description}", keyFieldName = "orderAdjustmentTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteAdjustment==null", target = "createQuoteAdjustment")
        }
    )
    public interface EditQuoteAdjustment {}

    @Form(
        name = "EditQuoteReportMail",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "sendQuoteReportMail",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "emailType", hidden = @HiddenField),
            @FormField(name = "sendTo", text = @TextField),
            @FormField(name = "sendCc", text = @TextField),
            @FormField(name = "note", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditQuoteReportMail {}

    @Form(
        name = "ListQuoteInfo",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        extendsForm = "ListQuoteTerms",
        fields = {
            @FormField(name = "editAction", hidden = @HiddenField),
            @FormField(name = "deleteAction", hidden = @HiddenField)
        }
    )
    public interface ListQuoteInfo {}

    @Form(
        name = "EditQuoteTerm",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteTerm",
        defaultMapName = "quoteTerm",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "activeSubMenuItems", hidden = @HiddenField(value = "${activeSubMenuItem}")),
            @FormField(name = "quoteItemSeqId", tooltip = "${uiLabelMap.OrderQuoteEmpty}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuoteItem", description = "${quoteItemSeqId}", constraints = {@EntityConstraint(name = "quoteId", value = "${quoteId}")}, orderBy = {@EntityOrderBy(fieldName = "quoteItemSeqId")}))),
            @FormField(name = "quoteItemSeqId", useWhen = "quoteItemSeqId!=null", display = @DisplayField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", position = 2, requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", description = "${description}", keyFieldName = "termTypeId"))),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", useWhen = "termTypeId!=null", position = 2, displayEntity = @DisplayEntityField(entityName = "TermType", keyFieldName = "termTypeId", description = "${description}")),
            @FormField(name = "termValue", title = "${uiLabelMap.CommonValue}", text = @TextField),
            @FormField(name = "textValue", title = "${uiLabelMap.CommonText}", position = 2, text = @TextField),
            @FormField(name = "termDays", title = "${uiLabelMap.CommonDays}", text = @TextField),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "uomId")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteTerm==null", target = "createQuoteTerm")
        },
        actions = @FormActions(set = {@SetAction(field = "activeSubMenuItem", fromField = "parameters.activeSubMenuItems")}, entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditQuoteTerm {}

    @Form(
        name = "EditQuoteTermItem",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "updateQuoteTermFromItem",
        defaultMapName = "quoteTerm",
        extendsForm = "EditQuoteTerm",
        headerRowStyle = "header-row",
        altTargets = {
            @AltTarget(useWhen = "quoteTerm==null", target = "createQuoteTermFromItem")
        }
    )
    public interface EditQuoteTermItem {}

    @Form(
        name = "ListQuoteTerms",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteTerms",
        paginateTarget = "ListQuoteTerms",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", display = @DisplayField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "TermType", keyFieldName = "termTypeId", description = "${description}")),
            @FormField(name = "termValue", title = "${uiLabelMap.CommonValue}", display = @DisplayField),
            @FormField(name = "textValue", title = "${uiLabelMap.CommonText}", display = @DisplayField),
            @FormField(name = "termDays", title = "${uiLabelMap.CommonDays}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonCurrency}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${uomId}")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditQuoteTerm", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "termTypeId"), @ParameterDef(paramName = "quoteItemSeqId"), @ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "target", value = "updateQuoteTerm"), @ParameterDef(paramName = "activeSubMenuItems", value = "QuoteTerms")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteQuoteTerm", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "termTypeId"), @ParameterDef(paramName = "quoteItemSeqId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteTerms {}

    @Form(
        name = "ListQuoteTermItem",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        extendsForm = "ListQuoteTerms",
        fields = {
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditQuoteTermItem", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "termTypeId"), @ParameterDef(paramName = "quoteItemSeqId"), @ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "target", value = "updateQuoteTermFromItem"), @ParameterDef(paramName = "activeSubMenuItems", value = "ListQuoteItems")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteQuoteTermFromItem", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "termTypeId"), @ParameterDef(paramName = "quoteItemSeqId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListQuoteTermItem {}

    @Form(
        name = "ListQuoteNotes",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteNotes",
        paginateTarget = "ListQuoteNotes",
        headerRowStyle = "${headerRowStyle}",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteNoteView", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "noteId", hidden = @HiddenField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditQuoteNote", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "noteId")}))
        }
    )
    public interface ListQuoteNotes {}

    @Form(
        name = "ListQuoteNoteInfo",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        extendsForm = "ListQuoteNotes",
        fields = {
            @FormField(name = "editAction", hidden = @HiddenField),
            @FormField(name = "noteInfo", display = @DisplayField)
        }
    )
    public interface ListQuoteNoteInfo {}

    @Form(
        name = "AddOrEditQuoteNote",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        target = "${target}",
        defaultMapName = "quoteNoteData",
        defaultEntityName = "QuoteNoteView",
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "noteId", hidden = @HiddenField),
            @FormField(name = "noteName", text = @TextField),
            @FormField(name = "noteInfo", textarea = @TextareaField(cols = 70, rows = 5)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface AddOrEditQuoteNote {}

    @Form(
        name = "QuoteHeader",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        defaultMapName = "quote",
        fields = {
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.CommonType}", position = 2, displayEntity = @DisplayEntityField(entityName = "QuoteType")),
            @FormField(name = "issueDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStore}", displayEntity = @DisplayEntityField(entityName = "ProductStore", description = "${storeName}")),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", position = 2, displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName}${firstName} ${middleInitial} ${lastName}")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, display = @DisplayField),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}", display = @DisplayField(type = "date")),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}", position = 2, display = @DisplayField(type = "date")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        }
    )
    public interface QuoteHeader {}

    @Form(
        name = "ViewQuoteProfit",
        location = "component://order/widget/ordermgr/QuoteForms.xml",
        type = FormType.LIST,
        listName = "quoteItemAndCostInfos",
        paginateTarget = "ViewQuoteProfit",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "custRequestId", title = "${uiLabelMap.OrderRequest}", useWhen = "custRequestId!=null && custRequestItemSeqId!=null", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "requestitem", description = "${custRequestId}-${custRequestItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteItem", description = "${quoteItemSeqId}", parameters = {@ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "productId", title = "${uiLabelMap.CommonProduct}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "averageCost", title = "${uiLabelMap.OrderOrderQuoteAverageCost}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "profit", title = "${uiLabelMap.OrderOrderQuoteProfit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "percProfit", title = "${uiLabelMap.OrderOrderQuotePercProfit}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField)
        }
    )
    public interface ViewQuoteProfit {}

}
