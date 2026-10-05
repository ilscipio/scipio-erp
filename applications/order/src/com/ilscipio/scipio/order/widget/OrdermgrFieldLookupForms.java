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
public class OrdermgrFieldLookupForms {

    @Form(
        name = "lookupOrderHeader",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupOrderHeader",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeader", mapName = "parameters", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "orderHeaderId", textFind = @TextFindField),
            @FormField(name = "orderTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "OrderType", description = "${description}", keyFieldName = "orderTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "salesChannelEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}))),
            @FormField(name = "productStoreId", lookup = @LookupField(targetFormName = "/marketing/control/LookupProductStore")),
            @FormField(name = "currencyUom", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupOrderHeader {}

    @Form(
        name = "listLookupOrderHeader",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupOrderHeader",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeader", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${orderId}')", urlMode = UrlMode.PLAIN, description = "${orderId}")),
            @FormField(name = "orderTypeId", displayEntity = @DisplayEntityField(entityName = "OrderType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "OrderHeader"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupOrderHeader {}

    @Form(
        name = "lookupOrderHeaderAndShipInfo",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupOrderHeaderAndShipInfo",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeaderAndShipGroups", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "orderId", textFind = @TextFindField),
            @FormField(name = "orderTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "OrderType", description = "${description}"))),
            @FormField(name = "partyId", textFind = @TextFindField),
            @FormField(name = "shipmentMethodTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentMethodType", description = "${description}"))),
            @FormField(name = "carrierPartyId", textFind = @TextFindField),
            @FormField(name = "shipAfterDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "shipByDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "city", textFind = @TextFindField),
            @FormField(name = "postalCode", textFind = @TextFindField),
            @FormField(name = "countryGeoId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}))),
            @FormField(name = "stateProvinceGeoId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "STATE")}))),
            @FormField(name = "grandTotal", rangeFind = @RangeFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupOrderHeaderAndShipInfo {}

    @Form(
        name = "listLookupOrderHeaderAndShipInfo",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupOrderHeaderAndShipInfo",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeaderAndShipGroups", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${orderId}')", urlMode = UrlMode.PLAIN, description = "${orderId}", alsoHidden = false)),
            @FormField(name = "orderTypeId", displayEntity = @DisplayEntityField(entityName = "OrderType")),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "shipmentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType")),
            @FormField(name = "carrierPartyId", display = @DisplayField),
            @FormField(name = "shipAfterDate", display = @DisplayField),
            @FormField(name = "shipByDate", display = @DisplayField),
            @FormField(name = "city", display = @DisplayField),
            @FormField(name = "postalCode", display = @DisplayField),
            @FormField(name = "countryGeoId", display = @DisplayField),
            @FormField(name = "stateProvinceGeoId", display = @DisplayField),
            @FormField(name = "grandTotal", display = @DisplayField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "brandName", display = @DisplayField),
            @FormField(name = "internalName", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "OrderHeaderAndShipGroupsByProduct"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupOrderHeaderAndShipInfo {}

    @Form(
        name = "lookupPurchaseOrderHeaderAndShipInfo",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupPurchaseOrderHeaderAndShipInfo",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeaderAndShipGroups", mapName = "parameters", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "orderId", textFind = @TextFindField),
            @FormField(name = "orderTypeId", hidden = @HiddenField(value = "PURCHASE_ORDER")),
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "SHIP_FROM_VENDOR")),
            @FormField(name = "partyId", textFind = @TextFindField),
            @FormField(name = "shipmentMethodTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentMethodType", description = "${description}"))),
            @FormField(name = "carrierPartyId", textFind = @TextFindField),
            @FormField(name = "shipAfterDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "shipByDate", dateFind = @DateFindField(type = "date")),
            @FormField(name = "city", textFind = @TextFindField),
            @FormField(name = "postalCode", textFind = @TextFindField),
            @FormField(name = "countryGeoId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}))),
            @FormField(name = "stateProvinceGeoId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "STATE")}))),
            @FormField(name = "grandTotal", rangeFind = @RangeFindField),
            @FormField(name = "productId", textFind = @TextFindField),
            @FormField(name = "brandName", textFind = @TextFindField),
            @FormField(name = "internalName", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPurchaseOrderHeaderAndShipInfo {}

    @Form(
        name = "lookupCustRequest",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupCustRequest",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequest", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "custRequestTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustRequestType", description = "${description}", keyFieldName = "custRequestTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CUSTREQ_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "REQ_REQUESTER", description = "${uiLabelMap.WorkEffortRequestingParty}"), @Option(key = "AGENT", description = "${uiLabelMap.OrderAgent}"), @Option(key = "REQ_TAKER", description = "${uiLabelMap.WorkEffortRequestTaker}"), @Option(key = "REQ_MANAGER", description = "${uiLabelMap.WorkEffortRequestManager}")})),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "custRequestCategoryId", hidden = @HiddenField),
            @FormField(name = "priority", hidden = @HiddenField),
            @FormField(name = "description", hidden = @HiddenField),
            @FormField(name = "createdDate", hidden = @HiddenField),
            @FormField(name = "createdByUserLogin", hidden = @HiddenField),
            @FormField(name = "lastModifiedDate", hidden = @HiddenField),
            @FormField(name = "lastModifiedByUserLogin", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCustRequest {}

    @Form(
        name = "listLookupCustRequest",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCustRequest",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequest", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "custRequestId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${custRequestId}')", urlMode = UrlMode.PLAIN, description = "${custRequestId}", alsoHidden = false)),
            @FormField(name = "custRequestName", display = @DisplayField),
            @FormField(name = "priority", display = @DisplayField),
            @FormField(name = "responseRequiredDate", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false)),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CustRequest"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupCustRequest {}

    @Form(
        name = "lookupCustRequestItem",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupCustRequestItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestItem", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CUSTREQ_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "custRequestResolutionId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustRequestResolution", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "priority", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCustRequestItem {}

    @Form(
        name = "listLookupCustRequestItem",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCustRequestItem",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestItem", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "custRequestId", display = @DisplayField),
            @FormField(name = "custRequestItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${custRequestItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${custRequestItemSeqId}", alsoHidden = false)),
            @FormField(name = "priority", display = @DisplayField),
            @FormField(name = "custRequestResolutionId", displayEntity = @DisplayEntityField(entityName = "CustRequestResolution", alsoHidden = false)),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", alsoHidden = false)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CustRequestItem"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupCustRequestItem {}

    @Form(
        name = "lookupQuote",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupQuote",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Quote", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "quoteId", title = "${uiLabelMap.OrderOrderQuoteId}"),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.OrderOrderQuoteTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuoteType", description = "${description}", keyFieldName = "quoteTypeId"))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}"),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "QUOTE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "productStoreId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", keyFieldName = "productStoreId"))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}"),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}"),
            @FormField(name = "quoteName", title = "${uiLabelMap.OrderOrderQuoteName}"),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupQuote {}

    @Form(
        name = "listLookupQuote",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupQuote",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Quote", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quoteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${quoteId}')", urlMode = UrlMode.PLAIN, description = "${quoteId}", alsoHidden = false)),
            @FormField(name = "quoteTypeId", title = "${uiLabelMap.OrderOrderQuoteTypeId}", displayEntity = @DisplayEntityField(entityName = "QuoteType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false)),
            @FormField(name = "partyId"),
            @FormField(name = "quoteName", title = "${uiLabelMap.OrderOrderQuoteName}"),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}"),
            @FormField(name = "issueDate", title = "${uiLabelMap.OrderOrderQuoteIssueDate}"),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}"),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}"),
            @FormField(name = "validFromDate", title = "${uiLabelMap.CommonValidFromDate}"),
            @FormField(name = "validThruDate", title = "${uiLabelMap.CommonValidThruDate}")
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Quote"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupQuote {}

    @Form(
        name = "lookupQuoteItem",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupQuoteItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteItem", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "isPromo", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", display = @DisplayField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productFeatureId", lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "deliverableTypeId", title = "${uiLabelMap.OrderOrderQuoteDeliverableTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DeliverableType", description = "${description}", keyFieldName = "deliverableTypeId"))),
            @FormField(name = "skillTypeId", title = "${uiLabelMap.OrderOrderQuoteSkillTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", keyFieldName = "skillTypeId"))),
            @FormField(name = "uomId", title = "${uiLabelMap.OrderOrderQuoteUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId"))),
            @FormField(name = "workEffortId", title = "${uiLabelMap.OrderOrderQuoteWorkEffortId}"),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}"),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}"),
            @FormField(name = "estimatedDeliveryDate", title = "${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}"),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}"),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupQuoteItem {}

    @Form(
        name = "listLookupQuoteItem",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupQuoteItem",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quoteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${quoteId}')", urlMode = UrlMode.PLAIN, description = "${quoteId}", alsoHidden = false)),
            @FormField(name = "quoteId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", hidden = @HiddenField),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", useWhen = "${groovy:isPromo==null}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${quoteItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${quoteItemSeqId}", alsoHidden = false)),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", useWhen = "${groovy: 'N'.equals(isPromo)}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${quoteItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${quoteItemSeqId}", alsoHidden = false)),
            @FormField(name = "quoteItemSeqId", title = "${uiLabelMap.OrderOrderQuoteItemSeqId}", useWhen = "${groovy: 'Y'.equals(isPromo)}", display = @DisplayField),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "workEffortId", title = "${uiLabelMap.OrderOrderQuoteWorkEffortId}"),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}"),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}"),
            @FormField(name = "estimatedDeliveryDate", title = "${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}"),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "skillTypeId", hidden = @HiddenField),
            @FormField(name = "deliverableTypeId", hidden = @HiddenField),
            @FormField(name = "comments", hidden = @HiddenField),
            @FormField(name = "uomId", hidden = @HiddenField),
            @FormField(name = "custRequestId", title = "${uiLabelMap.CommonViewRequest}", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "QuoteItem"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupQuoteItem {}

    @Form(
        name = "lookupRequirement",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupRequirement",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Requirement", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "requirementId", textFind = @TextFindField),
            @FormField(name = "requirementTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RequirementType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.OrderRequirementStatusId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "REQUIREMENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "requirementStartDate", title = "${uiLabelMap.OrderRequirementStartDate}", dateFind = @DateFindField(type = "date")),
            @FormField(name = "requiredByDate", title = "${uiLabelMap.OrderRequirementByDate}", dateFind = @DateFindField(type = "date")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupRequirement {}

    @Form(
        name = "listLookupRequirement",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupRequirement",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Requirement", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${requirementId}')", urlMode = UrlMode.PLAIN, description = "${requirementId}")),
            @FormField(name = "requirementTypeId", displayEntity = @DisplayEntityField(entityName = "RequirementType")),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "requirementStartDate", title = "${uiLabelMap.OrderRequirementStartDate}", display = @DisplayField),
            @FormField(name = "requiredByDate", title = "${uiLabelMap.OrderRequirementByDate}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Requirement"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupRequirement {}

    @Form(
        name = "lookupShoppingList",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        target = "LookupShoppingList",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShoppingList", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "shoppingListId", textFind = @TextFindField),
            @FormField(name = "shoppingListTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShoppingListType", description = "${description}"))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupShoppingList {}

    @Form(
        name = "listLookupShoppingList",
        location = "component://order/widget/ordermgr/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupShoppingList",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShoppingList", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "shoppingListId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${shoppingListId}')", urlMode = UrlMode.PLAIN, description = "${shoppingListId}")),
            @FormField(name = "shoppingListTypeId", displayEntity = @DisplayEntityField(entityName = "ShoppingListType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ShoppingList"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupShoppingList {}

}
