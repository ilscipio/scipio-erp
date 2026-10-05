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
public class OrdermgrCustRequestForms {

    @Form(
        name = "FindRequests",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "FindRequest",
        defaultMapName = "parameters",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequest", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "fromPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, allowMulti = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CUSTREQ_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", position = 2, dropDown = @DropDownField(allowEmpty = true, allowMulti = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "custRequestName", position = 2, textFind = @TextFindField),
            @FormField(name = "custRequestTypeId", dropDown = @DropDownField(allowEmpty = true, allowMulti = true, entityOptions = @EntityOptions(entityName = "CustRequestType", description = "${description}", keyFieldName = "custRequestTypeId"))),
            @FormField(name = "custRequestCategoryId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustRequestCategory", description = "${description}", keyFieldName = "custRequestCategoryId"))),
            @FormField(name = "priority", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "1", description = "${uiLabelMap.WorkEffortPriorityOne}"), @Option(key = "2", description = "${uiLabelMap.WorkEffortPriorityTwo}"), @Option(key = "3", description = "${uiLabelMap.WorkEffortPriorityThree}"), @Option(key = "4", description = "${uiLabelMap.WorkEffortPriorityFour}"), @Option(key = "5", description = "${uiLabelMap.WorkEffortPriorityFive}"), @Option(key = "6", description = "${uiLabelMap.WorkEffortPrioritySix}"), @Option(key = "7", description = "${uiLabelMap.WorkEffortPrioritySeventh}"), @Option(key = "8", description = "${uiLabelMap.WorkEffortPriorityEight}"), @Option(key = "9", description = "${uiLabelMap.WorkEffortPriorityNine}")})),
            @FormField(name = "billed", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "currencyUomId", ignored = @IgnoredField),
            @FormField(name = "maximumAmountUomId", ignored = @IgnoredField),
            @FormField(name = "fulfillContactMechId", ignored = @IgnoredField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "parentCustRequestId", position = 2, textFind = @TextFindField),
            @FormField(name = "createdDate", dateFind = @DateFindField),
            @FormField(name = "lastModifiedDate", position = 2, dateFind = @DateFindField),
            @FormField(name = "createdByUserLogin", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "lastModifiedByUserLogin", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "closedDateTime", position = 2, dateFind = @DateFindField),
            @FormField(name = "responseRequiredDate", position = 2, dateFind = @DateFindField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductProductStore}", dropDown = @DropDownField(allowEmpty = true, allowMulti = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "reason", position = 2, textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y"))
        },
        sortOrder = @SortOrder()
    )
    public interface FindRequests {}

    @Form(
        name = "ListRequests",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        extendsForm = "ListRequestList",
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListRequests {}

    @Form(
        name = "ListRequestList",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        title = "List of customer requests",
        listName = "custRequests",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        sortFieldParameterName = "custRequestSortField",
        useRowSubmit = true,
        fields = {
            @FormField(name = "custRequestId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "ViewRequest", description = "${custRequestId}", parameters = {@ParameterDef(paramName = "custRequestId")})),
            @FormField(name = "custRequestName", sortField = true, display = @DisplayField),
            @FormField(name = "priority", sortField = true, display = @DisplayField),
            @FormField(name = "responseRequiredDate", sortField = true, display = @DisplayField),
            @FormField(name = "fromPartyId", sortField = true, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${fromPartyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "fromPartyId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false)),
            @FormField(name = "lastModifiedDate", sortField = true, display = @DisplayField),
            @FormField(name = "rejectAction", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "setCustRequestStatus", description = "${uiLabelMap.FormFieldTitle_rejectButton}", linkType = "hidden-form", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "statusId", value = "CRQ_REJECTED")}))
        }
    )
    public interface ListRequestList {}

    @Form(
        name = "ListRequestItems",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "custRequestItems",
        paginateTarget = "requestitems",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "sequenceNum"),
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "requestitem", description = "${custRequestItemSeqId}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "story", textarea = @TextareaField(readonly = true)),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "custRequestResolutionId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", alsoHidden = false)),
            @FormField(name = "addNote", useWhen = "!custRequest.get(\"statusId\").equals(\"CRQ_CANCELLED\")&&!custRequest.get(\"statusId\").equals(\"CRQ_COMPLETED\")", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "requestitemnotes", description = "${uiLabelMap.FormFieldTitle_addNote}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")})),
            @FormField(name = "removeRequestItem", useWhen = "!custRequest.get(\"statusId\").equals(\"CRQ_CANCELLED\")&&!custRequest.get(\"statusId\").equals(\"CRQ_COMPLETED\")", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removerequestitem", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId")}))
        }
    )
    public interface ListRequestItems {}

    @Form(
        name = "OverviewRequestItems",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        extendsForm = "ListRequestItems",
        fields = {
            @FormField(name = "priority", ignored = @IgnoredField),
            @FormField(name = "sequenceNum", ignored = @IgnoredField),
            @FormField(name = "sequenceNumber", ignored = @IgnoredField),
            @FormField(name = "requiredByDate", ignored = @IgnoredField),
            @FormField(name = "selectedAmount", ignored = @IgnoredField),
            @FormField(name = "maximumAmount", ignored = @IgnoredField),
            @FormField(name = "reservStart", ignored = @IgnoredField),
            @FormField(name = "reservLength", ignored = @IgnoredField),
            @FormField(name = "reservPersons", ignored = @IgnoredField),
            @FormField(name = "configId", ignored = @IgnoredField)
        }
    )
    public interface OverviewRequestItems {}

    @Form(
        name = "ListRequestQuoteItems",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "quotes",
        paginateTarget = "RequestItemQuotes",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quoteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditQuoteItemForRequest", description = "${quoteItemSeqId}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "custRequestItemSeqId"), @ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${productId} - ${internalName}")),
            @FormField(name = "quoteItemSeqId", hidden = @HiddenField),
            @FormField(name = "productFeatureId", hidden = @HiddenField),
            @FormField(name = "skillTypeId", hidden = @HiddenField),
            @FormField(name = "deliverableTypeId", hidden = @HiddenField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}"),
            @FormField(name = "comments", hidden = @HiddenField),
            @FormField(name = "uomId", hidden = @HiddenField),
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField)
        }
    )
    public interface ListRequestQuoteItems {}

    @Form(
        name = "ViewRequestCommunicationEvents",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        extendsForm = "ListCommEvents",
        extendsResource = "component://party/widget/partymgr/CommunicationEventForms.xml",
        fields = {
            @FormField(name = "subject", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/partymgr/control/ViewCommunicationEvent", urlMode = UrlMode.INTER_APP, description = "${subject}", linkType = "hidden-form", parameters = {@ParameterDef(paramName = "communicationEventId")}))
        }
    )
    public interface ViewRequestCommunicationEvents {}

    @Form(
        name = "ViewRequestStatus",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestStatus", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestStatusId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}"))
        }
    )
    public interface ViewRequestStatus {}

    @Form(
        name = "ViewRequestRoles",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestParty", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        }
    )
    public interface ViewRequestRoles {}

    @Form(
        name = "ViewRequestWorkEfforts",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "requestWorkEfforts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "workEffortId", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${workEffortName} [${workEffortId}]", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "currentStatusId", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "startDate", display = @DisplayField(type = "date")),
            @FormField(name = "completionDate", display = @DisplayField(type = "date"))
        }
    )
    public interface ViewRequestWorkEfforts {}

    @Form(
        name = "EditCustRequest",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "updaterequest",
        title = "Request",
        defaultMapName = "custRequest",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCustRequest", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "form", hidden = @HiddenField(value = "list")),
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "custRequestId", useWhen = "custRequestId==null", hidden = @HiddenField),
            @FormField(name = "custRequestId", useWhen = "custRequest!=null", display = @DisplayField),
            @FormField(name = "communicationEventId", hidden = @HiddenField(value = "${communicationEvent.communicationEventId}")),
            @FormField(name = "custRequestName", encodeOutput = false, text = @TextField(defaultValue = "${communicationEvent.subject}")),
            @FormField(name = "custRequestTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "CustRequestType", description = "${description}"))),
            @FormField(name = "custRequestCategoryId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustRequestCategory", description = "${description}"))),
            @FormField(name = "statusId", useWhen = "custRequest==null", hidden = @HiddenField(value = "CRQ_SUBMITTED")),
            @FormField(name = "statusId", useWhen = "custRequest!=null", position = 2, dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${custRequest.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "priority", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "1", description = "${uiLabelMap.WorkEffortPriorityOne}"), @Option(key = "2", description = "${uiLabelMap.WorkEffortPriorityTwo}"), @Option(key = "3", description = "${uiLabelMap.WorkEffortPriorityThree}"), @Option(key = "4", description = "${uiLabelMap.WorkEffortPriorityFour}"), @Option(key = "5", description = "${uiLabelMap.WorkEffortPriorityFive}"), @Option(key = "6", description = "${uiLabelMap.WorkEffortPrioritySix}"), @Option(key = "7", description = "${uiLabelMap.WorkEffortPrioritySeventh}"), @Option(key = "8", description = "${uiLabelMap.WorkEffortPriorityEight}"), @Option(key = "9", description = "${uiLabelMap.WorkEffortPriorityNine}")})),
            @FormField(name = "story", useWhen = "custRequest==null", encodeOutput = false, textarea = @TextareaField(rows = 12, defaultValue = "${communicationEvent.content}")),
            @FormField(name = "description", position = 2, encodeOutput = false, textarea = @TextareaField(rows = 12, defaultValue = "${communicationEvent.content}")),
            @FormField(name = "salesChannelEnumId", title = "${uiLabelMap.OrderSalesChannel}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductProductStore}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName}", keyFieldName = "productStoreId"))),
            @FormField(name = "custRequestDate", title = "${uiLabelMap.OrderRequestDate}", dateTime = @DateTimeField),
            @FormField(name = "responseRequiredDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "fromPartyId", title = "${uiLabelMap.OrderRequestingParty}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${communicationEvent.partyIdFrom}")),
            @FormField(name = "fulfillContactMechId", position = 2, lookup = @LookupField(targetFormName = "LookupPreferredContactMech")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "maximumAmountUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createdDate", hidden = @HiddenField),
            @FormField(name = "createdByUserLogin", hidden = @HiddenField),
            @FormField(name = "lastModifiedDate", hidden = @HiddenField),
            @FormField(name = "lastModifiedByUserLogin", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "custRequest==null", target = "createrequest")
        },
        sortOrder = @SortOrder()
    )
    public interface EditCustRequest {}

    @Form(
        name = "EditSmallCustRequest",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        extendsForm = "EditCustRequest",
        fields = {
            @FormField(name = "salesChannelEnumId", ignored = @IgnoredField),
            @FormField(name = "custRequestCategoryId", ignored = @IgnoredField),
            @FormField(name = "maximumAmountUomId", ignored = @IgnoredField),
            @FormField(name = "productStoreId", ignored = @IgnoredField),
            @FormField(name = "fulfillContactMechId", ignored = @IgnoredField),
            @FormField(name = "currencyUomId", ignored = @IgnoredField),
            @FormField(name = "openDateTime", ignored = @IgnoredField),
            @FormField(name = "closedDateTime", ignored = @IgnoredField),
            @FormField(name = "internalComment", ignored = @IgnoredField),
            @FormField(name = "reason", ignored = @IgnoredField)
        }
    )
    public interface EditSmallCustRequest {}

    @Form(
        name = "EditCustRequestItem",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "updaterequestitem",
        defaultMapName = "custRequestItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestItem", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "custRequestResolutionId", hidden = @HiddenField),
            @FormField(name = "statusId", useWhen = "custRequestItem==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CUSTREQ_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", useWhen = "custRequestItem!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${custRequestItem.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "sequenceNum", entryName = "nextSequenceNum", useWhen = "custRequestItem==null", text = @TextField),
            @FormField(name = "sequenceNum", useWhen = "custRequestItem!=null", text = @TextField),
            @FormField(name = "priority", dropDown = @DropDownField(options = {@Option(key = "9"), @Option(key = "8"), @Option(key = "7"), @Option(key = "6"), @Option(key = "5"), @Option(key = "4"), @Option(key = "3"), @Option(key = "2"), @Option(key = "1")})),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "custRequestItem==null", target = "createrequestitem")
        }
    )
    public interface EditCustRequestItem {}

    @Form(
        name = "EditQuoteItemForRequest",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "updateQuoteItemForRequest",
        defaultMapName = "quoteItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuoteItem", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "quoteId", display = @DisplayField),
            @FormField(name = "quoteItemSeqId", display = @DisplayField),
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "isPromo", hidden = @HiddenField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProductAndPrice")),
            @FormField(name = "productFeatureId", lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "deliverableTypeId", title = "${uiLabelMap.OrderOrderQuoteDeliverableTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DeliverableType", description = "${description}", keyFieldName = "deliverableTypeId"))),
            @FormField(name = "skillTypeId", title = "${uiLabelMap.OrderOrderQuoteSkillTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", keyFieldName = "skillTypeId"))),
            @FormField(name = "uomId", title = "${uiLabelMap.OrderOrderQuoteUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "quantity", mapName = "parameters", useWhen = "quoteItem==null", text = @TextField),
            @FormField(name = "selectedAmount", mapName = "parameters", useWhen = "quoteItem==null", text = @TextField),
            @FormField(name = "quoteUnitPrice", title = "${uiLabelMap.OrderOrderQuoteUnitPrice}"),
            @FormField(name = "comments", mapName = "parameters", useWhen = "quoteItem==null", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quoteItem==null", target = "createQuoteItemForRequest")
        }
    )
    public interface EditQuoteItemForRequest {}

    @Form(
        name = "CreateQuoteAndQuoteItemForRequest",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "createQuoteAndQuoteItemForRequest",
        defaultMapName = "quoteItem",
        extendsForm = "EditQuoteItemForRequest",
        headerRowStyle = "header-row"
    )
    public interface CreateQuoteAndQuoteItemForRequest {}

    @Form(
        name = "ListRequestItemNotes",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "notes",
        paginateTarget = "RequestItemNotes",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestItemNoteView", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "noteId", hidden = @HiddenField),
            @FormField(name = "noteName", hidden = @HiddenField),
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "name", display = @DisplayField(description = "${firstName} ${lastName}")),
            @FormField(name = "firstName", hidden = @HiddenField),
            @FormField(name = "lastName", hidden = @HiddenField)
        }
    )
    public interface ListRequestItemNotes {}

    @Form(
        name = "ListRequestItemWorkEffortReq",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "requirements",
        paginateTarget = "RequestItemRequirements",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Requirement", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "requirementId", hidden = @HiddenField)
        }
    )
    public interface ListRequestItemWorkEffortReq {}

    @Form(
        name = "EditRequestItemNote",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "createrequestitemnote",
        defaultMapName = "quoteItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "note", textarea = @TextareaField(rows = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditRequestItemNote {}

    @Form(
        name = "ListRequestRoles",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        target = "updateCustRequestParty",
        listName = "custRequestParties",
        paginateTarget = "RequestRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustRequestParty", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "name", entryName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "expireCustRequestParty", description = "${uiLabelMap.CommonExpire}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListRequestRoles {}

    @Form(
        name = "EditRequestRole",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "createCustRequestParty",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", dropDown = @DropDownField(options = {@Option(key = "REQ_REQUESTER", description = "${uiLabelMap.WorkEffortRequestingParty}"), @Option(key = "AGENT", description = "${uiLabelMap.OrderAgent}"), @Option(key = "REQ_TAKER", description = "${uiLabelMap.WorkEffortRequestTaker}"), @Option(key = "REQ_MANAGER", description = "${uiLabelMap.WorkEffortRequestManager}")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditRequestRole {}

    @Form(
        name = "ListRequestItemRequirements",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "custRequestRequirements",
        paginateTarget = "RequestItemRequirements",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Requirement", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "requirementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditRequirement", description = "${requirementId}", parameters = {@ParameterDef(paramName = "requirementId")}))
        }
    )
    public interface ListRequestItemRequirements {}

    @Form(
        name = "ListCustRequestItemWorkEfforts",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "workEffortId", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "/workeffort/control/EditWorkEffort", description = "[${workEffortId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId")}))),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCustRequestItemWorkEffort", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListCustRequestItemWorkEfforts {}

    @Form(
        name = "AddCustRequestItemWorkEffort",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        target = "createCustRequestItemWorkEffort",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestItemSeqId", hidden = @HiddenField),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "workEffort==null", target = "createworkeffort")
        }
    )
    public interface AddCustRequestItemWorkEffort {}

    @Form(
        name = "requestInfo",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        title = "request information",
        defaultMapName = "custRequest",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "CustRequestType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "fromPartyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyNameResultTo.fullName} [${custRequest.fromPartyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "custRequest.fromPartyId")})),
            @FormField(name = "custRequestName", title = "${uiLabelMap.CommonName}", encodeOutput = false, display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductProductStore}", displayEntity = @DisplayEntityField(entityName = "ProductStore", description = "${storeName}")),
            @FormField(name = "internalComment", title = "${uiLabelMap.CommonInternalComment}", display = @DisplayField),
            @FormField(name = "reason", title = "${uiLabelMap.CommonReason}", display = @DisplayField),
            @FormField(name = "custRequestDate", title = "${uiLabelMap.OrderRequestDate}", display = @DisplayField),
            @FormField(name = "createdDate", title = "${uiLabelMap.OrderRequestCreatedDate}", display = @DisplayField),
            @FormField(name = "lastModifiedDate", title = "${uiLabelMap.OrderRequestLastModifiedDate}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "lookupPartyId", fromField = "custRequest.fromPartyId", defaultValue = "_NA_")}, service = {@ServiceAction(serviceName = "getPartyNameForDate", resultMapName = "partyNameResultTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "lookupPartyId"), @FieldMap(fieldName = "compareDate", fromField = "custRequest.custRequestDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})})
    )
    public interface requestInfo {}

    @Form(
        name = "AddCustRequestContent",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.UPLOAD,
        target = "createCustRequestContent?custRequestId=${parameters.custRequestId}",
        focusFieldName = "contentId",
        defaultMapName = "content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "custRequestId", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "contentId", title = "Existing Content Id", lookup = @LookupField(targetFormName = "LookupTreeContent")),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "contentId==void", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "contentId!=void", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${content.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "dataResourceName", title = "${uiLabelMap.CommonUpload}*", file = @FileField),
            @FormField(name = "contentIdFrom", title = "${uiLabelMap.ContentCompDocParentContentId}", lookup = @LookupField(targetFormName = "LookupDetailContentTree")),
            @FormField(name = "createAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "custRequestId", fromField = "parameters.custRequestId")}, entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false), @EntityOneAction(entityName = "DataResource", valueField = "dataResource", autoFieldMap = false)})
    )
    public interface AddCustRequestContent {}

    @Form(
        name = "ListCustRequestContent",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "custRequestAndContents",
        paginateTarget = "EditCustRequestContent",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "contentId", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "/content/control/ViewSimpleContent", description = "[${contentId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "mimeTypeId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", useWhen = "activeSubMenuItem!=void&&activeSubMenuItem.equals(\"custRequestContent\")", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCustRequestContent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "custRequestId"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListCustRequestContent {}

    @Form(
        name = "EditCustReqStatusId",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "statusGroup", dropDown = @DropDownField(options = {@Option(key = "OPEN", description = "Open"), @Option(key = "COMPLETED", description = "Completed"), @Option(key = "CANCELLED", description = "Cancelled")})),
            @FormField(name = "otherContacts", dropDown = @DropDownField(options = {@Option(key = "Y", description = "Yes"), @Option(key = "N", description = "No")})),
            @FormField(name = "saveAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCustReqStatusId {}

    @Form(
        name = "ListCustRequests",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        target = "updaterequest",
        listName = "custRequests",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", encodeOutput = false, hyperlink = @HyperlinkField(target = "ViewRequest", description = "${custRequestName} [${custRequestId}]", parameters = {@ParameterDef(paramName = "custRequestId")})),
            @FormField(name = "custRequestDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "fromPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName} [${fromPartyId}])")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "priority", useWhen = "statusGroup!=\"OPEN\"", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "currentStatusId", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "estimatedStartDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskPlanStartDate}", display = @DisplayField(type = "date")),
            @FormField(name = "estimatedCompletionDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskPlanEndDate}", display = @DisplayField(type = "date")),
            @FormField(name = "actualStartDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskActStartDate}", display = @DisplayField(type = "date")),
            @FormField(name = "actualCompletionDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskActEndDate}", display = @DisplayField(type = "date")),
            @FormField(name = "plannedHours", mapName = "taskResult.taskInfo", display = @DisplayField),
            @FormField(name = "actualHours", mapName = "taskResult.taskInfo", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "getProjectTaskExists", value = "true", type = "Boolean")})
    )
    public interface ListCustRequests {}

    @Form(
        name = "ListMyCustRequests",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        target = "updaterequest",
        listName = "custRequests",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "custRequestId", hidden = @HiddenField),
            @FormField(name = "custRequestName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", encodeOutput = false, hyperlink = @HyperlinkField(target = "ViewRequest", description = "${custRequestName} [${custRequestId}]", parameters = {@ParameterDef(paramName = "custRequestId")})),
            @FormField(name = "custRequestDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "priority", useWhen = "!statusId.equals(\"CRQ_COMPLETED\")", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "1", description = "${uiLabelMap.WorkEffortPriorityOne}"), @Option(key = "2", description = "${uiLabelMap.WorkEffortPriorityTwo}"), @Option(key = "3", description = "${uiLabelMap.WorkEffortPriorityThree}"), @Option(key = "4", description = "${uiLabelMap.WorkEffortPriorityFour}"), @Option(key = "5", description = "${uiLabelMap.WorkEffortPriorityFive}"), @Option(key = "6", description = "${uiLabelMap.WorkEffortPrioritySix}"), @Option(key = "7", description = "${uiLabelMap.WorkEffortPrioritySeventh}"), @Option(key = "8", description = "${uiLabelMap.WorkEffortPriorityEight}"), @Option(key = "9", description = "${uiLabelMap.WorkEffortPriorityNine}")})),
            @FormField(name = "priority", useWhen = "statusId.equals(\"CRQ_COMPLETED\")", display = @DisplayField),
            @FormField(name = "updateAction", useWhen = "!statusId.equals(\"CRQ_COMPLETED\")", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "currentStatusId", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "estimatedStartDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskPlanStartDate}", display = @DisplayField(type = "date")),
            @FormField(name = "estimatedCompletionDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskPlanEndDate}", display = @DisplayField(type = "date")),
            @FormField(name = "actualStartDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskActStartDate}", display = @DisplayField(type = "date")),
            @FormField(name = "actualCompletionDate", mapName = "taskResult.taskInfo", title = "${uiLabelMap.MyPortalTaskActEndDate}", display = @DisplayField(type = "date")),
            @FormField(name = "plannedHours", mapName = "taskResult.taskInfo", display = @DisplayField),
            @FormField(name = "actualHours", mapName = "taskResult.taskInfo", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "getProjectTaskExists", value = "true", type = "Boolean")})
    )
    public interface ListMyCustRequests {}

    @Form(
        name = "EditRequestCustomer",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        extendsForm = "EditSmallCustRequest",
        fields = {
            @FormField(name = "story", title = "${uiLabelMap.CommonContent}", textarea = @TextareaField(rows = 15)),
            @FormField(name = "fromPartyId", hidden = @HiddenField(value = "${userLogin.partyId}")),
            @FormField(name = "custRequestDate", ignored = @IgnoredField),
            @FormField(name = "description", ignored = @IgnoredField),
            @FormField(name = "custRequestName", ignored = @IgnoredField),
            @FormField(name = "subject", parameterName = "custRequestName", text = @TextField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "priority"), @SortField(name = "responseRequiredDate"), @SortField(name = "subject"), @SortField(name = "story"), @SortField(name = "submit")})
    )
    public interface EditRequestCustomer {}

    @Form(
        name = "EditCustRetStatusId",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "statusId", entryName = "attributeMap.statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_RETURN_STTS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "saveAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCustRetStatusId {}

    @Form(
        name = "ListReturns",
        location = "component://order/widget/ordermgr/CustRequestForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", title = "${uiLabelMap.OrderReturnId}", display = @DisplayField),
            @FormField(name = "entryDate", title = "${uiLabelMap.OrderEntryDate}", display = @DisplayField),
            @FormField(name = "destinationFacilityId", title = "${uiLabelMap.OrderReturnDestinationFacility}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", display = @DisplayField)
        }
    )
    public interface ListReturns {}

}
