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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrdermgrCustRequestScreens {

    @Screen(name = "FindRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderFindRequests")
    @Action(type = ActionType.SET, field = "entityName", value = "CustRequest")
    @Action(type = ActionType.SET, field = "asm_multipleSelectForm", value = "FindRequests")
    @Action(type = ActionType.SET, field = "asm_asmListItemPercentOfForm", value = "110")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "commonPleaseSelectText", resource = "CommonUiLabels", property = "CommonPleaseSelect")
    @Action(type = ActionType.SET, field = "custRequestType.asm_multipleSelect", value = "FindRequests_custRequestTypeId")
    @Action(type = ActionType.SET, field = "custRequestType.asm_sortable", value = "true")
    @Action(type = ActionType.SET, field = "custRequestType.asm_title", fromField = "commonPleaseSelectText")
    @Action(type = ActionType.SET, field = "statusId.asm_multipleSelect", value = "FindRequests_statusId")
    @Action(type = ActionType.SET, field = "statusId.asm_sortable", value = "true")
    @Action(type = ActionType.SET, field = "statusId.asm_title", fromField = "commonPleaseSelectText")
    @Action(type = ActionType.SET, field = "productStoreId.asm_multipleSelect", value = "FindRequests_productStoreId")
    @Action(type = ActionType.SET, field = "productStoreId.asm_sortable", value = "true")
    @Action(type = ActionType.SET, field = "productStoreId.asm_title", fromField = "commonPleaseSelectText")
    @Action(type = ActionType.SET, field = "salesChannelEnumId.asm_multipleSelect", value = "FindRequests_salesChannelEnumId")
    @Action(type = ActionType.SET, field = "salesChannelEnumId.asm_sortable", value = "true")
    @Action(type = ActionType.SET, field = "salesChannelEnumId.asm_title", fromField = "commonPleaseSelectText")
    @Action(type = ActionType.SET, field = "asm_listField[]", fromField = "custRequestType")
    @Action(type = ActionType.SET, field = "asm_listField[]", fromField = "statusId")
    @Action(type = ActionType.SET, field = "asm_listField[]", fromField = "productStoreId")
    @Action(type = ActionType.SET, field = "asm_listField[]", fromField = "salesChannelEnumId")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setMultipleSelectJsList.ftl"
                    ),
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})),
                @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}))})})
        }
    )
    public interface FindRequest {}

    @Screen(name = "ViewCustRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId", defaultValue = "${parameters.id}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "CustRequestType", toValueField = "custRequestType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "CurrencyUom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "FulfillContactMech", toValueField = "fulfillContactMech")
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestAndWorkEffort", list = "requestWorkEfforts", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "parameters.custRequestId")})
    @Action(type = ActionType.GET_RELATED, valueField = "custRequest", relationName = "CustRequestParty", list = "requestParties")
    @Action(type = ActionType.SET, field = "orderBy[]", value = "sequenceNum")
    @Action(type = ActionType.GET_RELATED, valueField = "custRequest", relationName = "CustRequestItem", list = "custRequestItems", orderByList = "orderBy")
    @Action(type = ActionType.ENTITY_AND, entityName = "CommunicationEventAndCustRequest", list = "commEvents", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "parameters.custRequestId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestItemNoteView", list = "notes", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "parameters.custRequestId")}, orderBy = {"custRequestItemSeqId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestContent", list = "custRequestContents", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequestAndContent", list = "custRequestAndContents", conditions = {@ConditionExpr(fieldName = "custRequestId", fromField = "custRequestId"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, orderBy = {"fromDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestItemNoteView", list = "notes", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTAINER, style = "clear", position = 3)}, screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleRequestItems}", includeForms = {@IncludeForm(name = "OverviewRequestItems", location = "component://order/widget/ordermgr/CustRequestForms.xml")}, position = 4), @Screenlet(title = "${uiLabelMap.PageTitleRequestItemNotes}", includeForms = {@IncludeForm(name = "ListRequestItemNotes", location = "component://order/widget/ordermgr/CustRequestForms.xml")}, position = 5)}, containers = {@Container(style = "${styles.grid_large}9", screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequest} ${custRequest.custRequestId} ${uiLabelMap.CommonInformation}", includeForms = {
                    @IncludeForm(name = "requestInfo", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}, position = 0), @Container(style = "${styles.grid_large}12", screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderRequestRoles}", includeForms = {
                    @IncludeForm(name = "ViewRequestRoles", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}, position = 1), @Container(style = "${styles.grid_large}12", htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/request/requestContactMech.ftl", position = 1)}, screenlets = {@ScreenletNested(title = "${uiLabelMap.OrderCustRequestStatusList}", navigationFormName = "ViewRequest", includeForms = {
                    @IncludeForm(name = "ViewRequestStatus", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, position = 0), @ScreenletNested(title = "${uiLabelMap.PartyListCommunicationEvents}", navigationFormName = "ViewRequest", includeForms = {
                    @IncludeForm(name = "ViewRequestCommunicationEvents", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, position = 2), @ScreenletNested(title = "${uiLabelMap.WorkEffortWorkEfforts}", navigationFormName = "ViewRequest", includeForms = {
                    @IncludeForm(name = "ViewRequestWorkEfforts", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, position = 3), @ScreenletNested(title = "${uiLabelMap.CommonContent}", navigationFormName = "ViewRequest", includeForms = {
                    @IncludeForm(name = "ListCustRequestContent", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, position = 4)}, position = 2)}))
    public interface ViewCustRequest {}

    @Screen(name = "ViewRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewRequest")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewRequest")
    @Action(type = ActionType.SET, field = "showRequestManagementLinks", value = "Y")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewCustRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml"
            )})
        }
    )
    public interface ViewRequest {}

    @Screen(name = "EditRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRequest")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.SET, field = "statusId", fromField = "custRequest.statusId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "currentStatus")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.custRequest ? 'OrderRequest' : 'OrderNewRequest'}")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"parameters.small", "equals", "Y"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditSmallCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditCustRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                    )}))})})
        }
    )
    public interface EditRequest {}

    @Screen(name = "EditRequestCustomer", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.SET, field = "statusId", fromField = "custRequest.statusId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "currentStatus")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.OrderRequest}", includeForms = {
                    @IncludeForm(name = "EditRequestCustomer", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})})
        }
    )
    public interface EditRequestCustomer {}

    @Screen(name = "RequestRoles", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestroles")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestParty", list = "custRequestParties", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestId")})
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRequestRoles", location = "component://order/widget/ordermgr/CustRequestForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditRequestRoles}", name = "EditRequestRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditRequestRole", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, position = 0)})
        }
    )
    public interface RequestRoles {}

    @Screen(name = "RequestItems", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitems")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestItem", list = "custRequestItems", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestId")}, orderBy = {"sequenceNum", "custRequestItemSeqId"})
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListRequestItems", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequestItem}", style = "${styles.link_nav} ${styles.action_add}", target = "requestitem"
                )})})
        }
    )
    public interface RequestItems {}

    @Screen(name = "EditRequestItem", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRequestItem")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitem")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.SET, field = "statusId", fromField = "custRequestItem.statusId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "currentStatus")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/request/GetNextSequenceNum.groovy")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "EditCustRequestItem", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/CopyRequestItem.ftl", position = 2
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequestItem}", style = "${styles.link_nav} ${styles.action_add}", target = "requestitem"
                    )}, position = 0)})})
        }
    )
    public interface EditRequestItem {}

    @Screen(name = "RequestItemNotes", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestItemNotes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitemnotes")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/request/RequestItemNotes.groovy")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "ListRequestItemNotes", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1
                ),
                @IncludeForm(name = "EditRequestItemNote", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 2
            )}, htmlTemplates = {
                @HtmlTemplate(location = "component://order/webapp/ordermgr/request/requestitemnotes.ftl", position = 0
            )})})
        }
    )
    public interface RequestItemNotes {}

    @Screen(name = "RequestItemRequirements", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestItemRequirements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "workeffortrequirements")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "RequirementCustRequestView", list = "custRequestRequirements", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestItem.custRequestId"), @FieldMap(fieldName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")})
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "ListRequestItemRequirements", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequirement}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRequirement"
                    )}, position = 0)})})
        }
    )
    public interface RequestItemRequirements {}

    @Screen(name = "RequestItemQuotes", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestItemQuotes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitemquotes")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteItem", list = "quotes", fieldMaps = {@FieldMap(fieldName = "custRequestId", fromField = "custRequestItem.custRequestId"), @FieldMap(fieldName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")})
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/request/SetRequestQuote.groovy")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "ListRequestQuoteItems", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/QuoteLinks.ftl", position = 0
                )})})
        }
    )
    public interface RequestItemQuotes {}

    @Screen(name = "EditQuoteItemForRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditQuoteItemForCustRequest")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitemquotes")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteItem", valueField = "quoteItem")
    @Action(type = ActionType.SET, field = "parameters.quantity", fromField = "custRequestItem.quantity")
    @Action(type = ActionType.SET, field = "parameters.selectedAmount", fromField = "custRequestItem.selectedAmount")
    @Action(type = ActionType.SET, field = "parameters.comments", fromField = "custRequestItem.story")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "EditQuoteItemForRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})})
        }
    )
    public interface EditQuoteItemForRequest {}

    @Screen(name = "CreateQuoteAndQuoteItemForRequest", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateQuoteForCustRequest")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "requestitemquotes")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "RequestItemSideBar")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteItem", valueField = "quoteItem")
    @Action(type = ActionType.SET, field = "parameters.quantity", fromField = "custRequestItem.quantity")
    @Action(type = ActionType.SET, field = "parameters.selectedAmount", fromField = "custRequestItem.selectedAmount")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", includeForms = {
                    @IncludeForm(name = "CreateQuoteAndQuoteItemForRequest", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})})
        }
    )
    public interface CreateQuoteAndQuoteItemForRequest {}

    @Screen(name = "EditRequestItemWorkEfforts", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRequestItemWorkEffort")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "task")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "custRequestItemSeqId", fromField = "parameters.custRequestItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequestItem", valueField = "custRequestItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "CustRequestItemWorkEffort", list = "custRequestItemWorkEffortList", fieldMaps = {@FieldMap(fieldName = "custRequestId"), @FieldMap(fieldName = "custRequestItemSeqId")}, orderBy = {"workEffortId"})
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCustRequestItemWorkEfforts", location = "component://order/widget/ordermgr/CustRequestForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${custRequestItem.custRequestItemSeqId} ${custRequestItem.description}", name = "AddCustRequestItemWorkEffortPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddCustRequestItemWorkEffort", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequestItem}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRequestItem"
                )}, position = 0)})
        }
    )
    public interface EditRequestItemWorkEfforts {}

    @Screen(name = "ViewRequestItemInfo", location = "component://order/widget/ordermgr/CustRequestScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"requestItems"})}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/request/ViewRequestItemInfo.ftl")}))
    public interface ViewRequestItemInfo {}

    @Screen(name = "EditCustRequestContent", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRequestContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "custRequestContent")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequestAndContent", list = "custRequestAndContents", conditions = {@ConditionExpr(fieldName = "custRequestId", fromField = "custRequestId"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, orderBy = {"fromDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "StatusItem", list = "statusItems", conditions = {@ConditionExpr(fieldName = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentType", list = "contentTypes", orderBy = {"description"})
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(id = "contentWrapper", includeForms = {
                    @IncludeForm(name = "ListCustRequestContent", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/AddCustRequestContent.ftl", position = 0
                )})})
        }
    )
    public interface EditCustRequestContent {}

    @Screen(name = "IncomingCustRequests", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "custRequestSortField", fromField = "parameters.custRequestSortField", defaultValue = "-custRequestDate")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustReqAndTypeAndPartyRel", list = "custRequests", conditions = {@ConditionExpr(fieldName = "statusId", operator = "equals", value = "CRQ_SUBMITTED")}, orderBy = {"${custRequestSortField}"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"custRequests"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderIncomingCustRequests}", includeForms = {@IncludeForm(name = "ListRequestList", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "RequestScreenletMenu", location = "component://order/widget/ordermgr/OrderMenus.xml", position = 0)})}))
    public interface IncomingCustRequests {}

    @Screen(name = "ListCustRequests", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "fromPartyId", fromField = "userLogin.partyId")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"otherContacts", "equals", "Y"})}), actions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetMyCompany.groovy"), @Action(type = ActionType.SET, field = "fromPartyId"), @Action(type = ActionType.SET, field = "notFromPartyId", fromField = "userLogin.partyId"), @Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderOpenCompanyCustomerRequests")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"statusGroup", "equals", "OPEN"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderOpenMyCustomerRequests"), @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequestInfoAndWorkEffortAndPartyRel", list = "custRequests", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "myCompanyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromPartyId", operator = "equals", fromField = "fromPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromPartyId", operator = "not-equals", fromField = "notFromPartyId", ignoreIfEmpty = true)}, orderBy = {"+priority", "+custRequestDate"})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"statusGroup", "equals", "COMPLETED"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderCompletedMyCustomerRequests"), @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequestInfoAndWorkEffortAndPartyRel", list = "custRequests", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "myCompanyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromPartyId", operator = "equals", fromField = "fromPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromPartyId", operator = "not-equals", fromField = "notFromPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "statusId", operator = "equals", value = "CRQ_COMPLETED")}, orderBy = {"-custRequestDate"})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"statusGroup", "equals", "CANCELLED"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderCancelledMyCustomerRequests"), @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequestInfoAndWorkEffortAndPartyRel", list = "custRequests", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "myCompanyId", ignoreIfNull = true), @ConditionExpr(fieldName = "fromPartyId", operator = "equals", fromField = "fromPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromPartyId", operator = "not-equals", fromField = "notFromPartyId", ignoreIfEmpty = true)}, orderBy = {"-custRequestDate"})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"otherContacts", "not-equals", "Y"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${screenletTitle} ${fromPartyId}", includeForms = {@IncludeForm(name = "ListMyCustRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"statusGroup", "equals", "OPEN"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequest}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRequestCustomer")}), position = 0)})}), failWidgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"custRequests"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderOpenCollequeCustomerRequests")}), widgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${screenletTitle}", navigationFormName = "ListCustRequests", includeForms = {
                    @IncludeForm(name = "ListCustRequests", location = "component://order/widget/ordermgr/CustRequestForms.xml"
                )})}))}))
    public interface ListCustRequests {}

    @Screen(name = "CreateCustRequestNotification", location = "component://order/widget/ordermgr/CustRequestScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"custRequestId"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "Customer requestId is required: ${parameters.custRequestId} value: ${custRequestId}")}))
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "person", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "custRequest.fromPartyId")})
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderCustRequestNotificationMailCreation} #${custRequestId}")
    @Action(type = ActionType.SET, field = "parameters.subject", value = "You request has been received and is registered as Customer request: ${custRequest.custRequestName}[${custRequest.custRequestId}]")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/request/CreateCustRequestNotification.ftl")}))
    public interface CreateCustRequestNotification {}

    @Screen(name = "CompletedCustRequestNotification", location = "component://order/widget/ordermgr/CustRequestScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"custRequestId"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "Customer requestId is required: ${parameters.custRequestId} value: ${custRequestId}")}))
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "person", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "custRequest.fromPartyId")})
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderCustRequestNotificationMailCompleted} #${custRequestId}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/request/CompletedCustRequestNotification.ftl")}))
    public interface CompletedCustRequestNotification {}

    @Screen(name = "AddNoteCustRequestNotification", location = "component://order/widget/ordermgr/CustRequestScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"custRequestId"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "Customer requestId is required: ${parameters.custRequestId} value: ${custRequestId}")}))
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "person", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "custRequest.fromPartyId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "NoteData", valueField = "noteData")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderCustRequestNotificationMailNoteAdded} #${custRequestId}")
    @Action(type = ActionType.SET, field = "subject", value = "A note has been added to your Customer request ${custRequest.custRequestName}[${custRequest.custRequestId}]")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/request/AddedNoteCustRequestNotification.ftl")}))
    public interface AddNoteCustRequestNotification {}

    @Screen(name = "ListCustReturns", location = "component://order/widget/ordermgr/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.SET, field = "screenletTitle", fromField = "uiLabelMap.OrderMyReturns")
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "statusItem", fieldMaps = {@FieldMap(fieldName = "statusId", fromField = "statusId")})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${screenletTitle} (${statusItem.description})", includeForms = {@IncludeForm(name = "ListReturns", location = "component://order/widget/ordermgr/CustRequestForms.xml")})}))
    public interface ListCustReturns {}

}
