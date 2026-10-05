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
public class OrdermgrQuoteScreens {

    @Screen(name = "FindQuote", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderFindQuote")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewQuote}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuote"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "FindQuotes", location = "component://order/widget/ordermgr/QuoteForms.xml"
                    ),
                    @IncludeForm(name = "ListQuotes", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )}, position = 1)})
        }
    )
    public interface FindQuote {}

    @Screen(name = "ViewQuote", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewQuote")
    @Action(type = ActionType.SET, field = "showQuoteManagementLinks", value = "Y")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "QuoteType", toValueField = "quoteType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "SalesChannelEnumeration", toValueField = "salesChannel")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Uom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.SET, field = "listOrderBy[]", value = "quoteItemSeqId")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteItem", list = "quoteItems", orderByList = "listOrderBy")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteAdjustment", list = "quoteAdjustments")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteRole", list = "quoteRoles")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderQuote")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}: ${quoteId}")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewQuoteTemplate", location = "component://order/widget/ordermgr/QuoteScreens.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonHeader}", includeForms = {
                    @IncludeForm(name = "QuoteHeader", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.OrderCopyQuote}", htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/quote/CopyQuote.ftl"
                )}, position = 2)})
        }
    )
    public interface ViewQuote {}

    @Screen(name = "ViewQuoteSimple", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "showQuoteManagementLinks", value = "N")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "QuoteType", toValueField = "quoteType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Uom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteItem", list = "quoteItems")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteAdjustment", list = "quoteAdjustments")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteRole", list = "quoteRoles")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewQuoteTemplate")}))
    public interface ViewQuoteSimple {}

    @Screen(name = "QuoteReport", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteReport")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "QuoteType", toValueField = "quoteType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Uom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Party", toValueField = "party")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteTerm", list = "quoteTerms")
    @Action(type = ActionType.SET, field = "listOrderBy[]", value = "quoteItemSeqId")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteItem", list = "quoteItems", orderByList = "listOrderBy")
    @Action(type = ActionType.GET_RELATED, valueField = "quote", relationName = "QuoteAdjustment", list = "quoteAdjustments")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/quote/GetPartyAddress.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteReportContactMechs.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteReportHeaderInfo.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteReportBody.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "footer", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/order/pdf/ScipioOrderFooter.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface QuoteReport {}

    @Screen(name = "EditQuote", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditQuote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.quoteId ? 'OrderOrderQuote' : 'OrderNewQuote'}")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}${groovy: context.quoteId ? (': ' + context.quoteId) : ''}${groovy: context.quote?.quoteName ? (' - ' + context.quote.quoteName ) : ''}")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuote", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface EditQuote {}

    @Screen(name = "ListQuoteRoles", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteListRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteRoles")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteRole", list = "quoteRoles", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteRoles", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface ListQuoteRoles {}

    @Screen(name = "EditQuoteRole", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteRoles")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteRole", valueField = "quoteRole")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteRole", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteRole}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteRole"
                    )}, position = 0)})})
        }
    )
    public interface EditQuoteRole {}

    @Screen(name = "ListQuoteItems", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteListItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteItems")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteItem", list = "quoteItems", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")}, orderBy = {"quoteItemSeqId"})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteItems", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface ListQuoteItems {}

    @Screen(name = "EditQuoteItem", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteItems")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteItem", valueField = "quoteItem")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteTerm", list = "quoteTerms", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId"), @FieldMap(fieldName = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditQuoteItem", location = "component://order/widget/ordermgr/QuoteForms.xml"
            )}, containers = {
                @Container(style = "${styles.grid_large}6", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"quoteItemSeqId"})}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.OrderOrderQuoteTermList}", includeForms = {
                    @IncludeForm(name = "ListQuoteTermItem", location = "component://order/widget/ordermgr/QuoteForms.xml"
                
                        )})}))})})
        }
    )
    public interface EditQuoteItem {}

    @Screen(name = "ListQuoteAttributes", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteListAttributes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteAttributes")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteAttribute", list = "quoteAttributes", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteAttributes", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteAttribute}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteAttribute"
                    )}, position = 0)})})
        }
    )
    public interface ListQuoteAttributes {}

    @Screen(name = "EditQuoteAttribute", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditAttributes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteAttributes")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "attrName", fromField = "parameters.attrName")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteAttribute", valueField = "quoteAttribute")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteAttribute", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteAttribute}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteAttribute"
                    )}, position = 0)})})
        }
    )
    public interface EditQuoteAttribute {}

    @Screen(name = "ListQuoteCoefficients", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteListCoefficients")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteCoefficients")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteCoefficient", list = "quoteCoefficients", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteCoefficients", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteCoefficient}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteCoefficient"
                    )}, position = 0)})})
        }
    )
    public interface ListQuoteCoefficients {}

    @Screen(name = "EditQuoteCoefficient", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditCoefficients")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteCoefficients")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "attrName", fromField = "parameters.coeffName")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteCoefficient", valueField = "quoteCoefficient")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteCoefficient", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteCoefficient}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteCoefficient"
                    )}, position = 0)})})
        }
    )
    public interface EditQuoteCoefficient {}

    @Screen(name = "ManageQuotePrices", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuotePrices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ManageQuotePrices")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteItem", list = "quoteItems", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")}, orderBy = {"custRequestId", "custRequestItemSeqId", "quoteItemSeqId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteCoefficient", list = "quoteCoefficients", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @Action(type = ActionType.SET, field = "quoteId", fromField = "quote.quoteId")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/quote/ManageQuotePrices.groovy")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.OrderOrderQuotePrices}", htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/quote/ManageQuotePrices.ftl"
                )}),
                @Screenlet(title = "${uiLabelMap.OrderOrderQuotePrices}", includeForms = {
                    @IncludeForm(name = "ManageQuotePrices", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/quote/ManageQuotePricesTotals.ftl"
                )})})
        }
    )
    public interface ManageQuotePrices {}

    @Screen(name = "ListQuoteAdjustments", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteListAdjustments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteAdjustments")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteAdjustment", list = "quoteAdjustments", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListQuoteAdjustments", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderRunStorePromotions}", style = "${styles.link_nav} ${styles.action_add}", target = "autoCreateQuoteAdjustments"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteAdjustment}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteAdjustment"
                )}, position = 0)})})
        }
    )
    public interface ListQuoteAdjustments {}

    @Screen(name = "EditQuoteAdjustment", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditAdjustments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteAdjustments")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "quoteAdjustmentId", fromField = "parameters.quoteAdjustmentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteAdjustment", valueField = "quoteAdjustment")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteAdjustment", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderCreateOrderQuoteAdjustment}", style = "${styles.link_nav} ${styles.action_add}", target = "EditQuoteAdjustment"
                    )}, position = 0)})})
        }
    )
    public interface EditQuoteAdjustment {}

    @Screen(name = "ViewQuoteProfit", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteViewProfit")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewQuoteProfit")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteItem", list = "quoteItems", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")}, orderBy = {"custRequestId", "custRequestItemSeqId", "quoteItemSeqId"})
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteCoefficient", list = "quoteCoefficients", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "quote.quoteId")})
    @Action(type = ActionType.SET, field = "quoteId", fromField = "quote.quoteId")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/quote/ViewQuoteProfit.groovy")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ViewQuoteProfit", location = "component://order/widget/ordermgr/QuoteForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/quote/ViewQuoteProfit.ftl", position = 0
                )})})
        }
    )
    public interface ViewQuoteProfit {}

    @Screen(name = "EditQuoteReportMail", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditReportMail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewQuote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "quote", relationName = "Party", toValueField = "party")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/quote/GetPartyEmailAddress.groovy")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteReportMail", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface EditQuoteReportMail {}

    @Screen(name = "ViewQuoteTemplate", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"quote"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoQuoteFound}", style = "common-msg-error")}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${note}"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewQuoteInfo"), @Widget(type = WidgetType.CONTAINER, style = "clear"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewQuoteItemInfo")}))
    public interface ViewQuoteTemplate {}

    @Screen(name = "ViewQuoteInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteTerm", list = "quoteTerms", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId"), @FieldMap(fieldName = "quoteItemSeqId", value = "_NA_")})
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteNoteView", list = "quoteNotes", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId")}, orderBy = {"-noteDateTime"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "QuoteTermsInfo")}))
    public interface ViewQuoteInfo {}

    @Screen(name = "quoteInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteInfo.ftl")}))
    public interface quoteInfo {}

    @Screen(name = "ListQuoteInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"quoteTerms"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderOrderQuoteTermList}", includeForms = {@IncludeForm(name = "ListQuoteInfo", location = "component://order/widget/ordermgr/QuoteForms.xml")})}))
    public interface ListQuoteInfo {}

    @Screen(name = "ListQuoteNoteInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"quoteNotes"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderOrderQuoteNotes}", includeForms = {@IncludeForm(name = "ListQuoteNoteInfo", location = "component://order/widget/ordermgr/QuoteForms.xml")})}))
    public interface ListQuoteNoteInfo {}

    @Screen(name = "quoteDate", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteDate.ftl")}))
    public interface quoteDate {}

    @Screen(name = "quoteRoles", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/quoteRoles.ftl")}))
    public interface quoteRoles {}

    @Screen(name = "ViewQuoteItemInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"quoteItems"})}))
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteItem", list = "quoteItemList", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId")})
    @Action(type = ActionType.SET, field = "quoteStatusId", fromField = "quote.statusId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/quote/ViewQuoteItemInfo.ftl")}))
    public interface ViewQuoteItemInfo {}

    @Screen(name = "EditQuoteTerm", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderOrderQuoteEditTerm}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteTerms")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "termTypeId", fromField = "parameters.termTypeId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteTerm", valueField = "quoteTerm")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.SET, field = "target", fromField = "parameters.target")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteTerm", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface EditQuoteTerm {}

    @Screen(name = "EditQuoteTermItem", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditTerm")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteItems")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "termTypeId", fromField = "parameters.termTypeId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteTerm", valueField = "quoteTerm")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.SET, field = "target", fromField = "parameters.target")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditQuoteTermItem", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface EditQuoteTermItem {}

    @Screen(name = "ListQuoteTerms", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteEditTerm")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteTerms")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "termTypeId", fromField = "parameters.termTypeId")
    @Action(type = ActionType.SET, field = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteTerm", valueField = "quoteTerm")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteTerm", list = "quoteTerms", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId")})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.OrderOrderQuoteTermList}", includeForms = {
                    @IncludeForm(name = "ListQuoteTerms", location = "component://order/widget/ordermgr/QuoteForms.xml"
                )})})
        }
    )
    public interface ListQuoteTerms {}

    @Screen(name = "ListQuoteNotes", location = "component://order/widget/ordermgr/QuoteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderOrderQuoteNoteList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListQuoteNotes")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "headerRowStyle", value = "header-row-2")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_AND, entityName = "QuoteNoteView", list = "quoteNotes", fieldMaps = {@FieldMap(fieldName = "quoteId", fromField = "parameters.quoteId")}, orderBy = {"-noteDateTime"})
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListQuoteNotes", location = "component://order/widget/ordermgr/QuoteForms.xml"
            )})
        }
    )
    public interface ListQuoteNotes {}

    @Screen(name = "QuoteNewNote", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.quoteId"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderViewPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderAddNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteNotes")
    @Action(type = ActionType.SET, field = "target", value = "createquotenote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddOrEditQuoteNote", location = "component://order/widget/ordermgr/QuoteForms.xml"
            )})
        }
    )
    public interface QuoteNewNote {}

    @Screen(name = "EditQuoteNote", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"parameters.noteId"}), @Condition(type = NotEmpty.class, params = {"parameters.quoteId"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderViewPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "QuoteEditNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuoteNotes")
    @Action(type = ActionType.SET, field = "target", value = "updateQuoteNote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "noteId", fromField = "parameters.noteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Quote", valueField = "quote")
    @Action(type = ActionType.ENTITY_ONE, entityName = "QuoteNoteView", valueField = "quoteNoteData")
    @DecoratorScreen(
        name = "CommonQuoteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddOrEditQuoteNote", location = "component://order/widget/ordermgr/QuoteForms.xml"
            )})
        }
    )
    public interface EditQuoteNote {}

    @Screen(name = "QuoteTermsInfo", location = "component://order/widget/ordermgr/QuoteScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"quoteTerms"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonTerms}", includeForms = {@IncludeForm(name = "ListQuoteInfo", location = "component://order/widget/ordermgr/QuoteForms.xml")})}))
    public interface QuoteTermsInfo {}

}
