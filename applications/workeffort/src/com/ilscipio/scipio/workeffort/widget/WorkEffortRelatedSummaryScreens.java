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
package com.ilscipio.scipio.workeffort.widget;

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
public class WorkEffortRelatedSummaryScreens {

    @Screen(name = "WorkEffortSummary", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleWorkEffortRelatedSummary")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortRelatedSummary")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleWorkEffortRelatedSummary")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "WorkEffortType", toValueField = "workEffortType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "CurrentStatusItem", toValueField = "currentStatusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "WorkEffortPurposeType", toValueField = "workEffortPurposeType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "ScopeEnumeration", toValueField = "scopeEnumeration")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortPartyAssignView", list = "partyAssignments", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAndFixedAssetAssign", list = "fixedAssetAssignments", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortCommunicationEventView", list = "commEvents", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortShoppingListView", list = "shoppingLists", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortQuoteView", list = "quotes", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortOrderHeaderView", list = "orderHeaders", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "screenlet-body", containers = {
                    @Container2(labels = {
                        @Label(text = "${uiLabelMap.CommonName}: ", style = "span"),
                        @Label(text = "${workEffort.workEffortName}", style = "span")
                    }),
                    @Container2(labels = {
                        @Label(text = "${uiLabelMap.CommonType}: ", style = "span"),
                        @Label(text = "${workEffortType.description}", style = "span"
                    )}),
                    @Container2(labels = {
                        @Label(text = "${uiLabelMap.CommonPurpose}: ", style = "span"
                    ),
                    @Label(text = "${workEffortPurposeType.description}", style = "span"
                )}),
                @Container2(labels = {
                    @Label(text = "${uiLabelMap.CommonStatus}: ", style = "span"),
                    @Label(text = "${currentStatusItem.description}", style = "span"
                )})}),
                @Container(style = "screenlet-body", containers = {
                    @Container2(labels = {
                        @Label(text = "${uiLabelMap.WorkEffortPercentComplete}: ", style = "span"
                    ),
                    @Label(text = "${workEffort.percentComplete}", style = "span"
                )}),
                @Container2(labels = {
                    @Label(text = "${uiLabelMap.CommonPriority}: ", style = "span"
                ),
                @Label(text = "${workEffort.priority}", style = "span")}),
                @Container2(labels = {
                    @Label(text = "${uiLabelMap.WorkEffortEstimatedStartDate}: ", style = "span"
                ),
                @Label(text = "${workEffort.estimatedStartDate}", style = "span"
            )}),
            @Container2(labels = {
                @Label(text = "${uiLabelMap.WorkEffortEstimatedCompletionDate}: ", style = "span"
            ),
            @Label(text = "${workEffort.estimatedCompletionDate}", style = "span"
            )})})}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"partyAssignments"})
                }), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                        @Container(style = "h2", labels = {
                            @Label(text = "${uiLabelMap.PageTitleListWorkEffortPartyAssigns}"
                        )}),
                        @Container(style = "screenlet-body", includeForms = {
                            @IncludeForm(name = "DisplayWorkEffortPartyAssigns", location = "component://workeffort/widget/WorkEffortForms.xml"
                        )})})),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"fixedAssetAssignments"
                        })}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                                @Container(style = "h2", labels = {
                                    @Label(text = "${uiLabelMap.PageTitleListWorkEffortFixedAssetAssigns}"
                                )}),
                                @Container(style = "screenlet-body", includeForms = {
                                    @IncludeForm(name = "DisplayWorkEffortFixedAssetAssigns", location = "component://workeffort/widget/WorkEffortForms.xml"
                                )})})),
                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                    @Condition(type = Empty.class, params = {"commEvents"})}), widgets = @InlineWidgets(value = {
                                        @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                                            @Container(style = "h2", labels = {
                                                @Label(text = "${uiLabelMap.WorkEffortCommEvents}")}),
                                                @Container(style = "screenlet-body", widgets = {
                                                    @Widget(type = WidgetType.ITERATE_SECTION, list = "commEvents", entry = "commEvent", name = "WorkEffortSummary-iterate1", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml"
                                                )})})),
                                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                                    @Condition(type = Empty.class, params = {"shoppingLists"})}), widgets = @InlineWidgets(value = {
                                                        @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                                                            @Container(style = "h2", labels = {
                                                                @Label(text = "${uiLabelMap.WorkEffortShopLists}")}),
                                                                @Container(style = "screenlet-body", widgets = {
                                                                    @Widget(type = WidgetType.ITERATE_SECTION, list = "shoppingLists", entry = "shopList", name = "WorkEffortSummary-iterate2", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml"
                                                                )})})),
                                                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                                                    @Condition(type = Empty.class, params = {"quotes"})}), widgets = @InlineWidgets(value = {
                                                                        @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                                                                            @Container(style = "h2", labels = {
                                                                                @Label(text = "${uiLabelMap.WorkEffortQuotes}")}),
                                                                                @Container(style = "screenlet-body", widgets = {
                                                                                    @Widget(type = WidgetType.ITERATE_SECTION, list = "quotes", entry = "quote", name = "WorkEffortSummary-iterate3", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml"
                                                                                )})})),
                                                                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                                                                    @Condition(type = Empty.class, params = {"quotes"})}), widgets = @InlineWidgets(value = {
                                                                                        @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                                                                                            @Container(style = "h2", labels = {
                                                                                                @Label(text = "${uiLabelMap.WorkEffortOrderHeaders}")}),
                                                                                                @Container(style = "screenlet-body", widgets = {
                                                                                                    @Widget(type = WidgetType.ITERATE_SECTION, list = "orderHeaders", entry = "orderHeader", name = "WorkEffortSummary-iterate4", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml"
                                                                                                )})}))})
        }
    )
    public interface WorkEffortSummary {}

    @Screen(name = "WorkEffortSummary-iterate1", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${commEvent.subject}")}, widgets = {@Widget(type = WidgetType.LINK, text = "${commEvent.communicationEventId}", style = "${styles.link_nav_info_id}", target = "/partymgr/control/EditCommunicationEvent")})}))
    public interface WorkEffortSummary_iterate1 {}

    @Screen(name = "WorkEffortSummary-iterate2", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${shopList.listName} ${shopList.description}")}, widgets = {@Widget(type = WidgetType.LINK, text = "${shopList.shoppingListId}", style = "${styles.link_nav_info_id}", target = "/partymgr/control/editShoppingList")})}))
    public interface WorkEffortSummary_iterate2 {}

    @Screen(name = "WorkEffortSummary-iterate3", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${quote.quoteName} ${quote.description}")}, widgets = {@Widget(type = WidgetType.LINK, text = "${quote.quoteId}", style = "${styles.link_nav_info_id}", target = "/ordermgr/control/EditQuote")})}))
    public interface WorkEffortSummary_iterate3 {}

    @Screen(name = "WorkEffortSummary-iterate4", location = "component://workeffort/widget/WorkEffortRelatedSummaryScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(widgets = {@Widget(type = WidgetType.LINK, text = "${orderHeader.orderId}", style = "${styles.link_nav_info_id}", target = "/ordermgr/control/orderview")}, containers = {@Container2(labels = {@Label(text = "${uiLabelMap.CommonType}: ", style = "span"), @Label(text = "${orderHeader.orderTypeDescription}", style = "span")}), @Container2(labels = {@Label(text = "${uiLabelMap.CommonStatus}: ", style = "span"), @Label(text = "${orderHeader.statusItemDescription}", style = "span")}), @Container2(labels = {@Label(text = "${uiLabelMap.CommonTotal}: ", style = "span"), @Label(text = "${orderHeader.grandTotal}", style = "span")}), @Container2(labels = {@Label(text = "${uiLabelMap.CommonDate}: ", style = "span"), @Label(text = "${orderHeader.orderDate}", style = "span")})})}))
    public interface WorkEffortSummary_iterate4 {}

}
