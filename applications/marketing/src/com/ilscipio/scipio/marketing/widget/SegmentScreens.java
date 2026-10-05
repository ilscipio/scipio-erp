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
package com.ilscipio.scipio.marketing.widget;

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
public class SegmentScreens {

    @Screen(name = "FindSegmentGroup", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindSegmentGroup")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingSegments}")
    @DecoratorScreen(
        name = "CommonSegmentGroupDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingSegmentGroupCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "viewSegmentGroup"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/segment/FindMarketingSegment.ftl"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "MarketingSegmentSearchResults"
                        )}))})})
        }
    )
    public interface FindSegmentGroup {}

    @Screen(name = "MarketingSegmentSearchResults", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/segment/FindMarketingSegment.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/segment/MarketingSegmentList.ftl")}))
    public interface MarketingSegmentSearchResults {}

    @Screen(name = "EditSegmentGroup", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "segmentGroupId", fromField = "parameters.segmentGroupId")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SegmentGroup", valueField = "segmentGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.segmentGroup ? 'SegmentGroup' : 'NewSegmentGroup'}")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingSegment} ${segmentGroupId}")
    @DecoratorScreen(
        name = "CommonSegmentGroupDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditSegmentGroup", location = "component://marketing/widget/SegmentForms.xml"
                )})})
        }
    )
    public interface EditSegmentGroup {}

    @Screen(name = "listSegmentGroupClass", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSegmentGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SegmentGroupClassification")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "segmentGroupId", fromField = "parameters.segmentGroupId")
    @DecoratorScreen(
        name = "CommonSegmentGroupDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listSegmentGroupClass", location = "component://marketing/widget/SegmentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.MarketingSegmentGroupClassCreate}", name = "AddSegmentGroupClassPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSegmentGroupClass", location = "component://marketing/widget/SegmentForms.xml"
                )}, position = 0)})
        }
    )
    public interface listSegmentGroupClass {}

    @Screen(name = "listSegmentGroupGeo", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListSegmentGroupGeo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SegmentGroupGeo")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "segmentGroupId", fromField = "parameters.segmentGroupId")
    @DecoratorScreen(
        name = "CommonSegmentGroupDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listSegmentGroupGeo", location = "component://marketing/widget/SegmentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditSegmentGroupGeo}", name = "AddSegmentGroupGeoPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSegmentGroupGeo", location = "component://marketing/widget/SegmentForms.xml"
                )}, position = 0)})
        }
    )
    public interface listSegmentGroupGeo {}

    @Screen(name = "listSegmentGroupRole", location = "component://marketing/widget/SegmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListSegmentGroupRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SegmentGroupRole")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "segmentGroupId", fromField = "parameters.segmentGroupId")
    @DecoratorScreen(
        name = "CommonSegmentGroupDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listSegmentGroupRole", location = "component://marketing/widget/SegmentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditSegmentGroupRole}", name = "AddSegmentGroupRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSegmentGroupRole", location = "component://marketing/widget/SegmentForms.xml"
                )}, position = 0)})
        }
    )
    public interface listSegmentGroupRole {}

}
