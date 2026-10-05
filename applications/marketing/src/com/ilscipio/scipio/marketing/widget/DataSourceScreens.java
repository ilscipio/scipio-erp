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
public class DataSourceScreens {

    @Screen(name = "CommonDataSourceDecorator", location = "component://marketing/widget/DataSourceScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/DataSourceMenus.xml#DataSource")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "${activeDataSourceSubMenuItem}")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "dataSourceId", fromField = "parameters.dataSourceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataSource", valueField = "dataSource")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonDataSourceDecorator {}

    @Screen(name = "EditDataSource", location = "component://marketing/widget/DataSourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataSource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataSource")
    @Action(type = ActionType.SET, field = "activeDataSourceSubMenuItem", value = "DataSource")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListDataSource")
    @Action(type = ActionType.SET, field = "dataSourceId", fromField = "parameters.dataSourceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataSource", valueField = "dataSource")
    @DecoratorScreen(
        name = "CommonDataSourceDecorator",
        location = "component://marketing/widget/DataSourceScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"dataSource"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleEditDataSource}", includeForms = {
                            @IncludeForm(name = "EditDataSource", location = "component://marketing/widget/DataSourceForms.xml", position = 1
                        )}, containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.DataSourceCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditDataSource"
                            )}, position = 0)})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleAddDataSource}", includeForms = {
                                    @IncludeForm(name = "EditDataSource", location = "component://marketing/widget/DataSourceForms.xml"
                                )})}))})
        }
    )
    public interface EditDataSource {}

    @Screen(name = "ListDataSource", location = "component://marketing/widget/DataSourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListDataSource")
    @Action(type = ActionType.SET, field = "activeDataSourceSubMenuItem", value = "DataSource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListDataSource")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListDataSource")
    @Action(type = ActionType.SET, field = "dataSourceId", fromField = "parameters.dataSourceId")
    @Action(type = ActionType.SET, field = "entityName", value = "DataSource")
    @DecoratorScreen(
        name = "CommonDataSourceDecorator",
        location = "component://marketing/widget/DataSourceScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListDataSource", location = "component://marketing/widget/DataSourceForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.DataSourceCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditDataSource"
                    )}, position = 0)})})
        }
    )
    public interface ListDataSource {}

    @Screen(name = "EditDataSourceType", location = "component://marketing/widget/DataSourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataSourceType")
    @Action(type = ActionType.SET, field = "activeDataSourceSubMenuItem", value = "DataSourceType")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataSourceType")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListDataSourceType")
    @Action(type = ActionType.SET, field = "dataSourceTypeId", fromField = "parameters.dataSourceTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataSourceType", valueField = "dataSourceType")
    @DecoratorScreen(
        name = "CommonDataSourceDecorator",
        location = "component://marketing/widget/DataSourceScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"dataSourceType"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleEditDataSourceType}", includeForms = {
                            @IncludeForm(name = "EditDataSourceType", location = "component://marketing/widget/DataSourceForms.xml", position = 1
                        )}, containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.DataSourceTypeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditDataSourceType"
                            )}, position = 0)})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleAddDataSourceType}", includeForms = {
                                    @IncludeForm(name = "EditDataSourceType", location = "component://marketing/widget/DataSourceForms.xml"
                                )})}))})
        }
    )
    public interface EditDataSourceType {}

    @Screen(name = "ListDataSourceType", location = "component://marketing/widget/DataSourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListDataSourceType")
    @Action(type = ActionType.SET, field = "activeDataSourceSubMenuItem", value = "DataSourceType")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListDataSourceType")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListDataSourceType")
    @Action(type = ActionType.SET, field = "dataSourceTypeId", fromField = "parameters.dataSourceTypeId")
    @Action(type = ActionType.SET, field = "entityName", value = "DataSourceType")
    @Action(type = ActionType.SET, field = "parameters.noConditionFind", value = "Y")
    @DecoratorScreen(
        name = "CommonDataSourceDecorator",
        location = "component://marketing/widget/DataSourceScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListDataSourceType", location = "component://marketing/widget/DataSourceForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.DataSourceTypeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditDataSourceType"
                    )}, position = 0)})})
        }
    )
    public interface ListDataSourceType {}

}
