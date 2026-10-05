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
public class SfaForecastScreens {

    @Screen(name = "CommonSalesForecastDecorator", location = "component://marketing/widget/sfa/ForecastScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Forecast")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(decoratorSectionIncludes = {
                    @DecoratorSectionInclude(name = "body")}, position = 1)}, sections = {
                        @InlineSection(widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "SalesForecastTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
                        )}), position = 0)})
        }
    )
    public interface CommonSalesForecastDecorator {}

    @Screen(name = "FindSalesForecast", location = "component://marketing/widget/sfa/ForecastScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSalesForecast")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Forecast")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "SalesForecastTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindSalesForecast", location = "component://marketing/widget/sfa/forms/ForecastForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "SalesForecastSearchResults", location = "component://marketing/widget/sfa/forms/ForecastForms.xml"
                    )}))}))})
        }
    )
    public interface FindSalesForecast {}

    @Screen(name = "EditSalesForecast", location = "component://marketing/widget/sfa/ForecastScreens.xml")
    @Action(type = ActionType.SET, field = "salesForecastId", fromField = "parameters.salesForecastId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SalesForecast", valueField = "salesForecast")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.salesForecast ? 'PageTitleEditSalesForecast' : 'SfaNewSalesForecast'}")
    @Action(type = ActionType.SET, field = "isNewSalesForecast", value = "${groovy: context.salesForecast ? false : true}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonSalesForecastDecorator",
        location = "component://marketing/widget/sfa/ForecastScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditSalesForecast", location = "component://marketing/widget/sfa/forms/ForecastForms.xml"
                )})})
        }
    )
    public interface EditSalesForecast {}

    @Screen(name = "EditSalesForecastDetail", location = "component://marketing/widget/sfa/ForecastScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSalesForecastDetail")
    @Action(type = ActionType.SET, field = "salesForecastId", fromField = "parameters.salesForecastId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SalesForecast", valueField = "salesForecast")
    @DecoratorScreen(
        name = "CommonSalesForecastDecorator",
        location = "component://marketing/widget/sfa/ForecastScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.SfaAddSalesForecastDetail} ${uiLabelMap.CommonFor} [${salesForecastId}]", includeForms = {
                    @IncludeForm(name = "AddSalesForecastDetail", location = "component://marketing/widget/sfa/forms/ForecastForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.SfaListSalesForecastDetail}", includeForms = {
                    @IncludeForm(name = "ListSalesForecastDetails", location = "component://marketing/widget/sfa/forms/ForecastForms.xml"
                )})})
        }
    )
    public interface EditSalesForecastDetail {}

}
