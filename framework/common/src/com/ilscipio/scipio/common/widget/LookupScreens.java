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
package com.ilscipio.scipio.common.widget;

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
public class LookupScreens {

    @Screen(name = "LookupGeo", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupGeo}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Geo")
    @Action(type = ActionType.SET, field = "searchFields", value = "[geoId, geoName, geoCode]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[geoId, geoName, geoCode]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupGeo", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupGeo", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupGeo {}

    @Screen(name = "LookupGeoName", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupGeo}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Geo")
    @Action(type = ActionType.SET, field = "searchFields", value = "[geoName, geoId, geoCode]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[geoId, geoName, geoCode]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupGeoName", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupGeoName", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupGeoName {}

    @Screen(name = "ListLocales", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonChooseLanguage}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocales", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListLocales {}

    @Screen(name = "ListLocalesCompact", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonChooseLanguage}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocalesCompact", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListLocalesCompact {}

    @Screen(name = "ListTimezones", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonTime}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listTimezones", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListTimezones {}

    @Screen(name = "ListVisualThemes", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonVisualThemes}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listVisualThemes", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListVisualThemes {}

    @Screen(name = "TimeDuration", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonTime}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/timeDuration.ftl"
            )})
        }
    )
    public interface TimeDuration {}

    @Screen(name = "LookupUserLogin", location = "component://common/widget/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"securityPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.LookupUserLogin}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupUserLogin", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookedUpUserLogins", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupUserLogin {}

    @Screen(name = "LookupPortalPage", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonPortalPage}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupPortalPage", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPortalPages", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupPortalPage {}

    @Screen(name = "LookupLocale", location = "component://common/widget/LookupScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonLookupLocale}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "inputFields", fromField = "parameters")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLocaleList.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupLocale", location = "component://common/widget/LookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLocales", location = "component://common/widget/LookupForms.xml"
            )})
        }
    )
    public interface LookupLocale {}

}
