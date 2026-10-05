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
package com.ilscipio.scipio.webtools.widget;

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
public class GeoManagementScreens {

    @Screen(name = "FindGeo", location = "component://webtools/widget/GeoManagementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsGeosFind")
    @Action(type = ActionType.SET, field = "currentUrl", value = "FindGeo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindGeo")
    @DecoratorScreen(
        name = "CommonGeoManagementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "no-clear", decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindGeo", location = "component://webtools/widget/GeoManagementForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGeos", location = "component://webtools/widget/GeoManagementForms.xml"
                    )}))}))})
        }
    )
    public interface FindGeo {}

    @Screen(name = "EditGeo", location = "component://webtools/widget/GeoManagementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsGeoEdit")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditGeo")
    @Action(type = ActionType.SET, field = "geoId", fromField = "parameters.geoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Geo", valueField = "geo")
    @DecoratorScreen(
        name = "CommonGeoManagementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditGeo", location = "component://webtools/widget/GeoManagementForms.xml"
                )})})
        }
    )
    public interface EditGeo {}

    @Screen(name = "LookupGeo", location = "component://webtools/widget/GeoManagementScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupGeo}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "Geo")
    @Action(type = ActionType.SET, field = "searchFields", value = "[geoId, geoName]")
    @Action(type = ActionType.SET, field = "currentUrl", value = "LookupGeo")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindGeo", location = "component://webtools/widget/GeoManagementForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupGeo", location = "component://webtools/widget/GeoManagementForms.xml"
            )})
        }
    )
    public interface LookupGeo {}

    @Screen(name = "LinkGeos", location = "component://webtools/widget/GeoManagementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsGeosLink")
    @Action(type = ActionType.SET, field = "currentUrl", value = "LinkGeos")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "LinkGeos")
    @Action(type = ActionType.SET, field = "noId", value = "true")
    @Action(type = ActionType.SET, field = "asm_multipleSelectForm", value = "LinkGeos")
    @Action(type = ActionType.SET, field = "asm_multipleSelect", value = "LinkGeos_geoIds")
    @Action(type = ActionType.SET, field = "asm_formSize", value = "700")
    @Action(type = ActionType.SET, field = "asm_asmListItemPercentOfForm", value = "95")
    @Action(type = ActionType.SET, field = "asm_sortable", value = "false")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "asm_title", value = "${uiLabelMap.WebtoolsGeosSelect}")
    @Action(type = ActionType.SET, field = "asm_relatedField", value = "LinkGeos_geoId")
    @Action(type = ActionType.SET, field = "asm_requestName", value = "getRelatedGeos")
    @Action(type = ActionType.SET, field = "asm_paramKey", value = "geoId")
    @Action(type = ActionType.SET, field = "asm_type", value = "geoAssocTypeId")
    @Action(type = ActionType.SET, field = "asm_typeField", value = "LinkGeos_geoAssocTypeId")
    @Action(type = ActionType.SET, field = "asm_responseName", value = "geoList")
    @DecoratorScreen(
        name = "CommonGeoManagementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setMultipleSelectJs.ftl"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.WebtoolsGeosLinkExplained}", includeForms = {
                    @IncludeForm(name = "LinkGeos", location = "component://webtools/widget/GeoManagementForms.xml"
                )})})
        }
    )
    public interface LinkGeos {}

}
