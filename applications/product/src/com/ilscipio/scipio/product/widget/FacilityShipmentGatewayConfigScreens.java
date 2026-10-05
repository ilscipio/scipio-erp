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
package com.ilscipio.scipio.product.widget;

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
public class FacilityShipmentGatewayConfigScreens {

    @Screen(name = "GenericShipmentGatewayConfigDecorator", location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#ShipmentGatewayConfig")
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]}", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface GenericShipmentGatewayConfigDecorator {}

    @Screen(name = "FindShipmentGatewayConfig", location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindShipmentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "shipmentGatewayConfigTab")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "GenericShipmentGatewayConfigDecorator",
        location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindShipmentGatewayConfig", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListShipmentGatewayConfig", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                    )}))})})
        }
    )
    public interface FindShipmentGatewayConfig {}

    @Screen(name = "EditShipmentGatewayConfig", location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUpdateShipmentGatewayConfig")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "shipmentGatewayConfigTab")
    @Action(type = ActionType.SET, field = "shipmentGatewayConfigId", fromField = "parameters.shipmentGatewayConfigId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayConfig", valueField = "shipmentGatewayConfig")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayDhl", valueField = "shipmentGatewayDhl", fieldMaps = {@FieldMap(fieldName = "shipmentGatewayConfigId", fromField = "parameters.shipmentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayFedex", valueField = "shipmentGatewayFedex", fieldMaps = {@FieldMap(fieldName = "shipmentGatewayConfigId", fromField = "parameters.shipmentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayUsps", valueField = "shipmentGatewayUsps", fieldMaps = {@FieldMap(fieldName = "shipmentGatewayConfigId", fromField = "parameters.shipmentGatewayConfigId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayUps", valueField = "shipmentGatewayUps", fieldMaps = {@FieldMap(fieldName = "shipmentGatewayConfigId", fromField = "parameters.shipmentGatewayConfigId")})
    @DecoratorScreen(
        name = "GenericShipmentGatewayConfigDecorator",
        location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleUpdateShipmentGatewayConfig}", includeForms = {
                    @IncludeForm(name = "EditShipmentGatewayConfig", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"shipmentGatewayDhl"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleUpdateShipmentGatewayConfigDhl}", includeForms = {
                            @IncludeForm(name = "EditShipmentGatewayConfigDhl", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                        )})})),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"shipmentGatewayFedex"
                        })}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleUpdateShipmentGatewayConfigFedex}", includeForms = {
                                @IncludeForm(name = "EditShipmentGatewayConfigFedex", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                            )})})),
                            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                @Condition(type = Empty.class, params = {"shipmentGatewayUps"
                            })}), widgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleUpdateShipmentGatewayConfigUps}", includeForms = {
                                    @IncludeForm(name = "EditShipmentGatewayConfigUps", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                                )})})),
                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                    @Condition(type = Empty.class, params = {"shipmentGatewayUsps"
                                })}), widgets = @InlineWidgets(screenlets = {
                                    @Screenlet(title = "${uiLabelMap.PageTitleUpdateShipmentGatewayConfigUsps}", includeForms = {
                                        @IncludeForm(name = "EditShipmentGatewayConfigUsps", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                                    )})}))})
        }
    )
    public interface EditShipmentGatewayConfig {}

    @Screen(name = "FindShipmentGatewayConfigTypes", location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindShipmentGatewayConfigTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "shipmentGatewayConfigTypesTab")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "GenericShipmentGatewayConfigDecorator",
        location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindShipmentGatewayConfigTypes", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListShipmentGatewayConfigTypes", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                    )}))})})
        }
    )
    public interface FindShipmentGatewayConfigTypes {}

    @Screen(name = "EditShipmentGatewayConfigType", location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUpdateShipmentGatewayConfigType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "shipmentGatewayConfigTypesTab")
    @Action(type = ActionType.SET, field = "shipmentGatewayConfTypeId", fromField = "parameters.shipmentGatewayConfTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentGatewayConfigType", valueField = "shipmentGatewayConfigType")
    @DecoratorScreen(
        name = "GenericShipmentGatewayConfigDecorator",
        location = "component://product/widget/facility/ShipmentGatewayConfigScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditShipmentGatewayConfigType", location = "component://product//widget/facility/ShipmentGatewayConfigForms.xml"
                )})})
        }
    )
    public interface EditShipmentGatewayConfigType {}

}
