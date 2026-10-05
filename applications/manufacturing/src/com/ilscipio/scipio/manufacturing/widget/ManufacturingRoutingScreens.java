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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingRoutingScreens {

    @Screen(name = "CommonRoutingDecorator", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Routing")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routing")
    @Action(type = ActionType.SET, field = "workEffortNameStr", value = " ${routing.workEffortName} [${routing.workEffortId}]")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}${groovy: context.routing ? context.workEffortNameStr : ''}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.routing}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonRoutingDecorator {}

    @Screen(name = "CommonRoutingTaskDecorator", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "RoutingTask")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routingTask")
    @Action(type = ActionType.SET, field = "workEffortNameStr", value = " ${routingTask.workEffortName} [${routingTask.workEffortId}]")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}${groovy: context.routingTask ? context.workEffortNameStr : ''}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.routingTask}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonRoutingTaskDecorator {}

    @Screen(name = "FindRouting", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindRouting")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRouting")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "requestParameters.workEffortTypeId", defaultValue = "ROUTING")
    @Action(type = ActionType.SET, field = "requestParameters.currentStatusId", defaultValue = "ROU_ACTIVE")
    @DecoratorScreen(
        name = "CommonRoutingDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewRouting}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRouting"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRoutings", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRoutings", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                        )}))})})
        }
    )
    public interface FindRouting {}

    @Screen(name = "EditRouting", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRouting")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routing")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.routing ? 'PageTitleEditRouting' : 'ManufacturingNewRouting'}")
    @DecoratorScreen(
        name = "CommonRoutingDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRouting", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"routing"})}), widgets = @InlineWidgets(containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewRouting}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRouting"
                            )})}), position = 0)})
        }
    )
    public interface EditRouting {}

    @Screen(name = "FindRoutingTask", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindRoutingTask")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRoutingTask")
    @Action(type = ActionType.SET, field = "requestParameters.workEffortTypeId", defaultValue = "ROU_TASK")
    @Action(type = ActionType.SET, field = "requestParameters.currentStatusId", defaultValue = "ROU_ACTIVE")
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewRoutingTask}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRoutingTask"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRoutingTasks", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRoutingTasks", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                        )}))})})
        }
    )
    public interface FindRoutingTask {}

    @Screen(name = "EditRoutingTask", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRoutingTask")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routingTask")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.routingTask ? 'PageTitleEditRoutingTask' : 'ManufacturingNewRoutingTask'}")
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRoutingTask", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"routingTask"})}), widgets = @InlineWidgets(containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewRoutingTask}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRoutingTask"
                            )})}), position = 0)})
        }
    )
    public interface EditRoutingTask {}

    @Screen(name = "EditRoutingTaskCosts", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRoutingTaskCosts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRoutingTaskCosts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routingTask")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortCostCalc", list = "allCosts", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "workEffortId")})
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRoutingTaskCosts", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.ManufacturingRoutingTaskCosts}", name = "AddRoutingTaskCostPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddRoutingTaskCost", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditRoutingTaskCosts {}

    @Screen(name = "ListRoutingTaskRoutings", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRoutingTaskRoutings")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "listRoutingTaskRoutings")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routingTask")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAssoc", list = "allRoutings", fieldMaps = {@FieldMap(fieldName = "workEffortIdTo", fromField = "workEffortId")})
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListRoutingTaskRoutings", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )})})
        }
    )
    public interface ListRoutingTaskRoutings {}

    @Screen(name = "ListRoutingTaskProducts", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRoutingTaskProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "listRoutingTaskProducts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "routingTask")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortGoodStandard", list = "allProducts", fieldMaps = {@FieldMap(fieldName = "workEffortGoodStdTypeId", value = "PRUNT_PROD_DELIV"), @FieldMap(fieldName = "workEffortId", fromField = "workEffortId")})
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewRoutingTaskProduct}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRoutingTaskProduct"
            )}, screenlets = {
                @Screenlet(name = "EditRoutingTaskProductPanel", includeForms = {
                    @IncludeForm(name = "ListRoutingTaskProducts", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )})})
        }
    )
    public interface ListRoutingTaskProducts {}

    @Screen(name = "EditRoutingTaskProduct", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRoutingTaskProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "listRoutingTaskProducts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortGoodStandard", list = "allRoutingProductLinks", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "workEffortId"), @FieldMap(fieldName = "workEffortGoodStdTypeId", value = "PRUNT_PROD_DELIV")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortGoodStandard", valueField = "routingProductLink")
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRoutingTaskProduct", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )})})
        }
    )
    public interface EditRoutingTaskProduct {}

    @Screen(name = "EditRoutingTaskAssoc", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRoutingTaskAssoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "routingTaskAssoc")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "workEffortAssocTypeId", value = "ROUTING_COMPONENT")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAssocView", list = "allRoutingTasks", fieldMaps = {@FieldMap(fieldName = "workEffortIdFrom", fromField = "workEffortId"), @FieldMap(fieldName = "workEffortAssocTypeId", fromField = "workEffortAssocTypeId")}, orderBy = {"sequenceNum", "fromDate"})
    @DecoratorScreen(
        name = "CommonRoutingDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/routing/EditRoutingTaskAssoc.ftl"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListRoutingTaskAssoc}", includeForms = {
                    @IncludeForm(name = "ListRoutingTaskAssoc", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )})})
        }
    )
    public interface EditRoutingTaskAssoc {}

    @Screen(name = "EditRoutingProductLink", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRoutingProductLink")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "routingProductLink")
    @Action(type = ActionType.SET, field = "workEffortGoodStdTypeId", value = "ROU_PROD_TEMPLATE")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortGoodStandard", list = "allRoutingProductLinks", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "workEffortId"), @FieldMap(fieldName = "workEffortGoodStdTypeId", fromField = "workEffortGoodStdTypeId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortGoodStandard", valueField = "routingProductLink")
    @DecoratorScreen(
        name = "CommonRoutingDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRoutingProductLink", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
            )}, screenlets = {
                @Screenlet(name = "EditRoutingProductLinkPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditRoutingProductLink", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditRoutingProductLink {}

    @Screen(name = "EditRoutingTaskFixedAssets", location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRoutingTaskFixedAsset")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editRoutingTaskFixedAssets")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortFixedAssetStd", list = "allFixedAssets", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "workEffortId")})
    @DecoratorScreen(
        name = "CommonRoutingTaskDecorator",
        location = "component://manufacturing/widget/manufacturing/RoutingScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRoutingTaskFixedAssets", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.ManufacturingRoutingTasks} ${uiLabelMap.ManufacturingRoutingTaskFixedAssets}", name = "EditRoutingTaskFixedAssetPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditRoutingTaskFixedAsset", location = "component://manufacturing/widget/manufacturing/RoutingTaskForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditRoutingTaskFixedAssets {}

}
