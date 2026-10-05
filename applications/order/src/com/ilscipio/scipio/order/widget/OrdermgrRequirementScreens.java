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
public class OrdermgrRequirementScreens {

    @Screen(name = "FindRequirements", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindRequirements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindRequirements")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonRequirementsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml"
                    )}))})})
        }
    )
    public interface FindRequirements {}

    @Screen(name = "ApproveRequirements", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindNotApprovedRequirements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ApproveRequirements")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/requirement/SelectCreatedProposed.groovy")
    @DecoratorScreen(
        name = "CommonRequirementsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindNotApprovedRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ApproveRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml"
                    )}))})})
        }
    )
    public interface ApproveRequirements {}

    @Screen(name = "ApprovedProductRequirements", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindApprovedProductRequirements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ApprovedProductRequirements")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "_rowSubmit", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/requirement/ApprovedProductRequirements.groovy")
    @DecoratorScreen(
        name = "CommonRequirementsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleFindApprovedProductRequirements}", includeForms = {
                    @IncludeForm(name = "FindApprovedProductRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})}, sections = {
                    @InlineSection(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "genericLinkName", value = "print"
                    ),
                    @Action(type = ActionType.SET, field = "genericLinkText", value = "${uiLabelMap.CommonPrint}"
                ),
                @Action(type = ActionType.SET, field = "genericLinkTarget", value = "ApprovedProductRequirementsReport"
            ),
            @Action(type = ActionType.SET, field = "genericLinkStyle", value = "${styles.link_run_sys} ${styles.action_export}"
            ),
            @Action(type = ActionType.SET, field = "genericLinkWindow", value = "reportWindow"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "genericLink", location = "component://common/widget/CommonScreens.xml"
            )})),
            @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                @Condition(type = Empty.class, params = {"parameters.partyId"
            })}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.OrderRequirementsList}", includeForms = {
                    @IncludeForm(name = "ApprovedProductRequirements", location = "component://order/widget/ordermgr/RequirementForms.xml", position = 0
                ),
                @IncludeForm(name = "ApprovedProductRequirementsSubmit", location = "component://order/widget/ordermgr/RequirementForms.xml", position = 2
            )}, screenlets = {
                @ScreenletNested(includeForms = {
                    @IncludeForm(name = "ApprovedProductRequirementsSummary", location = "component://order/widget/ordermgr/RequirementForms.xml"
                
            )}, position = 1)})}), failWidgets = @InlineWidgets(screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleFindApprovedProductRequirements}", includeForms = {
                    @IncludeForm(name = "ApprovedProductRequirementsList", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})}))})
        }
    )
    public interface ApprovedProductRequirements {}

    @Screen(name = "ApprovedProductRequirementsReport", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "pageLayoutName", value = "simple-landscape")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleApprovedProductRequirements")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ApprovedProductRequirementsList", location = "component://order/widget/ordermgr/RequirementForms.xml"
            )})
        }
    )
    public interface ApprovedProductRequirementsReport {}

    @Screen(name = "ApprovedProductRequirementsByVendor", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindApprovedRequirementsBySupplier")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ApprovedProductRequirementsByVendor")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/requirement/ApprovedProductRequirementsByVendor.groovy")
    @DecoratorScreen(
        name = "CommonRequirementsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ApprovedProductRequirementsByVendor", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})})
        }
    )
    public interface ApprovedProductRequirementsByVendor {}

    @Screen(name = "ApprovedProductRequirementsByVendorReport", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "pageLayoutName", value = "simple-landscape")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleApprovedProductRequirementsByVendor")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ApprovedProductRequirementsByVendor", location = "component://order/widget/ordermgr/RequirementForms.xml"
            )})
        }
    )
    public interface ApprovedProductRequirementsByVendorReport {}

    @Screen(name = "EditRequirement", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditRequirement")
    @Action(type = ActionType.SET, field = "requirementId", fromField = "parameters.requirementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Requirement", valueField = "requirement")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.requirementId ? 'PageTitleEditRequirement' : 'OrderNewRequirement'}")
    @DecoratorScreen(
        name = "CommonRequirementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRequirement", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})})
        }
    )
    public interface EditRequirement {}

    @Screen(name = "ListRequirementCustRequests", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRequirementCustRequests")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListRequirementCustRequests")
    @Action(type = ActionType.SET, field = "requirementId", fromField = "parameters.requirementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Requirement", valueField = "requirement")
    @Action(type = ActionType.ENTITY_AND, entityName = "RequirementCustRequest", list = "requirementCustRequests", fieldMaps = {@FieldMap(fieldName = "requirementId", fromField = "requirementId")})
    @DecoratorScreen(
        name = "CommonRequirementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListRequirementCustRequests", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})})
        }
    )
    public interface ListRequirementCustRequests {}

    @Screen(name = "ListRequirementOrders", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRequirementOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListRequirementOrdersTab")
    @Action(type = ActionType.SET, field = "requirementId", fromField = "parameters.requirementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Requirement", valueField = "requirement")
    @Action(type = ActionType.ENTITY_AND, entityName = "OrderRequirementCommitment", list = "orderRequirements", fieldMaps = {@FieldMap(fieldName = "requirementId", fromField = "requirementId")})
    @DecoratorScreen(
        name = "CommonRequirementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListRequirementOrders", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})})
        }
    )
    public interface ListRequirementOrders {}

    @Screen(name = "ListRequirementRoles", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRequirementRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListRequirementRolesTab")
    @Action(type = ActionType.SET, field = "requirementId", fromField = "parameters.requirementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Requirement", valueField = "requirement")
    @Action(type = ActionType.ENTITY_AND, entityName = "RequirementRole", list = "requirementRoles", fieldMaps = {@FieldMap(fieldName = "requirementId", fromField = "requirementId")})
    @DecoratorScreen(
        name = "CommonRequirementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListRequirementRoles", location = "component://order/widget/ordermgr/RequirementForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRequirementRole"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderAutoAssign}", style = "${styles.link_run_sys} ${styles.action_update}", target = "autoAssignRequirementToSupplier"
                )}, position = 0)})})
        }
    )
    public interface ListRequirementRoles {}

    @Screen(name = "EditRequirementRole", location = "component://order/widget/ordermgr/RequirementScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditRequirementRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListRequirementRolesTab")
    @Action(type = ActionType.SET, field = "requirementId", fromField = "parameters.requirementId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Requirement", valueField = "requirement")
    @Action(type = ActionType.ENTITY_ONE, entityName = "RequirementRole", valueField = "requirementRole")
    @DecoratorScreen(
        name = "CommonRequirementDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRequirementRole", location = "component://order/widget/ordermgr/RequirementForms.xml"
                )})})
        }
    )
    public interface EditRequirementRole {}

}
