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
package com.ilscipio.scipio.humanres.widget;

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
public class EmplLeaveScreens {

    @Screen(name = "FindEmplLeaves", location = "component://humanres/widget/EmplLeaveScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#EmplLeave")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindEmplLeave")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmployeeLeave")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "leaveTypeId", fromField = "parameters.leaveTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.SET, field = "emplLeaveCtx", fromField = "parameters")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewLeave}", style = "${styles.link_nav} ${styles.action_add}", target = "EditEmplLeave"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindEmplLeaves", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplLeaves", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                        )}))})})
        }
    )
    public interface FindEmplLeaves {}

    @Screen(name = "FindLeaveApprovals", location = "component://humanres/widget/EmplLeaveScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "EmplLeave")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindApprovals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Approval")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SET, field = "approverPartyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindLeaveApprovals", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLeaveApprovals", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                    )}))})})
        }
    )
    public interface FindLeaveApprovals {}

    @Screen(name = "EditEmplLeave", location = "component://humanres/widget/EmplLeaveScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmplLeave")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmployeeLeave")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "leaveTypeId", fromField = "parameters.leaveTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplLeave", valueField = "leaveApp", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "leaveTypeId", fromField = "leaveTypeId"), @FieldMap(fieldName = "fromDate", fromField = "fromDate")})
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "EmplLeave")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"leaveApp"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.HumanResAddEmplLeave}", name = "AddEmplLeavePanel", collapsible = true, includeForms = {
                            @IncludeForm(name = "EditEmplLeave", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                        )})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(name = "AddEmplLeavePanel", collapsible = true, includeForms = {
                                @IncludeForm(name = "EditEmplLeave", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                            )})}))})
        }
    )
    public interface EditEmplLeave {}

    @Screen(name = "EditEmplLeaveStatus", location = "component://humanres/widget/EmplLeaveScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditApprovalStatus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Approval")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "leaveTypeId", fromField = "parameters.leaveTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplLeave", valueField = "leaveApp", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "leaveTypeId", fromField = "leaveTypeId"), @FieldMap(fieldName = "fromDate", fromField = "fromDate")})
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "EmplLeave")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "EditEmplLeaveStatus", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditEmplLeaveStatus", location = "component://humanres/widget/forms/EmplLeaveForms.xml"
                )})})
        }
    )
    public interface EditEmplLeaveStatus {}

}
