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
public class WorkEffortScreens {

    @Screen(name = "UserJobs", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "userJobs")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortJobList")
    @Action(type = ActionType.SET, field = "filterByStatusId", fromField = "parameters.statusId", defaultValue = "SERVICE_RUNNING")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobSandbox", list = "userJobs", conditions = {@ConditionExpr(fieldName = "authUserLoginId", fromField = "userLogin.userLoginId"), @ConditionExpr(fieldName = "statusId", fromField = "filterByStatusId")})
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "FilterUserJobs", location = "component://workeffort/widget/WorkEffortForms.xml"
                ),
                @IncludeForm(name = "UserJobsList", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})})
        }
    )
    public interface UserJobs {}

    @Screen(name = "mytasks", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "task")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewActivityAndTaskList")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivities")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivitiesByRole")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedActivitiesByGroup")
    @Action(type = ActionType.SERVICE, serviceName = "getWorkEffortAssignedTasks")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonWorkEffortAppBasePermCond", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"workEffortGenericPermission", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"workEffortGenericPermission", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/task/mytasks.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WorkEffortViewPermissionError}", style = "h3"
                )}))})
        }
    )
    public interface mytasks {}

    @Screen(name = "EditWorkEffort", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/workeffort/control/ListWorkEfforts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "thisWorkEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.workEffort ? 'PageTitleEditWorkEffort' : 'PageTitleAddWorkEffort'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.workEffort ? 'WorkEffort' : 'NewWorkEffort'}")
    @Action(type = ActionType.SET, field = "labelTitleProperty", fromField = "titleProperty")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EditWorkEffortSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"workEffort"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditWorkEffort", location = "component://workeffort/widget/WorkEffortForms.xml"
                        )})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(includeForms = {
                                @IncludeForm(name = "EditWorkEffort", location = "component://workeffort/widget/WorkEffortForms.xml"
                            )})})),
                            @InlineSection(actions = @Actions(value = {
                                @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "workEffortStatus", fieldMaps = {
                                    @FieldMap(fieldName = "statusId", fromField = "workEffort.currentStatusId"
                                )}),
                                @Action(type = ActionType.ENTITY_AND, entityName = "StatusItem", list = "workEffortStatusList", fieldMaps = {
                                    @FieldMap(fieldName = "statusTypeId", fromField = "workEffortStatus.statusTypeId"
                                )}, orderBy = {"sequenceId"})}), widgets = @InlineWidgets(screenlets = {
                                    @Screenlet(title = "${uiLabelMap.WorkEffortDuplicateWorkEffort}", htmlTemplates = {
                                        @HtmlTemplate(location = "component://workeffort/webapp/workeffort/workeffort/EditWorkEffortDupForm.ftl"
                                    )})}))})
        }
    )
    public interface EditWorkEffort {}

    @Screen(name = "FindWorkEffort", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindWorkEffort")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindWorkEffort")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "FindWorkEffortSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindWorkEffort", location = "component://workeffort/widget/WorkEffortForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FoundWorkEfforts", location = "component://workeffort/widget/WorkEffortForms.xml"
                    )}))})})
        }
    )
    public interface FindWorkEffort {}

    @Screen(name = "ListWorkEfforts", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEfforts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListWorkEfforts", location = "component://workeffort/widget/WorkEffortForms.xml"
                )})})
        }
    )
    public interface ListWorkEfforts {}

    @Screen(name = "ChildWorkEfforts", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleTreeWorkEfforts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAssocs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEfforts")
    @Action(type = ActionType.SET, field = "trail", fromField = "requestParameters.trail")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "ChildWorkEfforts?workEffortId=${parameters.workEffortId}")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ChildWorkEffortsSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_TREE, name = "TreeWorkEffort", location = "component://workeffort/widget/WorkEffortTrees.xml"
            )})
        }
    )
    public interface ChildWorkEfforts {}

    @Screen(name = "AddWorkEffortAndAssoc", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWorkEffort")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "ChildWorkEfforts?workEffortId=${parameters.workEffortIdFrom}")
    @Action(type = ActionType.SET, field = "workEffortIdFrom", fromField = "parameters.workEffortIdFrom")
    @Action(type = ActionType.SET, field = "workEffortAssocTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "nowTimestamp")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleAddWorkEffort} ${uiLabelMap.WorkEffortAssociatedFromParentToChild}")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddWorkEffortAndAssoc", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})
        }
    )
    public interface AddWorkEffortAndAssoc {}

    @Screen(name = "EditWorkEffortAndAssoc", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWorkEffort")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE")
    @Action(type = ActionType.SET, field = "workEffortIdTo", fromField = "parameters.workEffortIdTo")
    @Action(type = ActionType.SET, field = "workEffortIdFrom", fromField = "parameters.workEffortIdFrom")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortIdTo", defaultValue = "${parameters.workEffortId}")
    @Action(type = ActionType.SET, field = "workEffortAssocTypeId", fromField = "parameters.workEffortAssocTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortAssoc", valueField = "workEffortAssoc")
    @Action(type = ActionType.SET, field = "parameters.workEffortId", fromField = "workEffortId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditWorkEffort} ${uiLabelMap.WorkEffortAssociatedFromParentToChild}")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EditWorkEffortAssocSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "EditWorkEffortAndAssoc", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})
        }
    )
    public interface EditWorkEffortAndAssoc {}

    @Screen(name = "AddWorkEffortAssoc", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWorkEffortAssoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAssocs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWorkEffortAssoc")
    @Action(type = ActionType.SET, field = "workEffortIdFrom", fromField = "parameters.workEffortIdFrom")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortAssoc", valueField = "workEffortAssoc")
    @Action(type = ActionType.SET, field = "donePage", value = "ChildWorkEfforts?workEffortId=${workEffortIdFrom}&trail=${workEffortIdFrom}")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "workEffortIdFrom")
    @Action(type = ActionType.SET, field = "parameters.workEffortId", fromField = "workEffortIdFrom")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddWorkEffortAssoc", location = "component://workeffort/widget/WorkEffortForms.xml"
                )})})
        }
    )
    public interface AddWorkEffortAssoc {}

    @Screen(name = "EditWorkEffortAssoc", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWorkEffortAssoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAssocs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWorkEffortAssoc")
    @Action(type = ActionType.SET, field = "workEffortIdTo", fromField = "parameters.workEffortIdTo")
    @Action(type = ActionType.SET, field = "workEffortIdFrom", fromField = "parameters.workEffortIdFrom")
    @Action(type = ActionType.SET, field = "workEffortAssocTypeId", fromField = "parameters.workEffortAssocTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortAssoc", valueField = "workEffortAssoc")
    @Action(type = ActionType.SET, field = "donePage", value = "ChildWorkEfforts?workEffortId=${workEffortIdFrom}&trail=${workEffortIdFrom}")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "workEffortIdFrom")
    @Action(type = ActionType.SET, field = "parameters.workEffortId", fromField = "workEffortIdFrom")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditWorkEffortAssoc", location = "component://workeffort/widget/WorkEffortForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "EditWorkEffortAssocSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml", position = 0
                )})})
        }
    )
    public interface EditWorkEffortAssoc {}

    @Screen(name = "ListWorkEffortPartyAssigns", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortPartyAssigns")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortPartyAssigns")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortPartyAssigns")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortPartyAssignment", valueField = "workEffortPartyAssignment")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortPartyAssigns", location = "component://workeffort/widget/WorkEffortPartyAssignForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortPartyAssign}", name = "AddWorkEffortPartyAssignsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditWorkEffortPartyAssign", location = "component://workeffort/widget/WorkEffortPartyAssignForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortPartyAssigns {}

    @Screen(name = "ListWorkEffortFixedAssetAssigns", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortFixedAssetAssigns")
    @Action(type = ActionType.SET, field = "labelTitleProperty", fromField = "titleProperty")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortFixedAssetAssigns")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortFixedAssetAssigns", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortFixedAssetAssign}", name = "AddWorkEffortFixedAssetAssign", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditWorkEffortFixedAssetAssign", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortFixedAssetAssigns {}

    @Screen(name = "EditWorkEffortRates", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortAssignmentRates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortRates")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortAssignmentRates")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortRates", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortAssignmentRate}", name = "AddWorkEffortRatesPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortRate", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortRates {}

    @Screen(name = "ListWorkEffortCommEvents", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortCommEvents")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortCommEvents")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortCommEvents", location = "component://workeffort/widget/WorkEffortCommEventForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortCommEvent}", name = "AddWorkEffortCommEventsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortCommEvent", location = "component://workeffort/widget/WorkEffortCommEventForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortCommEvents {}

    @Screen(name = "ListWorkEffortShopLists", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortShopLists")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortShopLists")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortShopLists")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortShopLists", location = "component://workeffort/widget/WorkEffortShopListForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortShopList}", name = "AddWorkEffortShopListsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortShopList", location = "component://workeffort/widget/WorkEffortShopListForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortShopLists {}

    @Screen(name = "ListWorkEffortRequests", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortRequests")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortRequests")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortRequests")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortRequests", location = "component://workeffort/widget/WorkEffortRequestForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortRequestItems", location = "component://workeffort/widget/WorkEffortRequestForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortRequest}", name = "AddWorkEffortRequestsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortRequest", location = "component://workeffort/widget/WorkEffortRequestForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortRequestItem}", name = "AddWorkEffortRequestItemPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortRequestItem", location = "component://workeffort/widget/WorkEffortRequestForms.xml"
                )}, position = 2)})
        }
    )
    public interface ListWorkEffortRequests {}

    @Screen(name = "ListWorkEffortRequirements", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortRequirements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortRequirements")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortRequirements")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/workeffort/control/ListWorkEfforts")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortRequirements", location = "component://workeffort/widget/WorkEffortRequirementForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortRequirement}", name = "AddWorkEffortRequirementsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortRequirement", location = "component://workeffort/widget/WorkEffortRequirementForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortRequirements {}

    @Screen(name = "ListWorkEffortQuotes", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortQuotes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortQuotes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortQuotes")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortQuotes", location = "component://workeffort/widget/WorkEffortQuoteForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortQuoteItems", location = "component://workeffort/widget/WorkEffortQuoteForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortQuote}", name = "AddWorkEffortQuotesPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortQuote", location = "component://workeffort/widget/WorkEffortQuoteForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortQuoteItem}", name = "AddWorkEffortQuoteItemPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortQuoteItem", location = "component://workeffort/widget/WorkEffortQuoteForms.xml"
                )}, position = 2)})
        }
    )
    public interface ListWorkEffortQuotes {}

    @Screen(name = "ListWorkEffortOrderHeaders", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortOrderHeaders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortOrderHeaders")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortOrderHeaders")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortOrderHeaders", location = "component://workeffort/widget/WorkEffortOrderHeaderForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortOrderHeader}", name = "AddWorkEffortOrderHeadersPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortOrderHeader", location = "component://workeffort/widget/WorkEffortOrderHeaderForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortOrderHeaders {}

    @Screen(name = "EditWorkEffortTimeEntries", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortTimeEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortTimeEntries")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortTimeEntries")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListWorkEffortTimeEntries", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortTimeEntry}", includeForms = {
                    @IncludeForm(name = "AddWorkEffortTimeEntry", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortTimeToInvoice}", includeForms = {
                    @IncludeForm(name = "AddWorkEffortTimeToInvoice", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortTimeToNewInvoice}", includeForms = {
                    @IncludeForm(name = "AddWorkEffortTimeToNewInvoice", location = "component://workeffort/widget/WorkEffortForms.xml"
                )})})
        }
    )
    public interface EditWorkEffortTimeEntries {}

    @Screen(name = "EditWorkEffortNotes", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortNotes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortNotes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortNotes")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "noteId", fromField = "parameters.noteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffortNoteAndData", valueField = "workEffortNoteAndData")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortNotes", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortNotes}", name = "add-workeffort-note", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortNote", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortNotes {}

    @Screen(name = "EditWorkEffortContents", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWorkEffortContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortContents")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortContents", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddContent}", name = "AddWorkEffortContentsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortContent", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortContents {}

    @Screen(name = "EditWorkEffortGoodStandards", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWorkEffortGoodStandards")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortGoodStandards")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortGoodStandards", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.WorkEffortAddGoodStandard}", name = "AddWorkEffortGoodStandardsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortGoodStandard", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortGoodStandards {}

    @Screen(name = "EditWorkEffortReviews", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortReviews")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortReviews")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortReviews")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "defaultUserLoginId", fromField = "parameters.userLogin.userLoginId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortReviews", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortReviews}", name = "AddWorkEffortReviewsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortReview", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortReviews {}

    @Screen(name = "EditWorkEffortKeywords", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortKeyword")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortKeywords")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListWorkEffortKeyword")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortKeywords", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortKeyword}", name = "AddWorkEffortKeywordsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortKeyword", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortKeywords {}

    @Screen(name = "EditWorkEffortContactMechs", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWorkEffortContactMechs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortContactMechs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWorkEffortContactMechs")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortContactMechs", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonCreate}", name = "AddWorkEffortContactMechsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortContactMech", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWorkEffortContactMechs {}

    @Screen(name = "WorkEffortSearchResults", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAdvancedSearch")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/find/WorkEffortSearchResults.groovy")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FindWorkEffortSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/find/WorkEffortSearchResults.ftl"
            )})
        }
    )
    public interface WorkEffortSearchResults {}

    @Screen(name = "WorkEffortSearchOptions", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAdvancedSearch")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/find/WorkEffortSearchOptions.groovy")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FindWorkEffortSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/find/WorkEffortSearchOptions.ftl"
            )})
        }
    )
    public interface WorkEffortSearchOptions {}

    @Screen(name = "EditAgreementWorkEffortApplics", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditAgreementWorkEffortApplics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAgreementAppls")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementWorkEffortApplics", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingAddAgreementWorkEffortApplic}", name = "AddAccountingAgreementWorkEffortApplicsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddAgreementWorkEffortApplic", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditAgreementWorkEffortApplics {}

    @Screen(name = "ListWorkEffortEventReminders", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListWorkEffortEventReminders")
    @Action(type = ActionType.SET, field = "labelTitleProperty", fromField = "titleProperty")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortEventReminders")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @DecoratorScreen(
        name = "CommonWorkEffortDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortEventReminders", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffortEventReminder}", name = "AddWorkEffortEventReminder", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditWorkEffortEventReminder", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}, position = 0)})
        }
    )
    public interface ListWorkEffortEventReminders {}

    @Screen(name = "WorkEffortEventReminderEmail", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "WorkEffortType", toValueField = "workEffortType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "CurrentStatusItem", toValueField = "currentStatusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "WorkEffortPurposeType", toValueField = "workEffortPurposeType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "ScopeEnumeration", toValueField = "scopeEnumeration")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortPartyAssignView", list = "partyAssignments", fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAndFixedAssetAssign", list = "fixedAssetAssignments", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "workEffortId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/workeffort/EventReminderEmail.ftl")}))
    public interface WorkEffortEventReminderEmail {}

    @Screen(name = "FindICalendars", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortICalendar")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortICalendarFind")
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"workEffortManagerPermission", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "FindICalendarsSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
                ),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListIcalendars", location = "component://workeffort/widget/WorkEffortForms.xml"
            )}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WorkEffortViewPermissionError}", style = "common-msg-error-perm"
            )}))})
        }
    )
    public interface FindICalendars {}

    @Screen(name = "EditICalendar", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(order = 0, type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(order = 1, type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffort")
    @Action(order = 2, type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(order = 3, type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(order = 4, type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarEdit} - ${workEffort.workEffortName}")
    @IfAction(order = 5, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"workEffort"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarAdd}")}))
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"workEffort"})}), actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "quickAssignPartyId", fromField = "userLogin.partyId"
                    )}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditICalendar", location = "component://workeffort/widget/WorkEffortForms.xml"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "EditICalendarsSubTabBar", location = "component://workeffort/widget/WorkEffortMenus.xml"
                    ),
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditICalendar", location = "component://workeffort/widget/WorkEffortForms.xml"
                )}))})
        }
    )
    public interface EditICalendar {}

    @Screen(name = "EditICalendarData", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ICalendarData")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "WorkEffortIcalData", toValueField = "iCalData")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarEditData} - ${workEffort.workEffortName}")
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditICalendarData", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})
        }
    )
    public interface EditICalendarData {}

    @Screen(name = "ICalendarChildren", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortAssocs")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "trail", fromField = "requestParameters.trail")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleTreeWorkEfforts} - ${workEffort.workEffortName}")
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_TREE, name = "ICalendarTree", location = "component://workeffort/widget/WorkEffortTrees.xml"
            )})
        }
    )
    public interface ICalendarChildren {}

    @Screen(name = "ICalendarParties", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortPartyAssigns")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarAddParty} - ${workEffort.workEffortName}")
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditICalendarPartyAssign", location = "component://workeffort/widget/WorkEffortForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListICalendarPartyAssigns", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})
        }
    )
    public interface ICalendarParties {}

    @Screen(name = "ICalendarFixedAssets", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WorkEffortFixedAssetAssigns")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarAddFixedAsset} - ${workEffort.workEffortName}")
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditICalendarFixedAssetAssign", location = "component://workeffort/widget/WorkEffortForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListICalendarFixedAssetAssigns", location = "component://workeffort/widget/WorkEffortForms.xml"
            )})
        }
    )
    public interface ICalendarFixedAssets {}

    @Screen(name = "ICalendarHelp", location = "component://workeffort/widget/WorkEffortScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ICalendarHelp")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WorkEffortICalendarHelp}")
    @Action(type = ActionType.SET, field = "document", fromField = "dom:readHtmlDocument(uiLabelMap.WorkEffortICalendarHelpUrl)")
    @Action(type = ActionType.SET, field = "wikiDivList", fromField = "document[\"//DIV[@class='wiki-content']\"]")
    @Action(type = ActionType.SET, field = "wikiContent", fromField = "dom:toHtmlString(wikiDivList, 'UTF-8', true, 2)")
    @Action(type = ActionType.SET, field = "wikiContent", value = "${groovy: context?.wikiContent.replaceAll('(?i)ofbiz', 'Scipio')}")
    @DecoratorScreen(
        name = "iCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/workeffort/ICalendarHelp.ftl"
            )})
        }
    )
    public interface ICalendarHelp {}

}
