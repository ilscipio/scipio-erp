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
public class GlobalHRSettingScreens {

    @Screen(name = "EditSkillTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SkillType")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListSkillTypes")
    @Action(type = ActionType.SET, field = "skillTypeId", fromField = "parameters.skillTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SkillType", valueField = "skillType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSkillTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddSkillType}", name = "AddSkillTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSkillType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditSkillTypes {}

    @Screen(name = "EditResponsibilityTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditResponsibilityType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ResponsibilityType")
    @Action(type = ActionType.SET, field = "responsibilityTypeId", fromField = "parameters.responsibilityTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ResponsibilityType", valueField = "responsibilityType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListResponsibilityTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplPositionResponsibility}", name = "AddResponsibilityTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddResponsibilityType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditResponsibilityTypes {}

    @Screen(name = "EditTerminationTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TerminationType")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResTerminationTypes")
    @Action(type = ActionType.SET, field = "terminationTypeId", fromField = "parameters.terminationTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TerminationType", valueField = "terminationType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTerminationTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddTerminationType}", name = "AddTerminationTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTerminationType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditTerminationTypes {}

    @Screen(name = "FindEmplPositionTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindPositionTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplPositionTypes")
    @Action(type = ActionType.SET, field = "emplPositionTypeId", fromField = "parameters.emplPositionTypeId")
    @Action(type = ActionType.SET, field = "emplPositionTypeCtx", fromField = "parameters")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewEmplPositionType}", style = "${styles.link_nav} ${styles.action_add}", target = "EditEmplPositionTypes"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindEmplPositionTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplPositionTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                        )}))})})
        }
    )
    public interface FindEmplPositionTypes {}

    @Screen(name = "EditEmplPositionTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplPositionTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "EditEmplPositionType")
    @Action(type = ActionType.SET, field = "emplPositionTypeId", fromField = "parameters.emplPositionTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplPositionType", valueField = "emplPositionType")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.emplPositionType ? 'HumanResEditEmplPositionType' : 'HumanResNewEmplPositionType'}")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditEmplPositionTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"emplPositionTypeId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "EmplPositionTypeTabBar", location = "component://humanres/widget/HumanresMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface EditEmplPositionTypes {}

    @Screen(name = "EditEmplPositionTypeRates", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEmplPositionTypeRate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplPositionTypes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "EditEmplPositionTypeRate")
    @Action(type = ActionType.SET, field = "emplPositionTypeId", fromField = "parameters.emplPositionTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplPositionTypeRate", valueField = "emplPositionTypeRate")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EmplPositionTypeTabBar", location = "component://humanres/widget/HumanresMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplPositionTypeRates", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResEmplPositionType} ${uiLabelMap.CommonRate}", name = "AddEmplPositionTypeRatePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplPositionTypeRate", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditEmplPositionTypeRates {}

    @Screen(name = "EditTerminationReasons", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResTerminationReason")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TerminationReason")
    @Action(type = ActionType.SET, field = "terminationReasonId", fromField = "parameters.terminationReasonId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TerminationReason", valueField = "terminationReason")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTerminationReasons", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddTerminationReason}", name = "AddTerminationReasonPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTerminationReason", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditTerminationReasons {}

    @Screen(name = "EditJobInterviewType", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "JobInterviewType")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditJobInterviewType")
    @Action(type = ActionType.SET, field = "jobInterviewTypeId", fromField = "parameters.jobInterviewTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "JobInterviewType", valueField = "interviewType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListJobInterviewType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddJobInterviewType}", name = "AddJobInterviewTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddJobInterviewType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditJobInterviewType {}

    @Screen(name = "EditTrainingTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditTrainingTypes")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTrainingTypes")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTrainingTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResTrainingTypes}", name = "AddTrainingTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddTrainingTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditTrainingTypes {}

    @Screen(name = "EditEmplLeaveTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmplLeaveType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplLeaveType")
    @Action(type = ActionType.SET, field = "leaveTypeId", fromField = "parameters.leaveTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplLeaveType", valueField = "emplLeaveType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EmplLeaveReasonTypeTabBar", location = "component://humanres/widget/HumanresMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplLeaveTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplLeaveType}", name = "AddEmplLeaveTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplLeaveType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditEmplLeaveTypes {}

    @Screen(name = "EditEmplLeaveReasonTypes", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEmplReasonType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplLeaveType")
    @Action(type = ActionType.SET, field = "emplLeaveReasonTypeId", fromField = "parameters.emplLeaveReasonTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplLeaveReasonType", valueField = "emplreasonType")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EmplLeaveReasonTypeTabBar", location = "component://humanres/widget/HumanresMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplLeaveReasonTypes", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplLeaveReasonType}", name = "AddEmplReasonTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplLeaveReasonType", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditEmplLeaveReasonTypes {}

    @Screen(name = "PublicHoliday", location = "component://humanres/widget/GlobalHRSettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePublicHoliday")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "publicHoliday")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPublicHoliday}", name = "addPublicHoliday", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPublicHoliday", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitlePublicHolidayList}", name = "listPublicHoliday", collapsible = true, includeForms = {
                    @IncludeForm(name = "ListPublicHoliday", location = "component://humanres/widget/forms/GlobalHRSettingForms.xml"
                )})})
        }
    )
    public interface PublicHoliday {}

}
