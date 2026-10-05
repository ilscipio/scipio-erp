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
public class PayGradeScreens {

    @Screen(name = "FindPayGrades", location = "component://humanres/widget/PayGradeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindPayGrade")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PayGrade")
    @Action(type = ActionType.SET, field = "payGradeId", fromField = "parameters.payGradeId")
    @Action(type = ActionType.SET, field = "payGradeCtx", fromField = "parameters")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewPayGrade}", style = "${styles.link_nav} ${styles.action_add}", target = "EditPayGrade"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPayGrades", location = "component://humanres/widget/forms/PayGradeForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPayGrades", location = "component://humanres/widget/forms/PayGradeForms.xml"
                        )}))})})
        }
    )
    public interface FindPayGrades {}

    @Screen(name = "EditPayGrade", location = "component://humanres/widget/PayGradeScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PayGrade")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "EditPayGrade")
    @Action(type = ActionType.SET, field = "payGradeId", fromField = "parameters.payGradeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PayGrade", valueField = "payGrade")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.payGrade ? 'HumanResEditPayGrade' : 'HumanResNewPayGrade'}")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle} ${payGradeId}")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditPayGrade", location = "component://humanres/widget/forms/PayGradeForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.payGradeId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "SalaryBar", location = "component://humanres/widget/HumanresMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface EditPayGrade {}

    @Screen(name = "EditSalarySteps", location = "component://humanres/widget/PayGradeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditSalaryStep")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PayGrade")
    @Action(type = ActionType.SET, field = "payGradeId", fromField = "parameters.payGradeId")
    @Action(type = ActionType.SET, field = "salaryStepSeqId", fromField = "parameters.salaryStepSeqId")
    @DecoratorScreen(
        name = "GlobalHRSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSalarySteps", location = "component://humanres/widget/forms/PayGradeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddSalaryStep} [${payGradeId}]", name = "AddSalaryStepPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSalaryStep", location = "component://humanres/widget/forms/PayGradeForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.payGradeId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "SalaryBar", location = "component://humanres/widget/HumanresMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface EditSalarySteps {}

}
