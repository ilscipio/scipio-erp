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
public class TempExprScreens {

    @Screen(name = "FindTemporalExpression", location = "component://webtools/widget/TempExprScreens.xml")
    @Action(type = ActionType.SET, field = "tabMenuItem", value = "findExpression")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${titleProperty}", defaultValue = "TemporalExpressionFind")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"tempExprPermissionCheck", "CREATE"})}))
    @DecoratorScreen(
        name = "TemporalExpressionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"tempExprPermissionCheck", "CREATE"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "editTemporalExpression"
                    )})}))}, decorators = {
                        @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                            @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindTemporalExpression", location = "component://webtools/widget/tempExprForms.xml"
                            )})),
                            @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTemporalExpressions", location = "component://webtools/widget/tempExprForms.xml"
                            )}))})})
        }
    )
    public interface FindTemporalExpression {}

    @Screen(name = "EditTemporalExpression", location = "component://webtools/widget/TempExprScreens.xml")
    @Action(type = ActionType.SET, field = "tabMenuItem", value = "editExpression")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${titleProperty}", defaultValue = "TemporalExpressionMaintenance")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TemporalExpression", valueField = "temporalExpression")
    @Action(type = ActionType.SET, field = "fromTempExprId", fromField = "parameters.tempExprId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TemporalExpressionChild", list = "childExpressionList", conditions = {@ConditionExpr(fieldName = "fromTempExprId", fromField = "fromTempExprId")})
    @DecoratorScreen(
        name = "TemporalExpressionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "TempExprTabBar", location = "component://webtools/widget/Menus.xml"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/tempexpr/tempExprMaint.ftl"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"childExpressionList"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListChildExpressions", location = "component://webtools/widget/tempExprForms.xml"
                )}))})
        }
    )
    public interface EditTemporalExpression {}

}
