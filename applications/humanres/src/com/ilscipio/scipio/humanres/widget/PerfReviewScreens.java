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
public class PerfReviewScreens {

    @Screen(name = "FindPerfReviews", location = "component://humanres/widget/PerfReviewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindPerfReview")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PerfReview")
    @Action(type = ActionType.SET, field = "employeePartyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewPartyReview}", style = "${styles.link_nav} ${styles.action_add}", target = "EditPerfReview"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPerfReviews", location = "component://humanres/widget/forms/PerfReviewForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPerfReviews", location = "component://humanres/widget/forms/PerfReviewForms.xml"
                        )}))})})
        }
    )
    public interface FindPerfReviews {}

    @Screen(name = "EditPerfReviews", location = "component://humanres/widget/PerfReviewScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPerfReview")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListPartyReview")
    @Action(type = ActionType.SET, field = "perfReviewId", fromField = "parameters.perfReviewId")
    @Action(type = ActionType.SET, field = "employeePartyId", fromField = "parameters.employeePartyId")
    @Action(type = ActionType.SET, field = "employeeRoleTypeId", fromField = "parameters.employeeRoleTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PerfReview", valueField = "perfReview")
    @DecoratorScreen(
        name = "CommonPerfReviewDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonEdit} ${uiLabelMap.HumanResPerfReview}", includeForms = {
                    @IncludeForm(name = "EditPerfReview", location = "component://humanres/widget/forms/PerfReviewForms.xml"
                )})})
        }
    )
    public interface EditPerfReviews {}

    @Screen(name = "EditPerfReviewItems", location = "component://humanres/widget/PerfReviewScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditPartyReviewItem")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPerfReviewItems")
    @Action(type = ActionType.SET, field = "perfReviewId", fromField = "parameters.perfReviewId")
    @Action(type = ActionType.SET, field = "employeePartyId", fromField = "parameters.employeePartyId")
    @Action(type = ActionType.SET, field = "employeeRoleTypeId", fromField = "parameters.employeeRoleTypeId")
    @DecoratorScreen(
        name = "CommonPerfReviewDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPerfReviewItems", location = "component://humanres/widget/forms/PerfReviewForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPerfReviewItems}", name = "AddPerfReviewItemPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPerfReviewItem", location = "component://humanres/widget/forms/PerfReviewForms.xml"
                )})})
        }
    )
    public interface EditPerfReviewItems {}

}
