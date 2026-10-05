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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaEventScreens {

    @Screen(name = "main", location = "component://marketing/widget/sfa/EventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SfaEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Events")
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAndPartyAssign", list = "myTasks", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "userLogin.partyId"), @FieldMap(fieldName = "statusId", value = "PRTYASGN_ASSIGNED"), @FieldMap(fieldName = "workEffortTypeId", value = "TASK"), @FieldMap(fieldName = "currentStatusId", value = "CAL_ACCEPTED"), @FieldMap(fieldName = "workEffortParentId", fromField = "null")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortAndPartyAssign", list = "tasksAssignedByMe", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "assignedByUserLoginId", fromField = "userLogin.userLoginId"), @FieldMap(fieldName = "statusId", value = "PRTYASGN_ASSIGNED"), @FieldMap(fieldName = "workEffortTypeId", value = "TASK"), @FieldMap(fieldName = "currentStatusId", value = "CAL_ACCEPTED"), @FieldMap(fieldName = "workEffortParentId", fromField = "null")})
    @DecoratorScreen(
        name = "CommonEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.SfaTaskAssignedToMe}", includeForms = {
                    @IncludeForm(name = "MyTasks", location = "component://marketing/widget/sfa/forms/EventForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.SfaTaskAssignedByMe}", includeForms = {
                    @IncludeForm(name = "TasksAssignedByMe", location = "component://marketing/widget/sfa/forms/EventForms.xml"
                )})})
        }
    )
    public interface main {}

    @Screen(name = "EditEvent", location = "component://marketing/widget/sfa/EventScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortAddCalendarEvent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Events")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "isNewEvent", value = "${groovy: context.workEffort ? false : true}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "createCalEventUrl", fromField = "createCalEventUrl", defaultValue = "createEventWorkEffortAndPartyAssign")
    @Action(type = ActionType.SET, field = "updateCalEventUrl", fromField = "updateCalEventUrl", defaultValue = "updateEventWorkEffort")
    @Action(type = ActionType.SET, field = "cancelEventFormId", value = "cancelEventForm")
    @Action(type = ActionType.SET, field = "completeEventFormId", value = "completeEventForm")
    @DecoratorScreen(
        name = "CommonEventDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditEvent", location = "component://marketing/widget/sfa/forms/EventForms.xml"
                ),
                @IncludeForm(name = "cancelEventHidden", location = "component://marketing/widget/sfa/forms/EventForms.xml"
            ),
            @IncludeForm(name = "completeEventHidden", location = "component://marketing/widget/sfa/forms/EventForms.xml"
            )})})
        }
    )
    public interface EditEvent {}

}
