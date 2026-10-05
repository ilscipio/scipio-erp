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
package com.ilscipio.scipio.commonext.widget;

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
public class SystemInfoScreens {

    @Screen(name = "SystemInfoNotes", location = "component://commonext/widget/SystemInfoScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonExtUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getSystemInfoNotes", resultMapName = "resultMap")
    @Action(type = ActionType.SET, field = "systemInfoNotes", fromField = "resultMap.systemInfoNotes")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonExtSystemInfoNoteForUser} ${userLogin.partyId}", includeForms = {@IncludeForm(name = "SystemInfoNotes", location = "component://commonext/widget/SystemInfoForms.xml", position = 2)}, includeMenus = {@IncludeMenu(name = "SystemInfoNotes", location = "component://commonext/widget/SystemInfoMenus.xml", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.createPublicMsg"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "CreateSystemInfoNote", location = "component://commonext/widget/SystemInfoForms.xml")}), position = 0)})}))
    public interface SystemInfoNotes {}

    @Screen(name = "SystemInfoStatus", location = "component://commonext/widget/SystemInfoScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonExtUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getSystemInfoStatus", resultMapName = "resultMap")
    @Action(type = ActionType.SET, field = "systemInfoStatus", fromField = "resultMap.systemInfoStatus")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"systemInfoStatus"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonExtSystemInfoStatusForUser} ${userLogin.partyId}", includeForms = {@IncludeForm(name = "SystemInfoStatus", location = "component://commonext/widget/SystemInfoForms.xml")})}))
    public interface SystemInfoStatus {}

}
