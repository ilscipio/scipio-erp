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
public class EntitySyncScreens {

    @Screen(name = "GenericDecorator", location = "component://webtools/widget/EntitySyncScreens.xml")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonRefresh}", style = "${styles.link_run_sys} ${styles.action_reload}", target = "EntitySyncStatus"
                )}, position = 0)})
        }
    )
    public interface GenericDecorator {}

    @Screen(name = "EntitySyncStatus", location = "component://webtools/widget/EntitySyncScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntitySyncStatus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entitySyncStatus")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "component://webtools/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonRefresh}", style = "${styles.link_run_sys} ${styles.action_reload}", target = "EntitySyncStatus"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EntitySyncStatus", location = "component://webtools/widget/EntitySyncForms.xml"
                    )}, position = 1),
                    @Screenlet(title = "${uiLabelMap.WebtoolsLoadOfflineData}", includeForms = {
                        @IncludeForm(name = "EntitySyncLoadOffline", location = "component://webtools/widget/EntitySyncForms.xml"
                    )}, position = 2)})
        }
    )
    public interface EntitySyncStatus {}

}
