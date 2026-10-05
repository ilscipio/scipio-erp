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
public class PortalAdmScreens {

    @Screen(name = "FindPortalPage", location = "component://webtools/widget/PortalAdmScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "portalAdmin")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindPortalPage")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PORTALPAGE", "_MAINT"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindPortalPages", location = "component://webtools/widget/PortalAdmForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPortalPages", location = "component://webtools/widget/PortalAdmForms.xml"
                    )}))})), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PortalPageViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindPortalPage {}

    @Screen(name = "CreatePortalPage", location = "component://webtools/widget/PortalAdmScreens.xml")
    @Action(type = ActionType.SET, field = "targetPortalPage", value = "createPortalPageAdm")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "portalAdmin")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "portalPage.portalPageId"
                ),
                @Action(type = ActionType.SET, field = "editPortalPageId", value = "Y"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditPortalPage", location = "component://webtools/widget/PortalAdmForms.xml"
            )}))})
        }
    )
    public interface CreatePortalPage {}

    @Screen(name = "EditPortalPage", location = "component://webtools/widget/PortalAdmScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "portalAdmin")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/myportal.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/images/myportal.css", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPage", valueField = "portalPage")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonPortalEditPage}: ${portalPage.portalPageName} [${portalPage.portalPageId}]", widgets = {
                    @Widget(type = WidgetType.INCLUDE_PORTAL_PAGE, id = "${portalPage.portalPageId}", confMode = "true", usePrivate = "false"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"portalPage"})}), actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "targetPortalPage", value = "updatePortalPageAdm"
                        )}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.CommonPortalEditPage}", includeForms = {
                                @IncludeForm(name = "EditPortalPage", location = "component://webtools/widget/PortalAdmForms.xml", position = 1
                            )}, includeMenus = {
                                @IncludeMenu(name = "PortalPageAdmin", location = "component://webtools/widget/Menus.xml", position = 0
                            )})}), position = 0)})
        }
    )
    public interface EditPortalPage {}

}
