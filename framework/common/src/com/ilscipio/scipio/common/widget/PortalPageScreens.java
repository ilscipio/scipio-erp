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
package com.ilscipio.scipio.common.widget;

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
public class PortalPageScreens {

    @Screen(name = "CommonPortletDecorator", location = "component://common/widget/PortalPageScreens.xml")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonPortletDecorator {}

    @Screen(name = "showPortlet", location = "component://common/widget/PortalPageScreens.xml")
    @Section(actions = @Actions(value = {@Action(type = ActionType.ENTITY_ONE, entityName = "PortalPortlet", valueField = "portlet")}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/portal/showPortlet.ftl")}))
    public interface showPortlet {}

    @Screen(name = "showPortletMainDecorator", location = "component://common/widget/PortalPageScreens.xml")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "showPortlet", location = "component://common/widget/PortalPageScreens.xml"
            )})
        }
    )
    public interface showPortletMainDecorator {}

    @Screen(name = "showPortletSimpleDecorator", location = "component://common/widget/PortalPageScreens.xml")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "showPortlet", location = "component://common/widget/PortalPageScreens.xml"
            )})
        }
    )
    public interface showPortletSimpleDecorator {}

    @Screen(name = "showPortalPage", location = "component://common/widget/PortalPageScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/myportal.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/images/myportal.css", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPage", valueField = "portalPage")
    @Action(type = ActionType.SET, field = "title", fromField = "portalPage.portalPageName")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(id = "portalContainerId", widgets = {
                    @Widget(type = WidgetType.INCLUDE_PORTAL_PAGE, id = "${parameters.portalPageId}", confMode = "${parameters.confMode}", usePrivate = "${parameters.usePrivate}"
                )})})
        }
    )
    public interface showPortalPage {}

    @Screen(name = "ManagePortalPages", location = "component://common/widget/PortalPageScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/myportal.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/images/myportal.css", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPage", valueField = "portalPage")
    @Action(type = ActionType.SET, field = "portalPages", value = "${groovy:org.ofbiz.widget.portal.PortalPageWorker.getPortalPages(parameters.parentPortalPageId,context)}")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonPortalPagesForApplication}: ${parameters.parentPortalPageId}", name = "PortalPagesList", collapsible = true, includeForms = {
                    @IncludeForm(name = "ListPortalPages", location = "component://common/widget/PortalPageForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNewPortalPage}", style = "${styles.link_nav} ${styles.action_add}", target = "NewPortalPage"
                    )}, position = 0)})}, sections = {
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"portalPage"})}), widgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.CommonPortalEditPage}: ${portalPage.portalPageName} [${portalPage.portalPageId}]", widgets = {
                                    @Widget(type = WidgetType.INCLUDE_PORTAL_PAGE, id = "${portalPage.portalPageId}", confMode = "true"
                                )})}))})
        }
    )
    public interface ManagePortalPages {}

    @Screen(name = "AddPortlet", location = "component://common/widget/PortalPageScreens.xml")
    @Action(type = ActionType.SET, field = "originalPortalPageId", fromField = "parameters.originalPortalPageId")
    @Action(type = ActionType.SET, field = "mainPortalPageId", fromField = "parameters.mainPortalPageId")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/ListPortalPortlets.groovy")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/portal/listPortalPortlets.ftl"
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "backLast"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonPortalCategoryPage}: ${parameters.parentPortalPageId}", includeForms = {
                        @IncludeForm(name = "PortletCategoryAndPortlet", location = "component://common/widget/PortalPageForms.xml"
                    )}, position = 1)})
        }
    )
    public interface AddPortlet {}

    @Screen(name = "GenericPortalPage", location = "component://common/widget/PortalPageScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_PORTAL_PAGE, id = "${pageId}")}))
    public interface GenericPortalPage {}

    @Screen(name = "FindGenericEntity", location = "component://common/widget/PortalPageScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.${titleLabel}}", includeForms = {@IncludeForm(name = "FindGenericEntity", location = "component://common/widget/PortalPageForms.xml")})}))
    public interface FindGenericEntity {}

    @Screen(name = "GenericScreenlet", location = "component://common/widget/PortalPageScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.${titleLabel}}", name = "scrlt_${formName}", collapsible = true, includeForms = {@IncludeForm(name = "${formName}", location = "${formLocation}")})}))
    public interface GenericScreenlet {}

    @Screen(name = "GenericScreenletAjax", location = "component://common/widget/PortalPageScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.${titleLabel}}", containers = {@Container(id = "${divIdArea}", includeScreens = {@IncludeScreen(name = "${screenName}", location = "${screenLocation}")})})}))
    public interface GenericScreenletAjax {}

    @Screen(name = "GenericScreenletAjaxWithMenu", location = "component://common/widget/PortalPageScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.${titleLabel}}", includeMenus = {@IncludeMenu(name = "${menuName}", location = "${menuLocation}")}, containers = {@Container(id = "${divIdArea}", includeScreens = {@IncludeScreen(name = "${screenName}", location = "${screenLocation}")})})}))
    public interface GenericScreenletAjaxWithMenu {}

    @Screen(name = "EditPortalPortletAttributes", location = "component://common/widget/PortalPageScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPortlet", valueField = "portalPortlet")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "backLast"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonEditPortletAttributes}: ${portalPortlet.portletName}", includeForms = {
                        @IncludeForm(name = "${portalPortlet.editFormName}", location = "${portalPortlet.editFormLocation}"
                    )}, position = 1)})
        }
    )
    public interface EditPortalPortletAttributes {}

    @Screen(name = "EditPortalPageColumnWidth", location = "component://common/widget/PortalPageScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPageColumn", valueField = "portalPageColumn")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "backLast"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "Set column width: ${portalPortlet.portletName}", includeForms = {
                        @IncludeForm(name = "EditPortalPageColumnWidth", location = "component://common/widget/PortalPageForms.xml"
                    )}, position = 1)})
        }
    )
    public interface EditPortalPageColumnWidth {}

    @Screen(name = "NewPortalPage", location = "component://common/widget/PortalPageScreens.xml")
    @DecoratorScreen(
        name = "CommonPortletDecorator",
        location = "component://common/widget/PortalPageScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "ManagePortalPages"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonPortalNewPage}", includeForms = {
                        @IncludeForm(name = "NewPortalPage", location = "component://common/widget/PortalPageForms.xml"
                    )}, position = 1)})
        }
    )
    public interface NewPortalPage {}

}
