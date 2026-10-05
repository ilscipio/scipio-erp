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
package com.ilscipio.scipio.cms.widget;

import com.ilscipio.scipio.widget.def.menu.*;
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
public class CMSMenus {

    @Menu(
        name = "MainAppBar",
        location = "component://cms/widget/CMSMenus.xml",
        title = "${uiLabelMap.CMSApplication}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        items = {
            @MenuItem(name = "pages", title = "${uiLabelMap.CommonPages}", link = @MenuLink(target = "pages")),
            @MenuItem(name = "templates", title = "${uiLabelMap.CmsTemplates}", link = @MenuLink(target = "templates")),
            @MenuItem(name = "media", title = "${uiLabelMap.CmsMedia}", link = @MenuLink(target = "media")),
            @MenuItem(name = "menus", title = "${uiLabelMap.CmsMenus}", link = @MenuLink(target = "menus")),
            @MenuItem(name = "importExport", title = "${uiLabelMap.WebtoolsImportExport}", link = @MenuLink(target = "CmsDataExport")),
            @MenuItem(name = "contentLibrary", title = "${uiLabelMap.CmsContentLibrary}", link = @MenuLink(target = "contentAssets")),
            @MenuItem(name = "settings", title = "${uiLabelMap.CommonSettings}", link = @MenuLink(target = "settings", title = "${uiLabelMap.CommonSettings}", parameters = {@MenuParameter(paramName = "editSettings", fromField = "editSettings")}))
        }
    )
    public interface MainAppBar {}

    @Menu(
        name = "MainAppSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        title = "${uiLabelMap.CMSApplication}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "MainAppBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        itemsSortMode = "off",
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "pages", subMenus = {@SubMenu(name = "Pages", include = "component://cms/widget/CMSMenus.xml#PageSideBar")}),
            @MenuItem(name = "templates", subMenus = {@SubMenu(name = "Templates", include = "component://cms/widget/CMSMenus.xml#TemplateSideBar")}),
            @MenuItem(name = "media", subMenus = {@SubMenu(name = "Media", include = "component://cms/widget/CMSMenus.xml#MediaSideBar")}),
            @MenuItem(name = "menus", subMenus = {@SubMenu(name = "Menus", include = "component://cms/widget/CMSMenus.xml#MenuSideBar")}),
            @MenuItem(name = "contentLibrary", subMenus = {@SubMenu(name = "ContentAsset", include = "component://cms/widget/CMSMenus.xml#ContentAssetSideBar")}),
            @MenuItem(name = "importExport", subMenus = {@SubMenu(name = "ImportExport", include = "component://cms/widget/CMSMenus.xml#ImportExportSideBar")}),
            @MenuItem(name = "settings", subMenus = {@SubMenu(name = "Settings", include = "component://cms/widget/CMSMenus.xml#SettingsSideBar")})
        }
    )
    public interface MainAppSideBar {}

    @Menu(
        name = "PageTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "pageId", fromField = "parameters.pageId"), @SetAction(field = "path", fromField = "parameters.path"), @SetAction(field = "webSiteId", fromField = "parameters.webSiteId")}),
        items = {
            @MenuItem(name = "listPages", title = "${uiLabelMap.CmsListPages}", link = @MenuLink(target = "pages")),
            @MenuItem(name = "editPage", title = "${uiLabelMap.CmsEditPage}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(conditions = {@Condition(type = Or.class, tree = {@ConditionNode(not = true, type = Empty.class, params = {"pageId"}), @ConditionNode(type = And.class), @ConditionNode(parent = 1, not = true, type = Empty.class, params = {"path"}), @ConditionNode(parent = 1, not = true, type = Empty.class, params = {"webSiteId"})})}), link = @MenuLink(target = "editPage", parameters = {@MenuParameter(paramName = "pageId", fromField = "pageId"), @MenuParameter(paramName = "path", fromField = "path"), @MenuParameter(paramName = "webSiteId", fromField = "webSiteId")}))
        }
    )
    public interface PageTabBar {}

    @Menu(
        name = "PageSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PageTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PageSideBar {}

    @Menu(
        name = "TemplateTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "templates", title = "${uiLabelMap.CmsListTemplates}", link = @MenuLink(target = "templates")),
            @MenuItem(name = "editTemplate", title = "${uiLabelMap.CmsEditTemplate}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.pageTemplateId"})}), link = @MenuLink(target = "editTemplate", parameters = {@MenuParameter(paramName = "pageTemplateId", fromField = "parameters.pageTemplateId")})),
            @MenuItem(name = "assets", title = "${uiLabelMap.CmsListAssets}", link = @MenuLink(target = "assets")),
            @MenuItem(name = "editAsset", title = "${uiLabelMap.CmsEditAsset}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.assetTemplateId"})}), link = @MenuLink(target = "editAsset", parameters = {@MenuParameter(paramName = "assetTemplateId", fromField = "parameters.assetTemplateId")})),
            @MenuItem(name = "scripts", title = "${uiLabelMap.CmsListScripts}", link = @MenuLink(target = "scripts")),
            @MenuItem(name = "editScript", title = "${uiLabelMap.CmsEditScript}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.scriptTemplateId"})}), link = @MenuLink(target = "editScript", parameters = {@MenuParameter(paramName = "scriptTemplateId", fromField = "parameters.scriptTemplateId")}))
        }
    )
    public interface TemplateTabBar {}

    @Menu(
        name = "TemplateSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TemplateTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface TemplateSideBar {}

    @Menu(
        name = "MediaTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "media", title = "${uiLabelMap.CmsListMedia}", link = @MenuLink(target = "media")),
            @MenuItem(name = "customImageSizePresets", title = "${uiLabelMap.CmsCustomImageSizePresets}", link = @MenuLink(target = "customImageSizePresets"))
        }
    )
    public interface MediaTabBar {}

    @Menu(
        name = "MediaSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "MediaTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface MediaSideBar {}

    @Menu(
        name = "MenuSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "menus", title = "${uiLabelMap.CmsMenus}", link = @MenuLink(target = "menus"))
        }
    )
    public interface MenuSideBar {}

    @Menu(
        name = "ImportExportSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "dataExport", title = "${uiLabelMap.WebtoolsDataExport}", link = @MenuLink(target = "CmsDataExport")),
            @MenuItem(name = "dataImport", title = "${uiLabelMap.WebtoolsDataImport}", link = @MenuLink(target = "CmsDataImport"))
        }
    )
    public interface ImportExportSideBar {}

    @Menu(
        name = "RedirectsTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "redirects", title = "${uiLabelMap.CommonRedirects}", link = @MenuLink(target = "redirects"))
        }
    )
    public interface RedirectsTabBar {}

    @Menu(
        name = "RedirectsSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RedirectsTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RedirectsSideBar {}

    @Menu(
        name = "RobotsTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "robots", title = "${uiLabelMap.CommonRobots}", link = @MenuLink(target = "robots"))
        }
    )
    public interface RobotsTabBar {}

    @Menu(
        name = "RobotsSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RobotsTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RobotsSideBar {}

    @Menu(
        name = "ContentAssetTabBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "contentAssets", title = "${uiLabelMap.CmsAllContentAssets}", link = @MenuLink(target = "contentAssets")),
            @MenuItem(name = "editContentAsset", title = "${uiLabelMap.CmsEditContentAsset}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.assetTemplateId"})}), link = @MenuLink(target = "editContentAsset", parameters = {@MenuParameter(paramName = "assetTemplateId", fromField = "parameters.assetTemplateId")}))
        }
    )
    public interface ContentAssetTabBar {}

    @Menu(
        name = "ContentAssetSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContentAssetTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ContentAssetSideBar {}

    @Menu(
        name = "SettingsSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "settings",
        alwaysExpandSelectedOrAncestor = "true",
        actions = @MenuActions(script = {@ScriptAction(location = "component://cms/webapp/cms/WEB-INF/actions/generated/SettingsSideBar_script1.groovy")}),
        items = {
            @MenuItem(name = "robots", title = "${uiLabelMap.CommonRobots}", link = @MenuLink(target = "robots")),
            @MenuItem(name = "redirects", title = "${uiLabelMap.CommonRedirects}", link = @MenuLink(target = "redirects")),
            @MenuItem(name = "wordpress", title = "Wordpress", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"wordpressAvailable", "equals", "true"})}), link = @MenuLink(target = "wordpress")),
            @MenuItem(name = "maileon", title = "Maileon", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"maileonAvailable", "equals", "true"})}), link = @MenuLink(target = "maileon"), subMenus = {@SubMenu(name = "Maileon", include = "component://cms/widget/CMSMenus.xml#MaileonSideBar")})
        }
    )
    public interface SettingsSideBar {}

    @Menu(
        name = "MaileonSideBar",
        location = "component://cms/widget/CMSMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "maileonCustomFields", title = "Maileon Custom Fields", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"maileonAvailable", "equals", "true"})}), link = @MenuLink(target = "maileonCustomFields")),
            @MenuItem(name = "maileonContactSync", title = "Maileon Contact Sync", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"maileonAvailable", "equals", "true"})}), link = @MenuLink(target = "maileonContactSync"))
        }
    )
    public interface MaileonSideBar {}

}
