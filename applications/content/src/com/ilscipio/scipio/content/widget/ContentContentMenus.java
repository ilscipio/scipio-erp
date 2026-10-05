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
package com.ilscipio.scipio.content.widget;

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
public class ContentContentMenus {

    @Menu(
        name = "ContentAppBar",
        location = "component://content/widget/content/ContentMenus.xml",
        title = "${uiLabelMap.ContentContentManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "websites", title = "${uiLabelMap.ContentWebSites}", link = @MenuLink(target = "FindWebSite")),
            @MenuItem(name = "survey", title = "${uiLabelMap.ContentSurvey}", link = @MenuLink(target = "FindSurvey")),
            @MenuItem(name = "Content", title = "${uiLabelMap.ContentContent}", link = @MenuLink(target = "findContent")),
            @MenuItem(name = "DataResource", title = "${uiLabelMap.ContentDataResource}", link = @MenuLink(target = "findDataResource")),
            @MenuItem(name = "ContentSetupMenu", title = "${uiLabelMap.ContentContentSetup}", link = @MenuLink(target = "ContentSetupMenu")),
            @MenuItem(name = "DataResourceSetupMenu", title = "${uiLabelMap.ContentDataSetup}", link = @MenuLink(target = "DataSetupMenu"))
        }
    )
    public interface ContentAppBar {}

    @Menu(
        name = "ContentAppSideBar",
        location = "component://content/widget/content/ContentMenus.xml",
        title = "${uiLabelMap.ContentContentManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContentAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true"
    )
    public interface ContentAppSideBar {}

    @Menu(
        name = "ContentTabBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "content", title = "${uiLabelMap.ContentContent}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContent", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "association", title = "${uiLabelMap.ContentAssociation}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentAssoc", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "role", title = "${uiLabelMap.FormFieldTitle_roles}", link = @MenuLink(target = "EditContentRole", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "purpose", title = "${uiLabelMap.FormFieldTitle_purposes}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentPurpose", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "attribute", title = "${uiLabelMap.ContentAttribute}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentAttribute", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "websites", title = "${uiLabelMap.ContentWebSites}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "ListWebSite", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "metaData", title = "${uiLabelMap.ContentMetadata}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentMetaData", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "workEffort", title = "${uiLabelMap.WorkEffortWorkEffort}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentWorkEfforts", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")})),
            @MenuItem(name = "keywords", title = "${uiLabelMap.ContentKeywords}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContentKeywords", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")}))
        }
    )
    public interface ContentTabBar {}

    @Menu(
        name = "ContentSideBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContentTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN"
    )
    public interface ContentSideBar {}

    @Menu(
        name = "ContentSubButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "NewContent", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.contentId"})}), link = @MenuLink(target = "EditContent")),
            @MenuItem(name = "NewContentAssoc", title = "${uiLabelMap.CommonCreateNew} ${uiLabelMap.ContentAssociation}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "equals", "association"})}), link = @MenuLink(target = "EditContentAssoc", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId")}))
        }
    )
    public interface ContentSubButtonBar {}

    @Menu(
        name = "WebSiteButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "EditWebSite", title = "${uiLabelMap.ContentWebSite}", link = @MenuLink(target = "EditWebSite", parameters = {@MenuParameter(paramName = "webSiteId", fromField = "parameters.webSiteId")})),
            @MenuItem(name = "PathAlias", title = "${uiLabelMap.ContentPathAlias}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.webSiteId"})}), link = @MenuLink(target = "WebSiteAliases", parameters = {@MenuParameter(paramName = "webSiteId", fromField = "parameters.webSiteId")})),
            @MenuItem(name = "WebSiteSEO", title = "${uiLabelMap.ContentSEO}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.webSiteId"})}), link = @MenuLink(target = "WebSiteSeo", parameters = {@MenuParameter(paramName = "webSiteId", fromField = "parameters.webSiteId")})),
            @MenuItem(name = "WebAnalytics", title = "${uiLabelMap.CatalogWebAnalytics}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.webSiteId"})}), link = @MenuLink(target = "WebAnalytics", parameters = {@MenuParameter(paramName = "webSiteId", fromField = "parameters.webSiteId")}))
        }
    )
    public interface WebSiteButtonBar {}

    @Menu(
        name = "WebSiteSideBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "WebSiteButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN"
    )
    public interface WebSiteSideBar {}

    @Menu(
        name = "BlogButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "ListBlog", title = "${uiLabelMap.CommonList}", link = @MenuLink(target = "blogMain")),
            @MenuItem(name = "EditBlog", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "editBlog", parameters = {@MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @MenuItem(name = "Articles", title = "${uiLabelMap.ContentBlogArticleList}", link = @MenuLink(target = "blogContent", parameters = {@MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @MenuItem(name = "Owners", title = "${uiLabelMap.FormFieldTitle_roles}", link = @MenuLink(target = "EditContentRole", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.blogContentId")}))
        }
    )
    public interface BlogButtonBar {}

    @Menu(
        name = "BlogSideBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "BlogButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN"
    )
    public interface BlogSideBar {}

    @Menu(
        name = "BlogSubTabBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "NewBlog", title = "${uiLabelMap.ContentCreateNewBlog}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "editBlog")),
            @MenuItem(name = "NewBlogArticle", title = "${uiLabelMap.ContentCreateNewBlogArticle}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "equals", "Articles"})}), link = @MenuLink(target = "EditBlogArticle", parameters = {@MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")}))
        }
    )
    public interface BlogSubTabBar {}

    @Menu(
        name = "BlogArticleTabBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        defaultPermissionOperation = "CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "ListlogArt", title = "${uiLabelMap.CommonList}", link = @MenuLink(target = "blogContent", parameters = {@MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @MenuItem(name = "ViewBlogArt", title = "${uiLabelMap.CommonView}", link = @MenuLink(target = "ViewBlogArticle", parameters = {@MenuParameter(paramName = "articleContentId", fromField = "parameters.articleContentId"), @MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @MenuItem(name = "EditBlogArt", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditBlogArticle", parameters = {@MenuParameter(paramName = "articleContentId", fromField = "parameters.articleContentId"), @MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @MenuItem(name = "Owners", title = "${uiLabelMap.FormFieldTitle_roles}", link = @MenuLink(target = "EditContentRole", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.articleContentId")}))
        }
    )
    public interface BlogArticleTabBar {}

    @Menu(
        name = "BlogArticleButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "NewBlogArticle", title = "${uiLabelMap.ContentCreateNewBlogArticle}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditBlogArticle", parameters = {@MenuParameter(paramName = "blogContentId", fromField = "parameters.blogContentId")}))
        }
    )
    public interface BlogArticleButtonBar {}

    @Menu(
        name = "ContentTopButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "findContent", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "findContent")),
            @MenuItem(name = "navigateContent", title = "${uiLabelMap.ContentNavigate}", link = @MenuLink(target = "navigateContent"))
        }
    )
    public interface ContentTopButtonBar {}

    @Menu(
        name = "ContentTopSideBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContentTopButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ContentTopSideBar {}

    @Menu(
        name = "lookupMenu",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "close", title = "${uiLabelMap.CommonClose}", widgetStyle = "+${styles.action_run_sys} ${styles.action_close}", link = @MenuLink(target = "javascript:window.close();", urlMode = UrlMode.PLAIN)),
            @MenuItem(name = "index", title = "${uiLabelMap.CommonExtIndex}", link = @MenuLink(target = "showHelp?helpTopic=navigateHelp"))
        }
    )
    public interface lookupMenu {}

    @Menu(
        name = "contentMenu",
        location = "component://content/widget/content/ContentMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditContent", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditContent")),
            @MenuItem(name = "ContentSearchOptions", title = "${uiLabelMap.CommonAdvancedSearch}", link = @MenuLink(target = "ContentSearchOptions")),
            @MenuItem(name = "UpdateContentAllKeywords", title = "${uiLabelMap.ContentAutoCreateKeywords}", widgetStyle = "+${styles.action_run_sys} ${styles.action_update}", link = @MenuLink(target = "updateContentAllKeywords"))
        }
    )
    public interface contentMenu {}

    @Menu(
        name = "websiteMenu",
        location = "component://content/widget/content/ContentMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditWebSite", title = "${uiLabelMap.ContentNewWebSite}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.webSiteId"})}), link = @MenuLink(target = "EditWebSite")),
            @MenuItem(name = "FindWebSite", title = "${uiLabelMap.PageTitleFindWebSite}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", link = @MenuLink(target = "FindWebSite"))
        }
    )
    public interface websiteMenu {}

    @Menu(
        name = "WebAnalyticsConfigButtonBar",
        location = "component://content/widget/content/ContentMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "EditWebAnalyticsConfig", title = "${uiLabelMap.CommonCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem2", "not-equals", "EditWebAnalyticsConfig"})}), link = @MenuLink(target = "EditWebAnalyticsConfig", parameters = {@MenuParameter(paramName = "webSiteId", fromField = "parameters.webSiteId")}))
        }
    )
    public interface WebAnalyticsConfigButtonBar {}

}
