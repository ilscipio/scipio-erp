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
public class Menus {

    @Menu(
        name = "WebtoolsAppBar",
        location = "component://webtools/widget/Menus.xml",
        title = "${uiLabelMap.FrameworkWebTools}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "server", title = "${uiLabelMap.WebtoolsServer}", link = @MenuLink(target = "LogView")),
            @MenuItem(name = "entity", title = "${uiLabelMap.WebtoolsEntityEngine}", link = @MenuLink(target = "entitymaint")),
            @MenuItem(name = "service", title = "${uiLabelMap.WebtoolsServiceEngineTools}", link = @MenuLink(target = "ServiceList")),
            @MenuItem(name = "properties", title = "${uiLabelMap.CommonProperties}", link = @MenuLink(target = "EntityLabels", parameters = {@MenuParameter(paramName = "resourceId", fromField = "resourceId")})),
            @MenuItem(name = "importExport", title = "${uiLabelMap.WebtoolsImportExport}", link = @MenuLink(target = "xmldsdump")),
            @MenuItem(name = "geoManagement", title = "${uiLabelMap.WebtoolsGeoManagement}", link = @MenuLink(target = "FindGeo")),
            @MenuItem(name = "Development", title = "${uiLabelMap.CommonDevelopment}", link = @MenuLink(target = "LayoutDemo")),
            @MenuItem(name = "solr", title = "${uiLabelMap.WebtoolsSolr}", link = @MenuLink(target = "SolrStatus"))
        }
    )
    public interface WebtoolsAppBar {}

    @Menu(
        name = "WebtoolsAppSideBar",
        location = "component://webtools/widget/Menus.xml",
        title = "${uiLabelMap.FrameworkWebTools}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "WebtoolsAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "server", subMenus = {@SubMenu(name = "Server", include = "component://webtools/widget/Menus.xml#ServerSideBar")}),
            @MenuItem(name = "entity", subMenus = {@SubMenu(name = "Entity", include = "component://webtools/widget/Menus.xml#EntitySideBar")}),
            @MenuItem(name = "service", subMenus = {@SubMenu(name = "service", include = "component://webtools/widget/Menus.xml#serviceSideBar")}),
            @MenuItem(name = "properties", subMenus = {@SubMenu(name = "Property", include = "component://webtools/widget/Menus.xml#PropertySideBar")}),
            @MenuItem(name = "importExport", subMenus = {@SubMenu(name = "importExport", include = "component://webtools/widget/Menus.xml#importExportSideBar")}),
            @MenuItem(name = "geoManagement", subMenus = {@SubMenu(name = "geoManagement", include = "component://webtools/widget/Menus.xml#geoManagementSideBar")}),
            @MenuItem(name = "Development", subMenus = {@SubMenu(name = "Development", include = "component://webtools/widget/Menus.xml#DevelopmentSideBar")}),
            @MenuItem(name = "solr", subMenus = {@SubMenu(name = "Solr", include = "component://webtools/widget/Menus.xml#SolrSideBar")})
        }
    )
    public interface WebtoolsAppSideBar {}

    @Menu(
        name = "configurationTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "tempexpr", title = "${uiLabelMap.WebtoolsTemporalExpression}", link = @MenuLink(target = "findTemporalExpression")),
            @MenuItem(name = "myCertificates", title = "${uiLabelMap.WebtoolsMyCertificates}", link = @MenuLink(target = "myCertificates"))
        }
    )
    public interface configurationTabBar {}

    @Menu(
        name = "configurationSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "configurationTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface configurationSideBar {}

    @Menu(
        name = "importExportTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "data", title = "${uiLabelMap.WebtoolsDataFileTools}", link = @MenuLink(target = "viewdatafile")),
            @MenuItem(name = "modelInduceFromDb", title = "${uiLabelMap.WebtoolsInduceModelXMLFromDatabase}", link = @MenuLink(target = "view/ModelInduceFromDb")),
            @MenuItem(name = "entityEoModelBundle", title = "${uiLabelMap.WebtoolsExportEntityEoModelBundle}", link = @MenuLink(target = "EntityEoModelBundle")),
            @MenuItem(name = "xmlDsDump", title = "${uiLabelMap.PageTitleEntityExport}", link = @MenuLink(target = "xmldsdump")),
            @MenuItem(name = "entityExportAll", title = "${uiLabelMap.PageTitleEntityExportAll}", link = @MenuLink(target = "EntityExportAll")),
            @MenuItem(name = "programExport", title = "${uiLabelMap.PageTitleProgramExport}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"}), @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"})}), link = @MenuLink(target = "ProgramExport")),
            @MenuItem(name = "entityImport", title = "${uiLabelMap.PageTitleEntityImport}", link = @MenuLink(target = "EntityImport")),
            @MenuItem(name = "entityImportDir", title = "${uiLabelMap.PageTitleEntityImportDir}", link = @MenuLink(target = "EntityImportDir")),
            @MenuItem(name = "entityImportReaders", title = "${uiLabelMap.PageTitleEntityImportReaders}", link = @MenuLink(target = "EntityImportReaders")),
            @MenuItem(name = "excelImport", title = "${uiLabelMap.EntityExcelImport}", link = @MenuLink(target = "excelimport"))
        }
    )
    public interface importExportTabBar {}

    @Menu(
        name = "importExportSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "importExportTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface importExportSideBar {}

    @Menu(
        name = "serviceTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "serviceList", title = "${uiLabelMap.WebtoolsServiceReference}", link = @MenuLink(target = "ServiceList")),
            @MenuItem(name = "findJob", title = "${uiLabelMap.WebtoolsJobList}", link = @MenuLink(target = "FindJob")),
            @MenuItem(name = "jobStats", title = "${uiLabelMap.PageTitleJobStats}", link = @MenuLink(target = "JobStats")),
            @MenuItem(name = "threadList", title = "${uiLabelMap.WebtoolsThreadList}", link = @MenuLink(target = "threadList")),
            @MenuItem(name = "scheduleJob", title = "${uiLabelMap.WebtoolsScheduleJob}", link = @MenuLink(target = "scheduleJob")),
            @MenuItem(name = "runService", title = "${uiLabelMap.PageTitleRunService}", link = @MenuLink(target = "runService"))
        }
    )
    public interface serviceTabBar {}

    @Menu(
        name = "serviceSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "serviceTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface serviceSideBar {}

    @Menu(
        name = "ServerTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "logging", title = "${uiLabelMap.WebtoolsLogging}", link = @MenuLink(target = "LogView")),
            @MenuItem(name = "cache", title = "${uiLabelMap.WebtoolsCacheMaintenance}", link = @MenuLink(target = "FindUtilCache")),
            @MenuItem(name = "prewarmcache", title = "${uiLabelMap.WebtoolsPrewarmCacheUrls}", link = @MenuLink(target = "EditPrewarmCacheUrls")),
            @MenuItem(name = "artifact", title = "${uiLabelMap.WebtoolsArtifactInfo}", link = @MenuLink(target = "ViewComponents"), subMenus = {@SubMenu(name = "artifact", include = "component://webtools/widget/Menus.xml#artifactSideBar")}),
            @MenuItem(name = "security", title = "${uiLabelMap.CommonSecurity}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = ServicePermission.class, params = {"securityPermissionCheck", "VIEW"})}), link = @MenuLink(target = "security"), subMenus = {@SubMenu(name = "SecurityGroup", include = "component://common/widget/SecurityMenus.xml#SecurityGroupSideBar")}),
            @MenuItem(name = "stats", title = "${uiLabelMap.WebtoolsStatistics}", link = @MenuLink(target = "StatsSinceStart")),
            @MenuItem(name = "configuration", title = "${uiLabelMap.WebtoolsCertsX509}", link = @MenuLink(target = "myCertificates"), subMenus = {@SubMenu(name = "configuration", include = "component://webtools/widget/Menus.xml#configurationSideBar")})
        }
    )
    public interface ServerTabBar {}

    @Menu(
        name = "ServerSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ServerTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "stats", subMenus = {@SubMenu(name = "Stats", include = "component://webtools/widget/Menus.xml#StatsSideBar")})
        }
    )
    public interface ServerSideBar {}

    @Menu(
        name = "artifactTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "artifactInfo", title = "${uiLabelMap.WebtoolsArtifactInfo} ${uiLabelMap.CommonSearch} ${uiLabelMap.WebtoolsArtifactInfoTimeToLoad}", link = @MenuLink(target = "ArtifactInfo")),
            @MenuItem(name = "viewents", title = "${uiLabelMap.CommonComponents}", link = @MenuLink(target = "ViewComponents"))
        }
    )
    public interface artifactTabBar {}

    @Menu(
        name = "artifactSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "artifactTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface artifactSideBar {}

    @Menu(
        name = "TempExprTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "tabMenuItem",
        items = {
            @MenuItem(name = "findExpression", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "findTemporalExpression")),
            @MenuItem(name = "createExpression", title = "${uiLabelMap.CommonCreate}", link = @MenuLink(target = "editTemporalExpression"))
        }
    )
    public interface TempExprTabBar {}

    @Menu(
        name = "EntityTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "entitymaint", title = "${uiLabelMap.WebtoolsEntityDataMaintenance}", link = @MenuLink(target = "entitymaint")),
            @MenuItem(name = "entityref", title = "${uiLabelMap.WebtoolsEntityReference} - Interactive", link = @MenuLink(target = "entityref", targetWindow = "_BLANK")),
            @MenuItem(name = "entityrefStatic", title = "${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceStaticVersion}", link = @MenuLink(target = "entityref", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "forstatic", value = "true")})),
            @MenuItem(name = "entityrefReport", title = "${uiLabelMap.WebtoolsEntityReferencePdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "entityrefReport", targetWindow = "_BLANK")),
            @MenuItem(name = "EntitySQLProcessor", title = "${uiLabelMap.PageTitleEntitySQLProcessor}", link = @MenuLink(target = "EntitySQLProcessor")),
            @MenuItem(name = "entitySyncStatus", title = "${uiLabelMap.WebtoolsEntitySyncStatus}", link = @MenuLink(target = "EntitySyncStatus")),
            @MenuItem(name = "checkDb", title = "${uiLabelMap.WebtoolsCheckUpdateDatabase}", link = @MenuLink(target = "view/checkdb")),
            @MenuItem(name = "ConnectionPoolStatus", title = "${uiLabelMap.ConnectionPoolStatus}", link = @MenuLink(target = "ConnectionPoolStatus")),
            @MenuItem(name = "entityPerformanceTest", title = "${uiLabelMap.WebtoolsPerformanceTests}", link = @MenuLink(target = "EntityPerformanceTest")),
            @MenuItem(name = "EntityUtilityServices", title = "Scipio ${uiLabelMap.WebtoolsUtilityServices}", link = @MenuLink(target = "EntityUtilityServices")),
            @MenuItem(name = "ListDemoDataGeneratorServices", title = "Scipio ${uiLabelMap.WebtoolsDemoDataGeneratorServiceList}", link = @MenuLink(target = "ListDemoDataGeneratorServices"))
        }
    )
    public interface EntityTabBar {}

    @Menu(
        name = "EntitySideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EntityTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface EntitySideBar {}

    @Menu(
        name = "EntitySubTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EntityMaint", title = "${uiLabelMap.WebtoolsBackToEntityList}", widgetStyle = "+${styles.action_nav_cancel}", link = @MenuLink(target = "entitymaint")),
            @MenuItem(name = "ViewRelations", title = "${uiLabelMap.WebtoolsViewRelations}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"entityName"}), @Condition(type = NotEmpty.class, params = {"modelEntity"})}), link = @MenuLink(target = "ViewRelations", parameters = {@MenuParameter(paramName = "entityName", fromField = "entityName")})),
            @MenuItem(name = "ViewGeneric", title = "${uiLabelMap.CommonCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"entityName"}), @Condition(type = NotEmpty.class, params = {"modelEntity"})}), link = @MenuLink(target = "ViewGeneric", parameters = {@MenuParameter(paramName = "entityName", fromField = "entityName"), @MenuParameter(paramName = "enableEdit", value = "true")}))
        }
    )
    public interface EntitySubTabBar {}

    @Menu(
        name = "PortalPageAdmin",
        location = "component://webtools/widget/Menus.xml",
        items = {
            @MenuItem(name = "new", title = "${uiLabelMap.CommonNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewPortalPage"))
        }
    )
    public interface PortalPageAdmin {}

    @Menu(
        name = "StatsTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "stats", title = "${uiLabelMap.WebtoolsStatistics}", link = @MenuLink(target = "StatsSinceStart")),
            @MenuItem(name = "metrics", title = "${uiLabelMap.WebtoolsMetrics}", link = @MenuLink(target = "ViewMetrics"))
        }
    )
    public interface StatsTabBar {}

    @Menu(
        name = "StatsSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "StatsTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface StatsSideBar {}

    @Menu(
        name = "StatsSinceStart",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "clearStats", title = "${uiLabelMap.WebtoolsStatsClearSince}", link = @MenuLink(target = "StatsSinceStart", parameters = {@MenuParameter(paramName = "clear", value = "true")})),
            @MenuItem(name = "refresh", title = "${uiLabelMap.CommonRefresh}", widgetStyle = "+refresh ${styles.action_reload}", link = @MenuLink(target = "StatsSinceStart"))
        }
    )
    public interface StatsSinceStart {}

    @Menu(
        name = "StatsBinHistory",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "refresh", title = "${uiLabelMap.CommonRefresh}", widgetStyle = "+refresh ${styles.action_reload}", link = @MenuLink(target = "StatBinsHistory", parameters = {@MenuParameter(paramName = "statsId", fromField = "parameters.statsId"), @MenuParameter(paramName = "type", fromField = "parameters.type")}))
        }
    )
    public interface StatsBinHistory {}

    @Menu(
        name = "FindCacheTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeCacheTabMenuItem",
        items = {
            @MenuItem(name = "refresh", title = "${uiLabelMap.CommonRefresh}", widgetStyle = "+refresh ${styles.action_reload}", link = @MenuLink(target = "FindUtilCache")),
            @MenuItem(name = "clearAll", title = "${uiLabelMap.WebtoolsClearAllCaches}", link = @MenuLink(target = "FindUtilCacheClearAll")),
            @MenuItem(name = "forceGarbageCollection", title = "${uiLabelMap.WebtoolsRunGC}", link = @MenuLink(target = "ForceGarbageCollection"))
        }
    )
    public interface FindCacheTabBar {}

    @Menu(
        name = "CacheElements",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "back", title = "${uiLabelMap.WebtoolsBackToCacheMaintenance}", link = @MenuLink(target = "FindUtilCache")),
            @MenuItem(name = "edit", title = "${uiLabelMap.PageTitleEditUtilCache}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"cache"})}), link = @MenuLink(target = "EditUtilCache", parameters = {@MenuParameter(paramName = "UTIL_CACHE_NAME", fromField = "cacheName")}))
        }
    )
    public interface CacheElements {}

    @Menu(
        name = "EditCache",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "back", title = "${uiLabelMap.WebtoolsBackToCacheMaintenance}", link = @MenuLink(target = "FindUtilCache")),
            @MenuItem(name = "clear", title = "${uiLabelMap.WebtoolsClearThisCache}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"cache"})}), link = @MenuLink(target = "EditUtilCacheClear", parameters = {@MenuParameter(paramName = "UTIL_CACHE_NAME", fromField = "cacheName"), @MenuParameter(paramName = "type", fromField = "parameters.type")})),
            @MenuItem(name = "elements", title = "${uiLabelMap.WebtoolsElements}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"cache"})}), link = @MenuLink(target = "FindUtilCacheElements", parameters = {@MenuParameter(paramName = "UTIL_CACHE_NAME", fromField = "cacheName")}))
        }
    )
    public interface EditCache {}

    @Menu(
        name = "geoManagementTabBar",
        location = "component://webtools/widget/Menus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "FindGeo", title = "${uiLabelMap.WebtoolsGeosFind}", link = @MenuLink(target = "FindGeo")),
            @MenuItem(name = "EditGeo", title = "${uiLabelMap.WebtoolsGeoCreateNew}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditGeo", parameters = {@MenuParameter(paramName = "geoId", fromField = "parameters.geoId")})),
            @MenuItem(name = "LinkGeos", title = "${uiLabelMap.WebtoolsGeosLink}", link = @MenuLink(target = "LinkGeos", parameters = {@MenuParameter(paramName = "geoId", fromField = "parameters.geoId")}))
        }
    )
    public interface geoManagementTabBar {}

    @Menu(
        name = "geoManagementSideBar",
        location = "component://webtools/widget/Menus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "geoManagementTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface geoManagementSideBar {}

    @Menu(
        name = "DevelopmentSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "demoTargetUrl", fromField = "demoTargetUrl", defaultValue = "LayoutDemo")}),
        items = {
            @MenuItem(name = "LayoutDemo", title = "${uiLabelMap.WebtoolsLayoutDemo}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "debugMode", fromField = "parameters.debugMode")}), subMenus = {@SubMenu(name = "LayoutDemo", include = "component://webtools/widget/Menus.xml#LayoutDemoSideBar")}),
            @MenuItem(name = "TemplateTest", title = "${uiLabelMap.WebtoolsTemplateTest}", condition = @MenuItemCondition(conditions = {@Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"}), @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"})}), link = @MenuLink(target = "TemplateTest")),
            @MenuItem(name = "DeveloperDocIndex", title = "${uiLabelMap.WebtoolsDeveloperDocumentation}", link = @MenuLink(target = "DeveloperDocIndex"), subMenus = {@SubMenu(name = "DeveloperDoc", include = "component://webtools/widget/Menus.xml#DeveloperDocSideBar")})
        }
    )
    public interface DevelopmentSideBar {}

    @Menu(
        name = "DeveloperDocSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "basePageInterWebappUri", value = "${groovy: request.getContextPath() + '/docs/templating/ftl/lib'}")}),
        items = {
            @MenuItem(name = "TemplateApiDoc", title = "${uiLabelMap.WebtoolsTemplateApiDocsTitle}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlTemplate#extLoginKey=OFF", urlMode = UrlMode.INTER_APP), subMenus = {@SubMenu(name = "TemplateApiDoc", include = "component://webtools/widget/Menus.xml#TemplateApiDocSideBar")}),
            @MenuItem(name = "ScipioWebsiteLink", title = "Scipio ERP Website", sortMode = "off", link = @MenuLink(target = "http://www.scipioerp.com/community/developer/", targetWindow = "_blank", urlMode = UrlMode.PLAIN))
        }
    )
    public interface DeveloperDocSideBar {}

    @Menu(
        name = "TemplateApiDocTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        actions = @MenuActions(set = {@SetAction(field = "basePageInterWebappUri", value = "${groovy: request.getContextPath() + '/docs/templating/ftl/lib'}")}),
        items = {
            @MenuItem(name = "htmlTemplate", title = "${docLibTitleMap['standard/htmlTemplate']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlTemplate#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlStructure", title = "${docLibTitleMap['standard/htmlStructure']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlStructure#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlContent", title = "${docLibTitleMap['standard/htmlContent']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlContent#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlInfo", title = "${docLibTitleMap['standard/htmlInfo']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlInfo#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlForm", title = "${docLibTitleMap['standard/htmlForm']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlForm#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlNav", title = "${docLibTitleMap['standard/htmlNav']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlNav#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "htmlScript", title = "${docLibTitleMap['standard/htmlScript']}", link = @MenuLink(target = "${basePageInterWebappUri}/standard/htmlScript#extLoginKey=OFF", urlMode = UrlMode.INTER_APP)),
            @MenuItem(name = "utilities", title = "${docLibTitleMap['utilities']}", link = @MenuLink(target = "${basePageInterWebappUri}/utilities#extLoginKey=OFF", urlMode = UrlMode.INTER_APP))
        }
    )
    public interface TemplateApiDocTabBar {}

    @Menu(
        name = "TemplateApiDocSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TemplateApiDocTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        itemsSortMode = "off"
    )
    public interface TemplateApiDocSideBar {}

    @Menu(
        name = "TemplateApiDocSubTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        actions = @MenuActions(set = {@SetAction(field = "basePageInterWebappUri", value = "${groovy: request.getContextPath() + '/docs/templating/ftl/lib'}"), @SetAction(field = "sideBarShown", value = "${context.widePage != true}", type = "Boolean"), @SetAction(field = "sideBarOnOffLabel", value = "${groovy: sideBarShown ? 'CommonOff' : 'CommonOn'}")}),
        items = {
            @MenuItem(name = "reloadDataModel", title = "Reload from source", widgetStyle = "+${styles.action_run_session} ${styles.action_update}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"targetLibPath"}), @Condition(type = NotEmpty.class, params = {"userLogin"}), @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"})}), link = @MenuLink(target = "${basePageInterWebappUri}/${targetLibPath}?reloadDataModel=true#extLoginKey=OFF", urlMode = UrlMode.INTER_APP))
        }
    )
    public interface TemplateApiDocSubTabBar {}

    @Menu(
        name = "LayoutDemoSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        items = {
            @MenuItem(name = "LayoutDemo", title = "Regular Demo", link = @MenuLink(target = "${demoTargetUrl}")),
            @MenuItem(name = "LayoutDemoDebug", title = "Advanced Demo", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "debugMode", value = "true")})),
            @MenuItem(name = "ExampleSideBar", title = "Example Sidebar", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "debugMode", fromField = "parameters.debugMode")}), subMenus = {@SubMenu(name = "LayoutDemoExampleSideBar", include = "component://webtools/widget/Menus.xml#LayoutDemoExampleSideBar")})
        }
    )
    public interface LayoutDemoSideBar {}

    @Menu(
        name = "LayoutDemoExampleSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Selected", title = "${uiLabelMap.CommonSelected}", widgetStyle = "+these-classes-manually-added ${styles.menu_sidebar_itemactive} ${styles.menu_sidebar_itemactivetarget}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "debugMode", fromField = "parameters.debugMode"), @MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "Enabled", title = "${uiLabelMap.CommonEnabled}", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "debugMode", fromField = "parameters.debugMode"), @MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "Disabled", title = "${uiLabelMap.CommonDisabled}", disabled = "true", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "debugMode", fromField = "parameters.debugMode"), @MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface LayoutDemoExampleSideBar {}

    @Menu(
        name = "DemoDataGeneratorSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ListDemoDataGeneratorServices", title = "${uiLabelMap.WebtoolsDemoDataGeneratorServiceList}", link = @MenuLink(target = "ListDemoDataGeneratorServices"))
        }
    )
    public interface DemoDataGeneratorSideBar {}

    @Menu(
        name = "SolrSideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "SolrStatus", title = "${uiLabelMap.CommonStatus}", link = @MenuLink(target = "SolrStatus")),
            @MenuItem(name = "SolrServices", title = "${uiLabelMap['GlResourceType.description.SERVICES']}", link = @MenuLink(target = "SolrServices")),
            @MenuItem(name = "SolrAdmin", title = "${uiLabelMap.SolrSolrAdmin}", disabled = "${not context.isSolrWebappLocal}", link = @MenuLink(target = "/solr/index.html", targetWindow = "_blank", urlMode = UrlMode.PLAIN, parameters = {@MenuParameter(paramName = "externalLoginKey", fromField = "externalLoginKey")})),
            @MenuItem(name = "SolrDoc", title = "Readme", link = @MenuLink(target = "SolrDoc"))
        }
    )
    public interface SolrSideBar {}

    @Menu(
        name = "SolrButtonBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "SolrAdmin", title = "${uiLabelMap.SolrSolrAdmin}", condition = @MenuItemCondition(conditions = {@Condition(type = True.class, params = {"isSolrWebappLocal"})}), link = @MenuLink(target = "/solr/index.html", targetWindow = "_blank", urlMode = UrlMode.PLAIN, parameters = {@MenuParameter(paramName = "externalLoginKey", fromField = "externalLoginKey")}))
        }
    )
    public interface SolrButtonBar {}

    @Menu(
        name = "SolrRebuildIndexSectionBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "rebuildSolrIndexJob", title = "${uiLabelMap.WebtoolsScheduleJob}: rebuildSolrIndex", link = @MenuLink(target = "scheduleJob", parameters = {@MenuParameter(paramName = "SERVICE_NAME", value = "rebuildSolrIndex")})),
            @MenuItem(name = "rebuildSolrIndexAutoJob", title = "${uiLabelMap.WebtoolsScheduleJob}: rebuildSolrIndexAuto", link = @MenuLink(target = "scheduleJob", parameters = {@MenuParameter(paramName = "SERVICE_NAME", value = "rebuildSolrIndexAuto")}))
        }
    )
    public interface SolrRebuildIndexSectionBar {}

    @Menu(
        name = "PropertyTabBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EntityLabels", title = "${uiLabelMap.WebtoolsEntityLabels}", link = @MenuLink(target = "EntityLabels", parameters = {@MenuParameter(paramName = "resourceId", fromField = "resourceId")})),
            @MenuItem(name = "FileLabels", title = "${uiLabelMap.WebtoolsLabelManagerFindLabels}", link = @MenuLink(target = "FileLabels"))
        }
    )
    public interface PropertyTabBar {}

    @Menu(
        name = "PropertySideBar",
        location = "component://webtools/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "PropertyTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface PropertySideBar {}

}
