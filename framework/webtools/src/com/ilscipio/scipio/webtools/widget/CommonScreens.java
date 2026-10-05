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
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/webapp-common-actions_script1.groovy")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "static-common-actions", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/static-common-actions_script1.groovy")
    public interface static_common_actions {}

    @Screen(name = "main-decorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "TemporalExpressionUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonEntityLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.WebtoolsCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.WebtoolsCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "webtools", global = true)
    @Action(type = ActionType.SET, field = "helpTopic", value = "${groovy: context.webappName?.toUpperCase() + '_' + requestAttributes._CURRENT_VIEW_}")
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://webtools/widget/Menus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "WebtoolsAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.FrameworkWebTools}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/base-theme/bower_components/rainbow/rainbow.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @DecoratorScreen(
        name = "GlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonWebtoolsAppDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonWebtoolsAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"WEBTOOLS", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonWebtoolsAppSideBarMenu", location = "component://webtools/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonWebtoolsAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonWebtoolsAppDecorator {}

    @Screen(name = "main", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "Webtools")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "infoMessage", value = "<strong>Developer Info:</strong> You can use the <a href='ListDemoDataGeneratorServices'>demo generator</a>, to generate test-data", valueType = "PlainString")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.WebtoolsServer}", containers = {
                    @ContainerInScreenlet(style = "${styles.grid_row}", containers = {
                        @ContainerInScreenlet2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioMemoryInfo", location = "component://webtools/widget/CommonScreens.xml"
                        
                    )}),
                        @ContainerInScreenlet2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "DashboardWSLiveRequests", location = "component://webtools/widget/CommonWidgets.xml"
                        
                )})})})})}),
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(includeScreens = {
                    @IncludeScreen(name = "ScipioLogView", location = "component://webtools/widget/CommonScreens.xml"
                
                    )})}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioSecurityAlerts", location = "component://party/widget/partymgr/CommonScreens.xml"
                    )})})})
        }
    )
    public interface main {}

    @Screen(name = "ScipioMemoryInfo", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "pie")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartDatasets", value = "1")
    @Action(type = ActionType.SET, field = "xlabel")
    @Action(type = ActionType.SET, field = "ylabel")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.WebtoolsMemoryUsage}")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/dashboard/MemoryInfo.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"memoryInfo"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.WebtoolsStatsSystemMemory}", htmlTemplates = {@HtmlTemplate(location = "component://webtools/webapp/webtools/dashboard/MemoryInfo.ftl")})}))
    public interface ScipioMemoryInfo {}

    @Screen(name = "ScipioUserRequestCount", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "hour")
    @Action(type = ActionType.SET, field = "chartIntervalScopeLabel", value = "CommonHour")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "12")
    @Action(type = ActionType.SET, field = "chartDatasets", value = "1")
    @Action(type = ActionType.SET, field = "xlabel")
    @Action(type = ActionType.SET, field = "ylabel")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.WebtoolsStatsRequestsBy} ${chartIntervalScope}")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/dashboard/UserRequestCount.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"userRequestCount"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.WebtoolsStatsRequestsBy} ${uiLabelMap[chartIntervalScopeLabel]}", htmlTemplates = {@HtmlTemplate(location = "component://webtools/webapp/webtools/dashboard/UserRequestCount.ftl")})}))
    public interface ScipioUserRequestCount {}

    @Screen(name = "ScipioLogView", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "logFileName", resource = "debug", property = "log4j.appender.css.File", defaultValue = "runtime/logs/ofbiz.log", noLocale = true)
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/log/LogView.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://webtools/webapp/webtools/dashboard/ViewLog.ftl")})}))
    public interface ScipioLogView {}

    @Screen(name = "printStart", location = "component://webtools/widget/CommonScreens.xml")
    public interface printStart {}

    @Screen(name = "printDone", location = "component://webtools/widget/CommonScreens.xml")
    public interface printDone {}

    @Screen(name = "browsercerts", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "configuration")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "myCertificates")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsCertsX509")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/cert/viewbrowsercerts.ftl"
            )})
        }
    )
    public interface browsercerts {}

    @Screen(name = "PopUpDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "TemporalExpressionUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "LookupDecorator", location = "component://common/widget/CommonScreens.xml")}))
    public interface PopUpDecorator {}

    @Screen(name = "CommonEntityDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Entity")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonEntityDecorator {}

    @Screen(name = "CommonServiceDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "service")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"WEBTOOLS", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonServiceDecorator {}

    @Screen(name = "CommonImportExportDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "importExport")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonImportExportDecorator {}

    @Screen(name = "CommonArtifactDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "artifact")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonArtifactDecorator {}

    @Screen(name = "CommonLabelDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Property")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "labels")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonLabelDecorator {}

    @Screen(name = "CommonConfigurationDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "configuration")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonConfigurationDecorator {}

    @Screen(name = "CommonGeoManagementDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "geoManagement")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonGeoManagementDecorator {}

    @Screen(name = "CommonDemoDataGeneratorDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Entity")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ENTITY_MAINT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonDemoDataGeneratorDecorator {}

    @Screen(name = "StatsDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Stats")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"SERVER_STATS", "_VIEW"})}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"SERVER_STATS", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface StatsDecorator {}

    @Screen(name = "TemporalExpressionDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "tempexpr")
    @Action(type = ActionType.PROPERTY_MAP, resource = "TemporalExpressionUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap[titleProperty]}", global = true)
    @DecoratorScreen(
        name = "CommonConfigurationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"tempExprPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface TemporalExpressionDecorator {}

    @Screen(name = "SecurityDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "securityTargetDecoratorName", fromField = "securityTargetDecoratorName", defaultValue = "CommonWebtoolsAppDecorator")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "component://common/widget/SecurityScreens.xml"
    )
    public interface SecurityDecorator {}

    @Screen(name = "CommonDocumentationDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Development")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"userLogin"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                    )}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(decoratorSectionIncludes = {
                            @DecoratorSectionInclude(name = "body")})}))})
        }
    )
    public interface CommonDocumentationDecorator {}

    @Screen(name = "CommonSolrDecorator", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "SolrUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingEntityLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Solr")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/CommonSolrDecorator_script1.groovy")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"WEBTOOLS", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "SolrButtonBar", location = "component://webtools/widget/Menus.xml"
                ),
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
            )}))})
        }
    )
    public interface CommonSolrDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://webtools/widget/Menus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "WebtoolsAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://webtools/widget/Menus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonWebtoolsAppSideBarMenu", location = "component://webtools/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonWebtoolsAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"WEBTOOLS", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonWebtoolsAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonWebtoolsAppSideBarMenu {}

}
