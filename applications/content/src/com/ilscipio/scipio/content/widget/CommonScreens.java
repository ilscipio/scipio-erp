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

    @Screen(name = "webapp-common-actions", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.ContentCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.ContentCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/content/images/contentForum.css", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "contentmgr", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "ContentAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://content/widget/content/ContentMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.ContentContentManagerApplication}")
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"userLogin"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ContentWebAppUiDeprecated}", style = "common-msg-warning"
                    )}), position = 0)})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonContentAppDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonContentAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonContentAppSideBarMenu", location = "component://content/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonContentAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ContentViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonContentAppDecorator {}

    @Screen(name = "CommonCmsDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/cms/CMSMenus.xml#Cms")
    @Action(type = ActionType.SET, field = "currentCMSMenuItemName", fromField = "currentCMSMenuItemName", fromScope = "user")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonCmsDecorator {}

    @Screen(name = "CommonContentDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#Content")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(includeMenus = {
                    @IncludeMenu(name = "ContentSubButtonBar", location = "component://content/widget/content/ContentMenus.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"currentValue.contentId"
                    })}), actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "currentValueNameSep", value = "${groovy:(context.currentValue?.contentName && context.currentValue?.description) ? ', ' : ''}"
                    )}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${currentValue.contentName}${currentValueNameSep}${currentValue.description} [${currentValue.contentId}]  ${${extraFunctionName}}", decoratorSectionIncludes = {
                            @DecoratorSectionInclude(name = "body")})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.PageTitleAddContent}", decoratorSectionIncludes = {
                                    @DecoratorSectionInclude(name = "body")})}))})
        }
    )
    public interface CommonContentDecorator {}

    @Screen(name = "ContentDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#ContentTop")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface ContentDecorator {}

    @Screen(name = "CommonDataResourceDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/DataResourceMenus.xml#DataResource")
    @Action(type = ActionType.SET, field = "currentContentMenuItemName", fromField = "currentContentMenuItemName", fromScope = "user")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"currentValue.dataResourceId"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "dataresource", location = "component://content/widget/content/DataResourceMenus.xml"
                ),
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${currentValue.dataResourceName} [${currentValue.dataResourceId}]  ${${extraFunctionName}}", style = "heading"
            )}), position = 0)})
        }
    )
    public interface CommonDataResourceDecorator {}

    @Screen(name = "CommonCompDocDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "CompDoc")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
            ),
            @Widget(type = WidgetType.INCLUDE_MENU, name = "${menuName}", location = "component://content/widget/compdoc/CompDocMenus.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${subTitle}", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonCompDocDecorator {}

    @Screen(name = "CommonContentSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/contentsetup/ContentSetupMenus.xml#ContentSetup")
    @Action(type = ActionType.SET, field = "currentMenuItemName", fromField = "currentMenuItemName", fromScope = "user")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonContentSetupDecorator {}

    @Screen(name = "CommonDataResourceSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/datasetup/DataResourceSetupMenus.xml#DataResourceSetup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", fromScope = "user")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonDataResourceSetupDecorator {}

    @Screen(name = "CommonWebSiteDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#WebSite")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "webSiteId", defaultValue = "${parameters.webSiteId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.webSiteId} ${${extraFunctionName}}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty parameters.webSiteId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(includeMenus = {
                    @IncludeMenu(name = "websiteMenu", location = "component://content/widget/content/ContentMenus.xml"
                )}, position = 0)})
        }
    )
    public interface CommonWebSiteDecorator {}

    @Screen(name = "CommonLayoutDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/layout/LayoutMenus.xml#Layout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", fromScope = "user")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonLayoutDecorator {}

    @Screen(name = "main", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentMain")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.ContentWelcome}")})})})
        }
    )
    public interface main {}

    @Screen(name = "responseTreeLine", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "getContentAndDataResource", resultMapName = "contentData", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rsp.contentId")})
    @Action(type = ActionType.SET, field = "textData", fromField = "contentData.resultData.electronicText.textData")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = CompareField.class, params = {"responseContentId", "equals", "rsp.contentId"})}), widgets = @Widgets(containers = {@Container(style = "responseSelected", labels = {@Label(text = "${rsp.contentName} - ${rsp.description} [${rsp.contentId}]", style = "responseheader", position = 0)}, widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyReply}", style = "${styles.link_nav} ${styles.action_add}", target = "addForumThreadMessage", position = 1)}, containers = {@Container2(style = "responsetext", includeForms = {@IncludeForm(name = "EditForumThreadMessage", location = "component://content/widget/forum/ForumForms.xml")}, position = 2)})}), failWidgets = @Widgets(containers = {@Container(labels = {@Label(text = "${rsp.contentName} - ${rsp.description} [${rsp.contentId}]", style = "responseheader", position = 0)}, widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PartyReply}", style = "${styles.link_nav} ${styles.action_add}", target = "addForumThreadMessage", position = 1)}, containers = {@Container2(style = "responsetext", includeForms = {@IncludeForm(name = "EditForumThreadMessage", location = "component://content/widget/forum/ForumForms.xml")}, position = 2)})}))
    public interface responseTreeLine {}

    @Screen(name = "fonts.fo", location = "component://content/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://content/webapp/content/fonts.fo.ftl", platform = "xsl-fo")}))
    public interface fonts_fo {}

    @Screen(name = "CommonWebAnalyticsDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#WebSite")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "webSiteId", defaultValue = "${parameters.webSiteId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${parameters.webSiteId} ${${extraFunctionName}}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty parameters.webSiteId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(style = "button-bar", includeMenus = {
                    @IncludeMenu(name = "WebAnalyticsConfigButtonBar", location = "component://content/widget/content/ContentMenus.xml"
                )}, position = 0)})
        }
    )
    public interface CommonWebAnalyticsDecorator {}

    @Screen(name = "CommonForumDecorator", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Forum")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "pageTitle", value = "${uiLabelMap.${titleProperty}}")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${pageTitle}", style = "heading"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonForumDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://content/widget/content/ContentMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "ContentAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://content/widget/content/ContentMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonContentAppSideBarMenu", location = "component://content/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonContentAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonContentAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonContentAppSideBarMenu {}

}
