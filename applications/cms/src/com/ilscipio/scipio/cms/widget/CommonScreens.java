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

    @Screen(name = "webapp-common-actions", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "static-common-actions", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsEditorCommon.groovy")
    public interface static_common_actions {}

    @Screen(name = "main-decorator", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CMSUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.CMSCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.CMSCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "cms", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "MainAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://cms/widget/CMSMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.CMSApplication}", global = true)
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
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "component://cms/widget/CommonScreens.xml"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonCMSAppDecorator", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonCMSAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CMS", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonCMSAppSideBarMenu", location = "component://cms/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonCMSAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CmsViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonCMSAppDecorator {}

    @Screen(name = "CommonSettingsDecorator", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Settings")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonSettingsDecorator {}

    @Screen(name = "CommonCmsImportExportDecorator", location = "component://cms/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "ImportExport")
    @Action(order = 1, type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(order = 2, type = ActionType.SET, field = "useEntityMaintCheck", fromField = "useEntityMaintCheck", valueType = "Boolean", defaultValue = "false", global = true)
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"useEntityMaintCheck", "equals", "true", "Boolean"})}), then = @Actions(value = {@Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}))}))
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifCompare = {
                        @IfCompare(field = "useEntityMaintCheck", operator = "equals", value = "false", type = "Boolean"
                    )}, ifHasPermission = {
                        @IfHasPermission(permission = "ENTITY_MAINT")})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                        )}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface CommonCmsImportExportDecorator {}

    @Screen(name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/base-theme/bower_components/rainbow/rainbow.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_HDR_JAVASCRIPT[]", value = "/base-theme/bower_components/rainbow/rainbow-custom.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/lib/codemirror.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/addon/fold/foldgutter.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/addon/hint/show-hint.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/lib/codemirror.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/edit/matchbrackets.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/edit/matchtags.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/edit/closetag.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/fold/foldcode.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/fold/foldgutter.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/fold/brace-fold.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/fold/xml-fold.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/fold/comment-fold.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/hint/show-hint.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/hint/xml-hint.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/hint/html-hint.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/mode/xml/xml.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/mode/htmlmixed/htmlmixed.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/mode/javascript/javascript.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/mode/vbscript/vbscript.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/mode/groovy/groovy.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror-mode-freemarker/freemarker/freemarker.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/CodeEditorCommonIncludes_script1.groovy")
    public interface CodeEditorCommonIncludes {}

    @Screen(name = "CmsBlank", location = "component://cms/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webcommon/common/CmsBlank.ftl")}))
    public interface CmsBlank {}

    @Screen(name = "404", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "Cms404")
    @Action(type = ActionType.SET, field = "noTitle", value = "true", global = true)
    @Action(type = ActionType.SET, field = "isErrorPage", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "${errorTemplateLocation}"
            )})
        }
    )
    public interface _404 {}

    @Screen(name = "CmsContentTree", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsContentTree.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://cms/webapp/cms/pages/contentTree.ftl")})}))
    public interface CmsContentTree {}

    @Screen(name = "CmsPageViewMappingsSelect", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetPageViewMappings.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/pages/pageViewMappingsSelect.ftl")}))
    public interface CmsPageViewMappingsSelect {}

    @Screen(name = "MainSideBarMenu", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://cms/widget/CMSMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "MainAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://cms/widget/CMSMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonCMSAppSideBarMenu", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonCMSAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CMS", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonCMSAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonCMSAppSideBarMenu {}

}
