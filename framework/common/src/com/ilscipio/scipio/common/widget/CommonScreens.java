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
public class CommonScreens {

    @Screen(name = "AjaxGlobalDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/htmlheader-for-ajax.ftl", position = 0), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/htmlfooter-for-ajax.ftl", position = 2)}, sections = {@SectionNested(name = "Global-Column-Main", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")}), position = 1)}))
    public interface AjaxGlobalDecorator {}

    @Screen(name = "ajaxAutocompleteOptions", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "autocompleteOptions", fromField = "parameters.autocompleteOptions")
    @DecoratorScreen(
        name = "AjaxGlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/ajaxAutocompleteOptions.ftl"
            )})
        }
    )
    public interface ajaxAutocompleteOptions {}

    @Screen(name = "GlobalActions", location = "component://common/widget/CommonScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"hotwire.enabled"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/common/js/hotwire-common.js", global = true), @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/common/dist/libs/@hotwired/turbo/turbo.es2017-umd.js", global = true)}))
    @Action(order = 1, type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/plugins/validate/jquery.validate.min.js", global = true)
    @Action(order = 2, type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/plugins/fjTimer/jquerytimer-min.js", global = true)
    @Action(order = 3, type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/images/selectall.js", global = true)
    @Action(order = 4, type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/images/fieldlookup.js", global = true)
    @Action(order = 5, type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/images/miscAjaxFunctions.js", global = true)
    @Action(order = 6, type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/images/selectMultipleRelatedValues.js", global = true)
    @Action(order = 7, type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/images/util.js", global = true)
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Action(order = 9, type = ActionType.SET, field = "setContextScipioTmplGlobalVarsAsGlobal", value = "true", valueType = "Boolean")
    @Action(order = 10, type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/SetContextScipioTmplGlobalVars.groovy")
    @Action(order = 11, type = ActionType.SCRIPT, location = "component://common/webapp/common/WEB-INF/actions/generated/GlobalActions_script1.groovy")
    @Action(order = 12, type = ActionType.SCRIPT, location = "component://common/webapp/common/WEB-INF/actions/generated/GlobalActions_script2.groovy")
    @Action(order = 13, type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/GetUserNotifications.groovy")
    public interface GlobalActions {}

    @Screen(name = "GlobalTemplateActions", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.commonHeaderImageLinkUrl", fromField = "layoutSettings.commonHeaderImageLinkUrl", defaultValue = "main", global = true)
    @Action(type = ActionType.SET, field = "headerTemplateLocation", fromField = "headerTemplateLocation", defaultValue = "${layoutSettings.VT_HDR_TMPLT_LOC[0]}")
    @Action(type = ActionType.SET, field = "footerTemplateLocation", fromField = "footerTemplateLocation", defaultValue = "${layoutSettings.VT_FTR_TMPLT_LOC[0]}")
    @Action(type = ActionType.SET, field = "appbarTemplateLocation", fromField = "appbarTemplateLocation", defaultValue = "${layoutSettings.VT_NAV_TMPLT_LOC[0]}")
    @Action(type = ActionType.SET, field = "appbarOpenTemplateLocation", fromField = "appbarOpenTemplateLocation", defaultValue = "${layoutSettings.VT_NAV_OPEN_TMPLT[0]}")
    @Action(type = ActionType.SET, field = "appbarCloseTemplateLocation", fromField = "appbarCloseTemplateLocation", defaultValue = "${layoutSettings.VT_NAV_CLOSE_TMPLT[0]}")
    @Action(type = ActionType.SET, field = "messagesTemplateLocation", fromField = "messagesTemplateLocation", defaultValue = "${layoutSettings.VT_MSG_TMPLT_LOC[0]}", global = true)
    @Action(type = ActionType.SET, field = "loginTemplateLocation", fromField = "loginTemplateLocation", defaultValue = "${layoutSettings.VT_LOGIN_TMPLT_LOC[0]}", global = true)
    @Action(type = ActionType.SET, field = "errorTemplateLocation", fromField = "errorTemplateLocation", defaultValue = "${layoutSettings.VT_ERROR_TMPLT_LOC[0]}", global = true)
    @Action(type = ActionType.SET, field = "origTitle", fromField = "title")
    @Action(type = ActionType.SET, field = "origTitleProperty", fromField = "titleProperty")
    @Action(type = ActionType.SCRIPT, location = "component://common/webapp/common/WEB-INF/actions/generated/GlobalTemplateActions_script1.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "layoutSettings.companyName", defaultValue = "SCIPIO", global = true)
    @Action(type = ActionType.SET, field = "customSideBar", fromField = "styles.customSideBar", defaultValue = "false", global = true)
    @Action(type = ActionType.SET, field = "scpOutParams.layoutSettings.javaScripts", fromField = "layoutSettings.javaScripts", global = true)
    @Action(type = ActionType.SET, field = "scpOutParams.layoutSettings.javaScriptsFooter", fromField = "layoutSettings.javaScriptsFooter", global = true)
    @Action(type = ActionType.SET, field = "scpOutParams.layoutSettings.styleSheets", fromField = "layoutSettings.styleSheets", global = true)
    public interface GlobalTemplateActions {}

    @Screen(name = "AllGlobalActions", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GlobalActions")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GlobalTemplateActions")
    public interface AllGlobalActions {}

    @Screen(name = "GlobalDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GlobalActions")
    @Action(order = 1, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "GlobalTemplateActions")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"customSideBar"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "headAppContainsExpr", value = "!$Global-Column-Main, !$Global-Column-Right")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "headAppContainsExpr", value = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"webSiteFound"})}), widgets = @Widgets(containers = {@Container(id = "content-main-section", position = 5, style = "${styles.grid_theme}", sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = EmptySection.class, params = {"content-full-screen"})}), widgets = @WidgetsForContainer(sections = {@SectionNested2(name = "Global-Content-Full", widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "content-full-screen")}))}), failWidgets = @WidgetsForContainer(sections = {@SectionNested2(name = "Global-Content-Main", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"customSideBar"})}), widgets = @WidgetsForContainer2(sections = {@SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"userLogin"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_columns_main_style}"), @Action(type = ActionType.SET, field = "noTitle", value = "true")}), widgets = @WidgetsForContainer3(containers = {@Container4(id = "content-main-body", includeScreens = {@IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml")})}), failWidgets = @WidgetsForContainer3(sections = {@SectionNested4(actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_0_main}")}), widgets = @WidgetsForContainer4(containers = {@Container4(id = "content-main-body", includeScreens = {@IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml")})}))}))}), failWidgets = @WidgetsForContainer2(sections = {@SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"userLogin"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_columns_main_style}"), @Action(type = ActionType.SET, field = "noTitle", value = "true")}), widgets = @WidgetsForContainer3(containers = {@Container4(id = "content-main-body", includeScreens = {@IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml")})}), failWidgets = @WidgetsForContainer3(sections = {@SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"left-column"}), @ConditionNode(type = False.class, params = {"showLeftColumn"})}), @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"right-column"}), @ConditionNode(type = False.class, params = {"showRightColumn"})})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_0_main}")}), widgets = @WidgetsForContainer4(containers = {@Container4(id = "content-main-body", includeScreens = {@IncludeScreen(name = "column-main", location = "component://common/widget/CommonScreens.xml")})})), @SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"left-column"}), @ConditionNode(not = true, type = False.class, params = {"showLeftColumn"})}), @Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"right-column"}), @ConditionNode(type = False.class, params = {"showRightColumn"})})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnLeftStyle", value = "${styles.grid_sidebar_1_side}"), @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_1_main}")}), widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-left"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main")})), @SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = EmptySection.class, params = {"left-column"}), @ConditionNode(type = False.class, params = {"showLeftColumn"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"right-column"}), @ConditionNode(not = true, type = False.class, params = {"showRightColumn"})})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnRightStyle", value = "${styles.grid_sidebar_1_side}"), @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_1_main}")}), widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-right")})), @SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"left-column"}), @ConditionNode(not = true, type = False.class, params = {"showLeftColumn"})}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = EmptySection.class, params = {"right-column"}), @ConditionNode(not = true, type = False.class, params = {"showRightColumn"})})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnLeftStyle", value = "${styles.grid_sidebar_2_side}"), @Action(type = ActionType.SET, field = "columnRightStyle", value = "${styles.grid_sidebar_2_side}"), @Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_sidebar_2_main}")}), widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-left"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-right")}))}))}))}))})}, sections = {@SectionNested(contains = "${headAppContainsExpr}, *", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.ajaxUpdateEvent"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/htmlHeadOpen.ftl")}, sections = {@SectionNested2(name = "Global-Head-Header", widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${headerTemplateLocation}")})), @SectionNested2(name = "Global-Main-Nav", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"appbarOpenTemplateLocation"})}), widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${appbarOpenTemplateLocation}")}), failWidgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${appbarTemplateLocation}")}))})), @SectionNested(contains = "${headAppContainsExpr}, *", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.ajaxUpdateEvent"}), @Condition(type = And.class, tree = {@ConditionNode(not = true, type = True.class, params = {"customSideBar"})})}), widgets = @WidgetsForContainer(sections = {@SectionNested2(name = "Global-App-Nav", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"userLogin"})}), widgets = @WidgetsForContainer2(sections = {@SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"appheaderTemplate"})}), widgets = @WidgetsForContainer3(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${appheaderTemplate}")}), failWidgets = @WidgetsForContainer3(sections = {@SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.applicationTitle"})}), widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.LABEL, text = "${parameters.applicationTitle}", style = "apptitle")})), @SectionNested4(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"applicationMenuLocation"})}), widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${applicationMenuName}", location = "${applicationMenuLocation}")}))}))}))})), @SectionNested(contains = "${headAppContainsExpr}, *", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.ajaxUpdateEvent"}), @Condition(type = NotEmpty.class, params = {"appbarCloseTemplateLocation"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${appbarCloseTemplateLocation}")})), @SectionNested(name = "Global-Head-Scripts", contains = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right, *", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/commonHeadScripts.ftl")})), @SectionNested(name = "Global-Pre-Content", contains = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right, *", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "pre-content")})), @SectionNested(name = "Global-Foot-Scripts", contains = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right, *", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/commonFootScripts.ftl")})), @SectionNested(contains = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right, *", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.ajaxUpdateEvent"})}), widgets = @WidgetsForContainer(sections = {@SectionNested2(name = "Global-Footer", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"applicationFooterTemplate"})}), widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${footerTemplateLocation}")}), failWidgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${applicationFooterTemplate}")}))}))}), failWidgets = @Widgets(sections = {@SectionNested(contains = "${headAppContainsExpr}, *", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/htmlHeadOpen.ftl")}, sections = {@SectionNested2(name = "Global-Head-Header", widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://base-theme/includes/fallback/header.ftl")}))})), @SectionNested(name = "Global-Content-Main", actions = @Actions(value = {@Action(type = ActionType.SET, field = "columnMainStyle", value = "${styles.grid_columns_main_style}"), @Action(type = ActionType.SET, field = "noTitle", value = "true")}), widgets = @WidgetsForContainer(containers = {@Container2(id = "content-main-body", containers = {@Container3(id = "main-content", style = "${columnMainStyle}", htmlTemplates = {@HtmlTemplate(location = "${messagesTemplateLocation}"), @HtmlTemplate(location = "component://base-theme/includes/fallback/main.ftl")})})})), @SectionNested(contains = "!$Global-Column-Main, !$Global-Column-Left, !$Global-Column-Right, *", widgets = @WidgetsForContainer(sections = {@SectionNested2(name = "Global-Footer", widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://base-theme/includes/fallback/footer.ftl")}))}))}))
    public interface GlobalDecorator {}

    @Screen(name = "LookupDecorator", location = "component://common/widget/CommonScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Compare.class, params = {"parameters.ajaxLookup", "equals", "Y"})}), failWidgets = @WidgetsForContainer(sections = {@SectionNested2(actions = @Actions(value = {@Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true), @Action(type = ActionType.SET, field = "searchType", fromField = "parameters.searchType", defaultValue = "${searchType}"), @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/FindAutocompleteOptions.groovy")}), widgets = @WidgetsForContainer2(decorator = @DecoratorScreenNested(name = "AjaxGlobalDecorator", location = "component://common/widget/CommonScreens.xml", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/ajaxAutocompleteOptions.ftl")}))})))}))
    @Section(actions = @Actions(value = {@Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true), @Action(type = ActionType.SERVICE, serviceName = "getUserPreferenceGroup", resultMapName = "prefResult", fieldMaps = {@FieldMap(fieldName = "userPrefGroupTypeId", value = "GLOBAL_PREFERENCES")}), @Action(type = ActionType.SET, field = "userPreferences", fromField = "prefResult.userPrefMap", global = true), @Action(type = ActionType.PROPERTY_MAP, resource = "general", mapName = "generalProperties", global = true), @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy"), @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/fieldlookup.js", global = true), @Action(type = ActionType.SET, field = "messagesTemplateLocation", fromField = "layoutSettings.VT_MSG_TMPLT_LOC[0]", defaultValue = "component://common/webcommon/includes/messages.ftl", global = true)}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.presentation", "not-equals", "layer"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/lookup.ftl")})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = EmptySection.class, params = {"body"})}), widgets = @WidgetsForContainer(containers = {@Container2(id = "content-main-body", htmlTemplates = {@HtmlTemplate(location = "${messagesTemplateLocation}")}, decoratorSectionIncludes = {@DecoratorSectionInclude(name = "body")})}), failWidgets = @WidgetsForContainer(screenlets = {@ScreenletNested(title = "${title}", id = "findScreenlet", collapsible = true, padded = false, containers = {
                    @ContainerInScreenlet(id = "search-options", decoratorSectionIncludes = {
                        @DecoratorSectionInclude(name = "search-options")})}), @ScreenletNested(containers = {
                    @ContainerInScreenlet(id = "search-results", decoratorSectionIncludes = {
                        @DecoratorSectionInclude(name = "search-results")})})})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.presentation", "not-equals", "layer"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/lookupFooter.ftl")}))}))
    public interface LookupDecorator {}

    @Screen(name = "SimpleDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/images/maincss.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.rtlStyleSheets[+0]", value = "/images/mainrtl.css", global = true)
    @Action(type = ActionType.SET, field = "initialLocaleComplete", value = "${groovy:parameters?.userLogin?.lastLocale}", valueType = "String", defaultValue = "${groovy:locale.toString()}")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "${groovy: org.ofbiz.common.JsLanguageFilesMapping.datejs.getFilePath(initialLocaleComplete)}", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "${groovy: org.ofbiz.common.JsLanguageFilesMapping.jquery.getFilePath(initialLocaleComplete)}", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "${groovy: org.ofbiz.common.JsLanguageFilesMapping.validation.getFilePath(initialLocaleComplete)}", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery-ui/jquery-ui.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/plugins/jeditable/jquery.jeditable.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/plugins/fjTimer/jquerytimer-min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/plugins/validate/jquery.validate.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery-migrate/jquery-migrate.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/images/jquery/jquery-1.11.0.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/OpenLayers-2.13.1.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/selectall.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/images/fieldlookup.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.shortcutIcon", value = "/images/scipio/favicon.ico", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "logoImageUrl", value = "/images/scipio/scipio-logo.svg")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/simple.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/simple.fo.ftl", platform = "xsl-fo"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/minimal-decorator.ftl", platform = "xml")}))
    public interface SimpleDecorator {}

    @Screen(name = "column-left", location = "component://common/widget/CommonScreens.xml")
    @Section(name = "Global-Column-Left", contains = "!$Global-Column-Main, !$Global-Column-Right, *", widgets = @Widgets(containers = {@Container(id = "left-sidebar", style = "${columnLeftStyle}", containers = {@Container2(style = "sidebar", decoratorSectionIncludes = {@DecoratorSectionInclude(name = "menu-bar"), @DecoratorSectionInclude(name = "left-column")})})}))
    public interface column_left {}

    @Screen(name = "column-right", location = "component://common/widget/CommonScreens.xml")
    @Section(name = "Global-Column-Right", contains = "!$Global-Column-Main, !$Global-Column-Left, *", widgets = @Widgets(containers = {@Container(id = "right-sidebar", style = "${columnRightStyle}", containers = {@Container2(style = "sidebar", decoratorSectionIncludes = {@DecoratorSectionInclude(name = "right-column")})})}))
    public interface column_right {}

    @Screen(name = "column-main", location = "component://common/widget/CommonScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"noTitle"})}), failWidgets = @WidgetsForContainer(containers = {@Container2(id = "main-content", style = "${columnMainStyle}", sections = {@SectionNested2(name = "Global-Column-Main-Inner", widgets = @WidgetsForContainer2(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${messagesTemplateLocation}"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "pre-body"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")}))})}))
    @Section(name = "Global-Column-Main", contains = "!$Global-Column-Left, !$Global-Column-Right, *", widgets = @Widgets(containers = {@Container(id = "main-content", style = "${columnMainStyle}", sections = {@SectionNested(name = "Global-Column-Main-Inner", widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${messagesTemplateLocation}"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "pre-body"), @Widget(type = WidgetType.LABEL, text = "${headerTitle}", style = "h1"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")}))})}))
    public interface column_main {}

    @Screen(name = "FoReportDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "logoImageUrl", value = "/images/scipio/scipio-logo.svg", global = true)
    @Action(type = ActionType.SET, field = "chiSetGlobal", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/CommonHeaderInfo.groovy")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultFontFamily", resource = "fop.properties", property = "fop.font.family", defaultValue = "Arial")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/fo/ScipioPdfTemplateDinA4.fo.ftl", platform = "xsl-fo")}))
    public interface FoReportDecorator {}

    @Screen(name = "GlobalFoDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "chiSetGlobal", value = "true", valueType = "Boolean")
    @Action(order = 1, type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/CommonHeaderInfo.groovy")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"layoutSettings.commonHeaderImageUrl"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "layoutSettings.commonHeaderImageUrl", fromField = "logoImageUrl", defaultValue = "/images/scipio/scipio-logo.svg", global = true)}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/fo/start.fo.ftl", platform = "xsl-fo"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/fo/basic-header.fo.ftl", platform = "xsl-fo"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/fo/basic-footer.fo.ftl", platform = "xsl-fo"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/fo/end.fo.ftl", platform = "xsl-fo")}))
    public interface GlobalFoDecorator {}

    @Screen(name = "FoError", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "logoImageUrl", value = "/images/scipio/scipio-logo.svg", global = true)
    @Action(type = ActionType.SET, field = "chiSetGlobal", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/CommonHeaderInfo.groovy")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface FoError {}

    @Screen(name = "ScipioEmailDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "logoImageUrl", value = "/images/scipio/scipio-logo.svg", global = true)
    @Action(type = ActionType.SET, field = "chiSetGlobal", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/CommonHeaderInfo.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/email/openEmailBody.ftl", platform = "email", position = 0), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 2), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/email/closeEmailBody.ftl", platform = "email", position = 4)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"headerTemplate"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${headerTemplate}", platform = "email")}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/email/emailHeader.ftl", platform = "email")}), position = 1), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"footerTemplate"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${footerTemplate}", platform = "email")}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/email/emailFooter.ftl", platform = "email")}), position = 3)}))
    public interface ScipioEmailDecorator {}

    @Screen(name = "login", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogin")
    @Action(type = ActionType.SET, field = "noTitle", value = "true", global = true)
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "${loginTemplateLocation}"
            )})
        }
    )
    public interface login {}

    @Screen(name = "error", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleError")
    @Action(type = ActionType.SET, field = "noTitle", value = "true", global = true)
    @Action(type = ActionType.SET, field = "isErrorPage", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webapp/common/WEB-INF/actions/generated/error_script1.groovy")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "${errorTemplateLocation}"
            )})
        }
    )
    public interface error {}

    @Screen(name = "ajaxNotLoggedIn", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "AjaxGlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonSessionTimeoutPleaseLogIn}", style = "common-msg-info-important"
            )})
        }
    )
    public interface ajaxNotLoggedIn {}

    @Screen(name = "requirePasswordChange", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/changePassword.ftl"
            )})
        }
    )
    public interface requirePasswordChange {}

    @Screen(name = "forgotPassword", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleForgotPassword")
    @Action(type = ActionType.SET, field = "noTitle", value = "true", global = true)
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/forgotPassword.ftl"
            )})
        }
    )
    public interface forgotPassword {}

    @Screen(name = "ListLocales", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonChooseLanguage}")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocales", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListLocales {}

    @Screen(name = "listLocales", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/listLocales.ftl")}))
    public interface listLocalesLc {}

    @Screen(name = "ListLocalesCompact", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonChooseLanguage}")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listLocalesCompact", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListLocalesCompact {}

    @Screen(name = "listLocalesCompact", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "setLocalesTarget", fromField = "setLocalesTarget", defaultValue = "${groovy: request.getAttribute('setLocalesTarget')}")
    @Action(type = ActionType.SET, field = "setLocalesTargetView", fromField = "setLocalesTargetView", defaultValue = "${groovy: request.getAttribute('setLocalesTargetView')}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/listLocalesCompact.ftl")}))
    public interface listLocalesCompactLc {}

    @Screen(name = "ListTimezones", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonTime}")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listTimezones", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListTimezones {}

    @Screen(name = "listTimezones", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/listTimezones.ftl")}))
    public interface listTimezonesLc {}

    @Screen(name = "ListVisualThemes", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonVisualThemes}")
    @Action(type = ActionType.SET, field = "parameters.presentation", value = "window")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "listVisualThemes", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface ListVisualThemes {}

    @Screen(name = "listVisualThemes", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "visualThemeSetId", fromField = "webSite.visualThemeSetId", defaultValue = "BACKOFFICE")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "VisualTheme", list = "visualThemes", conditions = {@ConditionExpr(fieldName = "visualThemeSetId", fromField = "visualThemeSetId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/listVisualThemes.ftl")}))
    public interface listVisualThemesLc {}

    @Screen(name = "EventMessages", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/messages.ftl")}))
    public interface EventMessages {}

    @Screen(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "httpMethod", value = "${groovy: request?.getMethod()?.toLowerCase()}")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = EmptySection.class, params = {"menu-bar"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "menu-bar")}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTAINER, style = "clear")}, screenlets = {@Screenlet(title = "${uiLabelMap.CommonSearchOptions}", name = "findScreenlet", collapsible = true, containers = {@Container(id = "search-options", decoratorSectionIncludes = {@DecoratorSectionInclude(name = "search-options")})})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = True.class, params = {"findScreenShowResults"}), @ConditionNode(type = And.class), @ConditionNode(parent = 1, not = true, type = False.class, params = {"findScreenShowResults"}), @ConditionNode(parent = 1, type = Or.class), @ConditionNode(parent = 3, type = Compare.class, params = {"httpMethod", "equals", "post"}), @ConditionNode(parent = 3, type = Compare.class, params = {"parameters.find", "equals", "true"}), @ConditionNode(parent = 3, type = Compare.class, params = {"parameters.find", "equals", "Y"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.VIEW_SIZE"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.VIEW_INDEX"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.viewSize"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.viewIndex"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.noConditionFind"}), @ConditionNode(parent = 3, not = true, type = Empty.class, params = {"parameters.inputFields"})})}), widgets = @Widgets(screenlets = {@Screenlet(padded = false, labels = {@Label(text = "${uiLabelMap.CommonSearchResults}", style = "heading")}, containers = {@Container(id = "search-results", style = "search-results", decoratorSectionIncludes = {@DecoratorSectionInclude(name = "search-results")})})}))
    public interface FindScreenDecorator {}

    @Screen(name = "help", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleHelp")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonHelpUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "helpText", value = "${uiLabelMap[parameters.topic]}", defaultValue = "${uiLabelMap.HelpNotFound}")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleHelp}", style = "h1"
            ),
            @Widget(type = WidgetType.LABEL, text = "${helpText}")})
        }
    )
    public interface help {}

    @Screen(name = "viewBlocked", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewBlocked")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/viewBlocked.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/messages.ftl")}))
    public interface viewBlocked {}

    @Screen(name = "geoChart", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommonGeoLocation")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/geolocation.ftl")}))
    public interface geoChart {}

    @Screen(name = "breadcrumbs", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/breadcrumbs.ftl")}))
    public interface breadcrumbs {}

    @Screen(name = "states", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "statesPreselect", fromField = "statesPreselect", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "statesPreselectFirst", fromField = "statesPreselectFirst", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "statesAllowEmpty", fromField = "statesAllowEmpty", valueType = "Boolean", defaultValue = "false")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/states.ftl")}))
    public interface states {}

    @Screen(name = "countries", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultSystemCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @Action(type = ActionType.SET, field = "defaultCountryGeoId", fromField = "defaultCountryGeoIdOverride", defaultValue = "${defaultSystemCountryGeoId}")
    @Action(type = ActionType.SET, field = "countriesUseDefault", fromField = "countriesUseDefault", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "countriesPreselect", fromField = "countriesPreselect", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "countriesPreselectFirst", fromField = "countriesPreselectFirst", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "countriesAllowEmpty", fromField = "countriesAllowEmpty", valueType = "Boolean", defaultValue = "false")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/countries.ftl")}))
    public interface countries {}

    @Screen(name = "cctypes", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/CcTypes.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/cctypes.ftl")}))
    public interface cctypes {}

    @Screen(name = "ccmonths", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/ccmonths.ftl")}))
    public interface ccmonths {}

    @Screen(name = "ccyears", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "thisDate", fromField = "nowTimestamp")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/ccyears.ftl")}))
    public interface ccyears {}

    @Screen(name = "genericLink", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/genericLink.ftl")}))
    public interface genericLink {}

    @Screen(name = "scipioMenuWidgetWrapper", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${scipioWidgetWrapperArgs.resName}", location = "${scipioWidgetWrapperArgs.resLocation}")}))
    public interface scipioMenuWidgetWrapper {}

    @Screen(name = "scipioFormWidgetWrapper", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "${scipioWidgetWrapperArgs.resName}", location = "${scipioWidgetWrapperArgs.resLocation}")}))
    public interface scipioFormWidgetWrapper {}

    @Screen(name = "scipioTreeWidgetWrapper", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_TREE, name = "${scipioWidgetWrapperArgs.resName}", location = "${scipioWidgetWrapperArgs.resLocation}")}))
    public interface scipioTreeWidgetWrapper {}

    @Screen(name = "scipioScreenWidgetWrapper", location = "component://common/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${scipioWidgetWrapperArgs.resName}", location = "${scipioWidgetWrapperArgs.resLocation}")}))
    public interface scipioScreenWidgetWrapper {}

    @Screen(name = "ComplexMenu", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareComplexMenu.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"useCplxMenu"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${cplxName}", location = "${cplxLoc}")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${smplName}", location = "${smplLoc}")}))
    public interface ComplexMenu {}

    @Screen(name = "DeriveComplexMenuItems", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/DeriveComplexMenuItems.groovy")
    public interface DeriveComplexMenuItems {}

    @Screen(name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.subMenuFilter", fromField = "menuCfg.subMenuFilter", defaultValue = "current")
    @Action(type = ActionType.SET, field = "menuCfg.nameSuffix", fromField = "menuCfg.nameSuffix", defaultValue = "SideBar")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareComplexMenu.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"useCplxMenu"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${cplxName}", location = "${cplxLoc}")}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "${smplName}", location = "${smplLoc}")}))
    public interface ComplexSideBarMenu {}

    @Screen(name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.nameSuffix", fromField = "menuCfg.nameSuffix", defaultValue = "SideBar")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/DeriveComplexMenuItems.groovy")
    public interface DeriveComplexSideBarMenuItems {}

    @Screen(name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/CommonSideBarMenu.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"hasTargetMenu"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${targetMenuName}", location = "${targetMenuLoc}")}))
    public interface CommonSideBarMenu {}

}
