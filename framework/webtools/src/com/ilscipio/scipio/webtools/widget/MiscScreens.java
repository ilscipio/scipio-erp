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
public class MiscScreens {

    @Screen(name = "viewdatafile", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsDataFileMainTitle")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "data")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/datafile/viewdatafile.groovy")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/datafile/viewdatafile.ftl"
                )})})
        }
    )
    public interface viewdatafile {}

    @Screen(name = "WebtoolsLayoutDemoActions", location = "component://webtools/widget/MiscScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "debugMode", fromField = "parameters.debugMode", valueType = "Boolean", defaultValue = "false")
    @Action(order = 1, type = ActionType.SET, field = "allowErrors", fromField = "parameters.allowErrors", valueType = "Boolean", defaultValue = "false")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"parameters.debugMode"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoActions_script1.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoActions_script2.groovy")}))
    @Action(order = 3, type = ActionType.SET, field = "adminScriptsBaseDir", value = "component://webtools/webapp/webtools/WEB-INF/actions")
    @Action(order = 4, type = ActionType.SCRIPT, location = "${adminScriptsBaseDir}/misc/PrepareLayoutDemo.groovy")
    @Action(order = 5, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "hotwireActions")
    public interface WebtoolsLayoutDemoActions {}

    @Screen(name = "WebtoolsLayoutDemo", location = "component://webtools/widget/MiscScreens.xml")
    @Action(order = 0, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "WebtoolsLayoutDemoActions")
    @Action(order = 1, type = ActionType.SET, field = "testtest", value = "test ' asdf ' > asdf < asdfasdf")
    @Action(order = 2, type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(order = 3, type = ActionType.SET, field = "titleProperty", value = "WebtoolsLayoutDemo")
    @Action(order = 4, type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "LayoutDemo")
    @Action(order = 5, type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: (context.debugMode ? 'LayoutDemoDebug' : 'LayoutDemo')}")
    @Action(order = 6, type = ActionType.SET, field = "demoText", value = "${uiLabelMap.WebtoolsLayoutDemoText}", global = true)
    @Action(order = 7, type = ActionType.SET, field = "errorMessage", fromField = "demoText", global = true)
    @Action(order = 8, type = ActionType.SET, field = "eventMessage", fromField = "demoText", global = true)
    @Action(order = 9, type = ActionType.SET, field = "demoTargetUrl", value = "LayoutDemo")
    @Action(order = 10, type = ActionType.SET, field = "demoParam1", value = "one")
    @Action(order = 11, type = ActionType.SET, field = "demoParam2", value = "two")
    @Action(order = 12, type = ActionType.SET, field = "demoParam3", value = "three")
    @Action(order = 13, type = ActionType.SET, field = "demoMap.name", value = "${uiLabelMap.WebtoolsLayoutDemo}")
    @Action(order = 14, type = ActionType.SET, field = "demoMap.description", value = "${uiLabelMap.WebtoolsLayoutDemoText}")
    @Action(order = 15, type = ActionType.SET, field = "demoMap.dropDown", value = "Y")
    @Action(order = 16, type = ActionType.SET, field = "demoMap.checkBox", value = "Y")
    @Action(order = 17, type = ActionType.SET, field = "demoMap.radioButton", value = "Y")
    @Action(order = 18, type = ActionType.SET, field = "demoList[]", fromField = "demoMap")
    @Action(order = 19, type = ActionType.SET, field = "demoList[]", fromField = "demoMap")
    @Action(order = 20, type = ActionType.SET, field = "demoList[]", fromField = "demoMap")
    @Action(order = 21, type = ActionType.SET, field = "altRowStyle")
    @Action(order = 22, type = ActionType.SET, field = "headerStyle", value = "header-row-1")
    @Action(order = 23, type = ActionType.SET, field = "tableStyle", value = "${styles.table_data_list} light-grid")
    @Action(order = 24, type = ActionType.SET, field = "ofbizWidgetsLayoutScreenLocation", value = "component://webtools/widget/MiscScreens.xml#WebtoolsLayoutDemoOfbizWidgets")
    @Action(order = 25, type = ActionType.SET, field = "parameters.showLeftColumn", value = "Y")
    @Action(order = 26, type = ActionType.SET, field = "complexCharString", value = "& < > ' \" [] () ü ö ä Ä Ü Ö ß")
    @Action(order = 27, type = ActionType.SET, field = "demoScreenContentUri", value = "/admin/images/does_not_exist.jpg?param1=value1&param2=value2")
    @Action(order = 28, type = ActionType.SET, field = "contentPathPrefix", value = "https://ilscipio.com/images")
    @Action(order = 29, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemo_script1.groovy")
    @Action(order = 30, type = ActionType.PROPERTY_TO_FIELD, field = "webSocketEnabled", resource = "catalina", property = "webSocket")
    @Action(order = 31, type = ActionType.SET, field = "webSocketEnabled", fromField = "webSocketEnabled", valueType = "Boolean", defaultValue = "false")
    @IfAction(order = 32, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"webSocketEnabled"}), @Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/webtools/images/ExamplePushNotifications.js", global = true)}))
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"
                })}), widgets = @InlineWidgets(sections = {
                    @SectionNested(name = "Grid", widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/layout/layoutdemo.ftl"
                    )}))}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface WebtoolsLayoutDemo {}

    @Screen(name = "WebtoolsLayoutDemoOfbizWidgetActions", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgetActions_script1.groovy")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "WebtoolsLayoutDemoExtraPoorActions")
    public interface WebtoolsLayoutDemoOfbizWidgetActions {}

    @Screen(name = "WebtoolsLayoutDemoExtraPoorActions", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SET, field = "commonActionField1", value = "This value 1 was set in WebtoolsLayoutDemoExtraPoorActions screen actions included using the new include-actions screen widget directive. [SUCCESS]")
    @Action(type = ActionType.SET, field = "commonActionField2", value = "This value 2 was set in WebtoolsLayoutDemoExtraPoorActions screen actions included using the new include-actions screen widget directive, but should get overridden by another include. [ERROR]")
    @Section(actions = @Actions(value = {@Action(type = ActionType.SET, field = "nonCommonActionField4", value = "This value 4 was set in WebtoolsLayoutDemoOfbizWidgetActions outside its top section and should not be included by include-actions. [ERROR]")}))
    public interface WebtoolsLayoutDemoExtraPoorActions {}

    @Screen(name = "WebtoolsLayoutDemoOfbizWidgets", location = "component://webtools/widget/MiscScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "clearFieldTest1", value = "[This is a test of the clear-field directive]")
    @Action(order = 1, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script1.groovy")
    @Action(order = 2, type = ActionType.CLEAR_FIELD, field = "clearFieldTest1")
    @Action(order = 3, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script2.groovy")
    @Action(order = 4, type = ActionType.SET, field = "commonActionField1", value = "This value should get overridden... [ERROR]")
    @Action(order = 5, type = ActionType.SET, field = "commonActionField2", value = "This value should get overridden... [ERROR]")
    @Action(order = 6, type = ActionType.SET, field = "nonCommonActionField4", value = "This value should keep its original value from WebtoolsLayoutDemoOfbizWidgets. [SUCCESS]")
    @Action(order = 7, type = ActionType.SET, field = "commonActionField5", value = "This value should get overridden... [ERROR]")
    @Action(order = 8, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "WebtoolsLayoutDemoOfbizWidgetActions")
    @Action(order = 9, type = ActionType.INCLUDE_FORM_ACTIONS, name = "LayoutDemoActionsIncludeTest2", location = "component://webtools/widget/MiscForms.xml")
    @Action(order = 10, type = ActionType.SET, field = "miscMenusLocation", value = "component://webtools/widget/MiscMenus.xml")
    @Action(order = 11, type = ActionType.INCLUDE_MENU_ACTIONS, name = "WebtoolsMenuActionsDemo4", location = "${miscMenusLocation}")
    @Action(order = 12, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script3.groovy")
    @Action(order = 13, type = ActionType.SET, field = "dummy", value = "${groovy: import org.ofbiz.base.util.*; Debug.logInfo('WebSite records in system (cached): ' + UtilMisc.collectMapValuesForKey(from('WebSite').orderBy('webSiteId').cache(true).queryList(), 'webSiteId'), 'InlineDemoAssignScript.groovy'); '';}")
    @Action(order = 14, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script4.groovy")
    @Action(order = 15, type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/scipio/webtools/MiscSimpleMethods.xml#testBshCompat")
    @Action(order = 16, type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script5.groovy")
    @IfAction(order = 17, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ComponentEnabled.class, params = {"webtools"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script6.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script7.groovy")}))
    @IfAction(order = 18, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServiceDefined.class, params = {"superFakeComponent"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script8.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script9.groovy")}))
    @IfAction(order = 19, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = EntityDefined.class, params = {"WebSite"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script10.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script11.groovy")}))
    @IfAction(order = 20, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = EntityDefined.class, params = {"SuperFakeEntity"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script12.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script13.groovy")}))
    @IfAction(order = 21, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServiceDefined.class, params = {"createProductStore"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script14.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script15.groovy")}))
    @IfAction(order = 22, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServiceDefined.class, params = {"superFakeService"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script16.groovy")}), elseActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/WebtoolsLayoutDemoOfbizWidgets_script17.groovy")}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CatchActionsTest", position = 6)}, screenlets = {@Screenlet(title = "Widget language tests", screenlets = {@ScreenletNested(title = "include-actions directive", labels = {
                    @Label(text = "commonActionField1 value: ${commonActionField1}"
                ),
                @Label(text = "commonActionField2 value: ${commonActionField2}"
                ),
                @Label(text = "commonActionField3 value: ${commonActionField3}"
                ),
                @Label(text = "nonCommonActionField4 value: ${nonCommonActionField4}"
                ),
                @Label(text = "commonActionField5 value: ${commonActionField5}"
                ),
                @Label(text = "commonActionField6 value: ${commonActionField6}"
                ),
                @Label(text = "commonActionField7 value: ${commonActionField7}"
                ),
                @Label(text = "commonActionField8 value: ${commonActionField8}"
                ),
                @Label(text = "commonActionField9 value: ${commonActionField9}"
                )})}, position = 0), @Screenlet(title = "Screenlets", name = "my-widget-screenlet-parent", includeMenus = {@IncludeMenu(name = "WebtoolsInlineSectionMenuDemo", location = "component://webtools/widget/MiscMenus.xml")}, labels = {@Label(text = "This section/screenlet has a special legacy Ofbiz navigation menu, rendered as section-inline menu type by default.")}, screenlets = {@ScreenletNested(title = "Complex title style", id = "my-widget-screenlet-child", titleStyle = "container:mycontainerclass;h:mytitleclass"
                ), @ScreenletNested(title = "Complex title style 2 (override title elem)", titleStyle = "container:mycontainerclass;container:+mytitleclass"
                ), @ScreenletNested(title = "Complex title style 3 (no classes)", titleStyle = "container;span")}, position = 1), @Screenlet(title = "Heading labels", labels = {@Label(text = "Complex title style", style = "container:mycontainerclass;heading:myheadingclass"), @Label(text = "Complex title style 3", style = "heading:myheadingclass"), @Label(text = "Complex title style 4", style = "div:mycontainerclass;h"), @Label(text = "H1.", style = "h1"), @Label(text = "H2.", style = "h2"), @Label(text = "H3.", style = "h3"), @Label(text = "H4.", style = "h4"), @Label(text = "H5.", style = "h5"), @Label(text = "H6.", style = "h6"), @Label(text = "Heading+0", style = "heading"), @Label(text = "Heading+1", style = "heading+1"), @Label(text = "Heading+2", style = "heading+2"), @Label(text = "Heading+3", style = "heading+3"), @Label(text = "Heading+4 (replacing class)", style = "heading+4:myheadingclass"), @Label(text = "Heading+4 (adding class)", style = "heading+4:+myheadingclass")}, position = 2), @Screenlet(title = "Specific markup labels", labels = {@Label(text = "Generic markup", style = "generic"), @Label(text = "Default markup"), @Label(text = "Paragraph", style = "p"), @Label(text = "Span", style = "span"), @Label(text = "Div", style = "div"), @Label(text = "Generic markup (with extra class)", style = "myclass"), @Label(text = "Generic markup (with extra class)", style = "+myclass"), @Label(text = "Generic markup (with extra class)", style = "generic:myclass"), @Label(text = "Generic markup (with extra class)", style = "generic:+myclass"), @Label(text = "Paragraph (with extra class)", style = "p:myclass"), @Label(text = "Span (with extra class)", style = "span:+myclass"), @Label(text = "Div (with extra class)", style = "div:+myclass")}, position = 3), @Screenlet(title = "Common message labels", labels = {@Label(text = "Result message", style = "common-msg-result"), @Label(text = " ", style = "common-msg-result-norecord"), @Label(text = "General warning message", style = "common-msg-warning"), @Label(text = "General error message", style = "common-msg-error"), @Label(text = "Permission error message", style = "common-msg-error-perm"), @Label(text = "Security error message", style = "common-msg-error-security"), @Label(text = "Custom common message", style = "common-msg-custom"), @Label(text = "Error message (with extra class)", style = "common-msg-error:+myclass"), @Label(text = "Error message (with extra class that replaces default)", style = "common-msg-error:myclass"), @Label(text = "Regular info message", style = "common-msg-info"), @Label(text = "Important info message", style = "common-msg-info-important")}, position = 4), @Screenlet(title = "Widget menus", includeMenus = {@IncludeMenu(name = "LayoutDemoButton2", location = "component://webtools/widget/MiscMenus.xml", position = 1), @IncludeMenu(name = "LayoutDemoNestedButton", location = "component://webtools/widget/MiscMenus.xml", position = 2), @IncludeMenu(name = "LayoutDemoButtonDropdown", location = "component://webtools/widget/MiscMenus.xml", position = 3)}, labels = {@Label(text = "Nested button menu (markup test only)", style = "heading", position = 0)}, screenlets = {@ScreenletNested(title = "Scope sharing test", includeMenus = {
                    @IncludeMenu(name = "LayoutDemoTest2", location = "component://webtools/widget/MiscMenus.xml", position = 0
                ),
                @IncludeMenu(name = "LayoutDemoTest2", location = "component://webtools/widget/MiscMenus.xml", position = 2
                )}, labels = {
                    @Label(text = "demoTestVar1 value (share-scope test): \"${demoTestVar1}\" (should be \"\")", position = 1
                ),
                @Label(text = "demoTestVar1 value (share-scope test): \"${demoTestVar1}\" (should be \"demoTestValue1\")", position = 3
                )}, position = 4), @ScreenletNested(title = "Depth limit test", includeMenus = {
                    @IncludeMenu(name = "LayoutDemoTest3", location = "component://webtools/widget/MiscMenus.xml", position = 1
                ),
                @IncludeMenu(name = "LayoutDemoTest3", location = "component://webtools/widget/MiscMenus.xml", position = 3
                ),
                @IncludeMenu(name = "LayoutDemoTest3", location = "component://webtools/widget/MiscMenus.xml", position = 5
                ),
                @IncludeMenu(name = "LayoutDemoTest3", location = "component://webtools/widget/MiscMenus.xml", position = 7
                )}, labels = {
                    @Label(text = "Full (default/-1)", style = "heading", position = 0
                ),
                @Label(text = "max-depth 2", style = "heading", position = 2),
                @Label(text = "max-depth 1", style = "heading", position = 4),
                @Label(text = "sub-menus 'none' filter (same as max-depth 1)", style = "heading", position = 6
                )}, position = 5), @ScreenletNested(title = "Screen widget include-screens directive test", widgets = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "WebtoolsLayoutDemoOfbizWidgets-part1", location = "component://webtools/widget/MiscScreens.xml", shareScope = true
                ),
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "WebtoolsLayoutDemoOfbizWidgets-part2", location = "component://webtools/widget/MiscScreens.xml", shareScope = true
                )}, position = 6), @ScreenletNested(title = "New widget condition-to-field element", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "testField1", value = "someValue"
                    ),
                    @Action(type = ActionType.SET, field = "testField2", value = "someValue2"
                ),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes1", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes2", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "equals", "NOTsomeValue"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "indicatorRes1", valueType = "Indicator", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                }),
                @Condition(type = Compare.class, params = {"testField2", "equals", "someValue2"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "indicatorRes2", valueType = "Indicator", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "not-equals", "someValue"
                }),
                @Condition(type = Compare.class, params = {"testField2", "equals", "someValue2"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "stringRes1", valueType = "String", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "stringRes2", valueType = "PlainString", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                }),
                @Condition(type = Compare.class, params = {"testField2", "not-equals", "someValue2"
                })})),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "mapRes1", valueType = "Map", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifCompare = {
                        @IfCompare(field = "testField1", operator = "equals", value = "someValue"
                    )})})),
                    @Action(type = ActionType.CONDITION_TO_FIELD, field = "mapRes2", valueType = "Map", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                        @OrCondition(ifCompare = {
                            @IfCompare(field = "testField1", operator = "not-equals", value = "someValue"
                        )})})),
                        @Action(type = ActionType.CONDITION_TO_FIELD, field = "testMap.testBoolList[]", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                        })})),
                        @Action(type = ActionType.CONDITION_TO_FIELD, field = "testMap.testBoolList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"testField1", "equals", "NOTsomeValue"
                        })})),
                        @Action(type = ActionType.SET, field = "boolRes3", value = "this is non-empty, won't be overridden"
                    ),
                    @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes3", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                    })})),
                    @Action(type = ActionType.SET, field = "boolRes4"),
                    @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes4", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"testField1", "equals", "someValue"
                    })})),
                    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenuEmu.cond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                        @OrCondition(ifHasPermission = {
                            @IfHasPermission(permission = "CATALOG", action = "_ADMIN"),
                            @IfHasPermission(permission = "CATALOG", action = "_CREATE"),
                            @IfHasPermission(permission = "CATALOG", action = "_UPDATE"),
                            @IfHasPermission(permission = "CATALOG", action = "_VIEW")})}
                        )),
                        @Action(type = ActionType.SET, field = "testField5", value = "true", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes5", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"testField5", "not-equals", "false", "Boolean"
                    })})),
                    @Action(type = ActionType.SET, field = "testField5", valueType = "Boolean"
                ),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes6", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField5", "not-equals", "false", "Boolean"
                })})),
                @Action(type = ActionType.SET, field = "testField5", value = "false", valueType = "Boolean"
                ),
                @Action(type = ActionType.CONDITION_TO_FIELD, field = "boolRes7", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"testField5", "not-equals", "false", "Boolean"
                })}))}), widgets = @WidgetsLeaf(labels = {
                    @Label(text = "boolRes1: '${boolRes1}'"),
                    @Label(text = "boolRes2: '${boolRes2}'"),
                    @Label(text = "indicatorRes1: '${indicatorRes1}'"),
                    @Label(text = "indicatorRes2: '${indicatorRes2}'"),
                    @Label(text = "stringRes1: '${stringRes1}'"),
                    @Label(text = "stringRes2: '${stringRes2}'"),
                    @Label(text = "mapRes1: '${mapRes1}'"),
                    @Label(text = "mapRes2 (should be null/empty): '${mapRes2}'"),
                    @Label(text = "testMap.testBoolList: '${testMap.testBoolList}'"
                ),
                @Label(text = "boolRes3 (only-if-field='empty'): '${boolRes3}'"
                ),
                @Label(text = "boolRes4 (only-if-field='empty'): '${boolRes4}'"
                ),
                @Label(text = "commonSideBarMenuEmu.cond: '${commonSideBarMenuEmu.cond}'"
                ),
                @Label(text = "boolRes5 (should be true): '${boolRes5}'"),
                @Label(text = "boolRes6 (should be true): '${boolRes6}'"),
                @Label(text = "boolRes7 (should be false): '${boolRes7}'")}))
                }, position = 7), @ScreenletNested(title = "Screen inline FTL templates and inline groovy scripts", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script18.groovy"
                    ),
                    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script19.groovy"
                )}), widgets = @WidgetsLeaf(htmlTemplates = {
                    @HtmlTemplate(location = "", content = "<#assign myVar = \"This was assigned within inline freemarker template!\">\n                                        <@row>\n                                          <@cell>\n                                            <@alert type=\"info\">${myVar}</@alert>\n                                          </@cell>\n                                        </@row>"
                ),
                @HtmlTemplate(location = "", content = "<@row>\n                                          <@cell>\n                                            <@alert type=\"info\">Second inline template! Explicit type (default is ftl anyway).<br/>\n                                                myVarFromInlineGroovy1: ${myVarFromInlineGroovy1}</@alert>\n                                          </@cell>\n                                        </@row>"
                ),
                @HtmlTemplate(location = "", content = "ftl:\n                                        <@row>\n                                          <@cell>\n                                            <@alert type=\"info\">Third inline template, with colon-prefixed type, fully redundant.</@alert>\n                                          </@cell>\n                                        </@row>"
                )}))}, position = 8), @ScreenletNested(title = "New include directive actions (post-context-stack-push)", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "mySimpleVar1", value = "OutsideValue"
                    ),
                    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script20.groovy"
                )}), widgets = @WidgetsLeaf(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "WebtoolsLayoutDemoOfbizWidgets-part3", location = "component://webtools/widget/MiscScreens.xml", shareScope = true
                )}))}, position = 9), @ScreenletNested(title = "New conditional element tests (if, if-widget)", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(order = 0, type = ActionType.SET, field = "testField1", value = "true"
                    ),
                    @Action(order = 2, type = ActionType.SET, field = "testField1", value = "false"
                ),
                @Action(order = 4, type = ActionType.SET, field = "testField1", value = "other"
                ),
                @Action(order = 6, type = ActionType.SET, field = "testField1", value = "other"
                ),
                @Action(order = 8, type = ActionType.CONDITION_TO_FIELD, field = "existsCrazyWidget1", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"crazyWidget1", "${parameters.mainDecoratorLocation}", "screen"
                })})),
                @Action(order = 9, type = ActionType.CONDITION_TO_FIELD, field = "existsMainDecorator", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"main-decorator", "${parameters.mainDecoratorLocation}", "screen"
                })})),
                @Action(order = 10, type = ActionType.CONDITION_TO_FIELD, field = "existsFormWidgetProgramExport", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"ProgramExport", "component://webtools/widget/MiscForms.xml", "form"
                })})),
                @Action(order = 11, type = ActionType.CONDITION_TO_FIELD, field = "existsFormWidgetRidiculous", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"Ridiculous", "component://webtools/widget/MiscForms.xml", "form"
                })})),
                @Action(order = 12, type = ActionType.CONDITION_TO_FIELD, field = "existsMenuWidgetRidiculous", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"Ridiculous", "component://webtools/widget/Menus.xml", "menu"
                })})),
                @Action(order = 13, type = ActionType.CONDITION_TO_FIELD, field = "existsMenuWidgetWebtoolsAppBar", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = WidgetDefined.class, params = {"WebtoolsAppBar", "component://webtools/widget/Menus.xml", "menu"
                })}))}, ifs = {
                    @IfAction2(order = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = True.class, params = {"testField1"})}), then = @Actions2(value = {
                            @Action(type = ActionType.SET, field = "resField1", value = "if block"
                        )}), elseIf = {
                            @ElseIfBlock2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                @Condition(type = False.class, params = {"testField1"})}), then = @Actions2(value = {
                                    @Action(type = ActionType.SET, field = "resField1", value = "else-if block"
                                )}))}, elseActions = @Actions2(value = {
                                    @Action(type = ActionType.SET, field = "resField1", value = "else block"
                                )})),
                                @IfAction2(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = True.class, params = {"testField1"})}), then = @Actions2(value = {
                                        @Action(type = ActionType.SET, field = "resField2", value = "if block"
                                    )}), elseIf = {
                                        @ElseIfBlock2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                            @Condition(type = False.class, params = {"testField1"})}), then = @Actions2(value = {
                                                @Action(type = ActionType.SET, field = "resField2", value = "else-if block"
                                            )}))}, elseActions = @Actions2(value = {
                                                @Action(type = ActionType.SET, field = "resField2", value = "else block"
                                            )})),
                                            @IfAction2(order = 5, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                @Condition(type = True.class, params = {"${testField1 == 'other'}"
                                            })}), then = @Actions2(value = {
                                                @Action(type = ActionType.SET, field = "resField4", value = "if block"
                                            )}), elseIf = {
                                                @ElseIfBlock2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                    @Condition(type = False.class, params = {"testField1"})}), then = @Actions2(value = {
                                                        @Action(type = ActionType.SET, field = "resField4", value = "else-if block"
                                                    )}))}, elseActions = @Actions2(value = {
                                                        @Action(type = ActionType.SET, field = "resField4", value = "else block"
                                                    )})),
                                                    @IfAction2(order = 7, conditionExpr = "${testField1 == 'other'}", then = @Actions2(value = {
                                                        @Action(type = ActionType.SET, field = "resField5", value = "if block"
                                                    )}), elseIf = {
                                                        @ElseIfBlock2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                                            @Condition(type = False.class, params = {"testField1"})}), then = @Actions2(value = {
                                                                @Action(type = ActionType.SET, field = "resField5", value = "else-if block"
                                                            )}))}, elseActions = @Actions2(value = {
                                                                @Action(type = ActionType.SET, field = "resField5", value = "else block"
                                                            )}))}), widgets = @WidgetsLeaf(labels = {
                                                                @Label(text = "resField1: ${resField1}"),
                                                                @Label(text = "resField2: ${resField2}"),
                                                                @Label(text = "resField3: ${resField3}"),
                                                                @Label(text = "resField4: ${resField4}"),
                                                                @Label(text = "resField5: ${resField5}"),
                                                                @Label(text = "existsCrazyWidget1: ${existsCrazyWidget1}"),
                                                                @Label(text = "existsMainDecorator: ${existsMainDecorator}"),
                                                                @Label(text = "existsFormWidgetProgramExport: ${existsFormWidgetProgramExport}"
                                                            ),
                                                            @Label(text = "existsFormWidgetRidiculous: ${existsFormWidgetRidiculous}"
                                                        ),
                                                        @Label(text = "existsMenuWidgetRidiculous: ${existsMenuWidgetRidiculous}"
                                                    ),
                                                    @Label(text = "existsMenuWidgetWebtoolsAppBar: ${existsMenuWidgetWebtoolsAppBar}"
                                                )}))}, position = 10)}, position = 5)}))
    public interface WebtoolsLayoutDemoOfbizWidgets {}

    @Screen(name = "WebtoolsLayoutDemoOfbizWidgets-part2", location = "component://webtools/widget/MiscScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "Actions-only screens", actions = @Actions(value = {@Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DemoExtraActions1"), @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DemoExtraActions2"), @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DemoExtraActions3"), @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DemoExtraActions4")}), includeScreens = {@IncludeScreen(name = "DemoExtraActions3", location = "component://webtools/widget/MiscScreens.xml", position = 6)}, labels = {@Label(text = "The following tests the actions includes produced by include-screens directive.\n                                        In the next, the first two should print because they contained only actions, so they were implemented by include-screens with an include-screen-actions delegate/placeholder,\n                                        and this ensures the include-screen-actions we just ran work transitively and succeed.\n                                        The two after should come up blank because they contained widgets and condition, so include-screens implemented their delegates with include-screen, and\n                                        the include-screen-actions we just ran above is not able to follow those.\n                                        This highlights the importance of creating the right kind of screens, in order for reuse to work after.", style = "p", position = 0), @Label(text = "demoExtraActions1Msg: ${demoExtraActions1Msg}", position = 1), @Label(text = "demoExtraActions2Msg: ${demoExtraActions2Msg}", position = 2), @Label(text = "demoExtraActions3Msg (should be empty, contained widgets): ${demoExtraActions3Msg}", position = 3), @Label(text = "demoExtraActions4Msg (should be empty, contained condition): ${demoExtraActions4Msg}", position = 4), @Label(text = "Now print the third one within its widget, and it should only show this way:", position = 5)})}))
    public interface WebtoolsLayoutDemoOfbizWidgets_part2 {}

    @Screen(name = "WebtoolsLayoutDemoOfbizWidgets-part3", location = "component://webtools/widget/MiscScreens.xml")
    @Section(actions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script1.groovy")}), widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "Hello from before include directive. mySimpleVar1: ${groovy: context.mySimpleVar1 ?: 'missing'}. mySimpleVar2: ${groovy: context.mySimpleVar2 ?: 'missing'}."), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DemoLabelWidgetWithVars")}, sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script2.groovy")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "Hello from after include directive (stack popped). mySimpleVar1: ${groovy: context.mySimpleVar1 ?: 'missing'}. mySimpleVar2: ${groovy: context.mySimpleVar2 ?: 'missing'}.")}))}))
    public interface WebtoolsLayoutDemoOfbizWidgets_part3 {}

    @Screen(name = "WebtoolsLayoutDemoOfbizWidgets-part1", location = "component://webtools/widget/MiscScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "Regular widget screens", includeScreens = {@IncludeScreen(name = "DemoExtraWidget1", location = "component://webtools/widget/MiscScreens.xml", position = 1), @IncludeScreen(name = "DemoExtraWidget2", location = "component://webtools/widget/MiscScreens.xml", position = 2)}, labels = {@Label(text = "The following simply includes two local screens that were included from another file with include-screens directive.\n                                Because the target includes were regular screens containing widgets, they were created in our local file by include-screens using simple\n                                delegating directives containing the legacy include-screen directive. No surprises here.", style = "p", position = 0)})}))
    public interface WebtoolsLayoutDemoOfbizWidgets_part1 {}

    @Screen(name = "TargetedRenderingTest", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "Targeted Rendering Test")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "LayoutDemo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: (context.debugMode ? 'LayoutDemoDebug' : 'LayoutDemo')}")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(name = "TR-Widget-Section-1", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(id = "tr-widget-container-1", htmlTemplates = {
                        @HtmlTemplate(location = "component://webtools/webapp/webtools/layout/targetedrenderingtest.ftl"
                    )}),
                    @Container(id = "tr-widget-container-2", decorator = @DecoratorScreenNested(name = "TargetedRenderingTestSubDecorator", location = "component://webtools/widget/MiscScreens.xml", sections = {
                        @DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(containers = {
                            @Container4(id = "tr-widget-container-3", labels = {
                                @Label(text = "Hello from sub-decorator body")})}))}))}), failWidgets = @InlineWidgets(value = {
                                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsViewPermissionError}", style = "common-msg-error-perm"
                                )}))})
        }
    )
    public interface TargetedRenderingTest {}

    @Screen(name = "TargetedRenderingTestDeepWidget1", location = "component://webtools/widget/MiscScreens.xml")
    @Section(name = "TR-Widget-Deep-Section-2", widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "Hello from widget within ftl file")}))
    public interface TargetedRenderingTestDeepWidget1 {}

    @Screen(name = "TargetedRenderingTestSubDecorator", location = "component://webtools/widget/MiscScreens.xml")
    @Section(name = "TR-SubDec-Section-1", widgets = @Widgets(screenlets = {@Screenlet(name = "tr-subdec-screenlet-1", htmlTemplates = {@HtmlTemplate(location = "", content = "<@virtualSection name=\"tr-subdec-ftl-virtual-1\">\n                                <div>\n                                    <p>We are inside sub decorator section 1</p>\n                                </div>\n                            </@virtualSection>")})}))
    @Section(name = "TR-SubDec-Section-2", widgets = @Widgets(screenlets = {@Screenlet(name = "tr-subdec-screenlet-2", htmlTemplates = {@HtmlTemplate(location = "", content = "<@virtualSection name=\"tr-subdec-ftl-virtual-2\">\n                                <div>\n                                    <p>We are inside sub decorator section 2</p>\n                                    <@render type=\"section\" name=\"body\"/>\n                                    <p>We are inside sub decorator section 2</p>\n                                </div>\n                            </@virtualSection>")})}))
    public interface TargetedRenderingTestSubDecorator {}

    @Screen(name = "TemplateTest", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CMSUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsTemplateTest")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Development")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TemplateTest")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "tmplTestEnabled", resource = "webtools", property = "dev.script.tools.enabled")
    @Action(type = ActionType.SET, field = "tmplTestEnabled", fromField = "tmplTestEnabled", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "hasTmplTestPerm", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"}), @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"}), @Condition(type = True.class, params = {"tmplTestEnabled"})}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/TemplateTest_script1.groovy")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"
                }),
                @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"
            })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, content = "<@script>\n                                    $(document).ready(function() {\n                                        CodeMirror.fromTextArea($('textarea#scriptBody')[0], {\n                                            lineNumbers: true,\n                                            matchBrackets: true,\n                                            mode: \"groovy\",\n                                            indentUnit: 4,\n                                            indentWithTabs: ${indentWithTabs?string},\n                                            foldGutter: true,\n                                            gutters: [\"CodeMirror-linenumbers\", \"CodeMirror-foldgutter\"],\n                                            extraKeys: {\"Ctrl-Space\": \"autocomplete\"}\n                                            }).on('change', function(cMirror){\n                                                $('textarea#scriptBody')[0].value = cMirror.getValue();\n                                            }\n                                        );\n                                        CodeMirror.fromTextArea($('textarea#templateBody')[0], {\n                                            lineNumbers: true,\n                                            matchBrackets: true,\n                                            mode: \"freemarker\",\n                                            indentUnit: 4,\n                                            indentWithTabs: ${indentWithTabs?string},\n                                            foldGutter: true,\n                                            gutters: [\"CodeMirror-linenumbers\", \"CodeMirror-foldgutter\"],\n                                            extraKeys: {\"Ctrl-Space\": \"autocomplete\"}\n                                            }).on('change', function(cMirror){\n                                                $('textarea#templateBody')[0].value = cMirror.getValue();\n                                            }\n                                        );\n                                    });\n                                  </@script>\n                                  <@section class=\"+cms-edit-elem cms-edit-template\">\n                                    <@form method=\"post\" id=\"editorForm\" action=makePageUrl(\"TemplateTest\")>\n                                       <@section class=\"+editorContent\">\n                                          <@fields type=\"default-compact\">\n                                            <@field label=(rawLabel('CmsScriptBody')+\" (Groovy)\") type=\"textarea\" class=\"+editor\" \n                                                name=\"scriptBody\" id=\"scriptBody\" value=(scriptBody!\"\") rows=30/>\n                                            <@field label=(rawLabel('CmsTemplateBody')+\" (Freemarker)\") type=\"textarea\" class=\"+editor\" \n                                                name=\"templateBody\" id=\"templateBody\" value=(templateBody!\"\") rows=30/>\n                                          </@fields>\n                                          <style type=\"text/css\">\n                                            .CodeMirror-hints {font-size:1em;}\n                                          </style>\n                                       </@section>\n                                       <@field type=\"submit\"/>\n                                    </@form>\n                                  </@section>\n                                   \n                                  <#-- FREEMARKER OUTPUT -->\n                                  <#if execTemplate>\n                                    <@section title=uiLabelMap.CmsOutput>\n                                      <#assign tmplBodyCode = raw(templateBody!\"\")?interpret>\n                                      <#assign tmplBodyOut><@tmplBodyCode/></#assign>\n                                      ${tmplBodyOut}\n                                    </@section>\n                                    <@section title=uiLabelMap.CmsRawOutput>\n                                      ${tmplBodyOut?html}\n                                    </@section>\n                                  </#if>", position = 1
            )}, sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = True.class, params = {"tmplTestEnabled"})}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonFunctionIsDisabled}", style = "common-msg-warning"
                    )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface TemplateTest {}

    @Screen(name = "validateSystemLocations", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/validateSystemLocations_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<form method=\"post\" action=\"<@pageUrl uri=raw(vslTargetUri)/>\">\n                        <@field type=\"textarea\" name=\"vslPaths\" label=\"Paths\" value=(parameters.vslPaths!)/>\n                        <@field type=\"textarea\" name=\"vslClassNames\" label=\"Class names\" value=(parameters.vslClassNames!)/>\n                        <@field type=\"submit\"/>\n                    </form>\n                    <#if vslResults??>\n                      <@section title=\"Results:\">\n                      <#if vslResults.pathResults??>\n                        <@section title=\"Valid paths:\">\n                          <ul>\n                            <#list vslResults.pathResults as vslResult>\n                              <#if vslResult.valid>\n                                <li>${vslResult.value}</li>\n                              </#if>\n                            </#list>\n                          </ul>\n                        </@section>\n                        <@section title=\"Invalid paths:\">\n                          <ul>\n                            <#list vslResults.pathResults as vslResult>\n                              <#if !vslResult.valid>\n                                <li>${vslResult.value}: ${vslResult.errMsg!}</li>\n                              </#if>\n                            </#list>\n                          </ul>\n                        </@section>\n                      </#if>\n                      <#if vslResults.classNameResults??>\n                        <@section title=\"Valid class names:\">\n                          <ul>\n                            <#list vslResults.classNameResults as vslResult>\n                              <#if vslResult.valid>\n                                <li>${vslResult.value}</li>\n                              </#if>\n                            </#list>\n                          </ul>\n                        </@section>\n                        <@section title=\"Invalid class names:\">\n                          <ul>\n                            <#list vslResults.classNameResults as vslResult>\n                              <#if !vslResult.valid>\n                                <li>${vslResult.value}: ${vslResult.errMsg!}</li>\n                              </#if>\n                            </#list>\n                          </ul>\n                        </@section>\n                      </#if>\n                      </@section>\n                    </#if>")}))
    public interface validateSystemLocations {}

    @Screen(name = "CatchActionsTest", location = "component://webtools/widget/MiscScreens.xml", catchActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script1.groovy")}), finallyActions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/_script2.groovy"), @Action(type = ActionType.CLOSE_OBJECT, field = "testIt")}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/CatchActionsTest_script3.groovy")
    @Action(type = ActionType.THROW_EXCEPTION, field = "testException")
    public interface CatchActionsTest {}

    @Screen(name = "hotwireTest", location = "component://webtools/widget/MiscScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<@section title=\"Static Comments\">\n                        <@render resource=\"component://webtools/widget/MiscScreens.xml#hotwireStaticComments\"/>\n                    </@section>\n                    <@section title=\"Websocket Comments\">\n                        <@render resource=\"component://webtools/widget/MiscScreens.xml#hotwireStreamCommentsSplash\"/>\n                    </@section>")}))
    public interface hotwireTest {}

    @Screen(name = "hotwireActions", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SET, field = "hotwire.enabled", fromField = "hotwire.enabled", valueType = "Boolean", defaultValue = "true", global = true)
    public interface hotwireActions {}

    @Screen(name = "hotwireStaticComments", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/hotwireStaticComments_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<turbo-frame id=\"hotwire-static-comments\">\n                        <div>\n                            <#if hotwireComments?has_content>\n                                <ol>\n                                    <#list hotwireComments as comment>\n                                        <li id=\"${'hotwire-comment-' + comment?index}\">${comment}</li>\n                                    </#list>\n                                </ol>\n                            <#else>\n                                (no comments)\n                            </#if>\n                        </div>\n\n                        <form action=\"<@pageUrl uri='hotwireStaticCommentsAdd'/>\" method=\"post\">\n                            <input type=\"text\" name=\"comment\" id=\"hotwire-comment-input\">\n                            <button type=\"submit\">Send</button>\n                        </form>\n                    </turbo-frame>\n\n                    <form action=\"<@pageUrl uri='hotwireStaticCommentsRemove'/>\" method=\"post\" data-turbo-frame=\"hotwire-static-comments\">\n                        <button type=\"submit\">Remove</button>\n                    </form>")}))
    public interface hotwireStaticComments {}

    @Screen(name = "hotwireStreamCommentsSplash", location = "component://webtools/widget/MiscScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<ul id=\"hotwire-stream-comments\"><li>First comment</li></ul>\n                    <p>Last time: <span id=\"hotwire-stream-date\">${nowTimestamp}</span></p>\n\n                    <turbo-stream-connection src=\"<@appUrl uri='/ws-turbo/comment-demo/subscribe'/>\"></turbo-stream-connection>\n\n                    <p><em>Note: This form sends through websockets for demo purposes only, after which the comments\n                        above are updated through a separate websocket push.</em></p>\n\n                    <form action=\"<@pageUrl uri='hotwireStreamCommentsAdd'/>\" method=\"post\" data-turbo-frame=\"hotwire-stream-dummy-frame\">\n                        <input type=\"text\" name=\"comment\">\n                        <button type=\"submit\">Send</button>\n                    </form>\n\n                    <turbo-frame id=\"hotwire-stream-dummy-frame\"></turbo-frame>")}))
    public interface hotwireStreamCommentsSplash {}

    @Screen(name = "hotwireStreamComments", location = "component://webtools/widget/MiscScreens.xml")
    @Action(type = ActionType.SET, field = "comment", value = "test")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<turbo-stream action=\"append\" target=\"hotwire-stream-comments\">\n                        <template>\n                            <li>${nowTimestamp}: ${comment!}</li>\n                        </template>\n                    </turbo-stream>\n                    <turbo-stream action=\"replace\" target=\"hotwire-stream-date\">\n                        <template>\n                            <span id=\"hotwire-stream-date\">${nowTimestamp}</span>\n                        </template>\n                    </turbo-stream>")}))
    public interface hotwireStreamComments {}

}
