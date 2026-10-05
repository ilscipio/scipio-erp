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
public class CacheScreens {

    @Screen(name = "FindUtilCache", location = "component://webtools/widget/CacheScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Server")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "cache")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindUtilCache")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/cache/FindUtilCache.groovy")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"UTIL_CACHE", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "FindCacheTabBar", location = "component://webtools/widget/Menus.xml"
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.WebtoolsMemory}", includeForms = {
                        @IncludeForm(name = "MemoryInfo", location = "component://webtools/widget/CacheForms.xml"
                    )}),
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "FindCache", location = "component://webtools/widget/CacheForms.xml"
                    ),
                    @IncludeForm(name = "ListCache", location = "component://webtools/widget/CacheForms.xml"
                )})}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface FindUtilCache {}

    @Screen(name = "FindUtilCacheElements", location = "component://webtools/widget/CacheScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Server")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "cache")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindUtilCacheElements")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/cache/FindUtilCacheElements.groovy")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"UTIL_CACHE", "_VIEW"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeMenus = {
                        @IncludeMenu(name = "CacheElements", location = "component://webtools/widget/Menus.xml", position = 1
                    )}, labels = {
                        @Label(text = "${uiLabelMap.WebtoolsCacheName}: ${cacheName} (${now}), ${uiLabelMap.WebtoolsSizeTotal}: ${totalSize} ${uiLabelMap.WebtoolsBytes}", position = 0
                    )}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"cache"})}), widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCacheElements", location = "component://webtools/widget/CacheForms.xml"
                            )}), failWidgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.LABEL, text = "${groovy:org.ofbiz.base.util.UtilProperties.getMessage('WebtoolsErrorUiLabels', 'utilCache.cacheNotFound', [name:context.cacheName], context.locale)}", style = "common-msg-error"
                            )}), position = 2)})}), failWidgets = @InlineWidgets(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                            )}))})
        }
    )
    public interface FindUtilCacheElements {}

    @Screen(name = "EditUtilCache", location = "component://webtools/widget/CacheScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Server")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "cache")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditUtilCache")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/cache/EditUtilCache.groovy")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"UTIL_CACHE", "_EDIT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "EditCache", location = "component://webtools/widget/Menus.xml"
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"cache"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "EditCache", location = "component://webtools/widget/CacheForms.xml"
                        )}), failWidgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${groovy:org.ofbiz.base.util.UtilProperties.getMessage('WebtoolsErrorUiLabels', 'utilCache.cacheNotFound', [name:context.cacheName], context.locale)}", style = "common-msg-error"
                        )}))}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface EditUtilCache {}

    @Screen(name = "EditPrewarmCacheUrls", location = "component://webtools/widget/CacheScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Server")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "prewarmcache")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsPrewarmCacheUrls")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/cache/EditPrewarmCacheUrls.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/EditPrewarmCacheUrls_script1.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/lib/codemirror.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/addon/fold/foldgutter.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/codemirror/addon/hint/show-hint.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/lib/codemirror.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/codemirror/addon/display/placeholder.js", global = true)
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
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"UTIL_CACHE", "_EDIT"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/cache/prewarmCacheUrls.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface EditPrewarmCacheUrls {}

}
