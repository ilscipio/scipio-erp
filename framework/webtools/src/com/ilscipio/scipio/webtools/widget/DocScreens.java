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
public class DocScreens {

    @Screen(name = "DeveloperDocIndex", location = "component://webtools/widget/DocScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsDeveloperDocumentation")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "DeveloperDoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "DeveloperDocIndex")
    @Action(type = ActionType.SET, field = "tmplApiBaseWebappUri", value = "/docs/templating/ftl/lib")
    @Action(type = ActionType.SET, field = "tmplApiMasterWebappUri", value = "${tmplApiBaseWebappUri}/standard/htmlTemplate")
    @DecoratorScreen(
        name = "CommonDocumentationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, content = "<#assign scpErpRelDesc = getPropertyValue('scipiometainfo', 'scipio.release.desc')!\"Scipio ERP\">\n                                <p><em><#-- Version: --><a href=\"https://www.scipioerp.com/community/developer/\" target=\"_blank\">${scpErpRelDesc}\n                                    ${getPropertyValue('scipiometainfo', 'scipio.release.version')!\"v?\"} \n                                    (${getPropertyValue('scipiometainfo', 'scipio.release.branch')!(\"branch ?\")})</a>, \n                                    <a href=\"https://freemarker.apache.org/docs/\" target=\"_blank\">Freemarker ${.version!}</a></em></p>\n                                    \n                                <#-- TODO: Localize? -->\n                                <p>This is the local instance hosted version of ${scpErpRelDesc} developer documentation and resources.\n                                  It focuses on providing developer information most closely matching your current version in use,\n                                  and when possible on the portions of documentation most benefitting from interaction and dynamic\n                                  rendering, or yet those requiring manual examination of source files.</p>\n                                  \n                                <p>NOTE: For a more comprehensive introduction and documentation, including architectural overview, please visit the \n                                  <a href=\"http://www.scipioerp.com/community/developer/\" target=\"_blank\">Scipio ERP website \n                                    at http://www.scipioerp.com/community/developer/</a></p>\n                                \n                              <@section title=getLabel('ContentContents', 'ContentUiLabels')>\n                                <ol>\n                                  <li>\n                                    <@heading><a href=\"<@appUrl uri=tmplApiMasterWebappUri escapeAs='html'/>\">\n                                      ${getLabel('WebtoolsTemplateApiDocsTitle')}</a></@heading> - current, automatically generated from your local instance source files.\n                                    <br/><small>Alternatively, the Scipio website provides a periodically updated version at \n                                        <a href=\"http://www.scipioerp.com/community/developer/freemarker-macros/\" target=\"_blank\">http://www.scipioerp.com/community/developer/freemarker-macros/</a>.</small>  \n                                    <br/><small>The Toolkit Freemarker source files - fully documented - can be consulted directly under the \n                                        <code>/framework/common/webcommon/includes/scipio/lib/</code> folder from the root of your local copy.</small>\n                                  </li>\n                                  \n                                  <li>\n                                    <#-- TODO: web-presentable version of the *.xsd files... (XSLT for *.xsd -> *.html maybe?) -->\n                                    <@heading>Screen and Menu Widget Reference</@heading> - is provided as annotations in <code>*.xsd</code> XML Schema definition files \n                                    under <code>/framework/widget/dtd/</code>, primarily: <code>widget-screen.xsd</code>, <code>widget-common.xsd</code> and\n                                    <code>widget-menu.xsd</code>. These are greatly enhanced and re-documented for Scipio.\n                                    <br/><small>To use these and other widget XML schema definition files (*.xsd) in your development environment for autocompletion and validation,\n                                        import the file named \".xmlcatalog.xml\" from the Scipio project root as XML Catalog file/project type using import dialog.\n                                        Note this also includes the definition files for all other Scipio/ofbiz XML resources such as entity models, service definitions, etc.</small>\n                                  </li>\n                                </ol>\n                              </@section>"
            )})
        }
    )
    public interface DeveloperDocIndex {}

    @Screen(name = "TemplateApiDocPage", location = "component://webtools/widget/DocScreens.xml")
    @Action(type = ActionType.SET, field = "curDocPurpose", fromField = "parameters.docPurpose", defaultValue = "${groovy: request.getSession().getAttribute('docPurposeSaved')}")
    @Action(type = ActionType.SET, field = "dummy", value = "${groovy: if (context.curDocPurpose) request.getSession().setAttribute('docPurposeSaved', curDocPurpose); return '';}")
    @Action(type = ActionType.SET, field = "fdtwArgs", valueType = "NewMap")
    @Action(type = ActionType.SET, field = "fdtwArgs.doCompile", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "fdtwArgs.docPurpose", fromField = "curDocPurpose")
    @Action(type = ActionType.SET, field = "fdtwArgs.targetLibPath", value = "${groovy: request.getAttribute('scipioTmplApiTargetLibPath')}")
    @Action(type = ActionType.SET, field = "fdtwArgs.targetLibName")
    @Action(type = ActionType.SET, field = "fdtwArgs.reloadDataModel", fromField = "parameters.reloadDataModel", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/includes/scipio/doc/WEB-INF/actions/ftlDocTemplateAdmin.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${docLibTitleMap[targetLibName]}")
    @Action(type = ActionType.SET, field = "subtitle", value = "${docContext.pageTitle}")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "TemplateApiDoc")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "${targetLibShortName}")
    @Action(type = ActionType.SET, field = "basePageIntraWebappUri", value = "/docs/templating/ftl/lib")
    @Action(type = ActionType.SET, field = "basePageInterWebappUri", value = "${groovy: request.getContextPath() + basePageIntraWebappUri}")
    @Action(type = ActionType.SET, field = "currPageIntraWebappUri", value = "${basePageIntraWebappUri}/${targetLibPath}")
    @Action(type = ActionType.SET, field = "commonWebtoolsAppBasePermCond", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonDocumentationDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "fdtwArgs", valueType = "NewMap"
                ),
                @Action(type = ActionType.SET, field = "fdtwArgs.doPrepFtlCtx", value = "true", valueType = "Boolean"
            ),
            @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/includes/scipio/doc/WEB-INF/actions/ftlDocTemplateAdmin.groovy"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/scipio/doc/ftlDocTemplateAdmin.ftl"
            )}))})
        }
    )
    public interface TemplateApiDocPage {}

}
