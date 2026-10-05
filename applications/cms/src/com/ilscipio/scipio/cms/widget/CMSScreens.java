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
public class CMSScreens {

    @Screen(name = "main", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "CmsContentTree", location = "component://cms/widget/CommonScreens.xml"
                    )})})})
        }
    )
    public interface main {}

    @Screen(name = "CommonScriptAssocActions", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/CommonScriptAssocActions_script1.groovy")
    public interface CommonScriptAssocActions {}

    @Screen(name = "pages", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Pages")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "listPages")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetPageList.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/pages/listPages.ftl"
            )})
        }
    )
    public interface pages {}

    @Screen(name = "contentEditorActions", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/trumbowyg/dist/trumbowyg.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/trumbowyg/dist/plugins/table/trumbowyg.table.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/trumbowyg.scipio_common.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_media/trumbowyg.scipio_media.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_links/trumbowyg.scipio_links.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_assets/trumbowyg.scipio_assets.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    public interface contentEditorActions {}

    @Screen(name = "editPage", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Pages")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editPage")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "contentEditorActions")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetPage.groovy")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonScriptAssocActions")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/pages/editPage.ftl"
            )})
        }
    )
    public interface editPage {}

    @Screen(name = "pageVersionList", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CMSUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetPage.groovy")
    @Section(widgets = @Widgets(containers = {@Container(htmlTemplates = {@HtmlTemplate(location = "component://cms/webapp/cms/pages/pageVersionList.ftl")})}))
    public interface pageVersionList {}

    @Screen(name = "CommonEditRenderTemplateActions", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonScriptAssocActions")
    public interface CommonEditRenderTemplateActions {}

    @Screen(name = "templates", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "templates")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CmsPageTemplate", list = "templateList", orderBy = {"templateName"})
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/templates/listTemplates.ftl"
            )})
        }
    )
    public interface templates {}

    @Screen(name = "editTemplate", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editTemplate")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonEditRenderTemplateActions")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetTemplate.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/templates/editTemplate.ftl"
            )})
        }
    )
    public interface editTemplate {}

    @Screen(name = "assets", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "assets")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CmsAssetTemplate", list = "assetList", orderBy = {"templateName"})
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/assets/listAssets.ftl"
            )})
        }
    )
    public interface assets {}

    @Screen(name = "editAsset", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editAsset")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "contentEditorActions")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonEditRenderTemplateActions")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetAsset.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/assets/editAsset.ftl"
            )})
        }
    )
    public interface editAsset {}

    @Screen(name = "contentAssets", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "envAssetType", value = "CONTENT")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "ContentAsset")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "contentAssets")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CmsAssetTemplate", list = "assetList", conditions = {@ConditionExpr(fieldName = "assetType", fromField = "envAssetType")}, orderBy = {"templateName"})
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/assets/listAssets.ftl"
            )})
        }
    )
    public interface contentAssets {}

    @Screen(name = "editContentAsset", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "envAssetType", value = "CONTENT")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "ContentAsset")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editContentAsset")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "contentEditorActions")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonEditRenderTemplateActions")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetAsset.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/assets/editAsset.ftl"
            )})
        }
    )
    public interface editContentAsset {}

    @Screen(name = "scripts", location = "component://cms/widget/CMSScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(order = 1, type = ActionType.SET, field = "activeSubMenuItem", value = "scripts")
    @Action(order = 2, type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(order = 3, type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(order = 4, type = ActionType.SET, field = "showStandaloneOnly", fromField = "parameters.showStandaloneOnly", valueType = "String", defaultValue = "N")
    @IfAction(order = 5, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"showStandaloneOnly", "equals", "Y"})}), then = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "CmsScriptTemplate", list = "scriptList", conditions = {@ConditionExpr(fieldName = "standalone", value = "Y"), @ConditionExpr(fieldName = "standalone")}, orderBy = {"templateName"})}), elseActions = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "CmsScriptTemplate", list = "scriptList", orderBy = {"templateName"})}))
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/scripts/listScripts.ftl"
            )})
        }
    )
    public interface scripts {}

    @Screen(name = "editScript", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Templates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editScript")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CodeEditorCommonIncludes", location = "component://cms/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetScript.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/scripts/editScript.ftl"
            )})
        }
    )
    public interface editScript {}

    @Screen(name = "CommonMediaActions", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/CommonMediaActions_script1.groovy")
    public interface CommonMediaActions {}

    @Screen(name = "media", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Media")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "media")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonMediaActions")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetMediaList.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/media/listMedia.ftl"
            )})
        }
    )
    public interface media {}

    @Screen(name = "editMedia", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Media")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "media")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonMediaActions")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsGetMedia.groovy")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/media/editMedia.ftl"
            )})
        }
    )
    public interface editMedia {}

    @Screen(name = "customImageSizePresets", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Media")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "customImageSizePresets")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "50")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ImageSizePreset", list = "customImageSizePresets")
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/media/customImageSizePreset.ftl"
            )})
        }
    )
    public interface customImageSizePresets {}

    @Screen(name = "libMimeTypeExport", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/libMimeTypeExport_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#if mimeTypes?has_content>\n  <#list mimeTypes as mimeType>\n    <MimeType mimeTypeId=\"${raw(mimeType.mimeTypeId)}\" description=\"${raw(mimeType.description)}\"/>\n  </#list>\n</#if>", platform = "xml")}))
    public interface libMimeTypeExport {}

    @Screen(name = "CmsDataExport", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsDataExport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "dataExport")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/importexport/CmsDataExport.groovy")
    @Action(type = ActionType.SET, field = "useEntityMaintCheck", value = "false", valueType = "Boolean", global = true)
    @DecoratorScreen(
        name = "CommonCmsImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://cms/webapp/cms/importexport/CmsDataExport.ftl"
                )})})
        }
    )
    public interface CmsDataExport {}

    @Screen(name = "menus", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "Menus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "menus")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_STYLESHEET[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsMenuTree.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/editor/CmsContentTree.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/trumbowyg/dist/trumbowyg.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/bower_components/trumbowyg/dist/plugins/table/trumbowyg.table.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/trumbowyg.scipio_common.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_media/trumbowyg.scipio_media.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_links/trumbowyg.scipio_links.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.VT_FTPR_JAVASCRIPT[]", value = "/base-theme/js/cms/trumbowyg/scipio_assets/trumbowyg.scipio_assets.js", global = true)
    @DecoratorScreen(
        name = "CommonCMSAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/menus/menuTree.ftl"
            )})
        }
    )
    public interface menus {}

    @Screen(name = "CmsDataImport", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsDataImport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "dataImport")
    @Action(type = ActionType.SET, field = "messages", fromField = "parameters.messages")
    @Action(type = ActionType.SCRIPT, location = "component://cms/script/com/ilscipio/scipio/cms/importexport/CmsDataImport.groovy")
    @Action(type = ActionType.SET, field = "useEntityMaintCheck", value = "false", valueType = "Boolean", global = true)
    @DecoratorScreen(
        name = "CommonCmsImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://cms/webapp/cms/importexport/CmsDataImport.ftl"
                )})})
        }
    )
    public interface CmsDataImport {}

    @Screen(name = "redirects", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonRedirects")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "redirects")
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/redirects_script1.groovy")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/redirect/redirects.ftl"
            )})
        }
    )
    public interface redirects {}

    @Screen(name = "robots", location = "component://cms/widget/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonRobots")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "robots")
    @Action(type = ActionType.SCRIPT, location = "component://cms/webapp/cms/WEB-INF/actions/generated/robots_script1.groovy")
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
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://cms/webapp/cms/robots/robots.ftl"
            )})
        }
    )
    public interface robots {}

}
