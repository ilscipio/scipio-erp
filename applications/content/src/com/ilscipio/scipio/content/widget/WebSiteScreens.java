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
public class WebSiteScreens {

    @Screen(name = "FindWebSite", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindWebSite")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindWebSite")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListWebSites", location = "component://content/widget/website/WebSiteForms.xml"
                )})})
        }
    )
    public interface FindWebSite {}

    @Screen(name = "EditWebSite", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSite")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditWebSite")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "isCreateWebSite", value = "${groovy: !(context.webSite || (parameters.webSiteId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateWebSite ? 'ContentNewWebSite' : 'ContentWebSite'}")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditWebSite", location = "component://content/widget/website/WebSiteForms.xml"
                )})})
        }
    )
    public interface EditWebSite {}

    @Screen(name = "WebSiteContent", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSiteContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListWebSiteContent")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.ENTITY_AND, entityName = "WebSiteContent", list = "webSiteContent", fieldMaps = {@FieldMap(fieldName = "webSiteId", fromField = "webSiteId")})
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ContentWebSiteContent")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleWebSiteContent}", includeForms = {
                    @IncludeForm(name = "ListWebSiteContent", location = "component://content/widget/website/WebSiteForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleCreateWebSiteContent}", includeForms = {
                    @IncludeForm(name = "CreateWebSiteContent", location = "component://content/widget/website/WebSiteForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAutoCreateContentPublishPoints}", includeForms = {
                    @IncludeForm(name = "AutoCreateWebsiteContent", location = "component://content/widget/website/WebSiteForms.xml"
                )})})
        }
    )
    public interface WebSiteContent {}

    @Screen(name = "EditWebSiteParties", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSiteParties")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditWebSiteParties")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ContentWebSiteParties")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/website/EditWebSiteParties.groovy")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateWebSiteRole", location = "component://content/widget/website/WebSiteForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleCreateWebSiteParties}", name = "AddWebSiteRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "CreateWebSiteRole", location = "component://content/widget/website/WebSiteForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditWebSiteParties {}

    @Screen(name = "WebSiteCMS", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSiteCMS")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WebSiteCMS")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWebSiteCMS")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/website/WebSitePublishPoint.groovy")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"contentRoot"})}), widgets = @InlineWidgets(containers = {
                        @Container(id = "cmsnav", style = "left", includeScreens = {
                            @IncludeScreen(name = "WebSiteCMSNav", location = "component://content/widget/WebSiteScreens.xml"
                        )}),
                        @Container(id = "cmsmain", style = "leftonly", includeScreens = {
                            @IncludeScreen(name = "WebSiteCMSContent", location = "component://content/widget/WebSiteScreens.xml"
                        )})}), failWidgets = @InlineWidgets(containers = {
                            @Container(id = "norender", style = "tableheadtext", labels = {
                                @Label(text = "${uiLabelMap.ContentCMSNotExist}")})}))})
        }
    )
    public interface WebSiteCMS {}

    @Screen(name = "WebSiteCMSNav", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/website/WebSitePublishPoint.groovy")
    @Action(type = ActionType.SET, field = "language", fromField = "userLogin.lastLocale", defaultValue = "en")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleWebSiteCMSNav}", htmlTemplates = {@HtmlTemplate(location = "component://content/webapp/content/website/WebSiteCMSNav.ftl")})}))
    public interface WebSiteCMSNav {}

    @Screen(name = "WebSiteCMSContent", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "contentAssocTypeId", fromField = "parameters.contentAssocTypeId")
    @Action(type = ActionType.SET, field = "dataResourceTypeId", fromField = "parameters.dataResourceTypeId")
    @Action(type = ActionType.SET, field = "contentIdFrom", fromField = "parameters.contentIdFrom")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "mimeTypeId", value = "text/html")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "dataResource", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "dataText", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Action(type = ActionType.SET, field = "parameters.fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssoc", list = "assocs", conditions = {@ConditionExpr(fieldName = "contentId", fromField = "parameters.contentIdFrom"), @ConditionExpr(fieldName = "contentIdTo", fromField = "parameters.contentId"), @ConditionExpr(fieldName = "fromDate", fromField = "parameters.fromDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "contentAssocTypeId", fromField = "parameters.contentAssocTypeId", ignoreIfEmpty = true)}, orderBy = {"-fromDate"})
    @Action(type = ActionType.SET, field = "assoc", value = "${assocs[0]}")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentPurpose", list = "currentPurposes", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentPurposeType", list = "purposeTypes", orderBy = {"description"})
    @Action(type = ActionType.ENTITY_AND, entityName = "DataResource", list = "templates", fieldMaps = {@FieldMap(fieldName = "dataCategoryId", value = "TEMPLATE")}, orderBy = {"dataResourceName"})
    @Action(type = ActionType.ENTITY_AND, entityName = "StatusItem", list = "statuses", fieldMaps = {@FieldMap(fieldName = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "DataTemplateType", list = "templateTypes")
    @Action(type = ActionType.ENTITY_AND, entityName = "Content", list = "decorators", fieldMaps = {@FieldMap(fieldName = "contentTypeId", value = "DECORATOR")})
    @Section(widgets = @Widgets(containers = {@Container(id = "cmscontent", style = "no-clear", screenlets = {@ScreenletNested(title = "${uiLabelMap.PageTitleWebSiteCMSContent}", htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/website/WebSiteCMSContent.ftl"
                )})})}))
    public interface WebSiteCMSContent {}

    @Screen(name = "WebSiteCMSEditor", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "mimeTypeId", value = "text/html")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "dataResource", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "dataText", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonNotImplementedSentence}")}))
    public interface WebSiteCMSEditor {}

    @Screen(name = "WebSiteCMSMetaInfo", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/website/WebSiteCMSMetaInfo.groovy")
    @Section(widgets = @Widgets(containers = {@Container(id = "cmscontent", style = "no-clear", screenlets = {@ScreenletNested(title = "${uiLabelMap.PageTitleWebSiteCMSContent}", htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/website/WebSiteCMSMeta.ftl"
                )})})}))
    public interface WebSiteCMSMetaInfo {}

    @Screen(name = "WebSiteCMSPathAlias", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @Action(type = ActionType.ENTITY_AND, entityName = "WebSitePathAlias", list = "aliases", fieldMaps = {@FieldMap(fieldName = "webSiteId", fromField = "webSiteId"), @FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Section(widgets = @Widgets(containers = {@Container(id = "cmscontent", style = "no-clear", screenlets = {@ScreenletNested(title = "${uiLabelMap.PageTitleWebSiteCMSContent}", htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/website/WebSiteCMSPathAlias.ftl"
                )})})}))
    public interface WebSiteCMSPathAlias {}

    @Screen(name = "WebSiteAliases", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentWebSitePathAlias")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ContentWebSitePathAlias")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PathAlias")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "30")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindWebSitePathAlias", location = "component://content/widget/website/WebSiteForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWebSitePathAlias", location = "component://content/widget/website/WebSiteForms.xml"
                    )}))})})
        }
    )
    public interface WebSiteAliases {}

    @Screen(name = "WebSiteAliasesSearchResults", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListWebSitePathAlias", location = "component://content/widget/website/WebSiteForms.xml")}))
    public interface WebSiteAliasesSearchResults {}

    @Screen(name = "WebSiteSEO", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentWebSiteSEO")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WebSiteSEO")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ContentWebSiteSEO")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleWebSiteSEO}", includeForms = {
                    @IncludeForm(name = "CreateWebsiteSEO", location = "component://content/widget/website/WebSiteForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.ContentGenerateSeoUrlInfo} ${uiLabelMap.ContentGenerateSeoUrlInfoMayChangeUrls}", style = "common-msg-info", position = 0
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleRemoveWebSiteSEO}", includeForms = {
                    @IncludeForm(name = "RemoveWebsiteSEO", location = "component://content/widget/website/WebSiteForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.ContentRemoveSeoUrlInfo}", style = "common-msg-info", position = 0
                )}),
                @Screenlet(title = "${uiLabelMap.ContentGenerateSitemaps}", includeForms = {
                    @IncludeForm(name = "CreateWebsiteSitemaps", location = "component://content/widget/website/WebSiteForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.ContentGenerateSitemapsInfo}", style = "common-msg-info", position = 0
                )})})
        }
    )
    public interface WebSiteSEO {}

    @Screen(name = "WebSiteContactList", location = "component://content/widget/WebSiteScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentWebSiteContactList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WebSiteContactList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ContentWebSiteContactList")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.ENTITY_AND, entityName = "WebSiteContactList", list = "webSiteContactLists", fieldMaps = {@FieldMap(fieldName = "webSiteId", fromField = "webSiteId")}, orderBy = {"-fromDate"})
    @DecoratorScreen(
        name = "CommonWebSiteDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentWebSiteContactListCreate}", includeForms = {
                    @IncludeForm(name = "CreateWebSiteContactList", location = "component://content/widget/website/WebSiteForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ContentWebSiteContactListView} of webSiteId[${webSiteId}]", includeForms = {
                    @IncludeForm(name = "ViewWebSiteContactList", location = "component://content/widget/website/WebSiteForms.xml"
                )})})
        }
    )
    public interface WebSiteContactList {}

}
