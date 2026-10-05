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
public class CmsCMSScreens {

    @Screen(name = "CMSContentFind", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCMSContent")
    @Action(type = ActionType.SET, field = "entityName", value = "ContentAssocDataResourceViewFrom")
    @Action(type = ActionType.SERVICE, serviceName = "urlEncodeArgs", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "mapIn", fromField = "requestParameters")})
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "20")
    @Action(type = ActionType.SET, field = "currentCMSMenuItemName", value = "contentfind")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditAddContent"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "findContent", location = "component://content/widget/cms/CMSForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "listFindContent", location = "component://content/widget/cms/CMSForms.xml"
                        )}))})))})
        }
    )
    public interface CMSContentFind {}

    @Screen(name = "CMSContentEdit", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCMSContent")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "electronicText", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "parameters.drDataResourceId")})
    @Action(type = ActionType.SET, field = "textData", fromField = "electronicText.textData")
    @Action(type = ActionType.SET, field = "contentId", fromField = "currentValue.contentId")
    @Action(type = ActionType.SET, field = "dataResourceId", fromField = "parameters.drDataResourceId")
    @Action(type = ActionType.SET, field = "rootForumId", value = "WebStoreFORUM")
    @Action(type = ActionType.SET, field = "rootForumId2", value = "WebStoreCONTENT")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/FeaturePrep.groovy")
    @Action(type = ActionType.SET, field = "menuContext.contentTarget", value = "CMSContentEdit?contentId=${contentId}&drDataResourceId=${dataResourceId}")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/cms/CMSContentEdit.ftl"
                )})})
        }
    )
    public interface CMSContentEdit {}

    @Screen(name = "EditAddContent", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentCMSEditPage")
    @Action(type = ActionType.SET, field = "entityOperation", value = "_UPDATE")
    @Action(type = ActionType.SET, field = "targetOperation", value = "CONTENT_UPDATE|CONTENT_CREATE|CONTENT_CREATE_SUB")
    @Action(type = ActionType.SET, field = "requiredRoles", value = "OWNER|BLOG_AUTHOR|BLOG_EDITOR|BLOG_ADMIN|BLOG_PUBLISHER")
    @Action(type = ActionType.SET, field = "contentPurposeTypeId", value = "ARTICLE")
    @Action(type = ActionType.SET, field = "MASTER_contentId", fromField = "parameters.MASTER_contentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "MASTER_drDataResourceId", fromField = "parameters.MASTER_drDataResourceId", defaultValue = "${parameters.drDataResourceId}")
    @Action(type = ActionType.SET, field = "MASTER_caContentIdTo", fromField = "parameters.MASTER_caContentIdTo", defaultValue = "${parameters.caContentIdTo}")
    @Action(type = ActionType.SET, field = "MASTER_caContentId", fromField = "parameters.MASTER_caContentId", defaultValue = "${parameters.caContentIdFrom}")
    @Action(type = ActionType.SET, field = "MASTER_caContentAssocTypeId", fromField = "parameters.MASTER_caContentAssocTypeId", defaultValue = "${parameters.caContentAssocTypeId}")
    @Action(type = ActionType.SET, field = "MASTER_caMapKey", fromField = "parameters.MASTER_caMapKey", defaultValue = "${parameters.caMapKey}")
    @Action(type = ActionType.SET, field = "MASTER_caFromDate", fromField = "parameters.MASTER_caFromDate", valueType = "Timestamp", defaultValue = "${parameters.caFromDate}")
    @Action(type = ActionType.SET, field = "MASTER_caThruDate", fromField = "parameters.MASTER_caThruDate", valueType = "Timestamp", defaultValue = "${parameters.caThruDate}")
    @Action(type = ActionType.SET, field = "contentId", fromField = "MASTER_contentId")
    @Action(type = ActionType.SET, field = "drDataResourceId", fromField = "MASTER_drDataResourceId")
    @Action(type = ActionType.SET, field = "caContentIdTo", fromField = "MASTER_caContentIdTo")
    @Action(type = ActionType.SET, field = "caContentId", fromField = "MASTER_caContentId")
    @Action(type = ActionType.SET, field = "caContentAssocTypeId", fromField = "MASTER_caContentAssocTypeId")
    @Action(type = ActionType.SET, field = "caFromDate", fromField = "MASTER_caFromDate")
    @Action(type = ActionType.SET, field = "caThruDate", fromField = "MASTER_caThruDate")
    @Action(type = ActionType.SET, field = "caMapKey", fromField = "MASTER_caMapKey")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/CmsEditAddPrep.groovy")
    @Action(type = ActionType.SET, field = "currentCMSMenuItemName", value = "EditAddContent")
    @Action(type = ActionType.SET, field = "enableEdit", value = "true")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAddContentStuff", location = "component://content/widget/cms/CMSForms.xml", position = 1
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentGoToFind}", style = "${styles.link_nav} ${styles.action_find}", target = "CMSContentFind?VIEW_INDEX=${CMSContentFindViewIndex}&${CMSContentFindQueryString}", position = 0
                ),
                @Widget(type = WidgetType.CONTENT, contentId = "${contentId}", editRequest = "EditAddSubContent?MASTER_caMapKey=${MASTER_caMapKey}&MASTER_contentId=${MASTER_contentId}&MASTER_caContentIdTo=${MASTER_caContentIdTo}&MASTER_caContentAssocTypeId=${MASTER_caContentAssocTypeId}&MASTER_caFromDate=${MASTER_caFromDate}&MASTER_caThruDate=${MASTER_caThruDate}&MASTER_drDataResourceId=${MASTER_drDataResourceId}&caContentIdTo=${caContentIdTo}", enableEditName = "notfound", position = 2
            )})})
        }
    )
    public interface EditAddContent {}

    @Screen(name = "EditAddSubContent", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentCMSAddSubContent")
    @Action(type = ActionType.SET, field = "entityOperation", value = "_UPDATE")
    @Action(type = ActionType.SET, field = "targetOperation", value = "CONTENT_UPDATE|CONTENT_CREATE|CONTENT_CREATE_SUB")
    @Action(type = ActionType.SET, field = "requiredRoles", value = "OWNER|BLOG_AUTHOR|BLOG_EDITOR|BLOG_ADMIN|BLOG_PUBLISHER")
    @Action(type = ActionType.SET, field = "contentPurposeTypeId", value = "ARTICLE")
    @Action(type = ActionType.SET, field = "MASTER_contentId", fromField = "parameters.MASTER_contentId")
    @Action(type = ActionType.SET, field = "MASTER_drDataResourceId", fromField = "parameters.MASTER_drDataResourceId")
    @Action(type = ActionType.SET, field = "MASTER_caContentIdTo", fromField = "parameters.MASTER_caContentIdTo")
    @Action(type = ActionType.SET, field = "MASTER_caContentId", fromField = "parameters.MASTER_caContentId")
    @Action(type = ActionType.SET, field = "MASTER_caContentAssocTypeId", fromField = "parameters.MASTER_caContentAssocTypeId")
    @Action(type = ActionType.SET, field = "MASTER_caFromDate", fromField = "parameters.MASTER_caFromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "caContentIdTo", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "caMapKey", fromField = "parameters.mapKey")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/CmsEditAddPrep.groovy")
    @Action(type = ActionType.SET, field = "currentCMSMenuItemName", value = "EditAddContent")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditAddSubContentStuff", location = "component://content/widget/cms/CMSForms.xml"
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentGoToFind}", style = "${styles.link_nav} ${styles.action_find}", target = "CMSContentFind?VIEW_INDEX=${CMSContentFindViewIndex}&${CMSContentFindQueryString}"
                )})})
        }
    )
    public interface EditAddSubContent {}

    @Screen(name = "CMSSites", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentCMSSearchPage")
    @Action(type = ActionType.SET, field = "forumId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "defaultSiteId", value = "WebStoreFORUM")
    @Action(type = ActionType.SET, field = "currentCMSMenuItemName", value = "subsites")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/UserPermPrep.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/MostRecentPrep.groovy")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/cms/CMSSites.ftl"
                )})})
        }
    )
    public interface CMSSites {}

    @Screen(name = "addSubSite", location = "component://content/widget/cms/CMSScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchContent")
    @DecoratorScreen(
        name = "CommonCmsDecorator",
        location = "${parameters.cmsDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/cms/addSubSite.ftl"
                )})})
        }
    )
    public interface addSubSite {}

}
