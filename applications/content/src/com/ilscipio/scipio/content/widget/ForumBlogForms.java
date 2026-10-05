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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ForumBlogForms {

    @Form(
        name = "ListBlogs",
        location = "component://content/widget/forum/BlogForms.xml",
        type = FormType.LIST,
        listName = "blogs",
        paginateTarget = "blogMain",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "editBlog", description = "${contentName} [${contentId}]", parameters = {@ParameterDef(paramName = "blogContentId", fromField = "contentId")})),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "localeString", displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType")),
            @FormField(name = "lastModifiedDate", display = @DisplayField(type = "date")),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField)
        }
    )
    public interface ListBlogs {}

    @Form(
        name = "BlogContent",
        location = "component://content/widget/forum/BlogForms.xml",
        type = FormType.LIST,
        listName = "blogContent",
        paginateTarget = "blogContent",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentName", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewBlogArticle", description = "${contentName} [${contentId}]", parameters = {@ParameterDef(paramName = "articleContentId", fromField = "contentId"), @ParameterDef(paramName = "blogContentId", fromField = "parameters.blogContentId")})),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "localeString", displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType"))
        }
    )
    public interface BlogContent {}

    @Form(
        name = "EditBlog",
        location = "component://content/widget/forum/BlogForms.xml",
        target = "updateBlog",
        defaultMapName = "content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentTypeId", useWhen = "content==null", hidden = @HiddenField(value = "WEB_SITE_PUB_PT")),
            @FormField(name = "contentIdFrom", useWhen = "content==null", hidden = @HiddenField(value = "BLOGROOT")),
            @FormField(name = "contentAssocTypeId", hidden = @HiddenField(value = "SUB_CONTENT")),
            @FormField(name = "contentName", title = "${uiLabelMap.ContentBlogName}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "description", title = "${uiLabelMap.ContentBlogDescription}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "content==null", target = "newBlog")
        }
    )
    public interface EditBlog {}

    @Form(
        name = "EditArticle",
        location = "component://content/widget/forum/BlogForms.xml",
        type = FormType.UPLOAD,
        target = "createBlogArticle",
        defaultMapName = "blogEntry",
        defaultTitleStyle = "treeHeader",
        defaultWidgetStyle = "inputBox",
        skipEnd = "true",
        fields = {
            @FormField(name = "blogContentId", hidden = @HiddenField(value = "${parameters.blogContentId}")),
            @FormField(name = "contentId", title = "${uiLabelMap.ContentBlogEntryId}", useWhen = "contentId!=void&&contentId!=null", display = @DisplayField),
            @FormField(name = "contentName", title = "${uiLabelMap.ContentArticleName}", text = @TextField(size = 40)),
            @FormField(name = "description", textarea = @TextareaField(rows = 2)),
            @FormField(name = "summaryData", title = "${uiLabelMap.ContentSummary}", widgetStyle = "inputBox", textarea = @TextareaField(rows = 4)),
            @FormField(name = "articleData", title = "${uiLabelMap.ContentBlogArticle}", widgetStyle = "inputBox", textarea = @TextareaField(cols = 100, rows = 20, visualEditorEnable = true)),
            @FormField(name = "uploadedFile", title = "${uiLabelMap.ContentImage}", file = @FileField),
            @FormField(name = "templateDataResourceId", title = "${uiLabelMap.ContentTemplate}", dropDown = @DropDownField(options = {@Option(key = "BLOG_TPL_TOPLEFT", description = "${uiLabelMap.ContentBlogTopLeft}"), @Option(key = "BLOG_TPL_TOPCENTER", description = "${uiLabelMap.ContentBlogTopCenter}")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(options = {@Option(key = "CTNT_PUBLISHED", description = "${uiLabelMap.ContentBlogPublish}"), @Option(key = "CTNT_INITIAL_DRAFT", description = "${uiLabelMap.ContentBlogPreview}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentId!=void&&contentId!=null", target = "updateBlogArticle")
        }
    )
    public interface EditArticle {}

}
