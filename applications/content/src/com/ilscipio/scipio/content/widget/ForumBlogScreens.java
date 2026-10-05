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
public class ForumBlogScreens {

    @Screen(name = "CommonBlogDecorator", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/content/ContentMenus.xml#Blog")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonBlogDecorator {}

    @Screen(name = "BlogDecorator", location = "component://content/widget/forum/BlogScreens.xml")
    @DecoratorScreen(
        name = "CommonBlogDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.blogContentId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.blogContentId"
                ),
                @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "blogContent"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "BlogSubTabBar", location = "component://content/widget/content/ContentMenus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${blogContent.contentName}"
            ),
            @Widget(type = WidgetType.LINK, text = " [${blogContent.contentId}]", style = "${styles.link_nav_info_id}", target = "blogContent"
            )}), position = 0)})
        }
    )
    public interface BlogDecorator {}

    @Screen(name = "BlogArticleDecorator", location = "component://content/widget/forum/BlogScreens.xml")
    @DecoratorScreen(
        name = "CommonBlogDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.articleContentId"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.blogContentId"
                ),
                @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "blogContent"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "BlogArticleTabBar", location = "component://content/widget/content/ContentMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_MENU, name = "BlogArticleButtonBar", location = "component://content/widget/content/ContentMenus.xml"
            )}, containers = {
                @Container(labels = {
                    @Label(text = "${blogContent.contentName}", style = "span", position = 0
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = " [${blogContent.contentId}]", style = "${styles.link_nav_info_id}", target = "blogContent", position = 1
                )})}), position = 0)})
        }
    )
    public interface BlogArticleDecorator {}

    @Screen(name = "BlogMain", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListBlog")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentBlogList")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssocViewTo", list = "blogs", useCache = true, conditions = {@ConditionExpr(fieldName = "contentIdStart", operator = "equals", value = "BLOGROOT")}, orderBy = {"contentName"})
    @DecoratorScreen(
        name = "CommonBlogDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "BlogSubTabBar", location = "component://content/widget/content/ContentMenus.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListBlogs", location = "component://content/widget/forum/BlogForms.xml"
                )})})
        }
    )
    public interface BlogMain {}

    @Screen(name = "EditBlog", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBlog")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.blogContentId")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentBlogEdit")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @DecoratorScreen(
        name = "BlogDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditBlog", location = "component://content/widget/forum/BlogForms.xml"
                )})})
        }
    )
    public interface EditBlog {}

    @Screen(name = "BlogContent", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Articles")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentBlogArticleList")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssocViewTo", list = "blogContent", conditions = {@ConditionExpr(fieldName = "contentIdStart", operator = "equals", fromField = "parameters.blogContentId"), @ConditionExpr(fieldName = "caContentAssocTypeId", operator = "equals", value = "PUBLISH_LINK"), @ConditionExpr(fieldName = "caThruDate", operator = "equals")}, orderBy = {"caFromDate DESC"})
    @DecoratorScreen(
        name = "BlogDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "BlogContent", location = "component://content/widget/forum/BlogForms.xml"
                )})})
        }
    )
    public interface BlogContent {}

    @Screen(name = "EditArticle", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditBlogArt")
    @Action(type = ActionType.SET, field = "blogContentId", fromField = "parameters.blogContentId")
    @Action(type = ActionType.SET, field = "upPerm.contentId", fromField = "parameters.blogContentId")
    @Action(type = ActionType.SET, field = "upPerm.contentOperationId", value = "CONTENT_UPDATE")
    @Action(type = ActionType.SET, field = "upPerm.contentPurposeTypeId", value = "ARTICLE")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.articleContentId")
    @Section(actions = @Actions(value = {@Action(type = ActionType.SERVICE, serviceName = "getBlogEntry", resultMapName = "blogEntry")}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "BlogArticleDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "IMAGE"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "EditArticle", location = "component://content/widget/forum/BlogForms.xml"
            )})
        }
    )), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.EditBlogArticlePermissionError}: ${contentId} ${uiLabelMap.ContentBlog} ${blogContentId}", style = "common-msg-error-perm")}))
    public interface EditArticle {}

    @Screen(name = "ViewArticle", location = "component://content/widget/forum/BlogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewBlogArt")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.articleContentId")
    @Action(type = ActionType.SET, field = "blogContentId", fromField = "parameters.blogContentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "blogEntry")
    @DecoratorScreen(
        name = "BlogArticleDecorator",
        location = "component://content/widget/forum/BlogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${blogEntry.contentName}: ${blogEntry.description}", style = "heading"
            ),
            @Widget(type = WidgetType.HORIZONTAL_SEPARATOR),
            @Widget(type = WidgetType.CONTAINER, style = "clear"),
            @Widget(type = WidgetType.HORIZONTAL_SEPARATOR),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ContentBlogArticle}"
            ),
            @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}, containers = {
                @Container(style = "${styles.grid_large}6", labels = {
                    @Label(text = "${uiLabelMap.ContentImage}")}, containers = {
                        @Container2(widgets = {
                            @Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "IMAGE"
                        )})}, position = 2),
                        @Container(style = "${styles.grid_large}6", labels = {
                            @Label(text = "${uiLabelMap.ContentBlogSummary}")}, containers = {
                                @Container2(widgets = {
                                    @Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "SUMMARY"
                                )})}, position = 3),
                                @Container(widgets = {
                                    @Widget(type = WidgetType.SUB_CONTENT, contentId = "${contentId}", mapKey = "ARTICLE"
                                )}, position = 7)})
        }
    )
    public interface ViewArticle {}

}
