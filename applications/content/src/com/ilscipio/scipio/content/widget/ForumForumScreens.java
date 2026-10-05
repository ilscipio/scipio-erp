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
public class ForumForumScreens {

    @Screen(name = "FindForumGroups", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindForumGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ForumGroups")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumGroup")
    @Action(type = ActionType.ENTITY_AND, entityName = "Content", list = "forumGroups", fieldMaps = {@FieldMap(fieldName = "contentTypeId", value = "FORUM_ROOT")}, orderBy = {"contentName"})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListForumGroups", location = "component://content/widget/forum/ForumForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumGroupCreate}", name = "ForumGroupPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddForumGroup", location = "component://content/widget/forum/ForumForms.xml"
                )}, position = 0)})
        }
    )
    public interface FindForumGroups {}

    @Screen(name = "FindForums", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "forums")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindForums")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumGroupSideBar")
    @Action(type = ActionType.SET, field = "forumGroupId", fromField = "parameters.forumGroupId", defaultValue = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumGroup", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "forumGroupId")})
    @Action(type = ActionType.SET, field = "objectName", fromField = "forumGroup.contentName")
    @Action(type = ActionType.SET, field = "objectId", fromField = "forumGroup.contentId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssocDataResourceViewTo", list = "forums", fieldMaps = {@FieldMap(fieldName = "contentIdStart", fromField = "forumGroupId")})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListForums", location = "component://content/widget/forum/ForumForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddTo} ${forumGroup.contentName}", name = "AddForumToForumGroupPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddForum", location = "component://content/widget/forum/ForumForms.xml"
                )}, position = 0)})
        }
    )
    public interface FindForums {}

    @Screen(name = "ForumGroupRoles", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleForumGroupRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "roles")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumGroupSideBar")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumGroup", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumGroupId")})
    @Action(type = ActionType.SET, field = "objectName", fromField = "forumGroup.contentName")
    @Action(type = ActionType.SET, field = "objectId", fromField = "forumGroup.contentId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentRole", list = "forumRoles", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumGroupId")}, orderBy = {"roleTypeId"})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ForumGroupRoles", location = "component://content/widget/forum/ForumForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddRoleTo} ${forumGroup.contentName} [${forumGroup.contentId}]", name = "ForumGroupRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddForumGroupRole", location = "component://content/widget/forum/ForumForms.xml"
                )}, position = 0)})
        }
    )
    public interface ForumGroupRoles {}

    @Screen(name = "ForumGroupPurposes", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleForumGroupPurposes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "purposes")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumGroupSideBar")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumGroup", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumGroupId")})
    @Action(type = ActionType.SET, field = "objectName", fromField = "forumGroup.contentName")
    @Action(type = ActionType.SET, field = "objectId", fromField = "forumGroup.contentId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentPurpose", list = "forumPurposes", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumGroupId")}, orderBy = {"contentPurposeTypeId"})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ForumGroupPurposes", location = "component://content/widget/forum/ForumForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddPurposeTo} ${forumGroup.contentName} [${forumGroup.contentId}]", name = "ForumGroupPurposePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddForumGroupPurpose", location = "component://content/widget/forum/ForumForms.xml"
                )}, position = 0)})
        }
    )
    public interface ForumGroupPurposes {}

    @Screen(name = "FindForumMessages", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumMessagesSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "messageList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forum", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumId")})
    @Action(type = ActionType.SET, field = "objectName", fromField = "forum.contentName")
    @Action(type = ActionType.SET, field = "objectId", fromField = "forum.contentId")
    @Action(type = ActionType.SET, field = "forumNbrMessages", fromField = "forum.childBranchCount", defaultValue = "0")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssocViewTo", list = "forumMessages", fieldMaps = {@FieldMap(fieldName = "ownerContentId", fromField = "parameters.forumId")}, orderBy = {"createdDate DESC"})
    @Action(type = ActionType.SET, field = "parameters.forumMessageIdTo", fromField = "parameters.forumId")
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListForumMessages", location = "component://content/widget/forum/ForumForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddThreadTo} ${forum.description}", includeForms = {
                    @IncludeForm(name = "AddForumMessage", location = "component://content/widget/forum/ForumForms.xml"
                )})})
        }
    )
    public interface FindForumMessages {}

    @Screen(name = "FindForumThreads", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.forumId")
    @Action(type = ActionType.SET, field = "responseContentId", fromField = "parameters.forumId")
    @Action(type = ActionType.SET, field = "threadContentId", fromField = "parameters.threadContentId", defaultValue = "${contentId}")
    @Action(type = ActionType.SET, field = "forumId", fromField = "parameters.forumId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Action(type = ActionType.SET, field = "ownerContentId", fromField = "content.ownerContentId", defaultValue = "${forumId}")
    @Action(type = ActionType.SET, field = "trail", fromField = "threadContentId")
    @Action(type = ActionType.SET, field = "enableEdit", value = "false")
    @Action(type = ActionType.SET, field = "webPutPt", fromField = "parameters.forumGroupId")
    @Action(type = ActionType.SET, field = "rsp.contentName", value = "${content.contentName}")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumMessagesSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "messageThread")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forum", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumId")})
    @Action(type = ActionType.SET, field = "objectName", fromField = "forum.contentName")
    @Action(type = ActionType.SET, field = "objectId", fromField = "forum.contentId")
    @Action(type = ActionType.SET, field = "forumNbrMessages", fromField = "forum.childBranchCount", defaultValue = "0")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumThread", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumThreadId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumMessage", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumMessageId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "electronicText", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "forumMessage.dataResourceId")})
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.forumId")
    @Action(type = ActionType.SET, field = "trail", fromField = "parameters.trail", defaultValue = "${contentId}")
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_TREE, name = "MessageTree", location = "component://content/widget/forum/ForumTrees.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.forumThreadId"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.ContentForumThread} ${parameters.forumThreadId}", includeForms = {
                        @IncludeForm(name = "EditForumThreadMessage", location = "component://content/widget/forum/ForumForms.xml"
                    )})})),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.forumMessageId"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.ContentForumMessage} ${parameters.forumMessageId}", includeForms = {
                            @IncludeForm(name = "EditForumThreadMessage", location = "component://content/widget/forum/ForumForms.xml"
                        )})}))})
        }
    )
    public interface FindForumThreads {}

    @Screen(name = "EditForumMessage", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditForumMessage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumMessagesSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumMessages")
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddResponseToMessage} ${contentData.resultData.content.description}", includeForms = {
                    @IncludeForm(name = "EditForumMessage", location = "component://content/widget/forum/ForumForms.xml"
                )})})
        }
    )
    public interface EditForumMessage {}

    @Screen(name = "AddForumMessage", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditForumMessage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumMessagesSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumMessage", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumMessageIdTo")})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddResponseToMessage} ${forumMessage.description}", includeForms = {
                    @IncludeForm(name = "AddForumMessage", location = "component://content/widget/forum/ForumForms.xml"
                )})})
        }
    )
    public interface AddForumMessage {}

    @Screen(name = "AddForumThreadMessage", location = "component://content/widget/forum/ForumScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditForumMessage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditForumMessage")
    @Action(type = ActionType.SET, field = "tabBar", value = "ForumMessagesSideBar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindForumMessages")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "forumMessage", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumMessageIdTo")})
    @DecoratorScreen(
        name = "CommonForumDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentForumAddResponseToMessage} ${forumMessage.description} [${forumMessage.contentId}]", includeForms = {
                    @IncludeForm(name = "AddForumThreadMessage", location = "component://content/widget/forum/ForumForms.xml"
                )})})
        }
    )
    public interface AddForumThreadMessage {}

}
