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
public class ForumForumForms {

    @Form(
        name = "ListForumGroups",
        location = "component://content/widget/forum/ForumForms.xml",
        type = FormType.LIST,
        target = "updateForumGroup",
        listName = "forumGroups",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "select", entryName = "contentId", parameterName = "contentId", widgetStyle = "${styles.link_nav} ${styles.action_find}", hyperlink = @HyperlinkField(target = "findForums", description = "${uiLabelMap.FormFieldTitle_forums}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "contentId")})),
            @FormField(name = "forumGroupName", entryName = "contentName", parameterName = "contentName", text = @TextField),
            @FormField(name = "forumGroupDescription", entryName = "description", parameterName = "description", text = @TextField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListForumGroups {}

    @Form(
        name = "AddForumGroup",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "createForumGroup",
        defaultMapName = "forumGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "forumGroupName", parameterName = "contentName", text = @TextField),
            @FormField(name = "forumGroupDescription", parameterName = "description", text = @TextField),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "FORUM_ROOT")),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddForumGroup {}

    @Form(
        name = "ListForums",
        location = "component://content/widget/forum/ForumForms.xml",
        type = FormType.LIST,
        target = "updateForum",
        listName = "forums",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "caContentId", hidden = @HiddenField(value = "${caContentId}")),
            @FormField(name = "caContentIdTo", hidden = @HiddenField(value = "${caContentIdTo}")),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "${caContentAssocTypeId}")),
            @FormField(name = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "select", entryName = "contentId", parameterName = "contentId", widgetStyle = "${styles.link_nav} ${styles.action_find}", hyperlink = @HyperlinkField(target = "findForumMessages", description = "${uiLabelMap.ContentForumMessages}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @ParameterDef(paramName = "forumId", fromField = "contentId")})),
            @FormField(name = "forumName", entryName = "contentName", parameterName = "contentName", text = @TextField),
            @FormField(name = "forumDescription", entryName = "description", parameterName = "description", text = @TextField),
            @FormField(name = "thruDate", entryName = "caThruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "updateForum", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @ParameterDef(paramName = "caContentIdTo", fromField = "contentId"), @ParameterDef(paramName = "caContentId"), @ParameterDef(paramName = "caContentAssocTypeId", fromField = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate", fromField = "caFromDate"), @ParameterDef(paramName = "deactivateExisting", value = "true")})),
            @FormField(name = "fromDate", entryName = "caFromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(description = "${groovy:caFromDate.toString().substring(0,10)}")),
            @FormField(name = "nbrOfMessages", entryName = "childBranchCount", display = @DisplayField)
        }
    )
    public interface ListForums {}

    @Form(
        name = "AddForum",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "createForum",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "forumName", parameterName = "contentName", text = @TextField),
            @FormField(name = "forumDescription", parameterName = "description", text = @TextField),
            @FormField(name = "caFromDate", dateTime = @DateTimeField),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "WEB_SITE_PUB_PT")),
            @FormField(name = "ownerContentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "caContentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "SUBSITE")),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddForum {}

    @Form(
        name = "ForumGroupPurposes",
        location = "component://content/widget/forum/ForumForms.xml",
        type = FormType.LIST,
        target = "deleteForumGroupPurpose",
        listName = "forumPurposes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentPurpose", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentPurposeTypeId", displayEntity = @DisplayEntityField(entityName = "ContentPurposeType", description = "${description}")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ForumGroupPurposes {}

    @Form(
        name = "AddForumGroupPurpose",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "createForumGroupPurpose",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentPurpose", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentPurposeTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentPurposeType", description = "${description}"))),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddForumGroupPurpose {}

    @Form(
        name = "ForumGroupRoles",
        location = "component://content/widget/forum/ForumForms.xml",
        type = FormType.LIST,
        target = "updateForumGroupRole",
        listName = "forumRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentRole", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName}${firstName} ${lastName}")),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteForumGroupRole", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @ParameterDef(paramName = "contentIdTo"), @ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ForumGroupRoles {}

    @Form(
        name = "AddForumGroupRole",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "createForumGroupRole",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentRole", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}"))),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddForumGroupRole {}

    @Form(
        name = "ListForumMessages",
        location = "component://content/widget/forum/ForumForms.xml",
        type = FormType.LIST,
        target = "updateForumMessage",
        listName = "forumMessages",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "forumId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "contentId", hidden = @HiddenField(value = "${caContentId}")),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "${caContentIdTo}")),
            @FormField(name = "contentAssocTypeId", hidden = @HiddenField(value = "${caContentAssocTypeId}")),
            @FormField(name = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "dataResourceId", hidden = @HiddenField(value = "${dataResourceId}")),
            @FormField(name = "messageTitle", entryName = "description", parameterName = "description", display = @DisplayField),
            @FormField(name = "createdBy", entryName = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "fromDate", entryName = "caFromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(description = "${groovy:caFromDate!=null?caFromDate.toString().substring(0,10):\"\"}")),
            @FormField(name = "thruDate", entryName = "caThruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(description = "${groovy:caThruDate!=null?caThruDate.toString().substring(0,10):\"\"}")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "updateForumMessage", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @ParameterDef(paramName = "forumId", fromField = "${parameters.forumId}"), @ParameterDef(paramName = "caContentIdTo"), @ParameterDef(paramName = "caContentId"), @ParameterDef(paramName = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate"), @ParameterDef(paramName = "deactivateExisting", value = "true")})),
            @FormField(name = "messageText", entryName = "contentData.resultData.electronicText.textData", parameterName = "textData", textarea = @TextareaField(rows = 8)),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "responseAction", title = "${uiLabelMap.FormFieldTitle_reponse}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "addForumMessage", description = "${uiLabelMap.FormFieldTitle_reponse}", parameters = {@ParameterDef(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @ParameterDef(paramName = "forumId", fromField = "parameters.forumId"), @ParameterDef(paramName = "forumMessageIdTo", fromField = "contentId"), @ParameterDef(paramName = "contentAssocTypeId", value = "RESPONSE")}))
        },
        rowActions = @RowActions(service = {@ServiceAction(serviceName = "getContentAndDataResource", resultMapName = "contentData", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})})
    )
    public interface ListForumMessages {}

    @Form(
        name = "EditForumMessage",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "updateForumMessage",
        defaultMapName = "message",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "forumId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "forumMessageId", hidden = @HiddenField),
            @FormField(name = "forumMessageName", parameterName = "contentName", hidden = @HiddenField),
            @FormField(name = "forumMessageTitle", parameterName = "description", text = @TextField),
            @FormField(name = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "ownerContentId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "caFromDate", hidden = @HiddenField(value = "${contentAssoc.fromDate}")),
            @FormField(name = "caToDate", hidden = @HiddenField(value = "${contentAssoc.toDate}")),
            @FormField(name = "caContentIdTo", hidden = @HiddenField),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "RESPONSE")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "forumMessageText", parameterName = "textData", textarea = @TextareaField(rows = 10)),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "message.forumMessageId", fromField = "contentData.resultData.content.contentId"), @SetAction(field = "message.contentTypeId", fromField = "contentData.resultData.content.contentTypeId"), @SetAction(field = "message.contentIdTo", fromField = "parameters.forumMessageIdTo"), @SetAction(field = "message.forumMessageName", fromField = "contentData.resultData.content.contentName"), @SetAction(field = "message.forumMessageTitle", fromField = "contentData.resultData.content.description"), @SetAction(field = "message.forumMessageText", fromField = "contentData.resultData.electronicText.textData"), @SetAction(field = "contentAssoc", fromField = "contentAssocList[0]")}, service = {@ServiceAction(serviceName = "getContentAndDataResource", resultMapName = "contentData", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumMessageId")})})
    )
    public interface EditForumMessage {}

    @Form(
        name = "AddForumMessage",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "updateForumMessage",
        defaultMapName = "message",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "VIEW_INDEX", hidden = @HiddenField(value = "${parameters.VIEW_INDEX}")),
            @FormField(name = "threadView", hidden = @HiddenField(value = "${parameters.threadView}")),
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "forumId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "contentName", hidden = @HiddenField(value = "New thread/message/response")),
            @FormField(name = "forumMessageTitle", parameterName = "description", text = @TextField(defaultValue = "${forumMessage.description}")),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "DOCUMENT")),
            @FormField(name = "ownerContentId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "caContentId", hidden = @HiddenField(value = "${parameters.forumMessageIdTo}")),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "RESPONSE")),
            @FormField(name = "forumMessageText", parameterName = "textData", textarea = @TextareaField(rows = 10, visualEditorEnable = true)),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddForumMessage {}

    @Form(
        name = "AddForumThreadMessage",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "updateForumThreadMessage",
        extendsForm = "AddForumMessage",
        headerRowStyle = "header-row"
    )
    public interface AddForumThreadMessage {}

    @Form(
        name = "EditForumThreadMessage",
        location = "component://content/widget/forum/ForumForms.xml",
        target = "updateForumThreadMessage",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "forumGroupId", hidden = @HiddenField(value = "${parameters.forumGroupId}")),
            @FormField(name = "forumId", hidden = @HiddenField(value = "${parameters.forumId}")),
            @FormField(name = "rsp.contentId", parameterName = "contentId", hidden = @HiddenField),
            @FormField(name = "rsp.contentName", parameterName = "contentName", hidden = @HiddenField),
            @FormField(name = "rsp.description", parameterName = "description", title = "${uiLabelMap.ContentForumDescriptionThread}", text = @TextField),
            @FormField(name = "rsp.contentTypeId", parameterName = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "rsp.ownerContentId", parameterName = "ownerContentId", hidden = @HiddenField),
            @FormField(name = "rsp.caFromDate", parameterName = "fromDate", hidden = @HiddenField),
            @FormField(name = "rsp.caThruDate", parameterName = "thruDate", hidden = @HiddenField),
            @FormField(name = "rsp.caContentIdTo", parameterName = "caContentIdTo", hidden = @HiddenField),
            @FormField(name = "rsp.caContentAssocTypeId", parameterName = "caContentAssocTypeId", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "textData", title = "${uiLabelMap.FormFieldTitle_textDataTitle}", textarea = @TextareaField(rows = 8)),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditForumThreadMessage {}

}
