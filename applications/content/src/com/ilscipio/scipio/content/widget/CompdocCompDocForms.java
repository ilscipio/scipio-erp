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
public class CompdocCompDocForms {

    @Form(
        name = "FindCompDoc",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "FindCompDoc",
        defaultEntityName = "Content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", mapName = "emptyMap", textFind = @TextFindField),
            @FormField(name = "contentName", textFind = @TextFindField),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(options = {@Option(key = "COMPDOC_TEMPLATE", description = "${uiLabelMap.ContentTemplateRoot}"), @Option(key = "COMPDOC_INSTANCE", description = "${uiLabelMap.ContentTemplateRootInstance}"), @Option(key = "TEMPLATE", description = "${uiLabelMap.ContentTemplateChild}"), @Option(key = "DOCUMENT", description = "${uiLabelMap.ContentTemplateInstanceChild}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindCompDoc {}

    @Form(
        name = "ListCompDoc",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindCompDoc",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentTypeId", display = @DisplayField),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "editAction", useWhen = "\"COMPDOC_TEMPLATE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRootCompDoc", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "rootContentId", fromField = "caContentIdTo"), @ParameterDef(paramName = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate")})),
            @FormField(name = "editAction", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRootCompDoc", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "rootContentId", fromField = "caContentIdTo"), @ParameterDef(paramName = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate")})),
            @FormField(name = "editAction", useWhen = "\"TEMPLATE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditChildCompDoc", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "rootContentId", fromField = "caContentIdTo"), @ParameterDef(paramName = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate")})),
            @FormField(name = "editAction", useWhen = "\"DOCUMENT\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditChildCompDoc", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "rootContentId", fromField = "caContentIdTo"), @ParameterDef(paramName = "caContentAssocTypeId"), @ParameterDef(paramName = "caFromDate")})),
            @FormField(name = "tree", useWhen = "\"COMPDOC_TEMPLATE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCompDocTemplateTree", description = "${uiLabelMap.ContentTree}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId")})),
            @FormField(name = "tree", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCompDocInstanceTree", description = "${uiLabelMap.ContentTree}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Content"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListCompDoc {}

    @Form(
        name = "ListCompDocInstances",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        listName = "compDocFindList",
        paginateTarget = "ListCompDocInstances",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "rootContentId", title = "${uiLabelMap.FormFieldTitle_contentIdTo}", display = @DisplayField),
            @FormField(name = "caContentAssocTypeId", title = "${uiLabelMap.FormFieldTitle_contentAssocTypeId}", display = @DisplayField),
            @FormField(name = "caFromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "caThruDate", hidden = @HiddenField),
            @FormField(name = "editTemplate", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditRootCompDoc", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId")})),
            @FormField(name = "templateTree", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCompDocInstanceTree", description = "${uiLabelMap.ContentTree}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId")}))
        }
    )
    public interface ListCompDocInstances {}

    @Form(
        name = "EditContentRevision",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateContentRevision",
        defaultMapName = "contentRevision",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentRevision")
        },
        fields = {
            @FormField(name = "rootContentId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "rootContentRevisionSeqId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "contentRevisionSeqId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "committedByPartyId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "comments", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentRevision==null", target = "createContentRevision")
        }
    )
    public interface EditContentRevision {}

    @Form(
        name = "ListContentRevisions",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        listName = "contentRevisionList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentRevisionSeqId", display = @DisplayField),
            @FormField(name = "committedByPartyId", display = @DisplayField),
            @FormField(name = "comments", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditContentRevision", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentRevisionSeqId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")})),
            @FormField(name = "itemAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ListContentRevisionItem", description = "${uiLabelMap.CommonItems}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentRevisionSeqId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")})),
            @FormField(name = "create", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_add}", hyperlink = @HyperlinkField(target = "EditContentRevision", description = "${uiLabelMap.CommonCreate}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentRevisionSeqId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")})),
            @FormField(name = "tree", title = " ", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCompDocInstanceTree", description = "${uiLabelMap.ContentTree}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId"), @ParameterDef(paramName = "rootContentRevisionSeqId", fromField = "contentRevisionSeqId")})),
            @FormField(name = "tree", title = " ", useWhen = "\"COMPDOC_TEMPLATE\".equals(contentTypeId)", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewCompDocTemplateTree", description = "${uiLabelMap.ContentTree}", alsoHidden = false, parameters = {@ParameterDef(paramName = "rootContentId", fromField = "contentId"), @ParameterDef(paramName = "rootContentRevisionSeqId", fromField = "contentRevisionSeqId")}))
        }
    )
    public interface ListContentRevisions {}

    @Form(
        name = "EditContentRevisionItem",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateContentRevisionItem",
        defaultMapName = "contentRevisionItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentRevisionItem")
        },
        fields = {
            @FormField(name = "rootContentId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "rootContentRevisionSeqId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "contentRevisionSeqId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "itemContentId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "oldDataResourceId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "newDataResourceId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "contentRevisionItem==null", target = "createContentRevisionItem")
        }
    )
    public interface EditContentRevisionItem {}

    @Form(
        name = "ListContentRevisionItem",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        listName = "contentRevisionItemList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentRevisionSeqId", display = @DisplayField),
            @FormField(name = "itemContentId", display = @DisplayField),
            @FormField(name = "oldDataResourceId", display = @DisplayField),
            @FormField(name = "newDataResourceId", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditContentRevisionItem", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentRevisionSeqId"), @ParameterDef(paramName = "itemContentId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")})),
            @FormField(name = "create", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_add}", hyperlink = @HyperlinkField(target = "EditContentRevisionItem", description = "${uiLabelMap.CommonCreate}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentRevisionSeqId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")}))
        }
    )
    public interface ListContentRevisionItem {}

    @Form(
        name = "EditContentApproval",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateContentApproval",
        defaultMapName = "contentApproval",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentApprovalId", display = @DisplayField),
            @FormField(name = "contentId", widgetStyle = "inputBox", hidden = @HiddenField),
            @FormField(name = "contentRevisionSeqId", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "partyId", widgetStyle = "inputBox", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approvalStatusId", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "SOFT_REJ", description = "${uiLabelMap.ContentSoftRejected}"), @Option(key = "REJECTED", description = "${uiLabelMap.ContentRejected}"), @Option(key = "APPROVED", description = "${uiLabelMap.CommonApproved}")})),
            @FormField(name = "approvalDate", widgetStyle = "inputBox", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "comments", widgetStyle = "inputBox", textarea = @TextareaField(cols = 30, rows = 3)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditContentApproval {}

    @Form(
        name = "AddContentApproval",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "createContentApproval",
        defaultMapName = "contentApproval",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", widgetStyle = "inputBox", hidden = @HiddenField),
            @FormField(name = "contentRevisionSeqId", mapName = "emptyMap", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "partyId", mapName = "emptyMap", widgetStyle = "inputBox", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approvalDate", mapName = "emptyMap", widgetStyle = "inputBox", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", mapName = "emptyMap", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "comments", mapName = "emptyMap", widgetStyle = "inputBox", text = @TextField(size = 40)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentApproval {}

    @Form(
        name = "ListContentApproval",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        target = "updateContentApproval",
        listName = "contentApprovalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentApprovalId", display = @DisplayField),
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "rootContentId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "contentRevisionSeqId", hidden = @HiddenField),
            @FormField(name = "partyId", widgetStyle = "inputBox", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approvalStatusId", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "SOFT_REJ", description = "${uiLabelMap.ContentSoftRejected}"), @Option(key = "REJECTED", description = "${uiLabelMap.ContentRejected}"), @Option(key = "APPROVED", description = "${uiLabelMap.CommonApproved}")})),
            @FormField(name = "approvalDate", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", widgetStyle = "inputBox", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", useWhen = "\"COMPDOC_TEMPLATE\".equals(contentTypeId)", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "comments", widgetStyle = "inputBox", textarea = @TextareaField(cols = 30, rows = 3)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentApproval", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "contentApprovalId"), @ParameterDef(paramName = "rootContentId"), @ParameterDef(paramName = "rootContentRevisionSeqId")}))
        }
    )
    public interface ListContentApproval {}

    @Form(
        name = "ListWaitingContentApproval",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.LIST,
        target = "updateWaitingContentApproval",
        listName = "contentApprovalList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentApprovalId", display = @DisplayField),
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "rootContentId", mapName = "emptyMap", hidden = @HiddenField),
            @FormField(name = "contentRevisionSeqId", display = @DisplayField),
            @FormField(name = "partyId", widgetStyle = "inputBox", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "approvalStatusId", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "SOFT_REJ", description = "${uiLabelMap.ContentSoftRejected}"), @Option(key = "REJECTED", description = "${uiLabelMap.ContentRejected}"), @Option(key = "APPROVED", description = "${uiLabelMap.CommonApproved}")})),
            @FormField(name = "approvalDate", widgetStyle = "inputBox", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", widgetStyle = "inputBox", text = @TextField),
            @FormField(name = "comments", widgetStyle = "inputBox", text = @TextField(size = 40)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "\"COMPDOC_INSTANCE\".equals(contentTypeId)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListWaitingContentApproval {}

    @Form(
        name = "AddRootCompDocInstance",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "genCompDocInstance",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "instanceOfContentId", mapName = "parameters", entryName = "rootContentId", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "COMPDOC_INSTANCE")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddRootCompDocInstance {}

    @Form(
        name = "EditRootCompDocInstance",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateRootCompDocTemplate",
        defaultMapName = "content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "instanceOfContentId", display = @DisplayField),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "COMPDOC_INSTANCE")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditRootCompDocInstance {}

    @Form(
        name = "AddRootCompDocTemplate",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "createRootCompDocTemplate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentName", title = "${uiLabelMap.ContentCompDocTemplateName}", text = @TextField),
            @FormField(name = "contentTypeId", mapName = "dummy", hidden = @HiddenField(value = "COMPDOC_TEMPLATE")),
            @FormField(name = "rootTemplateContentId", hidden = @HiddenField),
            @FormField(name = "rootTemplateRevSeqId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddRootCompDocTemplate {}

    @Form(
        name = "EditRootCompDocTemplate",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateRootCompDocTemplate",
        defaultMapName = "content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "contentTypeId", mapName = "dummy", hidden = @HiddenField(value = "COMPDOC_TEMPLATE")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditRootCompDocTemplate {}

    @Form(
        name = "AddChildCompDoc",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "${childCompDocTarget}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "rootContentId", hidden = @HiddenField),
            @FormField(name = "contentId", ignored = @IgnoredField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "contentTypeId", hidden = @HiddenField),
            @FormField(name = "instanceOfContentId", useWhen = "\"DOCUMENT\".equals(contentTypeId)", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "caContentIdTo", mapName = "parameters", entryName = "rootContentId", title = "${uiLabelMap.ContentCompDocParentContentId}", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "mimeTypeId", title = "${uiLabelMap.ContentDocumentType}", useWhen = "\"DOCUMENT\".equals(contentTypeId) && \"SURVEY\".equals(templateDataResourceTypeId)", hidden = @HiddenField(value = "application/vnd.ofbiz.survey.response")),
            @FormField(name = "displayMimeTypeId", mapName = "emptyMap", title = "${uiLabelMap.ContentDocumentType}", useWhen = "\"DOCUMENT\".equals(contentTypeId) && \"SURVEY\".equals(templateDataResourceTypeId)", display = @DisplayField(description = "Survey Response")),
            @FormField(name = "mimeTypeId", title = "${uiLabelMap.ContentDocumentType}", useWhen = "\"DOCUMENT\".equals(contentTypeId) && !\"SURVEY\".equals(templateDataResourceTypeId)", displayEntity = @DisplayEntityField(entityName = "MimeType", description = "${description}")),
            @FormField(name = "mimeTypeId", title = "${uiLabelMap.ContentDocumentType}", useWhen = "\"TEMPLATE\".equals(contentTypeId)", widgetStyle = "+smallSelect", dropDown = @DropDownField(options = {@Option(key = "application/msword", description = "${uiLabelMap.ContentMSWord}"), @Option(key = "application/pdf", description = "${uiLabelMap.ContentPDFFile}"), @Option(key = "application/vnd.ofbiz.survey", description = "${uiLabelMap.ContentSurvey}"), @Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "application/octet-stream", description = "${uiLabelMap.ContentResourceOther}")})),
            @FormField(name = "rootContentId", hidden = @HiddenField),
            @FormField(name = "rootContentRevisionSeqId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddChildCompDoc {}

    @Form(
        name = "EditChildCompDoc",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "${childCompDocTarget}",
        defaultMapName = "contentAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentName", mapName = "itemContent", text = @TextField),
            @FormField(name = "contentTypeId", mapName = "itemContent", hidden = @HiddenField),
            @FormField(name = "instanceOfContentId", mapName = "itemContent", useWhen = "\"DOCUMENT\".equals(contentTypeId)", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentIdTo", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", useWhen = "\"TEMPLATE\".equals(contentTypeId)", dropDown = @DropDownField(options = {@Option(key = "application/msword", description = "${uiLabelMap.ContentMSWord}"), @Option(key = "application/pdf", description = "${uiLabelMap.ContentPDFFile}"), @Option(key = "application/vnd.ofbiz.survey", description = "${uiLabelMap.ContentSurvey}"), @Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "application/octet-stream", description = "${uiLabelMap.ContentResourceOther}")})),
            @FormField(name = "mimeTypeId", mapName = "dataResource", useWhen = "\"DOCUMENT\".equals(contentTypeId) && mimeTypeId!=null && !\"application/vnd.ofbiz.survey\".equals(mimeTypeId)", displayEntity = @DisplayEntityField(entityName = "MimeType", keyFieldName = "mimeTypeId", description = "${description}")),
            @FormField(name = "mimeTypeId", mapName = "dataResource", useWhen = "\"DOCUMENT\".equals(contentTypeId) && mimeTypeId!=null && \"application/vnd.ofbiz.survey\".equals(mimeTypeId)", display = @DisplayField(description = "Survey Response")),
            @FormField(name = "relatedDetailId", mapName = "dataResource", title = "${uiLabelMap.ContentSurvey}", useWhen = "dataResource!=null && \"SURVEY\".equals(dataResource.getString(\"dataResourceTypeId\"))", lookup = @LookupField(targetFormName = "LookupSurvey")),
            @FormField(name = "relatedDetailId", mapName = "dataResource", title = "${uiLabelMap.ContentSurveyResponse}", useWhen = "dataResource!=null && \"SURVEY_RESPONSE\".equals(dataResource.getString(\"dataResourceTypeId\"))", lookup = @LookupField(targetFormName = "LookupSurveyResponse")),
            @FormField(name = "addSurveyResponse", mapName = "dummy", useWhen = "\"DOCUMENT\".equals(contentTypeId) && dataResource!=null && dataResource.get(\"relatedDetailId\")==null && templateDataResource!=null && templateDataResource.get(\"relatedDetailId\")!=null", widgetStyle = "${styles.link_nav_long} ${styles.action_add}", hyperlink = @HyperlinkField(target = "EditSurveyResponse", description = "${uiLabelMap.CommonCreate} ${uiLabelMap.ContentSurveyResponse} (${uiLabelMap.CommonFor} ${uiLabelMap.ContentSurvey} ${templateDataResource.relatedDetailId})", alsoHidden = false, targetWindow = "_blank", parameters = {@ParameterDef(paramName = "surveyId", fromField = "templateDataResource.relatedDetailId"), @ParameterDef(paramName = "dataResourceId", fromField = "dataResource.dataResourceId"), @ParameterDef(paramName = "rootContentId")})),
            @FormField(name = "updateSurveyResponse", mapName = "dummy", useWhen = "\"DOCUMENT\".equals(contentTypeId) && dataResource!=null && dataResource.get(\"relatedDetailId\")!=null && templateDataResource!=null && templateDataResource.get(\"relatedDetailId\")!=null", widgetStyle = "${styles.link_nav_long} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditSurveyResponse", description = "${uiLabelMap.CommonUpdate} ${uiLabelMap.ContentSurveyResponse} (${uiLabelMap.CommonFor} ${uiLabelMap.ContentSurvey} ${templateDataResource.relatedDetailId}, Response ${dataResource.relatedDetailId})", alsoHidden = false, targetWindow = "_blank", parameters = {@ParameterDef(paramName = "surveyId", fromField = "templateDataResource.relatedDetailId"), @ParameterDef(paramName = "surveyResponseId", fromField = "dataResource.relatedDetailId"), @ParameterDef(paramName = "dataResourceId", fromField = "dataResource.dataResourceId"), @ParameterDef(paramName = "rootContentId")})),
            @FormField(name = "objectInfo", mapName = "dataResource", useWhen = "\"DOCUMENT\".equals(contentTypeId) && mimeTypeId!=null && !\"application/vnd.ofbiz.survey\".equals(mimeTypeId)", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditChildCompDoc {}

    @Form(
        name = "ViewChildCompDoc",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        defaultMapName = "contentAssoc",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentName", mapName = "itemContent", display = @DisplayField),
            @FormField(name = "contentTypeId", mapName = "itemContent", hidden = @HiddenField),
            @FormField(name = "instanceOfContentId", mapName = "itemContent", useWhen = "contentTypeId.equals(\"DOCUMENT\")", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentIdTo", title = "${uiLabelMap.ContentCompDocParentContentId}", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName} [${contentId}]")),
            @FormField(name = "contentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "sequenceNum", title = "${uiLabelMap.CommonSequenceNum}", display = @DisplayField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", title = "${uiLabelMap.ContentDocumentType}", displayEntity = @DisplayEntityField(entityName = "MimeType", keyFieldName = "mimeTypeId", description = "${description}")),
            @FormField(name = "relatedDetailId", mapName = "dataResource", title = "${uiLabelMap.ContentSurvey}", useWhen = "dataResource!=null&&\"SURVEY\".equals(dataResource.getString(\"dataResourceTypeId\"))", display = @DisplayField),
            @FormField(name = "relatedDetailId", mapName = "dataResource", title = "${uiLabelMap.ContentSurveyResponse}", useWhen = "dataResource!=null&&\"SURVEY_RESPONSE\".equals(dataResource.getString(\"dataResourceTypeId\"))&&dataResource.get(\"relatedDetailId\")!=null", display = @DisplayField)
        }
    )
    public interface ViewChildCompDoc {}

    @Form(
        name = "EditContentRevisionAndItem",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "updateContentRevisionAndItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentRevision"),
            @AutoFieldsEntity(entityName = "ContentRevisionItem")
        },
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", text = @TextField),
            @FormField(name = "contentRevisionSeqId", hidden = @HiddenField),
            @FormField(name = "committedByPartyId", widgetStyle = "inputBox"),
            @FormField(name = "comments", widgetStyle = "inputBox"),
            @FormField(name = "itemContentId", widgetStyle = "inputBox"),
            @FormField(name = "oldDataResourceId", widgetStyle = "inputBox"),
            @FormField(name = "newDataResourceId", widgetStyle = "inputBox"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditContentRevisionAndItem {}

    @Form(
        name = "UploadCompDocContent",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.UPLOAD,
        target = "uploadCompDocContent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "caContentIdTo", mapName = "contentAssoc", entryName = "contentIdTo", hidden = @HiddenField),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "COMPDOC_PART")),
            @FormField(name = "caFromDate", mapName = "contentAssoc", entryName = "fromDate", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "contentAssoc", hidden = @HiddenField),
            @FormField(name = "dataResourceId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "imageData", title = "${uiLabelMap.ContentFile}", file = @FileField),
            @FormField(name = "rootContentId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface UploadCompDocContent {}

    @Form(
        name = "UploadCompDocPdf2Survey",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        type = FormType.UPLOAD,
        target = "persistCompDocPdf2Survey",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "caContentIdTo", mapName = "contentAssoc", entryName = "contentIdTo", hidden = @HiddenField),
            @FormField(name = "caContentAssocTypeId", hidden = @HiddenField(value = "COMPDOC_PART")),
            @FormField(name = "caFromDate", mapName = "contentAssoc", entryName = "fromDate", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "contentAssoc", hidden = @HiddenField),
            @FormField(name = "dataResourceId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "pdfName", title = "${uiLabelMap.ContentPDFSurveyName}", text = @TextField),
            @FormField(name = "imageData", title = "${uiLabelMap.ContentPDFFilePath}", file = @FileField(size = 60)),
            @FormField(name = "rootContentId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface UploadCompDocPdf2Survey {}

    @Form(
        name = "EditCompDocTextContent",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "uploadCompDocContent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "rootContentId", mapName = "dummy", hidden = @HiddenField),
            @FormField(name = "caContentIdTo", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "caContentAssocTypeId", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "caFromDate", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "content", hidden = @HiddenField),
            @FormField(name = "dataResourceId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "textData", title = "${uiLabelMap.FormFieldTitle_textDataTitle}", textarea = @TextareaField(rows = 30)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditCompDocTextContent {}

    @Form(
        name = "UploadCompDocSurveyId",
        location = "component://content/widget/compdoc/CompDocForms.xml",
        target = "uploadCompDocContent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "rootContentId", mapName = "dummy", hidden = @HiddenField),
            @FormField(name = "caContentIdTo", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "caContentAssocTypeId", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "caFromDate", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "content", hidden = @HiddenField),
            @FormField(name = "dataResourceId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "mimeTypeId", mapName = "dataResource", hidden = @HiddenField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentSurveySurveyId}", lookup = @LookupField(targetFormName = "LookupSurvey")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface UploadCompDocSurveyId {}

}
