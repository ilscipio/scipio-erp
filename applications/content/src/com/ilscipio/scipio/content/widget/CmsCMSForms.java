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
public class CmsCMSForms {

    @Form(
        name = "findContent",
        location = "component://content/widget/cms/CMSForms.xml",
        target = "CMSContentFind",
        defaultEntityName = "ContentAssocDataResourceViewFrom",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "caContentIdTo", textFind = @TextFindField),
            @FormField(name = "caMapKey", textFind = @TextFindField),
            @FormField(name = "caContentAssocTypeId", textFind = @TextFindField),
            @FormField(name = "caFromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "contentId", textFind = @TextFindField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", textFind = @TextFindField),
            @FormField(name = "contentName", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findContent {}

    @Form(
        name = "listFindContent",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "/CMSContentFind",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "caContentIdTo", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAddContent", description = "${caContentIdTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "MASTER_contentId", fromField = "contentId"), @ParameterDef(paramName = "MASTER_drDataResourceId", fromField = "drDataResourceId"), @ParameterDef(paramName = "MASTER_caContentIdTo", fromField = "caContentIdTo"), @ParameterDef(paramName = "MASTER_caContentAssocTypeId", fromField = "caContentAssocTypeId"), @ParameterDef(paramName = "MASTER_caFromDate", fromField = "caFromDate"), @ParameterDef(paramName = "MASTER_caMapKey", fromField = "caMapKey")})),
            @FormField(name = "caMapKey", display = @DisplayField),
            @FormField(name = "caFromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "contentName", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "noConditionFind", value = "Y"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listFindContent {}

    @Form(
        name = "EditContent",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        target = "uploadContentAndImage",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "imageData", file = @FileField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", dropDown = @DropDownField(options = {@Option(key = "TEMPLATE_TEXT_ONLY", description = "${uiLabelMap.ContentTemplateTextOnly}"), @Option(key = "TEMPLATE_IMAGE_CENTERED", description = "${uiLabelMap.ContentTemplateImageCentered}"), @Option(key = "TEMPLATE_IMAGE_LEFT", description = "${uiLabelMap.ContentTemplateImageLeft}")})),
            @FormField(name = "ftlContentId", display = @DisplayField(description = "${ftlContentId}")),
            @FormField(name = "contentIdTo", position = 2, display = @DisplayField(description = "${contentIdTo}")),
            @FormField(name = "ownerContentId", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "description", text = @TextField(size = 60)),
            @FormField(name = "summaryData", title = "${uiLabelMap.ContentBlogSummary}", useWhen = "\"${summaryDataResourceTypeId}\".length()>0", idName = "summaryData", textarea = @TextareaField(cols = 80, rows = 8)),
            @FormField(name = "textData", idName = "textData", textarea = @TextareaField(rows = 20)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "statusList", keyName = "statusId", description = "${description}"))),
            @FormField(name = "privilegeEnumId", dropDown = @DropDownField(listOptions = @ListOptions(listName = "privilegeList", keyName = "enumId", description = "${description}"))),
            @FormField(name = "section", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "sectionList", keyName = "contentId", description = "${description}"))),
            @FormField(name = "topic", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "topicList", keyName = "contentId", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField),
            @FormField(name = "ftlContentId", hidden = @HiddenField),
            @FormField(name = "sumContentId", hidden = @HiddenField),
            @FormField(name = "txtContentId", hidden = @HiddenField),
            @FormField(name = "imgContentId", hidden = @HiddenField),
            @FormField(name = "ftlDataResourceId", hidden = @HiddenField),
            @FormField(name = "sumDataResourceId", hidden = @HiddenField),
            @FormField(name = "txtDataResourceId", hidden = @HiddenField),
            @FormField(name = "imgDataResourceId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", hidden = @HiddenField),
            @FormField(name = "contentPurposeString", hidden = @HiddenField(value = "${contentPurposeTypeId}")),
            @FormField(name = "targetOperationString", hidden = @HiddenField(value = "${targetOperation}")),
            @FormField(name = "nodeTrailCsv", hidden = @HiddenField(value = "${nodeTrailCsv}"))
        }
    )
    public interface EditContent {}

    @Form(
        name = "UpdateContentPurposeOperation",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.LIST,
        target = "updateContentPurposeOperation",
        listName = "contentList",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentPurposeOperation", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentPurposeOperation", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentPurposeTypeId"), @ParameterDef(paramName = "contentOperationId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "statusId"), @ParameterDef(paramName = "privilegeEnumId")}))
        }
    )
    public interface UpdateContentPurposeOperation {}

    @Form(
        name = "AddContentPurposeOperation",
        location = "component://content/widget/cms/CMSForms.xml",
        target = "addContentPurposeOperation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentPurposeOperation")
        },
        fields = {
            @FormField(name = "contentPurposeTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentPurposeType", description = "${description}", keyFieldName = "contentPurposeTypeId"))),
            @FormField(name = "contentOperationId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentOperation", description = "${description}", keyFieldName = "contentOperationId"))),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonNA}")}, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonNA}")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PUBLISH_STATUS")}))),
            @FormField(name = "privilegeEnumId", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonNA}")}, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "SUBSCRIPTION_TYPE")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentPurposeOperation {}

    @Form(
        name = "EditAddContentMaster",
        location = "component://content/widget/cms/CMSForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "currentValue", useWhen = "\"${currentValue.contentId}\".length()>0", display = @DisplayField),
            @FormField(name = "ownerContentId", title = "Owning Department", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "contentName", mapName = "currentValue", display = @DisplayField),
            @FormField(name = "textData", entryName = "summaryData", useWhen = "\"${textSource}\".length()>0 && \"${textSource}\".equals(\"summaryData\") ", idName = "textData", textarea = @TextareaField(cols = 80, rows = 20)),
            @FormField(name = "textData", entryName = "textData", useWhen = "\"${textSource}\".length()>0 && \"${textSource}\".equals(\"textData\") ", idName = "textData", textarea = @TextareaField(cols = 80, rows = 20)),
            @FormField(name = "privilegeEnumId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField),
            @FormField(name = "statusId", mapName = "currentValue", hidden = @HiddenField),
            @FormField(name = "contentAssocTypeId", hidden = @HiddenField(value = "${contentAssocTypeId}")),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "${contentTypeId}")),
            @FormField(name = "contentPurposeString", hidden = @HiddenField(value = "${contentPurposeTypeId}")),
            @FormField(name = "targetOperationString", hidden = @HiddenField(value = "${targetOperation}")),
            @FormField(name = "nodeTrailCsv", hidden = @HiddenField(value = "${nodeTrailCsv}")),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "masterContentId", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "mapKey", hidden = @HiddenField(value = "${mapKey}")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "${dataResourceTypeId}")),
            @FormField(name = "deactivateExisting", hidden = @HiddenField(value = "true"))
        }
    )
    public interface EditAddContentMaster {}

    @Form(
        name = "EditAddContent",
        location = "component://content/widget/cms/CMSForms.xml",
        target = "persistContent",
        extendsForm = "EditAddContentMaster",
        headerRowStyle = "header-row"
    )
    public interface EditAddContent {}

    @Form(
        name = "EditAddBioContent",
        location = "component://content/widget/cms/CMSForms.xml",
        target = "persistBioContent",
        extendsForm = "EditAddContentMaster",
        headerRowStyle = "header-row"
    )
    public interface EditAddBioContent {}

    @Form(
        name = "EditAddImageMaster",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", hidden = @HiddenField),
            @FormField(name = "imageData", file = @FileField),
            @FormField(name = "ownerContentId", title = "Owning Department", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "contentName", mapName = "currentValue", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField),
            @FormField(name = "statusId", mapName = "currentValue", hidden = @HiddenField),
            @FormField(name = "contentPurposeString", hidden = @HiddenField(value = "${contentPurposeTypeId}")),
            @FormField(name = "targetOperationString", hidden = @HiddenField(value = "${targetOperation}")),
            @FormField(name = "entityOperation", hidden = @HiddenField(value = "${entityOperation}")),
            @FormField(name = "nodeTrailCsv", hidden = @HiddenField(value = "${nodeTrailCsv}")),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "mapKey", hidden = @HiddenField(value = "${mapKey}")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "${dataResourceTypeId}")),
            @FormField(name = "ftlContentId", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "imgContentId", hidden = @HiddenField),
            @FormField(name = "ftlDataResourceId", hidden = @HiddenField),
            @FormField(name = "imgDataResourceId", hidden = @HiddenField),
            @FormField(name = "deactivateExisting", hidden = @HiddenField(value = "true"))
        }
    )
    public interface EditAddImageMaster {}

    @Form(
        name = "EditAddImage",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        target = "persistImage",
        extendsForm = "EditAddImageMaster",
        headerRowStyle = "header-row"
    )
    public interface EditAddImage {}

    @Form(
        name = "EditAddBioImage",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        target = "persistBioImage",
        extendsForm = "EditAddImageMaster",
        headerRowStyle = "header-row"
    )
    public interface EditAddBioImage {}

    @Form(
        name = "EditAddContentStuff",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        target = "persistContentStuff",
        defaultMapName = "currentValue",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentAssocTitle", mapName = "dummy", title = "${uiLabelMap.ContentAssoc}", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "caContentIdTo", useWhen = "\"${caContentIdTo}\".length()>0", display = @DisplayField),
            @FormField(name = "caContentIdTo", useWhen = "\"${caContentIdTo}\".length()==0", text = @TextField),
            @FormField(name = "caMapKey", useWhen = "\"${caMapKey}\".length()==0", position = 2, text = @TextField),
            @FormField(name = "caMapKey", useWhen = "\"${caMapKey}\".length()>0", position = 2, text = @TextField),
            @FormField(name = "caContentAssocTypeId", useWhen = "\"${caContentAssocTypeId}\".length()>0", display = @DisplayField),
            @FormField(name = "caContentAssocTypeId", useWhen = "\"${caContentAssocTypeId}\".length()==0", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentAssocType", description = "${description}", keyFieldName = "contentAssocTypeId"))),
            @FormField(name = "caContentAssocPredicateId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MetaDataPredicate", description = "${description}", keyFieldName = "metaDataPredicateId"))),
            @FormField(name = "caFromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "\"${caFromDate}\".length()>0", widgetStyle = "${styles.link_nav_info_date}", display = @DisplayField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "caFromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "\"${caFromDate}\".length()==0", widgetStyle = "${styles.link_nav_info_date}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}")),
            @FormField(name = "caThruDate", title = "${uiLabelMap.CommonThru}", useWhen = "\"${caThruDate}\".length()>0", widgetStyle = "${styles.link_nav_info_date}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "caThruDate", title = "${uiLabelMap.CommonThru}", useWhen = "\"${caThruDate}\".length()==0", widgetStyle = "${styles.link_nav_info_date}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "contentTitle", mapName = "dummy", title = "${uiLabelMap.ContentContent}", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "contentId", useWhen = "\"${currentValue.contentId}\".length()>0", display = @DisplayField),
            @FormField(name = "contentId", useWhen = "\"${currentValue.contentId}\".length()==0", text = @TextField),
            @FormField(name = "templateDataResourceId", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", keyFieldName = "contentTypeId"))),
            @FormField(name = "ownerContentId", position = 2, lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "description", position = 2, text = @TextField(size = 60)),
            @FormField(name = "mimeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "characterSetId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CharacterSet", description = "${description}", keyFieldName = "characterSetId"))),
            @FormField(name = "localeString", position = 3, text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PUBLISH_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "privilegeEnumId", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "privilegeList", keyName = "enumId", description = "${description}"))),
            @FormField(name = "dataResourceTitle", mapName = "dummy", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "drDataResourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}", keyFieldName = "dataResourceTypeId"))),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.ContentFilePath}", position = 2, text = @TextField),
            @FormField(name = "drDataTemplateTypeId", position = 3, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataTemplateType", description = "${description}", keyFieldName = "dataTemplateTypeId"))),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "drCharacterSetId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CharacterSet", description = "${description}", keyFieldName = "characterSetId"))),
            @FormField(name = "drLocaleString", position = 3, text = @TextField),
            @FormField(name = "drDataSourceId", text = @TextField),
            @FormField(name = "drDataCategoryId", position = 2, text = @TextField),
            @FormField(name = "textDataTitle", mapName = "dummy", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "textData", textarea = @TextareaField(cols = 80, rows = 20)),
            @FormField(name = "imageDataTitle", mapName = "dummy", titleStyle = "h1", display = @DisplayField),
            @FormField(name = "imageData", file = @FileField),
            @FormField(name = "contentPurposeString", hidden = @HiddenField(value = "${contentPurposeTypeId}")),
            @FormField(name = "targetOperationString", hidden = @HiddenField(value = "${targetOperation}")),
            @FormField(name = "nodeTrailCsv", hidden = @HiddenField(value = "${nodeTrailCsv}")),
            @FormField(name = "masterContentId", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "deactivateExisting", hidden = @HiddenField(value = "true")),
            @FormField(name = "_rowCount", hidden = @HiddenField(value = "1")),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField),
            @FormField(name = "MASTER_contentId", hidden = @HiddenField(value = "${MASTER_contentId}")),
            @FormField(name = "MASTER_caContentId", hidden = @HiddenField(value = "${MASTER_caContentId}")),
            @FormField(name = "MASTER_caContentIdTo", hidden = @HiddenField(value = "${MASTER_caContentIdTo}")),
            @FormField(name = "MASTER_caContentAssocTypeId", hidden = @HiddenField(value = "${MASTER_caContentAssocTypeId}")),
            @FormField(name = "MASTER_caFromDate", hidden = @HiddenField(value = "${MASTER_caFromDate}")),
            @FormField(name = "MASTER_drDataResource", hidden = @HiddenField(value = "${MASTER_drDataResource}"))
        }
    )
    public interface EditAddContentStuff {}

    @Form(
        name = "EditAddSubContentStuff",
        location = "component://content/widget/cms/CMSForms.xml",
        type = FormType.UPLOAD,
        target = "persistSubContentStuff",
        defaultMapName = "currentValue",
        extendsForm = "EditAddContentStuff",
        headerRowStyle = "header-row"
    )
    public interface EditAddSubContentStuff {}

}
