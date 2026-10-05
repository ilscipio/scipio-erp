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
public class ContentDataResourceForms {

    @Form(
        name = "FindDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "findDataResource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", textFind = @TextFindField),
            @FormField(name = "dataResourceName", textFind = @TextFindField),
            @FormField(name = "dataResourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}", keyFieldName = "dataResourceTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "mimeTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "localeString", position = 2, lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "createdByUserLogin", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "dataCategoryId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataCategory", description = "${categoryName}", orderBy = {@EntityOrderBy(fieldName = "categoryName")}))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindDataResource {}

    @Form(
        name = "ListDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "findDataResource",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataResourceId", hidden = @HiddenField),
            @FormField(name = "dataResourceName", sortField = true, displayEntity = @DisplayEntityField(entityName = "DataResource", keyFieldName = "dataResourceId", description = "${dataResourceName}", subHyperlink = @SubHyperlink(target = "EditDataResource", description = "[${dataResourceId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "dataResourceId")}))),
            @FormField(name = "dataResourceTypeId", sortField = true, displayEntity = @DisplayEntityField(entityName = "DataResourceType", description = "${description}")),
            @FormField(name = "mimeTypeId", sortField = true, displayEntity = @DisplayEntityField(entityName = "MimeType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "localeString", sortField = true, displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "createdByUserLogin", sortField = true, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "dataCategoryId", sortField = true, displayEntity = @DisplayEntityField(entityName = "DataCategory", description = "${categoryName}[${dataCategoryId}]"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "DataResource"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "findDataResourceSearchResults")
        }
    )
    public interface ListDataResource {}

    @Form(
        name = "LookupDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "LookupDataResource",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "DataResource", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        sortOrder = @SortOrder()
    )
    public interface LookupDataResource {}

    @Form(
        name = "ListLookupDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupDataResource",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${dataResourceId}')", urlMode = UrlMode.PLAIN, description = "${dataResourceId}", alsoHidden = false)),
            @FormField(name = "dataResourceName", display = @DisplayField),
            @FormField(name = "dataResourceTypeId", displayEntity = @DisplayEntityField(entityName = "DataResourceType")),
            @FormField(name = "mimeTypeId", displayEntity = @DisplayEntityField(entityName = "MimeType")),
            @FormField(name = "statusId", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "localeString", displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "createdByUserLogin", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}", alsoHidden = false)),
            @FormField(name = "dataCategoryId", displayEntity = @DisplayEntityField(entityName = "DataCategory", description = "${categoryName}[${dataCategoryId}]"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "DataResource"), @FieldMap(fieldName = "orderBy", value = "dataResourceId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupDataResource {}

    @Form(
        name = "mruLookupDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "mruList",
        defaultEntityName = "DataResource",
        oddRowStyle = "alternate-row",
        defaultWidgetStyle = "display",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${dataResourceId}')", urlMode = UrlMode.PLAIN, description = "${dataResourceId}", alsoHidden = false)),
            @FormField(name = "dataResourceName", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "dataCategoryId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField)
        }
    )
    public interface mruLookupDataResource {}

    @Form(
        name = "EditDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateDataResource",
        defaultMapName = "currentValue",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "DataResource", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "dataResourceId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "currentValue!=null", display = @DisplayField),
            @FormField(name = "dataResourceId", useWhen = "currentValue==null&&dataResourceId==null", text = @TextField),
            @FormField(name = "dataResourceId", useWhen = "currentValue==null&&dataResourceId!=null", display = @DisplayField(description = "${uiLabelMap.CommonCannotBeFound}: [${dataResourceId}]", alsoHidden = false)),
            @FormField(name = "dataResourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}", keyFieldName = "dataResourceTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dataTemplateTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataTemplateType", description = "${description}", keyFieldName = "dataTemplateTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", useWhen = "currentValue==null", ignored = @IgnoredField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "currentValue!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${currentValue.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "dataCategoryId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "dataCategoryList", keyName = "dataCategoryId", description = "${categoryName}"))),
            @FormField(name = "localeString", lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "mimeTypeId", encodeOutput = false, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "characterSetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CharacterSet", description = "${description}", keyFieldName = "characterSetId"))),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", useWhen = "currentValue==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "currentValue!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createDataResource")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditDataResource {}

    @Form(
        name = "AddDataResourceFromContent",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResourceAndAssocToContent",
        extendsForm = "AddDataResource",
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.contentId}")),
            @FormField(name = "templateDataResource", hidden = @HiddenField(value = "${parameters.templateDataResource}"))
        }
    )
    public interface AddDataResourceFromContent {}

    @Form(
        name = "ListContentsAssociatedToDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "contentRecords",
        oddRowStyle = "alternate-row",
        defaultWidgetStyle = "display",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/EditContent", description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId")})),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface ListContentsAssociatedToDataResource {}

    @Form(
        name = "DataResourceMaster",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResource",
        defaultMapName = "currentValue",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "DataResource", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}"),
            @FormField(name = "dataResourceTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}", keyFieldName = "dataResourceTypeId"))),
            @FormField(name = "dataTemplateTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataTemplateType", description = "${description}", keyFieldName = "dataTemplateTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "dataCategoryId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "dataCategoryList", keyName = "dataCategoryId", description = "${categoryName}"))),
            @FormField(name = "localeString", lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "mimeTypeId", encodeOutput = false, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "characterSetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CharacterSet", description = "${description}", keyFieldName = "characterSetId"))),
            @FormField(name = "createdDate", display = @DisplayField),
            @FormField(name = "lastModifiedDate", display = @DisplayField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField)
        }
    )
    public interface DataResourceMaster {}

    @Form(
        name = "AddDataResource",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResource",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUrl}"),
            @FormField(name = "mode", hidden = @HiddenField(value = "CREATE"))
        }
    )
    public interface AddDataResource {}

    @Form(
        name = "AddDataResourceText",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResourceAndText",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", text = @TextField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "dataResourceTypeIdDisplay", fieldName = "dataResourceTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "ELECTRONIC_TEXT", alsoHidden = false)),
            @FormField(name = "textData", idName = "textData", textarea = @TextareaField(cols = 120, rows = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceText {}

    @Form(
        name = "AddDataResourceUrl",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResourceUrl",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", text = @TextField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUrl}", text = @TextField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "URL_RESOURCE")),
            @FormField(name = "dataResourceTypeIdDisplay", fieldName = "dataResourceTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "URL_RESOURCE", alsoHidden = false)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceUrl {}

    @Form(
        name = "AddDataResourceUpload",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResourceUpload",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", display = @DisplayField(description = "${dataResource.dataResourceId}")),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "IMAGE_OBJECT")),
            @FormField(name = "dataResourceTypeIdDisplay", fieldName = "dataResourceTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "IMAGE_OBJECT", alsoHidden = false)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUploadedFile}", display = @DisplayField)
        }
    )
    public interface AddDataResourceUpload {}

    @Form(
        name = "EditDataResourceText",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateDataResourceText",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "ELECTRONIC_TEXT")),
            @FormField(name = "dataResourceTypeIdDisplay", fieldName = "dataResourceTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "ELECTRONIC_TEXT", alsoHidden = false)),
            @FormField(name = "textData", idName = "textData", textarea = @TextareaField(cols = 120, rows = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditDataResourceText {}

    @Form(
        name = "EditDataResourceUpload",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateDataResourceUpload",
        extendsForm = "DataResourceMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", hidden = @HiddenField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField(value = "IMAGE_OBJECT")),
            @FormField(name = "dataResourceTypeIdDisplay", fieldName = "dataResourceTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField(description = "IMAGE_OBJECT", alsoHidden = false)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUploadedFile}", display = @DisplayField)
        }
    )
    public interface EditDataResourceUpload {}

    @Form(
        name = "EditDataResourceUrl",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateDataResourceUrl",
        extendsForm = "AddDataResourceUrl",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUrl}", text = @TextField)
        }
    )
    public interface EditDataResourceUrl {}

    @Form(
        name = "ImageUpload",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.UPLOAD,
        target = "uploadImage",
        defaultMapName = "currentValue",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "dataResourceTypeId", hidden = @HiddenField),
            @FormField(name = "objectInfo", title = "${uiLabelMap.ContentUploadedFile}", display = @DisplayField),
            @FormField(name = "imageData", entityName = "ImageDataResource", title = "${uiLabelMap.ContentFile}", file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface ImageUpload {}

    @Form(
        name = "AddDataResourceAttribute",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "addDataResourceAttribute",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createDataResourceAttribute")
        },
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceAttribute {}

    @Form(
        name = "ListDataResourceAttribute",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        target = "updateDataResourceAttribute",
        listName = "dataResourceAttribute",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateDataResourceAttribute", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "attrValue", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataResourceAttribute", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "attrName")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName", value = "DataResourceAttribute"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListDataResourceAttribute {}

    @Form(
        name = "AddDataResourceRole",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "addDataResourceRole",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "DataResourceRole")
        },
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "partyId", title = " ", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceRole {}

    @Form(
        name = "ListDataResourceRole",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        target = "updateDataResourceRole",
        listName = "dataResourceRole",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateDataResourceRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", widgetStyle = "${styles.link_nav_info_date}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataResourceRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName", value = "DataResourceRole"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListDataResourceRole {}

    @Form(
        name = "AddDataResourceProductFeature",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "createDataResourceProductFeature",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductFeatureDataResource")
        },
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", title = "${uiLabelMap.ContentDataResourceId}", display = @DisplayField),
            @FormField(name = "productFeatureId", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceProductFeature {}

    @Form(
        name = "ListDataResourceProductFeature",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "productFeatureDataResource",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeatureDataResource", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataResourceProductFeature", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "productFeatureId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName", value = "ProductFeatureDataResource"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListDataResourceProductFeature {}

    @Form(
        name = "lookupProductFeature",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "LookupProductFeature",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeature", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "productFeatureTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productFeatureCategoryId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductFeatureCategory", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupProductFeature {}

    @Form(
        name = "listLookupProductFeature",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "entityList",
        paginateTarget = "LookupProductFeature",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductFeature", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productFeatureId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productFeatureId}')", urlMode = UrlMode.PLAIN, description = "${productFeatureId}", alsoHidden = false))
        }
    )
    public interface listLookupProductFeature {}

    @Form(
        name = "mruLookupProductFeature",
        location = "component://content/widget/content/DataResourceForms.xml",
        type = FormType.LIST,
        listName = "mruList",
        defaultEntityName = "ProductFeature",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${productFeatureId}')", urlMode = UrlMode.PLAIN, description = "${productFeatureId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.FormFieldTitle_contentName}", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "productFeatureTypeId", title = "${uiLabelMap.CommonType}", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField)
        }
    )
    public interface mruLookupProductFeature {}

    @Form(
        name = "EditElectronicText",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateElectronicText",
        defaultMapName = "electronicText",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "textData", widgetStyle = "${styles.link_nav_info_text}", textarea = @TextareaField(cols = 120, rows = 24)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "electronicText!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "electronicText==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "electronicText==null", target = "addElectronicText")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ElectronicText", valueField = "electronicText")})
    )
    public interface EditElectronicText {}

    @Form(
        name = "AddHtmlText",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "addHtmlText",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ElectronicText")
        },
        fields = {
            @FormField(name = "dataResourceId", mapName = "currentValue", title = "${uiLabelMap.ContentDataResourceId}", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "textData", idName = "textData", textarea = @TextareaField(cols = 120, rows = 24)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddHtmlText {}

    @Form(
        name = "EditHtmlText",
        location = "component://content/widget/content/DataResourceForms.xml",
        target = "updateHtmlText",
        defaultMapName = "electronicText",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ElectronicText")
        },
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "textData", idName = "textData", textarea = @TextareaField(cols = 120, rows = 20, visualEditorEnable = true)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "electronicText==null", target = "addHtmlText")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ElectronicText", valueField = "electronicText")})
    )
    public interface EditHtmlText {}

}
