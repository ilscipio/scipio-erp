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
public class ContentContentForms {

    @Form(
        name = "FindContent",
        location = "component://content/widget/content/ContentForms.xml",
        target = "findContent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "contentId", textFind = @TextFindField),
            @FormField(name = "contentName", position = 2, textFind = @TextFindField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "localeString", position = 2, lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", keyFieldName = "contentTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "mimeTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "ownerContentId", position = 2, lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createdByUserLogin", position = 2, lookup = @LookupField(targetFormName = "LookupUserLoginAndPartyDetails")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindContent {}

    @Form(
        name = "ListContent",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "findContent",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentName", sortField = true, displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "editContent", description = "[${contentId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "description", sortField = true, display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "localeString", sortField = true, displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "contentTypeId", sortField = true, displayEntity = @DisplayEntityField(entityName = "ContentType")),
            @FormField(name = "mimeTypeId", sortField = true, displayEntity = @DisplayEntityField(entityName = "MimeType")),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", useWhen = "dataResourceId==null", sortField = true, display = @DisplayField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", useWhen = "dataResourceId!=null", sortField = true, displayEntity = @DisplayEntityField(entityName = "DataResource", description = "${dataResourceName}", subHyperlink = @SubHyperlink(target = "EditDataResource", description = "[${dataResourceId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "dataResourceId")}))),
            @FormField(name = "ownerContentId", useWhen = "ownerContentId==null", sortField = true, display = @DisplayField),
            @FormField(name = "ownerContentId", useWhen = "ownerContentId!=null", sortField = true, displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "editContent", description = "[${ownerContentId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contentId", value = "${ownerContentId}")}))),
            @FormField(name = "createdByUserLogin", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Content"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "findContentSearchResults")
        }
    )
    public interface ListContent {}

    @Form(
        name = "LookupContent",
        location = "component://content/widget/content/ContentForms.xml",
        target = "LookupContent",
        extendsForm = "FindContent",
        headerRowStyle = "header-row"
    )
    public interface LookupContent {}

    @Form(
        name = "ListLookupContent",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupContent",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contentId}')", urlMode = UrlMode.PLAIN, description = "${contentId}", alsoHidden = false)),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "localeString", displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}[${countryCode}]")),
            @FormField(name = "contentTypeId", title = "${uiLabelMap.ContentType}", displayEntity = @DisplayEntityField(entityName = "ContentType")),
            @FormField(name = "mimeTypeId", title = "${uiLabelMap.ContentMimeType}", displayEntity = @DisplayEntityField(entityName = "MimeType")),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.FormFieldTitle_dataResourceTitle}", useWhen = "dataResourceId==null", display = @DisplayField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.FormFieldTitle_dataResourceTitle}", useWhen = "dataResourceId!=null", displayEntity = @DisplayEntityField(entityName = "DataResource", keyFieldName = "dataResourceId", description = "${dataResourceName}")),
            @FormField(name = "ownerContentId", title = "${uiLabelMap.FormFieldTitle_ownerContentId}", useWhen = "ownerContentId==null", display = @DisplayField),
            @FormField(name = "ownerContentId", title = "${uiLabelMap.FormFieldTitle_ownerContentId}", useWhen = "ownerContentId!=null", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}"))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "contentId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Content"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupContent {}

    @Form(
        name = "EditContent",
        location = "component://content/widget/content/ContentForms.xml",
        target = "updateContent",
        defaultMapName = "currentValue",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Content")
        },
        fields = {
            @FormField(name = "contentId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "currentValue!=null", display = @DisplayField),
            @FormField(name = "contentId", useWhen = "currentValue==null&&contentId==null", text = @TextField),
            @FormField(name = "contentId", useWhen = "currentValue==null&&contentId!=null", display = @DisplayField(description = "${uiLabelMap.CommonCannotBeFound}: [${contentId}]", alsoHidden = false)),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", keyFieldName = "contentTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", useWhen = "dataResourceId != null", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", useWhen = "dataResourceId == null ", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "templateDataResourceId", useWhen = "templateDataResourceId != null", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "templateDataResourceId", useWhen = "templateDataResourceId == null", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "ownerContentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "decoratorContentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "localeString", lookup = @LookupField(targetFormName = "LookupLocale")),
            @FormField(name = "mimeTypeId", encodeOutput = false, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId} - ${description}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "characterSetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CharacterSet", description = "${description}", keyFieldName = "characterSetId"))),
            @FormField(name = "statusId", useWhen = "currentValue==null", ignored = @IgnoredField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "currentValue!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${currentValue.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", useWhen = "currentValue==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "currentValue!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createContent")
        },
        actions = @FormActions(set = {@SetAction(field = "dataResourceId", fromField = "currentValue.dataResourceId"), @SetAction(field = "templateDataResourceId", fromField = "currentValue.templateDataResourceId")}, entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditContent {}

    @Form(
        name = "mruLookupContent",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "mruList",
        defaultEntityName = "Content",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contentId}')", urlMode = UrlMode.PLAIN, description = "${contentId}", alsoHidden = false)),
            @FormField(name = "contentName", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        }
    )
    public interface mruLookupContent {}

    @Form(
        name = "EditContentAssoc",
        location = "component://content/widget/content/ContentForms.xml",
        target = "updateContentAssoc",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentAssoc")
        },
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentIdTo", display = @DisplayField),
            @FormField(name = "contentAssocTypeId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ContentAssoc", useCache = true)})
    )
    public interface EditContentAssoc {}

    @Form(
        name = "AddContentAssoc",
        location = "component://content/widget/content/ContentForms.xml",
        target = "createContentAssoc",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentAssoc")
        },
        fields = {
            @FormField(name = "contentId", display = @DisplayField(description = "${contentId}")),
            @FormField(name = "contentIdTo", mapName = "currentValue", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "contentAssocTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentAssocType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "metaDataPredicateId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MetaDataPredicate", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "createdDate", hidden = @HiddenField),
            @FormField(name = "createdByUserLogin", hidden = @HiddenField),
            @FormField(name = "lastModifiedDate", hidden = @HiddenField),
            @FormField(name = "lastModifiedByUserLogin", hidden = @HiddenField)
        }
    )
    public interface AddContentAssoc {}

    @Form(
        name = "ListContentAssocFrom",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "updateContentAssoc",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentIdTo", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName},${description}", subHyperlink = @SubHyperlink(target = "EditContentAssoc", description = "${contentIdTo}", parameters = {@ParameterDef(paramName = "contentId", fromField = "contentIdTo")}))),
            @FormField(name = "contentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType", description = "${description}")),
            @FormField(name = "mapKey", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(description = "${groovy:fromDate.toString().substring(0,10)}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentAssoc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentIdTo"), @ParameterDef(paramName = "contentAssocTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListContentAssocFrom {}

    @Form(
        name = "ListContentAssocTo",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "EditContentAssoc",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", displayEntity = @DisplayEntityField(entityName = "Content", description = "${contentName},${description}", subHyperlink = @SubHyperlink(target = "EditContentAssoc", description = "${contentId}", parameters = {@ParameterDef(paramName = "contentId")}))),
            @FormField(name = "contentIdTo", hidden = @HiddenField),
            @FormField(name = "contentAssocTypeId", displayEntity = @DisplayEntityField(entityName = "ContentAssocType", description = "${description}")),
            @FormField(name = "mapKey", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(description = "${groovy:fromDate.toString().substring(0,10)}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentAssoc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentIdTo"), @ParameterDef(paramName = "contentAssocTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListContentAssocTo {}

    @Form(
        name = "AddContentRole",
        location = "component://content/widget/content/ContentForms.xml",
        target = "addContentRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentRole")
        },
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentRole {}

    @Form(
        name = "EditContentRole",
        location = "component://content/widget/content/ContentForms.xml",
        target = "Edit${contentRoleTarget}ContentRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentRole")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditContentRole {}

    @Form(
        name = "ListContentRole",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "update${contentRoleTarget}ContentRole",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", widgetStyle = "${styles.link_nav_info_date}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "remove${contentRoleTarget}ContentRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListContentRole {}

    @Form(
        name = "AddContentPurpose",
        location = "component://content/widget/content/ContentForms.xml",
        target = "addContentPurpose",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentPurpose")
        },
        fields = {
            @FormField(name = "contentId", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "contentPurposeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentPurposeType", description = "${description}", keyFieldName = "contentPurposeTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentPurpose {}

    @Form(
        name = "ListContentPurpose",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "updateContentPurpose",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentPurpose", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentPurpose", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentPurposeTypeId")}))
        }
    )
    public interface ListContentPurpose {}

    @Form(
        name = "AddContentAttribute",
        location = "component://content/widget/content/ContentForms.xml",
        target = "addContentAttribute",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentAttribute")
        },
        fields = {
            @FormField(name = "contentId", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentAttribute {}

    @Form(
        name = "ListContentAttribute",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "updateContentAttribute",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentAttribute", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "attrValue", widgetStyle = "${styles.link_nav_info_text}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentAttribute", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "attrName")}))
        }
    )
    public interface ListContentAttribute {}

    @Form(
        name = "AddContentMetaData",
        location = "component://content/widget/content/ContentForms.xml",
        target = "addContentMetaData",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentMetaData")
        },
        fields = {
            @FormField(name = "contentId", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_find}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "metaDataPredicateId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MetaDataPredicate", description = "${description}", keyFieldName = "metaDataPredicateId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentMetaData {}

    @Form(
        name = "ListWebSites",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "webSites",
        paginate = "true",
        paginateTarget = "ListWebSite",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "webSiteId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav} ${styles.action_update}", sortField = true, hyperlink = @HyperlinkField(target = "EditWebSite", description = "${webSiteId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "webSiteId")})),
            @FormField(name = "siteName", title = "${uiLabelMap.CommonName}", sortField = true, display = @DisplayField),
            @FormField(name = "httpHost", sortField = true, display = @DisplayField),
            @FormField(name = "webSiteContentTypeId", displayEntity = @DisplayEntityField(entityName = "WebSiteContentType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        }
    )
    public interface ListWebSites {}

    @Form(
        name = "ListContentMetaData",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "updateContentMetaData",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentMetaData", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "metaDataPredicateId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentMetaData", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "metaDataPredicateId")}))
        }
    )
    public interface ListContentMetaData {}

    @Form(
        name = "mruLookupPerson",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "mruList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "firstName", title = " ", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "lastName", title = " ", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField)
        }
    )
    public interface mruLookupPerson {}

    @Form(
        name = "mruLookupPartyAndUserLoginAndPerson",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        listName = "mruList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}', '${userLoginId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "firstName", title = " ", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "lastName", title = " ", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField)
        }
    )
    public interface mruLookupPartyAndUserLoginAndPerson {}

    @Form(
        name = "AddWorkEffortContent",
        location = "component://content/widget/content/ContentForms.xml",
        target = "createWorkEffortContent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createWorkEffortContent")
        },
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.contentId}")),
            @FormField(name = "workEffortId", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "workEffortContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WorkEffortContentType", description = "${description}", keyFieldName = "workEffortContentTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortContent {}

    @Form(
        name = "ListWorkEffortContents",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "updateWorkEffortContent",
        listName = "workEffortContents",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWorkEffortContent", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "workEffortId", displayEntity = @DisplayEntityField(entityName = "WorkEffort", keyFieldName = "workEffortId", description = "${workEffortName}", subHyperlink = @SubHyperlink(target = "/workeffort/control/EditWorkEffort", description = "${workEffortId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "workEffortId")}))),
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "workEffortContentTypeId", displayEntity = @DisplayEntityField(entityName = "WorkEffortContentType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortContent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortContentTypeId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "contentId")}))
        }
    )
    public interface ListWorkEffortContents {}

    @Form(
        name = "AddDocument",
        location = "component://content/widget/content/ContentForms.xml",
        target = "addDocumentToTree",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentIdTo", hidden = @HiddenField),
            @FormField(name = "contentId", title = "${uiLabelMap.ContentRoot}", display = @DisplayField(description = "${content.contentName}[${content.contentId}]")),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contentAssocTypeId", hidden = @HiddenField(value = "SUB_CONTENT")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "contentAssocPredicateId", text = @TextField),
            @FormField(name = "dataSourceId", lookup = @LookupField(targetFormName = "LookupDataResource")),
            @FormField(name = "mapKey", text = @TextField),
            @FormField(name = "upperCoordinate", text = @TextField),
            @FormField(name = "leftCoordinate", text = @TextField),
            @FormField(name = "metaDataPredicateId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MetaDataPredicate", description = "${description}"))),
            @FormField(name = "submit", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "Content", valueField = "content")})
    )
    public interface AddDocument {}

    @Form(
        name = "ViewContentDetail",
        location = "component://content/widget/content/ContentForms.xml",
        defaultMapName = "lookupContentDetail",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contentId}')", urlMode = UrlMode.PLAIN, description = "${contentId}", alsoHidden = false)),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "contentTypeId", display = @DisplayField),
            @FormField(name = "ownerContentId", display = @DisplayField),
            @FormField(name = "mimeTypeId", display = @DisplayField),
            @FormField(name = "select", title = " ", useWhen = "contentId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contentId}')", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSelect}"))
        }
    )
    public interface ViewContentDetail {}

    @Form(
        name = "EditShowContentPortlet",
        location = "component://content/widget/content/ContentForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "contentId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Content", description = "${contentName}", constraints = {@EntityConstraint(name = "contentTypeId", value = "DOCUMENT")}, orderBy = {@EntityOrderBy(fieldName = "contentName")}))),
            @FormField(name = "saveAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditShowContentPortlet {}

    @Form(
        name = "AddContentKeyword",
        location = "component://content/widget/content/ContentForms.xml",
        target = "createContentKeyword",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentKeyword")
        },
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField(value = "${parameters.contentId}")),
            @FormField(name = "keyword", title = "${uiLabelMap.ContentKeyword}*", text = @TextField(size = 10)),
            @FormField(name = "relevancyWeight", title = "${uiLabelMap.ProductWeight}", text = @TextField(size = 5)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentKeyword {}

    @Form(
        name = "ListContentKeywords",
        location = "component://content/widget/content/ContentForms.xml",
        type = FormType.LIST,
        target = "deleteContentKeyword",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ContentKeyword", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListContentKeywords {}

}
