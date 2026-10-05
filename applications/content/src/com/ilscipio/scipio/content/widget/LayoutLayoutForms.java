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
public class LayoutLayoutForms {

    @Form(
        name = "findLayout",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "FindLayout",
        defaultEntityName = "Content",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "DOCUMENT")),
            @FormField(name = "contentId", textFind = @TextFindField),
            @FormField(name = "contentName", textFind = @TextFindField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "createdByUserLogin", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "createdDate", dateFind = @DateFindField),
            @FormField(name = "lastModifiedByUserLogin", lookup = @LookupField(targetFormName = "LookupParty")),
            @FormField(name = "lastModifiedDate", dateFind = @DateFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findLayout {}

    @Form(
        name = "EditLayout",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "updateLayoutSubContent",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "TEMPLATE_MASTER")),
            @FormField(name = "drDataResourceTypeId", display = @DisplayField),
            @FormField(name = "drMimeTypeId", display = @DisplayField),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.ContentFilePath}", display = @DisplayField)
        }
    )
    public interface EditLayout {}

    @Form(
        name = "AddLayout",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "createLayoutSubContent",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", text = @TextField),
            @FormField(name = "drDataResourceTypeId", dropDown = @DropDownField(options = {@Option(key = "LOCAL_FILE", description = "${uiLabelMap.ContentAbsoluteFile}"), @Option(key = "OFBIZ_FILE", description = "${uiLabelMap.ContentFileRelToOFBizHome}"), @Option(key = "CONTEXT_FILE", description = "${uiLabelMap.ContentFileRelToWebappRoot}"), @Option(key = "ELECTRONIC_TEXT", description = "${uiLabelMap.ContentDataBaseText}")})),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}")})),
            @FormField(name = "drDataResourceTypeId", dropDown = @DropDownField(options = {@Option(key = "LOCAL_FILE", description = "${uiLabelMap.ContentAbsoluteFile}"), @Option(key = "OFBIZ_FILE", description = "${uiLabelMap.ContentFileRelToOFBizHome}"), @Option(key = "CONTEXT_FILE", description = "${uiLabelMap.ContentFileRelToWebappRoot}"), @Option(key = "ELECTRONIC_TEXT", description = "${uiLabelMap.ContentDataBaseText}")})),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}")})),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.ContentFilePath}", text = @TextField),
            @FormField(name = "textData", title = "${uiLabelMap.ContentText}", idName = "textData", textarea = @TextareaField(cols = 80, rows = 24)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddLayout {}

    @Form(
        name = "listFindLayout",
        location = "component://content/widget/layout/LayoutForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "ContentDataResourceView",
        paginateTarget = "FindLayout",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditLayout", description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "drDataResourceId")})),
            @FormField(name = "dummy", mapName = "dummy", title = "${uiLabelMap.ContentClip}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "clipFindLayout", description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "drDataResourceId"), @ParameterDef(paramName = "viewSize"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "entityName")})),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drObjectInfo", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "requestParameters.contentTypeId", value = "DOCUMENT")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ContentDataResourceView"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listFindLayout {}

    @Form(
        name = "listListLayout",
        location = "component://content/widget/layout/LayoutForms.xml",
        type = FormType.LIST,
        listName = "layoutList",
        defaultEntityName = "ContentAssocDataResourceViewFrom",
        paginateTarget = "FindLayout",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditLayout", description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "drDataResourceId")})),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drObjectInfo", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeLayout", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "contentIdTo", fromField = "caContentIdTo"), @ParameterDef(paramName = "contentAssocTypeId", fromField = "caContentAssocTypeId"), @ParameterDef(paramName = "fromDate", fromField = "caFromDate")}))
        }
    )
    public interface listListLayout {}

    @Form(
        name = "LayoutSubContentMaster",
        location = "component://content/widget/layout/LayoutForms.xml",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId"),
            @FormField(name = "contentTypeIdDisplay", mapName = "dummy", title = "${uiLabelMap.ContentType}", position = 2, display = @DisplayField(description = "DOCUMENT")),
            @FormField(name = "contentIdToDisplay", mapName = "dummy", title = "${uiLabelMap.ContentParent}", position = 3, display = @DisplayField(description = "${contentIdTo}")),
            @FormField(name = "mapKeyDisplay", mapName = "dummy", title = "${uiLabelMap.ContentMapKey}", position = 4, display = @DisplayField(description = "${mapKey}")),
            @FormField(name = "contentTypeId", hidden = @HiddenField(value = "DOCUMENT")),
            @FormField(name = "drDataResourceId", title = "${uiLabelMap.FormFieldTitle_dataResourceId}", display = @DisplayField),
            @FormField(name = "contentName", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "mapKey", hidden = @HiddenField(value = "${mapKey}"))
        }
    )
    public interface LayoutSubContentMaster {}

    @Form(
        name = "EditLayoutHtml",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "updateLayoutHtml",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "drDataResourceTypeId", display = @DisplayField(description = "ELECTRONIC_TEXT")),
            @FormField(name = "drMimeTypeId", display = @DisplayField(description = "text/plain")),
            @FormField(name = "textData", title = "${uiLabelMap.ContentText}", idName = "textData", textarea = @TextareaField(cols = 80, rows = 24)),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createLayoutHtml")
        }
    )
    public interface EditLayoutHtml {}

    @Form(
        name = "EditLayoutText",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "updateLayoutText",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "drDataResourceTypeId", display = @DisplayField(description = "ELECTRONIC_TEXT")),
            @FormField(name = "drMimeTypeId", display = @DisplayField(description = "text/plain")),
            @FormField(name = "textData", title = "${uiLabelMap.ContentText}", textarea = @TextareaField(cols = 80, rows = 24)),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createLayoutText")
        }
    )
    public interface EditLayoutText {}

    @Form(
        name = "EditLayoutImage",
        location = "component://content/widget/layout/LayoutForms.xml",
        type = FormType.UPLOAD,
        target = "updateLayoutImage",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "drDataResourceTypeId", display = @DisplayField(description = "IMAGE_OBJECT")),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}")})),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.FormFieldTitle_imageFileName}", display = @DisplayField(description = "${currentValue.drObjectInfo}")),
            @FormField(name = "imageData", mapName = "dummy", entityName = "ImageDataResource", file = @FileField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditLayoutImage {}

    @Form(
        name = "AddLayoutImage",
        location = "component://content/widget/layout/LayoutForms.xml",
        type = FormType.UPLOAD,
        target = "createLayoutImage",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "drDataResourceTypeId", hidden = @HiddenField(value = "IMAGE_OBJECT")),
            @FormField(name = "drDataResourceTypeIdDisplay", title = "${uiLabelMap.FormFieldTitle_drDataResourceTypeId}", display = @DisplayField(description = "IMAGE_OBJECT")),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "image/gif", description = "${uiLabelMap.ContentGIF}"), @Option(key = "image/jpeg", description = "${uiLabelMap.ContentJPEG}"), @Option(key = "image/png", description = "${uiLabelMap.ContentPNG}"), @Option(key = "image/tiff", description = "${uiLabelMap.ContentTIFF}")})),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.FormFieldTitle_imageFileName}", display = @DisplayField(description = "${currentValue.drObjectInfo}")),
            @FormField(name = "imageData", entityName = "ImageDataResource", file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddLayoutImage {}

    @Form(
        name = "EditLayoutUrl",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "updateLayoutUrl",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "drDataResourceTypeId", display = @DisplayField(description = "URL_RESOURCE")),
            @FormField(name = "drMimeTypeId", display = @DisplayField(description = "text/plain")),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.ContentUrl}", text = @TextField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createLayoutUrl")
        }
    )
    public interface EditLayoutUrl {}

    @Form(
        name = "ListRelatedLayouts",
        location = "component://content/widget/layout/LayoutForms.xml",
        type = FormType.LIST,
        listName = "entityList",
        defaultEntityName = "ContentDataResourceView",
        paginateTarget = "FindLayout",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditLayout", description = "${contentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "drDataResourceId")})),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drObjectInfo", display = @DisplayField)
        }
    )
    public interface ListRelatedLayouts {}

    @Form(
        name = "EditLayoutSubContent",
        location = "component://content/widget/layout/LayoutForms.xml",
        target = "updateLayoutSubContent",
        defaultMapName = "currentValue",
        defaultEntityName = "SubContentDataResourceView",
        extendsForm = "LayoutSubContentMaster",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentIdToDisplay", title = "${uiLabelMap.ContentParent}", ignored = @IgnoredField),
            @FormField(name = "mapKeyDisplay", ignored = @IgnoredField),
            @FormField(name = "contentId", display = @DisplayField),
            @FormField(name = "contentIdTo", text = @TextField(defaultValue = "${parameters.contentIdTo}")),
            @FormField(name = "mapKey", text = @TextField(defaultValue = "${parameters.mapKey}")),
            @FormField(name = "drDataResourceTypeId", dropDown = @DropDownField(options = {@Option(key = "LOCAL_FILE", description = "${uiLabelMap.ContentAbsoluteFile}"), @Option(key = "OFBIZ_FILE", description = "${uiLabelMap.ContentFileRelToOFBizHome}"), @Option(key = "CONTEXT_FILE", description = "${uiLabelMap.ContentFileRelToWebappRoot}"), @Option(key = "ELECTRONIC_TEXT", description = "${uiLabelMap.ContentDataBaseText}")})),
            @FormField(name = "drDataTemplateTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataTemplateType", description = "${description}", keyFieldName = "dataTemplateTypeId"))),
            @FormField(name = "drMimeTypeId", dropDown = @DropDownField(options = {@Option(key = "text/html", description = "${uiLabelMap.ContentHtmlText}"), @Option(key = "text/plain", description = "${uiLabelMap.ContentPlainText}")})),
            @FormField(name = "drObjectInfo", title = "${uiLabelMap.ContentFilePath}", text = @TextField),
            @FormField(name = "textData", title = "${uiLabelMap.FormFieldTitle_textDataTitle}", idName = "textData", textarea = @TextareaField(cols = 80, rows = 24, defaultValue = "${context.textData}")),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "createdDate", position = 2, display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "currentValue==null", target = "createLayoutSubContent")
        }
    )
    public interface EditLayoutSubContent {}

}
