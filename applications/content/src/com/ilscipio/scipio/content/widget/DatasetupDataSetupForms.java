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
public class DatasetupDataSetupForms {

    @Form(
        name = "AddDataResourceType",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addDataResourceType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createDataResourceType")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceType {}

    @Form(
        name = "UpdateDataResourceType",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateDataResourceType",
        listName = "contentList",
        paginateTarget = "EditDataResourceType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateDataResourceType")
        },
        fields = {
            @FormField(name = "dataResourceTypeId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataResourceType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceTypeId")}))
        }
    )
    public interface UpdateDataResourceType {}

    @Form(
        name = "AddDataResourceTypeAttr",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addDataResourceTypeAttr",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createDataResourceTypeAttr")
        },
        fields = {
            @FormField(name = "dataResourceTypeId", entityName = "DataResource", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataResourceType", description = "${description}", keyFieldName = "dataResourceTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataResourceTypeAttr {}

    @Form(
        name = "UpdateDataResourceTypeAttr",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateDataResourceTypeAttr",
        listName = "contentList",
        paginateTarget = "EditDataResourceTypeAttr",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createDataResourceTypeAttr")
        },
        fields = {
            @FormField(name = "dataResourceTypeId", display = @DisplayField),
            @FormField(name = "attrName", title = "${uiLabelMap.ContentAttributeName}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataResourceTypeAttr", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataResourceTypeId"), @ParameterDef(paramName = "attrName")}))
        }
    )
    public interface UpdateDataResourceTypeAttr {}

    @Form(
        name = "AddCharacterSet",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addCharacterSet",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCharacterSet")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCharacterSet {}

    @Form(
        name = "UpdateCharacterSet",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateCharacterSet",
        listName = "contentList",
        paginateTarget = "EditCharacterSet",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateCharacterSet")
        },
        fields = {
            @FormField(name = "characterSetId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeCharacterSet", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "characterSetId")}))
        }
    )
    public interface UpdateCharacterSet {}

    @Form(
        name = "AddDataCategory",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addDataCategory",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createDataCategory")
        },
        fields = {
            @FormField(name = "parentCategoryId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataCategory", description = "${categoryName}", keyFieldName = "dataCategoryId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddDataCategory {}

    @Form(
        name = "UpdateDataCategory",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateDataCategory",
        listName = "dataCategoryList",
        paginateTarget = "EditDataCategory",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateDataCategory")
        },
        fields = {
            @FormField(name = "dataCategoryId", display = @DisplayField),
            @FormField(name = "parentCategoryId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "dataCategoryList", keyName = "dataCategoryId", description = "${categoryName}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeDataCategory", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataCategoryId")}))
        }
    )
    public interface UpdateDataCategory {}

    @Form(
        name = "AddFileExtension",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addFileExtension",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFileExtension")
        },
        fields = {
            @FormField(name = "mimeTypeId", title = " ", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "MimeType", description = "${description}", keyFieldName = "mimeTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFileExtension {}

    @Form(
        name = "UpdateFileExtension",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateFileExtension",
        listName = "contentList",
        paginateTarget = "EditFileExtension",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFileExtension")
        },
        fields = {
            @FormField(name = "fileExtensionId", display = @DisplayField),
            @FormField(name = "mimeTypeId", title = " ", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "MimeType", description = "${description}", keyFieldName = "mimeTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFileExtension", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fileExtensionId")}))
        }
    )
    public interface UpdateFileExtension {}

    @Form(
        name = "AddMetaDataPredicate",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addMetaDataPredicate",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createMetaDataPredicate")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddMetaDataPredicate {}

    @Form(
        name = "UpdateMetaDataPredicate",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateMetaDataPredicate",
        listName = "contentList",
        paginateTarget = "EditMetaDataPredicate",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateMetaDataPredicate")
        },
        fields = {
            @FormField(name = "metaDataPredicateId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeMetaDataPredicate", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "metaDataPredicateId")}))
        }
    )
    public interface UpdateMetaDataPredicate {}

    @Form(
        name = "AddMimeType",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "addMimeType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createMimeType")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddMimeType {}

    @Form(
        name = "UpdateMimeType",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateMimeType",
        listName = "contentList",
        paginateTarget = "EditMimeType",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateMimeType")
        },
        fields = {
            @FormField(name = "mimeTypeId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeMimeType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "mimeTypeId")}))
        }
    )
    public interface UpdateMimeType {}

    @Form(
        name = "CreateMimeTypeHtmlTemplate",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        target = "createMimeTypeHtmlTemplate",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createMimeTypeHtmlTemplate")
        },
        fields = {
            @FormField(name = "mimeTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "MimeType", description = "${description}", keyFieldName = "mimeTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateMimeTypeHtmlTemplate {}

    @Form(
        name = "UpdateMimeTypeHtmlTemplate",
        location = "component://content/widget/datasetup/DataSetupForms.xml",
        type = FormType.LIST,
        target = "updateMimeTypeHtmlTemplate",
        listName = "contentList",
        paginateTarget = "EditMimeTypeHtmlTemplate",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateMimeTypeHtmlTemplate")
        },
        fields = {
            @FormField(name = "mimeTypeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editMimeType", description = "${mimeTypeId}", parameters = {@ParameterDef(paramName = "mimeTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeMimeTypeHtmlTemplate", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "mimeTypeId"), @ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateMimeTypeHtmlTemplate {}

}
