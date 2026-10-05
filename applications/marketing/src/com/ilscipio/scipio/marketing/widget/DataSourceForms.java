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
package com.ilscipio.scipio.marketing.widget;

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
public class DataSourceForms {

    @Form(
        name = "EditDataSource",
        location = "component://marketing/widget/DataSourceForms.xml",
        target = "updateDataSource",
        defaultMapName = "dataSource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "dataSource!=null", display = @DisplayField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", useWhen = "dataSource==null&&dataSourceId==null", text = @TextField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${dataSourceId}]", useWhen = "dataSource==null&&dataSourceId!=null", display = @DisplayField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataSourceType", description = "${description}", keyFieldName = "dataSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "dataSource==null", target = "createDataSource")
        }
    )
    public interface EditDataSource {}

    @Form(
        name = "ListDataSource",
        location = "component://marketing/widget/DataSourceForms.xml",
        type = FormType.LIST,
        target = "ListDataSource",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditDataSource", description = "${dataSourceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataSourceId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", displayEntity = @DisplayEntityField(entityName = "DataSourceType")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteDataSource", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataSourceId")}))
        }
    )
    public interface ListDataSource {}

    @Form(
        name = "EditDataSourceType",
        location = "component://marketing/widget/DataSourceForms.xml",
        target = "updateDataSourceType",
        defaultMapName = "dataSourceType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "dataSourceType!=null", display = @DisplayField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", useWhen = "dataSourceType==null&&dataSourceTypeId==null", text = @TextField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${dataSourceTypeId}]", useWhen = "dataSourceType==null&&dataSourceTypeId!=null", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "dataSourceType==null", target = "createDataSourceType")
        }
    )
    public interface EditDataSourceType {}

    @Form(
        name = "FindDataSourceTypes",
        location = "component://marketing/widget/DataSourceForms.xml",
        target = "ListDataSourceTypes",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindDataSourceTypes {}

    @Form(
        name = "ListDataSourceType",
        location = "component://marketing/widget/DataSourceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditDataSourceType", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataSourceTypeId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteDataSourceType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "dataSourceTypeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "dataSourceTypeId"), @FieldMap(fieldName = "entityName", value = "DataSourceType"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListDataSourceType {}

    @Form(
        name = "LookupDataSource",
        location = "component://marketing/widget/DataSourceForms.xml",
        target = "LookupDataSource",
        defaultMapName = "dataSource",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataSourceType", description = "${description}", keyFieldName = "dataSourceTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface LookupDataSource {}

    @Form(
        name = "ListLookupDataSource",
        location = "component://marketing/widget/DataSourceForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataSourceId", title = "${uiLabelMap.DataSourceDataSourceId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${dataSourceId}')", urlMode = UrlMode.PLAIN, description = "${dataSourceId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", displayEntity = @DisplayEntityField(entityName = "DataSourceType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupDataSource {}

    @Form(
        name = "LookupDataSourceType",
        location = "component://marketing/widget/DataSourceForms.xml",
        target = "LookupDataSourceType",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", lookup = @LookupField(targetFormName = "LookupDataSourceType")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupDataSourceType {}

    @Form(
        name = "ListLookupDataSourceType",
        location = "component://marketing/widget/DataSourceForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "dataSourceTypeId", title = "${uiLabelMap.DataSourceDataSourceTypeId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${dataSourceTypeId}')", urlMode = UrlMode.PLAIN, description = "${dataSourceTypeId}", alsoHidden = false)),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupDataSourceType {}

}
