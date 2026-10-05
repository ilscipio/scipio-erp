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
public class WebsiteWebAnalyticsForms {

    @Form(
        name = "ListWebAnalyticsConfig",
        location = "component://content/widget/website/WebAnalyticsForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "WebAnalyticsConfig",
        paginateTarget = "FindWebAnalyticsConfigs",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "WebAnalyticsConfig", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "webAnalyticsTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "WebAnalyticsType", description = "${description} [${webAnalyticsTypeId}]")),
            @FormField(name = "webAnalyticsCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditWebAnalyticsConfig", description = "${webAnalyticsCode}", parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "webAnalyticsTypeId"), @ParameterDef(paramName = "webSiteId")})),
            @FormField(name = "webSiteId", hidden = @HiddenField(value = "${webSiteId}")),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWebAnalyticsConfig", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "webSiteId"), @ParameterDef(paramName = "webAnalyticsTypeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "webAnalyticsConfigCtx"), @FieldMap(fieldName = "entityName", value = "WebAnalyticsConfig"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListWebAnalyticsConfig {}

    @Form(
        name = "EditWebAnalyticsConfig",
        location = "component://content/widget/website/WebAnalyticsForms.xml",
        target = "updateWebAnalyticsConfig",
        defaultMapName = "webAnalyticsConfig",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateWebAnalyticsConfig", mapName = "webAnalyticsConfig")
        },
        fields = {
            @FormField(name = "webAnalyticsTypeId", title = "${uiLabelMap.CatalogWebAnalyticsType}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "webAnalyticsConfig!=null", displayEntity = @DisplayEntityField(entityName = "WebAnalyticsType")),
            @FormField(name = "webAnalyticsTypeId", title = "${uiLabelMap.CatalogWebAnalyticsType}", useWhen = "webAnalyticsConfig==null&&webAnalyticsTypeId==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "WebAnalyticsType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "webAnalyticsTypeId")}))),
            @FormField(name = "webAnalyticsTypeId", title = "${uiLabelMap.CatalogWebAnalyticsType}", tooltip = "${uiLabelMap.CommonCannotBeFound}:[${webAnalyticsTypeId}]", useWhen = "webAnalyticsConfig==null&&webAnalyticsTypeId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "webSiteId", title = "${uiLabelMap.CommonWebsite}", hidden = @HiddenField(value = "${parameters.webSiteId}")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "webAnalyticsConfig!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "webAnalyticsConfig==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "webAnalyticsConfig==null", target = "createWebAnalyticsConfig")
        }
    )
    public interface EditWebAnalyticsConfig {}

}
