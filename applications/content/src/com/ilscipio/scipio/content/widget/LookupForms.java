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
public class LookupForms {

    @Form(
        name = "lookupListLayout",
        location = "component://content/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "entityList",
        defaultEntityName = "ContentAssocDataResourceViewFrom",
        paginateTarget = "LookupListLayout",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contentId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:execRemoteCall('cloneLayout', '${drDataResourceId}', '${contentId}', 'TEMPLATE_MASTER', '')", urlMode = UrlMode.PLAIN, description = "${contentId}", alsoHidden = false)),
            @FormField(name = "contentName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "drObjectInfo", display = @DisplayField)
        }
    )
    public interface lookupListLayout {}

    @Form(
        name = "lookupDataResourceContent",
        location = "component://content/widget/LookupForms.xml",
        target = "LookupSubContent",
        defaultEntityName = "DataResourceContentView",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "dataResourceId", title = "${uiLabelMap.ContentDataResourceId}", textFind = @TextFindField),
            @FormField(name = "coContentId", textFind = @TextFindField),
            @FormField(name = "coContentName", textFind = @TextFindField),
            @FormField(name = "coDescription", textFind = @TextFindField),
            @FormField(name = "createdByUserLogin", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "createdDate", dateFind = @DateFindField),
            @FormField(name = "lastModifiedByUserLogin", lookup = @LookupField(targetFormName = "LookupParty")),
            @FormField(name = "lastModifiedDate", dateFind = @DateFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField),
            @FormField(name = "contentIdTo", hidden = @HiddenField(value = "${contentIdTo}")),
            @FormField(name = "mapKey", hidden = @HiddenField(value = "${mapKey}"))
        }
    )
    public interface lookupDataResourceContent {}

    @Form(
        name = "listLookupDataResourceContent",
        location = "component://content/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "DataResourceContentView",
        paginateTarget = "LookupSubContent",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "replaceAction", widgetStyle = "${styles.link_run_local} ${styles.action_copy}", hyperlink = @HyperlinkField(target = "javascript:execRemoteCall('replaceSubContent','${dataResourceId}','${coContentId}', '${contentIdTo}', '${mapKey}')", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.FormFieldTitle_replace} [${dataResourceId}/${coContentId}]", alsoHidden = false)),
            @FormField(name = "pasteContentAction", widgetStyle = "${styles.link_run_local} ${styles.action_copy}", hyperlink = @HyperlinkField(target = "javascript:execRemoteCall('pasteContent','${dataResourceId}','${coContentId}')", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.ContentPaste} [${dataResourceId}/${coContentId}]", alsoHidden = false)),
            @FormField(name = "dataResourceName", widgetStyle = "${styles.link_nav_info_name}", display = @DisplayField),
            @FormField(name = "dataCategoryId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "DataResourceContentView"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupDataResourceContent {}

}
