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
package com.ilscipio.scipio.cms.widget;

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
public class FieldLookupForms {

    @Form(
        name = "lookupMediaImage",
        location = "component://cms/widget/FieldLookupForms.xml",
        target = "LookupMediaImage",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contentId", title = "${uiLabelMap.FormFieldTitle_contentId}", textFind = @TextFindField),
            @FormField(name = "contentName", title = "${uiLabelMap.FormFieldTitle_contentName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupMediaImage {}

    @Form(
        name = "listLookupMediaImage",
        location = "component://cms/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupMediaImage",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "contentId", title = "${uiLabelMap.FormFieldTitle_contentId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contentId}')", urlMode = UrlMode.PLAIN, description = "${contentId}", alsoHidden = false)),
            @FormField(name = "contentName", title = "${uiLabelMap.FormFieldTitle_contentName}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.contentTypeId", value = "SCP_MEDIA")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Content"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupMediaImage {}

}
