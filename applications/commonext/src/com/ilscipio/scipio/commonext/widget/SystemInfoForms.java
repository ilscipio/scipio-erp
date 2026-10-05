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
package com.ilscipio.scipio.commonext.widget;

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
public class SystemInfoForms {

    @Form(
        name = "SystemInfoNotes",
        location = "component://commonext/widget/SystemInfoForms.xml",
        type = FormType.LIST,
        listName = "systemInfoNotes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "noteDateTime", title = "${uiLabelMap.CommonExtDateInfoCreated}", display = @DisplayField(type = "date-time")),
            @FormField(name = "noteInfo", title = "${uiLabelMap.CommonExtSystemInfoNote}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "${moreInfoUrl}${groovy: if (moreInfoItemName &&moreInfoItemId)\"?\" + moreInfoItemName + \"=\" + moreInfoItemId + \"&id=\" + moreInfoItemId;}", urlMode = UrlMode.INTER_APP, description = "${noteInfo}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSystemInfoNote", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "noteId"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId")}))
        }
    )
    public interface SystemInfoNotes {}

    @Form(
        name = "EditSysInfoPortletParams",
        location = "component://commonext/widget/SystemInfoForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "dummy", title = "BLOCK the following notifications:", display = @DisplayField),
            @FormField(name = "allNotifications", check = @CheckField),
            @FormField(name = "email", check = @CheckField),
            @FormField(name = "internalNotes", check = @CheckField),
            @FormField(name = "telephoneForwards", check = @CheckField),
            @FormField(name = "taskAssignment", check = @CheckField),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditSysInfoPortletParams {}

    @Form(
        name = "SystemInfoStatus",
        location = "component://commonext/widget/SystemInfoForms.xml",
        type = FormType.LIST,
        listName = "systemInfoStatus",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "noteDateTime", title = "${uiLabelMap.CommonExtDateLastChanged}", display = @DisplayField(type = "date-time")),
            @FormField(name = "noteInfo", title = "${uiLabelMap.CommonExtSystemInfoStatus}", widgetStyle = "${styles.link_nav_info_desc} ${styles.action_view}", hyperlink = @HyperlinkField(target = "${moreInfoUrl}", urlMode = UrlMode.INTER_APP, description = "${noteInfo}"))
        }
    )
    public interface SystemInfoStatus {}

    @Form(
        name = "CreateSystemInfoNote",
        location = "component://commonext/widget/SystemInfoForms.xml",
        target = "createSystemInfoNote",
        fields = {
            @FormField(name = "noteParty", text = @TextField),
            @FormField(name = "moreInfoUrl", text = @TextField(size = 50)),
            @FormField(name = "noteInfo", text = @TextField(size = 50)),
            @FormField(name = "createAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateSystemInfoNote {}

}
