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
package com.ilscipio.scipio.common.widget;

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
public class PortletEditForms {

    @Form(
        name = "CommonPortletEdit",
        location = "component://common/widget/PortletEditForms.xml",
        target = "setPortalPortletAttributes",
        defaultMapName = "attributeMap",
        fields = {
            @FormField(name = "portalPageId", hidden = @HiddenField(value = "${parameters.portalPageId}")),
            @FormField(name = "portalPortletId", hidden = @HiddenField(value = "${parameters.portalPortletId}")),
            @FormField(name = "portletSeqId", hidden = @HiddenField(value = "${parameters.portletSeqId}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "getPortletAttributes", fieldMaps = {@FieldMap(fieldName = "portalPageId", fromField = "parameters.portalPageId"), @FieldMap(fieldName = "portalPortletId", fromField = "parameters.portalPortletId"), @FieldMap(fieldName = "portletSeqId", fromField = "parameters.portletSeqId")})})
    )
    public interface CommonPortletEdit {}

    @Form(
        name = "GenericPortalPageParam",
        location = "component://common/widget/PortletEditForms.xml",
        extendsForm = "CommonPortletEdit",
        fields = {
            @FormField(name = "pageId", requiredField = true, text = @TextField),
            @FormField(name = "submit", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface GenericPortalPageParam {}

    @Form(
        name = "FindGenericEntityParam",
        location = "component://common/widget/PortletEditForms.xml",
        extendsForm = "CommonPortletEdit",
        fields = {
            @FormField(name = "titleLabel", requiredField = true, text = @TextField),
            @FormField(name = "entity", requiredField = true, text = @TextField),
            @FormField(name = "pkIdName", requiredField = true, text = @TextField),
            @FormField(name = "divIdRefresh", text = @TextField),
            @FormField(name = "submit", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindGenericEntityParam {}

    @Form(
        name = "GenericScreenletParam",
        location = "component://common/widget/PortletEditForms.xml",
        extendsForm = "CommonPortletEdit",
        fields = {
            @FormField(name = "titleLabel", requiredField = true, text = @TextField),
            @FormField(name = "divIdRefresh", text = @TextField),
            @FormField(name = "formName", requiredField = true, text = @TextField),
            @FormField(name = "formLocation", requiredField = true, text = @TextField),
            @FormField(name = "submit", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface GenericScreenletParam {}

    @Form(
        name = "GenericScreenletAjaxParam",
        location = "component://common/widget/PortletEditForms.xml",
        extendsForm = "CommonPortletEdit",
        fields = {
            @FormField(name = "titleLabel", requiredField = true, text = @TextField),
            @FormField(name = "divIdRefresh", text = @TextField),
            @FormField(name = "divIdArea", requiredField = true, text = @TextField),
            @FormField(name = "screenName", requiredField = true, text = @TextField),
            @FormField(name = "screenLocation", requiredField = true, text = @TextField),
            @FormField(name = "submit", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface GenericScreenletAjaxParam {}

    @Form(
        name = "GenericScreenletAjaxWithMenuParam",
        location = "component://common/widget/PortletEditForms.xml",
        extendsForm = "CommonPortletEdit",
        fields = {
            @FormField(name = "titleLabel", requiredField = true, text = @TextField),
            @FormField(name = "divIdRefresh", text = @TextField),
            @FormField(name = "divIdArea", requiredField = true, text = @TextField),
            @FormField(name = "screenName", requiredField = true, text = @TextField),
            @FormField(name = "screenLocation", requiredField = true, text = @TextField),
            @FormField(name = "menuName", requiredField = true, text = @TextField),
            @FormField(name = "menuLocation", requiredField = true, text = @TextField),
            @FormField(name = "submit", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface GenericScreenletAjaxWithMenuParam {}

}
