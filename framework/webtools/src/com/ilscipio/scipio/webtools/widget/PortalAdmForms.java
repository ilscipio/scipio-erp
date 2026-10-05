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
package com.ilscipio.scipio.webtools.widget;

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
public class PortalAdmForms {

    @Form(
        name = "FindPortalPages",
        location = "component://webtools/widget/PortalAdmForms.xml",
        target = "FindPortalPage",
        defaultEntityName = "PortalPage",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "portalPageId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "originalPortalPageId", position = 2, textFind = @TextFindField),
            @FormField(name = "portalPageName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "parentPortalPageId", position = 2, textFind = @TextFindField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "securityGroupId", title = "${uiLabelMap.CommonSecurityGroupId}", position = 2, textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPortalPages {}

    @Form(
        name = "ListPortalPages",
        location = "component://webtools/widget/PortalAdmForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindPortalPage",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "portalPageId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "EditPortalPage", description = "${portalPageId}", parameters = {@ParameterDef(paramName = "portalPageId")})),
            @FormField(name = "portalPageName", title = "${uiLabelMap.CommonName}", useWhen = "ownerUserLoginId!=null && ownerUserLoginId==\"_NA_\"", sortField = true, display = @DisplayField),
            @FormField(name = "portalPageName", title = "${uiLabelMap.CommonName}", useWhen = "ownerUserLoginId!=null && ownerUserLoginId!=\"_NA_\"", idName = "portalPageName", sortField = true, display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", useWhen = "ownerUserLoginId!=null && ownerUserLoginId==\"_NA_\"", sortField = true, display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", useWhen = "ownerUserLoginId!=null && ownerUserLoginId!=\"_NA_\"", idName = "portalDescription", sortField = true, display = @DisplayField),
            @FormField(name = "parentPortalPageId", sortField = true, display = @DisplayField),
            @FormField(name = "sequenceNum", sortField = true, display = @DisplayField),
            @FormField(name = "originalPortalPageId", sortField = true, display = @DisplayField),
            @FormField(name = "ownerUserLoginId", sortField = true, display = @DisplayField),
            @FormField(name = "securityGroupId", title = "${uiLabelMap.CommonSecurityGroupId}", sortField = true, display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", useWhen = "originalPortalPageId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePortalPage", description = "${uiLabelMap.CommonRevertPortalPage}", parameters = {@ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PortalPage"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPortalPages {}

    @Form(
        name = "EditPortalPage",
        location = "component://webtools/widget/PortalAdmForms.xml",
        target = "${targetPortalPage}",
        defaultMapName = "portalPage",
        fields = {
            @FormField(name = "portalPageId", useWhen = "!\"${portalPage.portalPageId}\".equals(\"\")", display = @DisplayField),
            @FormField(name = "portalPageId", useWhen = "\"${portalPage.portalPageId}\".equals(\"\")", text = @TextField),
            @FormField(name = "originalPortalPageId", position = 2, text = @TextField),
            @FormField(name = "ownerUserLoginId", text = @TextField),
            @FormField(name = "parentPortalPageId", position = 2, text = @TextField),
            @FormField(name = "portalPageName", text = @TextField),
            @FormField(name = "description", position = 2, text = @TextField(size = 60)),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "securityGroupId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SecurityGroup", description = "${groupId} -- ${description}", keyFieldName = "groupId"))),
            @FormField(name = "saveAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", position = 2, submit = @SubmitField)
        }
    )
    public interface EditPortalPage {}

}
