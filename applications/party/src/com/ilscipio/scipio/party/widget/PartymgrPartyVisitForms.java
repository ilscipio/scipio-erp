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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrPartyVisitForms {

    @Form(
        name = "FindVisits",
        location = "component://party/widget/partymgr/PartyVisitForms.xml",
        target = "findVisits",
        title = "Find and list party visits",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "activeOnly", check = @CheckField),
            @FormField(name = "visitId", textFind = @TextFindField),
            @FormField(name = "visitorId", textFind = @TextFindField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "userLoginId", textFind = @TextFindField),
            @FormField(name = "userCreated", dateFind = @DateFindField(type = "date")),
            @FormField(name = "webappName", textFind = @TextFindField),
            @FormField(name = "clientIpAddress", textFind = @TextFindField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindVisits {}

    @Form(
        name = "ListVisits",
        location = "component://party/widget/partymgr/PartyVisitForms.xml",
        type = FormType.LIST,
        title = "Visits List",
        listName = "listIt",
        defaultEntityName = "Visit",
        paginateTarget = "findVisits",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "visitId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "visitdetail", description = "${visitId}", parameters = {@ParameterDef(paramName = "visitId")})),
            @FormField(name = "visitorId", title = "${uiLabelMap.PartyVisitorId}", sortField = true, display = @DisplayField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "viewprofile", description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", sortField = true, display = @DisplayField),
            @FormField(name = "userCreated", title = "${uiLabelMap.PartyNewUser}", sortField = true, display = @DisplayField),
            @FormField(name = "webappName", title = "${uiLabelMap.PartyWebApp}", sortField = true, display = @DisplayField),
            @FormField(name = "clientIpAddress", title = "${uiLabelMap.PartyClientIP}", sortField = true, display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", sortField = true, display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-visitId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Visit"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "filterByDate", fromField = "parameters.activeOnly")})})
    )
    public interface ListVisits {}

    @Form(
        name = "ListLoggedInUsers",
        location = "component://party/widget/partymgr/PartyVisitForms.xml",
        type = FormType.LIST,
        title = "Visits List",
        listName = "listIt",
        defaultEntityName = "Visit",
        paginateTarget = "listLoggedInUsers",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", sortField = true, display = @DisplayField),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "viewprofile", description = "${partyId}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "clientIpAddress", title = "${uiLabelMap.PartyClientIP}", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-userLoginId")})
    )
    public interface ListLoggedInUsers {}

}
