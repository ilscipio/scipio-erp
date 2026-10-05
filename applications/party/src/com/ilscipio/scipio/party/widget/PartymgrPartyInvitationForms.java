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
public class PartymgrPartyInvitationForms {

    @Form(
        name = "FindPartyInvitations",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        target = "partyInvitation",
        defaultMapName = "partyInvitations",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyInvitationId", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "toName", position = 2, textFind = @TextFindField),
            @FormField(name = "emailAddress", textFind = @TextFindField),
            @FormField(name = "datefrom", title = "${uiLabelMap.CommonFrom}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_INV_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "dateThru", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPartyInvitations {}

    @Form(
        name = "ListPartyInvitations",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "PartyInvitation",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyInvitationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPartyInvitation", description = "${partyInvitationId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyInvitationId")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "toName", title = "${uiLabelMap.PartyToName}", display = @DisplayField),
            @FormField(name = "emailAddress", title = "${uiLabelMap.PartyEmailAddress}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "lastInviteDate", display = @DisplayField),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyInvitation", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyInvitationId", fromField = "partyInvitationId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.lastInviteDate_fld0_value", fromField = "parameters.datefrom"), @SetAction(field = "parameters.lastInviteDate_fld0_op", value = "greaterThanEqualTo"), @SetAction(field = "parameters.lastInviteDate", fromField = "parameters.dateThru"), @SetAction(field = "parameters.lastInviteDate_op", value = "lessThanEqualTo")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyInvitation"), @FieldMap(fieldName = "orderBy", value = "partyInvitationId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPartyInvitations {}

    @Form(
        name = "EditPartyInvitation",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        target = "updatePartyInvitation",
        defaultMapName = "partyInvitation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyInvitation")
        },
        fields = {
            @FormField(name = "partyInvitationId", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_INV_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "partyInvitation==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyInvitation!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "partyInvitation==null", target = "createPartyInvitation")
        }
    )
    public interface EditPartyInvitation {}

    @Form(
        name = "ListPartyInvitationGroupAssocs",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        type = FormType.LIST,
        listName = "partyInvitationGroupAssoc",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyInvitationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPartyInvitation", description = "${partyInvitationId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyInvitationId")})),
            @FormField(name = "partyId", entryName = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyIdTo}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyInvitationGroupAssoc", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyInvitationId"), @ParameterDef(paramName = "partyIdTo")}))
        }
    )
    public interface ListPartyInvitationGroupAssocs {}

    @Form(
        name = "AddPartyInvitationGroupAssoc",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        target = "createPartyInvitationGroupAssoc",
        defaultMapName = "partyInvitationGroupAssoc",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyInvitationGroupAssoc")
        },
        fields = {
            @FormField(name = "partyInvitationId", hidden = @HiddenField),
            @FormField(name = "partyIdTo", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Party", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "partyTypeId", value = "PARTY_GROUP")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyInvitationGroupAssoc {}

    @Form(
        name = "ListPartyInvitationRoleAssocs",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        type = FormType.LIST,
        listName = "partyInvitationRoleAssoc",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyInvitationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPartyInvitation", description = "${partyInvitationId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyInvitationId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyInvitationRoleAssoc", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyInvitationId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListPartyInvitationRoleAssocs {}

    @Form(
        name = "AddPartyInvitationRoleAssoc",
        location = "component://party/widget/partymgr/PartyInvitationForms.xml",
        target = "createPartyInvitationRoleAssoc",
        defaultMapName = "partyInvitationGroupAssoc",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyInvitationRoleAssoc")
        },
        fields = {
            @FormField(name = "partyInvitationId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "roleTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyInvitationRoleAssoc {}

}
