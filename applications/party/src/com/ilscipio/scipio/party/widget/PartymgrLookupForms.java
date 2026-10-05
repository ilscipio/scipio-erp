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
public class PartymgrLookupForms {

    @Form(
        name = "lookupPartyName",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPartyName",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}"))),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPartyName {}

    @Form(
        name = "listLookupPartyName",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPartyName",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", displayEntity = @DisplayEntityField(entityName = "PartyType", description = "${description}", alsoHidden = false)),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", display = @DisplayField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "inputFields", fromField = "parameters"), @SetAction(field = "orderBy", value = "partyId"), @SetAction(field = "entityName", value = "PartyNameView")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupPartyName {}

    @Form(
        name = "lookupPartyEmail",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPartyEmail",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}"))),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPartyEmail {}

    @Form(
        name = "listLookupPartyEmail",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPartyEmail",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactMechId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "name", title = "${uiLabelMap.PartyName}", display = @DisplayField),
            @FormField(name = "infoString", title = "${uiLabelMap.PartyEmailAddress}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.contactMechTypeId", value = "EMAIL_ADDRESS"), @SetAction(field = "filterByDate", value = "Y"), @SetAction(field = "inputFields", fromField = "parameters"), @SetAction(field = "orderBy", value = "partyId"), @SetAction(field = "entityName", value = "PartyNameContactMechView")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")}),
        rowActions = @RowActions(set = {@SetAction(field = "name", value = "${firstName} ${middleName} ${lastName} ${groupName}")})
    )
    public interface listLookupPartyEmail {}

    @Form(
        name = "lookupCustomerName",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupCustomerName",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "CUSTOMER")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}"))),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCustomerName {}

    @Form(
        name = "listLookupCustomerName",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCustomerName",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", displayEntity = @DisplayEntityField(entityName = "PartyType", description = "${description}", alsoHidden = false)),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", display = @DisplayField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "inputFields", fromField = "parameters"), @SetAction(field = "orderBy", value = "partyId"), @SetAction(field = "entityName", value = "PartyRoleNameDetail")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupCustomerName {}

    @Form(
        name = "lookupCustomerNameForSalesRep",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupCustomerNameForSalesRep",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "CUSTOMER")),
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField(value = "SALES_REP")),
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField(value = "CUSTOMER")),
            @FormField(name = "partyIdFrom", hidden = @HiddenField(value = "${userLogin.partyId}")),
            @FormField(name = "filterByDate", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}"))),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCustomerNameForSalesRep {}

    @Form(
        name = "listLookupCustomerNameForSalesRep",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCustomerNameForSalesRep",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", displayEntity = @DisplayEntityField(entityName = "PartyType", description = "${description}", alsoHidden = false)),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", display = @DisplayField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "lastName"), @FieldMap(fieldName = "entityName", value = "PartyRelationshipAndDetail"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupCustomerNameForSalesRep {}

    @Form(
        name = "lookupPerson",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPerson",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPerson {}

    @Form(
        name = "listLookupPerson",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPerson",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false)),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", display = @DisplayField),
            @FormField(name = "middleName", title = "${uiLabelMap.PartyMiddleInitial}", display = @DisplayField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", display = @DisplayField),
            @FormField(name = "personalTitle", title = "${uiLabelMap.PartyPersonalTitle}", display = @DisplayField),
            @FormField(name = "suffix", title = "${uiLabelMap.PartySuffix}", display = @DisplayField),
            @FormField(name = "nickname", title = "${uiLabelMap.PartyNickName}", display = @DisplayField)
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupPerson {}

    @Form(
        name = "lookupPartyAndUserLoginAndPerson",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPartyAndUserLoginAndPerson",
        paginateTarget = "LookupPartyAndUserLoginAndPerson",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", textFind = @TextFindField),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "middleName", title = "${uiLabelMap.PartyMiddleName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "createdDate", title = "${uiLabelMap.PartyCreatedDate}", dateFind = @DateFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPartyAndUserLoginAndPerson {}

    @Form(
        name = "listLookupPartyAndUserLoginAndPerson",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPartyAndUserLoginAndPerson",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField(description = "${partyId}")),
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${userLoginId}', '${userLoginId}', '${parameters.webSitePublishPoint}')", urlMode = UrlMode.PLAIN, description = "${userLoginId}", alsoHidden = false)),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyName}", display = @DisplayField(description = "${firstName} ${middleName} ${lastName} ${groupName}"))
        },
        actions = @FormActions(set = {@SetAction(field = "inputFields", fromField = "requestParameters"), @SetAction(field = "entityName", value = "PartyAndUserLoginAndPerson")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupPartyAndUserLoginAndPerson {}

    @Form(
        name = "lookupUserLoginAndPartyDetails",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupUserLoginAndPartyDetails",
        paginateTarget = "LookupUserLoginAndPartyDetails",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "middleName", title = "${uiLabelMap.PartyMiddleName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "createdDate", title = "${uiLabelMap.PartyCreatedDate}", dateFind = @DateFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupUserLoginAndPartyDetails {}

    @Form(
        name = "listLookupUserLoginAndPartyDetails",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupUserLoginAndPartyDetails",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${userLoginId}', '${userLoginId}', '${parameters.webSitePublishPoint}')", urlMode = UrlMode.PLAIN, description = "${userLoginId}", alsoHidden = false)),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField(description = "${partyId}")),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyName}", display = @DisplayField(description = "${firstName} ${middleName} ${lastName}")),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", display = @DisplayField(description = "${groupName}"))
        },
        actions = @FormActions(set = {@SetAction(field = "inputFields", fromField = "requestParameters"), @SetAction(field = "entityName", value = "UserLoginAndPartyDetails")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupUserLoginAndPartyDetails {}

    @Form(
        name = "lookupPartyGroup",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPartyGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPartyGroup {}

    @Form(
        name = "listLookupPartyGroup",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPartyGroup",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", display = @DisplayField),
            @FormField(name = "comments", title = "${uiLabelMap.PartyComments}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyId}", alsoHidden = false))
        },
        actions = @FormActions(script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupPartyGroup {}

    @Form(
        name = "LookupPartyClassificationGroup",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupPartyClassificationGroup",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyClassificationGroup", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyClassificationTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyClassificationType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPartyClassificationGroup {}

    @Form(
        name = "listLookupPartyClassificationGroup",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyClassificationGroupId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyClassificationGroupId}')", urlMode = UrlMode.PLAIN, description = "${partyClassificationGroupId}", alsoHidden = false)),
            @FormField(name = "partyClassificationTypeId", display = @DisplayField),
            @FormField(name = "parentGroupId", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyClassificationGroup"), @FieldMap(fieldName = "orderBy", value = "description"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupPartyClassificationGroup {}

    @Form(
        name = "LookupCommEvent",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupCommEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyRoleTypeIdTo}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "custRequestId", title = "${uiLabelMap.PartyServicemgntCustRequestId}", lookup = @LookupField(targetFormName = "LookupCustRequest", size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateFind = @DateFindField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", dateFind = @DateFindField),
            @FormField(name = "subject", mapName = "subjectMap", title = "${uiLabelMap.PartyCommEventSubject}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupCommEvent {}

    @Form(
        name = "ListLookupCommEvent",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${communicationEventId}')", urlMode = UrlMode.PLAIN, description = "${communicationEventId}", alsoHidden = false)),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName} [${partyId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyRoleTypeIdTo}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "custRequestId", title = "${uiLabelMap.PartyServicemgntCustRequestId}", display = @DisplayField),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CommunicationEvent"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupCommEvent {}

    @Form(
        name = "lookupContactMech",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupContactMech",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactMechId", textFind = @TextFindField),
            @FormField(name = "contactMechTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}"))),
            @FormField(name = "infoString", textFind = @TextFindField),
            @FormField(name = "paAddress1", textFind = @TextFindField),
            @FormField(name = "paAddress2", textFind = @TextFindField),
            @FormField(name = "paPostalCode", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupContactMech {}

    @Form(
        name = "listLookupContactMech",
        location = "component://party/widget/partymgr/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupContactMech",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactMechId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactMechId}')", urlMode = UrlMode.PLAIN, description = "${contactMechId}", alsoHidden = false)),
            @FormField(name = "partyTypeId", title = "${uiLabelMap.PartyTypeId}", displayEntity = @DisplayEntityField(entityName = "PartyType", description = "${description}", alsoHidden = false)),
            @FormField(name = "contactMechTypeId", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "cmDetail", display = @DisplayField(description = "[${infoString}] [${tnCountryCode}-${tnAreaCode}-${tnContactNumber}] [${paAddress1}, ${paAddress2}, ${paCity}, ${paStateProvinceGeoId}, ${paPostalCode}, ${paPostalCodeExt} ${paCountryGeoId}]", alsoHidden = false))
        },
        actions = @FormActions(set = {@SetAction(field = "inputFields", fromField = "parameters"), @SetAction(field = "entityName", value = "PartyAndContactMech")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/FindLookUp.groovy")})
    )
    public interface listLookupContactMech {}

    @Form(
        name = "LookupInternalOrganization",
        location = "component://party/widget/partymgr/LookupForms.xml",
        target = "LookupInternalOrganization",
        fields = {
            @FormField(name = "roleTypeId", hidden = @HiddenField(value = "INTERNAL_ORGANIZATIO")),
            @FormField(name = "partyId", textFind = @TextFindField),
            @FormField(name = "groupName", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupInternalOrganization {}

}
