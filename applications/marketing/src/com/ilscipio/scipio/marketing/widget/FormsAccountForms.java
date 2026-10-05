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
public class FormsAccountForms {

    @Form(
        name = "NewAccount",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        target = "createAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "accountType", hidden = @HiddenField(value = "${accountType}")),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyGroupName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "numEmployees", title = "${uiLabelMap.MarketingNoOfEmployees}", text = @TextField(size = 30)),
            @FormField(name = "siteName", title = "${uiLabelMap.FormFieldTitle_officeSiteName}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "postalAddressTitle", title = "${uiLabelMap.PartyGeneralCorrespondenceAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "address1", title = "${uiLabelMap.CommonAddress1}", requiredField = true, text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "address2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "city", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "stateProvinceGeoId", title = "${uiLabelMap.CommonState}", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName} - ${geoId}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "STATE,PROVINCE", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "geoName")}))),
            @FormField(name = "postalCode", title = "${uiLabelMap.CommonZipPostalCode}", requiredField = true, text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "stateProvinceGeoId", title = "${uiLabelMap.CommonState}", requiredField = true, dropDown = @DropDownField),
            @FormField(name = "countryGeoId", title = "${uiLabelMap.CommonCountry}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName} - ${geoId}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoName")}))),
            @FormField(name = "phoneTitle", title = "${uiLabelMap.PartyPrimaryPhone}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "countryCode", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "areaCode", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "contactNumber", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "extension", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "emailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface NewAccount {}

    @Form(
        name = "FindAccounts",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        target = "${currentUrl}",
        id = "FindAccounts",
        defaultMapName = "parameters",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "all", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", textFind = @TextFindField),
            @FormField(name = "groupName", title = "${uiLabelMap.PartyPartyGroupName}", textFind = @TextFindField),
            @FormField(name = "contactMechTypeId", event = "onchange", action = "javascript:ajaxUpdateAreas('contactMechContainer,ContactMechTypeOnly,contactMechTypeId=' + this.value);", dropDown = @DropDownField(options = {@Option(description = "${uiLabelMap.CommonNone}")}, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId"))),
            @FormField(name = "contactMechContainer", title = " ", idName = "contactMechContainer", container = @ContainerField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindAccounts {}

    @Form(
        name = "listAccounts",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        extendsForm = "listAccountsCommon",
        actions = @FormActions(set = {@SetAction(field = "roleTypeIdFrom", value = "OWNER"), @SetAction(field = "roleTypeIdTo", value = "ACCOUNT"), @SetAction(field = "relatedCompanyRoleTypeIdTo", value = "ACCOUNT"), @SetAction(field = "relatedCompanyRoleTypeIdFrom", value = "ACCOUNT"), @SetAction(field = "parameters.statusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.statusId_op", value = "notEqual"), @SetAction(field = "partyRelationshipTypeId", value = "ACCOUNT"), @SetAction(field = "parameters.roleTypeId", fromField = "roleTypeIdTo"), @SetAction(field = "fieldList", value = "${groovy:['partyId','roleTypeId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRoleAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface listAccounts {}

    @Form(
        name = "listAccountsCommon",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "${currentUrl}",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "viewprofile", description = "${partyName} [${partyId}]", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")})),
            @FormField(name = "emailAddress", display = @DisplayField(description = "${emailAddresses[0].infoString}")),
            @FormField(name = "telecomNumber", title = "${uiLabelMap.PartyPhoneNumber}", display = @DisplayField(description = "${telecomNumber.tnCountryCode} ${telecomNumber.tnAreaCode} ${telecomNumber.tnContactNumber} ${telecomNumber.tnAskForName}")),
            @FormField(name = "city", display = @DisplayField(description = "${postalAddress.paCity}")),
            @FormField(name = "countryGeoId", title = "${uiLabelMap.FormFieldTitle_country}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName}")),
            @FormField(name = "relatedCompany", title = "${uiLabelMap.PartyRelatedCompany}", useWhen = "relatedCompanyPartyId!=null", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "viewprofile", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator,relatedCompanyPartyId,true);} [${relatedCompanyPartyId}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "relatedCompanyPartyId")})),
            @FormField(name = "relatedCompany", title = "${uiLabelMap.PartyRelatedCompany}", useWhen = "relatedCompanyPartyId==null", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "partyName", value = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(party, true)}"), @SetAction(field = "telecomNumber", fromField = "telecomNumbers[0]", type = "Object"), @SetAction(field = "postalAddress", fromField = "postalAddresses[0]", type = "Object"), @SetAction(field = "countryGeoId", fromField = "postalAddress.paCountryGeoId"), @SetAction(field = "relatedCompanyPartyId", fromField = "relatedCompanies[0].partyIdFrom", type = "Object"), @SetAction(field = "existRelationship", fromField = "existRelationships[0]")}, entityOne = {@EntityOneAction(entityName = "Party", valueField = "party", useCache = true)})
    )
    public interface listAccountsCommon {}

    @Form(
        name = "ListAccounts",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        extendsForm = "listAccounts",
        fields = {
            @FormField(name = "assignToMe", title = "${uiLabelMap.SfaAssignToMe}", useWhen = "existRelationship==null&&!\"false\".equals(parameters.get(\"all\"))", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "createPartyRelationshipAndRole", description = "${uiLabelMap.SfaAssignToMe}", parameters = {@ParameterDef(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyRelationshipTypeId"), @ParameterDef(paramName = "partyIdTo", fromField = "partyId")})),
            @FormField(name = "relatedCompany", hidden = @HiddenField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "partyId"), @SortField(name = "emailAddress"), @SortField(name = "telecomNumber"), @SortField(name = "city"), @SortField(name = "countryGeoId"), @SortField(name = "assignToMe"), @SortField(name = "relatedCompany")})
    )
    public interface ListAccountsLc {}

    @Form(
        name = "ListMyAccounts",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        extendsForm = "ListAccounts",
        fields = {
            @FormField(name = "assignToMe", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.partyIdFrom", fromField = "userLogin.partyId"), @SetAction(field = "parameters.roleTypeIdFrom", value = "OWNER"), @SetAction(field = "parameters.roleTypeIdTo", value = "ACCOUNT"), @SetAction(field = "parameters.partyStatusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.partyStatusId_op", value = "notEqual"), @SetAction(field = "parameters.partyRelationshipTypeId", value = "ACCOUNT"), @SetAction(field = "fieldList", value = "${groovy:['partyIdFrom','partyId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRelationshipAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface ListMyAccounts {}

    @Form(
        name = "FindPostalAddress",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "paToName", title = "${uiLabelMap.PartyAddrToName}", textFind = @TextFindField),
            @FormField(name = "paAttnName", title = "${uiLabelMap.PartyAddrAttnName}", textFind = @TextFindField),
            @FormField(name = "paAddress1", title = "${uiLabelMap.FormFieldTitle_paAddress1}", textFind = @TextFindField),
            @FormField(name = "paAddress2", title = "${uiLabelMap.FormFieldTitle_paAddress2}", textFind = @TextFindField),
            @FormField(name = "paCity", title = "${uiLabelMap.FormFieldTitle_city}", textFind = @TextFindField),
            @FormField(name = "paStateProvinceGeoId", title = "${uiLabelMap.FormFieldTitle_stateProvince}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "states", keyName = "geoId", description = "${geoName}"))),
            @FormField(name = "paPostalCode", textFind = @TextFindField),
            @FormField(name = "paCountryGeoId", title = "${uiLabelMap.CommonCountry}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "countries", keyName = "geoId", description = "${geoName}")))
        }
    )
    public interface FindPostalAddress {}

    @Form(
        name = "FindTelecomNumber",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "tnCountryCode", title = "${uiLabelMap.CommonCountryCode}", textFind = @TextFindField),
            @FormField(name = "tnAreaCode", title = "${uiLabelMap.PartyAreaCode}", textFind = @TextFindField),
            @FormField(name = "tnContactNumber", title = "${uiLabelMap.PartyContactNumber}", textFind = @TextFindField),
            @FormField(name = "tnExtension", title = "${uiLabelMap.PartyExtension}", textFind = @TextFindField)
        }
    )
    public interface FindTelecomNumber {}

    @Form(
        name = "FindInfoStringContactMech",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "infoString", title = "${contactMechType.description}", textFind = @TextFindField)
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ContactMechType", valueField = "contactMechType")})
    )
    public interface FindInfoStringContactMech {}

    @Form(
        name = "listAccountLeads",
        location = "component://marketing/widget/sfa/forms/AccountForms.xml",
        extendsForm = "listAccountsCommon",
        actions = @FormActions(set = {@SetAction(field = "roleTypeIdTo", value = "ACCOUNT_LEAD"), @SetAction(field = "parameters.statusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.statusId_op", value = "notEqual"), @SetAction(field = "partyRelationshipTypeId", value = "ACCOUNT"), @SetAction(field = "parameters.roleTypeId", fromField = "roleTypeIdTo"), @SetAction(field = "fieldList", value = "${groovy:['partyId','roleTypeId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRoleAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface listAccountLeads {}

}
