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
public class FormsContactForms {

    @Form(
        name = "FindContacts",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        target = "${currentUrl}",
        extendsForm = "FindAccounts",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "groupName", hidden = @HiddenField),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "all"), @SortField(name = "groupName"), @SortField(name = "noConditionFind"), @SortField(name = "partyId"), @SortField(name = "firstName"), @SortField(name = "lastName"), @SortField(name = "contactMechTypeId"), @SortField(name = "contactMechContainer"), @SortField(name = "submitAction")})
    )
    public interface FindContacts {}

    @Form(
        name = "ListContacts",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        extendsForm = "listAccountsCommon",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        paginateTarget = "${currentUrl}",
        fields = {
            @FormField(name = "assignToMe", title = "${uiLabelMap.SfaAssignToMe}", useWhen = "existRelationship==null&&!\"false\".equals(parameters.get(\"all\"))", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "createPartyRelationshipAndRole", description = "${uiLabelMap.SfaAssignToMe}", parameters = {@ParameterDef(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyRelationshipTypeId"), @ParameterDef(paramName = "partyIdTo", fromField = "partyId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "roleTypeIdFrom", value = "ACCOUNT"), @SetAction(field = "roleTypeIdTo", value = "CONTACT"), @SetAction(field = "partyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "relatedCompanyRoleTypeIdTo", value = "CONTACT"), @SetAction(field = "relatedCompanyRoleTypeIdFrom", value = "ACCOUNT"), @SetAction(field = "relatedCompanyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "parameters.roleTypeId", fromField = "roleTypeIdTo"), @SetAction(field = "parameters.statusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.statusId_op", value = "notEqual"), @SetAction(field = "fieldList", value = "${groovy:['partyId','roleTypeId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRoleAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})}),
        sortOrder = @SortOrder(sortFields = {@SortField(name = "partyId"), @SortField(name = "emailAddress"), @SortField(name = "telecomNumber"), @SortField(name = "city"), @SortField(name = "countryGeoId"), @SortField(name = "relatedCompany"), @SortField(name = "export"), @SortField(name = "assignToMe")})
    )
    public interface ListContacts {}

    @Form(
        name = "ListMyContacts",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        extendsForm = "ListContacts",
        fields = {
            @FormField(name = "assignToMe", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "relatedCompanyRoleTypeIdTo", value = "CONTACT"), @SetAction(field = "relatedCompanyRoleTypeIdFrom", value = "ACCOUNT"), @SetAction(field = "relatedCompanyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "parameters.partyIdFrom", fromField = "userLogin.partyId"), @SetAction(field = "parameters.roleTypeIdTo", value = "CONTACT"), @SetAction(field = "parameters.partyStatusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.partyStatusId_op", value = "notEqual"), @SetAction(field = "parameters.partyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "fieldList", value = "${groovy:['partyIdFrom','partyId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRelationshipAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface ListMyContacts {}

    @Form(
        name = "NewContact",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        target = "createContact",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "firstName", requiredField = true, text = @TextField),
            @FormField(name = "lastName", requiredField = true, text = @TextField),
            @FormField(name = "suffix", text = @TextField),
            @FormField(name = "postalAddressTitle", title = "${uiLabelMap.PartyGeneralCorrespondenceAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "address1", title = "${uiLabelMap.CommonAddress1}", requiredField = true, text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "address2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "city", title = "${uiLabelMap.CommonCity}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "stateProvinceGeoId", title = "${uiLabelMap.CommonState}", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoId} - ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "STATE,PROVINCE", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "postalCode", title = "${uiLabelMap.CommonZipPostalCode}", requiredField = true, text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "countryGeoId", title = "${uiLabelMap.CommonCountry}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoId}: ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "phoneTitle", title = "${uiLabelMap.PartyPrimaryPhone}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "countryCode", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "areaCode", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "contactNumber", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "extension", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "emailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "contactListTitle", title = "${uiLabelMap.MarketingContactList}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactList}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactList", description = "${description}", keyFieldName = "contactListId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface NewContact {}

    @Form(
        name = "MergeContacts",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        target = "MergeContacts",
        fields = {
            @FormField(name = "partyIdTo", title = "${uiLabelMap.AccountingToParty}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "partyList", listEntryName = "contact", keyName = "contact.partyId", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, 'partyId', false)} ${contact.partyId}"))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "partyList", listEntryName = "contact", keyName = "contact.partyId", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, 'partyId', false)} ${contact.partyId}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.SfaMergeContacts}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", tooltipStyle = "button-text", position = 3, submit = @SubmitField(buttonType = "text-link"))
        },
        actions = @FormActions(set = {@SetAction(field = "roleTypeId", value = "CONTACT"), @SetAction(field = "partyTypeId", value = "PERSON"), @SetAction(field = "lookupFlag", value = "Y")}, service = {@ServiceAction(serviceName = "findParty")})
    )
    public interface MergeContacts {}

    @Form(
        name = "NewContactFromVCard",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        type = FormType.UPLOAD,
        target = "createContactFromVCard",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "infile", title = "${uiLabelMap.SfaUploadVCard}", file = @FileField),
            @FormField(name = "serviceName", hidden = @HiddenField(value = "createContact")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface NewContactFromVCard {}

    @Form(
        name = "ViewPartiesCreatedByVCard",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        type = FormType.LIST,
        listName = "partiesCreated",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", hyperlink = @HyperlinkField(target = "viewprofile", description = "${partyName} [${partyId}]", targetWindow = "_blank", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")})),
            @FormField(name = "emailAddress", display = @DisplayField(description = "${emailAddresses[0].infoString}")),
            @FormField(name = "telecomNumber", title = "${uiLabelMap.PartyPhoneNumber}", display = @DisplayField(description = "${telecomNumber.tnCountryCode} ${telecomNumber.tnAreaCode} ${telecomNumber.tnContactNumber} ${telecomNumber.tnAskForName}")),
            @FormField(name = "city", display = @DisplayField(description = "${postalAddress.paCity}"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "partyName", value = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(party, true)}"), @SetAction(field = "telecomNumber", fromField = "telecomNumbers[0]", type = "Object"), @SetAction(field = "postalAddress", fromField = "postalAddresses[0]", type = "Object")}, entityOne = {@EntityOneAction(entityName = "Party", valueField = "party", useCache = true)})
    )
    public interface ViewPartiesCreatedByVCard {}

    @Form(
        name = "ViewPartiesExistInVCard",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        type = FormType.LIST,
        listName = "partiesExist",
        extendsForm = "ViewPartiesCreatedByVCard"
    )
    public interface ViewPartiesExistInVCard {}

    @Form(
        name = "QuickAddContact",
        location = "component://marketing/widget/sfa/forms/ContactForms.xml",
        target = "quickAddContact",
        fields = {
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}*", requiredField = true, text = @TextField(size = 15)),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}*", requiredField = true, text = @TextField(size = 15)),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 15)),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactList}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactList", description = "${groovy:contactListName.substring(0,Math.min(contactListName.length(), 12))}...", keyFieldName = "contactListId"))),
            @FormField(name = "quickAdd", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface QuickAddContact {}

}
