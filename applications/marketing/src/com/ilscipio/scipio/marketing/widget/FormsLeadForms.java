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
public class FormsLeadForms {

    @Form(
        name = "createLead",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "createLead",
        defaultMapName = "contactDetailMap",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "firstName", requiredField = true, text = @TextField),
            @FormField(name = "lastName", requiredField = true, text = @TextField),
            @FormField(name = "suffix", text = @TextField),
            @FormField(name = "groupName", text = @TextField),
            @FormField(name = "title", text = @TextField),
            @FormField(name = "numEmployees", title = "${uiLabelMap.MarketingNoOfEmployees}", text = @TextField(size = 30)),
            @FormField(name = "officeSiteName", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "postalAddressTitle", title = "${uiLabelMap.PartyGeneralCorrespondenceAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "address1", title = "${uiLabelMap.CommonAddress1}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "address2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "city", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "stateProvinceGeoId", title = "${uiLabelMap.CommonState}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoId} - ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "STATE,PROVINCE", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "postalCode", title = "${uiLabelMap.CommonZipPostalCode}", text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "countryGeoId", title = "${uiLabelMap.FormFieldTitle_country}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoId}: ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "phoneTitle", title = "${uiLabelMap.PartyPrimaryPhone}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "countryCode", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "areaCode", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "contactNumber", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "extension", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "emailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "leadSourceTitle", title = "${uiLabelMap.SfaLeadSource}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaLeadSource}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataSource", description = "${dataSourceId} - ${description}", keyFieldName = "dataSourceId", constraints = {@EntityConstraint(name = "dataSourceTypeId", value = "LEAD_SOURCE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactListTitle", title = "${uiLabelMap.MarketingContactList}", titleAreaStyle = "group-label", display = @DisplayField),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactList}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactList", description = "${description}", keyFieldName = "contactListId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface createLead {}

    @Form(
        name = "ConvertLead",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "convertLead",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "leadId", entryName = "partyId", title = "${uiLabelMap.SfaCreateContactForLead}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName} [${parameters.partyId}]", alsoHidden = false)),
            @FormField(name = "partyGroupId", title = "${uiLabelMap.SfaCreateAccountForLead}", useWhen = "parameters.get(\"partyGroupId\")!=\"\"", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName} [${parameters.partyGroupId}]")),
            @FormField(name = "partyGroupId", title = "${uiLabelMap.SfaAccountName}", tooltip = "${uiLabelMap.SfaSelectExistingAccountOrLeaveBlankToCreateNew}", useWhen = "parameters.get(\"partyGroupId\")==\"\"", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRole", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, partyId, false)} : [partyId]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "ACCOUNT")}))),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ConvertLead {}

    @Form(
        name = "MergeLeads",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "MergeLeads",
        fields = {
            @FormField(name = "partyIdTo", title = "${uiLabelMap.AccountingToParty}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "partyList", listEntryName = "lead", keyName = "lead.partyId", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, 'partyId', false)} ${lead.partyId}"))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "partyList", listEntryName = "lead", keyName = "lead.partyId", description = "${groovy:org.ofbiz.party.party.PartyHelper.getPartyName(delegator, 'partyId', false)} ${lead.partyId}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.SfaMergeLeads}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", tooltipStyle = "button-text", position = 3, submit = @SubmitField(buttonType = "text-link"))
        },
        actions = @FormActions(set = {@SetAction(field = "roleTypeId", value = "LEAD"), @SetAction(field = "partyTypeId", value = "PERSON"), @SetAction(field = "lookupFlag", value = "Y")}, service = {@ServiceAction(serviceName = "findParty")})
    )
    public interface MergeLeads {}

    @Form(
        name = "NewLeadFromVCard",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        type = FormType.UPLOAD,
        target = "createLeadFromVCard",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "infile", title = "${uiLabelMap.SfaUploadVCard}", file = @FileField),
            @FormField(name = "serviceName", hidden = @HiddenField(value = "createLead")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface NewLeadFromVCard {}

    @Form(
        name = "QuickAddLead",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "quickAddLead",
        attribs = "{'fieldsType':'default-compact'}",
        fields = {
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}*", requiredField = true, text = @TextField(size = 15)),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}*", requiredField = true, text = @TextField(size = 15)),
            @FormField(name = "groupName", title = "${uiLabelMap.CommonGroup}", text = @TextField(size = 15)),
            @FormField(name = "emailAddress", title = "${uiLabelMap.CommonEmail}", text = @TextField(size = 15)),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactList}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactList", description = "${groovy:contactListName.substring(0,Math.min(contactListName.length(), 12))}...", keyFieldName = "contactListId"))),
            @FormField(name = "quickAdd", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface QuickAddLead {}

    @Form(
        name = "AddLeadPartyDataSource",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "createLeadPartyDataSource",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyDataSource")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaLeadSource}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataSource", description = "${dataSourceId} - ${description}", keyFieldName = "dataSourceId", constraints = {@EntityConstraint(name = "dataSourceTypeId", value = "LEAD_SOURCE")}, orderBy = {@EntityOrderBy(fieldName = "dataSourceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddLeadPartyDataSource {}

    @Form(
        name = "ViewLeadPartyDataSources",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        type = FormType.LIST,
        listName = "partyDataSources",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "dataSourceId", title = "${uiLabelMap.SfaLeadSource}", displayEntity = @DisplayEntityField(entityName = "DataSource")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField)
        }
    )
    public interface ViewLeadPartyDataSources {}

    @Form(
        name = "FindLeads",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "${currentUrl}",
        extendsForm = "FindAccounts",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", textFind = @TextFindField),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", textFind = @TextFindField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "all"), @SortField(name = "groupName"), @SortField(name = "noConditionFind"), @SortField(name = "partyId"), @SortField(name = "firstName"), @SortField(name = "lastName"), @SortField(name = "groupName"), @SortField(name = "contactMechTypeId"), @SortField(name = "contactMechContainer"), @SortField(name = "submitAction")})
    )
    public interface FindLeads {}

    @Form(
        name = "listLeads",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        extendsForm = "listAccountsCommon",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        paginateTarget = "${currentUrl}",
        actions = @FormActions(set = {@SetAction(field = "roleTypeIdFrom", value = "OWNER"), @SetAction(field = "roleTypeIdTo", value = "LEAD"), @SetAction(field = "relatedCompanyRoleTypeIdTo", value = "LEAD"), @SetAction(field = "relatedCompanyRoleTypeIdFrom", value = "ACCOUNT_LEAD"), @SetAction(field = "relatedCompanyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "parameters.statusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.statusId_op", value = "notEqual"), @SetAction(field = "partyRelationshipTypeId", value = "LEAD_OWNER"), @SetAction(field = "parameters.roleTypeId", fromField = "roleTypeIdTo"), @SetAction(field = "fieldList", value = "${groovy:['partyId','roleTypeId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRoleAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface listLeads {}

    @Form(
        name = "ListLeads",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        extendsForm = "listLeads",
        fields = {
            @FormField(name = "assignToMe", title = "${uiLabelMap.SfaAssignToMe}", useWhen = "existRelationship==null&&!\"false\".equals(parameters.get(\"all\"))", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "createPartyRelationshipAndRole", description = "${uiLabelMap.SfaAssignToMe}", parameters = {@ParameterDef(paramName = "partyIdFrom", fromField = "userLogin.partyId"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "partyRelationshipTypeId"), @ParameterDef(paramName = "partyIdTo", fromField = "partyId")}))
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "partyId"), @SortField(name = "emailAddress"), @SortField(name = "telecomNumber"), @SortField(name = "city"), @SortField(name = "countryGeoId"), @SortField(name = "relatedCompany"), @SortField(name = "assignToMe")})
    )
    public interface ListLeadsLc {}

    @Form(
        name = "ListMyLeads",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        extendsForm = "ListLeads",
        fields = {
            @FormField(name = "assignToMe", hidden = @HiddenField)
        },
        actions = @FormActions(set = {@SetAction(field = "relatedCompanyRoleTypeIdTo", value = "LEAD"), @SetAction(field = "relatedCompanyRoleTypeIdFrom", value = "ACCOUNT_LEAD"), @SetAction(field = "relatedCompanyRelationshipTypeId", value = "EMPLOYMENT"), @SetAction(field = "parameters.partyIdFrom", fromField = "userLogin.partyId"), @SetAction(field = "parameters.roleTypeIdTo", value = "LEAD"), @SetAction(field = "parameters.partyStatusId", value = "PARTY_DISABLED"), @SetAction(field = "parameters.partyStatusId_op", value = "notEqual"), @SetAction(field = "parameters.partyRelationshipTypeId", value = "LEAD_OWNER"), @SetAction(field = "fieldList", value = "${groovy:['partyIdFrom','partyId']}", type = "List")}, service = {@ServiceAction(serviceName = "performFind", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRelationshipAndContactMechDetail"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "distinct", value = "Y")})})
    )
    public interface ListMyLeads {}

    @Form(
        name = "AddRelatedCompany",
        location = "component://marketing/widget/sfa/forms/LeadForms.xml",
        target = "createPartyRelationship",
        fields = {
            @FormField(name = "roleTypeIdTo", hidden = @HiddenField(value = "LEAD")),
            @FormField(name = "roleTypeIdFrom", hidden = @HiddenField(value = "ACCOUNT_LEAD")),
            @FormField(name = "partyRelationshipTypeId", hidden = @HiddenField(value = "EMPLOYMENT")),
            @FormField(name = "partyIdTo", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyRelatedCompany}", lookup = @LookupField(targetFormName = "LookupAccountLeads")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddRelatedCompany {}

}
