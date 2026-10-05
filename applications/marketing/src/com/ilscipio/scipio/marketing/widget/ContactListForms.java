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
public class ContactListForms {

    @Form(
        name = "EditContactList",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "updateContactList",
        defaultMapName = "contactList",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "contactList!=null", display = @DisplayField),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", useWhen = "contactList==null&&contactListId==null", ignored = @IgnoredField),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${contactListId}]", useWhen = "contactList==null&&contactListId!=null", display = @DisplayField),
            @FormField(name = "contactListName", title = "${uiLabelMap.MarketingContactListContactListName}", text = @TextField),
            @FormField(name = "contactListTypeId", title = "${uiLabelMap.MarketingContactListContactListTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactListType", description = "${description}", keyFieldName = "contactListTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "isPublic", title = "${uiLabelMap.MarketingContactListIsPublic}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "singleUse", title = "${uiLabelMap.MarketingContactListIsSingleUse}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", orderBy = {@EntityOrderBy(fieldName = "campaignName")}))),
            @FormField(name = "ownerPartyId", title = "${uiLabelMap.MarketingContactListOwnerPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName", defaultValue = "${userLogin.partyId}")),
            @FormField(name = "verifyEmailFrom", title = "${uiLabelMap.MarketingContactListVerifyEmailFrom}", text = @TextField(size = 40)),
            @FormField(name = "verifyEmailScreen", title = "${uiLabelMap.MarketingContactListVerifyEmailScreen}", text = @TextField(size = 60)),
            @FormField(name = "verifyEmailSubject", title = "${uiLabelMap.MarketingContactListVerifyEmailSubject}", text = @TextField(size = 60)),
            @FormField(name = "verifyEmailWebSiteId", title = "${uiLabelMap.MarketingContactListVerifyEmailWebSiteId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WebSite", description = "${siteName} [${webSiteId}]", keyFieldName = "webSiteId", orderBy = {@EntityOrderBy(fieldName = "siteName")}))),
            @FormField(name = "optOutScreen", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "contactList==null", target = "createContactList")
        }
    )
    public interface EditContactList {}

    @Form(
        name = "FindContactLists",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "FindContactLists",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", textFind = @TextFindField),
            @FormField(name = "contactListName", title = "${uiLabelMap.MarketingContactListContactListName}", textFind = @TextFindField),
            @FormField(name = "contactListTypeId", title = "${uiLabelMap.MarketingContactListContactListTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactListType", description = "${description}", keyFieldName = "contactListTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", orderBy = {@EntityOrderBy(fieldName = "campaignName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindContactLists {}

    @Form(
        name = "ListContactLists",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        target = "ListContactLists",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditContactList", description = "${contactListId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contactListId")})),
            @FormField(name = "contactListName", title = "${uiLabelMap.MarketingContactListContactListName}", display = @DisplayField),
            @FormField(name = "isPublic", title = "${uiLabelMap.MarketingContactListIsPublic}", display = @DisplayField),
            @FormField(name = "singleUse", title = "${uiLabelMap.MarketingContactListIsSingleUse}", display = @DisplayField),
            @FormField(name = "contactListTypeId", title = "${uiLabelMap.MarketingContactListContactListTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactListType")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "contactListId"), @FieldMap(fieldName = "entityName", value = "ContactList"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListContactLists {}

    @Form(
        name = "EditContactListParty",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "updateContactListParty",
        defaultMapName = "contactListParty",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingContactListPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(defaultValue = "${groovy: org.ofbiz.base.util.UtilDateTime.nowTimestamp()}")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "partyId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "partyId!=null", display = @DisplayField),
            @FormField(name = "fromDate", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "fromDate!=null", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "optInVerifyCode", mapName = "parameters", title = "${uiLabelMap.MarketingContactListOptInVerifyCode}", text = @TextField(size = 10)),
            @FormField(name = "preferredContactMechId", title = "${uiLabelMap.MarketingContactListPreferredContactMech}", lookup = @LookupField(targetFormName = "LookupPreferredContactMech")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", useWhen = "partyId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "contactListParty==null", target = "createContactListParty")
        }
    )
    public interface EditContactListParty {}

    @Form(
        name = "FindContactListParties",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "FindContactListParties",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingContactListPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateFind = @DateFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "preferredContactMechId", title = "${uiLabelMap.MarketingContactListPreferredContactMech}", lookup = @LookupField(targetFormName = "LookupContactMech")),
            @FormField(name = "hideExpired", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindContactListParties {}

    @Form(
        name = "ListContactListParties",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListContactListParties",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactListId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingContactListPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "preferredContactMechId", title = "${uiLabelMap.MarketingContactListPreferredContactMech}", displayEntity = @DisplayEntityField(entityName = "ContactMechDetail", keyFieldName = "contactMechId", description = "[${contactMechId}]: [${infoString}] [${tnCountryCode}-${tnAreaCode}-${tnContactNumber}] [${paAddress1}, ${paAddress1}, ${paCity}, ${paStateProvinceGeoId}, ${paPostalCode}, ${paPostalCodeExt} ${paCountryGeoId}]")),
            @FormField(name = "editAction", useWhen = "${groovy:thruDate == null || isDateAfterNow == true}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditContactListParty", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contactListId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "expireAction", useWhen = "${groovy:thruDate == null || isDateAfterNow == true}", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "expireContactListParty", description = "${uiLabelMap.CommonExpire}", alsoHidden = false, linkType = "hidden-form", parameters = {@ParameterDef(paramName = "contactListId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "donePage"), @ParameterDef(paramName = "thruDate", value = "${nowTimestamp}"), @ParameterDef(paramName = "hideExpired", value = "Y")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "filterByDate", fromField = "parameters.hideExpired"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "entityName", value = "ContactListParty"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "thruDate", fromField = "thruDate", type = "Timestamp"), @SetAction(field = "isDateAfterNow", value = "${groovy:org.ofbiz.base.util.UtilValidate.isDateAfterNow(thruDate)}", type = "Boolean")})
    )
    public interface ListContactListParties {}

    @Form(
        name = "FindImportContactListParties",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "FindImportContactListParties",
        focusFieldName = "contactMechTypeId",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", hidden = @HiddenField),
            @FormField(name = "statusId", hidden = @HiddenField(value = "PARTY_DISABLED")),
            @FormField(name = "statusId_op", hidden = @HiddenField(value = "notEqual")),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingContactListPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleType}", dropDown = @DropDownField(options = {@Option(description = "${uiLabelMap.CommonAnyRoleType}")}, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechTypeId", mapName = "contactList", event = "onfocus", action = "javascript:ajaxUpdateAreas('contactMechContainer,ContactMechTypeOnly,contactMechTypeId=' + this.value);this.disabled=true;", text = @TextField),
            @FormField(name = "contactMechContainer", title = " ", idName = "contactMechContainer", container = @ContainerField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindImportContactListParties {}

    @Form(
        name = "ListImportContactListParties",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.MULTI,
        target = "importContactListParties?contactListId=${parameters.contactListId}",
        listName = "listIt",
        paginateTarget = "FindImportContactListParties",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "partyId", title = "${uiLabelMap.MarketingContactListPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "toName", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "attnName", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "address1", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "address2", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "city", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "abbreviation", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "postalCode", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", display = @DisplayField),
            @FormField(name = "countryGeoId", mapName = "postalAddress", useWhen = "contactMechTypeId.equals(\"POSTAL_ADDRESS\")", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName}")),
            @FormField(name = "countryCode", mapName = "telephone", useWhen = "contactMechTypeId.equals(\"TELECOM_NUMBER\")", display = @DisplayField),
            @FormField(name = "areaCode", mapName = "telephone", useWhen = "contactMechTypeId.equals(\"TELECOM_NUMBER\")", display = @DisplayField),
            @FormField(name = "contactNumber", mapName = "telephone", useWhen = "contactMechTypeId.equals(\"TELECOM_NUMBER\")", display = @DisplayField),
            @FormField(name = "extension", mapName = "telephone", useWhen = "contactMechTypeId.equals(\"TELECOM_NUMBER\")", display = @DisplayField),
            @FormField(name = "infoString", mapName = "contactMech", useWhen = "!contactMechTypeId.equals(\"POSTAL_ADDRESS\")&&!contactMechTypeId.equals(\"TELECOM_NUMBER\")", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabel.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_copy}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.contactMechTypeId", fromField = "contactMechTypeId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyRoleAndContactMechDetail"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "fieldList", fromField = "selectedFields"), @FieldMap(fieldName = "distinct", value = "Y")})}),
        rowActions = @RowActions(set = {@SetAction(field = "contactMech", fromField = "contactMechResults.valueMaps[0].contactMech", type = "Object")}, service = {@ServiceAction(serviceName = "getPartyPostalAddress", resultMapName = "postalAddress", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")}), @ServiceAction(serviceName = "getPartyTelephone", resultMapName = "telephone", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")}), @ServiceAction(serviceName = "getPartyContactMechValueMaps", resultMapName = "contactMechResults", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "contactMechTypeId", fromField = "contactMechTypeId")})})
    )
    public interface ListImportContactListParties {}

    @Form(
        name = "EditContactListCommEvent",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "updateContactListCommEvent",
        defaultMapName = "communicationEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", useWhen = "communicationEvent!=null", display = @DisplayField),
            @FormField(name = "communicationEventTypeId", mapName = "communicationEventType", hidden = @HiddenField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "communicationEvent==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "communicationEvent!=null", dropDown = @DropDownField(currentDescription = "${uiLabelMap.CommonSelectOne}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${communicationEvent.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", useWhen = "commEventContactMechType!=null&&parentCommEventContactMechType==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId"))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", useWhen = "commEventContactMechType==null&&parentCommEventContactMechType!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId"))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", useWhen = "commEventContactMechType==null&&parentCommEventContactMechType==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId"))),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", useWhen = "commEventRoleTypeIdFrom!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyRoleTypeIdTo}", useWhen = "commEventRoleTypeIdTo!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechIdFrom", title = "${uiLabelMap.PartyFromContactMech}", lookup = @LookupField(targetFormName = "LookupPreferredContactMech")),
            @FormField(name = "contactListId", lookup = @LookupField(targetFormName = "LookupContactList", size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateTime = @DateTimeField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", dateTime = @DateTimeField),
            @FormField(name = "subject", title = "${uiLabelMap.PartySubject}", text = @TextField(size = 50)),
            @FormField(name = "contentMimeTypeId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "MimeType", description = "${mimeTypeId}", keyFieldName = "mimeTypeId", orderBy = {@EntityOrderBy(fieldName = "mimeTypeId")}))),
            @FormField(name = "content", title = "${uiLabelMap.CommonContent} -- ${communicationEventType.description}", textarea = @TextareaField(rows = 10, visualEditorEnable = true)),
            @FormField(name = "note", title = "${uiLabelMap.CommonNote}", textarea = @TextareaField(rows = 3)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "communicationEvent==null", target = "createContactListCommEvent")
        },
        actions = @FormActions(set = {@SetAction(field = "contentMimeTypeId", fromField = "communicationEvent.contentMimeTypeId", defaultValue = "text/html")}, entityOne = {@EntityOneAction(entityName = "CommunicationEventType", valueField = "communicationEventType")}),
        sortOrder = @SortOrder()
    )
    public interface EditContactListCommEvent {}

    @Form(
        name = "FindContactListCommEvents",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "ListContactListCommEvents",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", text = @TextField),
            @FormField(name = "commEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", lookup = @LookupField(targetFormName = "LookupCommEvent")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateFind = @DateFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindContactListCommEvents {}

    @Form(
        name = "ListContactListCommEvents",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListContactListCommEvents",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditContactListCommEvent", description = "${communicationEventId}", parameters = {@ParameterDef(paramName = "contactListId"), @ParameterDef(paramName = "communicationEventId"), @ParameterDef(paramName = "DONE_PAGE", fromField = "donePage")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyRoleTypeIdTo}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "custRequestId", display = @DisplayField),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField),
            @FormField(name = "subject", mapName = "subjectMap", title = "${uiLabelMap.PartyCommEventSubject}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CommunicationEvent"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListContactListCommEvents {}

    @Form(
        name = "LookupContactList",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "LookupContactList",
        defaultMapName = "contactList",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", textFind = @TextFindField),
            @FormField(name = "contactListName", title = "${uiLabelMap.MarketingContactListContactListName}", textFind = @TextFindField),
            @FormField(name = "contactListTypeId", title = "${uiLabelMap.MarketingContactListContactListTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactListType", description = "${description}", keyFieldName = "contactListTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", orderBy = {@EntityOrderBy(fieldName = "campaignName")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupContactList {}

    @Form(
        name = "ListLookupContactList",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactListId", title = "${uiLabelMap.MarketingContactListContactListId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactListId}')", urlMode = UrlMode.PLAIN, description = "${contactListId}", alsoHidden = false)),
            @FormField(name = "contactListName", title = "${uiLabelMap.MarketingContactListContactListName}", display = @DisplayField),
            @FormField(name = "contactListTypeId", title = "${uiLabelMap.MarketingContactListContactListTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactListType")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ContactList"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupContactList {}

    @Form(
        name = "LookupCommEvent",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "LookupCommEvent",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "COM_EVENT_STATUS")}))),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContactMechType", description = "${description}", keyFieldName = "contactMechTypeId"))),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "custRequestId", lookup = @LookupField(targetFormName = "LookupCustRequest", size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", dateFind = @DateFindField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", dateFind = @DateFindField),
            @FormField(name = "subject", mapName = "subjectMap", title = "${uiLabelMap.PartyCommEventSubject}", textFind = @TextFindField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupCommEvent {}

    @Form(
        name = "ListLookupCommEvent",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", title = "${uiLabelMap.MarketingContactListCommEventId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${communicationEventId}')", urlMode = UrlMode.PLAIN, description = "${communicationEventId}", alsoHidden = false)),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "Person", keyFieldName = "partyId", description = "${firstName} ${lastName} [${partyId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType", keyFieldName = "contactMechTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyRoleTypeIdFrom}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyRoleTypeIdTo}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "custRequestId", text = @TextField(size = 20)),
            @FormField(name = "datetimeStarted", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "datetimeEnded", title = "${uiLabelMap.CommonFinishDate}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupCommEvent {}

    @Form(
        name = "ListPreferredContactMech",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactMechId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactMechId}')", urlMode = UrlMode.PLAIN, description = "${contactMechId}", alsoHidden = false)),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.MarketingContactListContactMechTypeId}", displayEntity = @DisplayEntityField(entityName = "ContactMechType")),
            @FormField(name = "cmDetail", display = @DisplayField(description = "[${infoString}] [${tnCountryCode}-${tnAreaCode}-${tnContactNumber}] [${paAddress1}, ${paAddress2}, ${paCity}, ${paStateProvinceGeoId}, ${paPostalCode}, ${paPostalCodeExt} ${paCountryGeoId}]", alsoHidden = false))
        }
    )
    public interface ListPreferredContactMech {}

    @Form(
        name = "ListContactListCommStatuses",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "communicationEventId", hidden = @HiddenField),
            @FormField(name = "contactListId", displayEntity = @DisplayEntityField(entityName = "ContactList", description = "${contactListName}[contactListId]")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "contactMechId", title = "${uiLabelMap.CommonEmailTo}", displayEntity = @DisplayEntityField(entityName = "ContactMech", description = "${infoString}")),
            @FormField(name = "lastUpdatedStamp", title = "${uiLabelMap.FormFieldTitle_lastModifiedDate}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}"))
        }
    )
    public interface ListContactListCommStatuses {}

    @Form(
        name = "CreateWebSiteContactList",
        location = "component://marketing/widget/ContactListForms.xml",
        target = "createWebSiteContactList",
        defaultMapName = "contactList",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", display = @DisplayField),
            @FormField(name = "contactListName", display = @DisplayField),
            @FormField(name = "fromDate", hidden = @HiddenField(value = "${fromDate}")),
            @FormField(name = "webSiteId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WebSite", description = "${siteName} [${webSiteId}]", keyFieldName = "webSiteId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "fromDate", value = "${groovy: import org.ofbiz.base.util.UtilDateTime; return UtilDateTime.nowTimestamp();}", type = "Timestamp")})
    )
    public interface CreateWebSiteContactList {}

    @Form(
        name = "ViewWebSiteContactList",
        location = "component://marketing/widget/ContactListForms.xml",
        type = FormType.LIST,
        target = "updateWebSiteContactList",
        listName = "webSiteContactLists",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", hidden = @HiddenField),
            @FormField(name = "webSiteId", hidden = @HiddenField),
            @FormField(name = "siteName", displayEntity = @DisplayEntityField(entityName = "WebSite", keyFieldName = "webSiteId", subHyperlink = @SubHyperlink(target = "/content/control/EditWebSite", description = "[${webSiteId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "webSiteId")}))),
            @FormField(name = "fromDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "thruDate", dateTime = @DateTimeField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWebSiteContactList", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "webSiteId"), @ParameterDef(paramName = "contactListId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "WebSite", valueField = "webSite")}),
        rowActions = @RowActions(set = {@SetAction(field = "siteName", fromField = "webSite.siteName")}, entityOne = {@EntityOneAction(entityName = "WebSite", valueField = "webSite")})
    )
    public interface ViewWebSiteContactList {}

}
