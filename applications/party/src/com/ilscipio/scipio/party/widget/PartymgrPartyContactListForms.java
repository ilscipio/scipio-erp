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
public class PartymgrPartyContactListForms {

    @Form(
        name = "ListPartyContactLists",
        location = "component://party/widget/partymgr/PartyContactListForms.xml",
        type = FormType.LIST,
        target = "updateContactListParty",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "contactListId", title = "${uiLabelMap.PartyContactLists}", displayEntity = @DisplayEntityField(entityName = "ContactList", description = "${contactListName}", subHyperlink = @SubHyperlink(target = "/marketing/control/EditContactList", description = "${contactListId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "contactListId")}))),
            @FormField(name = "fromDate", display = @DisplayField),
            @FormField(name = "statusHistory", dropDown = @DropDownField(options = {@Option(description = "-- ${uiLabelMap.CommonStatusHistory} --")}, entityOptions = @EntityOptions(entityName = "ContactListPartyAndStatus", description = "${statusDate} ${description} [${uiLabelMap.CommonBy}: ${setByUserLoginId}] [${uiLabelMap.FormFieldTitle_optInVerifyCode}: ${optInVerifyCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "partyId", envName = "partyId"), @EntityConstraint(name = "contactListId", envName = "contactListId"), @EntityConstraint(name = "fromDate", envName = "fromDate")}, orderBy = {@EntityOrderBy(fieldName = "-statusDate")}))),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "optInVerifyCode", text = @TextField(size = 10)),
            @FormField(name = "preferredContactMechId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyAndContactMech", description = "[${contactMechId}] [${infoString}] [${tnCountryCode}-${tnAreaCode}-${tnContactNumber}] [${paAddress1}, ${paAddress2}, ${paCity}, ${paStateProvinceGeoId}, ${paPostalCode}, ${paPostalCodeExt} ${paCountryGeoId}]", keyFieldName = "contactMechId", constraints = {@EntityConstraint(name = "partyId", envName = "partyId")}, orderBy = {@EntityOrderBy(fieldName = "contactMechId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListPartyContactLists {}

    @Form(
        name = "AddPartyContactList",
        location = "component://party/widget/partymgr/PartyContactListForms.xml",
        target = "createContactListParty",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "contactListId", lookup = @LookupField(targetFormName = "LookupContactList")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTACTLST_PARTY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "preferredContactMechId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyAndContactMech", description = "[${contactMechId}] [${infoString}] [${tnCountryCode}-${tnAreaCode}-${tnContactNumber}] [${paAddress1}, ${paAddress2}, ${paCity}, ${paStateProvinceGeoId}, ${paPostalCode}, ${paPostalCodeExt} ${paCountryGeoId}]", keyFieldName = "contactMechId", constraints = {@EntityConstraint(name = "partyId", envName = "partyId")}, orderBy = {@EntityOrderBy(fieldName = "contactMechId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddPartyContactList {}

    @Form(
        name = "ListLookupContactList",
        location = "component://party/widget/partymgr/PartyContactListForms.xml",
        type = FormType.LIST,
        target = "LookupContactList",
        defaultMapName = "contactList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "contactListId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${contactListId}')", urlMode = UrlMode.PLAIN, description = "${contactListId}", alsoHidden = false)),
            @FormField(name = "contactListName", display = @DisplayField),
            @FormField(name = "contactListTypeId", displayEntity = @DisplayEntityField(entityName = "ContactListType", description = "${description}")),
            @FormField(name = "contactMechTypeId", title = "${uiLabelMap.PartyContactMechType}", displayEntity = @DisplayEntityField(entityName = "ContactMechType", description = "${description}")),
            @FormField(name = "marketingCampaignId", displayEntity = @DisplayEntityField(entityName = "MarketingCampaign", description = "${campaignName}"))
        }
    )
    public interface ListLookupContactList {}

}
