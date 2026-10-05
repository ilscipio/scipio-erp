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
package com.ilscipio.scipio.marketing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    /**
     * Create a MarketingCampaign record
     */
    @Service(
        name = "createMarketingCampaign",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a MarketingCampaign record",
        defaultEntityName = "MarketingCampaign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateMarketingCampaign {}

    /**
     * Update a MarketingCampaign record
     */
    @Service(
        name = "updateMarketingCampaign",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a MarketingCampaign record",
        defaultEntityName = "MarketingCampaign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateMarketingCampaign {}

    /**
     * Remove a MarketingCampaign record
     */
    @Service(
        name = "deleteMarketingCampaign",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a MarketingCampaign record",
        defaultEntityName = "MarketingCampaign",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteMarketingCampaign {}

    @Service(
        name = "addPriceRuleToMarketingCampaign",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "MarketingCampaignPrice",
        auth = "true",
        implemented = {@Implements(service = "createMarketingCampaignPrice")}
    )
    public interface AddPriceRuleToMarketingCampaign {}

    /**
     * Add PriceRule to MarketingCampaign
     */
    @Service(
        name = "createMarketingCampaignPrice",
        engine = "entity-auto",
        invoke = "create",
        description = "Add PriceRule to MarketingCampaign",
        defaultEntityName = "MarketingCampaignPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMarketingCampaignPrice {}

    @Service(
        name = "removePriceRuleFromMarketingCampaign",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "MarketingCampaignPrice",
        auth = "true",
        implemented = {@Implements(service = "deleteMarketingCampaignPrice")}
    )
    public interface RemovePriceRuleFromMarketingCampaign {}

    /**
     * Update PriceRule to MarketingCampaign
     */
    @Service(
        name = "updateMarketingCampaignPrice",
        engine = "entity-auto",
        invoke = "update",
        description = "Update PriceRule to MarketingCampaign",
        defaultEntityName = "MarketingCampaignPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMarketingCampaignPrice {}

    /**
     * Remove PriceRule from MarketingCampaign
     */
    @Service(
        name = "deleteMarketingCampaignPrice",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove PriceRule from MarketingCampaign",
        defaultEntityName = "MarketingCampaignPrice",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteMarketingCampaignPrice {}

    @Service(
        name = "addPromoToMarketingCampaign",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "MarketingCampaignPromo",
        auth = "true",
        implemented = {@Implements(service = "createMarketingCampaignPromo")}
    )
    public interface AddPromoToMarketingCampaign {}

    /**
     * Add Promo to MarketingCampaign
     */
    @Service(
        name = "createMarketingCampaignPromo",
        engine = "entity-auto",
        invoke = "create",
        description = "Add Promo to MarketingCampaign",
        defaultEntityName = "MarketingCampaignPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMarketingCampaignPromo {}

    @Service(
        name = "removePromoFromMarketingCampaign",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "MarketingCampaignPromo",
        auth = "true",
        implemented = {@Implements(service = "deleteMarketingCampaignPromo")}
    )
    public interface RemovePromoFromMarketingCampaign {}

    /**
     * Update Promo to MarketingCampaign
     */
    @Service(
        name = "updateMarketingCampaignPromo",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Promo to MarketingCampaign",
        defaultEntityName = "MarketingCampaignPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMarketingCampaignPromo {}

    /**
     * Remove Promo to MarketingCampaign
     */
    @Service(
        name = "deleteMarketingCampaignPromo",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove Promo to MarketingCampaign",
        defaultEntityName = "MarketingCampaignPromo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteMarketingCampaignPromo {}

    /**
     * Signs an input email up for a ContactList with _NA_ party using the system userLogin.             The intent is for anonymous sign ups to email lists.  Also validates email format.
     */
    @Service(
        name = "signUpForContactList",
        engine = "java",
        location = "org.ofbiz.marketing.marketing.MarketingServices",
        invoke = "signUpForContactList",
        description = "Signs an input email up for a ContactList with _NA_ party using the system userLogin.\n            The intent is for anonymous sign ups to email lists.  Also validates email format.",
        attributes = {
            @Attribute(name = "contactListId", type = "String", mode = "IN"),
            @Attribute(name = "email", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SignUpForContactList {}

    /**
     * Unsubscribe an input email for a ContactList with _NA_ party using the system userLogin.             The intent is for anonymous unsubscribe to email lists.  Also validates email format.
     */
    @Service(
        name = "unsubscribeContactListParty",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "unsubscribeContactListParty",
        description = "Unsubscribe an input email for a ContactList with _NA_ party using the system userLogin.\n            The intent is for anonymous unsubscribe to email lists.  Also validates email format.",
        attributes = {
            @Attribute(name = "contactListId", type = "String", mode = "IN"),
            @Attribute(name = "email", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UnsubscribeContactListParty {}

    /**
     * Find email by contactMechId then call unsubscribeContactListParty service.
     */
    @Service(
        name = "unsubscribeContactListPartyContachMech",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "unsubscribeContactListPartyContachMech",
        description = "Find email by contactMechId then call unsubscribeContactListParty service.",
        attributes = {
            @Attribute(name = "contactListId", type = "String", mode = "IN"),
            @Attribute(name = "preferredContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UnsubscribeContactListPartyContachMech {}

    /**
     * Add Role to Campaign
     */
    @Service(
        name = "createMarketingCampaignRole",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/campaign/CampaignServices.xml",
        invoke = "createMarketingCampaignRole",
        description = "Add Role to Campaign",
        defaultEntityName = "MarketingCampaignRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateMarketingCampaignRole {}

    /**
     * Update Role to Campaign
     */
    @Service(
        name = "updateMarketingCampaignRole",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Role to Campaign",
        defaultEntityName = "MarketingCampaignRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateMarketingCampaignRole {}

    /**
     * Remove Role from Campaign
     */
    @Service(
        name = "deleteMarketingCampaignRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove Role from Campaign",
        defaultEntityName = "MarketingCampaignRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteMarketingCampaignRole {}

    /**
     * Create a ContactList record
     */
    @Service(
        name = "createContactList",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "createContactList",
        description = "Create a ContactList record",
        defaultEntityName = "ContactList",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "contactListTypeId", optional = "false"),
            @OverrideAttribute(name = "contactListName", optional = "false")
        }
    )
    public interface CreateContactList {}

    /**
     * Update a ContactList record
     */
    @Service(
        name = "updateContactList",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContactList record",
        defaultEntityName = "ContactList",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContactList {}

    /**
     * Remove a ContactList record
     */
    @Service(
        name = "removeContactList",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a ContactList record",
        defaultEntityName = "ContactList",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveContactList {}

    /**
     * Add Party to ContactList
     */
    @Service(
        name = "createContactListParty",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "createContactListParty",
        description = "Add Party to ContactList",
        defaultEntityName = "ContactListParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "statusId", optional = "false")
        }
    )
    public interface CreateContactListParty {}

    /**
     * Update Party to ContactList Join
     */
    @Service(
        name = "updateContactListParty",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updateContactListParty",
        description = "Update Party to ContactList Join",
        defaultEntityName = "ContactListParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactListId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "optInVerifyCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface UpdateContactListParty {}

    /**
     * Update Party to ContactList Join
     */
    @Service(
        name = "updateContactListPartyNoUserLogin",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updateContactListPartyNoUserLogin",
        description = "Update Party to ContactList Join",
        defaultEntityName = "ContactListParty",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "optInVerifyCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "email", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyId", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface UpdateContactListPartyNoUserLogin {}

    /**
     * Update ContactList Party Contact Mech
     */
    @Service(
        name = "updatePartyEmailContactListParty",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updatePartyEmailContactListParty",
        description = "Update ContactList Party Contact Mech",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN")
        }
    )
    public interface UpdatePartyEmailContactListParty {}

    /**
     * Remove Party from ContactList
     */
    @Service(
        name = "deleteContactListParty",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove Party from ContactList",
        defaultEntityName = "ContactListParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContactListParty {}

    /**
     * Create ContactListParty Status
     */
    @Service(
        name = "createContactListPartyStatus",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "createContactListPartyStatus",
        description = "Create ContactListParty Status",
        defaultEntityName = "ContactListPartyStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", excludeFields = {"statusDate"}),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"setByUserLoginId"})
        },
        attributes = {
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredContactMechId", type = "String", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "statusId", optional = "false"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateContactListPartyStatus {}

    /**
     * Send ContactListParty Verify Email
     */
    @Service(
        name = "sendContactListPartyVerifyEmail",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "sendContactListPartyVerifyEmail",
        description = "Send ContactListParty Verify Email",
        auth = "true",
        maxRetry = "3",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactListParty", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendContactListPartyVerifyEmail {}

    /**
     * Uses the communication event information to locate the contact list party information and removes the contact from the list
     */
    @Service(
        name = "optOutOfListFromCommEvent",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "optOutOfListFromCommEvent",
        description = "Uses the communication event information to locate the contact list party information and removes the contact from the list",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "contactListId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface OptOutOfListFromCommEvent {}

    @Service(
        name = "sendContactListPartySubscribeEmail",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "sendContactListPartySubscribeEmail",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendContactListPartySubscribeEmail {}

    @Service(
        name = "sendContactListPartyUnSubscribeVerifyEmail",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "sendContactListPartyUnSubscribeVerifyEmail",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "baseLocation", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendContactListPartyUnSubscribeVerifyEmail {}

    @Service(
        name = "sendContactListPartyUnSubscribeEmail",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "sendContactListPartyUnSubscribeEmail",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "preferredContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendContactListPartyUnSubscribeEmail {}

    @Service(
        name = "updateContactListCommStatus",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updateContactListCommStatus",
        defaultEntityName = "ContactListCommStatus",
        entityAttributes = {
            @EntityAttributes(mode = "IN", excludeFields = {"changeByUserLoginId"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "partyId", optional = "true"),
            @OverrideAttribute(name = "messageId", optional = "true", allowHtml = "any")
        }
    )
    public interface UpdateContactListCommStatus {}

    @Service(
        name = "updateCommStatusFromCommEvent",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updateCommStatusFromCommEvent",
        attributes = {
            @Attribute(name = "communicationEventId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN")
        }
    )
    public interface UpdateCommStatusFromCommEvent {}

    @Service(
        name = "createWebSiteContactList",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "createWebSiteContactList",
        defaultEntityName = "WebSiteContactList",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateWebSiteContactList {}

    @Service(
        name = "updateWebSiteContactList",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "updateWebSiteContactList",
        defaultEntityName = "WebSiteContactList",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateWebSiteContactList {}

    @Service(
        name = "deleteWebSiteContactList",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml",
        invoke = "deleteWebSiteContactList",
        defaultEntityName = "WebSiteContactList",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteWebSiteContactList {}

    /**
     * Create a TrackingCode record
     */
    @Service(
        name = "createTrackingCode",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a TrackingCode record",
        defaultEntityName = "TrackingCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "trackingCodeTypeId", optional = "false")
        }
    )
    public interface CreateTrackingCode {}

    /**
     * Update a TrackingCode record
     */
    @Service(
        name = "updateTrackingCode",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a TrackingCode record",
        defaultEntityName = "TrackingCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTrackingCode {}

    /**
     * Delete a TrackingCode record
     */
    @Service(
        name = "deleteTrackingCode",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a TrackingCode record",
        defaultEntityName = "TrackingCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTrackingCode {}

    /**
     * Create a TrackingCodeType record
     */
    @Service(
        name = "createTrackingCodeType",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "createTrackingCodeType",
        description = "Create a TrackingCodeType record",
        defaultEntityName = "TrackingCodeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateTrackingCodeType {}

    /**
     * Update a TrackingCodeType record
     */
    @Service(
        name = "updateTrackingCodeType",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "updateTrackingCodeType",
        description = "Update a TrackingCodeType record",
        defaultEntityName = "TrackingCodeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateTrackingCodeType {}

    /**
     * Update a TrackingCodeType record
     */
    @Service(
        name = "deleteTrackingCodeType",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "deleteTrackingCodeType",
        description = "Update a TrackingCodeType record",
        defaultEntityName = "TrackingCodeType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteTrackingCodeType {}

    /**
     * Create a SegmentGroup
     */
    @Service(
        name = "createSegmentGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SegmentGroup",
        defaultEntityName = "SegmentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateSegmentGroup {}

    /**
     * Update a SegmentGroup
     */
    @Service(
        name = "updateSegmentGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SegmentGroup",
        defaultEntityName = "SegmentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateSegmentGroup {}

    /**
     * Delete a SegmentGroup
     */
    @Service(
        name = "deleteSegmentGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SegmentGroup",
        defaultEntityName = "SegmentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteSegmentGroup {}

    /**
     * Create a SegmentGroupClassification
     */
    @Service(
        name = "createSegmentGroupClassification",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SegmentGroupClassification",
        defaultEntityName = "SegmentGroupClassification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateSegmentGroupClassification {}

    /**
     * Update a SegmentGroupClassification
     */
    @Service(
        name = "updateSegmentGroupClassification",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SegmentGroupClassification",
        defaultEntityName = "SegmentGroupClassification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateSegmentGroupClassification {}

    /**
     * Delete a SegmentGroupClassification
     */
    @Service(
        name = "deleteSegmentGroupClassification",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SegmentGroupClassification",
        defaultEntityName = "SegmentGroupClassification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteSegmentGroupClassification {}

    /**
     * Create a SegmentGroupGeo
     */
    @Service(
        name = "createSegmentGroupGeo",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SegmentGroupGeo",
        defaultEntityName = "SegmentGroupGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateSegmentGroupGeo {}

    /**
     * Update a SegmentGroupGeo
     */
    @Service(
        name = "updateSegmentGroupGeo",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SegmentGroupGeo",
        defaultEntityName = "SegmentGroupGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateSegmentGroupGeo {}

    /**
     * Delete a SegmentGroupGeo
     */
    @Service(
        name = "deleteSegmentGroupGeo",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SegmentGroupGeo",
        defaultEntityName = "SegmentGroupGeo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteSegmentGroupGeo {}

    /**
     * Create a SegmentGroupRole
     */
    @Service(
        name = "createSegmentGroupRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SegmentGroupRole",
        defaultEntityName = "SegmentGroupRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateSegmentGroupRole {}

    /**
     * Update a SegmentGroupRole
     */
    @Service(
        name = "updateSegmentGroupRole",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SegmentGroupRole",
        defaultEntityName = "SegmentGroupRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateSegmentGroupRole {}

    /**
     * Delete a SegmentGroupRole
     */
    @Service(
        name = "deleteSegmentGroupRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SegmentGroupRole",
        defaultEntityName = "SegmentGroupRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteSegmentGroupRole {}

    /**
     * Determine: are Parties Related Through SegmentGroup?
     */
    @Service(
        name = "arePartiesRelatedThroughSegmentGroup",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/segment/SegmentServices.xml",
        invoke = "arePartiesRelatedThroughSegmentGroup",
        description = "Determine: are Parties Related Through SegmentGroup?",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "toPartyId", type = "String", mode = "IN"),
            @Attribute(name = "toRoleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "areRelated", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "VIEW")
    )
    public interface ArePartiesRelatedThroughSegmentGroup {}

    /**
     * Create a TrackingCodeOrder record
     */
    @Service(
        name = "createTrackingCodeOrder",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "createTrackingCodeOrder",
        description = "Create a TrackingCodeOrder record",
        defaultEntityName = "TrackingCodeOrder",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateTrackingCodeOrder {}

    /**
     * Update a TrackingCode record
     */
    @Service(
        name = "updateTrackingCodeOrder",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "updateTrackingCodeOrder",
        description = "Update a TrackingCode record",
        defaultEntityName = "TrackingCodeOrder",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateTrackingCodeOrder {}

    /**
     * Create a TrackingCodeOrderReturn  record
     */
    @Service(
        name = "createTrackingCodeOrderReturn",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "createTrackingCodeOrderReturn",
        description = "Create a TrackingCodeOrderReturn  record",
        defaultEntityName = "TrackingCodeOrderReturn",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateTrackingCodeOrderReturn {}

    /**
     * Update a TrackingCode record
     */
    @Service(
        name = "updateTrackingCodeOrderReturn",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "updateTrackingCodeOrderReturn",
        description = "Update a TrackingCode record",
        defaultEntityName = "TrackingCodeOrderReturn",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "UPDATE")
    )
    public interface UpdateTrackingCodeOrderReturn {}

    /**
     * Update a TrackingCode record
     */
    @Service(
        name = "deleteTrackingCodeOrderReturn",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "deleteTrackingCodeOrderReturn",
        description = "Update a TrackingCode record",
        defaultEntityName = "TrackingCodeOrderReturn",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "DELETE")
    )
    public interface DeleteTrackingCodeOrderReturn {}

    /**
     * Create TrackingCodeOrderReturn for all the Return Items with Orders that have trackingCodeOrder entry
     */
    @Service(
        name = "createTrackingCodeOrderReturns",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml",
        invoke = "createTrackingCodeOrderReturns",
        description = "Create TrackingCodeOrderReturn for all the Return Items with Orders that have trackingCodeOrder entry",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "marketingPermissionService", mainAction = "CREATE")
    )
    public interface CreateTrackingCodeOrderReturns {}

    @Service(
        name = "marketingPermissionService",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "genericBasePermissionCheck",
        implemented = {@Implements(service = "permissionInterface")},
        attributes = {
            @Attribute(name = "primaryPermission", type = "String", mode = "IN", optional = "true", defaultValue = "MARKETING"),
            @Attribute(name = "altPermission", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface MarketingPermissionService {}

    @Service(
        name = "marketingManagerPermission",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml",
        invoke = "genericBasePermissionCheck",
        implemented = {@Implements(service = "marketingPermissionService")}
    )
    public interface MarketingManagerPermission {}

    /**
     *              Sales Lead can be just a person or a person representing a company or a company (party group).             createLead works 1) If person information is passed. 2) If company (party group) information is passed. 3) If Person and company (party group) information is passed.          
     */
    @Service(
        name = "createLead",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/sfa/lead/LeadServices.xml",
        invoke = "createLead",
        description = "\n            Sales Lead can be just a person or a person representing a company or a company (party group).\n            createLead works 1) If person information is passed. 2) If company (party group) information is passed. 3) If Person and company (party group) information is passed. \n        ",
        entityAttributes = {
            @EntityAttributes(entityName = "Person", mode = "IN", optional = "true", excludeFields = {"partyId"}),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", optional = "true", excludeFields = {"contactMechId"}),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", optional = "true", excludeFields = {"contactMechId"})
        },
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "title", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "numEmployees", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "officeSiteName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dataSourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "extension", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "partyGroupPartyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateLead {}

    /**
     * Create a Contact Person
     */
    @Service(
        name = "createContact",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/sfa/contact/ContactServices.xml",
        invoke = "createContact",
        description = "Create a Contact Person",
        entityAttributes = {
            @EntityAttributes(entityName = "Person", mode = "IN", optional = "true", excludeFields = {"partyId"}),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", optional = "true", excludeFields = {"contactMechId"}),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", optional = "true", excludeFields = {"contactMechId"})
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "OUT"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quickAdd", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "extension", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactListId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateContact {}

    /**
     * This service merges the contact details of two parties, partyId merges into partyIdTo
     */
    @Service(
        name = "mergeContacts",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/sfa/contact/ContactServices.xml",
        invoke = "mergeContacts",
        description = "This service merges the contact details of two parties, partyId merges into partyIdTo",
        attributes = {
            @Attribute(name = "addrContactMechIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "phoneContactMechIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailContactMechIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "addrContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "phoneContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdTo", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "INOUT"),
            @Attribute(name = "useAddress2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useContactNum2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useEmail2", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface MergeContacts {}

    /**
     * Create an Account Group
     */
    @Service(
        name = "createAccount",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/sfa/account/AccountServices.xml",
        invoke = "createAccount",
        description = "Create an Account Group",
        entityAttributes = {
            @EntityAttributes(entityName = "PartyGroup", mode = "IN", optional = "true", excludeFields = {"partyId"}),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", optional = "true", excludeFields = {"contactMechId"}),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", optional = "true", excludeFields = {"contactMechId"})
        },
        attributes = {
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "extension", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT")
        }
    )
    public interface CreateAccount {}

    @Service(
        name = "convertLeadToContact",
        engine = "simple",
        location = "component://marketing/script/org/ofbiz/sfa/lead/LeadServices.xml",
        invoke = "convertLeadToContact",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "INOUT"),
            @Attribute(name = "partyGroupId", type = "String", mode = "INOUT")
        }
    )
    public interface ConvertLeadToContact {}

    @Service(
        name = "importVCard",
        engine = "java",
        location = "org.ofbiz.sfa.vcard.VCard",
        invoke = "importVCard",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "infile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "partyType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "serviceContext", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "serviceName", type = "String", mode = "IN"),
            @Attribute(name = "partiesCreated", type = "List", mode = "OUT"),
            @Attribute(name = "partiesExist", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ImportVCard {}

    @Service(
        name = "exportVCard",
        engine = "java",
        location = "org.ofbiz.sfa.vcard.VCard",
        invoke = "exportVCard",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN")
        }
    )
    public interface ExportVCard {}

    /**
     * Create a MarketingCampaignNote
     */
    @Service(
        name = "createMarketingCampaignNote",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a MarketingCampaignNote",
        defaultEntityName = "MarketingCampaignNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateMarketingCampaignNote {}

    /**
     * Delete a MarketingCampaignNote
     */
    @Service(
        name = "deleteMarketingCampaignNote",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a MarketingCampaignNote",
        defaultEntityName = "MarketingCampaignNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteMarketingCampaignNote {}

    /**
     * Create SegmentGroupType
     */
    @Service(
        name = "createSegmentGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create SegmentGroupType",
        defaultEntityName = "SegmentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSegmentGroupType {}

    /**
     * Update SegmentGroupType
     */
    @Service(
        name = "updateSegmentGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update SegmentGroupType",
        defaultEntityName = "SegmentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSegmentGroupType {}

    /**
     * Delete SegmentGroupType
     */
    @Service(
        name = "deleteSegmentGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete SegmentGroupType",
        defaultEntityName = "SegmentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSegmentGroupType {}

}
