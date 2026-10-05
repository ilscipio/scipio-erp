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
package com.ilscipio.scipio.compliance.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Service definitions of the compliance component.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComplianceServices {

    @Service(
        name = "publishLegalDocument",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.ComplianceServiceImpl",
        invoke = "publishLegalDocument",
        description = "Publishes a new version of a legal text of a store. Without bodyText the shipped template of the locale is published. "
                + "Archives the previous published version of the same store, type and locale, and saves the current service-list hash.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "docTypeId", type = "String", mode = "IN", optional = "false", description = "LEGDOC_* id or its slug, e.g. privacy"),
            @Attribute(name = "localeString", type = "String", mode = "IN", optional = "true", defaultValue = "en"),
            @Attribute(name = "title", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bodyText", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "changeNote", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "legalDocumentId", type = "String", mode = "OUT", optional = "false"),
            @Attribute(name = "versionNum", type = "Long", mode = "OUT", optional = "false")
        }
    )
    public interface PublishLegalDocument {}

    @Service(
        name = "clearComplianceCaches",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.ComplianceServiceImpl",
        invoke = "clearComplianceCaches",
        description = "Clears the third-party service registry cache, e.g. after a service or store setting changed.",
        auth = "true"
    )
    public interface ClearComplianceCaches {}

    @Service(
        name = "checkWithdrawalOrder",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.WithdrawalServiceImpl",
        invoke = "checkWithdrawalOrder",
        description = "Checks an order for an online withdrawal: order of the store, and e-mail (or logged-in customer) matches. matched=false otherwise.",
        auth = "false",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "matched", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "orderHeader", type = "org.ofbiz.entity.GenericValue", mode = "OUT", optional = "true"),
            @Attribute(name = "items", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CheckWithdrawalOrder {}

    @Service(
        name = "createWithdrawal",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.WithdrawalServiceImpl",
        invoke = "createWithdrawal",
        description = "Records an EU withdrawal (CRD Art. 11a): customer return with reason RTN_WITHDRAWAL and the confirmation e-mail with content and time of receipt.",
        auth = "false",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "customerName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "matched", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "returnId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "receivedDate", type = "Timestamp", mode = "OUT", optional = "true"),
            @Attribute(name = "withdrawnItems", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "pendingItems", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateWithdrawal {}

    @Service(
        name = "exportPartyPersonalData",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.PrivacyServiceImpl",
        invoke = "exportPartyPersonalData",
        description = "All personal data of a party as JSON (GDPR Art. 15/20, CCPA right to know). The party itself or COMPLIANCE_UPDATE.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "dataJson", type = "String", mode = "OUT", optional = "false")
        }
    )
    public interface ExportPartyPersonalData {}

    @Service(
        name = "anonymizePartyPersonalData",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.PrivacyServiceImpl",
        invoke = "anonymizePartyPersonalData",
        description = "Anonymizes a party; keeps order and invoice records and their contact data for the tax retention period.",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "retainedContactMechs", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface AnonymizePartyPersonalData {}

    @Service(
        name = "createPrivacyRequest",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.PrivacyServiceImpl",
        invoke = "createPrivacyRequest",
        description = "Creates a privacy request: RECEIVED for a logged-in customer, UNVERIFIED with an e-mail token for a guest.",
        auth = "false",
        attributes = {
            @Attribute(name = "requestTypeId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "jurisdiction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "privacyRequestId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "verifyToken", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreatePrivacyRequest {}

    @Service(
        name = "verifyPrivacyRequest",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.PrivacyServiceImpl",
        invoke = "verifyPrivacyRequest",
        description = "Confirms a guest privacy request by its e-mail token.",
        auth = "false",
        attributes = {
            @Attribute(name = "verifyToken", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "verified", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "privacyRequestId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface VerifyPrivacyRequest {}

    @Service(
        name = "purgeExpiredPersonalData",
        engine = "java",
        location = "com.ilscipio.scipio.compliance.service.PrivacyServiceImpl",
        invoke = "purgeExpiredPersonalData",
        description = "Daily retention job: old consent events, price snapshots and unverified privacy requests.",
        auth = "true",
        attributes = {
            @Attribute(name = "consentYears", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "snapshotDays", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "removedConsentEvents", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "removedPriceSnapshots", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "removedUnverifiedRequests", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface PurgeExpiredPersonalData {}
}
