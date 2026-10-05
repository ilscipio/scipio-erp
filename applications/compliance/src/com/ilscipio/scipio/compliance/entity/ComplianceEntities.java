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
package com.ilscipio.scipio.compliance.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Entity definitions for the compliance component: store legal profile, versioned legal documents,
 * third-party services, consent log, privacy requests, EPR registrations, packaging data,
 * price snapshots (EU 30-day rule) and marketplace sellers.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComplianceEntities {

    @Entity(
        name = "StoreComplianceProfile",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Store Compliance Profile",
        description = "Legal settings of one product store: jurisdictions, trader identity, periods and feature flags.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "jurisdictions", type = "description", description = "Comma list, e.g. EU,US,US-CA"),
            @Field(name = "consentMode", type = "id", description = "EU_OPT_IN, US_OPT_OUT or AUTO"),
            @Field(name = "legalName", type = "name"),
            @Field(name = "addressLine", type = "description"),
            @Field(name = "postalCode", type = "short-varchar"),
            @Field(name = "city", type = "name"),
            @Field(name = "countryGeoId", type = "id"),
            @Field(name = "contactEmail", type = "email"),
            @Field(name = "contactPhone", type = "id-long"),
            @Field(name = "registerCourt", type = "name"),
            @Field(name = "registerNumber", type = "id-long"),
            @Field(name = "vatId", type = "id-long"),
            @Field(name = "representedBy", type = "description"),
            @Field(name = "dpoContact", type = "description"),
            @Field(name = "supervisoryAuthority", type = "description"),
            @Field(name = "euRespPartyId", type = "id", description = "Default EU responsible person (GPSR)"),
            @Field(name = "withdrawalDays", type = "numeric"),
            @Field(name = "returnDays", type = "numeric"),
            @Field(name = "legalGuaranteeYears", type = "numeric"),
            @Field(name = "retentionYears", type = "numeric", description = "Tax retention period for orders and invoices"),
            @Field(name = "euOrderButton", type = "indicator", description = "Y: checkout button says 'Order with obligation to pay'"),
            @Field(name = "sellsSubscriptions", type = "indicator"),
            @Field(name = "usesSensitivePi", type = "indicator"),
            @Field(name = "aiChat", type = "indicator"),
            @Field(name = "marketplaceMode", type = "indicator"),
            @Field(name = "consentVersion", type = "numeric", description = "Increases when the service list changes; the banner asks again")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")})
        }
    )
    public interface StoreComplianceProfileEntity {}

    @Entity(
        name = "LegalDocument",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Legal Document",
        description = "One version of one legal text of a store (imprint, privacy policy, terms ...). Body is sanitized HTML with {{token}} placeholders.",
        fields = {
            @Field(name = "legalDocumentId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "docTypeId", type = "id-ne"),
            @Field(name = "localeString", type = "short-varchar"),
            @Field(name = "versionNum", type = "numeric"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "title", type = "name"),
            @Field(name = "bodyText", type = "very-long"),
            @Field(name = "fromTemplate", type = "indicator"),
            @Field(name = "registryHash", type = "short-varchar", description = "Hash of the third-party service list at publish time"),
            @Field(name = "changeNote", type = "description"),
            @Field(name = "publishedDate", type = "date-time"),
            @Field(name = "publishedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "legalDocumentId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")}),
            @Relation(type = RelationType.ONE_NOFK, title = "DocType", relEntityName = "Enumeration", keyMaps = {@KeyMap(fieldName = "docTypeId", relFieldName = "enumId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "StatusItem", keyMaps = {@KeyMap(fieldName = "statusId")})
        },
        indexes = {
            @Index(name = "LEGDOC_STORE_TYPE", fields = {@IndexField(name = "productStoreId"), @IndexField(name = "docTypeId"), @IndexField(name = "localeString")})
        }
    )
    public interface LegalDocumentEntity {}

    @Entity(
        name = "ThirdPartyService",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Third Party Service",
        description = "A service that receives shopper data, added or overridden by the merchant for one store. Auto-detected services come from config/known-services.xml.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "serviceId", type = "id-ne"),
            @Field(name = "serviceName", type = "name"),
            @Field(name = "providerName", type = "name"),
            @Field(name = "purpose", type = "description"),
            @Field(name = "categoryId", type = "id", description = "NECESSARY, PREFERENCES, STATISTICS or MARKETING"),
            @Field(name = "cookies", type = "description"),
            @Field(name = "dataCategories", type = "description"),
            @Field(name = "countries", type = "description"),
            @Field(name = "legalBasis", type = "short-varchar"),
            @Field(name = "privacyUrl", type = "url"),
            @Field(name = "scriptDomains", type = "description", description = "Domains for the checkout CSP, comma list"),
            @Field(name = "enabled", type = "indicator", description = "N hides an auto-detected service")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "serviceId")
        }
    )
    public interface ThirdPartyServiceEntity {}

    @Entity(
        name = "ConsentEvent",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Consent Event",
        description = "Proof of one consent choice (GDPR Art. 7(1), CCPA opt-out, terms acceptance).",
        fields = {
            @Field(name = "consentEventId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "webSiteId", type = "id"),
            @Field(name = "visitorId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "consentTypeId", type = "id-ne"),
            @Field(name = "granted", type = "indicator"),
            @Field(name = "sourceId", type = "id"),
            @Field(name = "legalDocumentId", type = "id"),
            @Field(name = "documentVersion", type = "numeric"),
            @Field(name = "consentVersion", type = "numeric"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "eventDate", type = "date-time"),
            @Field(name = "ipHash", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "consentEventId")
        },
        indexes = {
            @Index(name = "CONSEV_PARTY", fields = {@IndexField(name = "partyId")}),
            @Index(name = "CONSEV_VISITOR", fields = {@IndexField(name = "visitorId")})
        }
    )
    public interface ConsentEventEntity {}

    @Entity(
        name = "PrivacyRequest",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Privacy Request",
        description = "A data subject or consumer request: access, delete, correct, opt-out, limit sensitive data.",
        fields = {
            @Field(name = "privacyRequestId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "emailAddress", type = "email"),
            @Field(name = "requestTypeId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "jurisdiction", type = "short-varchar"),
            @Field(name = "receivedDate", type = "date-time"),
            @Field(name = "dueDate", type = "date-time"),
            @Field(name = "verifiedDate", type = "date-time"),
            @Field(name = "completedDate", type = "date-time"),
            @Field(name = "verifyToken", type = "short-varchar"),
            @Field(name = "resultLocation", type = "long-varchar"),
            @Field(name = "note", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "privacyRequestId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "StatusItem", keyMaps = {@KeyMap(fieldName = "statusId")})
        },
        indexes = {
            @Index(name = "PRVREQ_PARTY", fields = {@IndexField(name = "partyId")}),
            @Index(name = "PRVREQ_TOKEN", fields = {@IndexField(name = "verifyToken")})
        }
    )
    public interface PrivacyRequestEntity {}

    @Entity(
        name = "EprRegistration",
        packageName = "com.ilscipio.scipio.compliance",
        title = "EPR Registration",
        description = "Extended producer responsibility registration of a party in one country (packaging, WEEE, batteries, textiles).",
        fields = {
            @Field(name = "eprRegistrationId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "countryGeoId", type = "id-ne"),
            @Field(name = "schemeId", type = "id-ne"),
            @Field(name = "registrationNumber", type = "id-long"),
            @Field(name = "authRepPartyId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "eprRegistrationId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "Party", keyMaps = {@KeyMap(fieldName = "partyId")}),
            @Relation(type = RelationType.ONE_NOFK, title = "Country", relEntityName = "Geo", keyMaps = {@KeyMap(fieldName = "countryGeoId", relFieldName = "geoId")})
        },
        indexes = {
            @Index(name = "EPRREG_PARTY", fields = {@IndexField(name = "partyId")})
        }
    )
    public interface EprRegistrationEntity {}

    @Entity(
        name = "PackagingComponent",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Packaging Component",
        description = "One packaging material of a shipment box type or a product, for EPR reports (PPWR).",
        fields = {
            @Field(name = "packagingComponentId", type = "id-ne"),
            @Field(name = "shipmentBoxTypeId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "levelId", type = "id", description = "PRIMARY, SECONDARY, TRANSPORT or ECOMMERCE"),
            @Field(name = "materialId", type = "id-ne"),
            @Field(name = "weight", type = "fixed-point"),
            @Field(name = "weightUomId", type = "id"),
            @Field(name = "recycledPct", type = "fixed-point"),
            @Field(name = "reusable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "packagingComponentId")
        },
        indexes = {
            @Index(name = "PKGCMP_BOX", fields = {@IndexField(name = "shipmentBoxTypeId")}),
            @Index(name = "PKGCMP_PROD", fields = {@IndexField(name = "productId")})
        }
    )
    public interface PackagingComponentEntity {}

    @Entity(
        name = "ProductPriceSnapshot",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Product Price Snapshot",
        description = "The effective price of a product in a store at a point in time; source of the EU 'lowest price in the last 30 days'.",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "snapshotDate", type = "date-time"),
            @Field(name = "price", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "currencyUomId"),
            @PrimaryKey(field = "snapshotDate")
        }
    )
    public interface ProductPriceSnapshotEntity {}

    @Entity(
        name = "MarketplaceSeller",
        packageName = "com.ilscipio.scipio.compliance",
        title = "Marketplace Seller",
        description = "A third-party seller in a marketplace store, with the trader data that EU DSA Art. 30 and the US INFORM Act require.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "sellerTypeId", type = "id", description = "BUSINESS or PRIVATE (CRD Art. 6a)"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "displayName", type = "name"),
            @Field(name = "shortBio", type = "description"),
            @Field(name = "category", type = "name"),
            @Field(name = "logoImageUrl", type = "url"),
            @Field(name = "tradeRegister", type = "id-long"),
            @Field(name = "vatId", type = "id-long"),
            @Field(name = "joinedDate", type = "date-time"),
            @Field(name = "verifiedDate", type = "date-time"),
            @Field(name = "verifiedByUserLogin", type = "id-vlong"),
            @Field(name = "selfCertified", type = "indicator", description = "Seller confirmed it offers only compliant products (DSA Art. 30(1)(e))"),
            @Field(name = "selfCertifiedDate", type = "date-time"),
            @Field(name = "highVolume", type = "indicator", description = "US INFORM Act high-volume third-party seller")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "Party", keyMaps = {@KeyMap(fieldName = "partyId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "PartyGroup", keyMaps = {@KeyMap(fieldName = "partyId")}),
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")})
        },
        indexes = {
            @Index(name = "MKTSEL_JOINED", fields = {@IndexField(name = "joinedDate")})
        }
    )
    public interface MarketplaceSellerEntity {}

}
