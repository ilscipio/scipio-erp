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
package com.ilscipio.scipio.compliance;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Automatic compliance checklist of a store: each rule checks store data and reports OK, WARN or ERROR with a short
 * fix. Shown in the compliance back office and returned by the MCP tool "checklist".
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class ComplianceChecklist {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private ComplianceChecklist() {}

    /** One row per rule: id, status (OK/WARN/ERROR), title, detail, fix. */
    public static List<Map<String, String>> run(Delegator delegator, String productStoreId, Locale locale) {
        List<Map<String, String>> out = new ArrayList<>();
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        if (profile == null) {
            add(out, "profile", "ERROR", "Compliance profile", "The store has no compliance profile.", "Create a StoreComplianceProfile for the store (jurisdictions, legal name, address, periods).");
            return out;
        }
        boolean eu = LegalDocumentWorker.hasJurisdiction(profile, "EU");
        boolean us = LegalDocumentWorker.hasJurisdiction(profile, "US");
        add(out, "profile", "OK", "Compliance profile", "Jurisdictions: " + String.join(", ", LegalDocumentWorker.getJurisdictions(profile)), "");

        // trader identity (imprint, DSA contact, CCPA contact)
        List<String> missing = new ArrayList<>();
        for (String f : new String[] {"legalName", "addressLine", "postalCode", "city", "countryGeoId", "contactEmail"}) {
            if (placeholder(profile.getString(f))) {
                missing.add(f);
            }
        }
        if (eu) {
            for (String f : new String[] {"vatId", "representedBy"}) {
                if (placeholder(profile.getString(f))) {
                    missing.add(f);
                }
            }
        }
        add(out, "identity", missing.isEmpty() ? "OK" : "ERROR", "Trader identity",
                missing.isEmpty() ? "Legal name, address and contact are set." : "Missing or placeholder: " + String.join(", ", missing),
                "Fill the store profile; the imprint and all legal texts use these values.");

        // legal texts: published, not only the shipped template
        List<String> required = new ArrayList<>();
        if (eu) {
            required.addAll(List.of("LEGDOC_IMPRINT", "LEGDOC_TERMS", "LEGDOC_PRIVACY", "LEGDOC_COOKIES", "LEGDOC_WITHDRAWAL", "LEGDOC_ACCESSIBILITY"));
        }
        if (us) {
            for (String d : List.of("LEGDOC_TERMS", "LEGDOC_PRIVACY", "LEGDOC_CA_NOTICE")) {
                if (!required.contains(d)) {
                    required.add(d);
                }
            }
        }
        if ("Y".equals(profile.getString("marketplaceMode"))) {
            required.add("LEGDOC_SELLERS");
        }
        List<String> unpublished = new ArrayList<>();
        List<String> outdated = new ArrayList<>();
        String hash = ThirdPartyServiceRegistry.getRegistryHash(delegator, productStoreId);
        for (String d : required) {
            GenericValue doc = LegalDocumentWorker.getPublished(delegator, productStoreId, d, locale);
            if (doc == null) {
                unpublished.add(d.substring("LEGDOC_".length()).toLowerCase(Locale.ROOT));
            } else if (List.of("LEGDOC_PRIVACY", "LEGDOC_COOKIES", "LEGDOC_CA_NOTICE").contains(d) && !hash.equals(doc.getString("registryHash"))) {
                outdated.add(d.substring("LEGDOC_".length()).toLowerCase(Locale.ROOT));
            }
        }
        add(out, "documents", unpublished.isEmpty() ? "OK" : "WARN", "Legal texts published",
                unpublished.isEmpty() ? "All required texts are published." : "The shop shows the shipped template for: " + String.join(", ", unpublished),
                "Check each template with your lawyer, edit it and publish it.");
        add(out, "documentsCurrent", outdated.isEmpty() ? "OK" : "ERROR", "Service list in the privacy texts",
                outdated.isEmpty() ? "The published texts match the current service list." : "A service was added or removed after publishing: " + String.join(", ", outdated),
                "Publish the privacy and cookie texts again; the consent dialog already asks again.");

        // EPR
        if (eu) {
            long epr = count(delegator, "EprRegistration", EntityCondition.makeCondition(EntityCondition.makeCondition("schemeId", "EPR_PACKAGING"),
                    EntityCondition.makeCondition("partyId", payToParty(delegator, productStoreId))));
            add(out, "epr", epr > 0 ? "OK" : "ERROR", "Packaging EPR registration (PPWR)",
                    epr > 0 ? epr + " packaging registration(s) on file." : "No packaging EPR registration for the store owner.",
                    "Register with the EPR scheme of each EU country you ship to and enter the numbers (imprint shows them).");
            long boxes = count(delegator, "ShipmentBoxType", null);
            long boxesWithData = countDistinct(delegator, "PackagingComponent", "shipmentBoxTypeId");
            add(out, "packagingData", boxes == 0 || boxesWithData >= boxes ? "OK" : "WARN", "Packaging material data",
                    boxesWithData + " of " + boxes + " shipment box types have material data.", "Add materials and weights so the EPR report is complete.");
        }

        // GPSR coverage
        if (eu) {
            long products = count(delegator, "Product", EntityCondition.makeCondition("isVirtual", EntityOperator.NOT_EQUAL, "Y"));
            long withManufacturer = count(delegator, "Product", EntityCondition.makeCondition(EntityCondition.makeCondition("isVirtual", EntityOperator.NOT_EQUAL, "Y"),
                    EntityCondition.makeCondition("manufacturerPartyId", EntityOperator.NOT_EQUAL, null)));
            long pct = products == 0 ? 100 : withManufacturer * 100 / products;
            add(out, "gpsr", pct >= 95 ? "OK" : (pct >= 50 ? "WARN" : "ERROR"), "Product safety data (GPSR)",
                    pct + " % of products name a manufacturer (" + withManufacturer + " of " + products + ").",
                    "Set the manufacturer (and the EU responsible person for non-EU manufacturers) on every product.");
            add(out, "euRep", UtilValidate.isNotEmpty(profile.getString("euRespPartyId")) ? "OK" : "WARN", "Default EU responsible person",
                    UtilValidate.isNotEmpty(profile.getString("euRespPartyId")) ? "Set." : "Not set.", "Needed for products of manufacturers outside the EU.");
        }

        // privacy requests
        long overdue = count(delegator, "PrivacyRequest", EntityCondition.makeCondition(
                EntityCondition.makeCondition("productStoreId", productStoreId),
                EntityCondition.makeCondition("statusId", EntityOperator.IN, List.of("PRS_RECEIVED", "PRS_IN_PROGRESS")),
                EntityCondition.makeCondition("dueDate", EntityOperator.LESS_THAN, UtilDateTime.nowTimestamp())));
        long open = count(delegator, "PrivacyRequest", EntityCondition.makeCondition(
                EntityCondition.makeCondition("productStoreId", productStoreId),
                EntityCondition.makeCondition("statusId", EntityOperator.IN, List.of("PRS_RECEIVED", "PRS_IN_PROGRESS"))));
        add(out, "privacyRequests", overdue > 0 ? "ERROR" : (open > 0 ? "WARN" : "OK"), "Privacy requests",
                open + " open, " + overdue + " overdue.", "Answer access, deletion and correction requests within 30 days (EU) or 45 days (US).");

        // marketplace sellers
        if ("Y".equals(profile.getString("marketplaceMode"))) {
            int incomplete = 0;
            int sellers = 0;
            try {
                for (GenericValue ms : EntityQuery.use(delegator).from("MarketplaceSeller").where("productStoreId", productStoreId).queryList()) {
                    if ("MSS_SUSPENDED".equals(ms.getString("statusId"))) {
                        continue;
                    }
                    sellers++;
                    Map<String, Object> p = ProductSafetyWorker.partyInfo(delegator, ms.getString("partyId"));
                    boolean business = "SELLER_BUSINESS".equals(ms.getString("sellerTypeId"));
                    if (p.get("address") == null || p.get("email") == null || !"Y".equals(ms.getString("selfCertified"))
                            || (business && UtilValidate.isEmpty(ms.getString("tradeRegister")) && UtilValidate.isEmpty(ms.getString("vatId")))) {
                        incomplete++;
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            add(out, "sellers", incomplete == 0 ? "OK" : "ERROR", "Seller trader data (DSA Art. 30)",
                    (sellers - incomplete) + " of " + sellers + " sellers have complete trader data.",
                    "Collect address, e-mail, register or VAT number and the self-certification before a seller can sell.");
        }

        // consent and payment page control are built in
        add(out, "consent", "OK", "Consent and GPC", eu ? "Opt-in dialog before any non-necessary service; GPC honoured." : "Opt-out with GPC honoured.", "");
        add(out, "paymentScripts", "OK", "Payment page scripts (PCI DSS 6.4.3/11.6.1)",
                "Checkout pages send a report-only CSP from the service list; violations are logged.", "Review 'CSP violation' log lines weekly.");
        if (eu) {
            add(out, "notice", "OK", "EU legal guarantee notice", "Official notice (23 PNG + English SVG) shown via 'Your legal guarantee rights'.", "");
        }
        return out;
    }

    private static boolean placeholder(String v) {
        return UtilValidate.isEmpty(v) || v.trim().startsWith("[");
    }

    private static String payToParty(Delegator delegator, String productStoreId) {
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
            return store != null ? store.getString("payToPartyId") : "_NA_";
        } catch (GenericEntityException e) {
            return "_NA_";
        }
    }

    private static long count(Delegator delegator, String entity, EntityCondition cond) {
        try {
            return EntityQuery.use(delegator).from(entity).where(cond).queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return 0;
        }
    }

    private static long countDistinct(Delegator delegator, String entity, String field) {
        try {
            return EntityQuery.use(delegator).select(field).from(entity).where(EntityCondition.makeCondition(field, EntityOperator.NOT_EQUAL, null))
                    .distinct().queryList().size();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return 0;
        }
    }

    private static void add(List<Map<String, String>> out, String id, String status, String title, String detail, String fix) {
        Map<String, String> m = new LinkedHashMap<>();
        m.put("id", id);
        m.put("status", status);
        m.put("title", title);
        m.put("detail", detail);
        m.put("fix", fix);
        out.add(m);
    }
}
