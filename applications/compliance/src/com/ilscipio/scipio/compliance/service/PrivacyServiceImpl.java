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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * Privacy rights: data export (GDPR Art. 15 and 20, CCPA right to know), anonymization (GDPR Art. 17, CCPA right to
 * delete) that keeps order and invoice records for the tax retention period, guest requests with e-mail
 * verification, and the retention purge job.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class PrivacyServiceImpl {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private PrivacyServiceImpl() {}

    /** The party itself, or a compliance administrator. */
    private static boolean mayAccess(Security security, GenericValue userLogin, String partyId) {
        return userLogin != null && (partyId.equals(userLogin.getString("partyId"))
                || security.hasPermission("COMPLIANCE_ADMIN", userLogin) || security.hasPermission("COMPLIANCE_UPDATE", userLogin));
    }

    public static Map<String, Object> exportPartyPersonalData(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String partyId = (String) context.get("partyId");
        if (!mayAccess(dctx.getSecurity(), (GenericValue) context.get("userLogin"), partyId)) {
            return ServiceUtil.returnError("No permission to export the data of this party.");
        }
        try {
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("exportedAt", UtilDateTime.nowTimestamp().toString());
            out.put("party", rows(delegator, "Party", "partyId", partyId));
            out.put("person", rows(delegator, "Person", "partyId", partyId));
            out.put("partyGroup", rows(delegator, "PartyGroup", "partyId", partyId));
            List<Map<String, Object>> logins = new ArrayList<>();
            for (GenericValue ul : EntityQuery.use(delegator).from("UserLogin").where("partyId", partyId).queryList()) {
                Map<String, Object> m = new LinkedHashMap<>(ul.getAllFields());
                m.remove("currentPassword");
                m.remove("passwordHint");
                logins.add(clean(m));
            }
            out.put("userLogins", logins);
            List<Map<String, Object>> contacts = new ArrayList<>();
            for (GenericValue pcm : EntityQuery.use(delegator).from("PartyContactMech").where("partyId", partyId).queryList()) {
                Map<String, Object> m = clean(new LinkedHashMap<>(pcm.getAllFields()));
                GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", pcm.getString("contactMechId")).queryOne();
                if (cm != null) {
                    m.put("contactMech", clean(new LinkedHashMap<>(cm.getAllFields())));
                    GenericValue pa = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", cm.getString("contactMechId")).queryOne();
                    if (pa != null) {
                        m.put("postalAddress", clean(new LinkedHashMap<>(pa.getAllFields())));
                    }
                    GenericValue tn = EntityQuery.use(delegator).from("TelecomNumber").where("contactMechId", cm.getString("contactMechId")).queryOne();
                    if (tn != null) {
                        m.put("telecomNumber", clean(new LinkedHashMap<>(tn.getAllFields())));
                    }
                }
                m.put("purposes", rows(delegator, "PartyContactMechPurpose", "partyId", partyId, "contactMechId", pcm.getString("contactMechId")));
                contacts.add(m);
            }
            out.put("contactMechs", contacts);
            out.put("partyAttributes", rows(delegator, "PartyAttribute", "partyId", partyId));
            List<Map<String, Object>> orders = new ArrayList<>();
            for (GenericValue role : EntityQuery.use(delegator).from("OrderRole").where("partyId", partyId, "roleTypeId", "PLACING_CUSTOMER").queryList()) {
                String orderId = role.getString("orderId");
                Map<String, Object> o = new LinkedHashMap<>();
                o.put("header", rows(delegator, "OrderHeader", "orderId", orderId));
                o.put("items", rows(delegator, "OrderItem", "orderId", orderId));
                o.put("contactMechs", rows(delegator, "OrderContactMech", "orderId", orderId));
                o.put("attributes", rows(delegator, "OrderAttribute", "orderId", orderId));
                orders.add(o);
            }
            out.put("orders", orders);
            out.put("returns", rows(delegator, "ReturnHeader", "fromPartyId", partyId));
            out.put("productReviews", rowsByUserLogins(delegator, "ProductReview", logins));
            out.put("shoppingLists", rows(delegator, "ShoppingList", "partyId", partyId));
            out.put("contactLists", rows(delegator, "ContactListParty", "partyId", partyId));
            out.put("consentEvents", rows(delegator, "ConsentEvent", "partyId", partyId));
            out.put("privacyRequests", rows(delegator, "PrivacyRequest", "partyId", partyId));
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("dataJson", JSON.from(out).toString());
            return result;
        } catch (Exception e) {
            Debug.logError(e, "Could not export the data of party " + partyId, module);
            return ServiceUtil.returnError("Could not export the data: " + e.getMessage());
        }
    }

    /**
     * Anonymizes a party. Kept (tax and commercial law): orders, invoices, returns and the contact data they use.
     * Removed or scrubbed: names, other contact data, login (disabled, password removed), attributes, shopping
     * lists, newsletter subscriptions; reviews become anonymous.
     */
    public static Map<String, Object> anonymizePartyPersonalData(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String partyId = (String) context.get("partyId");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!mayAccess(dctx.getSecurity(), userLogin, partyId)) {
            return ServiceUtil.returnError("No permission to delete the data of this party.");
        }
        Timestamp now = UtilDateTime.nowTimestamp();
        int kept = 0;
        try {
            GenericValue person = EntityQuery.use(delegator).from("Person").where("partyId", partyId).queryOne();
            if (person != null) {
                for (String f : new String[] {"salutation", "middleName", "personalTitle", "suffix", "nickname", "firstNameLocal", "middleNameLocal",
                        "lastNameLocal", "otherLocal", "birthDate", "deceasedDate", "height", "weight", "mothersMaidenName", "maritalStatus",
                        "socialSecurityNumber", "passportNumber", "passportExpireDate", "occupation", "comments", "gender"}) {
                    if (person.getModelEntity().isField(f)) {
                        person.set(f, null);
                    }
                }
                person.set("firstName", "Deleted");
                person.set("lastName", "Customer");
                person.store();
            }
            GenericValue group = EntityQuery.use(delegator).from("PartyGroup").where("partyId", partyId).queryOne();
            if (group != null) {
                group.set("groupName", "Deleted customer");
                group.store();
            }
            for (GenericValue pcm : EntityQuery.use(delegator).from("PartyContactMech").where("partyId", partyId).queryList()) {
                String cmId = pcm.getString("contactMechId");
                boolean usedByRecords = EntityQuery.use(delegator).from("OrderContactMech").where("contactMechId", cmId).queryCount() > 0
                        || EntityQuery.use(delegator).from("InvoiceContactMech").where("contactMechId", cmId).queryCount() > 0;
                if (pcm.get("thruDate") == null) {
                    pcm.set("thruDate", now);
                    pcm.store();
                }
                if (usedByRecords) {
                    kept++; // retained for the tax retention period, no longer active
                    continue;
                }
                GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", cmId).queryOne();
                if (cm != null && cm.get("infoString") != null) {
                    cm.set("infoString", "deleted-" + cmId);
                    cm.store();
                }
                GenericValue pa = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", cmId).queryOne();
                if (pa != null) {
                    for (String f : new String[] {"toName", "attnName", "address1", "address2", "houseNumber", "houseNumberExt", "directions", "postalCode", "postalCodeExt"}) {
                        if (pa.getModelEntity().isField(f)) {
                            pa.set(f, null);
                        }
                    }
                    pa.set("address1", "Deleted");
                    pa.set("city", pa.getString("city") != null ? "Deleted" : null);
                    pa.store();
                }
                GenericValue tn = EntityQuery.use(delegator).from("TelecomNumber").where("contactMechId", cmId).queryOne();
                if (tn != null) {
                    tn.set("contactNumber", "0");
                    tn.set("askForName", null);
                    tn.store();
                }
            }
            for (GenericValue ul : EntityQuery.use(delegator).from("UserLogin").where("partyId", partyId).queryList()) {
                ul.set("enabled", "N");
                ul.set("disabledDateTime", null); // permanently disabled
                ul.set("currentPassword", null);
                ul.set("passwordHint", null);
                ul.store();
                for (GenericValue r : EntityQuery.use(delegator).from("ProductReview").where("userLoginId", ul.getString("userLoginId")).queryList()) {
                    r.set("postedAnonymous", "Y");
                    r.store();
                }
            }
            delegator.removeByAnd("PartyAttribute", UtilMisc.toMap("partyId", partyId));
            for (GenericValue sl : EntityQuery.use(delegator).from("ShoppingList").where("partyId", partyId).queryList()) {
                delegator.removeByAnd("ShoppingListItem", UtilMisc.toMap("shoppingListId", sl.getString("shoppingListId")));
                sl.remove();
            }
            for (GenericValue clp : EntityQuery.use(delegator).from("ContactListParty").where("partyId", partyId).filterByDate().queryList()) {
                clp.set("thruDate", now);
                clp.store();
            }
            GenericValue party = EntityQuery.use(delegator).from("Party").where("partyId", partyId).queryOne();
            if (party != null && party.getModelEntity().isField("statusId")) {
                party.set("statusId", "PARTY_DISABLED");
                party.store();
            }
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("retainedContactMechs", kept);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not anonymize party " + partyId, module);
            return ServiceUtil.returnError("Could not delete the data: " + e.getMessage());
        }
    }

    /**
     * A privacy request. Logged-in customers: status RECEIVED. Guests: status UNVERIFIED with a token for the
     * e-mail link. The due date is 30 days (GDPR) or 45 days (US).
     */
    public static Map<String, Object> createPrivacyRequest(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String partyId = userLogin != null && !"anonymous".equals(userLogin.getString("userLoginId")) ? userLogin.getString("partyId") : null;
        String productStoreId = (String) context.get("productStoreId");
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        String jurisdiction = UtilValidate.isNotEmpty((String) context.get("jurisdiction")) ? (String) context.get("jurisdiction")
                : (profile == null || LegalDocumentWorker.hasJurisdiction(profile, "EU") ? "EU" : "US");
        Timestamp now = UtilDateTime.nowTimestamp();
        try {
            GenericValue pr = delegator.makeValue("PrivacyRequest");
            pr.set("privacyRequestId", delegator.getNextSeqId("PrivacyRequest"));
            pr.set("productStoreId", productStoreId);
            pr.set("partyId", partyId);
            pr.set("emailAddress", context.get("emailAddress"));
            pr.set("requestTypeId", context.get("requestTypeId"));
            pr.set("jurisdiction", jurisdiction);
            pr.set("receivedDate", now);
            pr.set("dueDate", UtilDateTime.adjustTimestamp(now, Calendar.DAY_OF_YEAR, "US".equals(jurisdiction) ? 45 : 30));
            pr.set("note", context.get("note"));
            if (partyId != null) {
                pr.set("statusId", "PRS_RECEIVED");
                pr.set("verifiedDate", now);
            } else {
                pr.set("statusId", "PRS_UNVERIFIED");
                pr.set("verifyToken", UUID.randomUUID().toString().replace("-", ""));
            }
            pr.create();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("privacyRequestId", pr.getString("privacyRequestId"));
            result.put("verifyToken", pr.getString("verifyToken"));
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> verifyPrivacyRequest(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String token = (String) context.get("verifyToken");
        try {
            GenericValue pr = UtilValidate.isNotEmpty(token) && token.length() >= 20
                    ? EntityQuery.use(delegator).from("PrivacyRequest").where("verifyToken", token, "statusId", "PRS_UNVERIFIED").queryFirst() : null;
            Map<String, Object> result = ServiceUtil.returnSuccess();
            if (pr == null) {
                result.put("verified", Boolean.FALSE);
                return result;
            }
            pr.set("statusId", "PRS_RECEIVED");
            pr.set("verifiedDate", UtilDateTime.nowTimestamp());
            pr.set("verifyToken", null);
            pr.store();
            result.put("verified", Boolean.TRUE);
            result.put("privacyRequestId", pr.getString("privacyRequestId"));
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /**
     * Daily retention job: removes consent events and price snapshots older than their periods and unverified
     * privacy requests older than 30 days. Periods (years/days) come from the service parameters.
     */
    public static Map<String, Object> purgeExpiredPersonalData(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Timestamp now = UtilDateTime.nowTimestamp();
        int consentYears = context.get("consentYears") != null ? ((Number) context.get("consentYears")).intValue() : 3;
        int snapshotDays = context.get("snapshotDays") != null ? ((Number) context.get("snapshotDays")).intValue() : 400;
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            int consents = delegator.removeByCondition("ConsentEvent", EntityCondition.makeCondition("eventDate", EntityOperator.LESS_THAN,
                    UtilDateTime.adjustTimestamp(now, Calendar.YEAR, -consentYears)));
            int snaps = com.ilscipio.scipio.compliance.PriceHistoryWorker.purgeOldSnapshots(delegator, snapshotDays);
            int requests = delegator.removeByCondition("PrivacyRequest", EntityCondition.makeCondition(UtilMisc.toList(
                    EntityCondition.makeCondition("statusId", "PRS_UNVERIFIED"),
                    EntityCondition.makeCondition("receivedDate", EntityOperator.LESS_THAN, UtilDateTime.adjustTimestamp(now, Calendar.DAY_OF_YEAR, -30)))));
            result.put("removedConsentEvents", consents);
            result.put("removedPriceSnapshots", snaps);
            result.put("removedUnverifiedRequests", requests);
            Debug.logInfo("Compliance retention purge: " + consents + " consent events, " + snaps + " price snapshots, "
                    + requests + " unverified requests removed", module);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }

    private static List<Map<String, Object>> rows(Delegator delegator, String entity, Object... fields) throws GenericEntityException {
        List<Map<String, Object>> out = new ArrayList<>();
        for (GenericValue v : EntityQuery.use(delegator).from(entity).where(fields).queryList()) {
            out.add(clean(new LinkedHashMap<>(v.getAllFields())));
        }
        return out;
    }

    private static List<Map<String, Object>> rowsByUserLogins(Delegator delegator, String entity, List<Map<String, Object>> logins) throws GenericEntityException {
        List<Map<String, Object>> out = new ArrayList<>();
        for (Map<String, Object> ul : logins) {
            out.addAll(rows(delegator, entity, "userLoginId", ul.get("userLoginId")));
        }
        return out;
    }

    /** Values as JSON-friendly strings; technical stamp fields removed. */
    private static Map<String, Object> clean(Map<String, Object> m) {
        Map<String, Object> out = new LinkedHashMap<>();
        for (Map.Entry<String, Object> e : m.entrySet()) {
            String k = e.getKey();
            if (k.endsWith("TxStamp") || k.equals("lastUpdatedStamp") || k.equals("createdStamp") || e.getValue() == null) {
                continue;
            }
            Object v = e.getValue();
            out.put(k, v instanceof String || v instanceof Number || v instanceof Boolean || v instanceof Map || v instanceof List ? v : v.toString());
        }
        return out;
    }
}
