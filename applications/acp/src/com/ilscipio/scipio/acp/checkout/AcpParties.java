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
package com.ilscipio.scipio.acp.checkout;

import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;

/**
 * SCIPIO: 4.0.0: Addresses and the guest customer of an ACP checkout. The shipping address is a postal address without
 * a party while the session is open (shipping and tax need it); at completion it joins the new customer.
 */
public final class AcpParties {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private AcpParties() {}

    /** The first missing required field of an ACP address (line_one, city, country, postal_code), or null. */
    public static String missingAddressField(Map<String, Object> a) {
        for (String f : new String[] { "line_one", "city", "country", "postal_code" }) {
            if (UtilValidate.isEmpty(AcpJson.str(a, f))) return f;
        }
        return null;
    }

    /** The contact mech id of the shipping address; a changed address makes a new one. */
    static String ensureAddress(AcpContext ctx, GenericValue row, Map<String, Object> state, Map<String, Object> fd) throws AcpException {
        Map<String, Object> a = AcpJson.map(fd.get("address"));
        String hash = AcpJson.sha256Hex(AcpJson.write(a));
        if (hash.equals(state.get("address_hash")) && UtilValidate.isNotEmpty(row.getString("shipContactMechId"))) {
            return row.getString("shipContactMechId");
        }
        try {
            Map<String, Object> in = postalFields(ctx.getDelegator(), a, AcpJson.str(fd, "name"));
            in.put("userLogin", ctx.getSystemLogin());
            Map<String, Object> res = ctx.getDispatcher().runSync("createPostalAddress", in);
            if (ServiceUtil.isError(res)) {
                throw AcpException.invalid("invalid", ServiceUtil.getErrorMessage(res), "$.fulfillment_details.address");
            }
            state.put("address_hash", hash);
            return (String) res.get("contactMechId");
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "ACP: address not saved: " + e.getMessage(), module);
            throw AcpException.processing("The address could not be saved.");
        }
    }

    static Map<String, Object> postalFields(Delegator delegator, Map<String, Object> a, String fallbackName) throws GenericEntityException, AcpException {
        String country = AcpJson.str(a, "country").trim().toUpperCase(Locale.ROOT);
        GenericValue countryGeo = EntityQuery.use(delegator).from("Geo").where("geoTypeId", "COUNTRY", "geoCode", country).cache().queryFirst();
        if (countryGeo == null) {
            countryGeo = EntityQuery.use(delegator).from("Geo").where("geoTypeId", "COUNTRY", "geoId", country).cache().queryFirst();
        }
        if (countryGeo == null) {
            throw AcpException.invalid("invalid", "Unknown country " + country + ".", "$.fulfillment_details.address.country");
        }
        String stateGeoId = null;
        String st = AcpJson.str(a, "state");
        if (UtilValidate.isNotEmpty(st)) {
            st = st.trim();
            for (GenericValue r : EntityQuery.use(delegator).from("GeoAssocAndGeoTo")
                    .where("geoIdFrom", countryGeo.getString("geoId"), "geoAssocTypeId", "REGIONS").cache().queryList()) {
                if (st.equalsIgnoreCase(r.getString("geoCode")) || st.equalsIgnoreCase(r.getString("abbreviation"))
                        || st.equalsIgnoreCase(r.getString("geoName")) || st.equalsIgnoreCase(r.getString("geoId"))) {
                    stateGeoId = r.getString("geoId");
                    break;
                }
            }
        }
        String name = AcpJson.str(a, "name");
        Map<String, Object> in = UtilMisc.toMap("toName", UtilValidate.isNotEmpty(name) ? name : fallbackName,
                "address1", AcpJson.str(a, "line_one"), "city", AcpJson.str(a, "city"), "postalCode", AcpJson.str(a, "postal_code"),
                "countryGeoId", countryGeo.getString("geoId"));
        if (UtilValidate.isNotEmpty(AcpJson.str(a, "line_two"))) in.put("address2", AcpJson.str(a, "line_two"));
        if (stateGeoId != null) in.put("stateProvinceGeoId", stateGeoId);
        if (UtilValidate.isNotEmpty(AcpJson.str(a, "company"))) in.put("attnName", AcpJson.str(a, "company"));
        return in;
    }

    /** A new guest customer (person, CUSTOMER role, e-mail, phone). Agents never reach an existing account by e-mail. */
    static String createBuyer(AcpContext ctx, Map<String, Object> buyer, Map<String, Object> fd) throws GenericEntityException, GenericServiceException {
        GenericValue system = ctx.getSystemLogin();
        String first = AcpJson.str(buyer, "first_name");
        String last = AcpJson.str(buyer, "last_name");
        if (UtilValidate.isEmpty(first) && UtilValidate.isEmpty(last)) {
            String full = AcpJson.str(buyer, "full_name");
            if (UtilValidate.isEmpty(full) && fd != null) full = AcpJson.str(fd, "name");
            full = UtilValidate.isNotEmpty(full) ? full.trim() : "Agent buyer";
            int sp = full.lastIndexOf(' ');
            first = sp > 0 ? full.substring(0, sp) : full;
            last = sp > 0 ? full.substring(sp + 1) : "-";
        }
        Map<String, Object> res = ctx.getDispatcher().runSync("createPerson", UtilMisc.toMap("firstName", nz(first, "-"),
                "lastName", nz(last, "-"), "userLogin", system));
        check(res);
        String partyId = (String) res.get("partyId");
        check(ctx.getDispatcher().runSync("createPartyRole", UtilMisc.toMap("partyId", partyId, "roleTypeId", "CUSTOMER", "userLogin", system)));
        String email = AcpJson.str(buyer, "email");
        if (UtilValidate.isNotEmpty(email)) {
            check(ctx.getDispatcher().runSync("createPartyEmailAddress", UtilMisc.toMap("partyId", partyId, "emailAddress", email,
                    "contactMechPurposeTypeId", "PRIMARY_EMAIL", "userLogin", system)));
        }
        String phone = AcpJson.str(buyer, "phone_number");
        if (UtilValidate.isEmpty(phone) && fd != null) phone = AcpJson.str(fd, "phone_number");
        if (UtilValidate.isNotEmpty(phone)) {
            Map<String, Object> ph = ctx.getDispatcher().runSync("createPartyTelecomNumber", UtilMisc.toMap("partyId", partyId,
                    "contactNumber", phone.length() > 60 ? phone.substring(0, 60) : phone, "contactMechPurposeTypeId", "PHONE_MOBILE", "userLogin", system));
            if (ServiceUtil.isError(ph)) {
                Debug.logWarning("ACP: buyer phone not saved: " + ServiceUtil.getErrorMessage(ph), module);
            }
        }
        return partyId;
    }

    static void linkAddress(AcpContext ctx, String partyId, String contactMechId, String purpose) throws GenericEntityException, GenericServiceException {
        check(ctx.getDispatcher().runSync("createPartyContactMech", UtilMisc.toMap("partyId", partyId, "contactMechId", contactMechId,
                "contactMechTypeId", "POSTAL_ADDRESS", "contactMechPurposeTypeId", purpose, "userLogin", ctx.getSystemLogin())));
    }

    static void addPostalAddress(AcpContext ctx, String partyId, Map<String, Object> a, String purpose) {
        try {
            if (missingAddressField(a) != null) return;
            Map<String, Object> in = postalFields(ctx.getDelegator(), a, null);
            in.put("partyId", partyId);
            in.put("contactMechPurposeTypeId", purpose);
            in.put("userLogin", ctx.getSystemLogin());
            Map<String, Object> res = ctx.getDispatcher().runSync("createPartyPostalAddress", in);
            if (ServiceUtil.isError(res)) {
                Debug.logWarning("ACP: billing address not saved: " + ServiceUtil.getErrorMessage(res), module);
            }
        } catch (GenericEntityException | GenericServiceException | AcpException e) {
            Debug.logWarning("ACP: billing address not saved: " + e.getMessage(), module);
        }
    }

    private static void check(Map<String, Object> res) throws GenericServiceException {
        if (ServiceUtil.isError(res)) throw new GenericServiceException(ServiceUtil.getErrorMessage(res));
    }

    private static String nz(String s, String def) {
        return UtilValidate.isEmpty(s) ? def : s;
    }
}
