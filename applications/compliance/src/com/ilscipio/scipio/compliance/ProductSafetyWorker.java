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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.party.contact.ContactHelper;
import org.ofbiz.party.party.PartyHelper;

/**
 * Product safety data for the product page (EU General Product Safety Regulation 2023/988, Art. 19): manufacturer,
 * EU responsible person, product identifiers, packaging, and for marketplaces the seller (DSA Art. 30-31).
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class ProductSafetyWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** GoodIdentification types shown as product identifiers, in this order. */
    private static final String[] ID_TYPES = {"EAN", "UPCA", "UPCE", "ISBN", "MANUFACTURER_ID_NO", "SKU"};

    private ProductSafetyWorker() {}

    /**
     * Keys: manufacturer, euResponsible, seller (maps: name, address, email, web, sellerType), identifiers (list of
     * type/value), packaging (list of material/weight/level), complete (all GPSR fields present). For a variant
     * without own data, the virtual parent's data counts.
     */
    public static Map<String, Object> getSafetyInfo(Delegator delegator, GenericValue product, String productStoreId) {
        Map<String, Object> out = new LinkedHashMap<>();
        if (product == null) {
            return out;
        }
        GenericValue parent = getVirtualParent(delegator, product);
        String manufacturerPartyId = firstNonEmpty(product.getString("manufacturerPartyId"), parent != null ? parent.getString("manufacturerPartyId") : null);
        out.put("manufacturer", partyInfo(delegator, manufacturerPartyId));
        String respPartyId = productRoleParty(delegator, product.getString("productId"), "EU_RESP_PERSON");
        if (respPartyId == null && parent != null) {
            respPartyId = productRoleParty(delegator, parent.getString("productId"), "EU_RESP_PERSON");
        }
        if (respPartyId == null) {
            GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
            respPartyId = profile != null ? profile.getString("euRespPartyId") : null;
        }
        out.put("euResponsible", partyInfo(delegator, respPartyId));
        String sellerPartyId = productRoleParty(delegator, product.getString("productId"), "MARKETPLACE_SELLER");
        if (sellerPartyId == null && parent != null) {
            sellerPartyId = productRoleParty(delegator, parent.getString("productId"), "MARKETPLACE_SELLER");
        }
        if (sellerPartyId != null) {
            Map<String, Object> seller = partyInfo(delegator, sellerPartyId);
            try {
                GenericValue ms = EntityQuery.use(delegator).from("MarketplaceSeller").where("productStoreId", productStoreId, "partyId", sellerPartyId).cache().queryOne();
                if (ms != null) {
                    seller.put("sellerType", ms.getString("sellerTypeId"));
                    seller.put("displayName", ms.getString("displayName"));
                    seller.put("tradeRegister", ms.getString("tradeRegister"));
                    seller.put("vatId", ms.getString("vatId"));
                    seller.put("verified", "MSS_VERIFIED".equals(ms.getString("statusId")));
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            out.put("seller", seller);
        }
        List<Map<String, String>> ids = new ArrayList<>();
        try {
            for (String type : ID_TYPES) {
                GenericValue gi = EntityQuery.use(delegator).from("GoodIdentification")
                        .where("productId", product.getString("productId"), "goodIdentificationTypeId", type).cache().queryFirst();
                if (gi == null && parent != null) {
                    gi = EntityQuery.use(delegator).from("GoodIdentification")
                            .where("productId", parent.getString("productId"), "goodIdentificationTypeId", type).cache().queryFirst();
                }
                if (gi != null && UtilValidate.isNotEmpty(gi.getString("idValue"))) {
                    Map<String, String> id = new LinkedHashMap<>();
                    id.put("type", type);
                    id.put("value", gi.getString("idValue"));
                    ids.add(id);
                }
            }
            List<Map<String, Object>> packaging = new ArrayList<>();
            for (GenericValue pc : EntityQuery.use(delegator).from("PackagingComponent").where("productId", product.getString("productId")).cache().queryList()) {
                Map<String, Object> m = new LinkedHashMap<>();
                GenericValue mat = EntityQuery.use(delegator).from("Enumeration").where("enumId", pc.getString("materialId")).cache().queryOne();
                m.put("material", mat != null ? mat.getString("description") : pc.getString("materialId"));
                m.put("weight", pc.getBigDecimal("weight"));
                GenericValue uom = pc.getString("weightUomId") != null ? EntityQuery.use(delegator).from("Uom").where("uomId", pc.getString("weightUomId")).cache().queryOne() : null;
                m.put("weightUomId", uom != null && uom.getString("abbreviation") != null ? uom.getString("abbreviation") : pc.getString("weightUomId"));
                m.put("levelId", pc.getString("levelId"));
                m.put("description", pc.getString("description"));
                packaging.add(m);
            }
            out.put("packaging", packaging);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        out.put("identifiers", ids);
        Map<?, ?> manu = (Map<?, ?>) out.get("manufacturer");
        out.put("complete", manu != null && manu.get("name") != null && (manu.get("address") != null) && (manu.get("email") != null || manu.get("web") != null) && !ids.isEmpty());
        return out;
    }

    private static GenericValue getVirtualParent(Delegator delegator, GenericValue product) {
        if (!"Y".equals(product.getString("isVariant"))) {
            return null;
        }
        try {
            GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc")
                    .where("productIdTo", product.getString("productId"), "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().cache().queryFirst();
            return assoc != null ? assoc.getRelatedOne("MainProduct", true) : null;
        } catch (GenericEntityException e) {
            return null;
        }
    }

    private static String productRoleParty(Delegator delegator, String productId, String roleTypeId) {
        try {
            GenericValue pr = EntityQuery.use(delegator).from("ProductRole").where("productId", productId, "roleTypeId", roleTypeId).filterByDate().cache().queryFirst();
            return pr != null ? pr.getString("partyId") : null;
        } catch (GenericEntityException e) {
            return null;
        }
    }

    /** name, address (one line), email, web of a party; empty map without partyId. */
    public static Map<String, Object> partyInfo(Delegator delegator, String partyId) {
        Map<String, Object> out = new LinkedHashMap<>();
        if (UtilValidate.isEmpty(partyId)) {
            return out;
        }
        try {
            GenericValue party = EntityQuery.use(delegator).from("Party").where("partyId", partyId).cache().queryOne();
            if (party == null) {
                return out;
            }
            out.put("partyId", partyId);
            out.put("name", PartyHelper.getPartyName(party, false));
            GenericValue address = firstContactMech(party, new String[] {"GENERAL_LOCATION", "PRIMARY_LOCATION", "BILLING_LOCATION"}, "POSTAL_ADDRESS");
            if (address != null) {
                GenericValue pa = address.getRelatedOne("PostalAddress", true);
                if (pa != null) {
                    String country = pa.getString("countryGeoId");
                    GenericValue geo = country != null ? EntityQuery.use(delegator).from("Geo").where("geoId", country).cache().queryOne() : null;
                    out.put("address", join(", ", pa.getString("address1"), pa.getString("address2"),
                            join(" ", pa.getString("postalCode"), pa.getString("city")), geo != null ? geo.getString("geoName") : country));
                }
            }
            GenericValue email = firstContactMech(party, new String[] {"PRIMARY_EMAIL", "OTHER_EMAIL"}, "EMAIL_ADDRESS");
            if (email != null) {
                out.put("email", email.getString("infoString"));
            }
            GenericValue web = firstContactMech(party, new String[] {"PRIMARY_WEB_URL"}, "WEB_ADDRESS");
            if (web != null) {
                out.put("web", web.getString("infoString"));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return out;
    }

    private static GenericValue firstContactMech(GenericValue party, String[] purposes, String type) {
        for (String purpose : purposes) {
            Collection<GenericValue> cms = ContactHelper.getContactMech(party, purpose, type, false);
            if (UtilValidate.isNotEmpty(cms)) {
                return cms.iterator().next();
            }
        }
        Collection<GenericValue> any = ContactHelper.getContactMechByType(party, type, false);
        return UtilValidate.isNotEmpty(any) ? any.iterator().next() : null;
    }

    private static String firstNonEmpty(String a, String b) {
        return UtilValidate.isNotEmpty(a) ? a : (UtilValidate.isNotEmpty(b) ? b : null);
    }

    private static String join(String sep, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            if (UtilValidate.isNotEmpty(p)) {
                if (sb.length() > 0) {
                    sb.append(sep);
                }
                sb.append(p);
            }
        }
        return sb.length() > 0 ? sb.toString() : null;
    }
}
