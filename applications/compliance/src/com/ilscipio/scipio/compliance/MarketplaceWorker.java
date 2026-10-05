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
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Marketplace sellers of a store for the storefront: newest verified sellers, one seller with its trader data
 * (EU DSA Art. 30-31, CRD Art. 6a, US INFORM Act) and its products.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class MarketplaceWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private MarketplaceWorker() {}

    public static boolean isMarketplace(Delegator delegator, String productStoreId) {
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        return profile != null && "Y".equals(profile.getString("marketplaceMode"));
    }

    /** Newest verified sellers first. Each map: partyId, displayName, category, joinedDate, sellerType, letter, productIds. */
    public static List<Map<String, Object>> getNewSellers(Delegator delegator, String productStoreId, int limit) {
        List<Map<String, Object>> out = new ArrayList<>();
        try {
            for (GenericValue ms : EntityQuery.use(delegator).from("MarketplaceSeller")
                    .where("productStoreId", productStoreId, "statusId", "MSS_VERIFIED").orderBy("-joinedDate").maxRows(limit).cache().queryList()) {
                out.add(toMap(delegator, ms, 3));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return out;
    }

    /** One seller of the store with trader data and up to 24 product ids; null if unknown or suspended. */
    public static Map<String, Object> getSeller(Delegator delegator, String productStoreId, String partyId) {
        if (UtilValidate.isEmpty(partyId)) {
            return null;
        }
        try {
            GenericValue ms = EntityQuery.use(delegator).from("MarketplaceSeller").where("productStoreId", productStoreId, "partyId", partyId).cache().queryOne();
            if (ms == null || "MSS_SUSPENDED".equals(ms.getString("statusId"))) {
                return null;
            }
            Map<String, Object> m = toMap(delegator, ms, 24);
            m.put("trader", ProductSafetyWorker.partyInfo(delegator, partyId));
            m.put("tradeRegister", ms.getString("tradeRegister"));
            m.put("vatId", ms.getString("vatId"));
            m.put("selfCertified", "Y".equals(ms.getString("selfCertified")));
            m.put("verified", "MSS_VERIFIED".equals(ms.getString("statusId")));
            m.put("shortBio", ms.getString("shortBio"));
            return m;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    private static Map<String, Object> toMap(Delegator delegator, GenericValue ms, int maxProducts) throws GenericEntityException {
        Map<String, Object> m = new LinkedHashMap<>();
        String name = UtilValidate.isNotEmpty(ms.getString("displayName")) ? ms.getString("displayName") : ms.getString("partyId");
        m.put("partyId", ms.getString("partyId"));
        m.put("displayName", name);
        m.put("letter", name.substring(0, 1).toUpperCase());
        m.put("category", ms.getString("category"));
        m.put("joinedDate", ms.getTimestamp("joinedDate"));
        m.put("sellerType", ms.getString("sellerTypeId"));
        m.put("logoImageUrl", ms.getString("logoImageUrl"));
        List<String> productIds = new ArrayList<>();
        for (GenericValue pr : EntityQuery.use(delegator).from("ProductRole").where("partyId", ms.getString("partyId"), "roleTypeId", "MARKETPLACE_SELLER")
                .filterByDate().maxRows(maxProducts).cache().queryList()) {
            productIds.add(pr.getString("productId"));
        }
        m.put("productIds", productIds);
        return m;
    }
}
