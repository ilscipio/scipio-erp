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
package com.ilscipio.scipio.product.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/UpgradeServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    private static final String MODULE = UpgradeServices.class.getName();


    /**
     * Migrate Data From OldFacilityRole To FacilityParty
     */
    public static Map<String, Object> migrateFacilityRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue facilityParty = null;
        GenericValue partyRole = null;
        List<GenericValue> oldFacilityRoles = null;
        try {
            oldFacilityRoles = EntityQuery.use(delegator)
                    .from("OldFacilityRole")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldFacilityRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp fromDate = new Timestamp(System.currentTimeMillis());
        if (oldFacilityRoles != null) {
            for (GenericValue oldFacilityRole : oldFacilityRoles) {
                facilityParty = delegator.makeValue("FacilityParty");
                facilityParty.put("facilityId", oldFacilityRole.get("facilityId"));
                facilityParty.put("partyId", oldFacilityRole.get("partyId"));
                facilityParty.put("roleTypeId", oldFacilityRole.get("roleTypeId"));
                facilityParty.put("fromDate", fromDate);
                try {
                    partyRole = EntityQuery.use(delegator)
                            .from("PartyRole")
                            .where(UtilMisc.toMap("partyId", facilityParty.get("partyId"), "roleTypeId", facilityParty.get("roleTypeId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(partyRole)) {
                    partyRole = delegator.makeValue("PartyRole");
                    partyRole.put("partyId", facilityParty.get("partyId"));
                    partyRole.put("roleTypeId", facilityParty.get("roleTypeId"));
                    try {
                        delegator.create(partyRole);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
                try {
                    delegator.create(facilityParty);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Migrate Data From from Facility.oldSquareFootage to Facility.facilitySize
     */
    public static Map<String, Object> migrateFacilitySquareFootage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> facilities = null;
        try {
            facilities = EntityQuery.use(delegator)
                    .from("Facility")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (facilities != null) {
            for (GenericValue facility : facilities) {
                if (UtilValidate.isNotEmpty(facility.get("oldSquareFootage"))) {
                    facility.put("facilitySize", facility.get("oldSquareFootage"));
                    facility.put("facilitySizeUomId", "AREA_ft2");
                    try {
                        delegator.store(facility);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Migrate Data From OldProductKeyword To ProductKeyword
     */
    public static Map<String, Object> migrateProductKeyword(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productKeyword = null;
        GenericValue checkProductKeyword = null;
        List<GenericValue> oldProductKeywords = null;
        try {
            oldProductKeywords = EntityQuery.use(delegator)
                    .from("OldProductKeyword")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldProductKeyword: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (oldProductKeywords != null) {
            for (GenericValue oldProductKeyword : oldProductKeywords) {
                try {
                    checkProductKeyword = EntityQuery.use(delegator)
                            .from("ProductKeyword")
                            .where(UtilMisc.toMap("productId", oldProductKeyword.get("productId"), "keyword", oldProductKeyword.get("keyword"), "keywordTypeId", "KWT_KEYWORD"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductKeyword: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(checkProductKeyword)) {
                    productKeyword = delegator.makeValue("ProductKeyword");
                    productKeyword.put("productId", oldProductKeyword.get("productId"));
                    productKeyword.put("keyword", oldProductKeyword.get("keyword"));
                    productKeyword.put("keywordTypeId", "KWT_KEYWORD");
                    productKeyword.put("relevancyWeight", oldProductKeyword.get("relevancyWeight"));
                    try {
                        delegator.create(productKeyword);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }

}
