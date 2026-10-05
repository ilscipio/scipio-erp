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
package com.ilscipio.scipio.shop.misc;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;


public class ThirdPartyEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public final static String DISTRIBUTOR_ID = "_DISTRIBUTOR_ID_";
    public final static String AFFILIATE_ID = "_AFFILIATE_ID_";

    /** Save the association id(s) specified in the request object into the session.
     *@param request The HTTPRequest object for the current request
     *@param response The HTTPResponse object for the current request
     *@return String specifying the exit status of this event
     */
    public static String setAssociationId(HttpServletRequest request, HttpServletResponse response) {
        Map<String, Object> requestParams = UtilHttp.getParameterMap(request);

        // check distributor
        String distriParam[] = { "distributor_id", "distributorid", "distributor" };
        String distributorId = null;

        for (int i = 0; i < distriParam.length; i++) {
            String param = distriParam[i];

            if (requestParams.containsKey(param)) {
                distributorId = (String) requestParams.get(param);
                break;
            } else if (requestParams.containsKey(param.toUpperCase())) {
                distributorId = (String) requestParams.get(param.toUpperCase());
                break;
            }
        }

        // check affiliate
        String affiliParam[] = { "affiliate_id", "affiliateid", "affiliate", "affil" };
        String affiliateId = null;

        for (int i = 0; i < affiliParam.length; i++) {
            String param = affiliParam[i];

            if (requestParams.containsKey(param)) {
                affiliateId = (String) requestParams.get(param);
                break;
            } else if (requestParams.containsKey(param.toUpperCase())) {
                affiliateId = (String) requestParams.get(param.toUpperCase());
                break;
            }
        }

        if (UtilValidate.isNotEmpty(distributorId)) {
            request.getSession().setAttribute(DISTRIBUTOR_ID, distributorId);
            updateAssociatedDistributor(request, response);
        }
        if (UtilValidate.isNotEmpty(affiliateId)) {
            request.getSession().setAttribute(AFFILIATE_ID, affiliateId);
            updateAssociatedAffiliate(request, response);
        }

        return "success";
    }

    /** Update the distributor association for the logged in user, if possible.
     *@param request The HTTPRequest object for the current request
     *@param response The HTTPResponse object for the current request
     *@return String specifying the exit status of this event
     */
    public static String updateAssociatedDistributor(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        GenericValue party = null;

        // SCIPIO: now points to shop
        java.net.URL shopPropertiesUrl = null;

        try {
            // SCIPIO: now points to shop
            // SCIPIO: get context using servlet API 3.0
            shopPropertiesUrl = request.getServletContext().getResource("/WEB-INF/shop.properties");
        } catch (java.net.MalformedURLException e) {
            Debug.logWarning(e, module);
        }

        String store = UtilProperties.getPropertyValue(shopPropertiesUrl, "distributor.store.customer");

        if (store == null || store.toUpperCase().startsWith("N")) {
            return "success";
        }

        String storeOnClick = UtilProperties.getPropertyValue(shopPropertiesUrl, "distributor.store.onclick");

        if (storeOnClick == null || storeOnClick.toUpperCase().startsWith("N")) {
            return "success";
        }

        try {
            party = userLogin == null ? null : userLogin.getRelatedOne("Party", false);
        } catch (GenericEntityException gee) {
            Debug.logWarning(gee, module);
        }

        if (party != null) {
            // if a distributorId is already associated, it will be used instead
            String currentDistributorId = getId(party, "DISTRIBUTOR");

            if (UtilValidate.isEmpty(currentDistributorId)) {
                String distributorId = (String) request.getSession().getAttribute(DISTRIBUTOR_ID);

                if (UtilValidate.isNotEmpty(distributorId)) {
                    List<GenericValue> toBeStored = new LinkedList<GenericValue>();

                    // create distributor Party ?? why?
                    // create distributor PartyRole ?? why?
                    // create PartyRelationship
                    GenericValue partyRelationship = delegator.makeValue("PartyRelationship", UtilMisc.toMap("partyIdFrom", party.getString("partyId"), "partyIdTo", distributorId, "roleTypeIdFrom", "CUSTOMER", "roleTypeIdTo", "DISTRIBUTOR"));

                    partyRelationship.set("fromDate", UtilDateTime.nowTimestamp());
                    partyRelationship.set("partyRelationshipTypeId", "DISTRIBUTION_CHANNEL");
                    toBeStored.add(partyRelationship);

                    toBeStored.add(delegator.makeValue("Party", UtilMisc.toMap("partyId", distributorId)));
                    toBeStored.add(delegator.makeValue("PartyRole", UtilMisc.toMap("partyId", distributorId, "roleTypeId", "DISTRIBUTOR")));
                    try {
                        delegator.storeAll(toBeStored);
                        if (Debug.infoOn()) Debug.logInfo("Distributor for user " + party.getString("partyId") + " set to " + distributorId, module);
                    } catch (GenericEntityException gee) {
                        Debug.logWarning(gee, module);
                    }
                } else {
                    // no distributorId is available
                    Debug.logInfo("No distributor in session or already associated with user " + userLogin.getString("partyId"), module);
                    return "success";
                }
            } else {
                request.getSession().setAttribute(DISTRIBUTOR_ID, currentDistributorId);
            }

            return "success";
        } else {
            // not logged in
            Debug.logWarning("Cannot associate distributor since not logged in yet", module);
            return "success";
        }
    }

    /** Update the affiliate association for the logged in user, if possible.
     *@param request The HTTPRequest object for the current request
     *@param response The HTTPResponse object for the current request
     *@return String specifying the exit status of this event
     */
    public static String updateAssociatedAffiliate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        GenericValue party = null;

        // SCIPIO: now points to shop
        java.net.URL shopPropertiesUrl = null;

        try {
            // SCIPIO: now points to shop
            // SCIPIO: get context using servlet API 3.0
            shopPropertiesUrl = request.getServletContext().getResource("/WEB-INF/shop.properties");
        } catch (java.net.MalformedURLException e) {
            Debug.logWarning(e, module);
        }

        String store = UtilProperties.getPropertyValue(shopPropertiesUrl, "affiliate.store.customer");

        if (store == null || store.toUpperCase().startsWith("N"))
            return "success";
        String storeOnClick = UtilProperties.getPropertyValue(shopPropertiesUrl, "affiliate.store.onclick");

        if (storeOnClick == null || storeOnClick.toUpperCase().startsWith("N"))
            return "success";

        try {
            party = userLogin == null ? null : userLogin.getRelatedOne("Party", false);
        } catch (GenericEntityException gee) {
            Debug.logWarning(gee, module);
        }

        if (party != null) {
            // if a distributorId is already associated, it will be used instead
            String currentAffiliateId = getId(party, "AFFILIATE");

            if (UtilValidate.isEmpty(currentAffiliateId)) {
                String affiliateId = (String) request.getSession().getAttribute(AFFILIATE_ID);

                if (UtilValidate.isNotEmpty(affiliateId)) {
                    // create PartyRelationship
                    GenericValue partyRelationship = delegator.makeValue("PartyRelationship", UtilMisc.toMap("partyIdFrom", party.getString("partyId"), "partyIdTo", affiliateId, "roleTypeIdFrom", "CUSTOMER", "roleTypeIdTo", "AFFILIATE"));

                    partyRelationship.set("fromDate", UtilDateTime.nowTimestamp());
                    partyRelationship.set("partyRelationshipTypeId", "SALES_AFFILIATE");
                    try {
                        delegator.create(partyRelationship);
                        if (Debug.infoOn()) Debug.logInfo("Affiliate for user " + party.getString("partyId") + " set to " + affiliateId, module);
                    } catch (GenericEntityException gee) {
                        Debug.logWarning(gee, module);
                    }
                } else {
                    // no distributorId is available
                    Debug.logInfo("No affiliate in session or already associated with user " + userLogin.getString("partyId"), module);
                    return "success";
                }
            } else {
                request.getSession().setAttribute(AFFILIATE_ID, currentAffiliateId);
            }

            return "success";
        } else {
            // not logged in
            Debug.logWarning("Cannot associate affiliate since not logged in yet", module);
            return "success";
        }
    }

    private static GenericValue getPartyRelationship(GenericValue party, String roleTypeTo) {
        try {
            return EntityUtil.getFirst(EntityUtil.filterByDate(party.getRelated("FromPartyRelationship", UtilMisc.toMap("roleTypeIdTo", roleTypeTo), null, false), true));
        } catch (GenericEntityException gee) {
            Debug.logWarning(gee, module);
        }
        return null;
    }

    private static String getId(GenericValue party, String roleTypeTo) {
        GenericValue partyRelationship = getPartyRelationship(party, roleTypeTo);

        return partyRelationship == null ? null : partyRelationship.getString("partyIdTo");
    }

}
