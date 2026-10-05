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
package com.ilscipio.scipio.common.event;

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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://common/script/org/ofbiz/common/CdyneServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CdyneServices {

    private static final String MODULE = CdyneServices.class.getName();


    /**
     * Cdyne PostalAddress Fill In County
     */
    public static Map<String, Object> cdynePostalAddressFillInCounty(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue postalAddress = null;
        Map<String, Object> cdyneReturnCityStateMap = null;
        List<GenericValue> geoList = null;
        GenericValue countyGeo = null;
        try {
            postalAddress = EntityQuery.use(delegator)
                    .from("PostalAddress")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PostalAddress: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logVerbose("In cdynePostalAddressFillInCounty contactMechId=" + context.get("contactMechId") + "; postalAddress=" + postalAddress, MODULE);
        Object cdyneReturnCityStateMap_zipcode = null;
        Object postalAddress_countyGeoId = null;
        if ((!(UtilValidate.isEmpty(postalAddress)) && "USA".equals(postalAddress.get("countryGeoId")) && UtilValidate.isEmpty(postalAddress.get("countyGeoId")))) {
            cdyneReturnCityStateMap.put("zipcode", postalAddress.get("postalCode"));
            Map<String, Object> cdyneResultMap = null;
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("cdyneReturnCityState", cdyneReturnCityStateMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                cdyneResultMap = serviceResult;
            } catch (Exception e) {
                Debug.logError(e, "Error calling cdyneReturnCityState: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Debug.logVerbose("Got result from CDyne service: " + cdyneResultMap, MODULE);
            try {
                geoList = EntityQuery.use(delegator)
                        .from("GeoAssocAndGeoTo")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying GeoAssocAndGeoTo: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            countyGeo = EntityUtil.getFirst((List<GenericValue>) geoList);
            if (UtilValidate.isNotEmpty(countyGeo)) {
                postalAddress.put("countyGeoId", countyGeo.get("geoId"));
                try {
                    delegator.store(postalAddress);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }

}
