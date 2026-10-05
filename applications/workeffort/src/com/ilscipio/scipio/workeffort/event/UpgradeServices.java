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
package com.ilscipio.scipio.workeffort.event;

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
 * <p>Generated from: component://workeffort/script/org/ofbiz/workeffort/workeffort/UpgradeServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    private static final String MODULE = UpgradeServices.class.getName();


    /**
     * Migrate data from OldWorkEffortContactMech to WorkEffortContactMech
     */
    public static Map<String, Object> migrateWorkEffortContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue workEffortContactMech = null;
        List<GenericValue> oldWorkEffortContactMechs = null;
        try {
            oldWorkEffortContactMechs = EntityQuery.use(delegator)
                    .from("OldWorkEffortContactMech")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp fromDate = new Timestamp(System.currentTimeMillis());
        if (oldWorkEffortContactMechs != null) {
            for (GenericValue oldWorkEffortContactMech : oldWorkEffortContactMechs) {
                workEffortContactMech = delegator.makeValue("WorkEffortContactMech");
                workEffortContactMech.put("workEffortId", oldWorkEffortContactMech.get("workEffortId"));
                workEffortContactMech.put("contactMechId", oldWorkEffortContactMech.get("contactMechId"));
                workEffortContactMech.put("fromDate", fromDate);
                try {
                    delegator.create(workEffortContactMech);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }

}
