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
package com.ilscipio.scipio.common;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import java.util.Objects;

/**
 * Common Events
 */
public class CommonEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());


    /**
     * Checks if scipioSysMsg exists in parameter map and updates message indicator to isRead. Ensures that a user has read and followed up on a system message.
     * */
    public static String checkMessageRedirect(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        String sysmsgId = request.getParameter("scipioSysMsgId");
        if(userLogin!=null && UtilValidate.isNotEmpty(sysmsgId) && userLogin.get("partyId") != null){
            try {
                GenericValue systemMessage = EntityQuery.use(delegator).from("SystemMessages").where("messageId", sysmsgId).queryOne();
                if (systemMessage != null) {
                    if (Objects.equals(userLogin.get("partyId"), systemMessage.get("toPartyId"))) {
                        systemMessage.put("isRead", "Y");
                        systemMessage.store();
                    } else {
                        Debug.logError("SystemMessages [" + sysmsgId + "] does not belong to party [" +
                                userLogin.get("partyId") + "]", module);
                    }
                }
             } catch (Exception e) {
                 Debug.logWarning(e, "Problem updating systemMessage", module);
             }
        }
        return "success";
    }
}
