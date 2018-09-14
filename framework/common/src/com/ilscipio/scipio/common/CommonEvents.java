/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.common;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

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
        if(userLogin!=null && UtilValidate.isNotEmpty(sysmsgId)){
            try {
                GenericValue systemMessage = null;
                systemMessage = EntityQuery.use(delegator).from("SystemMessages").where("messageId", sysmsgId,"toPartyId",userLogin.get("partyId")).queryOne();
                if (systemMessage != null) {
                    systemMessage.put("isRead", "Y");
                    systemMessage.store();
                }
             } catch (Exception e) {
                 Debug.logWarning(e, "Problem updating systemMessage", module);
             }

        }

        return "success";
    }
}
