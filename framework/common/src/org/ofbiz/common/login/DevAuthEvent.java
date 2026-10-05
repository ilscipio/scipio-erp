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
package org.ofbiz.common.login;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * DEV ONLY: Auto-login event for development testing.
 * Remove before production deployment.
 */
public class DevAuthEvent {
    private static final String MODULE = DevAuthEvent.class.getName();

    public static final String KEY_PROPERTY = "security.dev.auth.bypass.key";
    public static final String KEY_HEADER = "X-Dev-Auth-Key";

    /**
     * SCIPIO: 4.0.0: The bypass is inactive unless {@code security.dev.auth.bypass.key} is set in
     * security.properties or as a JVM system property, and the request carries the same value in the
     * {@code X-Dev-Auth-Key} header. Never set the key on a production system.
     */
    public static boolean isEnabledFor(HttpServletRequest request) {
        String key = UtilProperties.getPropertyValue("security", KEY_PROPERTY);
        if (UtilValidate.isEmpty(key)) {
            key = System.getProperty(KEY_PROPERTY);
        }
        return UtilValidate.isNotEmpty(key) && key.equals(request.getHeader(KEY_HEADER));
    }

    public static String autoLoginSystem(HttpServletRequest request, HttpServletResponse response) {
        if (!isEnabledFor(request)) {
            return "success";
        }
        HttpSession session = request.getSession();
        if (session.getAttribute("userLogin") != null) {
            return "success";
        }

        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (delegator == null) {
            return "success";
        }

        try {
            GenericValue userLogin = EntityQuery.use(delegator)
                .from("UserLogin")
                .where("userLoginId", "admin")
                .queryOne();  // NO cache - avoids immutable entity issues

            if (userLogin != null) {
                // A logout anywhere sets hasLoggedOut=Y on the record; LoginWorker then rejects
                // the session and every page falls back to the login view.
                if (!"N".equals(userLogin.getString("hasLoggedOut"))) {
                    userLogin.set("hasLoggedOut", "N");
                    userLogin.store();
                }

                // Match LoginWorker.doBasicLogin() pattern
                session.setAttribute("userLogin", userLogin);
                request.setAttribute("login.result.userLogin", userLogin);

                // Load related Person/PartyGroup (required by some templates)
                try {
                    GenericValue person = userLogin.getRelatedOne("Person", false);
                    GenericValue partyGroup = userLogin.getRelatedOne("PartyGroup", false);
                    if (person != null) {
                        session.setAttribute("person", person);
                    }
                    if (partyGroup != null) {
                        session.setAttribute("partyGroup", partyGroup);
                    }
                } catch (Exception e) {
                    // Person/PartyGroup may not exist for system user - OK
                }

                // Note: VisitHandler.setUserLogin() skipped - not available in common module

                Debug.logInfo("DEV: Auto-logged in as admin user", MODULE);
            }
        } catch (GenericEntityException e) {
            Debug.logWarning("DEV: Could not auto-login: " + e.getMessage(), MODULE);
        }
        return "success";
    }
}
