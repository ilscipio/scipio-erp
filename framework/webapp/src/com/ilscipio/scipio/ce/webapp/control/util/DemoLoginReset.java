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
package com.ilscipio.scipio.ce.webapp.control.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.entity.demo.DemoReset;

/**
 * SCIPIO: 4.0.0: Public demo: each login of a shipped demo account (a UserLogin of the data files) puts the account
 * back to the data files - its user preferences (theme, colour scheme, ...), its language and time zone - so that one
 * visitor's settings do not stay for the next. Runs from {@link org.ofbiz.webapp.control.LoginWorker#doMainLogin}
 * before the session reads the settings; off unless general.properties demo.reset.enabled=true (see {@link DemoReset}).
 */
public final class DemoLoginReset {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Preferences that the login itself writes (LoginWorker.login); they stay. */
    private static final Set<String> KEEP_TYPES = Set.of("javaScriptEnabled");
    /** UserLogin fields that the session takes over at login (AttrHandler basic login event). */
    private static final List<String> LOGIN_FIELDS = List.of("lastLocale", "lastTimeZone");
    /** Browser-side setting of the Aurora backend theme (light/dark); the preference AURORA_SCHEME is the stored one. */
    private static final String SCHEME_COOKIE = "auroraScheme";

    private DemoLoginReset() {
    }

    public static void resetUser(HttpServletResponse response, GenericValue userLogin) {
        if (userLogin == null || !DemoReset.isEnabled()) {
            return;
        }
        Delegator delegator = userLogin.getDelegator();
        String userLoginId = userLogin.getString("userLoginId");
        GenericValue fileLogin = null;
        for (GenericValue v : DemoReset.getBaseline(delegator, "UserLogin")) {
            if (userLoginId.equals(v.getString("userLoginId"))) {
                fileLogin = v;
            }
        }
        if (fileLogin == null) {
            return; // an account a visitor created: nobody else uses it
        }
        try {
            Map<String, GenericValue> filePrefs = new HashMap<>();
            for (GenericValue v : DemoReset.getBaseline(delegator, "UserPreference")) {
                if (userLoginId.equals(v.getString("userLoginId"))) {
                    filePrefs.put(DemoReset.pkKey(v), v);
                }
            }
            int removed = 0;
            int restored = 0;
            for (GenericValue pref : EntityQuery.use(delegator).from("UserPreference").where("userLoginId", userLoginId).queryList()) {
                GenericValue filePref = filePrefs.remove(DemoReset.pkKey(pref));
                if (filePref == null) {
                    if (!KEEP_TYPES.contains(pref.getString("userPrefTypeId"))) {
                        pref.remove();
                        removed++;
                    }
                } else if (DemoReset.differs(filePref, pref)) {
                    delegator.makeValue("UserPreference", filePref).store();
                    restored++;
                }
            }
            for (GenericValue filePref : filePrefs.values()) {
                delegator.makeValue("UserPreference", filePref).create();
                restored++;
            }
            // language and time zone of the data file; none there: the browser's
            GenericValue dbLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", userLoginId).queryOne();
            boolean loginChanged = false;
            for (String field : LOGIN_FIELDS) {
                Object fileValue = fileLogin.get(field);
                if (dbLogin != null && !Objects.equals(dbLogin.get(field), fileValue)) {
                    dbLogin.set(field, fileValue);
                    loginChanged = true;
                }
                userLogin.set(field, fileValue); // the session copy
            }
            if (loginChanged) {
                dbLogin.store();
            }
            if (response != null) {
                Cookie cookie = new Cookie(SCHEME_COOKIE, "");
                cookie.setPath("/");
                cookie.setMaxAge(0);
                response.addCookie(cookie);
            }
            if (removed > 0 || restored > 0 || loginChanged) {
                Debug.logInfo("Demo reset: login of " + userLoginId + ": " + removed + " preferences removed, " + restored
                        + " restored" + (loginChanged ? ", language and time zone reset" : ""), module);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Demo reset: cannot reset user " + userLoginId, module);
        }
    }
}
