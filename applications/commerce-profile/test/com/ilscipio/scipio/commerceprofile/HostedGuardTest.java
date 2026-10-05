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
package com.ilscipio.scipio.commerceprofile;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.commerceprofile.HostedGuard.Rule;

/**
 * The hosted rules: a store login is refused, an operator and the system pass, and nothing changes when the
 * profile is not hosted.
 */
public class HostedGuardTest {

    private Set<String> granted;

    @BeforeEach
    public void setUp() {
        granted = new HashSet<>();
        HostedProfile.hostedOverride = Boolean.TRUE;
    }

    @AfterEach
    public void tearDown() {
        HostedProfile.hostedOverride = null;
    }

    private GenericValue login(String id) {
        GenericValue ul = mock(GenericValue.class);
        when(ul.getString("userLoginId")).thenReturn(id);
        return ul;
    }

    private Security security() {
        Security s = mock(Security.class);
        when(s.hasPermission(anyString(), any(GenericValue.class))).thenAnswer(inv -> granted.contains(inv.getArgument(0)));
        when(s.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenAnswer(inv -> {
            String base = inv.getArgument(0);
            String action = inv.getArgument(1);
            return granted.contains(base + action) || granted.contains(base + "_ADMIN");
        });
        return s;
    }

    private static Map<String, Object> ctx(Object... kv) {
        Map<String, Object> m = new HashMap<>();
        for (int i = 0; i < kv.length; i += 2) m.put((String) kv[i], kv[i + 1]);
        return m;
    }

    @Test
    public void storeLoginIsRefusedOnCodeServices() {
        // The store owner holds every store permission that a token could carry, and even MCP_CODE_WRITE by mistake.
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "MCP_CODE_WRITE", "MCP_ENTITY_WRITE", "CMS_ADMIN", "CMS_UPDATE"));
        GenericValue owner = login("owner1");
        assertNotNull(HostedGuard.denyReason(Rule.CODE, owner, Collections.emptyMap(), security()));
        assertNotNull(HostedGuard.denyReason(Rule.CMS_CODE, owner, Collections.emptyMap(), security()));
    }

    @Test
    public void cmsCodeNeedsTheCodePermission() {
        GenericValue user = login("editor1");
        granted.add("CMS_UPDATE");
        assertNotNull(HostedGuard.denyReason(Rule.CMS_CODE, user, Collections.emptyMap(), security()));
        granted.add("CMS_CODE_UPDATE");
        assertNull(HostedGuard.denyReason(Rule.CMS_CODE, user, Collections.emptyMap(), security()));
    }

    @Test
    public void operatorAndSystemPass() {
        granted.add("HOSTED_OPS");
        for (Rule rule : Rule.values()) {
            assertNull(HostedGuard.denyReason(rule, login("scipio-ops"), ctx("sendVia", "smtp.evil", "filePath", "/x"), security()));
            assertNull(HostedGuard.denyReason(rule, login("system"), ctx("sendVia", "smtp.evil", "filePath", "/x"), security()));
        }
    }

    @Test
    public void noLoginIsRefused() {
        assertNotNull(HostedGuard.denyReason(Rule.CODE, null, Collections.emptyMap(), security()));
    }

    @Test
    public void mailServerParametersAreRefused() {
        GenericValue user = login("owner1");
        assertNull(HostedGuard.denyReason(Rule.MAIL_PARAMS, user, ctx("subject", "Hi", "sendTo", "a@b.c"), security()));
        assertNull(HostedGuard.denyReason(Rule.MAIL_PARAMS, user, ctx("sendVia", "", "authUser", null, "allowCustomHeaders", "N", "customHeaders", java.util.Collections.emptyMap(), "startTLSEnabled", "Y"), security()));
        assertNotNull(HostedGuard.denyReason(Rule.MAIL_PARAMS, user, ctx("allowCustomHeaders", "Y"), security()));
        assertNotNull(HostedGuard.denyReason(Rule.MAIL_PARAMS, user, ctx("allowCustomHeaders", Boolean.TRUE), security()));
        for (String p : HostedGuard.MAIL_PARAMS) {
            assertNotNull(HostedGuard.denyReason(Rule.MAIL_PARAMS, user, ctx(p, "x"), security()), p);
        }
    }

    @Test
    public void screenFileRules() {
        GenericValue user = login("owner1");
        String screen = "component://order/widget/X.xml#Y";
        assertNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("fileName", "a-1-", "screenLocation", screen), security()));
        assertNotNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("filePath", "/etc"), security()));
        assertNotNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("rootDir", "/"), security()));
        for (String bad : new String[] {"../x", "a/b", "a" + (char) 92 + "b", ".."}) {
            assertNotNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("fileName", bad), security()), bad);
        }
        assertNotNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("screenLocation", "/etc/x.xml"), security()));
        assertNotNull(HostedGuard.denyReason(Rule.SCREEN_FILE, user, ctx("screenLocation", "component://a/../../b"), security()));
    }

    @Test
    public void stayUnderTheStoreFolder() {
        java.io.File base = new java.io.File(System.getProperty("java.io.tmpdir"), "w102-scope");
        assertTrue(HostedGuard.staysUnder(base.getPath(), "invoice-1-"));
        assertFalse(HostedGuard.staysUnder(base.getPath(), "../evil"));
    }

    @Test
    public void importOnlyAllowedFiles() {
        String home = System.getProperty("java.io.tmpdir");
        String old = System.getProperty("ofbiz.home");
        System.setProperty("ofbiz.home", home);
        HostedGuard.importAllowOverride = Arrays.asList("a/data/Ok.xml");
        try {
            GenericValue user = login("owner1");
            String ok = new java.io.File(home, "a/data/Ok.xml").getPath();
            assertNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", ok), security()));
            assertNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", ok, "placeholderValues", new HashMap<>()), security()));
            assertNotNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", new java.io.File(home, "a/data/Other.xml").getPath()), security()));
            assertNotNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", ok, "fulltext", "<x/>"), security()));
            assertNotNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", ok, "isUrl", "Y"), security()));
            assertNotNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("fulltext", "<x/>"), security()));
            assertNotNull(HostedGuard.denyReason(Rule.IMPORT, user, ctx("filename", home + "/a/data/../data/../../etc/x"), security()));
        } finally {
            HostedGuard.importAllowOverride = null;
            if (old == null) System.clearProperty("ofbiz.home"); else System.setProperty("ofbiz.home", old);
        }
    }

    @Test
    public void cmsCodeViewRule() {
        GenericValue user = login("editor1");
        assertNotNull(HostedGuard.denyReason(Rule.CMS_VIEW, user, Collections.emptyMap(), security()));
        granted.add("CMS_CODE_VIEW");
        assertNull(HostedGuard.denyReason(Rule.CMS_VIEW, user, Collections.emptyMap(), security()));
    }

    @Test
    public void hostedFlagFailsClosed() {
        HostedProfile.hostedOverride = null;
        for (String v : new String[] {"true", "TRUE ", "1", "yes", "", null, "maybe"}) {
            assertTrue(HostedProfile.parse(v), String.valueOf(v));
        }
        assertFalse(HostedProfile.parse("false"));
        assertFalse(HostedProfile.parse(" FALSE "));
    }

    @Test
    public void notHostedChangesNothing() {
        HostedProfile.hostedOverride = Boolean.FALSE;
        GenericValue user = login("owner1");
        for (Rule rule : Rule.values()) {
            assertNull(HostedGuard.denyReason(rule, user, ctx("sendVia", "smtp", "filePath", "/x"), security()));
            assertNull(HostedGuard.denyReason(rule, null, ctx("sendVia", "smtp", "filePath", "/x"), security()));
        }
    }
}
