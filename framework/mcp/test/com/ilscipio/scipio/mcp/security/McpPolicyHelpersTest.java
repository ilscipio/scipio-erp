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
package com.ilscipio.scipio.mcp.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

public class McpPolicyHelpersTest {

    @Test
    public void globMatching() {
        assertTrue(McpConfig.globMatches("*UserLogin*", "createUserLogin"));
        assertTrue(McpConfig.globMatches("purge*", "purgeOldJobs"));
        assertTrue(McpConfig.globMatches("runService", "runService"));
        assertFalse(McpConfig.globMatches("runService", "runServiceX"));
        assertFalse(McpConfig.globMatches("*Password*", "createOrder"));
        assertTrue(McpConfig.anyGlobMatches(Arrays.asList("a*", "*Order*"), "createOrder"));
        assertFalse(McpConfig.anyGlobMatches(Arrays.asList("a*"), "createOrder"));
    }

    @Test
    public void remoteAddressRules() {
        assertTrue(McpAuthenticator.isRemoteAddrAllowed("", "1.2.3.4"));
        assertTrue(McpAuthenticator.isRemoteAddrAllowed("1.2.3.4", "1.2.3.4"));
        assertFalse(McpAuthenticator.isRemoteAddrAllowed("1.2.3.4", "1.2.3.5"));
        assertTrue(McpAuthenticator.isRemoteAddrAllowed("10.0.", "10.0.7.9"));
        assertTrue(McpAuthenticator.isRemoteAddrAllowed("192.168.1.0/24", "192.168.1.200"));
        assertFalse(McpAuthenticator.isRemoteAddrAllowed("192.168.1.0/24", "192.168.2.1"));
        assertTrue(McpAuthenticator.isRemoteAddrAllowed("10.0.0.0/8, 127.0.0.1", "127.0.0.1"));
        assertFalse(McpAuthenticator.isRemoteAddrAllowed("bad/99", "1.2.3.4"));
    }

    @Test
    public void rateLimiterBuckets() {
        McpRateLimiter rl = McpRateLimiter.get();
        String key = "test:" + System.nanoTime();
        assertTrue(rl.tryAcquire(key, 2));
        assertTrue(rl.tryAcquire(key, 2));
        assertFalse(rl.tryAcquire(key, 2));
        assertTrue(rl.tryAcquire(key + "-other", 1));
        assertTrue(rl.tryAcquire(key, 0));
        String slot = "slot:" + System.nanoTime();
        assertTrue(rl.tryAcquireSlot(slot, 1));
        assertFalse(rl.tryAcquireSlot(slot, 1));
        rl.release(slot);
        assertTrue(rl.tryAcquireSlot(slot, 1));
    }
}
