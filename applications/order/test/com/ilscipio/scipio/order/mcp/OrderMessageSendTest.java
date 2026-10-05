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
package com.ilscipio.scipio.order.mcp;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.order.service.OrderMessageServices;
import com.ilscipio.scipio.service.def.Service;

/** W1-08c: order/order message_send (service level; an MCP-level call needs a running server). */
public class OrderMessageSendTest {

    @Test
    void actionIsWiredToTheService() throws Exception {
        boolean found = false;
        for (McpServiceTool t : OrderMcp.class.getAnnotation(McpServer.class).serviceTools()) {
            if ("message_send".equals(t.name())) {
                assertEquals("order", t.topic());
                assertEquals("sendOrderMessage", t.service());
                found = true;
            }
        }
        assertTrue(found);
        assertNotNull(OrderMessageServices.class.getMethod("sendOrderMessage",
                DispatchContext.class, Map.class));
        boolean def = false;
        for (Class<?> c : OrderMessageServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            if (s != null && "sendOrderMessage".equals(s.name())) def = true;
        }
        assertTrue(def);
    }

    @Test
    void marketplaceChannelsAreRecognized() {
        assertTrue(OrderMessageServices.isMarketplaceChannelId("EBAY_SALES_CHANNEL"));
        assertTrue(OrderMessageServices.isMarketplaceChannelId("AMAZON_SALES_CHANNEL"));
        assertTrue(OrderMessageServices.isMarketplaceChannelId("amzn-de"));
        assertFalse(OrderMessageServices.isMarketplaceChannelId("WEB_SALES_CHANNEL"));
        assertFalse(OrderMessageServices.isMarketplaceChannelId(null));
        assertEquals("channel_message_not_supported", OrderMessageServices.ERR_CHANNEL);
    }

    @Test
    void emailCheck() {
        assertTrue(OrderMessageServices.isEmail("a@b.co"));
        assertFalse(OrderMessageServices.isEmail("a b@c.de"));
        assertFalse(OrderMessageServices.isEmail(null));
    }

    @Test
    void withoutPermissionTheServiceRefuses() {
        DispatchContext dctx = mock(DispatchContext.class);
        Security sec = mock(Security.class);
        when(dctx.getSecurity()).thenReturn(sec);
        Map<String, Object> r = OrderMessageServices.sendOrderMessage(dctx, Map.of("orderId", "X", "subject", "s", "body", "b"));
        assertTrue(ServiceUtil.isError(r));
        assertTrue(ServiceUtil.getErrorMessage(r).contains("ORDERMGR_UPDATE"));
    }

    @Test
    void senderUsesTheRealConfirmationEmailType() {
        java.util.List<String> types = java.util.Arrays.asList(OrderMessageServices.STORE_SENDER_EMAIL_TYPES);
        assertTrue(types.contains("PRDS_ODR_CONFIRM"));
        assertFalse(types.contains("PRDS_ORDER_CONFIRM"));
        assertTrue(types.contains("PRDS_ODR_CHANGE"));
    }
}
