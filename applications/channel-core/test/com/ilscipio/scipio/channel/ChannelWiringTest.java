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
package com.ilscipio.scipio.channel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.ofbiz.service.DispatchContext;

import com.ilscipio.scipio.channel.entity.ChannelEntities;
import com.ilscipio.scipio.channel.mcp.ChannelMcp;
import com.ilscipio.scipio.channel.service.ChannelServiceImpl;
import com.ilscipio.scipio.channel.service.ChannelServices;
import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.Index;
import com.ilscipio.scipio.entity.def.IndexField;
import com.ilscipio.scipio.entity.def.KeyMap;
import com.ilscipio.scipio.entity.def.PrimaryKey;
import com.ilscipio.scipio.entity.def.Relation;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * Checks the annotations without a running server: the entity definitions are consistent, each service points to a
 * method that exists, and each MCP service tool points to a service that channel-core defines.
 */
public class ChannelWiringTest {

    @Test
    void entityDefinitionsAreConsistent() {
        Set<String> names = new HashSet<>();
        int count = 0;
        for (Class<?> c : ChannelEntities.class.getDeclaredClasses()) {
            Entity e = c.getAnnotation(Entity.class);
            assertNotNull(e, c.getName());
            count++;
            assertTrue(names.add(e.name()), "duplicate entity " + e.name());
            assertTrue(e.name().length() <= 30, "entity name too long: " + e.name());
            Set<String> fields = new HashSet<>();
            for (Field f : e.fields()) {
                assertTrue(fields.add(f.name()), e.name() + " duplicate field " + f.name());
                assertTrue(f.name().length() <= 30, e.name() + " field name too long: " + f.name());
            }
            assertTrue(e.primaryKeys().length > 0, e.name() + " has no primary key");
            for (PrimaryKey pk : e.primaryKeys()) {
                assertTrue(fields.contains(pk.field()), e.name() + " primary key " + pk.field() + " is not a field");
            }
            for (Relation r : e.relations()) {
                for (KeyMap km : r.keyMaps()) {
                    assertTrue(fields.contains(km.fieldName()), e.name() + " relation key " + km.fieldName() + " is not a field");
                }
            }
            Set<String> indexNames = new HashSet<>();
            for (Index i : e.indexes()) {
                assertTrue(indexNames.add(i.name()), e.name() + " duplicate index " + i.name());
                assertTrue(i.name().length() <= 18, "index name too long for some databases: " + i.name());
                for (IndexField f : i.fields()) {
                    assertTrue(fields.contains(f.name()), e.name() + " index field " + f.name() + " is not a field");
                }
            }
        }
        assertEquals(7, count, "ChannelSetting, ChannelListing, ChannelProductData, ChannelStockRule, ChannelOrderRef, ChannelSyncTask, ChannelOutboxEvent");
    }

    @Test
    void everyServiceInvokesAnExistingMethod() throws Exception {
        Set<String> serviceNames = servicesOf();
        assertTrue(serviceNames.size() >= 9);
        for (Class<?> c : ChannelServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            if (s == null) {
                continue; // a SECA
            }
            assertEquals(ChannelServiceImpl.class.getName(), s.location(), s.name());
            Method m = ChannelServiceImpl.class.getMethod(s.invoke(), DispatchContext.class, java.util.Map.class);
            assertTrue(java.lang.reflect.Modifier.isStatic(m.getModifiers()), s.invoke() + " is static");
            Set<String> attrs = new HashSet<>();
            for (Attribute a : s.attributes()) {
                assertTrue(attrs.add(a.name()), s.name() + " duplicate attribute " + a.name());
            }
        }
    }

    @Test
    void everyMcpServiceToolPointsToAChannelService() {
        McpServer server = ChannelMcp.class.getAnnotation(McpServer.class);
        assertNotNull(server);
        assertEquals("channel", server.name());
        Set<String> services = servicesOf();
        Set<String> actions = new HashSet<>();
        for (McpServiceTool t : server.serviceTools()) {
            assertTrue(services.contains(t.service()), "no service " + t.service());
            assertEquals("channel", t.topic());
            assertTrue(actions.add(t.name()), "duplicate action " + t.name());
        }
        for (String required : new String[] {"stock_claim", "stock_report", "order_intake", "buyer_deletion", "save_setting",
                "outbox_claim", "outbox_ack", "outbox_release", "outbox_purge", "outbox_parked", "product_data"}) {
            assertTrue(actions.contains(required), "missing action " + required);
        }
    }

    /** The outbox hooks run in the transaction of their cause: event commit (before the commit), no new transaction, errors fail the cause. */
    @Test
    void outboxHooksRunInTheTransactionOfTheirCause() {
        Set<String> hooked = new HashSet<>();
        Set<String> services = servicesOf();
        for (Class<?> c : ChannelServices.class.getDeclaredClasses()) {
            for (com.ilscipio.scipio.service.def.seca.Seca seca : c.getAnnotationsByType(com.ilscipio.scipio.service.def.seca.Seca.class)) {
                for (com.ilscipio.scipio.service.def.seca.SecaAction a : seca.actions()) {
                    if (!a.service().startsWith("channelOutbox")) {
                        continue;
                    }
                    assertTrue(services.contains(a.service()), "no service " + a.service());
                    assertEquals("commit", seca.event(), a.service());
                    assertEquals("sync", a.mode(), a.service());
                    assertEquals("false", a.newTransaction(), a.service());
                    assertEquals("false", a.ignoreError(), a.service());
                    hooked.add(seca.service());
                }
            }
        }
        assertEquals(new HashSet<>(java.util.Arrays.asList("storeOrder", "changeOrderStatus", "createInventoryItemDetail")), hooked);
    }

    private static Set<String> servicesOf() {
        Set<String> names = new HashSet<>();
        for (Class<?> c : ChannelServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            if (s != null) {
                names.add(s.name());
            }
        }
        return names;
    }
}
