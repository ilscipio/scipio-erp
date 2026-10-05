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
package com.ilscipio.scipio.countrypack;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.ofbiz.service.DispatchContext;

import com.ilscipio.scipio.countrypack.entity.CountryPackEntities;
import com.ilscipio.scipio.countrypack.mcp.CountryPackMcp;
import com.ilscipio.scipio.countrypack.service.CountryPackServiceImpl;
import com.ilscipio.scipio.countrypack.service.CountryPackServices;
import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.KeyMap;
import com.ilscipio.scipio.entity.def.PrimaryKey;
import com.ilscipio.scipio.entity.def.Relation;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/** Checks the annotations without a running server: entities are consistent, services point to methods, MCP tools point to services. */
class CountryPackWiringTest {

    @Test
    void entityDefinitionsAreConsistent() {
        Set<String> names = new HashSet<>();
        for (Class<?> c : CountryPackEntities.class.getDeclaredClasses()) {
            Entity e = c.getAnnotation(Entity.class);
            assertNotNull(e, c.getName());
            assertTrue(names.add(e.name()), "duplicate entity " + e.name());
            assertTrue(e.name().length() <= 30, e.name());
            Set<String> fields = new HashSet<>();
            for (Field f : e.fields()) {
                assertTrue(fields.add(f.name()), e.name() + " duplicate field " + f.name());
                assertTrue(f.name().length() <= 30, f.name());
            }
            for (PrimaryKey pk : e.primaryKeys()) {
                assertTrue(fields.contains(pk.field()), e.name() + " primary key " + pk.field());
            }
            for (Relation r : e.relations()) {
                for (KeyMap km : r.keyMaps()) {
                    assertTrue(fields.contains(km.fieldName()), e.name() + " relation key " + km.fieldName());
                }
            }
        }
        assertEquals(Set.of("CountryPackAssignment", "CountryPackTask"), names);
    }

    @Test
    void everyServiceInvokesAnExistingMethod() throws Exception {
        Set<String> services = servicesOf();
        assertEquals(Set.of("countryPackApply", "countryPackCompleteTask", "countryPackStatus", "countryPackUpgradeAll"), services);
        for (Class<?> c : CountryPackServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            assertNotNull(s, c.getName());
            assertEquals(CountryPackServiceImpl.class.getName(), s.location(), s.name());
            Method m = CountryPackServiceImpl.class.getMethod(s.invoke(), DispatchContext.class, java.util.Map.class);
            assertTrue(java.lang.reflect.Modifier.isStatic(m.getModifiers()), s.invoke());
            assertEquals("true", s.auth(), s.name() + " needs a login");
            Set<String> attrs = new HashSet<>();
            for (Attribute a : s.attributes()) {
                assertTrue(attrs.add(a.name()), s.name() + " duplicate attribute " + a.name());
            }
        }
    }

    @Test
    void everyMcpServiceToolPointsToAService() {
        McpServer server = CountryPackMcp.class.getAnnotation(McpServer.class);
        assertNotNull(server);
        assertEquals("country-packs", server.name());
        Set<String> services = servicesOf();
        for (McpServiceTool t : server.serviceTools()) {
            assertTrue(services.contains(t.service()), "no service " + t.service());
            assertEquals("country-packs", t.topic());
        }
    }

    private static Set<String> servicesOf() {
        Set<String> names = new HashSet<>();
        for (Class<?> c : CountryPackServices.class.getDeclaredClasses()) {
            Service s = c.getAnnotation(Service.class);
            if (s != null) {
                names.add(s.name());
            }
        }
        return names;
    }
}
