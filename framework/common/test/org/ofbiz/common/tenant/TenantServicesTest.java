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
package org.ofbiz.common.tenant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * SCIPIO: 4.0.0: Pooled runtime: the pure parts of the store life cycle services (G18).
 */
public class TenantServicesTest {

    @Test
    public void jdbcUriWithDatabase() {
        assertEquals("jdbc:postgresql://db:5432/t_001", TenantServices.withDatabase("jdbc:postgresql://db:5432/postgres", "t_001"));
        assertEquals("jdbc:postgresql://db:6432/t_001?prepareThreshold=0",
                TenantServices.withDatabase("jdbc:postgresql://db:6432/postgres?prepareThreshold=0", "t_001"));
    }

    @Test
    public void databaseOfUri() {
        assertEquals("t_001", TenantServices.databaseOf("jdbc:postgresql://127.0.0.1:6432/t_001?prepareThreshold=0"));
        assertNull(TenantServices.databaseOf("jdbc:postgresql://127.0.0.1:6432/t_001;drop"));
        assertNull(TenantServices.databaseOf("jdbc:postgresql://127.0.0.1:6432/T\"x"));
        assertNull(TenantServices.databaseOf(null));
    }

    @Test
    public void initScriptSplitsIntoStatements() throws IOException {
        String sql = new String(Files.readAllBytes(Paths.get("config/tenant-init.sql")), StandardCharsets.UTF_8);
        List<String> statements = TenantServices.splitSql(sql);
        assertEquals(4, statements.size());
        for (String statement : statements) {
            assertTrue(!statement.endsWith(";") && !statement.startsWith("--"), statement);
        }
        assertTrue(statements.get(3).startsWith("UPDATE solr_status"));
    }
}
