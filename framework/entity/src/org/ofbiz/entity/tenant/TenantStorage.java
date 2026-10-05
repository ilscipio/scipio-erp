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
package org.ofbiz.entity.tenant;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;

/**
 * SCIPIO: 4.0.0: Pooled runtime: object storage for store files (G7). Keys have the form
 * {@code tenants/<tenantId>/<area>/<path>} (see {@link TenantFiles#key}); a backend never builds keys itself.
 */
public interface TenantStorage {

    /** Stores the file under the key (replaces an existing object). */
    void put(String key, Path file, String contentType) throws IOException;

    /** The object, or null when the key does not exist. The caller closes the stream. */
    InputStream get(String key) throws IOException;

    /** Deletes the object; no error when it does not exist. */
    void delete(String key) throws IOException;

    /** A short description for logs (never secrets). */
    String describe();
}
