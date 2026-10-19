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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Services of the store pause (R4-15, J5-01). The row {@code ProductStorePause} is the only state of the pause. The MCP server
 * {@code catalog} gives these services as the tools {@code store/pause}, {@code store/resume} and {@code store/pause_state}; the
 * MCP call writes the McpAuditLog row. channel-core plans the quantities of the sales channels after the commit (its service
 * ECA on these services).
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public class StorePauseServices {
    private static final String IMPL = "com.ilscipio.scipio.product.store.StorePauseServiceImpl";

    @Service(
        name = "pauseProductStore",
        engine = "java",
        location = IMPL,
        invoke = "pause",
        description = "Pauses the store: the storefront shows the note and takes no order, and each sales channel gets quantity 0. "
                + "Open orders stay. outcome PAUSED, or ALREADY_PAUSED (nothing changes).",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", description = "Default: the store of the call, else the only store"),
            @Attribute(name = "untilDate", type = "String", mode = "IN", optional = "true", description = "The back-on day, ISO-8601 with a zone (2026-10-12T23:59:59Z); empty: no back-on day"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true", description = "The note for the buyers, at most 300 characters"),
            @Attribute(name = "outcome", type = "String", mode = "OUT", optional = "false", description = "PAUSED or ALREADY_PAUSED"),
            @Attribute(name = "paused", type = "String", mode = "OUT", optional = "true", description = "Y or N"),
            @Attribute(name = "pausedDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseUntilDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseNote", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "stateSeq", type = "Long", mode = "OUT", optional = "true", description = "Counts up at each pause and resume; the reader keeps the answer with the higher number")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface PauseProductStore {}

    @Service(
        name = "resumeProductStore",
        engine = "java",
        location = IMPL,
        invoke = "resume",
        description = "Ends the pause of the store: the storefront takes orders again and each sales channel gets its quantity of the "
                + "stock rule. outcome RESUMED, or NOT_PAUSED (nothing changes). The row stays (pausedDate empty) for the reconcile counters.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", description = "Default: the store of the call, else the only store"),
            @Attribute(name = "outcome", type = "String", mode = "OUT", optional = "false", description = "RESUMED or NOT_PAUSED"),
            @Attribute(name = "paused", type = "String", mode = "OUT", optional = "true", description = "Y or N"),
            @Attribute(name = "pausedDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseUntilDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseNote", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "stateSeq", type = "Long", mode = "OUT", optional = "true", description = "Counts up at each pause and resume; the reader keeps the answer with the higher number")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface ResumeProductStore {}

    @Service(
        name = "getProductStorePause",
        engine = "java",
        location = IMPL,
        invoke = "state",
        description = "Read only. The pause of the store: paused (Y or N), since, back-on day and note. A pause whose back-on day passed reads as live.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", description = "Default: the store of the call, else the only store"),
            @Attribute(name = "paused", type = "String", mode = "OUT", optional = "true", description = "Y or N"),
            @Attribute(name = "pausedDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseUntilDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pauseNote", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "stateSeq", type = "Long", mode = "OUT", optional = "true", description = "Counts up at each pause and resume; the reader keeps the answer with the higher number")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "VIEW")
    )
    public interface GetProductStorePause {}

    @Service(
        name = "resumeDueProductStores",
        engine = "java",
        location = IMPL,
        invoke = "resumeDue",
        description = "Job: resumes each store whose back-on day passed (through resumeProductStore, one transaction for each store, with an McpAuditLog row).",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "resumed", type = "Integer", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "productStoreGenericPermission", mainAction = "UPDATE")
    )
    public interface ResumeDueProductStores {}
}
