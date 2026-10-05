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
package com.ilscipio.scipio.countrypack.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Service definitions of the country-pack framework. Each service checks a permission (COUNTRYPACK_* or SETUP_ADMIN, which the
 * store owner holds); the MCP server "country-packs" writes an audit row for each call (blueprint 3, rule 4).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public class CountryPackServices {
    private static final String IMPL = "com.ilscipio.scipio.countrypack.service.CountryPackServiceImpl";

    @Service(
        name = "countryPackApply",
        engine = "java",
        location = IMPL,
        invoke = "apply",
        description = "Applies a country pack to a store: adds the jurisdictions to the compliance profile, creates the setup tasks and "
                + "publishes the legal texts from the templates of the pack (marked TEMPLATE - not legal advice). Safe to repeat: it never changes a text "
                + "that exists. The first pack of a store is the home pack; later packs are markets.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "packId", type = "String", mode = "IN", optional = "false", description = "us, de, at ..."),
            @Attribute(name = "roleId", type = "String", mode = "IN", optional = "true", description = "HOME or MARKET; default: HOME when the store has no home pack"),
            @Attribute(name = "result", type = "Map", mode = "OUT", optional = "true", description = "What the call did: created tasks, published texts, review tasks, marketOpen")
        }
    )
    public interface CountryPackApply {}

    @Service(
        name = "countryPackCompleteTask",
        engine = "java",
        location = IMPL,
        invoke = "completeTask",
        description = "Sets the state of a setup task: DONE (with the number when the task asks for one), NOT_NEEDED, or NEEDS_YOU again.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "packId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "taskId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true", description = "DONE (default), NOT_NEEDED, NEEDS_YOU"),
            @Attribute(name = "value", type = "String", mode = "IN", optional = "true", description = "The number that the task asks for"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "marketOpen", type = "Boolean", mode = "OUT", optional = "true", description = "True when all required tasks of the pack are closed")
        }
    )
    public interface CountryPackCompleteTask {}

    @Service(
        name = "countryPackUpgradeAll",
        engine = "java",
        location = IMPL,
        invoke = "upgradeAll",
        description = "Applies each pack again to each store that uses an older version of it. A new version adds tasks; a changed template "
                + "becomes a review task. No text of the seller changes. Permission: COUNTRYPACK_ADMIN or the group SCIPIO_OPS.",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", description = "Only this store (it must exist); default: every store of the tenant"),
            @Attribute(name = "results", type = "List", mode = "OUT", optional = "true", description = "One entry for each store and pack that changed")
        }
    )
    public interface CountryPackUpgradeAll {}

    @Service(
        name = "countryPackStatus",
        engine = "java",
        location = IMPL,
        invoke = "status",
        description = "The packs of a store with their tasks and states. This extends the setup checklist (scipio://setup/status).",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "status", type = "Map", mode = "OUT", optional = "true")
        }
    )
    public interface CountryPackStatus {}
}
