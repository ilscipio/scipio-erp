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
/*
 * Scipio ERP - Agentic Commerce Protocol (ACP) Component
 *
 * Merchant side of ACP (agenticcommerce.dev, version 2026-04-17): discovery, checkout sessions (REST and the MCP
 * binding), Stripe Shared Payment Tokens (store key or the hub), signed order webhooks and the product feed.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("acp")
    globalName.set("acp")
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":framework:mcp"))
    api(project(":applications:party"))
    api(project(":applications:product"))
    api(project(":applications:order"))

    // Testing
    testImplementation(libs.bundles.testing)
}
