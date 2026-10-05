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
 * Scipio Commerce - commerce profile component (W1-02)
 *
 * Hosted profile: security groups for store logins and tokens, and the guard that refuses code actions.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("commerce-profile")
    globalName.set("commerce-profile")
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:common"))

    // Testing: the seca test reads the MCP tool definitions of the CMS
    testImplementation(project(":framework:mcp"))
    testImplementation(project(":applications:cms"))
    testImplementation(libs.bundles.testing)
}
