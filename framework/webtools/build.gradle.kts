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
 * Scipio ERP - Web Tools Component
 *
 * Administrative web tools for system management,
 * entity browser, service testing, and debugging.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("webtools")
    globalName.set("webtools")
    componentDependencies.set(listOf("base", "entity", "security", "service", "widget", "webapp", "common", "mcp"))
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:widget"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":framework:testtools"))
    api(project(":framework:mcp"))

    // POI for Excel import/export
    api(libs.bundles.poi)

    // Testing
    testImplementation(libs.bundles.testing)
}
