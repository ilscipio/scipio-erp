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
 * Scipio ERP - Start Component
 *
 * Application bootstrap and startup component providing
 * the main entry point for Scipio ERP.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("start")
    globalName.set("start")
    componentDependencies.set(listOf("base"))
}

dependencies {
    // Start component has minimal dependencies
    // It bootstraps the system and loads other components dynamically

    // Base component for core utilities
    implementation(project(":framework:base"))

    // Testing
    testImplementation(libs.bundles.testing)
}

// The start component produces the main executable JAR
tasks.named<Jar>("jar") {
    manifest {
        attributes(
            "Main-Class" to "org.ofbiz.base.start.Start",
            "Class-Path" to "."
        )
    }
}
