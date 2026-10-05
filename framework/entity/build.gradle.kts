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
 * Scipio ERP - Entity Component
 *
 * Entity engine providing database abstraction, entity definitions,
 * and data access layer functionality.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("entity")
    globalName.set("entity")
    componentDependencies.set(listOf("base"))
}

dependencies {
    // SCIPIO: L-06b: test classes in src (legacy layout) compile against JUnit 4; the runtime jar comes with :framework:testtools
    compileOnly(libs.junit)

    api(project(":framework:base"))

    // Database connection pooling
    api(libs.commons.dbcp2)

    // Database drivers
    api(libs.derby)
    api(libs.derbytools)
    api(libs.postgresql)

    // Geronimo Transaction Manager
    api(libs.geronimo.transaction)

    // Mockito (required for test classes in main source - legacy layout)
    api(libs.mockito.core)

    // Testing
    testImplementation(libs.bundles.testing)
}
