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
 * Scipio ERP - Ignite Admin Theme
 *
 * Modern admin theme based on the Ignite Admin
 * template for Scipio ERP backend applications.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("ignite-admin")
    globalName.set("ignite-admin")
}

dependencies {
    implementation(project(":framework:base"))
    implementation(project(":framework:widget"))
    implementation(project(":framework:webapp"))
    implementation(project(":themes:base-theme"))

    // Testing
    testImplementation(libs.bundles.testing)
}

sourceSets {
    main {
        resources {
            srcDirs("webapp", "data", "templates")
        }
    }
}
