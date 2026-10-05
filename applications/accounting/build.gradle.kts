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
 * Scipio ERP - Accounting Component
 *
 * Financial accounting including general ledger, accounts payable,
 * accounts receivable, and financial reporting.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("accounting")
    globalName.set("accounting")
}

dependencies {
    // SCIPIO: L-06b: test classes in src (legacy layout) compile against JUnit 4; the runtime jar comes with :framework:testtools
    compileOnly(libs.junit)

    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:widget"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":applications:content"))
    api(project(":applications:party"))
    api(project(":applications:product"))
    api(project(":applications:order"))

    // bank_statement_import MCP tool (CSV parsing)
    implementation(libs.commons.csv)

    // Testing
    testImplementation(libs.bundles.testing)
}

// Exclude third-party payment integrations without available libraries
sourceSets {
    main {
        java {
            exclude("**/thirdparty/**/*.java")  // All third-party payment integrations
        }
    }
}
