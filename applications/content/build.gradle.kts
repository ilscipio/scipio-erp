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
 * Scipio ERP - Content Component
 *
 * Content Management System providing document management,
 * content publishing, and digital asset management.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("content")
    globalName.set("content")
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:entityext"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:minilang"))
    api(project(":framework:widget"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":framework:datafile"))

    // POI for Office documents
    api(libs.bundles.poi)

    // PDF
    api(libs.bundles.pdf)

    // JasperReports (optional - for report data sources)
    // SCIPIO: L-06b: OpenPDF (com.lowagie packages) replaces the iText 2.1.7.js8 of JasperReports
    compileOnly(libs.jasperreports) {
        exclude(group = "com.lowagie", module = "itext")
    }

    // Testing
    testImplementation(libs.bundles.testing)
}

// Exclude optional integrations
sourceSets {
    main {
        java {
            exclude("**/openoffice/*.java")  // Requires LibreOffice UNO API
        }
    }
}
