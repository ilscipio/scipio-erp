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
 * Scipio ERP - Webapp Component
 *
 * Web application framework providing request handling,
 * controllers, view rendering, and web utilities.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("webapp")
    globalName.set("webapp")
    componentDependencies.set(listOf("base", "entity", "security", "service", "widget"))
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:minilang"))
    api(project(":framework:catalina"))

    // Tomcat (for WebXml, FilterDef, etc.)
    api(libs.bundles.tomcat)

    // PDF generation
    api(libs.openpdf)

    // RSS feeds
    api(libs.rome)

    // URL rewriting
    api(libs.urlrewritefilter)

    // JasperReports
    // SCIPIO: L-06b: OpenPDF (com.lowagie packages) replaces the iText 2.1.7.js8 of JasperReports
    compileOnly(libs.jasperreports) {
        exclude(group = "com.lowagie", module = "itext")
    }

    // Testing
    testImplementation(libs.bundles.testing)
}

// Exclude JasperReports handlers (uses optional JasperReports and has missing imports)
sourceSets {
    main {
        java {
            exclude("**/JasperReports*ViewHandler.java")
        }
    }
}
