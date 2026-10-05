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
 * Scipio ERP - Service Component
 *
 * Service engine providing service definitions, invocation,
 * SOAP/REST web services, and service orchestration.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("service")
    globalName.set("service")
    componentDependencies.set(listOf("base", "entity", "security"))
    exportServices.set(true)
}

dependencies {
    // SCIPIO: L-06b: test classes in src (legacy layout) compile against JUnit 4; the runtime jar comes with :framework:testtools
    compileOnly(libs.junit)

    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))

    // Web Services (Axis2)
    // SCIPIO: L-06b: no axis2-codegen (a code generator, no use at runtime; it brought javac-shaded, GPL with the Classpath
    // exception); the servlet API comes from Tomcat (:framework:base), not javax.servlet-api 3.1.0 through axis2-kernel
    api(libs.bundles.axis2) {
        exclude(group = "javax.servlet", module = "javax.servlet-api")
    }

    // Tomcat utilities (StringUtils)
    api(libs.tomcat.util)

    // Testing
    testImplementation(libs.bundles.testing)
}
