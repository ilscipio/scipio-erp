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
 * Scipio ERP - Test Tools Component
 *
 * Testing utilities, fixtures, and test infrastructure
 * for Scipio ERP components.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("testtools")
    globalName.set("testtools")
    componentDependencies.set(listOf("base", "entity", "service", "webapp"))
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:service"))
    api(project(":framework:webapp"))
    api(project(":framework:widget"))

    // Testing libraries
    api(libs.bundles.testing)

    // SCIPIO: L-06b: GroovyTestCase for GroovyScriptTestCase (groovy-all brought it, with JUnit, into every component)
    api(libs.groovy.test)

    // Ant JUnit for test runner
    api("org.apache.ant:ant-junit:1.10.14")
}
