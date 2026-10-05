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
 * Scipio ERP - Widget Component
 *
 * Widget framework for UI component rendering including
 * forms, screens, menus, and trees.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("widget")
    globalName.set("widget")
    componentDependencies.set(listOf("base", "entity", "service"))
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:minilang"))
    api(project(":framework:entityext"))
    api(project(":framework:webapp"))

    // Tomcat (for ClientAbortException)
    api(libs.bundles.tomcat)

    // Testing
    testImplementation(libs.bundles.testing)
}
