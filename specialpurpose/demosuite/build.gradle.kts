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
 * Scipio ERP - Demo suite (demo data generators)
 *
 * The JFairy data generator needs the jfairy library from the version catalog.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("demosuite")
    globalName.set("demosuite")
}

dependencies {
    rootProject.subprojects
        .filter { it.path.startsWith(":framework:") || it.path.startsWith(":applications:") }
        .forEach { api(it) }
    implementation(libs.jfairy)
}
