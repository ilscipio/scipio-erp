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
 * Scipio ERP - CMS Component
 *
 * Content Management System for website content,
 * pages, and digital assets.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("cms")
    globalName.set("cms")
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:widget"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":framework:webtools"))
    api(project(":applications:content"))
    api(project(":applications:party"))
    api(project(":applications:product"))
    api(project(":applications:solr"))

    // HTML parsing
    api(libs.jsoup)

    // Testing
    testImplementation(libs.bundles.testing)
}
