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
 * Scipio ERP - framework aggregate project
 *
 * The aggregate project holds the license check (work package L-06): gradlew checkLicensePolicy.
 * The policy data is gradle/license-policy.json; the classes and their reasons are in docs/licenses/policy.md.
 */

import com.github.jk1.license.LicenseReportExtension
import com.github.jk1.license.render.JsonReportRenderer
import com.github.jk1.license.render.ReportRenderer
import com.ilscipio.scipio.gradle.CheckLicensePolicyTask

// jk1 reads the licenses of each module from its POM, its manifest and its license files. It reads the
// configuration runtimeLibs of the root project (the union of the runtime jars of all components, the jars that
// syncLibs copies to framework/base/lib/gradle) and solrWebapp of :applications:solr (the jars in the Solr webapp).
// The projects of this build are not third-party modules. The plugin goes on the two projects from here, because the
// scope of work package L-06b excludes the root build file; jk1 needs its extension on each project that it reads.
val solrProject = project(":applications:solr")
for (p in listOf(rootProject, solrProject)) {
    p.pluginManager.apply("com.github.jk1.dependency-license-report")
}
rootProject.extensions.configure(LicenseReportExtension::class.java) {
    outputDir = rootProject.layout.buildDirectory.dir("reports/dependency-license").get().asFile.path
    projects = arrayOf(rootProject, solrProject)
    configurations = arrayOf("runtimeLibs", "solrWebapp")
    excludeGroups = arrayOf("com.ilscipio.scipio")
    excludeBoms = true
    renderers = arrayOf<ReportRenderer>(JsonReportRenderer("index.json", false))
}

tasks.register<CheckLicensePolicyTask>("checkLicensePolicy") {
    group = "verification"
    description = "Classify the license of each third-party module, committed jar and web library by gradle/license-policy.json; fail on a blocked one"
    dependsOn(rootProject.tasks.named("generateLicenseReport"))
    policyFile.set(rootProject.layout.projectDirectory.file("gradle/license-policy.json"))
    jk1Report.set(rootProject.layout.buildDirectory.file("reports/dependency-license/index.json"))
    reportFile.set(rootProject.layout.buildDirectory.file("reports/licenses/license-report.md"))
    repositoryRoot.set(rootProject.layout.projectDirectory)
}
