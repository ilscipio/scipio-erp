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
 * Scipio ERP - Multi-Project Gradle Build Settings
 *
 * This file defines all subprojects (components) in the Scipio ERP system.
 * Components are organized into framework, applications, and themes.
 */

rootProject.name = "scipio-erp"

// Enable version catalog for centralized dependency management
enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

// Plugin management
pluginManagement {
    repositories {
        gradlePluginPortal()
        mavenCentral()
    }
}

// Dependency resolution management
dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.PREFER_PROJECT)
    repositories {
        mavenCentral()
        maven {
            url = uri("https://repo.maven.apache.org/maven2")
        }
    }
}

// ============================================================================
// FRAMEWORK COMPONENTS
// Core framework components that provide the foundation for Scipio ERP
// ============================================================================

include(":framework:base")
include(":framework:entity")
include(":framework:datafile")
include(":framework:entityext")
include(":framework:security")
include(":framework:service")
include(":framework:minilang")
include(":framework:widget")
include(":framework:mcp")
include(":framework:webapp")
include(":framework:common")
include(":framework:catalina")
include(":framework:start")
include(":framework:testtools")
include(":framework:webtools")
include(":framework:resources")

// ============================================================================
// APPLICATION COMPONENTS
// Business application modules built on top of the framework
// ============================================================================

include(":applications:commonext")
include(":applications:content")
include(":applications:datamodel")
include(":applications:party")
include(":applications:product")
include(":applications:manufacturing")
include(":applications:accounting")
include(":applications:workeffort")
include(":applications:order")
include(":applications:marketing")
include(":applications:compliance")
include(":applications:channel-core")
include(":applications:country-packs")
include(":applications:humanres")
include(":applications:securityext")
include(":applications:solr")
include(":applications:shop")
include(":applications:setup")
include(":applications:cms")
include(":applications:commerce-profile")

// ============================================================================
// THEME COMPONENTS
// UI themes and templating toolkits
// ============================================================================

include(":themes:base-theme")
include(":themes:metro")
include(":themes:bluelight")
include(":themes:ignite-admin")
include(":themes:aurora")

// ============================================================================
// PROJECT DIRECTORY MAPPING
// Map Gradle subproject paths to physical directories
// ============================================================================

// Framework mappings
project(":framework:base").projectDir = file("framework/base")
project(":framework:entity").projectDir = file("framework/entity")
project(":framework:datafile").projectDir = file("framework/datafile")
project(":framework:entityext").projectDir = file("framework/entityext")
project(":framework:security").projectDir = file("framework/security")
project(":framework:service").projectDir = file("framework/service")
project(":framework:minilang").projectDir = file("framework/minilang")
project(":framework:widget").projectDir = file("framework/widget")
project(":framework:mcp").projectDir = file("framework/mcp")
project(":framework:webapp").projectDir = file("framework/webapp")
project(":framework:common").projectDir = file("framework/common")
project(":framework:catalina").projectDir = file("framework/catalina")
project(":framework:start").projectDir = file("framework/start")
project(":framework:testtools").projectDir = file("framework/testtools")
project(":framework:webtools").projectDir = file("framework/webtools")
project(":framework:resources").projectDir = file("framework/resources")

// Applications mappings
project(":applications:commonext").projectDir = file("applications/commonext")
project(":applications:content").projectDir = file("applications/content")
project(":applications:datamodel").projectDir = file("applications/datamodel")
project(":applications:party").projectDir = file("applications/party")
project(":applications:product").projectDir = file("applications/product")
project(":applications:manufacturing").projectDir = file("applications/manufacturing")
project(":applications:accounting").projectDir = file("applications/accounting")
project(":applications:workeffort").projectDir = file("applications/workeffort")
project(":applications:order").projectDir = file("applications/order")
project(":applications:marketing").projectDir = file("applications/marketing")
project(":applications:compliance").projectDir = file("applications/compliance")
project(":applications:channel-core").projectDir = file("applications/channel-core")
project(":applications:country-packs").projectDir = file("applications/country-packs")
project(":applications:commerce-profile").projectDir = file("applications/commerce-profile")
project(":applications:humanres").projectDir = file("applications/humanres")
project(":applications:securityext").projectDir = file("applications/securityext")
project(":applications:solr").projectDir = file("applications/solr")
project(":applications:shop").projectDir = file("applications/shop")
project(":applications:setup").projectDir = file("applications/setup")
project(":applications:cms").projectDir = file("applications/cms")

// Themes mappings
project(":themes:base-theme").projectDir = file("themes/base-theme")
project(":themes:metro").projectDir = file("themes/metro")
project(":themes:bluelight").projectDir = file("themes/bluelight")
project(":themes:ignite-admin").projectDir = file("themes/ignite-admin")
project(":themes:aurora").projectDir = file("themes/aurora")

// ============================================================================
// HOT-DEPLOY AND ADDON COMPONENTS (discovered)
// Every directory under hot-deploy/ and addons/ joins the build when it holds a build.gradle.kts or a
// component descriptor (scipio-component.xml, scipio-theme.xml, ofbiz-component.xml). A directory with
// a descriptor but no build script gets the standard component build from the root build script. A
// component-load.xml in hot-deploy/ or addons/ fixes the order; otherwise the directories load in name
// order. A directory with only an Ant build.xml is skipped with a warning.
// ============================================================================

val scipioDescriptorOnlyProjects = mutableListOf<String>()

fun discoverScipioComponents(parentName: String) {
    val parent = file(parentName)
    if (!parent.isDirectory) return
    val loadFile = file("$parentName/component-load.xml")
    val loadOrder = if (loadFile.isFile) {
        Regex("component-location=\"([^\"]+)\"").findAll(loadFile.readText())
            .map { it.groupValues[1].trimEnd('/').substringAfterLast('/') }.toList()
    } else {
        emptyList()
    }
    val dirs = (parent.listFiles() ?: emptyArray())
        .filter { it.isDirectory && !it.name.startsWith(".") && it.name != "build" }
        .sortedWith(compareBy({ loadOrder.indexOf(it.name).let { i -> if (i < 0) Int.MAX_VALUE else i } }, { it.name }))
    for (dir in dirs) {
        val hasGradle = File(dir, "build.gradle.kts").isFile || File(dir, "build.gradle").isFile
        val hasDescriptor = listOf("scipio-component.xml", "scipio-theme.xml", "ofbiz-component.xml")
            .any { File(dir, it).isFile }
        if (!hasGradle && !hasDescriptor) {
            if (File(dir, "build.xml").isFile) {
                logger.warn("Scipio: ${dir.path} has only an Ant build.xml and no component descriptor; it is not built")
            }
            continue
        }
        val path = ":$parentName:${dir.name}"
        include(path)
        project(path).projectDir = dir
        if (!hasGradle) scipioDescriptorOnlyProjects.add(path)
    }
}

discoverScipioComponents("hot-deploy")
discoverScipioComponents("addons")
// SCIPIO: 4.0.0: specialpurpose/component-load.xml loads demosuite and assetmaint at runtime, so their Java must be
// built too; without it the demosuite scripts failed with "unable to resolve class ...AbstractDataGenerator".
discoverScipioComponents("specialpurpose")
gradle.rootProject { extra["scipioDescriptorOnlyProjects"] = scipioDescriptorOnlyProjects.toList() }
