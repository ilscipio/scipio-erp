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
 * Scipio ERP - Component Plugin
 *
 * Gradle plugin for Scipio ERP components. This plugin:
 * - Parses scipio-component.xml for runtime metadata
 * - Configures standard source layouts
 * - Sets up output directories matching Ant behavior
 */

package com.ilscipio.scipio.gradle

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.file.DuplicatesStrategy
import org.gradle.api.tasks.Copy
import org.gradle.api.tasks.SourceSetContainer
import org.gradle.jvm.tasks.Jar
import java.io.File

/**
 * Gradle plugin for Scipio ERP components.
 *
 * Apply to subprojects using:
 * ```
 * plugins {
 *     id("scipio-component")
 * }
 * ```
 */
class ScipioComponentPlugin : Plugin<Project> {

    override fun apply(project: Project) {
        // Create the DSL extension
        val extension = project.extensions.create(
            "scipioComponent",
            ScipioComponentExtension::class.java
        )

        // Set default values
        extension.componentName.convention(project.name)
        extension.globalName.convention(project.name)
        extension.exportServices.convention(false)
        extension.agentTools.convention(!project.path.startsWith(":framework:"))

        // Every application, addon and hot-deploy component gets the agent layer on its classpath, so a
        // component can declare @McpServer / @McpServerExtension classes without a build file edit.
        project.afterEvaluate {
            val mcp = project.rootProject.findProject(":framework:mcp")
            if (extension.agentTools.get() && mcp != null && project.path != mcp.path
                    && project.configurations.findByName("api") != null) {
                project.dependencies.add("api", mcp)
            }
        }

        // Parse scipio-component.xml if it exists (for runtime metadata only)
        val componentXml = project.file("scipio-component.xml")
        if (componentXml.exists()) {
            parseComponentXml(componentXml, extension, project)
        }

        // Configure after evaluation to allow user overrides
        project.afterEvaluate {
            configureSourceSets(project)
            configureOutputDirectories(project)
            configureProcessResources(project)
        }

        // Register utility tasks
        registerTasks(project)
    }

    /**
     * Parse scipio-component.xml and populate extension with metadata.
     * Note: This is for runtime metadata, NOT build dependencies.
     */
    private fun parseComponentXml(
        xmlFile: File,
        extension: ScipioComponentExtension,
        project: Project
    ) {
        try {
            val parser = ComponentXmlParser()
            val info = parser.parse(xmlFile)

            extension.componentName.set(info.name)
            extension.globalName.set(info.globalName)
            extension.webapps.set(info.webapps)

            // Log info for debugging
            project.logger.info("Parsed scipio-component.xml: ${info.name} (${info.globalName})")
            info.webapps.forEach { webapp ->
                project.logger.info("  Webapp: ${webapp.name} -> ${webapp.mount}")
            }
        } catch (e: Exception) {
            project.logger.warn("Failed to parse scipio-component.xml: ${e.message}")
        }
    }

    /**
     * Configure standard Scipio source set layout.
     */
    private fun configureSourceSets(project: Project) {
        val sourceSets = project.extensions.findByType(SourceSetContainer::class.java)
            ?: return

        sourceSets.named("main").configure {
            // Java sources
            java.setSrcDirs(listOf("src"))

            // Resource directories (only include those that exist)
            // Include "src" for .properties and other resources alongside Java files
            val resourceDirs = mutableListOf<String>()
            if (project.file("src").exists()) {
                resourceDirs.add("src")
            }
            resourceDirs.addAll(listOf(
                "config",
                "dtd",
                "servicedef",
                "entitydef",
                "data",
                "templates",
                "script",
                "widget"
            ).filter { project.file(it).exists() })

            resources.setSrcDirs(resourceDirs)
            // Exclude Java files from resources (they're handled by java source set)
            resources.exclude("**/*.java")
        }

        // Configure test sources - set to empty if no test directory exists
        sourceSets.named("test").configure {
            if (project.file("test").exists()) {
                java.setSrcDirs(listOf("test"))
            } else {
                // Clear test sources to prevent any default source scanning
                java.setSrcDirs(emptyList<String>())
            }
        }
    }

    /**
     * Configure output directories to match Ant behavior.
     */
    private fun configureOutputDirectories(project: Project) {
        // JAR output to build/lib
        project.tasks.withType(Jar::class.java).configureEach {
            destinationDirectory.set(project.file("build/lib"))
            archiveBaseName.set("scipio-${project.name}")
            archiveVersion.set("")  // Don't include version in JAR name for classpath consistency
        }

        // Configure classes output directories
        val sourceSets = project.extensions.findByType(SourceSetContainer::class.java)

        // Main classes output to build/classes
        sourceSets?.named("main")?.configure {
            java.destinationDirectory.set(project.file("build/classes"))
        }

        // Test classes output to build/test-classes (completely separate from main to avoid
        // Gradle detecting implicit dependencies between compileJava and compileTestJava)
        sourceSets?.named("test")?.configure {
            java.destinationDirectory.set(project.file("build/test-classes"))
        }
    }

    /**
     * Configure processResources task to handle duplicates.
     */
    private fun configureProcessResources(project: Project) {
        // Since src is in both java and resources source sets,
        // configure processResources to exclude duplicates
        project.tasks.withType(Copy::class.java).matching { it.name == "processResources" }.configureEach {
            duplicatesStrategy = DuplicatesStrategy.EXCLUDE
        }
    }

    /**
     * Register utility tasks for the component.
     */
    private fun registerTasks(project: Project) {
        // Component info task
        project.tasks.register("componentInfo") {
            group = "scipio"
            description = "Display component information"

            doLast {
                val ext = project.extensions.getByType(ScipioComponentExtension::class.java)
                println("Component: ${ext.componentName.get()}")
                println("Global Name: ${ext.globalName.get()}")
                println("Project Path: ${project.path}")

                val webapps = ext.webapps.getOrElse(emptyList())
                if (webapps.isNotEmpty()) {
                    println("Webapps:")
                    webapps.forEach { webapp ->
                        println("  - ${webapp.name}: ${webapp.mount}")
                    }
                }

                val deps = ext.componentDependencies.getOrElse(emptyList())
                if (deps.isNotEmpty()) {
                    println("Dependencies:")
                    deps.forEach { dep ->
                        println("  - $dep")
                    }
                }
            }
        }

        // List resources task
        project.tasks.register("listResources") {
            group = "scipio"
            description = "List all resource directories for this component"

            doLast {
                val sourceSets = project.extensions.findByType(SourceSetContainer::class.java)
                val mainSourceSet = sourceSets?.findByName("main")

                println("Source directories:")
                mainSourceSet?.java?.srcDirs?.forEach { dir ->
                    println("  - $dir (exists: ${dir.exists()})")
                }

                println("Resource directories:")
                mainSourceSet?.resources?.srcDirs?.forEach { dir ->
                    println("  - $dir (exists: ${dir.exists()})")
                }
            }
        }
    }
}
