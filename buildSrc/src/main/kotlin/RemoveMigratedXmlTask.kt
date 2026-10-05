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
 * Scipio ERP - Remove Migrated XML Task
 *
 * Gradle task that removes XML files that have been fully migrated to Java annotations.
 * Only removes XML files when a corresponding Java annotation file exists.
 *
 * Usage:
 *   ./gradlew removeMigratedXml -Pcomponent=cms
 *   ./gradlew removeMigratedXml -Pcomponent=content
 *   ./gradlew removeMigratedXml -Pcomponent=cms -PdryRun=true  // Preview only, don't delete
 */

package com.ilscipio.scipio.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.options.Option
import java.io.File
import javax.xml.parsers.DocumentBuilderFactory

/**
 * Gradle task for removing XML files that have been migrated to Java annotations.
 * Only removes XML files when corresponding Java annotation files exist.
 */
open class RemoveMigratedXmlTask : DefaultTask() {

    init {
        group = "scipio"
        description = "Remove XML files that have been fully migrated to Java annotations"
    }

    @get:Input
    @get:Optional
    @Option(option = "component", description = "Component name to scan for migrated XML files")
    var component: String? = null

    @get:Input
    @get:Optional
    @Option(option = "dryRun", description = "Preview only - don't actually delete files (true/false)")
    var dryRun: String? = null

    data class MigrationMapping(
        val xmlFile: File,
        val javaFile: File,
        val type: String,
        val exists: Boolean
    )

    @TaskAction
    fun removeMigrated() {
        val componentName = component
            ?: project.findProperty("component")?.toString()
            ?: throw GradleException("Component name required. Use -Pcomponent=<name>")

        val isDryRun = (dryRun ?: project.findProperty("dryRun")?.toString()) == "true"

        logger.lifecycle("")
        logger.lifecycle("=" .repeat(70))
        logger.lifecycle("Scanning component '$componentName' for migrated XML files...")
        logger.lifecycle("=" .repeat(70))

        if (isDryRun) {
            logger.lifecycle("DRY RUN MODE - No files will be deleted")
        }
        logger.lifecycle("")

        val componentDir = findComponentDir(componentName)
        val mappings = scanForMigratedFiles(componentDir, componentName)

        // Separate into migrated (can remove) and not migrated (keep)
        val migrated = mappings.filter { it.exists }
        val notMigrated = mappings.filter { !it.exists }

        // Report findings
        logger.lifecycle("SCAN RESULTS:")
        logger.lifecycle("-" .repeat(70))

        if (migrated.isNotEmpty()) {
            logger.lifecycle("")
            logger.lifecycle("MIGRATED (${migrated.size} files - will be removed):")
            migrated.groupBy { it.type }.forEach { (type, files) ->
                logger.lifecycle("  $type:")
                files.forEach { mapping ->
                    logger.lifecycle("    [OK] ${mapping.xmlFile.relativeTo(componentDir)}")
                    logger.lifecycle("         -> ${mapping.javaFile.relativeTo(componentDir)}")
                }
            }
        }

        if (notMigrated.isNotEmpty()) {
            logger.lifecycle("")
            logger.lifecycle("NOT MIGRATED (${notMigrated.size} files - will be kept):")
            notMigrated.groupBy { it.type }.forEach { (type, files) ->
                logger.lifecycle("  $type:")
                files.forEach { mapping ->
                    logger.lifecycle("    [SKIP] ${mapping.xmlFile.relativeTo(componentDir)}")
                    logger.lifecycle("           (no Java file: ${mapping.javaFile.relativeTo(componentDir)})")
                }
            }
        }

        logger.lifecycle("")
        logger.lifecycle("-" .repeat(70))
        logger.lifecycle("Summary: ${migrated.size} migrated, ${notMigrated.size} not migrated")
        logger.lifecycle("-" .repeat(70))

        // Remove migrated files
        if (migrated.isNotEmpty()) {
            logger.lifecycle("")
            if (isDryRun) {
                logger.lifecycle("DRY RUN - Would remove ${migrated.size} XML file(s)")
            } else {
                logger.lifecycle("Removing ${migrated.size} migrated XML file(s)...")
                var removed = 0
                var failed = 0
                migrated.forEach { mapping ->
                    if (mapping.xmlFile.delete()) {
                        logger.lifecycle("  Deleted: ${mapping.xmlFile.relativeTo(componentDir)}")
                        removed++
                    } else {
                        logger.warn("  FAILED to delete: ${mapping.xmlFile.path}")
                        failed++
                    }
                }
                logger.lifecycle("")
                logger.lifecycle("Removal complete: $removed deleted, $failed failed")
            }
        } else {
            logger.lifecycle("")
            logger.lifecycle("No migrated XML files to remove.")
        }

        logger.lifecycle("")
    }

    /**
     * Finds the component directory.
     */
    private fun findComponentDir(componentName: String): File {
        // Try applications first, then framework, then hot-deploy
        var componentDir = project.file("applications/$componentName")
        if (!componentDir.exists()) {
            componentDir = project.file("framework/$componentName")
        }
        if (!componentDir.exists()) {
            componentDir = project.file("hot-deploy/$componentName")
        }
        if (!componentDir.exists()) {
            throw GradleException("Component directory not found: $componentName")
        }
        return componentDir
    }

    /**
     * Scans a component for XML files and their corresponding Java annotation files.
     */
    private fun scanForMigratedFiles(componentDir: File, componentName: String): List<MigrationMapping> {
        val mappings = mutableListOf<MigrationMapping>()
        val srcDir = File(componentDir, "src")

        // Scan entity definitions
        val entityDefDir = File(componentDir, "entitydef")
        if (entityDefDir.exists()) {
            scanEntityDef(entityDefDir, srcDir, componentName, mappings)
        }

        // Scan service definitions
        val serviceDefDir = File(componentDir, "servicedef")
        if (serviceDefDir.exists()) {
            scanServiceDef(serviceDefDir, srcDir, componentName, mappings)
        }

        // Scan widget definitions
        val widgetDir = File(componentDir, "widget")
        if (widgetDir.exists()) {
            scanWidgetDir(widgetDir, srcDir, componentName, componentDir, mappings)
        }

        return mappings
    }

    /**
     * Scans entitydef directory for entity and EECA XML files.
     */
    private fun scanEntityDef(
        entityDefDir: File,
        srcDir: File,
        componentName: String,
        mappings: MutableList<MigrationMapping>
    ) {
        val cleanName = componentName.lowercase().replace(Regex("[^a-z0-9]"), "")

        // Check entitymodel.xml -> entity/Entities.java
        val entityModelXml = File(entityDefDir, "entitymodel.xml")
        if (entityModelXml.exists()) {
            val javaFile = File(srcDir, "com/ilscipio/scipio/$cleanName/entity/Entities.java")
            mappings.add(MigrationMapping(entityModelXml, javaFile, "entity", javaFile.exists()))
        }

        // Check eecas.xml -> eeca/Eecas.java
        val eecasXml = File(entityDefDir, "eecas.xml")
        if (eecasXml.exists()) {
            val javaFile = File(srcDir, "com/ilscipio/scipio/$cleanName/eeca/Eecas.java")
            mappings.add(MigrationMapping(eecasXml, javaFile, "eeca", javaFile.exists()))
        }
    }

    /**
     * Scans servicedef directory for service and SECA XML files.
     */
    private fun scanServiceDef(
        serviceDefDir: File,
        srcDir: File,
        componentName: String,
        mappings: MutableList<MigrationMapping>
    ) {
        val cleanName = componentName.lowercase().replace(Regex("[^a-z0-9]"), "")

        // Check for services*.xml files -> service/Services.java
        // All service XML files map to a single Services.java
        val servicesJava = File(srcDir, "com/ilscipio/scipio/$cleanName/service/Services.java")
        serviceDefDir.listFiles { file ->
            file.name.startsWith("services") && file.name.endsWith(".xml")
        }?.forEach { xmlFile ->
            mappings.add(MigrationMapping(xmlFile, servicesJava, "service", servicesJava.exists()))
        }

        // Check mca.xml -> service/Services.java (mail conditions also go to Services)
        val mcaXml = File(serviceDefDir, "mca.xml")
        if (mcaXml.exists()) {
            mappings.add(MigrationMapping(mcaXml, servicesJava, "service", servicesJava.exists()))
        }

        // Check secas.xml -> seca/Secas.java
        val secasXml = File(serviceDefDir, "secas.xml")
        if (secasXml.exists()) {
            val javaFile = File(srcDir, "com/ilscipio/scipio/$cleanName/seca/Secas.java")
            mappings.add(MigrationMapping(secasXml, javaFile, "seca", javaFile.exists()))
        }
    }

    /**
     * Scans widget directory for screen, form, menu, and tree XML files.
     * Note: The converter flattens subdirectories - all Java files go to widget/ directly.
     */
    private fun scanWidgetDir(
        widgetDir: File,
        srcDir: File,
        componentName: String,
        componentDir: File,
        mappings: MutableList<MigrationMapping>
    ) {
        val cleanName = componentName.lowercase().replace(Regex("[^a-z0-9]"), "")

        widgetDir.walkTopDown()
            .filter { it.isFile && it.extension == "xml" && isWidgetXml(it) }
            .forEach { xmlFile ->
                // Compute Java class name
                var baseName = xmlFile.nameWithoutExtension
                baseName = baseName.replace(Regex("[^a-zA-Z0-9]"), "_")
                if (baseName.first().isDigit()) {
                    baseName = "_$baseName"
                }

                // The converter flattens all widget subdirectories into the main widget package
                // e.g., widget/content/ContentForms.xml -> widget/ContentForms.java
                val packagePath = "com/ilscipio/scipio/$cleanName/widget"
                val javaFile = File(srcDir, "$packagePath/$baseName.java")

                mappings.add(MigrationMapping(xmlFile, javaFile, "widget", javaFile.exists()))
            }
    }

    /**
     * Checks if a file is a widget XML file.
     */
    private fun isWidgetXml(file: File): Boolean {
        return try {
            val content = file.readText(Charsets.UTF_8).take(2000)
            val hasWidgetTag = content.contains("<screens") ||
                    content.contains("<forms") ||
                    content.contains("<menus") ||
                    content.contains("<tree")
            if (!hasWidgetTag) {
                return false
            }
            // Parse to verify it's a valid widget file
            val factory = DocumentBuilderFactory.newInstance()
            factory.isNamespaceAware = true
            factory.isValidating = false
            factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
            factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
            factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
            val doc = factory.newDocumentBuilder().parse(file)
            val rootName = doc.documentElement?.nodeName
            rootName == "screens" || rootName == "forms" || rootName == "menus" || rootName == "tree" || rootName == "trees"
        } catch (e: Exception) {
            false
        }
    }
}
