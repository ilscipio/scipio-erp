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
 * Scipio ERP - XML to Annotation Converter Task
 *
 * Gradle task that converts XML widget definitions to Java annotation classes.
 *
 * Usage:
 *   ./gradlew convertXmlToAnnotation -PxmlFile=applications/setup/widget/SetupForms.xml
 *   ./gradlew convertXmlToAnnotation -Pcomponent=setup
 *   ./gradlew convertXmlToAnnotation -PxmlFile=... -PoutputPackage=com.ilscipio.scipio.setup.widget
 *   ./gradlew convertXmlToAnnotation -Pcomponent=setup -PremoveXml=true  // Also remove original XML files
 */

package com.ilscipio.scipio.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.tasks.Input
import org.gradle.api.tasks.Optional
import org.gradle.api.tasks.TaskAction
import org.gradle.api.tasks.options.Option
import java.io.File
import java.net.URLClassLoader
import javax.xml.parsers.DocumentBuilderFactory

/**
 * Gradle task for converting XML widget definitions to Java annotation classes.
 *
 * This task uses the Java converters in framework/widget to perform the actual conversion.
 */
open class ConvertXmlToAnnotationTask : DefaultTask() {

    init {
        group = "scipio"
        description = "Convert XML widget definitions to Java annotation classes"
    }

    @get:Input
    @get:Optional
    @Option(option = "xmlFile", description = "Path to XML file to convert")
    var xmlFile: String? = null

    @get:Input
    @get:Optional
    @Option(option = "component", description = "Component name to convert all XML files from")
    var component: String? = null

    @get:Input
    @get:Optional
    @Option(option = "outputPackage", description = "Override output package name")
    var outputPackage: String? = null

    @get:Input
    @get:Optional
    @Option(option = "removeXml", description = "Remove original XML files after successful conversion (true/false)")
    var removeXml: String? = null

    @TaskAction
    fun convert() {
        val xmlFiles = collectXmlFiles()

        if (xmlFiles.isEmpty()) {
            throw GradleException("No XML files found to convert. Specify -PxmlFile=<path> or -Pcomponent=<name>")
        }

        // Check removeXml setting from property or gradle property
        val shouldRemoveXml = (removeXml ?: project.findProperty("removeXml")?.toString()) == "true"
        if (shouldRemoveXml) {
            logger.lifecycle("XML files will be REMOVED after successful conversion")
        }

        logger.lifecycle("Converting ${xmlFiles.size} XML file(s)...")

        val successfullyConverted = mutableListOf<File>()

        xmlFiles.forEach { xmlFile ->
            try {
                convertFile(xmlFile)
                successfullyConverted.add(xmlFile)
            } catch (e: Exception) {
                logger.error("Failed to convert ${xmlFile.name}: ${e.message}")
                throw GradleException("Conversion failed for ${xmlFile.path}", e)
            }
        }

        // Remove original XML files if requested (only after ALL conversions succeed)
        if (shouldRemoveXml && successfullyConverted.isNotEmpty()) {
            logger.lifecycle("")
            logger.lifecycle("Removing ${successfullyConverted.size} original XML file(s)...")
            successfullyConverted.forEach { xmlFile ->
                if (xmlFile.delete()) {
                    logger.lifecycle("  Deleted: ${xmlFile.path}")
                } else {
                    logger.warn("  Failed to delete: ${xmlFile.path}")
                }
            }
        }

        logger.lifecycle("Conversion complete!")
    }

    /**
     * Collects XML files to convert based on task parameters.
     */
    private fun collectXmlFiles(): List<File> {
        return when {
            !xmlFile.isNullOrEmpty() -> {
                val file = project.file(xmlFile!!)
                if (!file.exists()) {
                    throw GradleException("XML file not found: ${file.absolutePath}")
                }
                listOf(file)
            }
            !component.isNullOrEmpty() -> {
                findComponentXmlFiles(component!!)
            }
            else -> {
                // Check for Gradle properties
                val propXmlFile = project.findProperty("xmlFile")?.toString()
                val propComponent = project.findProperty("component")?.toString()

                when {
                    !propXmlFile.isNullOrEmpty() -> {
                        val file = project.file(propXmlFile)
                        if (!file.exists()) {
                            throw GradleException("XML file not found: ${file.absolutePath}")
                        }
                        listOf(file)
                    }
                    !propComponent.isNullOrEmpty() -> {
                        findComponentXmlFiles(propComponent)
                    }
                    else -> emptyList()
                }
            }
        }
    }

    /**
     * Finds all convertible XML files in a component.
     */
    private fun findComponentXmlFiles(componentName: String): List<File> {
        val files = mutableListOf<File>()

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

        // Find widget XML files (including subdirectories like ordermgr/, partymgr/, catalog/, etc.)
        val widgetDir = File(componentDir, "widget")
        if (widgetDir.exists()) {
            widgetDir.walkTopDown()
                .filter { it.isFile && it.extension == "xml" && isWidgetXml(it) }
                .forEach { files.add(it) }
        }

        // Find ALL controller.xml files in webapp subdirectories
        val webappDir = File(componentDir, "webapp")
        if (webappDir.exists()) {
            webappDir.listFiles { file -> file.isDirectory }?.forEach { webappSubDir ->
                val controllerFile = File(webappSubDir, "WEB-INF/controller.xml")
                if (controllerFile.exists()) {
                    files.add(controllerFile)
                }
            }
        }

        // Find services.xml, services_*.xml, secas.xml, secas_*.xml,
        // groups.xml, groups_*.xml, and service_groups.xml in servicedef
        val serviceDefDir = File(componentDir, "servicedef")
        if (serviceDefDir.exists()) {
            serviceDefDir.listFiles { file ->
                file.name == "services.xml" ||
                (file.name.startsWith("services_") && file.name.endsWith(".xml")) ||
                file.name == "secas.xml" ||
                (file.name.startsWith("secas_") && file.name.endsWith(".xml")) ||
                file.name == "mcas.xml" ||
                (file.name.startsWith("mcas_") && file.name.endsWith(".xml")) ||
                (file.name.startsWith("smcas") && file.name.endsWith(".xml")) ||
                file.name == "groups.xml" ||
                (file.name.startsWith("groups_") && file.name.endsWith(".xml")) ||
                (file.name.startsWith("service_groups") && file.name.endsWith(".xml"))
            }?.forEach { files.add(it) }
        }

        // Find entitymodel.xml, entitymodel_*.xml, eecas.xml and eecas_*.xml in entitydef
        val entityDefDir = File(componentDir, "entitydef")
        if (entityDefDir.exists()) {
            entityDefDir.listFiles { file ->
                (file.name == "entitymodel.xml" || (file.name.startsWith("entitymodel_") && file.name.endsWith(".xml"))) ||
                (file.name == "eecas.xml" || (file.name.startsWith("eecas_") && file.name.endsWith(".xml")))
            }?.forEach { files.add(it) }
        }

        val scriptDir = File(componentDir, "script")
        if (scriptDir.exists()) {
            scriptDir.walkTopDown()
                .filter { it.extension == "xml" && isSimpleMethodsXml(it) && !isTestFile(it) }
                .forEach { files.add(it) }
        }

        return files
    }

    /**
     * Checks if a file is a widget XML file.
     * SCIPIO: 4.0.0: Increased from 500 to 2000 chars to handle Apache license headers
     * SCIPIO: 4.0.0: Now uses XML parsing to skip commented-out or empty files
     */
    private fun isWidgetXml(file: File): Boolean {
        return try {
            // Quick text check first for performance
            val content = file.readText(Charsets.UTF_8).take(20000) // SCIPIO: 4.0.0: license headers can exceed 2000 chars (CommonMenus.xml)
            val hasWidgetTag = content.contains("<screens") ||
                    content.contains("<forms") ||
                    content.contains("<menus") ||
                    content.contains("<tree")
            if (!hasWidgetTag) {
                return false
            }
            // Try to parse the file to ensure it has a valid root element
            // This filters out files where widget tags only appear in comments
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
            // Parsing failed - file is malformed or empty, skip it
            false
        }
    }

    /**
     * Checks if a file is a simple-methods XML file.
     */
    private fun isSimpleMethodsXml(file: File): Boolean {
        return try {
            val content = file.readText(Charsets.UTF_8).take(20000) // SCIPIO: 4.0.0: license headers can exceed 2000 chars (CommonMenus.xml)
            content.contains("<simple-methods")
        } catch (e: Exception) {
            false
        }
    }

    /**
     * Checks if a file is a test file that should be skipped.
     * Test files use inline method patterns that don't convert well to Java.
     */
    private fun isTestFile(file: File): Boolean {
        val name = file.nameWithoutExtension
        // Skip all test files (Auto*Test*, *Tests, *Test)
        if (name.startsWith("Auto") && name.contains("Test")) return true
        if (name.endsWith("Tests") || name.endsWith("Test")) return true
        // Also skip files in test/ directories
        return file.absolutePath.replace('\\', '/').contains("/test/")
    }

    /**
     * Converts a single XML file to annotation class.
     */
    private fun convertFile(xmlFile: File) {
        logger.lifecycle("Converting: ${xmlFile.name}")

        // Parse the XML file
        val doc = parseXml(xmlFile)
        val xmlType = detectXmlType(doc)

        // Determine component name from file path
        val componentName = determineComponentName(xmlFile)

        // Determine output directories
        val componentDir = findComponentDir(xmlFile)
        val srcDir = File(componentDir, "src")

        // SCIPIO: 4.0.0: For controller conversions, derive webapp name from file path
        // (e.g., webapp/partymgr/WEB-INF/controller.xml -> "partymgr")
        val webappName = if (xmlType == "controller") {
            deriveWebappName(xmlFile) ?: componentName
        } else {
            componentName
        }

        // Determine package name
        val pkgName = outputPackage
            ?: project.findProperty("outputPackage")?.toString()
            ?: derivePackageName(componentName, xmlType)

        // Determine class name (include subdirectory prefix for widgets to avoid conflicts)
        // For controllers with webapp name different from component name, prefix with webapp name
        val className = if (xmlType == "controller" && webappName != componentName) {
            webappName.replaceFirstChar { it.uppercase() } + "ControllerDef"
        } else {
            deriveClassName(xmlFile, xmlType, componentDir)
        }
        val scriptOutputDir = File(componentDir, "webapp/$componentName/WEB-INF/actions/generated")

        // SCIPIO: 4.0.0: Compute source location URL for location alias
        val sourceLocation = computeSourceLocation(xmlFile, componentName, componentDir)

        logger.lifecycle("  Type: $xmlType")
        logger.lifecycle("  Package: $pkgName")
        logger.lifecycle("  Class: $className")
        logger.lifecycle("  WebApp: $webappName")
        logger.lifecycle("  Location: $sourceLocation")
        logger.lifecycle("  Output: $srcDir")

        // Use reflection to call the Java converter
        val result = invokeConverter(doc, xmlType, pkgName, className, componentName, srcDir, scriptOutputDir, sourceLocation, webappName)
        val source = result.first
        val converter = result.second

        // Write the generated source, splitting if too large for Java constant pool
        val packageDir = File(srcDir, pkgName.replace('.', File.separatorChar))
        packageDir.mkdirs()
        if (xmlType == "controller") {
            val files = splitControllerSource(source, className, pkgName)
            files.forEach { (splitClassName, splitSource) ->
                val outputFile = File(packageDir, "$splitClassName.java")
                if (source.isBlank()) {
                    logger.lifecycle("  Skipped (no output produced): ${outputFile.path}") // SCIPIO: 4.0.0: never write an empty ControllerDef
                } else {
                    outputFile.writeText(splitSource)
                }
                logger.lifecycle("  Generated: ${outputFile.path}")
            }
            // Delete any stale Part files from the old multi-file split approach
            val deletedParts = packageDir.listFiles { f ->
                f.name.matches(Regex("${Regex.escape(className)}Part\\d+\\.java"))
            }?.onEach { it.delete() } ?: emptyArray()
            if (deletedParts.isNotEmpty()) {
                logger.lifecycle("  Deleted ${deletedParts.size} stale Part file(s) from ${packageDir.path}")
            }
        } else {
            val outputFile = File(packageDir, "$className.java")
            // SCIPIO: 4.0.0: Skip writing if the generated source has no actual definitions
            // (e.g., XML stub with only screen-settings) and the output file already has content.
            // This prevents overwriting manually-written annotation classes with empty stubs.
            // SCIPIO: 4.0.0: the class wrapper is always emitted; only count real definition annotations
            val hasDefinitions = Regex("@(Screen|Form|Menu|Tree|Entity|ViewEntity|ExtendEntity|Service|Seca|Eeca|Mca|Request|View|EmailTemplate)[(]").containsMatchIn(source)
            if (source.isBlank()) {
                // Converter declined this file (e.g. unimplemented service-group conversion)
                logger.lifecycle("  Skipped (no output produced): ${outputFile.path}")
            } else if (!hasDefinitions && outputFile.exists() && outputFile.length() > source.length) {
                logger.lifecycle("  Skipped (no definitions in source XML, preserving existing): ${outputFile.path}")
            } else {
                outputFile.writeText(source)
                logger.lifecycle("  Generated: ${outputFile.path}")
            }
        }

        // SCIPIO: 4.0.0: Write extracted scripts (from inline script blocks in XML)
        if (converter != null) {
            try {
                val converterClass = converter.javaClass
                val writeScriptsMethod = converterClass.getMethod("writeExtractedScripts")
                writeScriptsMethod.invoke(converter)
                logger.lifecycle("  Extracted scripts written to: $scriptOutputDir")
            } catch (e: Exception) {
                // Method may not exist for non-widget converters
                logger.debug("No extracted scripts to write: ${e.message}")
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Splits a generated controller source to avoid the Java constant pool limit.
     * Each annotation member (starting with indented @View or @Request) is treated as one item.
     * Members are split at MAX_MEMBERS_PER_FILE boundaries.
     *
     * Instead of producing multiple separate files, overflow members are placed into
     * inner static classes (Part2, Part3, …) inside the single outer class. Each inner
     * class compiles to its own .class file with its own constant pool, giving the same
     * protection without file proliferation.
     *
     * Returns exactly ONE (className, source) pair.
     */
    private fun splitControllerSource(source: String, className: String, packageName: String): List<Pair<String, String>> {
        val maxMembers = 20 // Conservative limit to stay well under Java constant pool ceiling

        // Find the class body boundaries
        val classOpenPattern = Regex("""public class ${Regex.escape(className)} \{""")
        val classOpenMatch = classOpenPattern.find(source)
            ?: return listOf(className to source) // Can't parse, return as-is

        val headerEnd = classOpenMatch.range.last + 1
        val header = source.substring(0, headerEnd)

        // Find the last closing brace (class footer)
        val bodyAndFooter = source.substring(headerEnd)
        val lastBrace = bodyAndFooter.lastIndexOf('}')
        if (lastBrace < 0) return listOf(className to source)

        val body = bodyAndFooter.substring(0, lastBrace)
        val footer = bodyAndFooter.substring(lastBrace) // just "}"

        // Split body into individual members: each member starts with @View or @Request
        // (not @Response or @Event which are part of the same member)
        // Use \r?\n to handle both Unix and Windows line endings
        val memberPattern = Regex("""\r?\n(?=    @(?:com\.ilscipio\.scipio\.ce\.webapp\.control\.def\.)?(?:View|Request)\()""")
        val members = body.split(memberPattern).filter { it.isNotBlank() }

        if (members.size <= maxMembers) {
            return listOf(className to source) // Small enough, no split needed
        }

        val numParts = (members.size + maxMembers - 1) / maxMembers
        logger.lifecycle("  Splitting $className: ${members.size} members into 1 file with ${numParts - 1} inner class(es)")

        // Detect line ending used in source
        val lineEnding = if (source.contains("\r\n")) "\r\n" else "\n"

        // Split into chunks; first chunk stays in the outer class, rest become inner classes
        val chunks = members.chunked(maxMembers)

        val resultSource = buildString {
            // Outer class header (unchanged)
            append(header)

            // First chunk — members at standard 4-space indent (already formatted that way)
            val firstChunk = chunks[0]
            firstChunk.forEach { member ->
                append(lineEnding)
                append(member)
            }

            // Subsequent chunks — each becomes a public static inner class
            for (index in 1 until chunks.size) {
                val chunk = chunks[index]
                val partNum = index + 1 // Part2, Part3, …
                append(lineEnding)
                append(lineEnding)
                append("    // Auto-generated split (Part $partNum)")
                append(lineEnding)
                append("    public static class Part$partNum {")
                chunk.forEach { member ->
                    // Re-indent from 4-space (outer) to 8-space (inner)
                    val reindented = member.lines().joinToString(lineEnding) { line ->
                        if (line.isNotEmpty()) "    $line" else line
                    }
                    append(lineEnding)
                    append(reindented)
                }
                append(lineEnding)
                append("    }") // close inner class
            }

            append(lineEnding)
            append(footer) // outer class closing "}"
        }

        return listOf(className to resultSource)
    }

    /**
     * SCIPIO: 4.0.0: Derives the webapp name from a controller.xml file path.
     * Expects path like: .../webapp/<webappName>/WEB-INF/controller.xml
     */
    private fun deriveWebappName(xmlFile: File): String? {
        val webInfDir = xmlFile.parentFile ?: return null
        if (webInfDir.name != "WEB-INF") return null
        val webappDir = webInfDir.parentFile ?: return null
        return webappDir.name
    }

    /**
     * SCIPIO: 4.0.0: Computes the component:// source location URL for an XML file.
     */
    private fun computeSourceLocation(xmlFile: File, componentName: String, componentDir: File): String {
        // Get relative path from component directory
        val relativePath = xmlFile.relativeTo(componentDir).path.replace('\\', '/')
        return "component://$componentName/$relativePath"
    }

    /**
     * Finds the component directory for a file.
     */
    private fun findComponentDir(file: File): File {
        var parent = file.parentFile
        while (parent != null) {
            val componentXml = File(parent, "scipio-component.xml")
            val ofbizComponentXml = File(parent, "ofbiz-component.xml")
            if (componentXml.exists() || ofbizComponentXml.exists()) {
                return parent
            }
            parent = parent.parentFile
        }
        // Fallback
        return file.parentFile.parentFile
    }

    /**
     * Parses an XML file into a Document.
     */
    private fun parseXml(file: File): org.w3c.dom.Document {
        val factory = DocumentBuilderFactory.newInstance()
        factory.isNamespaceAware = true
        factory.isValidating = false
        // Disable external entity loading for security
        factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
        factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
        factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)

        val builder = factory.newDocumentBuilder()
        return builder.parse(file)
    }

    /**
     * Detects the XML type from root element.
     */
    private fun detectXmlType(doc: org.w3c.dom.Document): String {
        val rootName = doc.documentElement.nodeName
        // Map root element names to type names
        return when (rootName) {
            "site-conf" -> "controller"
            "services" -> "services"
            "service-group" -> "service-group"
            "service-eca" -> "secas"
            "service-mca" -> "mcas"
            "entity-eca" -> "eecas"
            "entitymodel" -> "entitymodel"
            "simple-methods" -> "simple-methods"
            else -> rootName
        }
    }

    /**
     * Determines component name from file path.
     */
    private fun determineComponentName(file: File): String {
        // Navigate up to find component directory
        var parent = file.parentFile
        while (parent != null) {
            val componentXml = File(parent, "scipio-component.xml")
            val ofbizComponentXml = File(parent, "ofbiz-component.xml")
            if (componentXml.exists() || ofbizComponentXml.exists()) {
                return parent.name
            }
            parent = parent.parentFile
        }
        // Fallback: use parent of widget directory
        return file.parentFile.parentFile.name
    }

    /**
     * Derives package name from component name and xml type.
     */
    private fun derivePackageName(componentName: String, xmlType: String): String {
        val cleanName = componentName.lowercase().replace(Regex("[^a-z0-9]"), "")
        val suffix = when (xmlType) {
            "controller" -> "controller"
            "services" -> "service"
            "service-group" -> "service"
            "secas" -> "seca"
            "mcas" -> "mca"
            "eecas" -> "eeca"
            "entitymodel" -> "entity"
            "simple-methods" -> "event"
            else -> "widget"
        }
        return "com.ilscipio.scipio.$cleanName.$suffix"
    }

    /**
     * Derives class name from XML file, type, and component directory.
     * For widget files in subdirectories, prefixes with capitalized subdirectory name
     * to avoid class name conflicts (e.g., website/CommonScreens.xml -> WebsiteCommonScreens).
     */
    private fun deriveClassName(xmlFile: File, xmlType: String, componentDir: File): String {
        var baseName = xmlFile.name
        if (baseName.lowercase().endsWith(".xml")) {
            baseName = baseName.dropLast(4)
        }
        // For special file types, use proper class name
        baseName = when (xmlType) {
            "controller" -> "ControllerDef"
            "services" -> {
                // For supplementary service files (services_invoice.xml, services_products.xml etc.),
                // generate a descriptive class name instead of the generic "Services"
                val fileName = xmlFile.nameWithoutExtension  // e.g. "services_invoice"
                if (fileName == "services") {
                    "Services"
                } else {
                    // services_invoice -> InvoiceServices, services_products -> ProductsServices
                    val suffix = fileName.removePrefix("services_")
                        .replaceFirstChar { it.uppercase() }
                    "${suffix}Services"
                }
            }
            "mcas" -> {
                // SCIPIO: 4.0.0: mcas.xml -> Mcas, smcas_test.xml -> TestMcas
                val fileName = xmlFile.nameWithoutExtension
                val stem = fileName.removePrefix("s").removePrefix("mcas").removePrefix("_")
                if (stem.isEmpty()) "Mcas" else stem.replaceFirstChar { it.uppercase() } + "Mcas"
            }
            "secas" -> {
                // For supplementary seca files (secas_payment.xml, secas_ledger.xml etc.),
                // generate a descriptive class name instead of the generic "Secas"
                val fileName = xmlFile.nameWithoutExtension  // e.g. "secas_payment"
                if (fileName == "secas") {
                    "Secas"
                } else {
                    // secas_payment -> PaymentSecas, secas_ledger -> LedgerSecas
                    val suffix = fileName.removePrefix("secas_")
                        .replaceFirstChar { it.uppercase() }
                    "${suffix}Secas"
                }
            }
            "eecas" -> {
                val fileName = xmlFile.nameWithoutExtension
                if (fileName == "eecas") {
                    "Eecas"
                } else {
                    val suffix = fileName.removePrefix("eecas_")
                        .replaceFirstChar { it.uppercase() }
                    "${suffix}Eecas"
                }
            }
            "service-group" -> {
                // For service group files: groups.xml -> Groups,
                // groups_test.xml -> TestGroups, service_groups.xml -> ServiceGroups
                val fileName = xmlFile.nameWithoutExtension  // e.g. "groups", "groups_test", "service_groups"
                if (fileName == "groups") {
                    "Groups"
                } else if (fileName.startsWith("groups_")) {
                    val suffix = fileName.removePrefix("groups_")
                        .replaceFirstChar { it.uppercase() }
                    "${suffix}Groups"
                } else {
                    // service_groups -> ServiceGroups
                    fileName.split("_").joinToString("") { it.replaceFirstChar { c -> c.uppercase() } }
                }
            }
            "entitymodel" -> {
                // For supplementary entity files (entitymodel_view.xml, entitymodel_shipment.xml etc.),
                // generate a descriptive class name instead of the generic "Entities"
                val fileName = xmlFile.nameWithoutExtension  // e.g. "entitymodel_view"
                if (fileName == "entitymodel") {
                    "Entities"
                } else {
                    // entitymodel_view -> ViewEntities, entitymodel_shipment -> ShipmentEntities
                    val suffix = fileName.removePrefix("entitymodel_")
                        .replaceFirstChar { it.uppercase() }
                    "${suffix}Entities"
                }
            }
            else -> {
                // For widget files, check if in a subdirectory and prefix with subdir name
                val widgetDir = File(componentDir, "widget")
                if (widgetDir.exists() && xmlFile.absolutePath.startsWith(widgetDir.absolutePath)) {
                    val relativePath = xmlFile.relativeTo(widgetDir).parent
                    if (relativePath != null && relativePath.isNotEmpty()) {
                        // Get immediate parent directory name and capitalize it
                        val subDirName = relativePath.split(File.separator).last()
                            .replaceFirstChar { it.uppercase() }
                        baseName = "$subDirName$baseName"
                    }
                }
                // Replace non-identifier characters
                baseName = baseName.replace(Regex("[^a-zA-Z0-9]"), "_")
                if (baseName.first().isDigit()) {
                    baseName = "_$baseName"
                }
                baseName
            }
        }
        return baseName
    }

    /**
     * Builds the classpath needed for the converter classes.
     * This includes the widget project's compiled classes and all its dependencies.
     */
    private fun buildConverterClasspath(): List<File> {
        val classpathFiles = mutableListOf<File>()

        // Widget project's compiled classes
        classpathFiles.add(project.file("framework/widget/build/classes"))

        // Base project's compiled classes (for UtilXml etc.)
        classpathFiles.add(project.file("framework/base/build/classes"))

        // Add framework/base/lib jars (contains dependencies like Groovy)
        val baseLibDir = project.file("framework/base/lib")
        if (baseLibDir.exists()) {
            baseLibDir.walkTopDown()
                .filter { it.extension == "jar" }
                .forEach { classpathFiles.add(it) }
        }

        // Add other framework libraries that may be needed
        listOf("entity", "service", "webapp", "minilang").forEach { component ->
            val classesDir = project.file("framework/$component/build/classes")
            if (classesDir.exists()) {
                classpathFiles.add(classesDir)
            }
        }

        return classpathFiles.filter { it.exists() }
    }

    /**
     * Creates a custom ClassLoader with the converter classes on the classpath.
     */
    private fun createConverterClassLoader(): ClassLoader {
        val classpathFiles = buildConverterClasspath()
        val urls = classpathFiles.map { it.toURI().toURL() }.toTypedArray()

        logger.info("Converter classpath:")
        classpathFiles.forEach { logger.info("  - ${it.path}") }

        return URLClassLoader(urls, Thread.currentThread().contextClassLoader)
    }

    /**
     * Invokes the Java converter using reflection with a custom ClassLoader.
     * Returns a Pair of (source code string, converter object for script extraction).
     */
    private fun invokeConverter(
        doc: org.w3c.dom.Document,
        xmlType: String,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File,
        scriptOutputDir: File,
        sourceLocation: String,
        webappName: String = componentName
    ): Pair<String, Any?> {
        // Create custom ClassLoader with project classes
        val classLoader = createConverterClassLoader()

        return when (xmlType) {
            "controller" -> {
                // SCIPIO: 4.0.0: a stripped controller.xml (includes/handlers/events only) must not overwrite the
                // generated ControllerDef with an empty class
                val hasMaps = doc.getElementsByTagName("request-map").length > 0 || doc.getElementsByTagName("view-map").length > 0
                if (!hasMaps) {
                    logger.lifecycle("  Skipped (controller has no request/view maps): $className")
                    Pair("", null)
                } else {
                    invokeControllerConverter(classLoader, doc, packageName, className, componentName, outputDir, webappName)
                }
            }
            "services" -> {
                invokeServiceConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "service-group" -> {
                // SCIPIO: 4.0.0: groups.xml / service_groups.xml -> @Service(engine = "group", invokes = {...})
                invokeServiceGroupConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "secas" -> {
                invokeSecaConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "mcas" -> {
                invokeMcaConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "eecas" -> {
                invokeEecaConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "entitymodel" -> {
                invokeEntityConverter(classLoader, doc, packageName, className, componentName, outputDir)
            }
            "simple-methods" -> {
                invokeSimpleMethodConverter(classLoader, doc, packageName, className, componentName, outputDir, sourceLocation)
            }
            else -> {
                invokeWidgetConverter(classLoader, doc, xmlType, packageName, className, componentName, outputDir, scriptOutputDir, sourceLocation)
            }
        }
    }

    /**
     * Invokes the widget converter using reflection.
     * Returns Pair of (source code, converter object).
     */
    private fun invokeWidgetConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        xmlType: String,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File,
        scriptOutputDir: File,
        sourceLocation: String
    ): Pair<String, Any?> {
        // Load the factory class using our custom ClassLoader
        val factoryClass = classLoader.loadClass("com.ilscipio.scipio.widget.converter.WidgetXmlConverterFactory")

        // Get createConverter method
        val enumClass = classLoader.loadClass("com.ilscipio.scipio.widget.converter.WidgetXmlConverterFactory\$WidgetType")
        val parseMethod = factoryClass.getMethod("parseWidgetType", String::class.java)
        val widgetType = parseMethod.invoke(null, xmlType)

        val createMethod = factoryClass.getMethod(
            "createConverter",
            enumClass,
            String::class.java,
            String::class.java,
            String::class.java,
            File::class.java,
            File::class.java
        )

        val converter = createMethod.invoke(null, widgetType, packageName, className, componentName, outputDir, scriptOutputDir)

        // SCIPIO: 4.0.0: Set source location for location alias support
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.widget.converter.XmlToAnnotationConverter")
        val setSourceLocationMethod = converterClass.getMethod("setSourceLocation", String::class.java)
        setSourceLocationMethod.invoke(converter, sourceLocation)

        // Call convert method
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        val source = convertMethod.invoke(converter, doc) as String
        return Pair(source, converter)
    }

    /**
     * Invokes the controller converter using reflection.
     */
    private fun invokeControllerConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File,
        webappName: String = componentName
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.ce.webapp.control.converter.ControllerXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            String::class.java,  // controllerName (webapp name)
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, webappName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the service converter using reflection.
     */
    private fun invokeServiceConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.service.converter.ServiceXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the service group converter using reflection.
     * Service group files (<service-group>) define group bodies that are converted
     * to @Service(engine="group") annotations.
     */
    private fun invokeServiceGroupConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.service.converter.ServiceXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convertServiceGroup", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the simple method converter using reflection.
     */
    private fun invokeSimpleMethodConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File,
        sourceLocation: String
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.minilang.converter.SimpleMethodXmlToJavaConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)

        // Set source location
        val setSourceLocationMethod = converterClass.getMethod("setSourceLocation", String::class.java)
        setSourceLocationMethod.invoke(converter, sourceLocation)

        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the SECA converter using reflection.
     */
    private fun invokeSecaConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.service.converter.SecaXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the MCA converter using reflection.
     *
     * SCIPIO: 4.0.0: Added; service-mca.xml had no converter, so those rules stayed in XML.
     */
    private fun invokeMcaConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.service.converter.McaXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the EECA converter using reflection.
     */
    private fun invokeEecaConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.service.converter.EecaXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }

    /**
     * Invokes the Entity converter using reflection.
     */
    private fun invokeEntityConverter(
        classLoader: ClassLoader,
        doc: org.w3c.dom.Document,
        packageName: String,
        className: String,
        componentName: String,
        outputDir: File
    ): Pair<String, Any?> {
        val converterClass = classLoader.loadClass("com.ilscipio.scipio.entity.converter.EntityXmlToAnnotationConverter")
        val constructor = converterClass.getConstructor(
            String::class.java,  // packageName
            String::class.java,  // className
            String::class.java,  // componentName
            File::class.java     // outputDir
        )

        val converter = constructor.newInstance(packageName, className, componentName, outputDir)
        val convertMethod = converterClass.getMethod("convert", org.w3c.dom.Document::class.java)

        return Pair(convertMethod.invoke(converter, doc) as String, null)
    }
}
