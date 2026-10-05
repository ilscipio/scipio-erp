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
 * Scipio ERP - Component XML Parser
 *
 * Parses scipio-component.xml files for runtime metadata.
 * Note: This is used for runtime configuration discovery, NOT for build dependencies.
 * Build dependencies should be declared explicitly in build.gradle.kts files.
 */

package com.ilscipio.scipio.gradle

import org.dom4j.io.SAXReader
import java.io.File

/**
 * Parser for scipio-component.xml files.
 */
class ComponentXmlParser {

    /**
     * Parse a scipio-component.xml file and extract component info.
     */
    fun parse(xmlFile: File): ComponentInfo {
        val reader = SAXReader()
        val document = reader.read(xmlFile)
        val root = document.rootElement

        val name = root.attributeValue("name") ?: xmlFile.parentFile.name
        val globalName = root.attributeValue("global-name") ?: name
        val enabled = root.attributeValue("enabled")?.toBoolean() ?: true

        // Parse webapp elements
        val webapps = root.elements("webapp").mapNotNull { element ->
            val webappName = element.attributeValue("name")
            if (webappName != null) {
                WebappInfo(
                    name = webappName,
                    title = element.attributeValue("title") ?: webappName,
                    mount = element.attributeValue("mount-point") ?: "/$webappName",
                    location = element.attributeValue("location") ?: "webapp/$webappName",
                    server = element.attributeValue("server") ?: "default-server"
                )
            } else null
        }

        // Parse classpath entries
        val classpathEntries = root.elements("classpath").map { element ->
            ClasspathEntry(
                type = element.attributeValue("type") ?: "jar",
                location = element.attributeValue("location") ?: ""
            )
        }

        // Parse entity resources
        val entityResources = root.elements("entity-resource").map { element ->
            EntityResource(
                type = element.attributeValue("type") ?: "",
                reader = element.attributeValue("reader-name") ?: "",
                loader = element.attributeValue("loader") ?: "main",
                location = element.attributeValue("location") ?: ""
            )
        }

        // Parse service resources
        val serviceResources = root.elements("service-resource").map { element ->
            ServiceResource(
                type = element.attributeValue("type") ?: "",
                loader = element.attributeValue("loader") ?: "main",
                location = element.attributeValue("location") ?: ""
            )
        }

        return ComponentInfo(
            name = name,
            globalName = globalName,
            enabled = enabled,
            webapps = webapps,
            classpathEntries = classpathEntries,
            entityResources = entityResources,
            serviceResources = serviceResources
        )
    }
}

/**
 * Parsed component information from scipio-component.xml
 */
data class ComponentInfo(
    val name: String,
    val globalName: String,
    val enabled: Boolean,
    val webapps: List<WebappInfo>,
    val classpathEntries: List<ClasspathEntry>,
    val entityResources: List<EntityResource>,
    val serviceResources: List<ServiceResource>
)

/**
 * Classpath entry from scipio-component.xml
 */
data class ClasspathEntry(
    val type: String,
    val location: String
)

/**
 * Entity resource definition
 */
data class EntityResource(
    val type: String,
    val reader: String,
    val loader: String,
    val location: String
)

/**
 * Service resource definition
 */
data class ServiceResource(
    val type: String,
    val loader: String,
    val location: String
)
