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
 * Scipio ERP - Component Extension
 *
 * DSL extension for configuring Scipio components in build.gradle.kts
 */

package com.ilscipio.scipio.gradle

import org.gradle.api.provider.ListProperty
import org.gradle.api.provider.Property

/**
 * Extension for Scipio component configuration.
 *
 * Usage in build.gradle.kts:
 * ```
 * scipioComponent {
 *     componentName.set("base")
 *     globalName.set("base")
 * }
 * ```
 */
abstract class ScipioComponentExtension {
    /**
     * Component name (from scipio-component.xml or explicit)
     */
    abstract val componentName: Property<String>

    /**
     * Global name for the component (used for classpath resolution)
     */
    abstract val globalName: Property<String>

    /**
     * Whether this component exports services
     */
    abstract val exportServices: Property<Boolean>

    /**
     * List of webapp configurations
     */
    abstract val webapps: ListProperty<WebappInfo>

    /**
     * Explicit component dependencies for build ordering
     * Note: This is for build-time dependency resolution, NOT runtime
     */
    abstract val componentDependencies: ListProperty<String>

    /**
     * Whether the component may declare agent tools (@McpServer, @McpServerExtension). When true the plugin
     * adds `api(project(":framework:mcp"))` automatically. Defaults to true outside `framework/`.
     */
    abstract val agentTools: Property<Boolean>
}

/**
 * Webapp configuration info parsed from scipio-component.xml
 */
data class WebappInfo(
    val name: String,
    val title: String,
    val mount: String,
    val location: String,
    val server: String = "default-server"
)
