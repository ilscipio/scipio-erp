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
 * Scipio ERP - Agent Plugin Assembly Task
 *
 * Gradle task that packages every Agent Skill and the MCP connection config into one
 * Claude Code plugin directory, then zips it for distribution.
 *
 * Usage:
 *   ./gradlew assembleAgentPlugin
 */

package com.ilscipio.scipio.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction
import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

/**
 * Gradle task that scans every component for a `skills/<name>/SKILL.md` folder, copies each skill
 * into a Claude Code plugin layout under `build/agent/scipio-claude-plugin/`, writes the plugin
 * manifest and the MCP connection file, writes a README with install steps, and zips the result
 * to `build/agent/scipio-claude-plugin.zip`.
 */
open class AssembleAgentPluginTask : DefaultTask() {

    init {
        group = "scipio"
        description = "Assemble every Agent Skill and the MCP connection config into one Claude Code plugin, then zip it"
    }

    /** Top-level directories that may hold components with a skills/ folder. */
    private val componentGroups = listOf("framework", "applications", "addons", "hot-deploy")

    @TaskAction
    fun assemble() {
        val rootDir = project.rootDir
        val agentDir = File(rootDir, "build/agent")
        val pluginDir = File(agentDir, "scipio-claude-plugin")
        val skillsOutDir = File(pluginDir, "skills")

        if (pluginDir.exists()) {
            pluginDir.deleteRecursively()
        }
        skillsOutDir.mkdirs()

        val copied = mutableListOf<String>()
        for (groupName in componentGroups) {
            val groupDir = File(rootDir, groupName)
            val componentDirs = groupDir.listFiles { f -> f.isDirectory } ?: continue
            for (componentDir in componentDirs) {
                val skillsDir = File(componentDir, "skills")
                val skillDirs = skillsDir.listFiles { f -> f.isDirectory } ?: continue
                for (skillDir in skillDirs.sortedBy { it.name }) {
                    val skillMd = File(skillDir, "SKILL.md")
                    if (!skillMd.isFile) {
                        continue
                    }
                    val dest = File(skillsOutDir, skillDir.name)
                    if (dest.exists()) {
                        logger.warn("assembleAgentPlugin: duplicate skill name '${skillDir.name}', " +
                            "keeping the first one found and skipping $skillDir")
                        continue
                    }
                    skillDir.copyRecursively(dest, overwrite = true)
                    copied.add("$groupName/${componentDir.name}/skills/${skillDir.name}")
                }
            }
        }

        val pluginMetaDir = File(pluginDir, ".claude-plugin").apply { mkdirs() }
        File(pluginMetaDir, "plugin.json").writeText(PLUGIN_JSON)
        File(pluginDir, ".mcp.json").writeText(MCP_JSON)
        File(pluginDir, "README.md").writeText(readmeText())

        val zipFile = File(agentDir, "scipio-claude-plugin.zip")
        if (zipFile.exists()) {
            zipFile.delete()
        }
        zipDirectory(pluginDir, zipFile)

        logger.lifecycle("assembleAgentPlugin: packaged ${copied.size} skill(s):")
        copied.forEach { logger.lifecycle("  - $it") }
        logger.lifecycle("assembleAgentPlugin: plugin directory: $pluginDir")
        logger.lifecycle("assembleAgentPlugin: plugin zip:       $zipFile")
    }

    private fun zipDirectory(sourceDir: File, zipFile: File) {
        ZipOutputStream(zipFile.outputStream().buffered()).use { zos ->
            val basePrefix = "scipio-claude-plugin/"
            sourceDir.walkTopDown().filter { it.isFile }.forEach { file ->
                val relativePath = basePrefix + file.relativeTo(sourceDir).path.replace(File.separatorChar, '/')
                zos.putNextEntry(ZipEntry(relativePath))
                BufferedInputStream(FileInputStream(file)).use { input -> input.copyTo(zos) }
                zos.closeEntry()
            }
        }
    }

    private fun readmeText(): String = """
        # Scipio ERP agent plugin

        This plugin bundles every Scipio ERP Agent Skill and the connection settings for the
        Scipio MCP server. Build it with:

        ```
        ./gradlew assembleAgentPlugin
        ```

        The task writes the plugin to `build/agent/scipio-claude-plugin/` and a zip archive to
        `build/agent/scipio-claude-plugin.zip`.

        ## Install steps

        1. Set two environment variables in the shell that runs Claude Code:

           ```
           export SCIPIO_MCP_URL="https://your-scipio-host"
           export SCIPIO_MCP_TOKEN="<a Scipio MCP access token>"
           ```

           Create a token from the Scipio webtools "Agent Access" screen, or ask a `MCP_ADMIN` user
           to create one for you.

        2. Add the MCP server connection directly, when you only need the tools and not the
           skills:

           ```
           claude mcp add --transport http scipio "${'$'}{SCIPIO_MCP_URL}/admin/mcp" \
               --header "Authorization: Bearer ${'$'}{SCIPIO_MCP_TOKEN}"
           ```

        3. Install the full plugin, when you also want the packaged skills, from the built
           directory or the zip:

           ```
           claude plugin install ./build/agent/scipio-claude-plugin
           ```

        4. Restart Claude Code. Run `scipio_whoami` to confirm the connection and the token
           permissions.

        ## What is in the plugin

        - `.claude-plugin/plugin.json`: the plugin manifest.
        - `.mcp.json`: the Scipio MCP server connection, read from `SCIPIO_MCP_URL` and
          `SCIPIO_MCP_TOKEN` at launch time.
        - `skills/`: one folder per Agent Skill, copied from every component's own
          `skills/<name>/SKILL.md`.
    """.trimIndent() + "\n"

    companion object {
        private val PLUGIN_JSON = """
            {
              "name": "scipio-erp",
              "version": "4.0.0",
              "description": "Scipio ERP agent skills and MCP connection",
              "author": {
                "name": "ilscipio"
              }
            }
        """.trimIndent() + "\n"

        private val MCP_JSON = """
            {
              "mcpServers": {
                "scipio": {
                  "type": "http",
                  "url": "${'$'}{SCIPIO_MCP_URL}/admin/mcp",
                  "headers": {
                    "Authorization": "Bearer ${'$'}{SCIPIO_MCP_TOKEN}"
                  }
                }
              }
            }
        """.trimIndent() + "\n"
    }
}
