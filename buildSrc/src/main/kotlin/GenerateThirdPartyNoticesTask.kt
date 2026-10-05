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
package com.ilscipio.scipio.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.file.ConfigurableFileCollection
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import java.io.File
import java.security.MessageDigest
import java.util.zip.ZipFile

/**
 * Third-party notice file (work package L-01, Apache-2.0 section 4). It writes THIRD-PARTY-NOTICES.md:
 * 1. the table of all third-party entries, from the report of checkLicensePolicy (the jk1 data and the policy),
 * 2. the NOTICE texts and 3. the license texts that the jars and the tracked web libraries carry, each text once.
 * Run: gradlew generateThirdPartyNotices (it runs checkLicensePolicy and the library sync first).
 */
abstract class GenerateThirdPartyNoticesTask : DefaultTask() {
    /** The report of checkLicensePolicy (build/reports/licenses/license-report.md) */
    @get:Internal abstract val entriesReport: RegularFileProperty
    /** The folders with the runtime jars (syncLibs, syncSolrWebappLibs) */
    @get:Internal abstract val jarDirs: ConfigurableFileCollection
    @get:Internal abstract val repositoryRoot: DirectoryProperty
    @get:Internal abstract val outputFile: RegularFileProperty

    init {
        outputs.upToDateWhen { false }
    }

    private val nameRegex = Regex("(?i)^(notice|license|licence|copying)([-_.][^/]*)?$")
    private val maxBytes = 300_000

    private class Text(val kind: String, val text: String, val sources: MutableSet<String> = sortedSetOf())

    private fun sha(text: String): String =
        MessageDigest.getInstance("SHA-256").digest(text.trim().replace("\r\n", "\n").toByteArray()).joinToString("") { "%02x".format(it) }

    private fun isTextEntry(name: String): Boolean {
        if (name.endsWith("/") || name.endsWith(".class") || name.endsWith(".java")) return false
        val base = name.substringAfterLast('/')
        if (!nameRegex.matches(base)) return false
        return !name.contains('/') || name.startsWith("META-INF/")
    }

    private fun tracked(vararg pathspec: String): List<String> {
        val p = ProcessBuilder(listOf("git", "-C", repositoryRoot.get().asFile.path, "ls-files", "-z", "--") + pathspec).start()
        val out = p.inputStream.bufferedReader(Charsets.UTF_8).readText().split('\u0000').filter { it.isNotEmpty() }
        p.waitFor()
        return out
    }

    private fun scanJar(jar: File, label: String, into: MutableMap<String, Text>) {
        try {
            ZipFile(jar).use { zip ->
                for (e in zip.entries()) {
                    if (e.isDirectory || e.size <= 0 || e.size > maxBytes || !isTextEntry(e.name)) continue
                    val text = zip.getInputStream(e).use { String(it.readBytes(), Charsets.UTF_8) }
                    val kind = if (e.name.substringAfterLast('/').lowercase().startsWith("notice")) "NOTICE" else "LICENSE"
                    into.getOrPut(kind + sha(text)) { Text(kind, text.trim()) }.sources.add(label)
                }
            }
        } catch (ex: Exception) {
            logger.warn("Cannot read ${jar.name}: ${ex.message}")
        }
    }

    private fun fence(text: String): String {
        val f = if (text.contains("~~~~")) "~~~~~~" else "~~~~"
        return "$f\n$text\n$f\n"
    }

    @TaskAction
    fun generate() {
        val root = repositoryRoot.get().asFile
        val texts = sortedMapOf<String, Text>()
        val jars = HashSet<File>()
        jarDirs.files.filter { it.isDirectory }.forEach { d -> d.listFiles { f -> f.name.endsWith(".jar") }?.let { jars.addAll(it) } }
        tracked("*.jar").filter { !it.startsWith("gradle/wrapper/") }.forEach { jars.add(File(root, it)) }
        jars.sortedBy { it.name }.forEach { scanJar(it, it.name, texts) }
        // License files of the tracked web libraries
        tracked("*").filter { it.substringAfterLast('/').let { b -> nameRegex.matches(b) } }
            .filter { it != "LICENSE" && it != "NOTICE" && !it.startsWith("legal/") && !it.startsWith("docs/") && !it.startsWith("addons/") }
            .forEach { p ->
                val f = File(root, p)
                if (f.isFile && f.length() in 1..maxBytes) {
                    val text = f.readText(Charsets.UTF_8)
                    val kind = if (f.name.lowercase().startsWith("notice")) "NOTICE" else "LICENSE"
                    texts.getOrPut(kind + sha(text)) { Text(kind, text.trim()) }.sources.add(p)
                }
            }
        val rows = entriesReport.get().asFile.readLines()
            .filter { it.startsWith("| module |") || it.startsWith("| jar |") || it.startsWith("| web |") }
            .map { l -> l.trim().trim('|').split('|').map { it.trim() } }
            .filter { it.size >= 6 }
        if (rows.isEmpty()) throw GradleException("No entries in ${entriesReport.get().asFile}: run checkLicensePolicy first")

        val sb = StringBuilder()
        sb.append("# Third-party notices\n\n")
        sb.append("Generated by `gradlew generateThirdPartyNotices` (work package L-01). Do not edit by hand.\n")
        sb.append("Source of the table: the report of `gradlew checkLicensePolicy` (jk1 data and `gradle/license-policy.json`).\n")
        sb.append("Source of the texts: the NOTICE and LICENSE files in the runtime jars, in the tracked jars and in the tracked web libraries.\n")
        sb.append("Each text is once in this file. The Apache License 2.0 text of this product is in `LICENSE`, part B.\n\n")
        sb.append("## 1. Entries (${rows.size})\n\n")
        sb.append("| Scope | Entry | Version | License |\n|---|---|---|---|\n")
        rows.sortedWith(compareBy({ it[0] }, { it[1].lowercase() }, { it[2] })).forEach {
            sb.append("| ${it[0]} | ${it[1]} | ${it[2]} | ${it[3]} |\n")
        }
        for ((kind, title) in listOf("NOTICE" to "2. NOTICE texts", "LICENSE" to "3. License texts")) {
            val list = texts.values.filter { it.kind == kind }.sortedBy { it.sources.first().lowercase() }
            sb.append("\n## $title (${list.size})\n")
            var n = 0
            for (t in list) {
                n++
                val shown = t.sources.take(8).joinToString(", ") + if (t.sources.size > 8) ", and ${t.sources.size - 8} more" else ""
                sb.append("\n### $kind $n: $shown\n\n")
                sb.append(fence(t.text))
            }
        }
        outputFile.get().asFile.writeText(sb.toString(), Charsets.UTF_8)
        logger.lifecycle("Third-party notices: ${rows.size} entries, ${texts.values.count { it.kind == "NOTICE" }} NOTICE texts, " +
            "${texts.values.count { it.kind == "LICENSE" }} license texts, ${outputFile.get().asFile.length() / 1024} KB")
    }
}
