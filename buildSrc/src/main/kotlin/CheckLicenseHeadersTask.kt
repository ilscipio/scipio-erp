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
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import java.io.File

/**
 * Header check (work package L-01, docs/wp/L-01.md). Each tracked source file of a type in rules.txt has an Apache
 * header (the ASF text) or an AGPL header (the SPDX tag and the notice), or a rule skips it. The task fails on a file
 * that has no header, on a file that has both headers, and on a file with a wrong AGPL header. The rules are in
 * buildSrc/license-headers/rules.txt; the tool apply_headers.py reads the same file and uses the same classes.
 */
abstract class CheckLicenseHeadersTask : DefaultTask() {
    @get:Internal
    abstract val rulesFile: RegularFileProperty

    @get:Internal
    abstract val repositoryRoot: DirectoryProperty

    private val headLines = 40
    // A header line starts after comment characters only, so a string in code does not count.
    private val lead = "^[\\s/*#<!\\-;%~]*(?:REM[ \\t]+)?"
    private val asf = Regex(
        "The\\s+ASF\\s+licenses\\s+this\\s+file(?:\\s|rem|[/*#~%;])*to\\s+you\\s+under\\s+the\\s+Apache\\s+License",
        RegexOption.IGNORE_CASE
    )
    private val agplTag = Regex(lead + "SPDX-License-Identifier: AGPL-3\\.0-only", RegexOption.MULTILINE)
    private val agplTxt = Regex(lead + "[^\\n]*GNU Affero General", RegexOption.MULTILINE)
    private val spdxAny = Regex(lead + "SPDX-License-Identifier:(?! AGPL-3\\.0-only)", RegexOption.MULTILINE)
    private val oldNotice = Regex(
        "This file is subject to the terms and conditions defined in the\\s+" +
            "files 'LICENSE' and 'NOTICE', which are part of this source\\s+code package\\."
    )
    // Latin-1 view of the bytes: the copyright sign is the two characters Â©.
    private val third = Regex(
        lead + "(copyright|\\(c\\)|Â©|licensed under|licen[sc]e\\s*:|permission is hereby granted|@license|@preserve)",
        setOf(RegexOption.IGNORE_CASE, RegexOption.MULTILINE)
    )
    private val third2 = Regex(
        lead + "[^\\n]{0,60}\\b(MIT|BSD|GPL|LGPL|MPL|Apache)\\b[^\\n]{0,20}\\blicen[cs]e",
        setOf(RegexOption.IGNORE_CASE, RegexOption.MULTILINE)
    )

    private class Rules(
        val types: Set<String>,
        val skipDir: Set<String>,
        val skipPrefix: List<String>,
        val skipWebPrefix: List<String>,
        val skipName: List<Regex>
    )

    private fun globToRegex(g: String): Regex =
        Regex(g.map { c -> when (c) { '*' -> ".*"; '?' -> "."; else -> Regex.escape(c.toString()) } }.joinToString(""))

    private fun loadRules(f: File): Rules {
        val types = HashSet<String>()
        val skipDir = HashSet<String>()
        val skipPrefix = ArrayList<String>()
        val skipWeb = ArrayList<String>()
        val skipName = ArrayList<Regex>()
        f.readLines().map { it.trim() }.filter { it.isNotEmpty() && !it.startsWith("#") }.forEach { line ->
            val kind = line.substringBefore(' ')
            val value = line.substringAfter(' ').trim()
            when (kind) {
                "type" -> types.add(value.substringBefore(' '))
                "skip-dir" -> skipDir.add(value)
                "skip-prefix" -> skipPrefix.add(value)
                "skip-web-prefix" -> skipWeb.add(value)
                "skip-name" -> skipName.add(globToRegex(value))
                else -> throw GradleException("Unknown rule in ${f.path}: $line")
            }
        }
        return Rules(types, skipDir, skipPrefix, skipWeb, skipName)
    }

    /** Returns the class of a file: skip-type, skip-path, skip-content, apache, agpl, missing or wrong. */
    private fun classify(path: String, file: File, rules: Rules): Pair<String, String> {
        val base = path.substringAfterLast('/')
        val ext = if (base.contains('.')) "." + base.substringAfterLast('.').lowercase() else ""
        if (ext !in rules.types) return "skip-type" to ext
        val parts = path.split('/')
        parts.dropLast(1).firstOrNull { it in rules.skipDir }?.let { return "skip-path" to "dir $it" }
        rules.skipPrefix.firstOrNull { path.startsWith(it) }?.let { return "skip-path" to "prefix $it" }
        if (ext in setOf(".js", ".css", ".scss", ".less")) {
            rules.skipWebPrefix.firstOrNull { path.startsWith(it) }?.let { return "skip-path" to "web prefix $it" }
        }
        rules.skipName.firstOrNull { it.matches(base) }?.let { return "skip-path" to "name" }
        val bytes = file.inputStream().use { it.readNBytes(16384) }
        val head = String(bytes, Charsets.ISO_8859_1).split('\n', limit = headLines + 1).take(headLines).joinToString("\n")
        val a = asf.containsMatchIn(head)
        val s = agplTag.containsMatchIn(head)
        if (a && s) return "wrong" to "Apache and AGPL header together"
        if (a) return "apache" to ""
        if (s) {
            if (!agplTxt.containsMatchIn(head)) return "wrong" to "AGPL tag without the notice text"
            if (spdxAny.containsMatchIn(head)) return "wrong" to "second SPDX id"
            return "agpl" to ""
        }
        if (oldNotice.containsMatchIn(head)) return "missing" to "old Scipio notice"
        if (third.containsMatchIn(head) || third2.containsMatchIn(head)) return "skip-content" to "third-party notice"
        return "missing" to "no header"
    }

    @TaskAction
    fun check() {
        val root = repositoryRoot.get().asFile
        val rules = loadRules(rulesFile.get().asFile)
        val git = ProcessBuilder(listOf("git", "-C", root.path, "ls-files", "-z")).redirectErrorStream(false).start()
        val paths = git.inputStream.bufferedReader(Charsets.UTF_8).readText().split('\u0000').filter { it.isNotEmpty() }
        if (git.waitFor() != 0) throw GradleException("git ls-files failed")
        val counts = sortedMapOf<String, Int>()
        val problems = ArrayList<String>()
        for (p in paths) {
            val f = File(root, p)
            if (!f.isFile) continue
            val (cls, detail) = classify(p, f, rules)
            counts.merge(cls, 1, Int::plus)
            if (cls == "missing" || cls == "wrong") problems.add("$p: $cls ($detail)")
        }
        logger.lifecycle(
            "License headers: AGPL ${counts["agpl"] ?: 0}, Apache ${counts["apache"] ?: 0}, skipped " +
                "${(counts["skip-path"] ?: 0) + (counts["skip-content"] ?: 0)} of source types " +
                "(path rules ${counts["skip-path"] ?: 0}, third-party notice ${counts["skip-content"] ?: 0}), " +
                "other file types ${counts["skip-type"] ?: 0}, problems ${problems.size}"
        )
        if (problems.isNotEmpty()) {
            throw GradleException(
                "Header check failed for ${problems.size} file(s). Rules: buildSrc/license-headers/rules.txt, " +
                    "docs/wp/L-01.md.\n" + problems.take(50).joinToString("\n")
            )
        }
    }
}
