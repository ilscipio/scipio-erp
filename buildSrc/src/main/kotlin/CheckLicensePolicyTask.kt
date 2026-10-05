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
 * Scipio ERP - License policy check (work package L-06, L-06b)
 *
 * Classifies the license of each third-party module (the runtime jars of the core and the Solr webapp jars), of each
 * committed jar, and of each third-party JavaScript or CSS library, by gradle/license-policy.json. It writes the
 * report and fails on a blocked entry. Rules: docs/licenses/policy.md.
 *
 * Ported from scipio-ai (build.gradle.kts, task checkLicensePolicy). Changes: the scan reads the configuration
 * runtimeLibs of the root project and solrWebapp of :applications:solr; the scan of the web libraries is new.
 */

package com.ilscipio.scipio.gradle

import groovy.json.JsonSlurper
import org.gradle.api.DefaultTask
import org.gradle.api.GradleException
import org.gradle.api.artifacts.component.ComponentIdentifier
import org.gradle.api.artifacts.component.ModuleComponentIdentifier
import org.gradle.api.artifacts.component.ProjectComponentIdentifier
import org.gradle.api.artifacts.result.ResolvedComponentResult
import org.gradle.api.artifacts.result.ResolvedDependencyResult
import org.gradle.api.file.DirectoryProperty
import org.gradle.api.file.RegularFileProperty
import org.gradle.api.tasks.Internal
import org.gradle.api.tasks.TaskAction
import java.io.File
import java.util.Properties
import java.util.zip.ZipFile

/** One row of the license report */
data class LicenseRow(
    val scope: String, val module: String, val version: String, val licenses: List<String>,
    val licenseClass: String, val source: String, val exception: String?, val users: Set<String> = emptySet()
)

abstract class CheckLicensePolicyTask : DefaultTask() {
    @get:Internal abstract val policyFile: RegularFileProperty
    @get:Internal abstract val jk1Report: RegularFileProperty
    @get:Internal abstract val reportFile: RegularFileProperty
    @get:Internal abstract val repositoryRoot: DirectoryProperty

    init {
        outputs.upToDateWhen { false }
    }

    @Suppress("UNCHECKED_CAST")
    private fun <T> cast(value: Any?) = value as T

    private fun oneLine(text: String) = text.replace(Regex("\\s+"), " ").trim()

    /** The files that Git tracks, matching the pathspecs (Gradle does not see committed files) */
    private fun trackedFiles(vararg pathspec: String): List<String> {
        val git = ProcessBuilder(listOf("git", "-C", repositoryRoot.get().asFile.path, "ls-files", "-z", "--") + pathspec)
            .redirectError(ProcessBuilder.Redirect.INHERIT).start()
        val paths = git.inputStream.bufferedReader().readText().split('\u0000').filter { it.isNotEmpty() }
        if (git.waitFor() != 0) throw GradleException("git ls-files failed")
        return paths
    }

    @TaskAction
    fun check() {
        val root = repositoryRoot.get().asFile
        val policy: Map<String, Any> = cast(JsonSlurper().parse(policyFile.get().asFile))
        val classes = listOf("allowed", "review", "blocked")
        val classOfId = classes.flatMap { c -> cast<List<String>>(policy[c]).map { it to c } }.toMap()
        val aliases = cast<List<Map<String, Any>>>(policy["aliases"]).map { alias ->
            alias["id"] as String to cast<List<String>>(alias["patterns"]).map { Regex(it, RegexOption.IGNORE_CASE) }
        }
        val closedOnly: List<String> = cast(policy["closedOnly"])
        val declared: Map<String, Map<String, Any>> = cast(policy["declared"])
        val declaredJars: List<Map<String, Any>> = cast(policy["jars"])
        val declaredWeb: List<Map<String, Any>> = cast(policy["web"])
        val ownWeb: List<Regex> = cast<List<String>>(policy["webOwn"]).map { Regex(it) }
        val exceptions: Map<String, String> = cast(policy["exceptions"])

        fun normalize(text: String?): String? = text?.trim()?.takeIf { it.isNotEmpty() }?.let { t ->
            aliases.firstOrNull { (_, patterns) -> patterns.any { it.containsMatchIn(t) } }?.first
        }
        // The order of the aliases is important: the samples in the policy guard it
        val wrongAliases = cast<List<List<String>>>(policy["aliasSamples"]).filter { (text, id) -> normalize(text) != id }
            .map { (text, id) -> "'$text' gives ${normalize(text)}, expected $id" }
        if (wrongAliases.isNotEmpty()) throw GradleException("Wrong aliases in gradle/license-policy.json:\n  " + wrongAliases.joinToString("\n  "))

        // A list of licenses is a choice: the best class counts. In one entry, OR takes the best class and AND the
        // worst. An id that the policy does not name is blocked, and so is closed code (all of this repository is core).
        fun classOf(licenses: List<String>): String = classes[licenses.minOf { entry ->
            entry.split(" OR ").minOf { choice ->
                choice.split(" AND ").map { it.trim() }.maxOf { id ->
                    if (id in closedOnly) classes.lastIndex else classes.indexOf(classOfId[id] ?: "blocked")
                }
            }
        }]
        val usedExceptions = mutableSetOf<String>()
        val usedDeclared = mutableSetOf<String>()
        fun row(scope: String, module: String, version: String, keys: List<String>, found: List<String>, source: String): LicenseRow {
            val licenses = found.distinct().ifEmpty { listOf("LicenseRef-Unknown") }
            val licenseClass = classOf(licenses)
            val exceptionKey = keys.firstOrNull { it in exceptions }
            if (exceptionKey != null) usedExceptions.add(exceptionKey)
            val exception = exceptionKey?.takeIf { licenseClass == "blocked" }?.let { exceptions[it] }
            return LicenseRow(scope, module, version, licenses, licenseClass, source, exception)
        }

        // The projects that use a module: the module is reached from the runtime classpath (or solrWebapp) of the
        // project without another project between.
        fun ResolvedComponentResult.selectedDependencies() = dependencies.filterIsInstance<ResolvedDependencyResult>().map { it.selected }
        fun walk(start: ResolvedComponentResult): Set<ResolvedComponentResult> {
            val seen = mutableSetOf<ComponentIdentifier>()
            val found = mutableSetOf<ResolvedComponentResult>()
            val queue = ArrayDeque(listOf(start))
            while (queue.isNotEmpty()) {
                val component = queue.removeFirst()
                if (!seen.add(component.id)) continue
                found.add(component)
                if (component == start || component.id !is ProjectComponentIdentifier) queue.addAll(component.selectedDependencies())
            }
            return found
        }
        val users = mutableMapOf<String, MutableSet<String>>()
        for (p in project.rootProject.allprojects) {
            for (name in listOf("runtimeClasspath", "solrWebapp")) {
                val configuration = p.configurations.findByName(name)?.takeIf { it.isCanBeResolved } ?: continue
                val label = if (name == "solrWebapp") "${p.path} (solrWebapp)" else p.path
                walk(configuration.incoming.resolutionResult.root).mapNotNull { it.id as? ModuleComponentIdentifier }
                    .forEach { users.getOrPut("${it.group}:${it.module}:${it.version}") { sortedSetOf() }.add(label) }
            }
        }

        val rows = mutableListOf<LicenseRow>()

        // 1. The Gradle modules: the jk1 report has the licenses from the POM, the manifest and the license files
        val jk1: Map<String, Any> = cast(JsonSlurper().parse(jk1Report.get().asFile))
        for (dependency in cast<List<Map<String, Any?>>>(jk1["dependencies"])) {
            val module = dependency["moduleName"] as String
            val version = dependency["moduleVersion"] as String
            val used = users["$module:$version"].orEmpty()
            val keys = listOf("$module:$version", module)
            val fact = keys.firstOrNull { it in declared }?.also { usedDeclared.add(it) }?.let { declared[it]!! }
            val licenseRow = if (fact != null) {
                row("module", module, version, keys, cast(fact["licenses"]), "declared: ${fact["source"]}")
            } else {
                val raw = cast<List<Map<String, Any?>>?>(dependency["moduleLicenses"]).orEmpty()
                    .map { (it["moduleLicense"] as String?).orEmpty() to (it["moduleLicenseUrl"] as String?).orEmpty() }
                // A name that no alias maps is an unknown license: next to a known one, the module shows as a choice
                val mapped = raw.map { (name, url) -> Triple(name, url, normalize(name) ?: normalize(url)) }
                val unmapped = mapped.filter { it.third == null }.map { (name, url) -> oneLine("$name $url") }
                row("module", module, version, keys, mapped.map { it.third ?: "LicenseRef-Unknown" },
                    "metadata" + if (unmapped.isEmpty()) "" else "; not mapped: ${unmapped.joinToString("; ")}")
            }
            rows += licenseRow.copy(users = used)
        }

        // 2. The committed jars: the policy entry for the path, else the manifest (Bundle-License), else the license file
        val licenseFileName = Regex("^(META-INF/)?LICEN[CS]E[^/]*$", RegexOption.IGNORE_CASE)
        for (path in trackedFiles("*.jar").filterNot { it.startsWith("gradle/wrapper/") }.sorted()) {
            val jar = root.resolve(path)
            // The component of a lib/ folder: framework/base/lib is on the start classpath of the core
            val component = Regex("^([^/]+)/([^/]+)/lib/").find(path)?.groupValues?.let { ":${it[1]}:${it[2]}" }
            ZipFile(jar).use { zip ->
                val entries = zip.entries().asSequence().filterNot { it.isDirectory }.toList()
                // A jar can bundle other libraries with their own pom.properties: take the one of the jar itself
                val pom = entries.filter { it.name.startsWith("META-INF/maven/") && it.name.endsWith("/pom.properties") }
                    .map { entry -> Properties().apply { zip.getInputStream(entry).use { load(it) } } }
                    .firstOrNull { jar.name.startsWith(it.getProperty("artifactId") + "-") }
                val manifest = zip.getEntry("META-INF/MANIFEST.MF")?.let { entry -> zip.getInputStream(entry).use { java.util.jar.Manifest(it).mainAttributes } }
                val identity = pom?.let { "${it.getProperty("groupId")}:${it.getProperty("artifactId")}" }
                    ?: manifest?.getValue("Implementation-Title") ?: manifest?.getValue("Bundle-SymbolicName")
                val version = pom?.getProperty("version") ?: manifest?.getValue("Implementation-Version")?.substringBefore(' ')
                    ?: manifest?.getValue("Bundle-Version") ?: Regex("-([0-9]+([.][0-9A-Za-z]+)*)").find(jar.nameWithoutExtension)?.groupValues?.get(1) ?: ""
                val keys = listOf(path)
                val fact = declaredJars.firstOrNull { Regex(it["path"] as String).containsMatchIn(path) }?.also { usedDeclared.add(it["path"] as String) }
                val bundleLicense = normalize(manifest?.getValue("Bundle-License"))
                val licenseFile = entries.filter { licenseFileName.matches(it.name) }.minByOrNull { it.name.length }
                val fileLicense = licenseFile?.let { entry -> normalize(oneLine(zip.getInputStream(entry).use { String(it.readNBytes(400), Charsets.ISO_8859_1) })) }
                val what = identity?.let { "$it; " }.orEmpty()
                val licenseRow = when {
                    fact != null -> row("jar", path, version, keys, cast(fact["licenses"]), "${what}declared: ${fact["source"]}")
                    bundleLicense != null -> row("jar", path, version, keys, listOf(bundleLicense), "${what}manifest Bundle-License")
                    fileLicense != null -> row("jar", path, version, keys, listOf(fileLicense), "${what}license file ${licenseFile.name}")
                    licenseFile != null -> row("jar", path, version, keys, emptyList(), "${what}license file ${licenseFile.name}: no alias matches")
                    else -> row("jar", path, version, keys, emptyList(), "${what}no license data in the jar")
                }
                rows += licenseRow.copy(users = setOfNotNull(component))
            }
        }

        // 3. The web libraries (JavaScript and CSS files that Git tracks). A library is a folder below bower_components,
        // node_modules or libs, or a folder with a bower.json or package.json that names a license, or (outside these) a
        // single file with a license notice or a minified file. Its license comes from the policy ("web"), else from
        // the metadata file, else from a license file in the folder, else from the notice in the first file.
        val webFiles = trackedFiles("*.js", "*.css")
        val allTracked = trackedFiles().toHashSet()
        fun readHead(path: String, limit: Int): String =
            root.resolve(path).inputStream().use { String(it.readNBytes(limit), Charsets.UTF_8) }
        fun jsonLicenseTexts(node: Any?): List<String> = when (node) {
            is String -> listOf(node)
            is List<*> -> node.flatMap { jsonLicenseTexts(it) }
            is Map<*, *> -> listOf(listOfNotNull(node["type"], node["name"], node["url"]).joinToString(" "))
            else -> emptyList()
        }
        val metaNames = listOf(".bower.json", "bower.json", "package.json")
        val libraryRoot = Regex("^(.*?/(?:bower_components|node_modules|libs)/(?:@[^/]+/)?[^/]+)/")
        // A folder with metadata that names a license is a library, except the first-party roots of the policy
        val firstPartyRoots = cast<List<String>>(policy["webFirstParty"]).map { Regex(it) }
        val metaLicenses = mutableMapOf<String, List<String>>()
        for (path in allTracked.filter { f -> metaNames.any { f.endsWith("/$it") } }) {
            val dir = path.substringBeforeLast('/')
            if (firstPartyRoots.any { it.containsMatchIn(dir) } || dir in metaLicenses) continue
            val json: Map<String, Any?> = try { cast(JsonSlurper().parse(root.resolve(path))) } catch (e: Exception) { continue }
            val texts = jsonLicenseTexts(json["license"] ?: json["licenses"])
            if (texts.isNotEmpty()) metaLicenses[dir] = texts
        }
        val webUnits = sortedMapOf<String, MutableList<String>>()
        val singles = mutableListOf<String>()
        for (path in webFiles) {
            val dir = path.substringBeforeLast('/')
            val unit = libraryRoot.find(path)?.groupValues?.get(1)
                ?: metaLicenses.keys.filter { dir == it || dir.startsWith("$it/") }.maxByOrNull { it.length }
            if (unit != null) webUnits.getOrPut(unit) { mutableListOf() }.add(path) else singles.add(path)
        }
        val noticePattern = Regex("(@license|licensed under|released under|license:|mit license|\\bmit\\b|\\bgpl|apache license|\\bbsd\\b|\\bisc\\b)", RegexOption.IGNORE_CASE)
        // The licenses that the header of a file names. A header that names two (for example "MIT or GPL") is a choice.
        fun noticeLicense(path: String): List<String> {
            // Only comments count: a CSS selector such as .gplan is not a license
            val head = Regex("""/\*[\s\S]*?\*/|//[^\n]*""").findAll(readHead(path, 3000)).joinToString("\n") { it.value }
            return noticePattern.findAll(head).mapNotNull { m ->
                normalize(m.value) ?: normalize(oneLine(head.substring(maxOf(0, m.range.first - 40), minOf(head.length, m.range.last + 120))))
            }.distinct().toList()
        }
        for ((unit, files) in webUnits) {
            val fact = declaredWeb.firstOrNull { Regex(it["path"] as String).containsMatchIn(unit) }?.also { usedDeclared.add(it["path"] as String) }
            val name = unit.substringAfterLast("/")
            val metaTexts = metaLicenses[unit].orEmpty()
            val licenseFile = allTracked.filter { it.substringBeforeLast('/') == unit && Regex("^(LICEN[CS]E|COPYING|MIT-LICENSE)[^/]*$", RegexOption.IGNORE_CASE).matches(it.substringAfterLast('/')) }.minOrNull()
            val metaIds = metaTexts.mapNotNull { normalize(it) }
            val fileId = licenseFile?.let { normalize(oneLine(readHead(it, 600))) }
            val noticeIds = files.sorted().map { noticeLicense(it) }.firstOrNull { it.isNotEmpty() }
            val version = ""
            val licenseRow = when {
                fact != null -> row("web", unit, version, listOf(unit), cast(fact["licenses"]), "declared: ${fact["source"]}")
                metaIds.isNotEmpty() && metaIds.size == metaTexts.size -> row("web", unit, version, listOf(unit), metaIds, "metadata file (${files.size} files)")
                fileId != null -> row("web", unit, version, listOf(unit), listOf(fileId), "license file ${licenseFile.substringAfterLast('/')} (${files.size} files)")
                noticeIds != null -> row("web", unit, version, listOf(unit), noticeIds, "notice in the file header (${files.size} files)")
                else -> row("web", unit, version, listOf(unit), emptyList(), "no license data ($name, ${files.size} files)")
            }
            rows += licenseRow.copy(users = setOf(unit.substringBefore("/webapp/").ifEmpty { unit }))
        }
        // A single file outside a library: a license notice names the license; a minified file without a notice is unknown
        val generatedName = Regex("([.-]min[.]|[.]bundle[.])")
        for (path in singles.filterNot { p -> ownWeb.any { it.containsMatchIn(p) } }) {
            val fact = declaredWeb.firstOrNull { Regex(it["path"] as String).containsMatchIn(path) }?.also { usedDeclared.add(it["path"] as String) }
            val ids = if (fact == null) noticeLicense(path) else emptyList()
            when {
                fact != null -> rows += row("web", path, "", listOf(path), cast(fact["licenses"]), "declared: ${fact["source"]}")
                ids.isNotEmpty() -> rows += row("web", path, "", listOf(path), ids, "notice in the file header")
                generatedName.containsMatchIn(path.substringAfterLast('/')) -> rows += row("web", path, "", listOf(path), emptyList(), "minified file without a license notice")
                // else: own code of this repository, not a third-party library
            }
        }

        // The report
        val scopes = listOf("module", "jar", "web")
        val sorted = rows.sortedWith(compareBy({ scopes.indexOf(it.scope) }, { it.module }, { it.version }))
        fun expression(r: LicenseRow) = r.licenses.joinToString(" OR ") { if (" " in it && r.licenses.size > 1) "($it)" else it }
        fun cell(text: String) = text.replace("|", "\\|")
        val out = StringBuilder()
        out.appendLine("# Third-party licenses")
        out.appendLine()
        out.appendLine("Generated by `gradlew checkLicensePolicy` from `gradle/license-policy.json`. Policy: `docs/licenses/policy.md`.")
        out.appendLine("A list of licenses on one module is a choice (OR); the best class counts.")
        out.appendLine("Scopes: module = a Gradle module (the runtime jars of the core and the Solr webapp jars),")
        out.appendLine("jar = a jar that Git tracks, web = a JavaScript or CSS library that Git tracks.")
        out.appendLine()
        out.appendLine("## Counts per class")
        out.appendLine()
        out.appendLine("| Class | " + scopes.joinToString(" | ") + " | All |")
        out.appendLine("|---|" + "---:|".repeat(scopes.size + 1))
        for (c in classes) {
            val counts = scopes.map { s -> rows.count { it.scope == s && it.licenseClass == c } }
            out.appendLine("| $c | " + counts.joinToString(" | ") + " | ${counts.sum()} |")
        }
        out.appendLine("| all | " + scopes.map { s -> rows.count { it.scope == s } }.joinToString(" | ") + " | ${rows.size} |")
        out.appendLine()
        out.appendLine("## Counts per license")
        out.appendLine()
        out.appendLine("| License | Class | Entries |")
        out.appendLine("|---|---|---:|")
        rows.groupBy { expression(it) }.entries.sortedWith(compareBy({ classes.indexOf(it.value.first().licenseClass) }, { -it.value.size }, { it.key }))
            .forEach { (license, group) -> out.appendLine("| ${cell(license)} | ${group.first().licenseClass} | ${group.size} |") }
        out.appendLine()
        fun usedBy(r: LicenseRow) = r.users.take(3).joinToString(", ") + if (r.users.size > 3) ", and ${r.users.size - 3} more" else ""
        out.appendLine("## Blocked and review")
        out.appendLine()
        out.appendLine("| Class | Scope | Entry | Version | License | Used by | Exception |")
        out.appendLine("|---|---|---|---|---|---|---|")
        sorted.filter { it.licenseClass != "allowed" }.sortedByDescending { classes.indexOf(it.licenseClass) }.forEach {
            out.appendLine("| ${it.licenseClass} | ${it.scope} | ${cell(it.module)} | ${cell(it.version)} | ${cell(expression(it))} | ${cell(usedBy(it))} | ${cell(it.exception.orEmpty())} |")
        }
        out.appendLine()
        out.appendLine("## Allowed by a choice")
        out.appendLine()
        out.appendLine("These entries name a review or blocked license next to an allowed one. The check reads them as a choice. Confirm")
        out.appendLine("that the licenses are a choice and not parts of the entry (then add an AND entry to `declared`).")
        out.appendLine()
        out.appendLine("| Scope | Entry | Version | License |")
        out.appendLine("|---|---|---|---|")
        sorted.filter { r -> r.licenseClass == "allowed" && r.licenses.any { classOf(listOf(it)) != "allowed" } }.forEach {
            out.appendLine("| ${it.scope} | ${cell(it.module)} | ${cell(it.version)} | ${cell(expression(it))} |")
        }
        out.appendLine()
        out.appendLine("## All entries")
        out.appendLine()
        out.appendLine("| Scope | Entry | Version | License | Class | Source |")
        out.appendLine("|---|---|---|---|---|---|")
        sorted.forEach {
            out.appendLine("| ${it.scope} | ${cell(it.module)} | ${cell(it.version)} | ${cell(expression(it))} | ${it.licenseClass} | ${cell(it.source)} |")
        }
        val report = reportFile.get().asFile
        report.parentFile.mkdirs()
        report.writeText(out.toString())

        val blocked = sorted.filter { it.licenseClass == "blocked" && it.exception == null }
        logger.lifecycle("License report: ${report.toURI()}")
        logger.lifecycle(classes.joinToString(", ") { c -> "$c ${rows.count { it.licenseClass == c }}" } +
            " (${rows.count { it.exception != null }} blocked with an exception, ${rows.size} entries)")
        logger.lifecycle(scopes.joinToString(", ") { s -> "$s ${rows.count { it.scope == s }}" })
        (exceptions.keys - usedExceptions).forEach { logger.warn("License exception that matches no dependency, remove it: $it") }
        (declared.keys + declaredJars.map { it["path"] as String } + declaredWeb.map { it["path"] as String } - usedDeclared)
            .forEach { logger.warn("Declared license that matches no dependency, remove it: $it") }
        if (blocked.isNotEmpty()) {
            throw GradleException("Blocked licenses (docs/licenses/policy.md):\n" +
                blocked.joinToString("\n") { "  ${it.scope}: ${it.module}:${it.version} ${expression(it)} (${it.source})" })
        }
    }
}
