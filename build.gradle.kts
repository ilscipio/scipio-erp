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
 * Scipio ERP - Root Gradle Build Configuration
 *
 * This is the main build file for Scipio ERP. It configures:
 * - Common settings for all subprojects
 * - Source set layouts matching Scipio's non-standard structure
 * - Custom tasks for server management and data loading
 */

import com.ilscipio.scipio.gradle.CheckLicenseHeadersTask
import com.ilscipio.scipio.gradle.GenerateThirdPartyNoticesTask
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

plugins {
    java
    idea
    eclipse
}

// License headers (work package L-01): gradlew checkLicenseHeaders; part of check. Rules: buildSrc/license-headers/rules.txt.
val checkLicenseHeaders by tasks.registering(CheckLicenseHeadersTask::class) {
    group = "verification"
    description = "Fail on a source file without an Apache or AGPL header (rules: buildSrc/license-headers/rules.txt)"
    rulesFile.set(layout.projectDirectory.file("buildSrc/license-headers/rules.txt"))
    repositoryRoot.set(layout.projectDirectory)
}
tasks.named("check") { dependsOn(checkLicenseHeaders) }
// Third-party notice file (work package L-01): gradlew generateThirdPartyNotices writes THIRD-PARTY-NOTICES.md.
tasks.register<GenerateThirdPartyNoticesTask>("generateThirdPartyNotices") {
    group = "verification"
    description = "Write THIRD-PARTY-NOTICES.md: the third-party entries, the NOTICE texts and the license texts"
    dependsOn(":framework:checkLicensePolicy", "syncLibs", ":applications:solr:syncSolrWebappLibs")
    entriesReport.set(layout.buildDirectory.file("reports/licenses/license-report.md"))
    jarDirs.from(layout.projectDirectory.dir("framework/base/lib/gradle"), layout.projectDirectory.dir("applications/solr/webapp/WEB-INF/lib"))
    repositoryRoot.set(layout.projectDirectory)
    outputFile.set(layout.projectDirectory.file("THIRD-PARTY-NOTICES.md"))
}

// ============================================================================
// ALL PROJECTS CONFIGURATION
// ============================================================================

allprojects {
    group = "com.ilscipio.scipio"
    version = "4.0.0"

    repositories {
        mavenCentral()
        maven {
            url = uri("https://repo.maven.apache.org/maven2")
        }
    }
}

// ============================================================================
// SUBPROJECTS CONFIGURATION
// ============================================================================

// Aggregate projects that should NOT have java plugin applied
val aggregateProjects = setOf("framework", "applications", "themes", "hot-deploy", "addons")

subprojects {
    // Skip aggregate projects - they don't have source code
    if (project.name in aggregateProjects) {
        return@subprojects
    }

    apply(plugin = "java-library")

    // SCIPIO: 4.0.0: a discovered hot-deploy or addon component without its own build.gradle.kts gets the
    // standard component build and the whole framework and application classpath (see settings.gradle.kts).
    @Suppress("UNCHECKED_CAST")
    val descriptorOnlyProjects = rootProject.extra.properties["scipioDescriptorOnlyProjects"] as? List<String> ?: emptyList()
    if (project.path in descriptorOnlyProjects) {
        apply(plugin = "scipio-component")
        rootProject.subprojects
            .filter { it.path.startsWith(":framework:") || it.path.startsWith(":applications:") }
            .forEach { dependencies.add("api", it) }
        // SCIPIO: 4.0.0: an Ant-era addon ships its jars in lib/ (descriptor: <classpath type="jar" location="lib/*"/>).
        dependencies.add("implementation", fileTree("lib") { include("*.jar") })
    }

    // Java configuration
    configure<JavaPluginExtension> {
        sourceCompatibility = JavaVersion.VERSION_11
        targetCompatibility = JavaVersion.VERSION_11

        // Optionally use newer Java if available
        if (JavaVersion.current() >= JavaVersion.VERSION_17) {
            toolchain {
                languageVersion.set(JavaLanguageVersion.of(17))
            }
        }
    }

    // Compiler options
    tasks.withType<JavaCompile> {
        options.encoding = "UTF-8"
        options.compilerArgs.addAll(listOf(
            "-Xlint:unchecked",
            "-Xlint:deprecation"
        ))
    }

    // Configure Scipio's non-standard source layout
    configure<SourceSetContainer> {
        named("main") {
            java {
                setSrcDirs(listOf("src"))
                // Set main output to build/classes to match Ant behavior
                destinationDirectory.set(file("build/classes"))
            }
            resources {
                setSrcDirs(listOf(
                    "src",       // Include .properties and other resources alongside Java files
                    "config",
                    "dtd",
                    "servicedef",
                    "entitydef",
                    "data",
                    "templates",
                    "script",
                    "widget"
                ).filter { file(it).exists() })
                // Exclude Java files from resources (they're handled by java source set)
                exclude("**/*.java")
            }
        }
        named("test") {
            java {
                // Only include test directory if it exists, otherwise empty
                if (file("test").exists()) {
                    setSrcDirs(listOf("test"))
                } else {
                    setSrcDirs(emptyList<String>())
                }
                // CRITICAL: Set test output to completely separate directory to avoid
                // Gradle detecting implicit dependencies between compileJava and compileTestJava
                destinationDirectory.set(file("build/test-classes"))
            }
        }
    }

    // Output JAR to build/lib to match Ant behavior
    tasks.withType<Jar> {
        destinationDirectory.set(file("build/lib"))
        archiveBaseName.set("scipio-${project.name}")
        archiveVersion.set("")  // Don't include version in JAR name for classpath consistency
    }

    // Test configuration
    tasks.withType<Test> {
        useJUnitPlatform()
    }

    // Handle duplicate resources (src is in both java and resources source sets)
    tasks.withType<Copy> {
        if (name == "processResources") {
            duplicatesStrategy = DuplicatesStrategy.EXCLUDE
        }
    }
}

// ============================================================================
// CUSTOM TASKS
// ============================================================================

// Clean all build artifacts
tasks.register("cleanAll") {
    group = "build"
    description = "Clean all build artifacts including data and logs"
    dependsOn("clean")
    doLast {
        delete(file("runtime/data/derby"))
        delete(fileTree("runtime/logs") { include("*.log") })
    }
}

// SCIPIO: 4.0.0: Pooled runtime (W1-01, G19): the store isolation suite end to end (Linux or Git Bash, Docker).
// The CI job isolation-suite (.gitlab-ci.yml) runs the same script. Env: STORES, SOAK_SECONDS, THREADS, HEAP.
tasks.register<Exec>("isolationSuite") {
    group = "verification"
    description = "Pooled runtime: provision test stores and run the store isolation suite (needs Docker)"
    commandLine("bash", "tools/pooled-spike/ci/run-isolation.sh")
    environment("SKIP_BUILD", "1")
    dependsOn("build", "syncLibs", ":applications:solr:syncSolrWebappLibs")
}

// Clean data only
tasks.register("cleanData") {
    group = "build"
    description = "Clean local Derby database"
    doLast {
        delete(file("runtime/data/derby"))
    }
}

// Clean logs only
tasks.register("cleanLogs") {
    group = "build"
    description = "Clean log files"
    doLast {
        delete(fileTree("runtime/logs") { include("*.log") })
    }
}

// Helper to get subprojects that have Java code (exclude aggregate projects)
val javaSubprojects = subprojects.filter { it.name !in aggregateProjects }

// SCIPIO: 4.0.0: Third-party runtime jars. The component descriptors put lib/* on the runtime classpath, as
// they did when ivy filled those directories; Gradle now resolves the union of every component's runtime
// dependencies once (one version per module) and mirrors the jars into framework/base/lib/gradle, so a fresh
// checkout starts. Every JavaExec task that boots the server depends on it.
val runtimeLibs: Configuration by configurations.creating {
    isCanBeConsumed = false
    isCanBeResolved = true
    // One BouncyCastle generation only: the catalog declares the jdk18on jars; the jdk14 and jdk15on variants
    // arrive transitively, carry the same packages under other signatures, and make the JVM refuse the classes
    // ("signer information does not match"), which took the sshd file system provider and Groovy down with it.
    exclude(group = "bouncycastle")
    for (m in listOf("bcprov-jdk14", "bcmail-jdk14", "bctsp-jdk14", "bcprov-jdk15on", "bcpkix-jdk15on", "bcmail-jdk15on",
            "bcutil-jdk15on", "bcprov-jdk15to18", "bcpkix-jdk15to18", "bcutil-jdk15to18")) {
        exclude(group = "org.bouncycastle", module = m)
    }
    // Both carry their own org.apache.commons.logging.LogFactory next to commons-logging, and the jar order in a
    // directory is not fixed on every file system (ivy excluded them for the same reason).
    exclude(group = "org.slf4j", module = "jcl-over-slf4j")
    exclude(group = "org.springframework", module = "spring-jcl")
    // The same for javax.el.ExpressionFactory: tomcat-el-api is the EL API (see the servlet-api bundle in the catalog).
    exclude(group = "javax.el", module = "javax.el-api")
}
dependencies {
    javaSubprojects.forEach { runtimeLibs(project(it.path)) }
}
val syncLibs by tasks.registering(Sync::class) {
    group = "scipio"
    description = "Mirror the resolved third-party runtime jars into framework/base/lib/gradle"
    // Repository modules only: the component jars load from each component's build/lib, and a component's own
    // lib/*.jar files from its descriptor; mirrored here, an addon's private jars would reach every component.
    from(runtimeLibs.incoming.artifactView {
        componentFilter { it is org.gradle.api.artifacts.component.ModuleComponentIdentifier }
    }.files) {
        include("*.jar")
    }
    into(layout.projectDirectory.dir("framework/base/lib/gradle"))
}
// SCIPIO: 4.0.0: the footers include ofbizhome://runtime/gitinfo.ftl and ofbizhome://runtime/svninfo.ftl, which the Ant
// targets gitinfo and clean-gitinfo wrote; git ignores both, so a fresh checkout has neither and the footer include fails.
val gitinfo by tasks.registering {
    group = "scipio"
    description = "Write the Git branch and revision for the footer into runtime/gitinfo.ftl"
    val gitInfoFile = file("runtime/gitinfo.ftl")
    val svnInfoFile = file("runtime/svninfo.ftl")
    val rootDir = projectDir
    outputs.upToDateWhen { false }
    doLast {
        fun git(vararg args: String): String? = try {
            val proc = ProcessBuilder(listOf("git") + args).directory(rootDir).redirectErrorStream(true).start()
            val out = proc.inputStream.bufferedReader().readText().trim()
            if (proc.waitFor() == 0 && out.isNotEmpty()) out else null
        } catch (e: Exception) {
            null
        }
        // a CI checkout is a detached HEAD; GitLab names the branch in CI_COMMIT_REF_NAME
        val branch = git("rev-parse", "--abbrev-ref", "HEAD")?.takeIf { it != "HEAD" } ?: System.getenv("CI_COMMIT_REF_NAME")
        val revision = git("rev-parse", "HEAD")
        val dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
        gitInfoFile.parentFile.mkdirs()
        gitInfoFile.writeText(if (branch != null && revision != null) {
            " - Branch-revision: $branch-$revision, \${uiLabelMap.CommonBuiltOn} $dateTime"
        } else {
            ""
        })
        if (!svnInfoFile.isFile) {
            svnInfoFile.writeText("")
        }
    }
}
tasks.withType<JavaExec>().configureEach { dependsOn(syncLibs, gitinfo, ":applications:solr:syncSolrWebappLibs") }

// Bootstrap classpath for Scipio Start class
// Start class dynamically loads additional classpaths from scipio-component.xml files
val startClasspath = files(
    "framework/start/build/lib/scipio-start.jar",
    "framework/base/build/lib/scipio-base.jar",
    "framework/base/config",
    fileTree("framework/base/lib") { include("**/*.jar") }
)

// SCIPIO: 4.0.0: JVM module-system opens required at runtime by reflective libraries (e.g. FreeMarker's
// BeansWrapper walking java.util/java.lang/sun.util.calendar classes). Without these, JavaExec-forked
// server JVMs (Java 11+) log "cannot access class ... because module java.base does not export ...
// to unnamed module" errors - most visibly sun.util.calendar.ZoneInfo from FreeMarker. Shared across
// all JavaExec tasks that boot org.ofbiz.base.start.Start so the flags stay in sync.
val scipioAddOpensJvmArgs = listOf(
    "--add-opens=java.base/java.util=ALL-UNNAMED",
    "--add-opens=java.base/java.lang=ALL-UNNAMED",
    "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
    "--add-opens=java.base/java.net=ALL-UNNAMED",
    "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED"
)

// Load demo data
tasks.register<JavaExec>("loadDemo") {
    group = "scipio"
    description = "Load demo data into database"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    jvmArgs = scipioAddOpensJvmArgs
    // SCIPIO: optional -PloadComponent=<name> limits the load to one component, -PloadReaders=a,b overrides the readers
    // SCIPIO: 4.0.0: ext-demo last: demo data that builds on the demo data of several applications (the manufacturing
    // bicycle scenario needs accounting, order and shop data; the shop loads after manufacturing)
    val loadArgs = mutableListOf("load-data", "readers=" + (project.findProperty("loadReaders") ?: "seed,seed-initial,demo,ext,ext-demo"))
    project.findProperty("loadComponent")?.let { loadArgs.add("component=$it") }
    args = loadArgs
    workingDir = projectDir

    dependsOn(javaSubprojects.map { it.tasks.named("build") })
}

// Load seed data only
tasks.register<JavaExec>("loadSeed") {
    group = "scipio"
    description = "Load seed data into database"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    jvmArgs = scipioAddOpensJvmArgs
    // SCIPIO: optional -PloadComponent=<name> limits the load to one component, -PloadReaders=a,b overrides the readers
    val loadArgs = mutableListOf("load-data", "readers=" + (project.findProperty("loadReaders") ?: "seed,seed-initial"))
    project.findProperty("loadComponent")?.let { loadArgs.add("component=$it") }
    args = loadArgs
    workingDir = projectDir

    dependsOn(javaSubprojects.map { it.tasks.named("build") })
}

// Run tests (OFBiz test framework)
// Usage: ./gradlew runTest -PtestComponent=service -PtestCase=service-annotation-tests
tasks.register<JavaExec>("runTest") {
    group = "scipio"
    description = "Run OFBiz test suite. Use -PtestComponent=X -PtestCase=Y for specific tests"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    jvmArgs = scipioAddOpensJvmArgs
    workingDir = projectDir

    // Build the test command arguments
    val testArgs = mutableListOf("test")
    project.findProperty("testComponent")?.let { testArgs.add("-component=$it") }
    project.findProperty("testCase")?.let { testArgs.add("-case=$it") }
    project.findProperty("testSuite")?.let { testArgs.add("-suite=$it") }
    args = testArgs

    dependsOn(javaSubprojects.map { it.tasks.named("build") })
}

// Start server
tasks.register<JavaExec>("start") {
    group = "scipio"
    description = "Start Scipio ERP server"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    jvmArgs = scipioAddOpensJvmArgs
    workingDir = projectDir
    // Direct stdout/stderr to console without Gradle's logging wrapper
    standardOutput = System.out
    errorOutput = System.err
    standardInput = System.`in`

    dependsOn(javaSubprojects.map { it.tasks.named("build") })
}

// Start server with debug
tasks.register<JavaExec>("startDebug") {
    group = "scipio"
    description = "Start Scipio ERP server with debug port 5005"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    jvmArgs = scipioAddOpensJvmArgs + listOf(
        "-Xms512m",
        "-Xmx1024m",
        "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005"
    )
    workingDir = projectDir
    // Direct stdout/stderr to console without Gradle's logging wrapper
    standardOutput = System.out
    errorOutput = System.err
    standardInput = System.`in`

    dependsOn(javaSubprojects.map { it.tasks.named("build") })
}

// Stop server
tasks.register<JavaExec>("stop") {
    group = "scipio"
    description = "Stop Scipio ERP server"
    mainClass.set("org.ofbiz.base.start.Start")
    classpath = startClasspath
    args = listOf("-shutdown") // SCIPIO: Start expects a single dash
    workingDir = projectDir
}

// Rebuild (clean + build)
tasks.register("rebuild") {
    group = "build"
    description = "Clean and rebuild all projects"
    dependsOn("clean")
    dependsOn("build")
}
// Ensure build runs after clean for rebuild task
tasks.named("build") {
    mustRunAfter("clean")
}

// ============================================================================
// COMPONENT CREATION TASKS
// ============================================================================

// Create a new component in hot-deploy
// Usage: ./gradlew createComponent -PcomponentName=mycomponent -PresourceName=MyComponent -PwebappName=mycomponent -PbasePermission=MYCOMPONENT
tasks.register("createComponent") {
    group = "scipio"
    description = "Create a new component in hot-deploy folder"
    doLast {
        val componentName = project.findProperty("componentName")?.toString()
            ?: throw GradleException("componentName is required. Use -PcomponentName=mycomponent")
        val resourceName = project.findProperty("resourceName")?.toString()
            ?: throw GradleException("resourceName is required. Use -PresourceName=MyComponent")
        val webappName = project.findProperty("webappName")?.toString() ?: componentName
        val basePermission = project.findProperty("basePermission")?.toString()
            ?: componentName.uppercase()
        val componentPackage = project.findProperty("componentPackage")?.toString()
            ?: "com.ilscipio.scipio.ce.external"

        val templateDir = file("framework/resources/templates")
        val targetDir = file("hot-deploy/$componentName")

        // Java package directory for the generated annotation classes: <componentPackage>/<componentName>
        val packageDir = componentPackage.replace(".", "/") + "/" + componentName
        val javaSrcDir = "src/$packageDir"

        if (targetDir.exists()) {
            throw GradleException("Component already exists: $targetDir")
        }

        println("Creating component: $componentName")
        println("  Resource name: $resourceName")
        println("  Webapp name: $webappName")
        println("  Base permission: $basePermission")
        println("  Package: $componentPackage")

        // Create directory structure
        listOf(
            "", "config", "data", "data/helpdata", "dtd", "documents",
            "entitydef", "lib", "libsrc", "patches", "patches/test",
            "patches/qa", "patches/production", "script", "servicedef",
            "src", "testdef", "webapp", "webapp/$webappName",
            "webapp/$webappName/error", "webapp/$webappName/WEB-INF",
            "webapp/$webappName/WEB-INF/actions", "widget",
            "$javaSrcDir/controller", "$javaSrcDir/widget",
            "$javaSrcDir/service", "$javaSrcDir/entity", "$javaSrcDir/mcp"
        ).forEach { dir ->
            file("$targetDir/$dir").mkdirs()
        }

        // Copy .gitignore files
        copy {
            from("$templateDir/.gitignore-component")
            into(targetDir)
            rename { ".gitignore" }
        }
        listOf("dtd", "patches/test", "patches/qa", "patches/production",
               "script", "testdef", "webapp/$webappName/WEB-INF/actions").forEach { dir ->
            copy {
                from("$templateDir/.gitignore-empty")
                into("$targetDir/$dir")
                rename { ".gitignore" }
            }
        }
        listOf("lib", "libsrc").forEach { dir ->
            copy {
                from("$templateDir/.gitignore-lib")
                into("$targetDir/$dir")
                rename { ".gitignore" }
            }
        }

        // Filter function for template placeholders
        val filterTokens = mapOf(
            "@component-name@" to componentName,
            "@component-resource-name@" to resourceName,
            "@base-permission@" to basePermission,
            "@webapp-name@" to webappName,
            "@component-package@" to componentPackage
        )

        fun copyTemplate(from: String, to: String) {
            val content = file("$templateDir/$from").readText()
            var filtered = content
            filterTokens.forEach { (token, value) ->
                filtered = filtered.replace(token, value)
            }
            file("$targetDir/$to").writeText(filtered)
        }

        // Copy and filter template files
        copyTemplate("scipio-component.xml", "scipio-component.xml")
        copyTemplate("build.gradle.kts", "build.gradle.kts")
        copyTemplate("TypeData.xml", "data/${resourceName}TypeData.xml")
        copyTemplate("SecurityPermissionSeedData.xml", "data/${resourceName}SecurityPermissionSeedData.xml")
        copyTemplate("SecurityGroupDemoData.xml", "data/${resourceName}SecurityGroupDemoData.xml")
        copyTemplate("DemoData.xml", "data/${resourceName}DemoData.xml")
        copyTemplate("HELP.xml", "data/helpdata/HELP_${resourceName}.xml")
        copyTemplate("document.xml", "documents/${resourceName}.xml")
        copyTemplate("Tests.xml", "testdef/${resourceName}Tests.xml")
        copyTemplate("UiLabels.xml", "config/${resourceName}UiLabels.xml")
        copyTemplate("index.jsp", "webapp/$webappName/index.jsp")
        copyTemplate("controller.xml", "webapp/$webappName/WEB-INF/controller.xml")
        copyTemplate("web.xml", "webapp/$webappName/WEB-INF/web.xml")
        copyTemplate("CommonScreens.xml", "widget/CommonScreens.xml")

        // Copy and filter the Java annotation templates (controller, screens, forms, menus,
        // services, entities and one MCP tool) in place of the former XML widget/service/entity defs.
        copyTemplate("java/controller/ControllerDef.java", "$javaSrcDir/controller/${resourceName}ControllerDef.java")
        copyTemplate("java/widget/CommonScreens.java", "$javaSrcDir/widget/CommonScreens.java")
        copyTemplate("java/widget/Screens.java", "$javaSrcDir/widget/${resourceName}Screens.java")
        copyTemplate("java/widget/Forms.java", "$javaSrcDir/widget/${resourceName}Forms.java")
        copyTemplate("java/widget/Menus.java", "$javaSrcDir/widget/${resourceName}Menus.java")
        copyTemplate("java/service/Services.java", "$javaSrcDir/service/${resourceName}Services.java")
        copyTemplate("java/service/ServiceImpl.java", "$javaSrcDir/service/${resourceName}ServiceImpl.java")
        copyTemplate("java/entity/Entities.java", "$javaSrcDir/entity/${resourceName}Entities.java")
        copyTemplate("java/mcp/Mcp.java", "$javaSrcDir/mcp/${resourceName}Mcp.java")

        println("")
        println("Component created successfully in: $targetDir")
        println("Restart Scipio and visit: https://localhost:8443/$webappName")
    }
}

// Create a new shop component in hot-deploy
// Usage: ./gradlew createShopComponent -PcomponentName=myshop -PresourceName=MyShop -PwebappName=myshop
tasks.register("createShopComponent") {
    group = "scipio"
    description = "Create a new shop component in hot-deploy folder"
    doLast {
        val componentName = project.findProperty("componentName")?.toString()
            ?: throw GradleException("componentName is required. Use -PcomponentName=myshop")
        val resourceName = project.findProperty("resourceName")?.toString()
            ?: throw GradleException("resourceName is required. Use -PresourceName=MyShop")
        val webappName = project.findProperty("webappName")?.toString() ?: componentName
        val basePermission = project.findProperty("basePermission")?.toString()
            ?: componentName.uppercase()
        val componentPackage = project.findProperty("componentPackage")?.toString()
            ?: "com.ilscipio.scipio.ce.external"

        val templateDir = file("framework/resources/templates/shop")
        val baseTemplateDir = file("framework/resources/templates")
        val targetDir = file("hot-deploy/$componentName")

        if (targetDir.exists()) {
            throw GradleException("Component already exists: $targetDir")
        }

        println("Creating shop component: $componentName")

        // Create directory structure (same as regular component)
        listOf(
            "", "config", "data", "data/helpdata", "dtd", "documents",
            "entitydef", "lib", "libsrc", "patches", "patches/test",
            "patches/qa", "patches/production", "script", "servicedef",
            "src", "testdef", "webapp", "webapp/$webappName",
            "webapp/$webappName/error", "webapp/$webappName/WEB-INF",
            "webapp/$webappName/WEB-INF/actions", "widget"
        ).forEach { dir ->
            file("$targetDir/$dir").mkdirs()
        }

        // Filter function
        val filterTokens = mapOf(
            "@component-name@" to componentName,
            "@component-resource-name@" to resourceName,
            "@base-permission@" to basePermission,
            "@webapp-name@" to webappName,
            "@component-package@" to componentPackage
        )

        fun copyTemplate(templateBase: File, from: String, to: String) {
            val sourceFile = file("$templateBase/$from")
            if (sourceFile.exists()) {
                val content = sourceFile.readText()
                var filtered = content
                filterTokens.forEach { (token, value) ->
                    filtered = filtered.replace(token, value)
                }
                file("$targetDir/$to").writeText(filtered)
            }
        }

        // Copy shop-specific templates
        copyTemplate(templateDir, "build.gradle.kts", "build.gradle.kts")

        // Copy base templates for common files
        copyTemplate(baseTemplateDir, "scipio-component.xml", "scipio-component.xml")
        copyTemplate(baseTemplateDir, "TypeData.xml", "data/${resourceName}TypeData.xml")
        copyTemplate(baseTemplateDir, "DemoData.xml", "data/${resourceName}DemoData.xml")
        copyTemplate(baseTemplateDir, "entitymodel.xml", "entitydef/entitymodel.xml")
        copyTemplate(baseTemplateDir, "services.xml", "servicedef/services.xml")
        copyTemplate(baseTemplateDir, "UiLabels.xml", "config/${resourceName}UiLabels.xml")
        copyTemplate(baseTemplateDir, "controller.xml", "webapp/$webappName/WEB-INF/controller.xml")
        copyTemplate(baseTemplateDir, "web.xml", "webapp/$webappName/WEB-INF/web.xml")

        println("")
        println("Shop component created successfully in: $targetDir")
        println("Restart Scipio and visit: https://localhost:8443/$webappName")
    }
}

// ============================================================================
// XML TO ANNOTATION CONVERTER
// ============================================================================

// Convert XML widget definitions to Java annotation classes
// Usage:
//   ./gradlew convertXmlToAnnotation -PxmlFile=applications/setup/widget/SetupForms.xml
//   ./gradlew convertXmlToAnnotation -Pcomponent=setup
//   ./gradlew convertXmlToAnnotation -PxmlFile=... -PoutputPackage=com.ilscipio.scipio.setup.widget
//   ./gradlew convertXmlToAnnotation -Pcomponent=setup -PremoveXml=true  // Also remove original XML files
tasks.register<com.ilscipio.scipio.gradle.ConvertXmlToAnnotationTask>("convertXmlToAnnotation") {
    // Task depends on the widget project being compiled first (for converter classes)
    dependsOn(":framework:widget:classes")

    // Configure from gradle properties if provided
    xmlFile = project.findProperty("xmlFile")?.toString()
    component = project.findProperty("component")?.toString()
    outputPackage = project.findProperty("outputPackage")?.toString()
    removeXml = project.findProperty("removeXml")?.toString()
}

// Remove XML files that have been fully migrated to Java annotations
// Usage:
//   ./gradlew removeMigratedXml -Pcomponent=cms
//   ./gradlew removeMigratedXml -Pcomponent=content
//   ./gradlew removeMigratedXml -Pcomponent=cms -PdryRun=true  // Preview only
tasks.register<com.ilscipio.scipio.gradle.RemoveMigratedXmlTask>("removeMigratedXml") {
    // Configure from gradle properties if provided
    component = project.findProperty("component")?.toString()
    dryRun = project.findProperty("dryRun")?.toString()
}

// Package every Agent Skill and the MCP connection config into one Claude Code plugin, then zip it
// Usage:
//   ./gradlew assembleAgentPlugin
tasks.register<com.ilscipio.scipio.gradle.AssembleAgentPluginTask>("assembleAgentPlugin") {
}

// ============================================================================
// IDEA CONFIGURATION
// ============================================================================

idea {
    project {
        jdkName = "11"
        languageLevel = org.gradle.plugins.ide.idea.model.IdeaLanguageLevel("11")
    }
}
