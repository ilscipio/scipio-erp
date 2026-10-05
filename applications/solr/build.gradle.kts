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
 * Scipio ERP - Solr Component
 *
 * Apache Solr integration for full-text search
 * and advanced search functionality.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("solr")
    globalName.set("solr")
}

dependencies {
    api(project(":framework:base"))
    api(project(":framework:entity"))
    api(project(":framework:security"))
    api(project(":framework:service"))
    api(project(":framework:widget"))
    api(project(":framework:webapp"))
    api(project(":framework:common"))
    api(project(":applications:content"))
    api(project(":applications:product"))

    // Solr client
    api(libs.bundles.solr)

    // Testing
    testImplementation(libs.bundles.testing)
}

// SCIPIO: 4.0.0: the embedded Solr server webapp loads its own jars from webapp/WEB-INF/lib (ivy filled that
// directory before); they are mirrored there, so a fresh checkout has a working Solr. The set is the curated
// one of the former ivy.xml (the solr-7.x server/solr-webapp jars plus Scipio security upgrades), resolved
// WITHOUT transitive dependencies: solr-core's POM would add Jetty, a servlet API, slf4j 1.7 and log4j-core
// 2.11.0 (Log4Shell, CVE-2021-44228), and take Jackson and HttpClient back to the vulnerable versions.
// Solr uses the server's slf4j and log4j jars. The directory is git-ignored; only its .gitignore is kept.
val solrWebapp: Configuration by configurations.creating {
    isCanBeConsumed = false
    isCanBeResolved = true
    isTransitive = false
}
repositories {
    // org.restlet.jee is not on Maven Central
    maven { url = uri("https://maven.restlet.talend.com") }
}
dependencies {
    val solrServer = libs.solr.core.get().version
    solrWebapp(libs.solr.core)
    solrWebapp("org.apache.solr:solr-solrj:$solrServer")
    for (m in listOf("analyzers-common", "analyzers-kuromoji", "analyzers-nori", "analyzers-phonetic",
            "backward-codecs", "classification", "codecs", "core", "expressions", "grouping", "highlighter", "join",
            "memory", "misc", "queries", "queryparser", "sandbox", "spatial3d", "spatial-extras", "suggest")) {
        solrWebapp("org.apache.lucene:lucene-$m:$solrServer")
    }
    for (c in listOf(
        "org.antlr:antlr4-runtime:4.5.1-1",
        "org.ow2.asm:asm:5.1",
        "org.ow2.asm:asm-commons:5.1",
        "org.apache.calcite.avatica:avatica-core:1.10.0",
        "com.github.ben-manes.caffeine:caffeine:2.4.0",
        "org.apache.calcite:calcite-core:1.13.0",
        "org.apache.calcite:calcite-linq4j:1.13.0",
        "commons-cli:commons-cli:1.2",
        "commons-codec:commons-codec:1.11",
        "commons-collections:commons-collections:3.2.2",
        "org.codehaus.janino:commons-compiler:2.7.6",
        "commons-configuration:commons-configuration:1.6",
        "org.apache.commons:commons-exec:1.3",
        "commons-fileupload:commons-fileupload:1.3.3",
        "commons-io:commons-io:2.5",
        "commons-lang:commons-lang:2.6",
        "org.apache.commons:commons-lang3:3.6",
        "org.apache.commons:commons-math3:3.6.1",
        "org.apache.curator:curator-client:2.8.0",
        "org.apache.curator:curator-framework:2.8.0",
        "org.apache.curator:curator-recipes:2.8.0",
        "com.lmax:disruptor:3.4.0",
        "dom4j:dom4j:1.6.1",
        "net.hydromatic:eigenbase-properties:1.1.5",
        "com.google.guava:guava:14.0.1",
        "org.apache.hadoop:hadoop-annotations:2.7.4",
        "org.apache.hadoop:hadoop-auth:2.7.4",
        "org.apache.hadoop:hadoop-common:2.7.4",
        "org.apache.hadoop:hadoop-hdfs:2.7.4",
        "com.carrotsearch:hppc:0.8.1",
        "org.apache.htrace:htrace-core:3.2.0-incubating",
        // SECURITY: newer than the Solr 7.7.3 distribution (4.5.6/4.4.10)
        "org.apache.httpcomponents:httpclient:4.5.14",
        "org.apache.httpcomponents:httpcore:4.4.16",
        "org.apache.httpcomponents:httpmime:4.5.14",
        // SECURITY: newer than the Solr 7.7.3 distribution (2.9.8, jackson-databind deserialization CVEs)
        "com.fasterxml.jackson.core:jackson-annotations:2.16.0",
        "com.fasterxml.jackson.core:jackson-core:2.16.0",
        "com.fasterxml.jackson.core:jackson-databind:2.16.0",
        "com.fasterxml.jackson.dataformat:jackson-dataformat-smile:2.16.0",
        "org.codehaus.jackson:jackson-core-asl:1.9.13",
        "org.codehaus.jackson:jackson-mapper-asl:1.9.13",
        "org.codehaus.janino:janino:2.7.6",
        "joda-time:joda-time:2.2",
        "org.noggit:noggit:0.8",
        "org.restlet.jee:org.restlet:2.3.0",
        "org.restlet.jee:org.restlet.ext.servlet:2.3.0",
        "com.google.protobuf:protobuf-java:3.1.0",
        "org.rrd4j:rrd4j:3.2",
        "org.locationtech.spatial4j:spatial4j:0.7",
        "org.codehaus.woodstox:stax2-api:3.1.4",
        "com.tdunning:t-digest:3.1",
        "org.codehaus.woodstox:woodstox-core-asl:4.4.1",
        "org.apache.zookeeper:zookeeper:3.4.14")) {
        solrWebapp(c)
    }
}

// SCIPIO: 4.0.0: the Solr security plugins (ScipioUserLoginAuthPlugin, ScipioRuleBasedAuthorizationPlugin) run
// inside the Solr webapp and extend solr-core classes, so they stay out of the component jar. As in the Ant
// build, they are compiled against the webapp jars into scipio-solr-plugins.jar in WEB-INF/lib; an
// applications/solr/security.json turns them on (see README.txt and security_scipiouserlogin_disabled.json).
sourceSets {
    main {
        java {
            exclude("**/plugin/**/*.java")
        }
    }
    create("solrPlugins") {
        java {
            setSrcDirs(listOf("src"))
            include("com/ilscipio/scipio/solr/plugin/**")
            // outside build/classes, which is the main output and goes into the component jar whole
            destinationDirectory.set(layout.buildDirectory.dir("solr-plugins/classes"))
        }
        resources.setSrcDirs(emptyList<String>())
        // the webapp jars first: they carry solrj 7.7.3, the component compiles against solrj 7.5.0
        compileClasspath = solrWebapp + sourceSets["main"].compileClasspath
    }
}
val solrPluginsJar by tasks.registering(Jar::class) {
    description = "Package the Solr security plugins for the Solr webapp"
    from(sourceSets["solrPlugins"].output)
    archiveFileName.set("scipio-solr-plugins.jar")
    destinationDirectory.set(layout.buildDirectory.dir("solr-plugins"))
}
val syncSolrWebappLibs by tasks.registering(Sync::class) {
    group = "scipio"
    description = "Mirror the Solr server jars and the Scipio Solr plugins into webapp/WEB-INF/lib"
    from(solrWebapp) { include("*.jar") }
    from(solrPluginsJar)
    into(layout.projectDirectory.dir("webapp/WEB-INF/lib"))
    preserve { include(".gitignore") }
}
// The root build runs this before every server JavaExec task (start, loadDemo, ...), next to syncLibs.
