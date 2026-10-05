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
 * Scipio ERP - Base Component
 *
 * Core foundation component providing base utilities, configuration,
 * and common functionality used by all other components.
 */

plugins {
    id("scipio-component")
}

scipioComponent {
    componentName.set("base")
    globalName.set("base")
}

// Include start component sources due to tight coupling (circular dependency)
// In Ant build, all classes are on the same classpath - this replicates that behavior
afterEvaluate {
    sourceSets {
        main {
            java {
                srcDirs("src", "../start/src")
            }
        }
    }
}

dependencies {
    // SCIPIO: L-06b: test classes in src (legacy layout) compile against JUnit 4; the runtime jar comes with :framework:testtools
    compileOnly(libs.junit)

    // Servlet API
    // SCIPIO: L-06b: the servlet and JSP API from Tomcat, not javax.servlet-api and javax.servlet.jsp-api (CDDL)
    api(libs.tomcat.servlet.api)
    api(libs.tomcat.jsp.api)
    api(libs.tomcat.el.api) // SCIPIO: 4.0.0: not javax.el:javax.el-api (see the servlet-api bundle in the catalog)
    api(libs.annotation.api)
    api(libs.json.api)
    api(libs.json.impl)

    // Apache Commons
    api(libs.bundles.commons)

    // Logging
    api(libs.bundles.logging)
    api(libs.sentry.log4j2)

    // XML Processing
    api(libs.bundles.xml.processing)

    // Template Engines
    api(libs.freemarker)

    // Scripting
    api(libs.bundles.groovy)
    api(libs.bsf)

    // JSON
    api(libs.bundles.jackson)
    api(libs.gson)

    // HTTP Client
    api(libs.bundles.httpclient)

    // Security
    api(libs.bundles.security)

    // XML Graphics / Barcode
    api(libs.bundles.xml.graphics)

    // Image Processing
    api(libs.bundles.image.processing)

    // Utilities
    api(libs.guava)
    api(libs.jsr305)
    api(libs.icu4j)
    api(libs.javolution)
    api(libs.juel.impl)
    api(libs.juel.spi)
    api(libs.xstream)
    api(libs.joda.time)
    api(libs.avalon.framework.impl)
    api(libs.avalon.framework.api)
    api(libs.juniversalchardet)
    api(libs.concurrentlinkedhashmap)
    api(libs.ical4j)
    // SCIPIO: L-06b: no optional dom4j dependencies (JAXB, StAX, activation, xpp3, pull-parser: CDDL, xpp; the JDK has StAX)
    api(libs.dom4j) {
        exclude(group = "javax.xml.bind", module = "jaxb-api")
        exclude(group = "javax.xml.stream", module = "stax-api")
        exclude(group = "javax.activation", module = "javax.activation-api")
        exclude(group = "xpp3", module = "xpp3")
        exclude(group = "pull-parser", module = "pull-parser")
    }
    api(libs.jdom)
    api(libs.nekohtml)
    api(libs.ez.vcard)
    api(libs.reflections)
    api(libs.reflections8)
    api(libs.libphonenumber)
    api(libs.zxing.core)

    // Websocket
    api(libs.tomcat.websocket.api)

    // Tomcat JNI (for process signals)
    api(libs.tomcat.jni)

    // Apache Tika
    api(libs.bundles.tika)

    // Mail
    api(libs.jakarta.mail)

    // OkHttp
    api(libs.okhttp)
    api(libs.okhttp.logging)

    // SSH
    api(libs.bundles.sshd)

    // Geronimo specs
    api(libs.bundles.geronimo)

    // JAXB (JDK 11+)
    api(libs.bundles.jaxb)

    // Database connection pooling (also used in base for error logging)
    api(libs.commons.dbcp2)

    // Cobertura (optional - for code coverage instrumentation)
    compileOnly("net.sourceforge.cobertura:cobertura:2.1.1")

    // Hamcrest (required for test classes in main source - legacy layout)
    api(libs.hamcrest.all)

    // Testing
    testImplementation(libs.bundles.testing)

    // Nashorn for JDK 15+ (conditional)
    if (JavaVersion.current() >= JavaVersion.VERSION_15) {
        runtimeOnly(libs.nashorn.core)
    }
}
