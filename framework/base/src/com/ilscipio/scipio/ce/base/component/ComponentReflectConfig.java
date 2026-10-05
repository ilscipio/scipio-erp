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
package com.ilscipio.scipio.ce.base.component;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;

import javax.annotation.concurrent.NotThreadSafe;
import java.io.File;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Component lib/jar scan and annotations config reader/builder - Lists jar files to be scanned by
 * {@link PlatformJarScanner} and the web container.
 *
 * <p>NOTE: Currently does not cache reads (could be done with component/webapp name as keys), which could be useful
 * to pass this around a greater scope.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
@NotThreadSafe
public class ComponentReflectConfig {

    /**
     * Default JAR scan locations - needed to prevent over-scanning of third-party libraries (slowdowns and
     * inappropriate default).
     */
    private static final Set<String> DEFAULT_JAR_SCAN_DIRS = UtilMisc.unmodifiableLinkedHashSet("build/lib");

    public enum ScanType {
        PLATFORM,
        WEBSERVER
    }

    protected Set<String> defaultJarScanDirs;
    protected Collection<File> platformJars;
    protected Collection<File> webserverJars;

    public ComponentReflectConfig(Collection<File> platformJars, Collection<File> webserverJars) {
        this.platformJars = platformJars;
        this.webserverJars = webserverJars;
        this.defaultJarScanDirs = DEFAULT_JAR_SCAN_DIRS;
    }

    public ComponentReflectConfig(ScanType... scanTypes) {
        this(List.of(scanTypes).contains(ScanType.PLATFORM) ? new LinkedHashSet<>() : null,
                List.of(scanTypes).contains(ScanType.WEBSERVER) ? new LinkedHashSet<>() : null);
    }

    public static Set<String> getDefaultJarScanDirs() {
        return DEFAULT_JAR_SCAN_DIRS;
    }

    public Collection<File> getPlatformJars() {
        return platformJars;
    }

    public Collection<File> getWebserverJars() {
        return webserverJars;
    }

    public ComponentReflectConfig readScanJars(ComponentConfig component) {
        readDefaultScanJars(component);
        readExplicitScanJars(component);
        return this;
    }

    public ComponentReflectConfig readScanJars(ComponentConfig.WebappInfo webappInfo) {
        readDefaultScanJars(webappInfo);
        readExplicitScanJars(webappInfo);
        return this;
    }

    public ComponentReflectConfig readDefaultScanJars(ComponentConfig component) {
        readComponentLibScanJars(component);
        return this;
    }

    public ComponentReflectConfig readDefaultScanJars(ComponentConfig.WebappInfo webappInfo) {
        // WARN: This scan all component-level sources for each webapp, so definitions will be shared between webapps
        //  which must be understood by the developer. So to make definitions webapp-specific, either deploy libs to
        //  WEB-INF/lib using build script or create single-webapp components.
        readComponentLibScanJars(webappInfo.getComponentConfig());
        readWebappLibScanJars(webappInfo);
        return this;
    }

    /**
     * @deprecated This was used to emulate upstream's ofbiz.jar scanning behavior where jars for all components were
     *  scanned by the container for each webapp (even though here only explicitly marked ones were read), but this
     *  didn't really hold up since @ServerEndpoint's declared on component level classpath would clash and be read by
     *  all webapps .
     */
    @Deprecated
    public ComponentReflectConfig readDefaultScanJars() {
        // Here ofbiz.jar would have been loaded if it amalgamated all build libs like upstream, but even then we don't
        //  really want it here (design flaw)
        //jarFiles.add(..."ofbiz.jar")
        return this;
    }

    public ComponentReflectConfig readExplicitScanJars(ComponentConfig component) {
        Collection<File> jarFiles;

        if (UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false)) { // deprecated
            // SCIPIO: 2018-10-02: This should not be needed in our current setup,
            // will only slow loading down.
            //jarNames.add("ofbiz.jar");
            jarFiles = ComponentConfig.readClasspathSpecialJarLocations(component,"websockets");
            UtilMisc.addAll(webserverJars, jarFiles);
        }

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(component,"server-scan"); // deprecated
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(component,"annotations");
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(component,"webserver-annotations");
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(component,"platform-annotations");
        UtilMisc.addAll(platformJars, jarFiles);

        return this;
    }

    public ComponentReflectConfig readExplicitScanJars(ComponentConfig.WebappInfo webappInfo) {
        Collection<File> jarFiles;

        if (UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false)) { // deprecated
            // SCIPIO: 2018-10-02: This should not be needed in our current setup,
            // will only slow loading down.
            //jarNames.add("ofbiz.jar");
            jarFiles = ComponentConfig.readClasspathSpecialJarLocations(webappInfo.getComponentConfig(),
                    "websockets", webappInfo.getName());
            UtilMisc.addAll(webserverJars, jarFiles);
        }

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(webappInfo.getComponentConfig(),
                "server-scan", webappInfo.getName());
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(webappInfo.getComponentConfig(),
                "annotations", webappInfo.getName());
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(webappInfo.getComponentConfig(),
                "webserver-annotations", webappInfo.getName());
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations(webappInfo.getComponentConfig(),
                "platform-annotations", webappInfo.getName());
        UtilMisc.addAll(platformJars, jarFiles);

        return this;
    }

    public ComponentReflectConfig readExplicitScanJars() {
        Collection<File> jarFiles;

        if (UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false)) { // deprecated
            // SCIPIO: 2018-10-02: This should not be needed in our current setup,
            // will only slow loading down.
            //jarNames.add("ofbiz.jar");
            jarFiles = ComponentConfig.readClasspathSpecialJarLocations("websockets");
            UtilMisc.addAll(webserverJars, jarFiles);
        }

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations("server-scan"); // deprecated
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations("annotations");
        UtilMisc.addAll(platformJars, jarFiles);
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations("webserver-annotations");
        UtilMisc.addAll(webserverJars, jarFiles);

        jarFiles = ComponentConfig.readClasspathSpecialJarLocations("platform-annotations");
        UtilMisc.addAll(platformJars, jarFiles);

        return this;
    }

    public ComponentReflectConfig readComponentLibScanJars(ComponentConfig component) {
        String configRoot = component.getRootLocation();
        Set<String> scannedDirs = new LinkedHashSet<>();

        // SCIPIO: 4.0.0: First, automatically scan all default JAR directories (build/lib) even without
        // explicit classpath entries. This is convention-over-configuration for Gradle builds.
        Set<String> defaultJarScanDirs = getDefaultJarScanDirs();
        if (defaultJarScanDirs != null) {
            for (String defaultDir : defaultJarScanDirs) {
                scanJarDirectory(configRoot, defaultDir);
                scannedDirs.add(defaultDir);
            }
        }

        // Also scan any explicitly configured classpath entries that match default scan dirs
        // (this maintains backward compatibility with existing scipio-component.xml configurations)
        for (ComponentConfig.ClasspathInfo info : component.getClasspathInfos()) {
            if (!"jar".equals(info.getType())) {
                continue;
            }
            String location = info.getLocation();
            if (location.startsWith("/")) {
                location = location.substring(1);
            }
            String dirLoc = location;
            if (dirLoc.endsWith("/*")) {
                dirLoc = location.substring(0, location.length() - 2);
            }
            // Skip if already scanned via default dirs or not in default scan dirs
            if (scannedDirs.contains(dirLoc)) {
                continue;
            }
            if (defaultJarScanDirs != null && !defaultJarScanDirs.contains(dirLoc)) {
                continue;
            }
            scanJarDirectory(configRoot, dirLoc);
            scannedDirs.add(dirLoc);
        }
        return this;
    }

    /**
     * Scans a directory for JAR files and adds them to the platform and webserver JAR collections.
     *
     * <p>SCIPIO: 4.0.0: Extracted from readComponentLibScanJars for reuse.</p>
     */
    private void scanJarDirectory(String configRoot, String dirLoc) {
        File path = new File(configRoot, dirLoc);
        if (path.exists()) {
            if (path.isDirectory()) {
                File[] files = path.listFiles();
                if (files != null) {
                    for (File file : files) {
                        if (file.getName().toLowerCase().endsWith(".jar")) {
                            UtilMisc.add(platformJars, file);
                            UtilMisc.add(webserverJars, file);
                        }
                    }
                }
            } else {
                UtilMisc.add(platformJars, path);
                UtilMisc.add(webserverJars, path);
            }
        }
    }

    public ComponentReflectConfig readWebappLibScanJars(ComponentConfig.WebappInfo webappInfo) {
        File path = new File(webappInfo.getLocation(), "WEB-INF/lib");
        if (path.exists() && path.isDirectory()) {
            File[] files = path.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.getName().toLowerCase().endsWith(".jar")) {
                        // TODO: REVIEW: For now, do not scan WEB-INF/lib for platform because 1) they can contain tons of 3rd-party libraries,
                        //  2) they can be specified manually using classpath-special
                        //UtilMisc.add(platformJars, file);
                        UtilMisc.add(webserverJars, file);
                    }
                }
            }
        }
        return this;
    }

}
