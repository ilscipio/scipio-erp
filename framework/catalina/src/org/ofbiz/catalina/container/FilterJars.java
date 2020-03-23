/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.catalina.container;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.apache.tomcat.JarScanFilter;
import org.apache.tomcat.JarScanType;
import org.apache.tomcat.util.scan.StandardJarScanFilter;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.reflections.Reflections;

import javax.servlet.ServletContext;
import javax.websocket.DeploymentException;
import javax.websocket.server.ServerContainer;

/**
 * Catalina Container Jar Filter.
 * <p>
 * SCIPIO: 2018-10-02: This class is completely redesigned.
 */
final class FilterJars extends StandardJarScanFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static Set<String> globalScanEnabledJarNames;
    private final WebappInfo webappInfo;
    private static ServletContext servletContext = null;

    static {
        Set<String> jarNames = new HashSet<>();
        readGlobalScanEnabledJarNames(jarNames);
        Debug.logInfo("Global server-scan enabled JAR names: " + new TreeSet<>(jarNames), module);
        globalScanEnabledJarNames = jarNames;
    }

    //private final WebappInfo webappInfo;
    private Set<String> scanEnabledJarNames;

    public FilterJars(WebappInfo webappInfo, ServletContext context) {
        super();
        this.webappInfo = webappInfo;
        this.servletContext = context;
        this.scanEnabledJarNames = getCombinedScanEnabledJarNames(webappInfo);
    }

    @Override
    public boolean check(final JarScanType jarScanType, final String jarName) {
        return scanEnabledJarNames.contains(jarName); // SCIPIO: 2018-10-03: Simplified
    }
    
    static Set<String> getCombinedScanEnabledJarNames(WebappInfo webappInfo) {
        Set<String> jarNames = new HashSet<>();

        // SCIPIO: 2019-12-21: Added scanning of base jars
        String configRoot = webappInfo.componentConfig.getRootLocation();
        configRoot = configRoot.replace('\\', '/');
        for(ComponentConfig.ClasspathInfo info : webappInfo.componentConfig.getClasspathInfos()){
            if("jar".equals(info.type)){
                String location = info.location.replace('\\', '/');
                if (location.startsWith("/")) {
                    location = location.substring(1);
                }
                String dirLoc = location;
                if (dirLoc.endsWith("/*")) {
                    // strip off the slash splat
                    dirLoc = location.substring(0, location.length() - 2);
                }
                String fileNameSeparator = ("\\".equals(File.separator) ? "\\" + File.separator : File.separator);
                dirLoc = dirLoc.replaceAll("/+|\\\\+", fileNameSeparator);
                File path = new File(configRoot, dirLoc);
                if (path.exists()) {
                    if (path.isDirectory()) {
                        for (File file: path.listFiles()) {
                            String fileName = file.getName().toLowerCase();
                            if (fileName.endsWith(".jar")) {
                                jarNames.add(file.getName());
                            }
                        }
                    } else {
                        jarNames.add(path.getName());
                    }
                }
            }
        }

        readWebappScanEnabledJarNames(jarNames, webappInfo);
        if (jarNames.isEmpty()) {
            return globalScanEnabledJarNames;
        }
        Debug.logInfo("Webapp-specific server-scan enabled JAR names for " 
                + webappInfo + ": " + new TreeSet<>(jarNames), module);
        jarNames.addAll(globalScanEnabledJarNames);
        return jarNames;
    }

    static void readGlobalScanEnabledJarNames(Set<String> jarNames) {
        if (UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false)) {
            // SCIPIO: 2018-10-02: This should not be needed in our current setup, 
            // will only slow loading down.
            //jarNames.add("ofbiz.jar");
            for(File file : ComponentConfig.readClasspathSpecialJarLocations("websockets")) {
                jarNames.add(file.getName());
            }


            /*
            https://stackoverflow.com/questions/20127800/mapping-websocketendpoints-in-a-web-xml-file

            Reflections reflections = new Reflections("org.home.junk");
            Set<Class<?>> annotated = reflections.getTypesAnnotatedWith(javax.annotation.);

            // Get a reference to the ServerContainer
            javax.websocket.server.ServerContainer ServerContainer =
                    (javax.websocket.server.ServerContainer)
                            servletContext.getAttribute("javax.websocket.server.ServerContainer");
            // Add endpoint manually to server container
            serverContainer.addEndpoint(game.WebSocketEndpoint.class);
             */
        }

        for(File file : ComponentConfig.readClasspathSpecialJarLocations("server-scan")) {
            jarNames.add(file.getName());
        }
    }

    static void readWebappScanEnabledJarNames(Set<String> jarNames, WebappInfo webappInfo) {
        if (UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false)) {
            // SCIPIO: 2018-10-02: This should not be needed in our current setup, 
            // will only slow loading down.
            //jarNames.add("ofbiz.jar");
            for(File file : ComponentConfig.readClasspathSpecialJarLocations(webappInfo.componentConfig,
                    "websockets", webappInfo.getName())) {
                jarNames.add(file.getName());
            }
        }
        for(File file : ComponentConfig.readClasspathSpecialJarLocations(webappInfo.componentConfig,
                "server-scan", webappInfo.getName())) {
            jarNames.add(file.getName());
        }
    }
}
