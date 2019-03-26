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
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

/**
 * Catalina Container Jar Filter.
 * <p>
 * SCIPIO: 2018-10-02: This class is completely redesigned.
 */
final class FilterJars implements JarScanFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static Set<String> globalScanEnabledJarNames;
    static {
        Set<String> jarNames = new HashSet<>();
        readGlobalScanEnabledJarNames(jarNames);
        Debug.logInfo("Global server-scan enabled JAR names: " + new TreeSet<>(jarNames), module);
        globalScanEnabledJarNames = jarNames;
    }

    //private final WebappInfo webappInfo;
    private Set<String> scanEnabledJarNames;

    public FilterJars(WebappInfo webappInfo) {
        //this.webappInfo = webappInfo;
        this.scanEnabledJarNames = getCombinedScanEnabledJarNames(webappInfo);
    }

    @Override
    public boolean check(final JarScanType jarScanType, final String jarName) {
        return scanEnabledJarNames.contains(jarName); // SCIPIO: 2018-10-03: Simplified
    }
    
    static Set<String> getCombinedScanEnabledJarNames(WebappInfo webappInfo) {
        Set<String> jarNames = new HashSet<>();
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
