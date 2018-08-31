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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.tomcat.JarScanFilter;
import org.apache.tomcat.JarScanType;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

final class FilterJars implements JarScanFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: static var for this
    private static final boolean webSocketEnabled = UtilProperties.getPropertyAsBoolean("catalina", "webSocket", false);
    // SCIPIO (NOTE: volatile not required - immutable pattern instead)
    private static Set<String> webSocketEnabledJarNames = null;

    @Override
    public boolean check(final JarScanType jarScanType, final String jarName) {
        return webSocketEnabled ? getWebSocketEnabledJarNames().contains(jarName) : false;
    }

    private static Set<String> getWebSocketEnabledJarNames() { // SCIPIO
        Set<String> jarNames = FilterJars.webSocketEnabledJarNames;
        if (jarNames == null) {
            jarNames = Collections.unmodifiableSet(readWebSocketEnabledJarNames());
            FilterJars.webSocketEnabledJarNames = jarNames;
        }
        return jarNames;
    }

    private static Set<String> readWebSocketEnabledJarNames() { // SCIPIO
        Set<String> jarNames = new HashSet<>();
        jarNames.add("ofbiz.jar");
        for(File file : ComponentConfig.readClasspathSpecialJarLocations("websockets")) {
            jarNames.add(file.getName());
        }
        if (Debug.infoOn()) {
            List<String> jarNamesList = new ArrayList<>(jarNames);
            Collections.sort(jarNamesList);
            Debug.logInfo("WebSockets-enabled JAR names: " + jarNamesList, module);
        }
        return jarNames;
    }
}
