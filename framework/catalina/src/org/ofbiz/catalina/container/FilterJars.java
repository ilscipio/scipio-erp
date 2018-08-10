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
