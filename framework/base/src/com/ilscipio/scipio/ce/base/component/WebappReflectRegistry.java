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

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Per-webapp reflection and annotation scanner.
 *
 * <p>Abstracts the jar file locations and bundles them using {@link ReflectQuery}.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class WebappReflectRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, WebappReflectInfo> NAME_REGISTRY = new ConcurrentHashMap<>();
    private static final Map<String, WebappReflectInfo> MOUNT_REGISTRY = new ConcurrentHashMap<>();

    public static Collection<WebappReflectInfo> getReflectInfos() {
        return NAME_REGISTRY.values();
    }

    public static WebappReflectInfo getReflectInfo(ComponentConfig.WebappInfo webappInfo) {
        return NAME_REGISTRY.get(webappInfo.getComponentConfig().getGlobalName() + "::" + webappInfo.getName());
    }

    public static WebappReflectInfo getReflectInfoByName(String componentName, String webappName) {
        return NAME_REGISTRY.get(componentName + "::" + webappName);
    }

    public static WebappReflectInfo getReflectInfoByMount(String server, String contextRoot) {
        return MOUNT_REGISTRY.get(server + "::" + contextRoot);
    }

    static WebappReflectInfo registerReflectInfo(ComponentConfig.WebappInfo webappInfo, Collection<URL> jarUrls) {
        String nameKey = webappInfo.getComponentConfig().getGlobalName() + "::" + webappInfo.getName();
        WebappReflectInfo wri = NAME_REGISTRY.get(nameKey);
        if (wri == null) {
            wri = new WebappReflectInfo(webappInfo, jarUrls, true);
            WebappReflectInfo prevWri = NAME_REGISTRY.putIfAbsent(nameKey, wri);
            if (prevWri != null) {
                wri = prevWri;
            } else {
                MOUNT_REGISTRY.put(webappInfo.getServer() + "::" + webappInfo.getContextRoot(), wri);
                // Also register the component part, excluding webapp jars
                // SCIPIO: 3.0.0: Now loaded separately by CatalinaContainer
                //ComponentReflectRegistry.registerComponentReflectInfo(wri, jarUrls);
            }
        }
        return wri;
    }

    /**
     * For an absolute or relative-to-project-root path, URI or URL to a file, if the path points under a webapp,
     * returns a {@link ReflectQuery} for the whole webapp and component jars; if only under component, returns for
     * component jars only.
     */
    public static ReflectQuery getReflectQueryForResource(Object location) {
        Debug.logInfo("getReflectQueryForResource: location=[" + location + "]", module);
        ComponentConfig.WebappInfo webappInfo = ComponentConfig.getWebappInfoFromResource(location, false);
        Debug.logInfo("getReflectQueryForResource: webappInfo=[" + (webappInfo != null ? webappInfo.getName() : "null") + "]", module);
        if (webappInfo != null) {
            WebappReflectInfo wri = WebappReflectRegistry.getReflectInfo(webappInfo);
            Debug.logInfo("getReflectQueryForResource: WebappReflectInfo found=[" + (wri != null) + "]", module);
            if (wri != null) {
                return wri.getReflectQuery();
            } else {
                Debug.logWarning("getReflectQueryForResource: No WebappReflectInfo registered for webapp [" +
                        webappInfo.getComponentConfig().getGlobalName() + "::" + webappInfo.getName() + "]", module);
                Debug.logInfo("getReflectQueryForResource: Registered webapps: " + NAME_REGISTRY.keySet(), module);
            }
        }
        ComponentConfig componentConfig = ComponentConfig.getComponentConfigFromResource(location);
        Debug.logInfo("getReflectQueryForResource: componentConfig=[" + (componentConfig != null ? componentConfig.getComponentName() : "null") + "]", module);
        if (componentConfig != null) {
            ComponentReflectInfo cri = ComponentReflectRegistry.getReflectInfo(componentConfig);
            Debug.logInfo("getReflectQueryForResource: ComponentReflectInfo found=[" + (cri != null) + "]", module);
            if (cri != null) {
                return cri.getReflectQuery();
            }
        }
        Debug.logWarning("getReflectQueryForResource: No ReflectQuery found for location [" + location + "]", module);
        return null;
    }
}
