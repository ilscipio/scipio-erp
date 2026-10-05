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
import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.container.ContainerException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.GeneralException;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;

/**
 * Loads component annotations.
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class ComponentReflectLoader {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static void loadComponentAnnotations() {
        Debug.logInfo("Loading component and webapp platform annotations", module);

        List<Future<Object>> futures = new ArrayList<>();

        for (ComponentConfig component : ComponentConfig.getAllComponents()) {
            futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(() -> {
                try {
                    scanJars(component);
                    return new Object();
                } catch (Exception e) {
                    throw new GeneralException(e.getMessage() + " [component: " + component.getGlobalName() + "]", e);
                }
            }));

            for (ComponentConfig.WebappInfo webappInfo : component.getWebappInfos()) {
                if (webappInfo.hasLocation()) {
                    futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(() -> {
                        try {
                            scanJars(webappInfo);
                            return new Object();
                        } catch (Exception e) {
                            throw new ContainerException(e.getMessage() + " [webapp: " + webappInfo.getName()
                                    + ", component: " + webappInfo.getComponentConfig().getGlobalName() + "]", e);
                        }
                    }));
                }
            }
        }

        ExecutionPool.getAllFutures(futures);
    }

    public static ComponentReflectConfig scanJars(ComponentConfig component) {
        ComponentReflectConfig libScan = new ComponentReflectConfig(ComponentReflectConfig.ScanType.PLATFORM).readScanJars(component);
        Debug.logInfo("[component=" + component.getComponentName() + "]" +
                ": Scanning platform JARs: " + FileUtil.fileNames(libScan.getPlatformJars()), module);
        FilterJarScanner.Registry.getDefault().scanJars(component, libScan);
        return libScan;
    }

    public static ComponentReflectConfig scanJars(ComponentConfig.WebappInfo webappInfo) {
        ComponentReflectConfig libScan = new ComponentReflectConfig(ComponentReflectConfig.ScanType.PLATFORM).readScanJars(webappInfo);
        Debug.logInfo("[component=" + webappInfo.getComponentConfig().getComponentName() +
                ", webapp=" + webappInfo.getName() + "]" +
                ": Scanning platform JARs: " + FileUtil.fileNames(libScan.getPlatformJars()), module);
        FilterJarScanner.Registry.getDefault().scanJars(webappInfo, libScan);
        return libScan;
    }

}
