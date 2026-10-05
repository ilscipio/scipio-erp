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
package org.ofbiz.catalina.container;

import java.util.Set;

import com.ilscipio.scipio.ce.base.component.ComponentReflectConfig;
import org.apache.tomcat.JarScanType;
import org.apache.tomcat.util.scan.StandardJarScanFilter;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;

import javax.servlet.ServletContext;

/**
 * Catalina Container Jar Filter.
 *
 * <p>SCIPIO: 2018-10-02: This class is completely redesigned.</p>
 */
final class FilterJars extends StandardJarScanFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final WebappInfo webappInfo;
    private final Set<String> scanEnabledJarNames;

    public FilterJars(WebappInfo webappInfo, ServletContext context, ComponentReflectConfig libScan) {
        this.webappInfo = webappInfo;
        //this.servletContext = context;
        this.scanEnabledJarNames = FileUtil.fileNames(libScan.getWebserverJars());
    }

    public static FilterJars createFilter(WebappInfo webappInfo, ServletContext context) {
        ComponentReflectConfig libScan = new ComponentReflectConfig(ComponentReflectConfig.ScanType.WEBSERVER).readScanJars(webappInfo);
        Debug.logInfo("[component=" + webappInfo.getComponentConfig().getComponentName() +
                ", webapp=" + webappInfo.getName() + "]" +
                ": Webserver filter JARs: " + FileUtil.fileNames(libScan.getWebserverJars()), module);
        return new FilterJars(webappInfo, context, libScan);
    }

    @Override
    public boolean check(JarScanType jarScanType, String jarName) {
        return scanEnabledJarNames.contains(jarName);
    }

}
