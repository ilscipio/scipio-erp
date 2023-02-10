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
