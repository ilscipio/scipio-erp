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
import org.ofbiz.base.util.FileUtil;

/**
 * {@link ReflectQuery} Jar Scanner.
 *
 * <p>SCIPIO: 3.0.0: Enhanced for annotations support.</p>
 */
public class PlatformJarScanner implements FilterJarScanner {
    private static final PlatformJarScanner DEFAULT = new PlatformJarScanner();

    @Override
    public void scanJars(ComponentConfig component, ComponentReflectConfig libScan) {
        ComponentReflectRegistry.registerReflectInfo(component, FileUtil.fileUrls(libScan.getPlatformJars()));
    }

    @Override
    public void scanJars(ComponentConfig.WebappInfo webappInfo, ComponentReflectConfig libScan) {
        WebappReflectRegistry.registerReflectInfo(webappInfo, FileUtil.fileUrls(libScan.getPlatformJars()));
    }

    public static class Factory implements FilterJarScanner.Factory {
        @Override
        public FilterJarScanner makeScanner() {
            return DEFAULT;
        }
    }
}
