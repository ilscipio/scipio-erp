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
