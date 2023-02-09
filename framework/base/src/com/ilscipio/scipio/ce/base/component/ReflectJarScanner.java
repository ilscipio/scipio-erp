package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.FileUtil;

/**
 * {@link ReflectQuery} Jar Scanner.
 *
 * <p>SCIPIO: 3.0.0: Enhanced for annotations support.</p>
 */
public class ReflectJarScanner implements FilterJarScanner {
    private static final ReflectJarScanner DEFAULT = new ReflectJarScanner();

    @Override
    public void scanJars(ComponentConfig component, ComponentLibScanConfig libScan) {
        ComponentReflectRegistry.registerComponentReflectInfo(component, FileUtil.fileUrls(libScan.getPlatformJars()));
    }

    @Override
    public void scanJars(ComponentConfig.WebappInfo webappInfo, ComponentLibScanConfig libScan) {
        WebappReflectRegistry.registerWebappReflectInfo(webappInfo, FileUtil.fileUrls(libScan.getPlatformJars()));
    }

    public static class Factory implements FilterJarScanner.Factory {
        @Override
        public FilterJarScanner makeScanner() {
            return DEFAULT;
        }
    }
}
