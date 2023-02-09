package com.ilscipio.scipio.ce.base.component;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Callback interface for manual JAR scanners.
 *
 * <p>SCIPIO: 3.0.0: Enhanced for annotations support.</p>
 */
public interface FilterJarScanner {

    void scanJars(ComponentConfig component, ComponentLibScanConfig libScan);

    void scanJars(ComponentConfig.WebappInfo webappInfo, ComponentLibScanConfig libScan);

    interface Factory {
        FilterJarScanner makeScanner();
    }

    class Registry {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        private static final Registry DEFAULT = new Registry();

        private final Map<String, Factory> factoryMap;

        public Registry() {
            this.factoryMap = readFactories();
        }

        public static Registry getDefault() {
            return DEFAULT;
        }

        private Map<String, Factory> readFactories() {
            Map<String, Factory> factoryMap = new HashMap<>();
            Map<String, Map<String, String>> configs = new LinkedHashMap<>();
            UtilProperties.extractPropertiesWithPrefixAndId(configs, UtilProperties.getProperties("catalina"), "filterJarsScanner.");
            for (Map.Entry<String, Map<String, String>> entry : configs.entrySet()) {
                String factoryClsName = entry.getValue().get("factoryClass");
                if (UtilValidate.isNotEmpty(factoryClsName)) {
                    try {
                        Class<? extends Factory> factoryCls = UtilGenerics.cast(Thread.currentThread().getContextClassLoader().loadClass(factoryClsName));
                        Factory factory = factoryCls.getConstructor().newInstance();
                        factoryMap.put(entry.getKey(), factory);
                    } catch(Exception e) {
                        Debug.logError("Could not load factory [" + factoryClsName + "] for filterJarsScanner [" + entry.getKey() + "]", module);
                    }
                }
            }
            Debug.logInfo("Read filterJarsScanner factories: " + factoryMap, module);
            return factoryMap;
        }

        public void scanJars(ComponentConfig component, ComponentLibScanConfig libScan) {
            for(Map.Entry<String, Factory> entry : factoryMap.entrySet()) {
                entry.getValue().makeScanner().scanJars(component, libScan);
            }
        }

        public void scanJars(ComponentConfig.WebappInfo webappInfo, ComponentLibScanConfig libScan) {
            for(Map.Entry<String, Factory> entry : factoryMap.entrySet()) {
                entry.getValue().makeScanner().scanJars(webappInfo, libScan);
            }
        }
    }
}
