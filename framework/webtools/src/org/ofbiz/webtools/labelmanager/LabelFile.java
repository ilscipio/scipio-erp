/*
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
 */
package org.ofbiz.webtools.labelmanager;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * LabelFile.
 * SCIPIO: For a local thread instance with delegator caching, call {@link #toLocal()}.
 */
public class LabelFile {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final boolean fileLoaded = false;
    protected final File file;
    protected final String componentName;
    protected Map<String, Map<String, String>> staticPropertyMap = null; // SCIPIO

    protected LabelFile(File file, String componentName) {
        this.file = file;
        this.componentName = componentName;
    }

    public File getFile() {
        return this.file;
    }

    public String getFileName() {
        return this.file.getName();
    }

    public String getFilePath() {
        return this.file.getPath();
    }

    public String getBaseName() {
        String name = getFileName();
        int lastDot = name.lastIndexOf('.');
        if (lastDot > 0) {
            name = name.substring(0, lastDot);
        }
        return name;
    }

    public String getComponentName() { // SCIPIO (missing)
        return componentName;
    }

    public Map<String, Map<String, String>> getStaticPropertyMap() {
        // FIXME: this should now use UtilProperties.ResourceBundleProperties.from()
        Map<String, Map<String, String>> staticPropertyMap = this.staticPropertyMap;
        if (staticPropertyMap == null) {
            try {
                staticPropertyMap = UtilProperties.xmlToLocalePropertyMap(getFile().toURI().toURL(), true, new TreeMap<>());
            } catch (Exception e) {
                Debug.logError("Could not get locale property map for file [" + getFile() + "]: " + e.toString(), module);
            }
            if (UtilValidate.isEmpty(staticPropertyMap)) {
                staticPropertyMap = Collections.emptyMap();
            }
            this.staticPropertyMap = staticPropertyMap;
        }
        return staticPropertyMap;
    }

    public Map<String, String> getStaticPropertyValues(String propertyName) {
        return getStaticPropertyMap().get(propertyName);
    }

    public Map<String, Map<String, String>> getEntityPropertyMap() {
        // FIXME: LabelFile ends up in a static var so this doesn't work
        //Map<String, Map<String, String>> entityPropertyMap = this.entityPropertyMap;
        //if (entityPropertyMap == null) {
        Map<String, Map<String, String>> entityPropertyMap = null;
        try {
            // FIXME: null delegator (default)
            entityPropertyMap = UtilProperties.entityResourceToLocalePropertyMap(getBaseName(), true, null, false, new TreeMap<>());
        } catch (Exception e) {
            Debug.logError("Could not get locale property map for file [" + getFile() + "]: " + e.toString(), module);
        }
        if (UtilValidate.isEmpty(entityPropertyMap)) {
            entityPropertyMap = Collections.emptyMap();
        }
        //this.entityPropertyMap = entityPropertyMap;
        //}
        return entityPropertyMap;
    }

    public Map<String, String> getEntityPropertyValues(String propertyName) {
        return getEntityPropertyMap().get(propertyName);
    }

    public List<String> getAllPropertyNames() {
        //List<String> allPropertyNames = this.allPropertyNames;
        //if (allPropertyNames == null) {
        Set<String> allPropertyNamesSet = new TreeSet<>(getStaticPropertyMap().keySet());
        allPropertyNamesSet.addAll(getEntityPropertyMap().keySet());
        List<String> allPropertyNames = Collections.unmodifiableList(new ArrayList<>(allPropertyNamesSet));
        //this.allPropertyNames = allPropertyNames;
        //}
        return allPropertyNames;
    }

    public LocalLabelFile toLocal() { // SCIPIO
        return new LocalLabelFile(this);
    }

    /**
     * Buffered entity version of LabelFile (SCIPIO).
     */
    public static class LocalLabelFile extends LabelFile {

        protected Map<String, Map<String, String>> entityPropertyMap = null; // SCIPIO: can't cache
        protected List<String> allPropertyNames = null;

        public LocalLabelFile(LabelFile labelFile) {
            super(labelFile.getFile(), labelFile.getComponentName());
        }

        @Override
        public Map<String, Map<String, String>> getEntityPropertyMap() {
            Map<String, Map<String, String>> entityPropertyMap = this.entityPropertyMap;
            if (entityPropertyMap == null) {
                entityPropertyMap = super.getEntityPropertyMap();
                this.entityPropertyMap = entityPropertyMap;
            }
            return entityPropertyMap;
        }

        @Override
        public List<String> getAllPropertyNames() {
            List<String> allPropertyNames = this.allPropertyNames;
            if (allPropertyNames == null) {
                allPropertyNames = super.getAllPropertyNames();
                this.allPropertyNames = allPropertyNames;
            }
            return allPropertyNames;
        }
    }
}
