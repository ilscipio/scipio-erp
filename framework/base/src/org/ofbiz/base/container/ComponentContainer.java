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
package org.ofbiz.base.container;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.component.AlreadyLoadedException;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.component.ComponentLoaderConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.ce.build.util.DependencyGraph;

/**
 * ComponentContainer - StartupContainer implementation for Components
 * <p/>
 * Example ofbiz-container.xml configuration:
 * <pre>
 *   <container name="component-container" class="org.ofbiz.base.component.ComponentContainer"/>
 * </pre>
 */
public class ComponentContainer implements Container {

    public static final String module = ComponentContainer.class.getName();

    protected String configFileLocation = null;
    private String name;
    private boolean loaded = false;

    @Override
    public void init(String[] args, String name, String configFile) throws ContainerException {
        this.name = name;
        this.configFileLocation = configFile;

        // get the config for this container
        ContainerConfig.Container cc = ContainerConfig.getContainer(name, configFileLocation);

        // check for an override loader config
        String loaderConfig = null;
        if (cc.getProperty("loader-config") != null) {
            loaderConfig = cc.getProperty("loader-config").value;
        }

        // load the components
        try {
            loadComponents(loaderConfig);
        } catch (AlreadyLoadedException e) {
            throw new ContainerException(e);
        } catch (ComponentException e) {
            throw new ContainerException(e);
        }
    }

    /**
     * @see org.ofbiz.base.container.Container#start()
     */
    public boolean start() throws ContainerException {
        return true;
    }

    public synchronized void loadComponents(String loaderConfig) throws AlreadyLoadedException, ComponentException {
        // set the loaded list; and fail if already loaded
        if (!loaded) {
            loaded = true;
        } else {
            throw new AlreadyLoadedException("Components already loaded, cannot start");
        }

        // get the components to load
        List<ComponentLoaderConfig.ComponentDef> components = ComponentLoaderConfig.getRootComponents(loaderConfig);

        String parentPath;
        try {
            parentPath = FileUtil.getFile(System.getProperty("ofbiz.home")).getCanonicalFile().toString().replaceAll("\\\\", "/");
        } catch (MalformedURLException e) {
            throw new ComponentException(e.getMessage(), e);
        } catch (IOException e) {
            throw new ComponentException(e.getMessage(), e);
        }
        // load each component
        List<ComponentConfig> componentList = new ArrayList<>(); // SCIPIO: 2017-01-16: explicit list, for ordering and post-load modification purposes
        if (components != null) {
            for (ComponentLoaderConfig.ComponentDef def: components) {
                this.loadComponentFromConfig(parentPath, def, true, componentList); // SCIPIO: always considering top-levels explicit (for now)
            }
        }
        
        Debug.logInfo("All components loaded (" + componentList.size() + ")", module); // SCIPIO: count
        
        // SCIPIO: re-store the component definitions, in order to apply any load order or other modifications we may have made
        // FIXME: in theory we should only clear the ones we loaded (the sizes could differ), in case multiple loaders are involved... but likely never happen
        int allComponentsSize = ComponentConfig.getAllComponents().size();
        if (componentList.size() != allComponentsSize) {
            Debug.logError("Scipio: The number of components loaded by our container (" +  componentList.size() 
                + ") is different from the number of components that were registered in the system (" + allComponentsSize + 
                "); this is either an unsupported configuration or an error", module);
        }
        
        // SCIPIO: 2017-01-19: Post-process loaded components
        componentList = ComponentConfig.postProcessComponentConfigs(componentList);
        
        Debug.logInfo("All components post-processed (" + componentList.size() + " final)", module); // SCIPIO
        
        // SCIPIO: Replace the globally-cached component definitions with our revamped list
        ComponentConfig.clearStoreComponents(componentList);
    }

    // SCIPIO: augmented with explict componentList and explicitOrder
    private void loadComponentFromConfig(String parentPath, ComponentLoaderConfig.ComponentDef def, boolean explicitOrder, List<ComponentConfig> componentList) {
        String location;
        if (def.location.startsWith("/")) {
            location = def.location;
        } else {
            location = parentPath + "/" + def.location;
        }
        if (def.type == ComponentLoaderConfig.SINGLE_COMPONENT) {
            ComponentConfig config = null;
            try {
                config = ComponentConfig.getComponentConfig(def.name, location);
                if (UtilValidate.isEmpty(def.name)) {
                    def.name = config.getGlobalName();
                }
            } catch (ComponentException e) {
                Debug.logError("Cannot load component : " + def.name + " @ " + def.location + " : " + e.getMessage(), module);
            }
            if (config == null) {
                Debug.logError("Cannot load component : " + def.name + " @ " + def.location, module);
            } else {
                this.loadComponent(config);
            }
            // SCIPIO
            componentList.add(config);
        } else if (def.type == ComponentLoaderConfig.COMPONENT_DIRECTORY) {
            this.loadComponentDirectory(location, explicitOrder, componentList);
        }
    }

    private void loadComponentDirectory(String directoryName, boolean explicitOrder, List<ComponentConfig> componentList) {
        Debug.logInfo("Auto-Loading component directory : [" + directoryName + "]", module);
        File parentPath = FileUtil.getFile(directoryName);
        if (!parentPath.exists() || !parentPath.isDirectory()) {
            Debug.logError("Auto-Load Component directory not found : " + directoryName, module);
        } else {
            File componentLoadConfig = new File(parentPath, "component-load.xml");
            if (componentLoadConfig != null && componentLoadConfig.exists()) {
                URL configUrl = null;
                try {
                    configUrl = componentLoadConfig.toURI().toURL();
                    List<ComponentLoaderConfig.ComponentDef> componentsToLoad = ComponentLoaderConfig.getComponentsFromConfig(configUrl);
                    if (componentsToLoad != null) {
                        for (ComponentLoaderConfig.ComponentDef def: componentsToLoad) {
                            this.loadComponentFromConfig(parentPath.toString(), def, true, componentList);
                        }
                    }
                } catch (MalformedURLException e) {
                    Debug.logError(e, "Unable to locate URL for component loading file: " + componentLoadConfig.getAbsolutePath(), module);
                } catch (ComponentException e) {
                    Debug.logError(e, "Unable to load components from URL: " + configUrl.toExternalForm(), module);
                }
            } else {
                String[] fileNames = parentPath.list();
                Arrays.sort(fileNames);
                // SCIPIO: local list for dependency ordering
                List<ComponentConfig> localComponentList = new ArrayList<>();
                for (String sub: fileNames) {
                    try {
                        File componentPath = FileUtil.getFile(parentPath.getCanonicalPath() + "/" + sub);
                        if (componentPath.isDirectory() && !sub.equals("CVS") && !sub.equals(".svn")) {
                            // make sure we have a component configuration file
                            String componentLocation = componentPath.getCanonicalPath();
                            File configFile = FileUtil.getFile(componentLocation + "/ofbiz-component.xml");
                            if (configFile.exists()) {
                                ComponentConfig config = null;
                                try {
                                    // pass null for the name, will default to the internal component name
                                    config = ComponentConfig.getComponentConfig(null, componentLocation);
                                } catch (ComponentException e) {
                                    Debug.logError(e, "Cannot load component : " + componentPath.getName() + " @ " + componentLocation + " : " + e.getMessage(), module);
                                }
                                if (config == null) {
                                    Debug.logError("Cannot load component : " + componentPath.getName() + " @ " + componentLocation, module);
                                } else {
                                    // SCIPIO: do this below, after ordering
                                    //loadComponent(config);
                                    localComponentList.add(config);
                                }
                            }
                        }
                    } catch (IOException ioe) {
                        Debug.logError(ioe, module);
                    }
                }
                localComponentList = resolveComponentDependencies(localComponentList, componentList);
                for(ComponentConfig config : localComponentList) {
                    loadComponent(config);
                }
                componentList.addAll(localComponentList);
            }
        }
    }

    private void loadComponent(ComponentConfig config) {
        // make sure the component is enabled
        if (!config.enabled()) {
            Debug.logInfo("Not Loaded component : [" + config.getComponentName() + "] (disabled)", module);
            return;
        }
        Debug.logInfo("Loaded component : [" + config.getComponentName() + "]", module);
    }

    /**
     * SCIPIO: implement component dependency resolution and change the global component ordering accordingly.
     */
    private List<ComponentConfig> resolveComponentDependencies(List<ComponentConfig> componentList, List<ComponentConfig> alreadyLoaded) {
        if (componentList == null || componentList.size() == 0) {
            return componentList;
        }
        try {
            Map<String, ComponentConfig> nameMap = ComponentConfig.makeComponentNameMap(componentList);
            List<String> names = new ArrayList<>(nameMap.keySet());
            Map<String, ComponentConfig> alreadyLoadedNameMap = ComponentConfig.makeComponentNameMap(alreadyLoaded);
            
            // Get the dependencies and perform some preliminary checks
            Map<String, List<String>> dependencies = new HashMap<>();
            for(Map.Entry<String, ComponentConfig> entry : nameMap.entrySet()) {
                String name = entry.getKey();
                ComponentConfig config = entry.getValue();
                List<String> deps = new ArrayList<>(config.getComponentDependencies().size());
                for(String depName : config.getComponentDependencies()) {
                    if (!nameMap.containsKey(depName) && !alreadyLoadedNameMap.containsKey(depName)) {
                        Debug.logError("Scipio: component '" + name + "' depends on component '" + depName 
                                + "', which is either missing or cannot be loaded before this component; ignoring dependency", module);
                        continue;
                    }
                    // Only consider the dependencies that aren't already set in stone
                    if (!alreadyLoadedNameMap.containsKey(depName)) {
                        deps.add(depName);
                    }
                }
                dependencies.put(name, deps);
            }
            
            DependencyGraph<String> depGraph = new DependencyGraph<>(names, dependencies);
            List<String> resolved = depGraph.getResolvedDependenciesDfs();

            List<ComponentConfig> resultList = new ArrayList<>(componentList.size());
            for(String resolvedName : resolved) {
                resultList.add(nameMap.get(resolvedName));
            }
            
            Debug.logInfo("Scipio: Performed automatic component dependency analysis and ordering for " + componentList.size() + " components:"
                    + "\nOriginal order: " + names.toString()
                    + "\nResolved order: " + resolved.toString(), module);
            return resultList;
        } catch(Exception e) {
            Debug.logError(e, "Scipio: Automatic component dependency analysis failure: " + e.getMessage()
                + "; load order unchanged", module);
            return componentList;
        }
    }

    /**
     * @see org.ofbiz.base.container.Container#stop()
     */
    public void stop() throws ContainerException {
    }

    public String getName() {
        return name;
    }

}
