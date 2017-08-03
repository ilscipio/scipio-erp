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
package org.ofbiz.base.component;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.security.KeyStore;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.ofbiz.base.container.ContainerConfig;
import org.ofbiz.base.container.ContainerException;
import org.ofbiz.base.container.ContainerConfig.Container;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.KeyStoreUtil;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * An object that models the <code>&lt;ofbiz-component&gt;</code> element.
 * 
 * @see <code>ofbiz-component.xsd</code>
 *
 */
public final class ComponentConfig {

    public static final String module = ComponentConfig.class.getName();
    public static final String OFBIZ_COMPONENT_XML_FILENAME = "ofbiz-component.xml";
    /* Note: These Maps are not UtilCache instances because there is no strategy or implementation for reloading components.
     * Also, we are using LinkedHashMap to maintain insertion order - which client code depends on. This means
     * we will need to use synchronization code because there is no concurrent implementation of LinkedHashMap.
     */
    private static final ComponentConfigCache componentConfigCache = new ComponentConfigCache();
    private static final Map<String, List<WebappInfo>> serverWebApps = new LinkedHashMap<String, List<WebappInfo>>();

    public static Boolean componentExists(String componentName) {
        Assert.notEmpty("componentName", componentName);
        return componentConfigCache.fromGlobalName(componentName) != null;
    }

    public static List<ClasspathInfo> getAllClasspathInfos() {
        return getAllClasspathInfos(null);
    }

    public static List<ClasspathInfo> getAllClasspathInfos(String componentName) {
        List<ClasspathInfo> classpaths = new ArrayList<ClasspathInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                classpaths.addAll(cc.getClasspathInfos());
            }
        }
        return classpaths;
    }

    public static Collection<ComponentConfig> getAllComponents() {
        return componentConfigCache.values();
    }

    public static List<ContainerConfig.Container> getAllContainers() {
        return getAllContainers(null);
    }

    public static List<ContainerConfig.Container> getAllContainers(String componentName) {
        List<ContainerConfig.Container> containers = new ArrayList<ContainerConfig.Container>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                containers.addAll(cc.getContainers());
            }
        }
        return containers;
    }

    public static List<EntityResourceInfo> getAllEntityResourceInfos(String type) {
        return getAllEntityResourceInfos(type, null);
    }

    public static List<EntityResourceInfo> getAllEntityResourceInfos(String type, String componentName) {
        List<EntityResourceInfo> entityInfos = new ArrayList<EntityResourceInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                List<EntityResourceInfo> ccEntityInfoList = cc.getEntityResourceInfos();
                if (UtilValidate.isEmpty(type)) {
                    entityInfos.addAll(ccEntityInfoList);
                } else {
                    for (EntityResourceInfo entityResourceInfo : ccEntityInfoList) {
                        if (type.equals(entityResourceInfo.type)) {
                            entityInfos.add(entityResourceInfo);
                        }
                    }
                }
            }
        }
        return entityInfos;
    }

    public static List<KeystoreInfo> getAllKeystoreInfos() {
        return getAllKeystoreInfos(null);
    }

    public static List<KeystoreInfo> getAllKeystoreInfos(String componentName) {
        List<KeystoreInfo> keystoreInfos = new ArrayList<KeystoreInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                keystoreInfos.addAll(cc.getKeystoreInfos());
            }
        }
        return keystoreInfos;
    }

    public static List<ServiceResourceInfo> getAllServiceResourceInfos(String type) {
        return getAllServiceResourceInfos(type, null);
    }

    public static List<ServiceResourceInfo> getAllServiceResourceInfos(String type, String componentName) {
        List<ServiceResourceInfo> serviceInfos = new ArrayList<ServiceResourceInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                List<ServiceResourceInfo> ccServiceInfoList = cc.getServiceResourceInfos();
                if (UtilValidate.isEmpty(type)) {
                    serviceInfos.addAll(ccServiceInfoList);
                } else {
                    for (ServiceResourceInfo serviceResourceInfo : ccServiceInfoList) {
                        if (type.equals(serviceResourceInfo.type)) {
                            serviceInfos.add(serviceResourceInfo);
                        }
                    }
                }
            }
        }
        return serviceInfos;
    }

    public static List<TestSuiteInfo> getAllTestSuiteInfos() {
        return getAllTestSuiteInfos(null);
    }

    public static List<TestSuiteInfo> getAllTestSuiteInfos(String componentName) {
        List<TestSuiteInfo> testSuiteInfos = new ArrayList<TestSuiteInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                testSuiteInfos.addAll(cc.getTestSuiteInfos());
            }
        }
        return testSuiteInfos;
    }

    public static List<WebappInfo> getAllWebappResourceInfos() {
        return getAllWebappResourceInfos(null);
    }

    public static List<WebappInfo> getAllWebappResourceInfos(String componentName) {
        List<WebappInfo> webappInfos = new ArrayList<WebappInfo>();
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                webappInfos.addAll(cc.getWebappInfos());
            }
        }
        return webappInfos;
    }
    
    /**
     * SCIPIO: Returns webapp infos by context root (mount-point).
     */
    private static Map<String, List<WebappInfo>> getAllWebappResourceInfosByContextRoot(Collection<ComponentConfig> componentList, String componentName) {
        Map<String, List<WebappInfo>> contextRootWebappInfos = new HashMap<>();
        for (ComponentConfig cc : componentList) {
            if (componentName == null || componentName.equals(cc.getComponentName())) {
                for(WebappInfo webappInfo : cc.getWebappInfos()) {
                    List<WebappInfo> webappInfoList = contextRootWebappInfos.get(webappInfo.getContextRoot());
                    if (webappInfoList == null) {
                        webappInfoList = new ArrayList<>();
                        contextRootWebappInfos.put(webappInfo.getContextRoot(), webappInfoList);
                    }
                    webappInfoList.add(webappInfo);
                }
            }
        }
        return contextRootWebappInfos;
    }

    public static List<WebappInfo> getAppBarWebInfos(String serverName) {
        return ComponentConfig.getAppBarWebInfos(serverName, null, null);
    }

    public static List<WebappInfo> getAppBarWebInfos(String serverName, Comparator<? super String> comp, String menuName) {
        String serverWebAppsKey = serverName + menuName;
        List<WebappInfo> webInfos = null;
        synchronized (serverWebApps) {
            webInfos = serverWebApps.get(serverWebAppsKey);
        }
        if (webInfos == null) {
            Map<String, WebappInfo> tm = null;
            // use a TreeMap to sort the components alpha by title
            if (comp != null) {
                tm = new TreeMap<String, WebappInfo>(comp);
            } else {
                tm = new TreeMap<String, WebappInfo>();
            }
            for (ComponentConfig cc : getAllComponents()) {
                for (WebappInfo wInfo : cc.getWebappInfos()) {
                    String key = UtilValidate.isNotEmpty(wInfo.position) ? wInfo.position : wInfo.title;
                    if (serverName.equals(wInfo.server) && wInfo.getAppBarDisplay()) {
                        if (UtilValidate.isNotEmpty(menuName)) {
                            if (menuName.equals(wInfo.menuName)) {
                                tm.put(key, wInfo);
                            }
                        } else {
                            tm.put(key, wInfo);
                        }
                    }
                }
            }
            webInfos = new ArrayList<WebappInfo>(tm.size());
            webInfos.addAll(tm.values());
            webInfos = Collections.unmodifiableList(webInfos);
            synchronized (serverWebApps) {
                // We are only preventing concurrent modification, we are not guaranteeing a singleton.
                serverWebApps.put(serverWebAppsKey, webInfos);
            }
        }
        return webInfos;
    }

    public static List<WebappInfo> getAppBarWebInfos(String serverName, String menuName) {
        return getAppBarWebInfos(serverName, null, menuName);
    }

    public static ComponentConfig getComponentConfig(String globalName) throws ComponentException {
        // TODO: we need to look up the rootLocation from the container config, or this will blow up
        return getComponentConfig(globalName, null);
    }

    public static ComponentConfig getComponentConfig(String globalName, String rootLocation) throws ComponentException {
        ComponentConfig componentConfig = null;
        if (globalName != null && !globalName.isEmpty()) {
            componentConfig = componentConfigCache.fromGlobalName(globalName);
            if (componentConfig != null) {
                return componentConfig;
            }
        }
        if (rootLocation != null && !rootLocation.isEmpty()) {
            componentConfig = componentConfigCache.fromRootLocation(rootLocation);
            if (componentConfig != null) {
                return componentConfig;
            }
        }
        if (rootLocation != null) {
            componentConfig = new ComponentConfig(globalName, rootLocation);
            if (componentConfig.enabled()) {
                componentConfigCache.put(componentConfig);
            }
            return componentConfig;
        } else {
            // Do we really need to do this?
            // SCIPIO: 2017-08-03: the answer is yes
            throw new ComponentException.ComponentNotFoundException("No component found named : " + globalName); // SCIPIO: 2017-08-03: better exception
        }
    }
    
    /**
     * SCIPIO: Special method for initialization only to set the global component order.
     * Public only by force - intended ONLY for use from {@link org.ofbiz.base.container.ComponentContainer}.
     */
    public static void clearStoreComponents(List<ComponentConfig> componentList) {
        componentConfigCache.clearAndPutAll(componentList);
    }

    public static String getFullLocation(String componentName, String resourceLoaderName, String location) throws ComponentException {
        ComponentConfig cc = getComponentConfig(componentName, null);
        if (cc == null) {
            throw new ComponentException.ComponentNotFoundException("Could not find component with name: " + componentName); // SCIPIO: 2017-08-03: better exception
        }
        return cc.getFullLocation(resourceLoaderName, location);
    }

    public static KeystoreInfo getKeystoreInfo(String componentName, String keystoreName) {
        for (ComponentConfig cc : getAllComponents()) {
            if (componentName != null && componentName.equals(cc.getComponentName())) {
                for (KeystoreInfo ks : cc.getKeystoreInfos()) {
                    if (keystoreName != null && keystoreName.equals(ks.getName())) {
                        return ks;
                    }
                }
            }
        }
        return null;
    }

    public static String getRootLocation(String componentName) throws ComponentException {
        ComponentConfig cc = getComponentConfig(componentName);
        if (cc == null) {
            throw new ComponentException.ComponentNotFoundException("Could not find component with name: " + componentName); // SCIPIO: 2017-08-03: better exception
        }
        return cc.getRootLocation();
    }

    public static InputStream getStream(String componentName, String resourceLoaderName, String location) throws ComponentException {
        ComponentConfig cc = getComponentConfig(componentName);
        if (cc == null) {
            throw new ComponentException.ComponentNotFoundException("Could not find component with name: " + componentName); // SCIPIO: 2017-08-03: better exception
        }
        return cc.getStream(resourceLoaderName, location);
    }

    public static URL getURL(String componentName, String resourceLoaderName, String location) throws ComponentException {
        ComponentConfig cc = getComponentConfig(componentName);
        if (cc == null) {
            throw new ComponentException.ComponentNotFoundException("Could not find component with name: " + componentName); // SCIPIO: 2017-08-03: better exception
        }
        return cc.getURL(resourceLoaderName, location);
    }

    public static WebappInfo getWebAppInfo(String serverName, String contextRoot) {
        if (serverName == null || contextRoot == null) {
            return null;
        }
        ComponentConfig.WebappInfo info = null;
        for (ComponentConfig cc : getAllComponents()) {
            for (WebappInfo wInfo : cc.getWebappInfos()) {
                if (serverName.equals(wInfo.server) && contextRoot.equals(wInfo.getContextRoot())) {
                    info = wInfo;
                }
            }
        }
        return info;
    }

    public static boolean isFileResourceLoader(String componentName, String resourceLoaderName) throws ComponentException {
        ComponentConfig cc = getComponentConfig(componentName);
        if (cc == null) {
            throw new ComponentException.ComponentNotFoundException("Could not find component with name: " + componentName); // SCIPIO: 2017-08-03: better exception
        }
        return cc.isFileResourceLoader(resourceLoaderName);
    }

    // ========== ComponentConfig instance ==========

    private final String globalName;
    private final String rootLocation;
    private final String componentName;
    private final boolean enabled;
    private final Map<String, ResourceLoaderInfo> resourceLoaderInfos;
    private final List<ClasspathInfo> classpathInfos;
    private final List<EntityResourceInfo> entityResourceInfos;
    private final List<ServiceResourceInfo> serviceResourceInfos;
    private final List<TestSuiteInfo> testSuiteInfos;
    private final List<KeystoreInfo> keystoreInfos;
    private final List<WebappInfo> webappInfos;
    private final List<ContainerConfig.Container> containers;
    /**
     * SCIPIO: The components this one depends on.
     * This is the implementation of the "depends-on" element (existed in stock ofbiz xsd, but not implemented).
     * Does not contain duplicates.
     */
    private final List<String> componentDependencies;
    
    private ComponentConfig(String globalName, String rootLocation) throws ComponentException {
        if (!rootLocation.endsWith("/")) {
            rootLocation = rootLocation + "/";
        }
        this.rootLocation = rootLocation.replace('\\', '/');
        File rootLocationDir = new File(rootLocation);
        if (!rootLocationDir.exists()) {
            throw new ComponentException("The component root location does not exist: " + rootLocation);
        }
        if (!rootLocationDir.isDirectory()) {
            throw new ComponentException("The component root location is not a directory: " + rootLocation);
        }
        String xmlFilename = rootLocation + "/" + OFBIZ_COMPONENT_XML_FILENAME;
        URL xmlUrl = UtilURL.fromFilename(xmlFilename);
        if (xmlUrl == null) {
            throw new ComponentException("Could not find the " + OFBIZ_COMPONENT_XML_FILENAME + " configuration file in the component root location: " + rootLocation);
        }
        Document ofbizComponentDocument = null;
        try {
            ofbizComponentDocument = UtilXml.readXmlDocument(xmlUrl, true);
        } catch (Exception e) {
            throw new ComponentException("Error reading the component config file: " + xmlUrl, e);
        }
        Element ofbizComponentElement = ofbizComponentDocument.getDocumentElement();
        this.componentName = ofbizComponentElement.getAttribute("name");
        this.enabled = "true".equalsIgnoreCase(ofbizComponentElement.getAttribute("enabled"));
        if (UtilValidate.isEmpty(globalName)) {
            this.globalName = this.componentName;
        } else {
            this.globalName = globalName;
        }
        // resource-loader - resourceLoaderInfos
        List<? extends Element> childElements = UtilXml.childElementList(ofbizComponentElement, "resource-loader");
        if (!childElements.isEmpty()) {
            Map<String, ResourceLoaderInfo> resourceLoaderInfos = new LinkedHashMap<String, ResourceLoaderInfo>();
            for (Element curElement : childElements) {
                ResourceLoaderInfo resourceLoaderInfo = new ResourceLoaderInfo(curElement);
                resourceLoaderInfos.put(resourceLoaderInfo.name, resourceLoaderInfo);
            }
            this.resourceLoaderInfos = Collections.unmodifiableMap(resourceLoaderInfos);
        } else {
            this.resourceLoaderInfos = Collections.emptyMap();
        }
        // classpath - classpathInfos
        childElements = UtilXml.childElementList(ofbizComponentElement, "classpath");
        if (!childElements.isEmpty()) {
            List<ClasspathInfo> classpathInfos = new ArrayList<ClasspathInfo>(childElements.size());
            for (Element curElement : childElements) {
                ClasspathInfo classpathInfo = new ClasspathInfo(this, curElement);
                classpathInfos.add(classpathInfo);
            }
            this.classpathInfos = Collections.unmodifiableList(classpathInfos);
        } else {
            this.classpathInfos = Collections.emptyList();
        }
        // entity-resource - entityResourceInfos
        childElements = UtilXml.childElementList(ofbizComponentElement, "entity-resource");
        if (!childElements.isEmpty()) {
            List<EntityResourceInfo> entityResourceInfos = new ArrayList<EntityResourceInfo>(childElements.size());
            for (Element curElement : childElements) {
                EntityResourceInfo entityResourceInfo = new EntityResourceInfo(this, curElement);
                entityResourceInfos.add(entityResourceInfo);
            }
            this.entityResourceInfos = Collections.unmodifiableList(entityResourceInfos);
        } else {
            this.entityResourceInfos = Collections.emptyList();
        }
        // service-resource - serviceResourceInfos
        childElements = UtilXml.childElementList(ofbizComponentElement, "service-resource");
        if (!childElements.isEmpty()) {
            List<ServiceResourceInfo> serviceResourceInfos = new ArrayList<ServiceResourceInfo>(childElements.size());
            for (Element curElement : childElements) {
                ServiceResourceInfo serviceResourceInfo = new ServiceResourceInfo(this, curElement);
                serviceResourceInfos.add(serviceResourceInfo);
            }
            this.serviceResourceInfos = Collections.unmodifiableList(serviceResourceInfos);
        } else {
            this.serviceResourceInfos = Collections.emptyList();
        }
        // test-suite - serviceResourceInfos
        childElements = UtilXml.childElementList(ofbizComponentElement, "test-suite");
        if (!childElements.isEmpty()) {
            List<TestSuiteInfo> testSuiteInfos = new ArrayList<TestSuiteInfo>(childElements.size());
            for (Element curElement : childElements) {
                TestSuiteInfo testSuiteInfo = new TestSuiteInfo(this, curElement);
                testSuiteInfos.add(testSuiteInfo);
            }
            this.testSuiteInfos = Collections.unmodifiableList(testSuiteInfos);
        } else {
            this.testSuiteInfos = Collections.emptyList();
        }
        // keystore - (cert/trust store infos)
        childElements = UtilXml.childElementList(ofbizComponentElement, "keystore");
        if (!childElements.isEmpty()) {
            List<KeystoreInfo> keystoreInfos = new ArrayList<KeystoreInfo>(childElements.size());
            for (Element curElement : childElements) {
                KeystoreInfo keystoreInfo = new KeystoreInfo(this, curElement);
                keystoreInfos.add(keystoreInfo);
            }
            this.keystoreInfos = Collections.unmodifiableList(keystoreInfos);
        } else {
            this.keystoreInfos = Collections.emptyList();
        }
        // webapp - webappInfos
        childElements = UtilXml.childElementList(ofbizComponentElement, "webapp");
        if (!childElements.isEmpty()) {
            List<WebappInfo> webappInfos = new ArrayList<WebappInfo>(childElements.size());
            for (Element curElement : childElements) {
                WebappInfo webappInfo = new WebappInfo(this, curElement);
                webappInfos.add(webappInfo);
            }
            this.webappInfos = Collections.unmodifiableList(webappInfos);
        } else {
            this.webappInfos = Collections.emptyList();
        }
        // containers
        try {
            Collection<Container> containers = ContainerConfig.getContainers(xmlUrl);
            if (!containers.isEmpty()) {
                this.containers = Collections.unmodifiableList(new ArrayList<ContainerConfig.Container>(containers));
            } else {
                this.containers = Collections.emptyList();
            }
        } catch (ContainerException ce) {
            throw new ComponentException("Error reading containers for component: " + this.globalName, ce);
        }
        // SCIPIO: component dependencies
        childElements = UtilXml.childElementList(ofbizComponentElement, "depends-on");
        if (!childElements.isEmpty()) {
            Collection<String> componentDependencies = new LinkedHashSet<>();
            for (Element curElement : childElements) {
                String depName = curElement.getAttribute("component-name");
                if (depName.equals(this.componentName)) {
                    Debug.logWarning("SCIPIO: component '" + this.componentName + "' has dependency on itself", module);
                    continue;
                }
                if (componentDependencies.contains(depName)) {
                    Debug.logWarning("SCIPIO: component '" + this.componentName + "' has duplicate dependency on component '" + depName + "'", module);
                    continue;
                }
                componentDependencies.add(depName);
            }
            this.componentDependencies = Collections.unmodifiableList(new ArrayList<String>(componentDependencies));
        } else {
            this.componentDependencies = Collections.emptyList();
        }
        if (Debug.verboseOn())
            Debug.logVerbose("Read component config : [" + rootLocation + "]", module);
    }
    
    /**
     * SCIPIO: Copy constructor, with extra optional overrides such as explicit webappInfos.
     */
    private ComponentConfig(ComponentConfig other, List<WebappInfo> webappInfos) {
        this.globalName = other.globalName;
        this.rootLocation = other.rootLocation;
        this.componentName = other.componentName;
        this.enabled = other.enabled;
        this.resourceLoaderInfos = other.resourceLoaderInfos;
        this.classpathInfos = other.classpathInfos;
        this.entityResourceInfos = other.entityResourceInfos;
        this.serviceResourceInfos = other.serviceResourceInfos;
        this.testSuiteInfos = other.testSuiteInfos;
        this.keystoreInfos = other.keystoreInfos;
        if (webappInfos != null) {
            List<WebappInfo> newWebappInfos = new ArrayList<>(webappInfos.size());
            for(WebappInfo webappInfo : webappInfos) {
                if (webappInfo.componentConfig != this) {
                    // NOTE: IMPORTANT: We must duplicate all the webapps if the backreference to ComponentConfig has changed!
                    newWebappInfos.add(new WebappInfo(webappInfo, this));
                } else {
                    newWebappInfos.add(webappInfo);
                }
            }
            this.webappInfos = Collections.unmodifiableList(newWebappInfos);
        } else {
            this.webappInfos = other.webappInfos;
        }
        this.containers = other.containers;
        this.componentDependencies = other.componentDependencies;
    }

    public boolean enabled() {
        return this.enabled;
    }

    public List<ClasspathInfo> getClasspathInfos() {
        return this.classpathInfos;
    }

    public String getComponentName() {
        return this.componentName;
    }

    public List<ContainerConfig.Container> getContainers() {
        return this.containers;
    }

    public List<EntityResourceInfo> getEntityResourceInfos() {
        return this.entityResourceInfos;
    }

    public String getFullLocation(String resourceLoaderName, String location) throws ComponentException {
        ResourceLoaderInfo resourceLoaderInfo = resourceLoaderInfos.get(resourceLoaderName);
        if (resourceLoaderInfo == null) {
            throw new ComponentException("Could not find resource-loader named: " + resourceLoaderName);
        }
        StringBuilder buf = new StringBuilder();
        // pre-pend component root location if this is a type component resource-loader
        if ("component".equals(resourceLoaderInfo.type)) {
            buf.append(rootLocation);
        }

        if (UtilValidate.isNotEmpty(resourceLoaderInfo.prependEnv)) {
            String propValue = System.getProperty(resourceLoaderInfo.prependEnv);
            if (propValue == null) {
                String errMsg = "The Java environment (-Dxxx=yyy) variable with name " + resourceLoaderInfo.prependEnv + " is not set, cannot load resource.";
                Debug.logError(errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
            buf.append(propValue);
        }
        if (UtilValidate.isNotEmpty(resourceLoaderInfo.prefix)) {
            buf.append(resourceLoaderInfo.prefix);
        }
        buf.append(location);
        return buf.toString();
    }

    public String getGlobalName() {
        return this.globalName;
    }

    public List<KeystoreInfo> getKeystoreInfos() {
        return this.keystoreInfos;
    }

    public Map<String, ResourceLoaderInfo> getResourceLoaderInfos() {
        return this.resourceLoaderInfos;
    }

    public String getRootLocation() {
        return this.rootLocation;
    }

    public List<ServiceResourceInfo> getServiceResourceInfos() {
        return this.serviceResourceInfos;
    }

    public InputStream getStream(String resourceLoaderName, String location) throws ComponentException {
        URL url = getURL(resourceLoaderName, location);
        try {
            return url.openStream();
        } catch (java.io.IOException e) {
            throw new ComponentException("Error opening resource at location [" + url.toExternalForm() + "]", e);
        }
    }

    public List<TestSuiteInfo> getTestSuiteInfos() {
        return this.testSuiteInfos;
    }

    public URL getURL(String resourceLoaderName, String location) throws ComponentException {
        ResourceLoaderInfo resourceLoaderInfo = resourceLoaderInfos.get(resourceLoaderName);
        if (resourceLoaderInfo == null) {
            throw new ComponentException("Could not find resource-loader named: " + resourceLoaderName);
        }
        if ("component".equals(resourceLoaderInfo.type) || "file".equals(resourceLoaderInfo.type)) {
            String fullLocation = getFullLocation(resourceLoaderName, location);
            URL fileUrl = UtilURL.fromFilename(fullLocation);
            if (fileUrl == null) {
                throw new ComponentException("File Resource not found: " + fullLocation);
            }
            return fileUrl;
        } else if ("classpath".equals(resourceLoaderInfo.type)) {
            String fullLocation = getFullLocation(resourceLoaderName, location);
            URL url = UtilURL.fromResource(fullLocation);
            if (url == null) {
                throw new ComponentException("Classpath Resource not found: " + fullLocation);
            }
            return url;
        } else if ("url".equals(resourceLoaderInfo.type)) {
            String fullLocation = getFullLocation(resourceLoaderName, location);
            URL url = null;
            try {
                url = FlexibleLocation.resolveLocation(location);
            } catch (java.net.MalformedURLException e) {
                throw new ComponentException("Error with malformed URL while trying to load URL resource at location [" + fullLocation + "]", e);
            }
            if (url == null) {
                throw new ComponentException("URL Resource not found: " + fullLocation);
            }
            return url;
        } else {
            throw new ComponentException("The resource-loader type is not recognized: " + resourceLoaderInfo.type);
        }
    }

    public List<WebappInfo> getWebappInfos() {
        return this.webappInfos;
    }

    /**
     * SCIPIO: Returns the names of components this component depends on.
     */
    public List<String> getComponentDependencies() {
        return componentDependencies;
    }
    
    public boolean isFileResource(ResourceInfo resourceInfo) throws ComponentException {
        return isFileResourceLoader(resourceInfo.loader);
    }

    public boolean isFileResourceLoader(String resourceLoaderName) throws ComponentException {
        ResourceLoaderInfo resourceLoaderInfo = resourceLoaderInfos.get(resourceLoaderName);
        if (resourceLoaderInfo == null) {
            throw new ComponentException("Could not find resource-loader named: " + resourceLoaderName);
        }
        return "file".equals(resourceLoaderInfo.type) || "component".equals(resourceLoaderInfo.type);
    }

    /**
     * SCIPIO: Returns the component names for a list of configs.
     */
    public static List<String> getComponentNames(Collection<ComponentConfig> componentList) {
        List<String> names = new ArrayList<>(componentList.size());
        for(ComponentConfig config : componentList) {
            names.add(config.getComponentName());
        }
        return names;
    }
    
    /**
     * SCIPIO: Creates a (order-preserving) map of component names to configs from a list of configs.
     */
    public static Map<String, ComponentConfig> makeComponentNameMap(Collection<ComponentConfig> componentList) {
        Map<String, ComponentConfig> map = new LinkedHashMap<>();
        for(ComponentConfig config : componentList) {
            map.put(config.getComponentName(), config);
        }
        return map;
    }
    
    /**
     * SCIPIO: Post-processes the global list of loaded components after all
     * <code>ofbiz-component.xml</code> files have been read (hook/callback) (new 2017-01-19).
     */
    public static List<ComponentConfig> postProcessComponentConfigs(List<ComponentConfig> componentList) {
        componentList = (postProcessWebappInfos(componentList));
        return componentList;
    }
    
    /**
     * SCIPIO: Post-processes the webapp infos of component list.
     */
    private static List<ComponentConfig> postProcessWebappInfos(List<ComponentConfig> componentList) {
        // Find all duplicate mount-points 
        Map<String, ComponentConfig> modifiedComponentsByName = new HashMap<>();
        for(Map.Entry<String, List<WebappInfo>> entry : getAllWebappResourceInfosByContextRoot(componentList, null).entrySet()) {
            // FIXME?: this part is not the most efficient and in certain cases may create needless copies
            List<WebappInfo> webappInfoList = entry.getValue();
            if (webappInfoList != null && webappInfoList.size() >= 2) {
                WebappInfo lastWebappInfo = webappInfoList.get(webappInfoList.size() - 1);
                // Handle special override-modes for all webapps before the last one
                for(WebappInfo webappInfo : webappInfoList.subList(0, webappInfoList.size() - 1)) {
                    if ("remove-overridden-webapp".equals(lastWebappInfo.getOverrideMode())) {
                        ComponentConfig prevCc = webappInfo.componentConfig;
                        // NOTE: if the CC has multiple webapps, it may already have been updated,
                        // so fetch it out of the map
                        if (modifiedComponentsByName.containsKey(prevCc.getComponentName())) {
                            prevCc = modifiedComponentsByName.get(prevCc.getComponentName());
                        }
                        List<WebappInfo> prevWebappInfoList = prevCc.getWebappInfos();
                        List<WebappInfo> newWebappInfoList = new ArrayList<>(prevWebappInfoList.size());
                        // removed this webapp from the list
                        for(WebappInfo prevWebappInfo : prevWebappInfoList) {
                            if (prevWebappInfo != webappInfo && !prevWebappInfo.getName().equals(webappInfo.getName())) {
                                newWebappInfoList.add(prevWebappInfo);
                            }
                        }
                        ComponentConfig newCc = new ComponentConfig(prevCc, newWebappInfoList);
                        modifiedComponentsByName.put(newCc.getComponentName(), newCc);
                    }
                }
            }
        }
        
        if (modifiedComponentsByName.size() == 0) { // optimization
            return componentList;
        }
        
        List<ComponentConfig> resultList = new ArrayList<>(componentList.size());
        for(ComponentConfig cc : componentList) {
            if (modifiedComponentsByName.containsKey(cc.getComponentName())) {
                resultList.add(modifiedComponentsByName.get(cc.getComponentName()));
            } else {
                resultList.add(cc);
            }
        }
        
        return resultList;
    }
    
    /**
     * An object that models the <code>&lt;classpath&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class ClasspathInfo {
        public final ComponentConfig componentConfig;
        public final String type;
        public final String location;

        private ClasspathInfo(ComponentConfig componentConfig, Element element) {
            this.componentConfig = componentConfig;
            this.type = element.getAttribute("type");
            this.location = element.getAttribute("location");
        }
    }

    // ComponentConfig instances need to be looked up by their global name and root location,
    // so this class encapsulates the Maps and synchronization code required to do that.
    private static final class ComponentConfigCache {
        // Key is the global name.
        private final Map<String, ComponentConfig> componentConfigs = new LinkedHashMap<String, ComponentConfig>();
        // Root location mapped to global name.
        private final Map<String, String> componentLocations = new HashMap<String, String>();
        
        private synchronized ComponentConfig fromGlobalName(String globalName) {
            return componentConfigs.get(globalName);
        }

        private synchronized ComponentConfig fromRootLocation(String rootLocation) {
            String globalName = componentLocations.get(rootLocation);
            if (globalName == null) {
                return null;
            }
            return componentConfigs.get(globalName);
        }

        private synchronized ComponentConfig put(ComponentConfig config) {
            String globalName = config.getGlobalName();
            String fileLocation = config.getRootLocation();
            componentLocations.put(fileLocation, globalName);
            return componentConfigs.put(globalName, config);
        }

        private synchronized Collection<ComponentConfig> values() {
            return Collections.unmodifiableList(new ArrayList<ComponentConfig>(componentConfigs.values()));
        }
        
        /**
         * SCIPIO: Clears all (for reordering).
         */
        private synchronized void clear() {
            componentConfigs.clear();
            componentLocations.clear();
        }
        
        /**
         * SCIPIO: putAll.
         */
        private synchronized void putAll(Collection<? extends ComponentConfig> configList) {
            for(ComponentConfig config : configList) {
                put(config);
            }
        }
        
        /**
         * SCIPIO: Atomic clear + putAll method.
         */
        private synchronized void clearAndPutAll(Collection<? extends ComponentConfig> configList) {
            clear();
            putAll(configList);
        }
    }

    /**
     * An object that models the <code>&lt;entity-resource&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class EntityResourceInfo extends ResourceInfo {
        public final String type;
        public final String readerName;

        private EntityResourceInfo(ComponentConfig componentConfig, Element element) {
            super(componentConfig, element);
            this.type = element.getAttribute("type");
            this.readerName = element.getAttribute("reader-name");
        }
    }

    /**
     * An object that models the <code>&lt;keystore&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class KeystoreInfo extends ResourceInfo {
        private final String name;
        private final String type;
        private final String password;
        private final boolean isCertStore;
        private final boolean isTrustStore;

        private KeystoreInfo(ComponentConfig componentConfig, Element element) {
            super(componentConfig, element);
            this.name = element.getAttribute("name");
            this.type = element.getAttribute("type");
            this.password = element.getAttribute("password");
            this.isCertStore = "true".equalsIgnoreCase(element.getAttribute("is-certstore"));
            this.isTrustStore = "true".equalsIgnoreCase(element.getAttribute("is-truststore"));
        }

        public KeyStore getKeyStore() {
            ComponentResourceHandler rh = this.createResourceHandler();
            if (rh != null) {
                try {
                    return KeyStoreUtil.getStore(rh.getURL(), this.getPassword(), this.getType());
                } catch (Exception e) {
                    Debug.logWarning(e, module);
                }
            }
            return null;
        }

        public String getName() {
            return name;
        }

        public String getPassword() {
            return password;
        }

        public String getType() {
            return type;
        }

        public boolean isCertStore() {
            return isCertStore;
        }

        public boolean isTrustStore() {
            return isTrustStore;
        }
    }

    public static abstract class ResourceInfo {
        private final ComponentConfig componentConfig;
        private final String loader;
        private final String location;

        protected ResourceInfo(ComponentConfig componentConfig, Element element) {
            this.componentConfig = componentConfig;
            this.loader = element.getAttribute("loader");
            this.location = element.getAttribute("location");
        }

        public ComponentResourceHandler createResourceHandler() {
            return new ComponentResourceHandler(componentConfig.getGlobalName(), loader, location);
        }

        public ComponentConfig getComponentConfig() {
            return componentConfig;
        }

        public String getLocation() {
            return location;
        }
    }

    /**
     * An object that models the <code>&lt;resource-loader&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class ResourceLoaderInfo {
        public final String name;
        public final String type;
        public final String prependEnv;
        public final String prefix;

        private ResourceLoaderInfo(Element element) {
            this.name = element.getAttribute("name");
            this.type = element.getAttribute("type");
            this.prependEnv = element.getAttribute("prepend-env");
            this.prefix = element.getAttribute("prefix");
        }
    }

    /**
     * An object that models the <code>&lt;service-resource&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class ServiceResourceInfo extends ResourceInfo {
        public final String type;

        private ServiceResourceInfo(ComponentConfig componentConfig, Element element) {
            super(componentConfig, element);
            this.type = element.getAttribute("type");
        }
    }

    /**
     * An object that models the <code>&lt;test-suite&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class TestSuiteInfo extends ResourceInfo {
        public TestSuiteInfo(ComponentConfig componentConfig, Element element) {
            super(componentConfig, element);
        }
    }

    /**
     * An object that models the <code>&lt;webapp&gt;</code> element.
     * 
     * @see <code>ofbiz-component.xsd</code>
     *
     */
    public static final class WebappInfo {
        // FIXME: These fields should be private - since we have accessors - but
        // client code accesses the fields directly.
        public final ComponentConfig componentConfig;
        public final List<String> virtualHosts;
        public final Map<String, String> initParameters;
        public final String name;
        public final String title;
        public final String description;
        public final String menuName;
        public final String server;
        public final String mountPoint;
        public final String contextRoot;
        public final String location;
        public final String[] basePermission;
        public final String position;
        public final boolean sessionCookieAccepted;
        public final boolean privileged;
        // CatalinaContainer modifies this field.
        private volatile boolean appBarDisplay;
        private final String accessPermission;
        private final String overrideMode; // SCIPIO: 2017-01-19: new override mode, to handle duplicate mount-points

        private WebappInfo(ComponentConfig componentConfig, Element element) {
            this.componentConfig = componentConfig;
            this.name = element.getAttribute("name");
            String title = element.getAttribute("title");
            if (title.isEmpty()) {
                // default title is name w/ upper-cased first letter
                title = Character.toUpperCase(name.charAt(0)) + name.substring(1).toLowerCase();
            }
            this.title = title;
            String description = element.getAttribute("description");
            if (description.isEmpty()) {
                description = this.title;
            }
            this.description = description;
            this.server = element.getAttribute("server");
            String mountPoint = element.getAttribute("mount-point");
            // check the mount point and make sure it is properly formatted
            if (!mountPoint.isEmpty()) {
                if (!mountPoint.startsWith("/")) {
                    mountPoint = "/" + mountPoint;
                }
                if (!mountPoint.endsWith("/*")) {
                    if (!mountPoint.endsWith("/")) {
                        mountPoint = mountPoint + "/";
                    }
                    mountPoint = mountPoint + "*";
                }
            }
            this.mountPoint = mountPoint;
            if (this.mountPoint.endsWith("/*")) {
                this.contextRoot = this.mountPoint.substring(0, this.mountPoint.length() - 2);
            } else {
                this.contextRoot = this.mountPoint;
            }
            this.location = element.getAttribute("location");
            this.appBarDisplay = !"false".equals(element.getAttribute("app-bar-display"));
            this.sessionCookieAccepted = !"false".equals(element.getAttribute("session-cookie-accepted"));
            this.privileged = !"false".equals(element.getAttribute("privileged"));
            this.accessPermission = element.getAttribute("access-permission");
            String basePermStr = element.getAttribute("base-permission");
            if (!basePermStr.isEmpty()) {
                this.basePermission = basePermStr.split(",");
            } else {
                // default base permission is NONE
                this.basePermission = new String[] { "NONE" };
            }
            // trim the permissions (remove spaces)
            for (int i = 0; i < this.basePermission.length; i++) {
                this.basePermission[i] = this.basePermission[i].trim();
                if (this.basePermission[i].indexOf('_') != -1) {
                    this.basePermission[i] = this.basePermission[i].substring(0, this.basePermission[i].indexOf('_'));
                }
            }
            String menuNameStr = element.getAttribute("menu-name");
            if (UtilValidate.isNotEmpty(menuNameStr)) {
                this.menuName = menuNameStr;
            } else {
                this.menuName = "main";
            }
            this.position = element.getAttribute("position");
            // load the virtual hosts
            List<? extends Element> virtHostList = UtilXml.childElementList(element, "virtual-host");
            if (!virtHostList.isEmpty()) {
                List<String> virtualHosts = new ArrayList<String>(virtHostList.size());
                for (Element e : virtHostList) {
                    virtualHosts.add(e.getAttribute("host-name"));
                }
                this.virtualHosts = Collections.unmodifiableList(virtualHosts);
            } else {
                this.virtualHosts = Collections.emptyList();
            }
            // load the init parameters
            List<? extends Element> initParamList = UtilXml.childElementList(element, "init-param");
            if (!initParamList.isEmpty()) {
                Map<String, String> initParameters = new LinkedHashMap<String, String>();
                for (Element e : initParamList) {
                    initParameters.put(e.getAttribute("name"), e.getAttribute("value"));
                }
                this.initParameters = Collections.unmodifiableMap(initParameters);
            } else {
                this.initParameters = Collections.emptyMap();
            }
            // SCIPIO: new
            this.overrideMode = element.getAttribute("override-mode");
        }

        /**
         * SCIPIO: Copy constructor, with optional parent/backreference override for component config.
         */
        private WebappInfo(WebappInfo other, ComponentConfig componentConfig) {
            this.componentConfig = (componentConfig != null) ? componentConfig : other.componentConfig;
            this.virtualHosts = other.virtualHosts;
            this.initParameters = other.initParameters;
            this.name = other.name;
            this.title = other.title;
            this.description = other.description;
            this.menuName = other.menuName;
            this.server = other.server;
            this.mountPoint = other.mountPoint;
            this.contextRoot = other.contextRoot;
            this.location = other.location;
            this.basePermission = other.basePermission;
            this.position = other.position;
            this.sessionCookieAccepted = other.sessionCookieAccepted;
            this.privileged = other.privileged;
            this.appBarDisplay = other.appBarDisplay;
            this.accessPermission = other.accessPermission;
            this.overrideMode = other.overrideMode;
        }
        
        public synchronized boolean getAppBarDisplay() {
            return this.appBarDisplay;
        }

        public String getAccessPermission() {
            return this.accessPermission;
        }

        public String[] getBasePermission() {
            return this.basePermission;
        }

        public String getContextRoot() {
            return contextRoot;
        }

        public String getDescription() {
            return description;
        }

        public Map<String, String> getInitParameters() {
            return initParameters;
        }

        public String getLocation() {
            return componentConfig.getRootLocation() + location;
        }

        public String getName() {
            return name;
        }

        public String getTitle() {
            return title;
        }

        public List<String> getVirtualHosts() {
            return virtualHosts;
        }

        public boolean isSessionCookieAccepted() {
            return sessionCookieAccepted;
        }
        
        public String getOverrideMode() { // SCIPIO: new
            return overrideMode;
        }

        public synchronized void setAppBarDisplay(boolean appBarDisplay) {
            this.appBarDisplay = appBarDisplay;
        }
    }
}
