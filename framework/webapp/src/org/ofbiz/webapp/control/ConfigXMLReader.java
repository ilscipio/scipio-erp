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
package org.ofbiz.webapp.control;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.component.ComponentURLException.ComponentNotFoundURLException;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.metrics.Metrics;
import org.ofbiz.base.metrics.MetricsFactory;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.collections.MapContext;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * ConfigXMLReader.java - Reads and parses the XML site config files.
 */
public class ConfigXMLReader {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String controllerXmlFileName = "/WEB-INF/controller.xml";
    private static final UtilCache<URL, ControllerConfig> controllerCache = UtilCache.createUtilCache("webapp.ControllerConfig");
    private static final UtilCache<String, List<ControllerConfig>> controllerSearchResultsCache = UtilCache.createUtilCache("webapp.ControllerSearchResults");
    public static final RequestResponse emptyNoneRequestResponse = RequestResponse.createEmptyNoneRequestResponse();

    public static Set<String> findControllerFilesWithRequest(String requestUri, String controllerPartialPath) throws GeneralException {
        Set<String> allControllerRequestSet = new HashSet<String>();
        if (UtilValidate.isEmpty(requestUri)) {
            return allControllerRequestSet;
        }
        String cacheId = controllerPartialPath != null ? controllerPartialPath : "NOPARTIALPATH";
        List<ControllerConfig> controllerConfigs = controllerSearchResultsCache.get(cacheId);
        if (controllerConfigs == null) {
            try {
                // find controller.xml file with webappMountPoint + "/WEB-INF" in the path
                List<File> controllerFiles = FileUtil.findXmlFiles(null, controllerPartialPath, "site-conf", "site-conf.xsd");
                controllerConfigs = new LinkedList<ControllerConfig>();
                for (File controllerFile : controllerFiles) {
                    URL controllerUrl = null;
                    try {
                        controllerUrl = controllerFile.toURI().toURL();
                    } catch (MalformedURLException mue) {
                        throw new GeneralException(mue);
                    }
                    ControllerConfig cc = ConfigXMLReader.getControllerConfig(controllerUrl);
                    controllerConfigs.add(cc);
                }
                controllerConfigs = controllerSearchResultsCache.putIfAbsentAndGet(cacheId, controllerConfigs);
            } catch (IOException e) {
                throw new GeneralException("Error finding controller XML files to lookup request references: " + e.toString(), e);
            }
        }
        if (controllerConfigs != null) {
            for (ControllerConfig cc : controllerConfigs) {
                // make sure it has the named request in it
                if (cc.requestMapMap.get(requestUri) != null) {
                    String requestUniqueId = cc.url.toExternalForm() + "#" + requestUri;
                    allControllerRequestSet.add(requestUniqueId);
                    // Debug.logInfo("========== In findControllerFilesWithRequest found controller with request here [" + requestUniqueId + "]", module);
                }
            }
        }
        return allControllerRequestSet;
    }

    public static Set<String> findControllerRequestUniqueForTargetType(String target, String urlMode) throws GeneralException {
        if (UtilValidate.isEmpty(urlMode)) {
            urlMode = "intra-app";
        }
        int indexOfDollarSignCurlyBrace = target.indexOf("${");
        int indexOfQuestionMark = target.indexOf("?");
        if (indexOfDollarSignCurlyBrace >= 0 && (indexOfQuestionMark < 0 || indexOfQuestionMark > indexOfDollarSignCurlyBrace)) {
            // we have an expanded string in the requestUri part of the target, not much we can do about that...
            return null;
        }
        if ("intra-app".equals(urlMode)) {
            // look through all controller.xml files and find those with the request-uri referred to by the target
            String requestUri = UtilHttp.getRequestUriFromTarget(target);
            Set<String> controllerLocAndRequestSet = ConfigXMLReader.findControllerFilesWithRequest(requestUri, null);
            // if (controllerLocAndRequestSet.size() > 0) Debug.logInfo("============== In findRequestNamesLinkedtoInWidget, controllerLocAndRequestSet: " + controllerLocAndRequestSet, module);
            return controllerLocAndRequestSet;
        } else if ("inter-app".equals(urlMode)) {
            String webappMountPoint = UtilHttp.getWebappMountPointFromTarget(target);
            if (webappMountPoint != null)
                webappMountPoint += "/WEB-INF";
            String requestUri = UtilHttp.getRequestUriFromTarget(target);

            Set<String> controllerLocAndRequestSet = ConfigXMLReader.findControllerFilesWithRequest(requestUri, webappMountPoint);
            // if (controllerLocAndRequestSet.size() > 0) Debug.logInfo("============== In findRequestNamesLinkedtoInWidget, controllerLocAndRequestSet: " + controllerLocAndRequestSet, module);
            return controllerLocAndRequestSet;
        } else {
            return new HashSet<String>();
        }
    }

    public static ControllerConfig getControllerConfig(WebappInfo webAppInfo) throws WebAppConfigurationException, MalformedURLException {
        Assert.notNull("webAppInfo", webAppInfo);
        String filePath = webAppInfo.getLocation().concat(controllerXmlFileName);
        File configFile = new File(filePath);
        return getControllerConfig(configFile.toURI().toURL());
    }
    
    /**
     * SCIPIO: Overload that supports optional loading.
     * Added 2017-11-18.
     */
    public static ControllerConfig getControllerConfig(WebappInfo webAppInfo, boolean optional) throws WebAppConfigurationException, MalformedURLException {
        Assert.notNull("webAppInfo", webAppInfo);
        String filePath = webAppInfo.getLocation().concat(controllerXmlFileName);
        File configFile = new File(filePath);
        return getControllerConfig(configFile.toURI().toURL(), optional);
    }

    public static ControllerConfig getControllerConfig(URL url) throws WebAppConfigurationException {
        ControllerConfig controllerConfig = controllerCache.get(url);
        if (controllerConfig == null) {
            // SCIPIO: use one single factory method from now on...
            //controllerConfig = controllerCache.putIfAbsentAndGet(url, new ControllerConfig(url));
            controllerConfig = readControllerConfig(url, false);
            controllerConfig = controllerCache.putIfAbsentAndGet(url, 
                    controllerConfig != null ? controllerConfig : ControllerConfig.NULL_CONFIG); // special null cache key
            return controllerConfig;
        }
        return controllerConfig.isNull() ? null : controllerConfig; // SCIPIO: check for special null key
    }

    /**
     * SCIPIO: version of getControllerConfig that supports optional loading.
     * Added 2017-05-03.
     */
    public static ControllerConfig getControllerConfig(URL url, boolean optional) throws WebAppConfigurationException {
        ControllerConfig controllerConfig = controllerCache.get(url);
        if (controllerConfig == null) {
            controllerConfig = readControllerConfig(url, optional);
            controllerConfig = controllerCache.putIfAbsentAndGet(url, 
                    controllerConfig != null ? controllerConfig : ControllerConfig.NULL_CONFIG); // special null cache key
            return controllerConfig;
        }
        return controllerConfig.isNull() ? null : controllerConfig;
    }

    /**
     * SCIPIO: version of getControllerConfig that bypasses cache.
     * Added 2018-06-13.
     */
    public static ControllerConfig readControllerConfig(URL url, boolean optional) throws WebAppConfigurationException {
        try {
            return ControllerConfigFactory.getFactory().readControllerConfig(url);
        } catch(WebAppConfigurationException e) {
            if (optional && (e.getCause() instanceof java.io.FileNotFoundException)) {
                if (Debug.infoOn()) {
                    Debug.logInfo("controller skipped (not found, optional): " + url.toString(), module);
                }
                return null;
            } else {
                throw e;
            }
        }
    }

    public static URL getControllerConfigURL(ServletContext context) {
        try {
            return context.getResource(controllerXmlFileName);
        } catch (MalformedURLException e) {
            Debug.logError(e, "Error Finding XML Config File: " + controllerXmlFileName, module);
            return null;
        }
    }

    /** Loads the XML file and returns the root element 
     * @throws WebAppConfigurationException */
    private static Element loadDocument(URL location) throws WebAppConfigurationException {
        try {
            Document document = UtilXml.readXmlDocument(location, true);
            Element rootElement = document.getDocumentElement();
            if (Debug.verboseOn())
                Debug.logVerbose("Loaded XML Config - " + location, module);
            return rootElement;
        } catch (java.io.FileNotFoundException e) { // SCIPIO: special case: let caller log this one, IF necessary
            throw new WebAppConfigurationException(e);
        } catch (Exception e) {
            //Scipio: not all components have a WebApp, so this should not be logged as an error.
            // Debug.logError(e, module);
            throw new WebAppConfigurationException(e);
        }
    }

    /**
     * SCIPIO: Controller config factory - see requestHandler.properties.
     * 2018-06-13.
     */
    public static abstract class ControllerConfigFactory {
        private static final ControllerConfigFactory defaultFactory = 
                getFactoryFromProperty("requestHandler", "controller.config.factoryClass");
        
        public abstract ControllerConfig readControllerConfig(URL url) throws WebAppConfigurationException;

        public static ControllerConfigFactory getFactory() {
            return defaultFactory;
        }
        
        @SuppressWarnings("unchecked")
        public static ControllerConfigFactory getFactoryFromProperty(String resource, String property) {
            String factoryClassName = UtilProperties.getPropertyValue(resource, property);
            ControllerConfigFactory factory;
            try {
                Class<? extends ControllerConfigFactory> factoryClass = 
                        (Class<? extends ControllerConfigFactory>) Thread.currentThread().getContextClassLoader().loadClass(factoryClassName);
                factory = (ControllerConfigFactory) factoryClass.newInstance();
            } catch (Exception e) {
                Debug.logError(e, "Could not initialize controller config factory '" + factoryClassName + "': " + e.getMessage(), module);
                factory = new ControllerConfig.Factory();
            }
            Debug.logInfo("Initialized ControllerConfigFactory from properties (" 
                + resource + "#" + property + "): " + factory.getClass().getName(), module);
            return factory;
        }
    }

    public static class ControllerConfig {
        // SCIPIO: special key for cache lookups that return null
        public static final ControllerConfig NULL_CONFIG = new ControllerConfig();
        
        public URL url;
        // SCIPIO: switched all to protected from private (see ResolvedControllerConfig)
        protected String errorpage;
        protected String protectView;
        protected String owner;
        protected String securityClass;
        protected String defaultRequest;
        protected String statusCode;
        // SCIPIO: extended info on includes needed
        //protected List<URL> includes = new ArrayList<URL>();
        protected List<Include> includes = new ArrayList<>();
        // SCIPIO: split-up includes
        protected List<Include> includesPreLocal = new ArrayList<>();
        protected List<Include> includesPostLocal = new ArrayList<>();
        protected Map<String, Event> firstVisitEventList = new LinkedHashMap<String, Event>(); // SCIPIO: 2018-03-13: should be ordered!
        protected Map<String, Event> preprocessorEventList = new LinkedHashMap<String, Event>(); // SCIPIO: 2018-03-13: should be ordered!
        protected Map<String, Event> postprocessorEventList = new LinkedHashMap<String, Event>(); // SCIPIO: 2018-03-13: should be ordered!
        protected Map<String, Event> afterLoginEventList = new LinkedHashMap<String, Event>(); // SCIPIO: 2018-03-13: should be ordered!
        protected Map<String, Event> beforeLogoutEventList = new LinkedHashMap<String, Event>(); // SCIPIO: 2018-03-13: should be ordered!
        protected Map<String, String> eventHandlerMap = new HashMap<String, String>();
        protected Map<String, String> viewHandlerMap = new HashMap<String, String>();
        protected Map<String, RequestMap> requestMapMap = new HashMap<String, RequestMap>();
        protected Map<String, ViewMap> viewMapMap = new HashMap<String, ViewMap>();
        protected ViewAsJsonConfig viewAsJsonConfig; // SCIPIO: added 2017-05-15
        protected Boolean allowViewSaveDefault; // SCIPIO: added 2018-06-13
        protected List<NameFilter<Boolean>> allowViewSaveViewNameFilters; // SCIPIO: added 2018-06-13
        
        public ControllerConfig(URL url) throws WebAppConfigurationException {
            this.url = url;
            Element rootElement = loadDocument(url);
            if (rootElement != null) {
                long startTime = System.currentTimeMillis();
                loadIncludes(rootElement);
                loadGeneralConfig(rootElement);
                loadHandlerMap(rootElement);
                loadRequestMap(rootElement);
                loadViewMap(rootElement);
                if (Debug.infoOn()) {
                    double totalSeconds = (System.currentTimeMillis() - startTime) / 1000.0;
                    String locString = this.url.toExternalForm();
                    Debug.logInfo("controller loaded: " + totalSeconds + "s, " + this.requestMapMap.size() + " requests, " + this.viewMapMap.size() + " views in " + locString, module);
                }
            }
        }
        
        private ControllerConfig() { // SCIPIO: special null config 
            this.url = null;
        }

        public static class Factory extends ControllerConfigFactory { // SCIPIO
            @Override
            public ControllerConfig readControllerConfig(URL url) throws WebAppConfigurationException {
                return new ControllerConfig(url);
            }
        }

        public boolean isNull() { // SCIPIO: special
            return this.url == null;
        }

        // SCIPIO: all calls below modified for more complex include options (non-recursive include)
        
        public Map<String, Event> getAfterLoginEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getAfterLoginEventList());
                    } else {
                        result.push(controllerConfig.afterLoginEventList);
                    }
                }
            }
            result.push(afterLoginEventList);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getAfterLoginEventList());
                    } else {
                        result.push(controllerConfig.afterLoginEventList);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getBeforeLogoutEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getBeforeLogoutEventList());
                    } else {
                        result.push(controllerConfig.beforeLogoutEventList);
                    }
                }
            }
            result.push(beforeLogoutEventList);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getBeforeLogoutEventList());
                    } else {
                        result.push(controllerConfig.beforeLogoutEventList);
                    }
                }
            }
            return result;
        }

        public String getDefaultRequest() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String defaultRequest = controllerConfig.getDefaultRequest();
                    String defaultRequest;
                    if (include.recursive) {
                        defaultRequest = controllerConfig.getDefaultRequest();
                    } else {
                        defaultRequest = controllerConfig.defaultRequest;
                    }
                    if (defaultRequest != null) {
                        return defaultRequest;
                    }
                }
            }
            if (defaultRequest != null) {
                return defaultRequest;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String defaultRequest = controllerConfig.getDefaultRequest();
                    String defaultRequest;
                    if (include.recursive) {
                        defaultRequest = controllerConfig.getDefaultRequest();
                    } else {
                        defaultRequest = controllerConfig.defaultRequest;
                    }
                    if (defaultRequest != null) {
                        return defaultRequest;
                    }
                }
            }
            return null;
        }

        public String getErrorpage() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String errorpage = controllerConfig.getErrorpage();
                    String errorpage;
                    if (include.recursive) {
                        errorpage = controllerConfig.getErrorpage();
                    } else {
                        errorpage = controllerConfig.errorpage;
                    }
                    if (errorpage != null) {
                        return errorpage;
                    }
                }
            }
            if (errorpage != null) {
                return errorpage;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String errorpage = controllerConfig.getErrorpage();
                    String errorpage;
                    if (include.recursive) {
                        errorpage = controllerConfig.getErrorpage();
                    } else {
                        errorpage = controllerConfig.errorpage;
                    }
                    if (errorpage != null) {
                        return errorpage;
                    }
                }
            }
            return null;
        }

        public Map<String, String> getEventHandlerMap() throws WebAppConfigurationException {
            MapContext<String, String> result = MapContext.getMapContext();
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getEventHandlerMap());
                    } else {
                        result.push(controllerConfig.eventHandlerMap);
                    }
                }
            }
            result.push(eventHandlerMap);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getEventHandlerMap());
                    } else {
                        result.push(controllerConfig.eventHandlerMap);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getFirstVisitEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getFirstVisitEventList());
                    } else {
                        result.push(controllerConfig.firstVisitEventList);
                    }
                }
            }
            result.push(firstVisitEventList);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getFirstVisitEventList());
                    } else {
                        result.push(controllerConfig.firstVisitEventList);
                    }
                }
            }
            return result;
        }

        public String getOwner() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String owner = controllerConfig.getOwner();
                    String owner;
                    if (include.recursive) {
                        owner = controllerConfig.getOwner();
                    } else {
                        owner = controllerConfig.owner;
                    }
                    if (owner != null) {
                        return owner;
                    }
                }
            }
            if (owner != null) {
                return owner;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String owner = controllerConfig.getOwner();
                    String owner;
                    if (include.recursive) {
                        owner = controllerConfig.getOwner();
                    } else {
                        owner = controllerConfig.owner;
                    }
                    if (owner != null) {
                        return owner;
                    }
                }
            }
            return null;
        }

        public Map<String, Event> getPostprocessorEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostprocessorEventList());
                    } else {
                        result.push(controllerConfig.postprocessorEventList);
                    }
                }
            }
            result.push(postprocessorEventList);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostprocessorEventList());
                    } else {
                        result.push(controllerConfig.postprocessorEventList);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getPreprocessorEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreprocessorEventList());
                    } else {
                        result.push(controllerConfig.preprocessorEventList);
                    }
                }
            }
            result.push(preprocessorEventList);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreprocessorEventList());
                    } else {
                        result.push(controllerConfig.preprocessorEventList);
                    }
                }
            }
            return result;
        }

        public String getProtectView() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String protectView = controllerConfig.getProtectView();
                    String protectView;
                    if (include.recursive) {
                        protectView = controllerConfig.getProtectView();
                    } else {
                        protectView = controllerConfig.protectView;
                    }
                    if (protectView != null) {
                        return protectView;
                    }
                }
            }
            if (protectView != null) {
                return protectView;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String protectView = controllerConfig.getProtectView();
                    String protectView;
                    if (include.recursive) {
                        protectView = controllerConfig.getProtectView();
                    } else {
                        protectView = controllerConfig.protectView;
                    }
                    if (protectView != null) {
                        return protectView;
                    }
                }
            }
            return null;
        }

        public Map<String, RequestMap> getRequestMapMap() throws WebAppConfigurationException {
            MapContext<String, RequestMap> result = MapContext.getMapContext();
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getRequestMapMap());
                    } else {
                        result.push(controllerConfig.requestMapMap);
                    }
                }
            }
            result.push(requestMapMap);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getRequestMapMap());
                    } else {
                        result.push(controllerConfig.requestMapMap);
                    }
                }
            }
            return result;
        }

        public String getSecurityClass() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String securityClass = controllerConfig.getSecurityClass();
                    String securityClass;
                    if (include.recursive) {
                        securityClass = controllerConfig.getSecurityClass();
                    } else {
                        securityClass = controllerConfig.securityClass;
                    }
                    if (securityClass != null) {
                        return securityClass;
                    }
                }
            }
            if (securityClass != null) {
                return securityClass;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String securityClass = controllerConfig.getSecurityClass();
                    String securityClass;
                    if (include.recursive) {
                        securityClass = controllerConfig.getSecurityClass();
                    } else {
                        securityClass = controllerConfig.securityClass;
                    }
                    if (securityClass != null) {
                        return securityClass;
                    }
                }
            }
            return null;
        }

        public String getStatusCode() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String statusCode = controllerConfig.getStatusCode();
                    String statusCode;
                    if (include.recursive) {
                        statusCode = controllerConfig.getStatusCode();
                    } else {
                        statusCode = controllerConfig.statusCode;
                    }
                    if (statusCode != null) {
                        return statusCode;
                    }
                }
            }
            if (statusCode != null) {
                return statusCode;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    //String statusCode = controllerConfig.getStatusCode();
                    String statusCode;
                    if (include.recursive) {
                        statusCode = controllerConfig.getStatusCode();
                    } else {
                        statusCode = controllerConfig.statusCode;
                    }
                    if (statusCode != null) {
                        return statusCode;
                    }
                }
            }
            return null;
        }

        public Map<String, String> getViewHandlerMap() throws WebAppConfigurationException {
            MapContext<String, String> result = MapContext.getMapContext();
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getViewHandlerMap());
                    } else {
                        result.push(controllerConfig.viewHandlerMap);
                    }
                }
            }
            result.push(viewHandlerMap);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getViewHandlerMap());
                    } else {
                        result.push(controllerConfig.viewHandlerMap);
                    }
                }
            }
            return result;
        }

        public Map<String, ViewMap> getViewMapMap() throws WebAppConfigurationException {
            MapContext<String, ViewMap> result = MapContext.getMapContext();
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getViewMapMap());
                    } else {
                        result.push(controllerConfig.viewMapMap);
                    }
                }
            }
            result.push(viewMapMap);
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getViewMapMap());
                    } else {
                        result.push(controllerConfig.viewMapMap);
                    }
                }
            }
            return result;
        }

        /**
         * SCIPIO: returns view-as-json configuration, corresponding to site-conf.xsd view-as-json element.
         */
        public ViewAsJsonConfig getViewAsJsonConfig() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    ViewAsJsonConfig viewAsJsonConfig;
                    if (include.recursive) {
                        viewAsJsonConfig = controllerConfig.getViewAsJsonConfig();
                    } else {
                        viewAsJsonConfig = controllerConfig.viewAsJsonConfig;
                    }
                    if (viewAsJsonConfig != null) {
                        return viewAsJsonConfig;
                    }
                }
            }
            if (viewAsJsonConfig != null) {
                return viewAsJsonConfig;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    ViewAsJsonConfig viewAsJsonConfig;
                    if (include.recursive) {
                        viewAsJsonConfig = controllerConfig.getViewAsJsonConfig();
                    } else {
                        viewAsJsonConfig = controllerConfig.viewAsJsonConfig;
                    }
                    if (viewAsJsonConfig != null) {
                        return viewAsJsonConfig;
                    }
                }
            }
            return null;
        }

        /**
         * SCIPIO: returns view-as-json configuration, corresponding to site-conf.xsd view-as-json element.
         */
        public ViewAsJsonConfig getViewAsJsonConfigOrDefault() throws WebAppConfigurationException {
            ViewAsJsonConfig config = getViewAsJsonConfig();
            return config != null ? config : new ViewAsJsonConfig();
        }

        /**
         * SCIPIO: returns view-as-json configuration, corresponding to site-conf.xsd view-as-json element.
         */
        public Boolean getAllowViewSaveDefault() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    Boolean result;
                    if (include.recursive) {
                        result = controllerConfig.getAllowViewSaveDefault();
                    } else {
                        result = controllerConfig.allowViewSaveDefault;
                    }
                    if (result != null) {
                        return result;
                    }
                }
            }
            if (allowViewSaveDefault != null) {
                return allowViewSaveDefault;
            }
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    Boolean result;
                    if (include.recursive) {
                        result = controllerConfig.getAllowViewSaveDefault();
                    } else {
                        result = controllerConfig.allowViewSaveDefault;
                    }
                    if (result != null) {
                        return result;
                    }
                }
            }
            return null;
        }

        /**
         * SCIPIO: returns allowViewSaveViewNameFilters.
         */
        public List<NameFilter<Boolean>> getAllowViewSaveViewNameFilters() throws WebAppConfigurationException {
            List<NameFilter<Boolean>> result = new ArrayList<>();
            for (Include include : includesPreLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.addAll(controllerConfig.getAllowViewSaveViewNameFilters());
                    } else if (controllerConfig.allowViewSaveViewNameFilters != null) {
                        result.addAll(controllerConfig.allowViewSaveViewNameFilters);
                    }
                }
            }
            if (this.allowViewSaveViewNameFilters != null) {
                result.addAll(this.allowViewSaveViewNameFilters);
            }
            for (Include include : includesPostLocal) {
                ControllerConfig controllerConfig = getControllerConfig(include.location, include.optional);
                if (controllerConfig != null) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.addAll(controllerConfig.getAllowViewSaveViewNameFilters());
                    } else if (controllerConfig.allowViewSaveViewNameFilters != null) {
                        result.addAll(controllerConfig.allowViewSaveViewNameFilters);
                    }
                }
            }
            return result;
        }

        private void loadGeneralConfig(Element rootElement) {
            this.errorpage = UtilXml.childElementValue(rootElement, "errorpage");
            this.statusCode = UtilXml.childElementValue(rootElement, "status-code");
            Element protectElement = UtilXml.firstChildElement(rootElement, "protect");
            if (protectElement != null) {
                this.protectView = protectElement.getAttribute("view");
            }
            this.owner = UtilXml.childElementValue(rootElement, "owner");
            this.securityClass = UtilXml.childElementValue(rootElement, "security-class");
            Element defaultRequestElement = UtilXml.firstChildElement(rootElement, "default-request");
            if (defaultRequestElement != null) {
                this.defaultRequest = defaultRequestElement.getAttribute("request-uri");
            }
            // first visit event
            Element firstvisitElement = UtilXml.firstChildElement(rootElement, "firstvisit");
            if (firstvisitElement != null) {
                for (Element eventElement : UtilXml.childElementList(firstvisitElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (UtilValidate.isEmpty(eventName)) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.firstVisitEventList.put(eventName, new Event(eventElement));
                }
            }
            // preprocessor events
            Element preprocessorElement = UtilXml.firstChildElement(rootElement, "preprocessor");
            if (preprocessorElement != null) {
                for (Element eventElement : UtilXml.childElementList(preprocessorElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (UtilValidate.isEmpty(eventName)) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.preprocessorEventList.put(eventName, new Event(eventElement));
                }
            }
            // postprocessor events
            Element postprocessorElement = UtilXml.firstChildElement(rootElement, "postprocessor");
            if (postprocessorElement != null) {
                for (Element eventElement : UtilXml.childElementList(postprocessorElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (UtilValidate.isEmpty(eventName)) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.postprocessorEventList.put(eventName, new Event(eventElement));
                }
            }
            // after-login events
            Element afterLoginElement = UtilXml.firstChildElement(rootElement, "after-login");
            if (afterLoginElement != null) {
                for (Element eventElement : UtilXml.childElementList(afterLoginElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (UtilValidate.isEmpty(eventName)) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.afterLoginEventList.put(eventName, new Event(eventElement));
                }
            }
            // before-logout events
            Element beforeLogoutElement = UtilXml.firstChildElement(rootElement, "before-logout");
            if (beforeLogoutElement != null) {
                for (Element eventElement : UtilXml.childElementList(beforeLogoutElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (UtilValidate.isEmpty(eventName)) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.beforeLogoutEventList.put(eventName, new Event(eventElement));
                }
            }
            // SCIPIO: new
            Element viewAsJsonElement = UtilXml.firstChildElement(rootElement, "view-as-json");
            if (viewAsJsonElement != null) {
                this.viewAsJsonConfig = new ViewAsJsonConfig(viewAsJsonElement);
            } else {
                this.viewAsJsonConfig = null;
            }
            // SCIPIO: new
            Boolean allowViewSaveDefault = null;
            ArrayList<NameFilter<Boolean>> allowViewSaveViewNameFilters = null;
            Element commonSettingsElem = UtilXml.firstChildElement(rootElement, "common-settings");
            if (commonSettingsElem != null) {
                Element requestMapSettingsElem = UtilXml.firstChildElement(commonSettingsElem, "request-map-settings");
                if (requestMapSettingsElem != null) {
                    Element responseSettingsElem = UtilXml.firstChildElement(requestMapSettingsElem, "response-settings");
                    if (responseSettingsElem != null) {
                        Element allowViewSaveDefaultElem = UtilXml.firstChildElement(responseSettingsElem, "allow-view-save-default");
                        if (allowViewSaveDefaultElem != null) {
                            allowViewSaveDefault = UtilMisc.booleanValue(allowViewSaveDefaultElem.getAttribute("value"));
                            List<? extends Element> avsdFilterByNameElems = UtilXml.childElementList(allowViewSaveDefaultElem, "name-filter");
                            allowViewSaveViewNameFilters = new ArrayList<>(avsdFilterByNameElems.size());
                            for(Element avdsFilterByNameElem : avsdFilterByNameElems) {
                                if ("view-name".equals(avdsFilterByNameElem.getAttribute("field"))) {
                                    NameFilter<Boolean> nameFilter = NameFilter.fromElement(avdsFilterByNameElem, Boolean.class);
                                    allowViewSaveViewNameFilters.add(nameFilter);
                                }
                            }
                            allowViewSaveViewNameFilters.trimToSize();
                        }
                    }
                }
            }
            this.allowViewSaveDefault = allowViewSaveDefault;
            this.allowViewSaveViewNameFilters = allowViewSaveViewNameFilters;
        }
        
        private void loadHandlerMap(Element rootElement) {
            for (Element handlerElement : UtilXml.childElementList(rootElement, "handler")) {
                String name = handlerElement.getAttribute("name");
                String type = handlerElement.getAttribute("type");
                String className = handlerElement.getAttribute("class");

                if ("view".equals(type)) {
                    this.viewHandlerMap.put(name, className);
                } else {
                    this.eventHandlerMap.put(name, className);
                }
            }
        }

        protected void loadIncludes(Element rootElement) {
            for (Element includeElement : UtilXml.childElementList(rootElement, "include")) {
                String includeLocation = includeElement.getAttribute("location");
                if (UtilValidate.isNotEmpty(includeLocation)) {
                    // SCIPIO: support non-recursive
                    boolean recursive = !"no".equals(includeElement.getAttribute("recursive"));
                    boolean optional = "true".equals(includeElement.getAttribute("optional"));
                    try {
                        URL urlLocation = FlexibleLocation.resolveLocation(includeLocation);
                        String order = includeElement.getAttribute("order");
                        Include include = new Include(urlLocation, recursive, optional, order);
                        includes.add(include);
                        if (include.isPostLocal()) {
                            includesPostLocal.add(include);
                        } else {
                            includesPreLocal.add(include);
                        }
                    } catch (ComponentNotFoundURLException mue) { // SCIPIO: 2017-08-03: special case needed for missing component
                        if (optional) {
                            if (Debug.verboseOn()) Debug.logVerbose("Skipping optional processing include at [" + includeLocation + "]: component not found", module);
                        } else {
                            Debug.logError(mue, "Error processing include at [" + includeLocation + "]: " + mue.toString(), module);
                        }
                    } catch (MalformedURLException mue) {
                        Debug.logError(mue, "Error processing include at [" + includeLocation + "]: " + mue.toString(), module); // SCIPIO: 2017-08-03: typo fix
                    }
                }
            }
        }

        private void loadRequestMap(Element root) {
            for (Element requestMapElement : UtilXml.childElementList(root, "request-map")) {
                RequestMap requestMap = new RequestMap(requestMapElement);
                this.requestMapMap.put(requestMap.uri, requestMap);
            }
        }

        private void loadViewMap(Element rootElement) {
            for (Element viewMapElement : UtilXml.childElementList(rootElement, "view-map")) {
                ViewMap viewMap = new ViewMap(viewMapElement);
                this.viewMapMap.put(viewMap.name, viewMap);
            }
        }

        // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
        
        public URL getUrl() {
            return url;
        }
        
        private <K, V> MapContext<K, V> getMapContextForEventList() { // SCIPIO: refactored into factory method
            return MapContext.getMapContext();
        }

        /**
         * SCIPIO: Include with support for non-recursive.
         */
        public static class Include {
            public final URL location;
            public final boolean recursive;
            public final boolean optional; // SCIPIO: added 2017-05-03
            public final Order order; // SCIPIO: added 2017-05-03
            
            public Include(URL location, boolean recursive, boolean optional, Order order) {
                this.location = location;
                this.recursive = recursive;
                this.optional = optional;
                this.order = order;
            }
            
            public Include(URL location, boolean recursive, boolean optional, String order) {
                this(location, recursive, optional, Order.fromName(order));
            }
            
            public Include(URL location, boolean recursive) {
                this(location, recursive, false, Order.PRE_LOCAL);
            }
            
            public boolean isPostLocal() {
                return this.order == Order.POST_LOCAL;
            }
            public enum Order {
                PRE_LOCAL,
                POST_LOCAL;
                
                public static Order fromName(String name) {
                    if (name == null || name.isEmpty() || name.equals("pre-local")) {
                        return PRE_LOCAL;
                    } else if ("post-local".equals(name)) {
                        return POST_LOCAL;
                    } else {
                        throw new IllegalArgumentException("invalid controller include order value: " + name);
                    }
                }
            }
            
            // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
            
            public URL getLocation() {
                return location;
            }

            public boolean isRecursive() {
                return recursive;
            }

            public boolean isOptional() {
                return optional;
            }

            public Order getOrder() {
                return order;
            }
        }
    }

    /**
     * SCIPIO: Instance of ControllerConfig with all members completely pre-resolved
     * (prior to storage in cache).
     * <p>
     * TODO: In future this will have to be rewritten for thread safety using a ControllerConfig
     * interface, but cache _may_ save this for now...
     * <p>
     * Added 2018-06-13.
     */
    public static class ResolvedControllerConfig extends ControllerConfig {

        protected final ViewAsJsonConfig viewAsJsonConfigOrDefault;
        
        public ResolvedControllerConfig(URL url) throws WebAppConfigurationException {
            super(url);
            
            this.errorpage = super.getErrorpage();
            this.protectView = super.getProtectView();
            this.owner = super.getOwner();
            this.securityClass = super.getSecurityClass();
            this.defaultRequest = super.getDefaultRequest();
            this.statusCode = super.getStatusCode();

            // SCIPIO: split-up includes
            this.firstVisitEventList = getOrderedOptMap(super.getFirstVisitEventList());
            this.preprocessorEventList = getOrderedOptMap(super.getPreprocessorEventList());
            this.postprocessorEventList = getOrderedOptMap(super.getPostprocessorEventList());
            this.afterLoginEventList = getOrderedOptMap(super.getAfterLoginEventList());
            this.beforeLogoutEventList = getOrderedOptMap(super.getBeforeLogoutEventList());
            this.eventHandlerMap = getOptMap(super.getEventHandlerMap());
            this.viewHandlerMap = getOptMap(super.getViewHandlerMap());
            this.requestMapMap = getOptMap(super.getRequestMapMap());
            this.viewMapMap = getOptMap(super.getViewMapMap());
            this.viewAsJsonConfig = super.getViewAsJsonConfig(); // SCIPIO: added 2017-05-15
            this.allowViewSaveDefault = super.getAllowViewSaveDefault(); // SCIPIO: added 2018-06-13
            this.viewAsJsonConfigOrDefault = super.getViewAsJsonConfigOrDefault();
            this.allowViewSaveViewNameFilters = getOptList(super.getAllowViewSaveViewNameFilters()); // SCIPIO: added 2018-06-13
        }

        private static <K, V> Map<K, V> getOptMap(Map<K, V> map) {
            return new HashMap<>(map); // convert MapContext to much faster HashMap
        }
        
        private static <K, V> Map<K, V> getOrderedOptMap(Map<K, V> map) {
            // SCIPIO: 2018-06-14: FIXME: the MapContext iteration order will be wrong here...
            return new LinkedHashMap<>(map); // convert MapContext to much faster HashMap
        }

        private static <V> List<V> getOptList(List<V> list) {
            ArrayList<V> arrayList;
            if (list instanceof ArrayList) {
                arrayList = (ArrayList<V>) list;
            } else {
                arrayList = new ArrayList<>(list);
            }
            arrayList.trimToSize();
            return arrayList;
        }

        public static class Factory extends ControllerConfigFactory {
            @Override
            public ControllerConfig readControllerConfig(URL url) throws WebAppConfigurationException {
                return new ResolvedControllerConfig(url);
            }
        }
        
        @Override
        public Map<String, Event> getAfterLoginEventList() throws WebAppConfigurationException {
            return afterLoginEventList;
        }

        @Override
        public Map<String, Event> getBeforeLogoutEventList() throws WebAppConfigurationException {
            return beforeLogoutEventList;
        }

        @Override
        public String getDefaultRequest() throws WebAppConfigurationException {
            return defaultRequest;
        }

        @Override
        public String getErrorpage() throws WebAppConfigurationException {
            return errorpage;
        }

        @Override
        public Map<String, String> getEventHandlerMap() throws WebAppConfigurationException {
            return eventHandlerMap;
        }

        @Override
        public Map<String, Event> getFirstVisitEventList() throws WebAppConfigurationException {
            return firstVisitEventList;
        }

        @Override
        public String getOwner() throws WebAppConfigurationException {
            return owner;
        }

        @Override
        public Map<String, Event> getPostprocessorEventList() throws WebAppConfigurationException {
            return postprocessorEventList;
        }

        @Override
        public Map<String, Event> getPreprocessorEventList() throws WebAppConfigurationException {
            return preprocessorEventList;
        }

        @Override
        public String getProtectView() throws WebAppConfigurationException {
            return protectView;
        }

        @Override
        public Map<String, RequestMap> getRequestMapMap() throws WebAppConfigurationException {
            return requestMapMap;
        }

        @Override
        public String getSecurityClass() throws WebAppConfigurationException {
            return securityClass;
        }

        @Override
        public String getStatusCode() throws WebAppConfigurationException {
            return statusCode;
        }

        @Override
        public Map<String, String> getViewHandlerMap() throws WebAppConfigurationException {
            return viewHandlerMap;
        }

        @Override
        public Map<String, ViewMap> getViewMapMap() throws WebAppConfigurationException {
            return viewMapMap;
        }

        @Override
        public ViewAsJsonConfig getViewAsJsonConfig() throws WebAppConfigurationException {
            return viewAsJsonConfig;
        }

        @Override
        public ViewAsJsonConfig getViewAsJsonConfigOrDefault() throws WebAppConfigurationException {
            return viewAsJsonConfigOrDefault;
        }

        @Override
        public Boolean getAllowViewSaveDefault() throws WebAppConfigurationException {
            return allowViewSaveDefault;
        }

        @Override
        public List<NameFilter<Boolean>> getAllowViewSaveViewNameFilters() throws WebAppConfigurationException {
            return allowViewSaveViewNameFilters;
        }
    }
    
    public static class Event {
        public String type;
        public String path;
        public String invoke;
        public boolean globalTransaction = true;
        public Metrics metrics = null;
        public Boolean transaction = null; // SCIPIO: A generic transaction flag
        public String abortTransaction = ""; // SCIPIO: Allow aborting transaction 

        public Event(Element eventElement) {
            this.type = eventElement.getAttribute("type");
            this.path = eventElement.getAttribute("path");
            this.invoke = eventElement.getAttribute("invoke");
            this.globalTransaction = !"false".equals(eventElement.getAttribute("global-transaction"));
            // Get metrics.
            Element metricsElement = UtilXml.firstChildElement(eventElement, "metric");
            if (metricsElement != null) {
                this.metrics = MetricsFactory.getInstance(metricsElement);
            }
            // SCIPIO: new attribs
            String transStr = eventElement.getAttribute("transaction");
            if ("true".equals(transStr)) {
                transaction = Boolean.TRUE;
            } else if ("false".equals(transStr)) {
                transaction = Boolean.FALSE;
            } else {
                transaction = null;
            }
            this.abortTransaction = eventElement.getAttribute("abort-transaction");
        }

        public Event(String type, String path, String invoke, boolean globalTransaction) {
            this.type = type;
            this.path = path;
            this.invoke = invoke;
            this.globalTransaction = globalTransaction;
        }

        public Event(String type, String path, String invoke, boolean globalTransaction, Metrics metrics,
                Boolean transaction, String abortTransaction) {
            super();
            this.type = type;
            this.path = path;
            this.invoke = invoke;
            this.globalTransaction = globalTransaction;
            this.transaction = transaction;
            this.abortTransaction = abortTransaction;
        }

        // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
        
        public String getType() {
            return type;
        }

        public String getPath() {
            return path;
        }

        public String getInvoke() {
            return invoke;
        }

        public boolean isGlobalTransaction() {
            return globalTransaction;
        }

        public Metrics getMetrics() {
            return metrics;
        }

        public Boolean getTransaction() {
            return transaction;
        }

        public String getAbortTransaction() {
            return abortTransaction;
        }
    }

    public static class RequestMap {
        public String uri;
        public boolean edit = true;
        public boolean trackVisit = true;
        public boolean trackServerHit = true;
        public String description;
        public Event event;
        public boolean securityHttps = true;
        public boolean securityAuth = false;
        public boolean securityCert = false;
        public boolean securityExternalView = true;
        public boolean securityDirectRequest = true;
        public Map<String, RequestResponse> requestResponseMap = new HashMap<String, RequestResponse>();
        public Metrics metrics = null;

        public RequestMap(Element requestMapElement) {
            // Get the URI info
            this.uri = requestMapElement.getAttribute("uri");
            this.edit = !"false".equals(requestMapElement.getAttribute("edit"));
            this.trackServerHit = !"false".equals(requestMapElement.getAttribute("track-serverhit"));
            this.trackVisit = !"false".equals(requestMapElement.getAttribute("track-visit"));
            // Check for security
            Element securityElement = UtilXml.firstChildElement(requestMapElement, "security");
            if (securityElement != null) {
                if (!UtilProperties.propertyValueEqualsIgnoreCase("url", "no.http", "Y")) {
                    // SCIPIO: 2018-07-09: default is now true
                    //this.securityHttps = "true".equals(securityElement.getAttribute("https"));
                    this.securityHttps = !"false".equals(securityElement.getAttribute("https"));
                } else {
                    String httpRequestMapList = UtilProperties.getPropertyValue("url", "http.request-map.list");
                    if (UtilValidate.isNotEmpty(httpRequestMapList)) {
                        List<String> reqList = StringUtil.split(httpRequestMapList, ",");
                        if (reqList.contains(this.uri)) {
                            // SCIPIO: 2018-07-09: default is now true
                            //this.securityHttps = "true".equals(securityElement.getAttribute("https"));
                            this.securityHttps = !"false".equals(securityElement.getAttribute("https"));
                        }
                    }
                }
                this.securityAuth = "true".equals(securityElement.getAttribute("auth"));
                this.securityCert = "true".equals(securityElement.getAttribute("cert"));
                this.securityExternalView = !"false".equals(securityElement.getAttribute("external-view"));
                this.securityDirectRequest = !"false".equals(securityElement.getAttribute("direct-request"));
            }
            // Check for event
            Element eventElement = UtilXml.firstChildElement(requestMapElement, "event");
            if (eventElement != null) {
                this.event = new Event(eventElement);
            }
            // Check for description
            this.description = UtilXml.childElementValue(requestMapElement, "description");
            // Get the response(s)
            for (Element responseElement : UtilXml.childElementList(requestMapElement, "response")) {
                RequestResponse response = new RequestResponse(responseElement);
                requestResponseMap.put(response.name, response);
            }
            // Get metrics.
            Element metricsElement = UtilXml.firstChildElement(requestMapElement, "metric");
            if (metricsElement != null) {
                this.metrics = MetricsFactory.getInstance(metricsElement);
            }
        }

        // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
        
        public String getUri() {
            return uri;
        }

        public boolean isEdit() {
            return edit;
        }

        public boolean isTrackVisit() {
            return trackVisit;
        }

        public boolean isTrackServerHit() {
            return trackServerHit;
        }

        public String getDescription() {
            return description;
        }

        public Event getEvent() {
            return event;
        }

        public boolean isSecurityHttps() {
            return securityHttps;
        }

        public boolean isSecurityAuth() {
            return securityAuth;
        }

        public boolean isSecurityCert() {
            return securityCert;
        }

        public boolean isSecurityExternalView() {
            return securityExternalView;
        }

        public boolean isSecurityDirectRequest() {
            return securityDirectRequest;
        }

        public Map<String, RequestResponse> getRequestResponseMap() {
            return requestResponseMap;
        }

        public Metrics getMetrics() {
            return metrics;
        }
    }

    public static class RequestResponse {

        public static RequestResponse createEmptyNoneRequestResponse() {
            RequestResponse requestResponse = new RequestResponse();
            requestResponse.name = "empty-none";
            requestResponse.type = "none";
            // SCIPIO: This is an error; Element.getAttribute returns empty string if missing, so this is not equivalent to
            // rest of code
            //requestResponse.value = null;
            requestResponse.value = "";
            requestResponse.typeEnum = Type.NONE;// SCIPIO
            return requestResponse;
        }

        public String name;
        public String type;
        public String value;
        public String statusCode;
        public boolean saveLastView = false;
        public boolean saveCurrentView = false;
        public boolean saveHomeView = false;
        public Map<String, String> redirectParameterMap = new HashMap<String, String>();
        public Map<String, String> redirectParameterValueMap = new HashMap<String, String>();
        public Set<String> excludeParameterSet = null; // SCIPIO: new 2017-04-24
        public String includeMode = "auto"; // SCIPIO: new 2017-04-24
        public Boolean allowViewSave; // SCIPIO: new 2018-06-12: can be set explicit false to prevent recording this view
        private Type typeEnum; // SCIPIO: new 2018-06-13

        public RequestResponse() {
        }

        public RequestResponse(Element responseElement) {
            this.name = responseElement.getAttribute("name");
            this.type = responseElement.getAttribute("type");
            this.value = responseElement.getAttribute("value");
            this.statusCode = responseElement.getAttribute("status-code");
            this.saveLastView = "true".equals(responseElement.getAttribute("save-last-view"));
            this.saveCurrentView = "true".equals(responseElement.getAttribute("save-current-view"));
            this.saveHomeView = "true".equals(responseElement.getAttribute("save-home-view"));
            for (Element redirectParameterElement : UtilXml.childElementList(responseElement, "redirect-parameter")) {
                if (UtilValidate.isNotEmpty(redirectParameterElement.getAttribute("value"))) {
                    this.redirectParameterValueMap.put(redirectParameterElement.getAttribute("name"), redirectParameterElement.getAttribute("value"));
                } else {
                    String from = redirectParameterElement.getAttribute("from");
                    if (UtilValidate.isEmpty(from))
                        from = redirectParameterElement.getAttribute("name");
                    this.redirectParameterMap.put(redirectParameterElement.getAttribute("name"), from);
                }
            }
            // SCIPIO: new 2017-04-24
            Set<String> excludeParameterSet = new HashSet<>();
            for (Element redirectParametersElement : UtilXml.childElementList(responseElement, "redirect-parameters")) {
                for (Element redirectParameterElement : UtilXml.childElementList(redirectParametersElement, "param")) {
                    if ("exclude".equals(redirectParameterElement.getAttribute("mode"))) {
                        excludeParameterSet.add(redirectParameterElement.getAttribute("name"));
                    } else {
                        if (UtilValidate.isNotEmpty(redirectParameterElement.getAttribute("value"))) {
                            this.redirectParameterValueMap.put(redirectParameterElement.getAttribute("name"), redirectParameterElement.getAttribute("value"));
                        } else {
                            String from = redirectParameterElement.getAttribute("from");
                            if (UtilValidate.isEmpty(from))
                                from = redirectParameterElement.getAttribute("name");
                            this.redirectParameterMap.put(redirectParameterElement.getAttribute("name"), from);
                        }
                    }
                }
                this.includeMode = redirectParametersElement.getAttribute("include-mode");
            } 
            if (excludeParameterSet.size() > 0) {
                this.excludeParameterSet = Collections.unmodifiableSet(excludeParameterSet);
            }
            Boolean allowViewSave = UtilMisc.booleanValue(responseElement.getAttribute("allow-view-save")); // SCIPIO
            this.allowViewSave = allowViewSave;
            this.typeEnum = Type.fromName(this.type);
        }

        // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
        
        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }

        public String getValue() {
            return value;
        }

        public String getStatusCode() {
            return statusCode;
        }

        public boolean isSaveLastView() {
            return saveLastView;
        }

        public boolean isSaveCurrentView() {
            return saveCurrentView;
        }

        public boolean isSaveHomeView() {
            return saveHomeView;
        }

        public Map<String, String> getRedirectParameterMap() {
            return redirectParameterMap;
        }

        public Map<String, String> getRedirectParameterValueMap() {
            return redirectParameterValueMap;
        }

        public Set<String> getExcludeParameterSet() {
            return excludeParameterSet;
        }

        public String getIncludeMode() {
            return includeMode;
        }

        public Boolean getAllowViewSave() { // SCIPIO
            return allowViewSave;
        }
        
        public Type getTypeEnum() { // SCIPIO
            return typeEnum;
        }

        public enum Type { // SCIPIO: 2018-06
            NONE("none"),
            VIEW("view"),
            VIEW_LAST("view-last"),
            VIEW_LAST_NOPARAM("view-last-noparam"),
            VIEW_HOME("view-home"),
            REQUEST("request"),
            REQUEST_REDIRECT("request-redirect"),
            REQUEST_REDIRECT_NOPARAM("request-redirect-noparam"),
            URL("url"),
            CROSS_REDIRECT("cross-redirect");
            
            private static final Map<String, Type> nameMap;
            static {
                Map<String, Type> map = new HashMap<>();
                for(Type type : Type.values()) { map.put(type.getName(), type); }
                nameMap = map;
            }
            
            private final String name;
            private final boolean redirectType;
            private final boolean requestType;
            private final boolean viewType;
            
            private Type(String name) {
                this.name = name;
                this.redirectType = name.contains("redirect") || "url".equals(name);
                this.requestType = "request".equals(name);
                this.viewType = name.contains("view");
            }
            
            public static Type fromName(String name) {
                Type type = nameMap.get(name);
                if (type == null) throw new IllegalArgumentException("unrecognized"
                        + " controller request response type: " + name);
                return type;
            }

            public String getName() {
                return name;
            }

            public boolean isRedirectType() {
                return redirectType;
            }

            public boolean isRequestType() {
                return requestType;
            }

            public boolean isViewType() {
                return viewType;
            }
        }
    }

    public static class ViewMap {
        public String viewMap;
        public String name;
        public String page;
        public String type;
        public String info;
        public String contentType;
        public String encoding;
        public String xFrameOption;
        public String strictTransportSecurity;
        public String description;
        public boolean noCache = false;

        public ViewMap(Element viewMapElement) {
            this.name = viewMapElement.getAttribute("name");
            this.page = viewMapElement.getAttribute("page");
            this.type = viewMapElement.getAttribute("type");
            this.info = viewMapElement.getAttribute("info");
            this.contentType = viewMapElement.getAttribute("content-type");
            this.noCache = "true".equals(viewMapElement.getAttribute("no-cache"));
            this.encoding = viewMapElement.getAttribute("encoding");
            this.xFrameOption = viewMapElement.getAttribute("x-frame-options");
            this.strictTransportSecurity = viewMapElement.getAttribute("strict-transport-security");
            this.description = UtilXml.childElementValue(viewMapElement, "description");
            if (UtilValidate.isEmpty(this.page)) {
                this.page = this.name;
            }
        }

        // SCIPIO: Added getters for languages that can't read public properties (2017-05-08)
        
        public String getViewMap() {
            return viewMap;
        }

        public String getName() {
            return name;
        }

        public String getPage() {
            return page;
        }

        public String getType() {
            return type;
        }

        public String getInfo() {
            return info;
        }

        public String getContentType() {
            return contentType;
        }

        public String getEncoding() {
            return encoding;
        }

        public String getDescription() {
            return description;
        }

        public boolean isNoCache() {
            return noCache;
        }
    }

    /**
     * SCIPIO: Implements "view-as-json" element in site-conf.xsd.
     * Added 2017-05-15.
     */
    public static class ViewAsJsonConfig {
        public boolean enabled;
        public boolean updateSession;
        public boolean regularLogin;
        public String jsonRequestUri;
        
        public ViewAsJsonConfig(Element element) {
            this.enabled = UtilMisc.booleanValue(element.getAttribute("enabled"), false);
            this.updateSession = UtilMisc.booleanValue(element.getAttribute("update-session"), false);
            this.regularLogin = UtilMisc.booleanValue(element.getAttribute("regular-login"), false);
            this.jsonRequestUri = element.getAttribute("json-request-uri");
            if (jsonRequestUri.isEmpty()) this.jsonRequestUri = null;
        }
        
        public ViewAsJsonConfig() {
            // all false/null by default
        }

        public boolean isEnabled() {
            return enabled;
        }
        public boolean isUpdateSession() {
            return updateSession;
        }
        public boolean isRegularLogin() {
            return regularLogin;
        }
        public String getJsonRequestUri() {
            return jsonRequestUri;
        }
        public String getJsonRequestUriAlways() throws WebAppConfigurationException {
            if (jsonRequestUri == null) throw new WebAppConfigurationException(new IllegalStateException("Cannot forward view-as-json: missing json-request-uri configuration"));
            return jsonRequestUri;
        }
    }

    /**
     * SCIPIO: Name filter for controller filters.
     * Added 2018-06-13.
     */
    public static abstract class NameFilter<V> {
        private final V useValue;
        
        public NameFilter(V useValue) {
            this.useValue = useValue;
        }

        @SuppressWarnings("unchecked")
        public static <V> NameFilter<V> fromElement(Element element, Class<V> cls) {
            String useValueStr = element.getAttribute("use-value");
            V useValue = null;
            if (UtilValidate.isNotEmpty(useValueStr)) {
                if (String.class.isAssignableFrom(cls)) {
                    useValue = (V) useValueStr;
                } else {
                    try {
                        useValue = (V) ObjectType.simpleTypeConvert(useValueStr, cls.getName(), null, null);
                    } catch (GeneralException e) {
                        Debug.logError("Could not convert controller name filter result '" 
                                + useValueStr + "' to type: " + cls.getName(), module);
                    }
                }
            }
            if (element.getAttribute("prefix").length() > 0) {
                return new PrefixNameFilter(element.getAttribute("prefix"), useValue);
            } else if (element.getAttribute("suffix").length() > 0) {
                return new SuffixNameFilter(element.getAttribute("suffix"), useValue);
            } else if (element.getAttribute("regexp").length() > 0) {
                return new RegexNameFilter(element.getAttribute("regexp"), useValue);
            } else {
                return new FalseNameFilter(useValue);
            }
        }

        public V getUseValue() {
            return useValue;
        }
        
        public abstract boolean matches(String fieldValue);

        public static class FalseNameFilter<V> extends NameFilter<V> {
            public FalseNameFilter(V useValue) {
                super(useValue);
            }
            @Override
            public boolean matches(String fieldValue) {
                return false;
            }
        }
        
        public static class PrefixNameFilter<V> extends NameFilter<V> {
            private final String prefix;
            public PrefixNameFilter(String prefix, V useValue) {
                super(useValue);
                this.prefix = prefix;
            }
            @Override
            public boolean matches(String fieldValue) {
                return fieldValue.startsWith(prefix);
            }
        }
        
        public static class SuffixNameFilter<V> extends NameFilter<V> {
            private final String suffix;
            public SuffixNameFilter(String suffix, V useValue) {
                super(useValue);
                this.suffix = suffix;
            }
            @Override
            public boolean matches(String fieldValue) {
                return fieldValue.endsWith(suffix);
            }
        }
        public static class RegexNameFilter<V> extends NameFilter<V> {
            private final java.util.regex.Pattern pattern;
            public RegexNameFilter(String regex, V useValue) {
                super(useValue);
                this.pattern = java.util.regex.Pattern.compile(regex);
            }
            @Override
            public boolean matches(String fieldValue) {
                return pattern.matcher(fieldValue).matches();
            }
        }
    }
}
