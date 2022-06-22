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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.ilscipio.scipio.ce.util.servlet.FieldFilter;
import org.ofbiz.base.component.ComponentConfig;
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
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.collections.MapContext;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.webapp.event.EventUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * ConfigXMLReader.java - Reads and parses the XML site config files.
 * <p>
 * SCIPIO: NOTE: 2018-11-07: All public fields in these classes are now final, and should always have
 * been treated as final (thread safety and other reasons).
 * TODO?: Should probably use unmodifiable maps and lists everywhere...
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
     * SCIPIO: version of getControllerConfig that supports optional loading.
     * Added 2017-05-03.
     */
    private static ControllerConfig getControllerConfig(ControllerConfig.Include include) throws WebAppConfigurationException {
        return getControllerConfig(include.getLocation(), include.isOptional());
    }

    /**
     * SCIPIO: version of getControllerConfig that supports optional loading and wildcards.
     * Added 2017-05-03.
     */
    public static List<ControllerConfig> getControllerConfigs(URL url, boolean optional) throws WebAppConfigurationException {
        if (url.toString().startsWith("component://*/")) {
            if (!optional) {
                throw new WebAppConfigurationException(
                        new IllegalArgumentException("Wildcard include in the form component://*/ requires optional=\"true\""));
            }
            String path = url.toString().substring("component://*".length());
            Collection<ComponentConfig> components = ComponentConfig.getAllComponents();
            List<ControllerConfig> controllerConfigList = new ArrayList<>(components.size());
            for (ComponentConfig component : components) {
                if (!component.enabled()) {
                    continue;
                }
                URL specUrl;
                try {
                    specUrl = new URL("component://" + component.getGlobalName() + path);
                } catch (MalformedURLException e) {
                    throw new WebAppConfigurationException(e);
                }
                ControllerConfig controllerConfig = getControllerConfig(specUrl, true); // NOTE: Always optional here
                if (controllerConfig != null) {
                    controllerConfigList.add(controllerConfig);
                }
            }
            return controllerConfigList;
        } else {
            ControllerConfig controllerConfig = getControllerConfig(url, optional);
            if (controllerConfig == null) {
                return Collections.emptyList();
            }
            List<ControllerConfig> controllerConfigList = new ArrayList<>(1);
            controllerConfigList.add(controllerConfig);
            return controllerConfigList;
        }
    }

    /**
     * SCIPIO: version of getControllerConfig that supports optional loading and wildcards.
     * Added 2017-05-03.
     */
    private static List<ControllerConfig> getControllerConfigs(ControllerConfig.Include include) throws WebAppConfigurationException {
        return getControllerConfigs(include.getLocation(), include.isOptional());
    }

    /**
     * SCIPIO: version of getControllerConfig that bypasses cache.
     * Added 2018-06-13.
     */
    public static ControllerConfig readControllerConfig(URL url, boolean optional) throws WebAppConfigurationException {
        try {
            return ControllerConfigFactory.getFactory().readControllerConfig(url);
        } catch(FatalWebAppConfigurationException e) {
            // TODO: REVIEW: This block allows factory to bypass the FileNotFoundException check 
            if (e.getCause() instanceof WebAppConfigurationException) {
                throw (WebAppConfigurationException) e.getCause();
            } else {
                throw new WebAppConfigurationException(e.getCause());
            }
        } catch(WebAppConfigurationException e) {
            if (optional && (e.getCause() instanceof java.io.FileNotFoundException)) {
                // SCIPIO: NOTE: Changed this to verbose to simplify and to make this consistent with loadIncludes
                // below, which also can generate a similar message (when component missing)
                // FIXME: SCIPIO: 2.1.0: Don't log at all to avoid clogging up logs from wildcard lookups, re-add logging later
                //if (Debug.verboseOn()) {
                //    Debug.logVerbose("controller skipped (not found, optional): " + url.toString(), module);
                //}
                return null;
            } else {
                throw e;
            }
        }
    }

    /**
     * SCIPIO: A hack for {@link #readControllerConfig} to allow passing through FileNotFoundException.
     * TODO: REVIEW: hackish
     */
    @SuppressWarnings("serial")
    private static class FatalWebAppConfigurationException extends WebAppConfigurationException {
        public FatalWebAppConfigurationException(Throwable t) {
            super(t);
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
            if (Debug.verboseOn()) {
                 Debug.logVerbose("Loaded XML Config - " + location, module);
            }
            return rootElement;
        } catch (java.io.FileNotFoundException e) { // SCIPIO: special case: let caller log this one, IF necessary
            throw new WebAppConfigurationException(e);
        } catch (Exception e) {
            // SCIPIO: not all components have a WebApp, so this should not be logged as an error.
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

        public static ControllerConfigFactory getFactoryFromProperty(String resource, String property) {
            String factoryClassName = UtilProperties.getPropertyValue(resource, property);
            ControllerConfigFactory factory;
            try {
                Class<? extends ControllerConfigFactory> factoryClass = UtilGenerics.cast(Thread.currentThread().getContextClassLoader().loadClass(factoryClassName));
                factory = factoryClass.getConstructor().newInstance();
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
        public static final ControllerConfig NULL_CONFIG; // SCIPIO: special key for cache lookups that return null
        static {
            ControllerConfig config = null;
            try {
                config = new ControllerConfig(null);
            } catch (Exception e) { // SCIPIO: NOTE: this is only here to satisfy compiler, never thrown when url == null
                Debug.logError(e, module);
            }
            NULL_CONFIG = config;
        }

        public final URL url;
        // SCIPIO: switched all to protected from private (see ResolvedControllerConfig)
        protected final String errorpage;
        protected final String protectView;
        protected final String owner;
        protected final String securityClass;
        protected final String defaultRequest;
        protected final Integer statusCodeNumber; // SCIPIO: Now an integer: String statusCode
        // SCIPIO: extended info on includes needed
        //protected List<URL> includes = new ArrayList<URL>();
        protected final List<Include> includes; // = new ArrayList<>();
        // SCIPIO: split-up includes
        protected final List<Include> includesPreLocal; // = new ArrayList<>();
        protected final List<Include> includesPostLocal; // = new ArrayList<>();
        protected final Map<String, Event> firstVisitEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> preprocessorEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> postprocessorEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> preViewRenderEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> postViewRenderEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> preScreenRenderEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> postScreenRenderEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> afterLoginEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> beforeLogoutEventList; // = new LinkedHashMap<String, Event>();
        protected final Map<String, Event> afterLogoutEventList; // = new LinkedHashMap<String, Event>(); // SCIPIO: added 2018-12-03
        protected final Map<String, String> eventHandlerMap; // = new HashMap<String, String>();
        protected final Map<String, String> viewHandlerMap; // = new HashMap<String, String>();
        protected final Map<String, RequestMap> requestMapMap; // = new HashMap<String, RequestMap>();
        protected final Map<String, ViewMap> viewMapMap; // = new HashMap<String, ViewMap>();
        protected final ViewAsJsonConfig viewAsJsonConfig; // SCIPIO: added 2017-05-15
        protected final Boolean allowViewSaveDefault; // SCIPIO: added 2018-06-13
        protected final List<NameFilter<Boolean>> allowViewSaveViewNameFilters; // SCIPIO: added 2018-06-13
        protected final String defaultViewLastView; // SCIPIO: added 2018-10-26
        protected final Map<String, EventHandlerWrapperDef> eventHandlerWrapperMap; // SCIPIO: added 2018-11-23
        protected final String defaultViewAccess; // SCIPIO: Added 2.1.0
        protected final FieldFilter requestParamFilter;

        // SCIPIO: DEV NOTE:
        // If you add any members to this class, make sure to reflect it in ResolvedControllerConfig further below!
        // i.e the public getters such as getRequestMapMap must be overridden in ResolvedControllerConfig!

        /**
         * SCIPIO: Copy of above fields, ugly kludge to avoid having to more invasive rewrites. Only
         * used during init.
         */
        private static class ConfigFields {
            protected String errorpage;
            protected String protectView;
            protected String owner;
            protected String securityClass;
            protected String defaultRequest;
            protected Integer statusCodeNumber; // SCIPIO: Now an integer: String statusCode
            // SCIPIO: extended info on includes needed
            //protected List<URL> includes = new ArrayList<URL>();
            protected List<Include> includes = new ArrayList<>();
            // SCIPIO: split-up includes
            protected List<Include> includesPreLocal = new ArrayList<>();
            protected List<Include> includesPostLocal = new ArrayList<>();
            protected Map<String, Event> firstVisitEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> preprocessorEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> postprocessorEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> preViewRenderEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> postViewRenderEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> preScreenRenderEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> postScreenRenderEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> afterLoginEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> beforeLogoutEventList = new LinkedHashMap<String, Event>();
            protected Map<String, Event> afterLogoutEventList = new LinkedHashMap<String, Event>();
            protected Map<String, String> eventHandlerMap = new LinkedHashMap<String, String>();
            protected Map<String, String> viewHandlerMap = new LinkedHashMap<String, String>();
            protected Map<String, RequestMap> requestMapMap = new LinkedHashMap<String, RequestMap>();
            protected Map<String, ViewMap> viewMapMap = new LinkedHashMap<String, ViewMap>();
            protected ViewAsJsonConfig viewAsJsonConfig; // SCIPIO: added 2017-05-15
            protected Boolean allowViewSaveDefault; // SCIPIO: added 2018-06-13
            protected List<NameFilter<Boolean>> allowViewSaveViewNameFilters; // SCIPIO: added 2018-06-13
            protected String defaultViewLastView; // SCIPIO: added 2018-10-26
            protected Map<String, EventHandlerWrapperDef> eventHandlerWrapperMap = new LinkedHashMap<>(); // SCIPIO: added 2018-11-23
            protected String defaultViewAccess; // SCIPIO: Added 2.1.0
            protected FieldFilter requestParamFilter; // SCIPIO: Added 2.1.0
        }

        public ControllerConfig(URL url) throws WebAppConfigurationException {
            this.url = url;
            Builder builder = new Builder(); // SCIPIO

            if (url != null) { // SCIPIO: Added condition (for ControllerConfig.NULL_CONFIG)
                Element rootElement = loadDocument(url);
                if (rootElement != null) {
                    long startTime = System.currentTimeMillis();
                    builder.loadIncludes(rootElement);
                    builder.loadGeneralConfig(rootElement);
                    builder.loadHandlerMap(rootElement);
                    builder.loadRequestMap(rootElement);
                    builder.loadViewMap(rootElement);
                    if (Debug.infoOn()) {
                        double totalSeconds = (System.currentTimeMillis() - startTime) / 1000.0;
                        String locString = this.url.toExternalForm();
                        Debug.logInfo("controller loaded: " + totalSeconds + "s, " + builder.requestMapMap.size() + " requests, " + builder.viewMapMap.size() + " views in " + locString, module);
                    }
                } else {
                    Debug.logError("No root element found for controller: " + url, module); // SCIPIO: Added log line, hopefully never happens
                }
            }

            // SCIPIO: Locals
            builder.optimizeFields();
            this.errorpage = builder.errorpage;
            this.protectView = builder.protectView;
            this.owner = builder.owner;
            this.securityClass = builder.securityClass;
            this.defaultRequest = builder.defaultRequest;
            this.statusCodeNumber = builder.statusCodeNumber;
            this.includes = builder.includes;
            this.includesPreLocal = builder.includesPreLocal;
            this.includesPostLocal = builder.includesPostLocal;
            this.firstVisitEventList = builder.firstVisitEventList;
            this.preprocessorEventList = builder.preprocessorEventList;
            this.postprocessorEventList = builder.postprocessorEventList;
            this.preViewRenderEventList = builder.preViewRenderEventList;
            this.postViewRenderEventList = builder.postViewRenderEventList;
            this.preScreenRenderEventList = builder.preScreenRenderEventList;
            this.postScreenRenderEventList = builder.postScreenRenderEventList;
            this.afterLoginEventList = builder.afterLoginEventList;
            this.beforeLogoutEventList = builder.beforeLogoutEventList;
            this.afterLogoutEventList = builder.afterLogoutEventList;
            this.eventHandlerMap = builder.eventHandlerMap;
            this.viewHandlerMap = builder.viewHandlerMap;
            this.requestMapMap = builder.requestMapMap;
            this.viewMapMap = builder.viewMapMap;
            this.viewAsJsonConfig = builder.viewAsJsonConfig;
            this.allowViewSaveDefault = builder.allowViewSaveDefault;
            this.allowViewSaveViewNameFilters = builder.allowViewSaveViewNameFilters;
            this.defaultViewLastView = builder.defaultViewLastView;
            this.eventHandlerWrapperMap = builder.eventHandlerWrapperMap;
            this.defaultViewAccess = builder.defaultViewAccess;
            this.requestParamFilter = builder.requestParamFilter;
        }

        /**
         * SCIPIO: Optimizing copy constructor: Copies config, with option to pre-resolve fields from 
         * the public getter operations.
         * <p>
         * NOTE: Passing useResolvedFields false makes this practically useless because fields should not be modified.
         * <p>
         * Used to implement {@link ConfigXMLReader.ResolvedControllerConfig}.
         */
        protected ControllerConfig(ControllerConfig srcConfig, boolean useResolvedFields) throws WebAppConfigurationException {
            this.url = srcConfig.url;

            if (useResolvedFields) {
                this.errorpage = srcConfig.getErrorpage();
                this.protectView = srcConfig.getProtectView();
                this.owner = srcConfig.getOwner();
                this.securityClass = srcConfig.getSecurityClass();
                this.defaultRequest = srcConfig.getDefaultRequest();
                this.statusCodeNumber = srcConfig.getStatusCodeNumber();
                this.includes = getOptList(srcConfig.includes);
                this.includesPreLocal = getOptList(srcConfig.includesPreLocal);
                this.includesPostLocal = getOptList(srcConfig.includesPostLocal);
                this.firstVisitEventList = getOrderedOptMap(srcConfig.getFirstVisitEventList());
                this.preprocessorEventList = getOrderedOptMap(srcConfig.getPreprocessorEventList());
                this.postprocessorEventList = getOrderedOptMap(srcConfig.getPostprocessorEventList());
                this.preViewRenderEventList = getOrderedOptMap(srcConfig.getPreViewRenderEventList());
                this.postViewRenderEventList = getOrderedOptMap(srcConfig.getPostViewRenderEventList());
                this.preScreenRenderEventList = getOrderedOptMap(srcConfig.getPreScreenRenderEventList());
                this.postScreenRenderEventList = getOrderedOptMap(srcConfig.getPostScreenRenderEventList());
                this.afterLoginEventList = getOrderedOptMap(srcConfig.getAfterLoginEventList());
                this.beforeLogoutEventList = getOrderedOptMap(srcConfig.getBeforeLogoutEventList());
                this.afterLogoutEventList = getOrderedOptMap(srcConfig.getAfterLogoutEventList());
                this.eventHandlerMap = getOptMap(srcConfig.getEventHandlerMap());
                this.viewHandlerMap = getOptMap(srcConfig.getViewHandlerMap());
                this.requestMapMap = getOptMap(srcConfig.getRequestMapMap());
                this.viewMapMap = getOptMap(srcConfig.getViewMapMap());
                this.viewAsJsonConfig = srcConfig.getViewAsJsonConfig(); // SCIPIO: added 2017-05-15
                this.allowViewSaveDefault = srcConfig.getAllowViewSaveDefault(); // SCIPIO: added 2018-06-13
                this.allowViewSaveViewNameFilters = getOptList(srcConfig.getAllowViewSaveViewNameFilters()); // SCIPIO: added 2018-06-13
                this.defaultViewLastView = srcConfig.getDefaultViewLastView();
                this.eventHandlerWrapperMap = getOrderedOptMap(srcConfig.getEventHandlerWrapperMap());
                this.defaultViewAccess = srcConfig.getDefaultViewAccess();
                this.requestParamFilter = srcConfig.getRequestParamFilter();
            } else {
                this.errorpage = srcConfig.errorpage;
                this.protectView = srcConfig.protectView;
                this.owner = srcConfig.owner;
                this.securityClass = srcConfig.securityClass;
                this.defaultRequest = srcConfig.defaultRequest;
                this.statusCodeNumber = srcConfig.statusCodeNumber;
                this.includes = srcConfig.includes;
                this.includesPreLocal = srcConfig.includesPreLocal;
                this.includesPostLocal = srcConfig.includesPostLocal;
                this.firstVisitEventList = srcConfig.firstVisitEventList;
                this.preprocessorEventList = srcConfig.preprocessorEventList;
                this.postprocessorEventList = srcConfig.postprocessorEventList;
                this.preViewRenderEventList = srcConfig.preViewRenderEventList;
                this.postViewRenderEventList = srcConfig.postViewRenderEventList;
                this.preScreenRenderEventList = srcConfig.preScreenRenderEventList;
                this.postScreenRenderEventList = srcConfig.postScreenRenderEventList;
                this.afterLoginEventList = srcConfig.afterLoginEventList;
                this.beforeLogoutEventList = srcConfig.beforeLogoutEventList;
                this.afterLogoutEventList = srcConfig.afterLogoutEventList;
                this.eventHandlerMap = srcConfig.eventHandlerMap;
                this.viewHandlerMap = srcConfig.viewHandlerMap;
                this.requestMapMap = srcConfig.requestMapMap;
                this.viewMapMap = srcConfig.viewMapMap;
                this.viewAsJsonConfig = srcConfig.viewAsJsonConfig;
                this.allowViewSaveDefault = srcConfig.allowViewSaveDefault;
                this.allowViewSaveViewNameFilters = srcConfig.allowViewSaveViewNameFilters;
                this.defaultViewLastView = srcConfig.defaultViewLastView;
                this.eventHandlerWrapperMap = srcConfig.eventHandlerWrapperMap;
                this.defaultViewAccess = srcConfig.defaultViewAccess;
                this.requestParamFilter = srcConfig.requestParamFilter;
            }
        }

        private static <K, V> Map<K, V> getOptMap(Map<K, V> map) { // SCIPIO: Used to optimize MapContext down to a simpler map.
            //return new HashMap<>(map); // convert MapContext to much faster HashMap
            // SCIPIO: 2.1.0: Use LinkedHashMap here too for predictable order
            return new LinkedHashMap<>(map);
        }

        protected static <K, V> Map<K, V> getOrderedOptMap(Map<K, V> map) { // SCIPIO: Used to optimize MapContext down to a simpler order-preserving map.
            // SCIPIO: 2018-06-14: FIXME: the MapContext iteration order will be wrong here (minor issue)...
            return new LinkedHashMap<>(map); // convert MapContext to much faster HashMap
        }

        protected static <V> List<V> getOptList(List<V> list) { // SCIPIO: Used to optimize LinkedList or other down to a fast list.
            ArrayList<V> arrayList;
            if (list instanceof ArrayList) {
                arrayList = (ArrayList<V>) list;
                // NO! do not modify original instance (ControllerConfig field)
                //arrayList.trimToSize();
            } else {
                arrayList = new ArrayList<>(list);
                arrayList.trimToSize();
            }
            return arrayList;
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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

        public Map<String, Event> getAfterLogoutEventList() throws WebAppConfigurationException { // SCIPIO
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getAfterLogoutEventList());
                    } else {
                        result.push(controllerConfig.afterLogoutEventList);
                    }
                }
            }
            result.push(afterLogoutEventList);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getAfterLogoutEventList());
                    } else {
                        result.push(controllerConfig.afterLogoutEventList);
                    }
                }
            }
            return result;
        }

        public String getDefaultRequest() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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

        /**
         * SCIPIO: Resolves and returns all event handler wrappers, by name.
         * Added 2018-11-23.
         */
        public Map<String, EventHandlerWrapperDef> getEventHandlerWrapperMap() throws WebAppConfigurationException { // SCIPIO
            MapContext<String, EventHandlerWrapperDef> result = MapContext.getMapContext();
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getEventHandlerWrapperMap());
                    } else {
                        result.push(controllerConfig.eventHandlerWrapperMap);
                    }
                }
            }
            result.push(eventHandlerWrapperMap);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getEventHandlerWrapperMap());
                    } else {
                        result.push(controllerConfig.eventHandlerWrapperMap);
                    }
                }
            }
            return result;
        }
        
        public Map<String, Event> getFirstVisitEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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

        public Map<String, Event> getPreViewRenderEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreViewRenderEventList());
                    } else {
                        result.push(controllerConfig.preViewRenderEventList);
                    }
                }
            }
            result.push(preViewRenderEventList);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreViewRenderEventList());
                    } else {
                        result.push(controllerConfig.preViewRenderEventList);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getPostViewRenderEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostViewRenderEventList());
                    } else {
                        result.push(controllerConfig.postViewRenderEventList);
                    }
                }
            }
            result.push(postViewRenderEventList);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostViewRenderEventList());
                    } else {
                        result.push(controllerConfig.postViewRenderEventList);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getPreScreenRenderEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreScreenRenderEventList());
                    } else {
                        result.push(controllerConfig.preScreenRenderEventList);
                    }
                }
            }
            result.push(preScreenRenderEventList);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPreScreenRenderEventList());
                    } else {
                        result.push(controllerConfig.preScreenRenderEventList);
                    }
                }
            }
            return result;
        }

        public Map<String, Event> getPostScreenRenderEventList() throws WebAppConfigurationException {
            MapContext<String, Event> result = getMapContextForEventList(); // SCIPIO: factory method
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostScreenRenderEventList());
                    } else {
                        result.push(controllerConfig.postScreenRenderEventList);
                    }
                }
            }
            result.push(postScreenRenderEventList);
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        result.push(controllerConfig.getPostScreenRenderEventList());
                    } else {
                        result.push(controllerConfig.postScreenRenderEventList);
                    }
                }
            }
            return result;
        }

        public String getProtectView() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        mergePushRequestMapMap(result, controllerConfig.getRequestMapMap(), controllerConfig);
                    } else {
                        mergePushRequestMapMap(result, controllerConfig.requestMapMap, controllerConfig);
                    }
                }
            }
            mergePushRequestMapMap(result, requestMapMap, this); // SCIPIO: pushMergeRequestMapMap
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        mergePushRequestMapMap(result, controllerConfig.getRequestMapMap(), controllerConfig);
                    } else {
                        mergePushRequestMapMap(result, controllerConfig.requestMapMap, controllerConfig);
                    }
                }
            }
            return result;
        }

        /**
         * SCIPIO: Add the requestMapMap on top of the result map context, honoring request-map override-mode.
         * <p>
         * TODO: REVIEW: The whole MapContext on top of these copies is probably slow for nothing, but for scipio not concerned with the performance
         * because this is cached by {@link ConfigXMLReader.ResolvedControllerConfig}. Maybe review in future.
         */
        private void mergePushRequestMapMap(MapContext<String, RequestMap> result, Map<String, RequestMap> requestMapMap, ControllerConfig controllerConfig) {
            Map<String, RequestMap> updatedRequestMapMap = new LinkedHashMap<>();
            for(Map.Entry<String, RequestMap> entry : requestMapMap.entrySet()) {
                String uri = entry.getKey();
                RequestMap requestMap = entry.getValue();
                if (requestMap.getOverrideMode() == RequestMap.OverrideMode.MERGE) {
                    RequestMap existingRequestMap = result.get(uri);
                    if (existingRequestMap != null) {
                        updatedRequestMapMap.put(uri, new RequestMap(existingRequestMap, requestMap));
                    } else {
                        updatedRequestMapMap.put(uri, requestMap);
                    }
                } else {
                    updatedRequestMapMap.put(uri, requestMap);
                }
            }
            result.push(updatedRequestMapMap);
        }

        public String getSecurityClass() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
            // SCIPIO: now delegating
            Integer statusCodeNumber = getStatusCodeNumber();
            return (statusCodeNumber != null) ? statusCodeNumber.toString() : null;
        }

        public Integer getStatusCodeNumber() throws WebAppConfigurationException { // SCIPIO
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    //String statusCode = controllerConfig.getStatusCode();
                    Integer statusCode;
                    if (include.recursive) {
                        statusCode = controllerConfig.getStatusCodeNumber();
                    } else {
                        statusCode = controllerConfig.statusCodeNumber;
                    }
                    if (statusCode != null) {
                        return statusCode;
                    }
                }
            }
            if (statusCodeNumber != null) {
                return statusCodeNumber;
            }
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    //String statusCode = controllerConfig.getStatusCode();
                    Integer statusCode;
                    if (include.recursive) {
                        statusCode = controllerConfig.getStatusCodeNumber();
                    } else {
                        statusCode = controllerConfig.statusCodeNumber;
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
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

        /**
         * SCIPIO: returns view-last default-view-name.
         */
        public String getDefaultViewLastView() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    String result;
                    if (include.recursive) {
                        result = controllerConfig.getDefaultViewLastView();
                    } else {
                        result = controllerConfig.defaultViewLastView;
                    }
                    if (result != null) {
                        return result;
                    }
                }
            }
            if (defaultViewLastView != null) {
                return defaultViewLastView;
            }
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    String result;
                    if (include.recursive) {
                        result = controllerConfig.getDefaultViewLastView();
                    } else {
                        result = controllerConfig.defaultViewLastView;
                    }
                    if (result != null) {
                        return result;
                    }
                }
            }
            return null;
        }

        public String getDefaultViewAccess() throws WebAppConfigurationException {
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    //String owner = controllerConfig.getOwner();
                    String defaultViewAccess;
                    if (include.recursive) {
                        defaultViewAccess = controllerConfig.getDefaultViewAccess();
                    } else {
                        defaultViewAccess = controllerConfig.defaultViewAccess;
                    }
                    if (defaultViewAccess != null) {
                        return defaultViewAccess;
                    }
                }
            }
            if (defaultViewAccess != null) {
                return defaultViewAccess;
            }
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    //String owner = controllerConfig.getOwner();
                    String defaultViewAccess;
                    if (include.recursive) {
                        defaultViewAccess = controllerConfig.getDefaultViewAccess();
                    } else {
                        defaultViewAccess = controllerConfig.defaultViewAccess;
                    }
                    if (defaultViewAccess != null) {
                        return defaultViewAccess;
                    }
                }
            }
            return null;
        }

        public FieldFilter getRequestParamFilter() throws WebAppConfigurationException {
            FieldFilter requestParamFilter = FieldFilter.EMPTY;
            for (Include include : includesPreLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        requestParamFilter = requestParamFilter.merge(controllerConfig.getRequestParamFilter());
                    } else if (controllerConfig.requestParamFilter != null) {
                        requestParamFilter = requestParamFilter.merge(controllerConfig.requestParamFilter);
                    }
                }
            }
            if (this.requestParamFilter != null) {
                requestParamFilter = requestParamFilter.merge(this.requestParamFilter);
            }
            for (Include include : includesPostLocal) {
                for(ControllerConfig controllerConfig : getControllerConfigs(include)) {
                    // SCIPIO: support non-recursive
                    if (include.recursive) {
                        requestParamFilter = requestParamFilter.merge(controllerConfig.getRequestParamFilter());
                    } else if (controllerConfig.requestParamFilter != null) {
                        requestParamFilter = requestParamFilter.merge(controllerConfig.requestParamFilter);
                    }
                }
            }
            return requestParamFilter;
        }

        protected static class Builder extends ConfigFields { // SCIPIO: 2018-11-07: ugly kludge for initialization

        private void optimizeFields() {
            ((ArrayList<Include>) this.includes).trimToSize();
            ((ArrayList<Include>) this.includesPreLocal).trimToSize();
            ((ArrayList<Include>) this.includesPostLocal).trimToSize();
        }

        private void loadGeneralConfig(Element rootElement) {
            this.errorpage = UtilXml.childElementValue(rootElement, "errorpage");
            String statusCode = UtilXml.childElementValue(rootElement, "status-code");
            Integer statusCodeNumber = null; // SCIPIO: Straight Integer
            if (UtilValidate.isNotEmpty(statusCode)) {
                try {
                    statusCodeNumber = Integer.parseInt(statusCode);
                } catch(NumberFormatException e) {
                    Debug.logError("Invalid status-code (" + statusCode + ") for controller", module);
                }
            }
            this.statusCodeNumber = statusCodeNumber;
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
                    if (eventName.isEmpty()) {
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
                    if (eventName.isEmpty()) {
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
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.postprocessorEventList.put(eventName, new Event(eventElement));
                }
            }
            // pre-view-render events
            Element preViewRenderElement = UtilXml.firstChildElement(rootElement, "pre-view-render");
            if (preViewRenderElement != null) {
                for (Element eventElement : UtilXml.childElementList(preViewRenderElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.preViewRenderEventList.put(eventName, new Event(eventElement));
                }
            }
            // post-view-render events
            Element postViewRenderElement = UtilXml.firstChildElement(rootElement, "post-view-render");
            if (postViewRenderElement != null) {
                for (Element eventElement : UtilXml.childElementList(postViewRenderElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.postViewRenderEventList.put(eventName, new Event(eventElement));
                }
            }
            // pre-screen-render events
            Element preScreenRenderElement = UtilXml.firstChildElement(rootElement, "pre-screen-render");
            if (preScreenRenderElement != null) {
                for (Element eventElement : UtilXml.childElementList(preScreenRenderElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.preScreenRenderEventList.put(eventName, new Event(eventElement));
                }
            }
            // post-screen-render events
            Element postScreenRenderElement = UtilXml.firstChildElement(rootElement, "post-screen-render");
            if (postScreenRenderElement != null) {
                for (Element eventElement : UtilXml.childElementList(postScreenRenderElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.postScreenRenderEventList.put(eventName, new Event(eventElement));
                }
            }
            // after-login events
            Element afterLoginElement = UtilXml.firstChildElement(rootElement, "after-login");
            if (afterLoginElement != null) {
                for (Element eventElement : UtilXml.childElementList(afterLoginElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
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
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.beforeLogoutEventList.put(eventName, new Event(eventElement));
                }
            }
            // SCIPIO: after-logout events
            Element afterLogoutElement = UtilXml.firstChildElement(rootElement, "after-logout");
            if (afterLogoutElement != null) {
                for (Element eventElement : UtilXml.childElementList(afterLogoutElement, "event")) {
                    String eventName = eventElement.getAttribute("name");
                    if (eventName.isEmpty()) {
                        eventName = eventElement.getAttribute("type") + "::" + eventElement.getAttribute("path") + "::" + eventElement.getAttribute("invoke");
                    }
                    this.afterLogoutEventList.put(eventName, new Event(eventElement));
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
            String defaultViewLastView = null;
            ArrayList<NameFilter<Boolean>> allowViewSaveViewNameFilters = null;
            Element commonSettingsElem = UtilXml.firstChildElement(rootElement, "common-settings");
            String defaultViewAccess = null;
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
                        Element viewLastElement = UtilXml.firstChildElement(responseSettingsElem, "view-last");
                        if (viewLastElement != null) {
                            defaultViewLastView = viewLastElement.getAttribute("default-view");
                        }
                    }
                }
                Element viewMapSettingsElem = UtilXml.firstChildElement(commonSettingsElem, "view-map-settings");
                if (viewMapSettingsElem != null) {
                    defaultViewAccess = UtilValidate.nullIfEmpty(viewMapSettingsElem.getAttribute("default-view-access"));
                }
            }
            this.allowViewSaveDefault = allowViewSaveDefault;
            this.allowViewSaveViewNameFilters = allowViewSaveViewNameFilters;
            this.defaultViewLastView = (UtilValidate.isNotEmpty(defaultViewLastView) && !"_none_".equals(defaultViewLastView)) ? defaultViewLastView : null;
            this.defaultViewAccess = defaultViewAccess;
            FieldFilter requestParamFilter = FieldFilter.EMPTY;
            Element iofElem = UtilXml.firstChildElement(rootElement, "input-output-filters");
            if (iofElem != null) {
                Element rptaElem = UtilXml.firstChildElement(iofElem, "request-parameter-filter");
                if (rptaElem != null) {
                    requestParamFilter = new FieldFilter(rptaElem);
                }
            }
            this.requestParamFilter = requestParamFilter;
        }

        private void loadHandlerMap(Element rootElement) {
            for (Element handlerElement : UtilXml.childElementList(rootElement, "handler")) {
                String name = handlerElement.getAttribute("name");
                String type = handlerElement.getAttribute("type");
                String className = handlerElement.getAttribute("class");

                if ("request-handler-wrapper".equals(type)) { // SCIPIO
                    String triggersStr = handlerElement.getAttribute("triggers");
                    List<String> triggers;
                    if (triggersStr.isEmpty() || "all".equals(triggersStr)) {
                        triggers = null;
                    } else {
                        triggers = new ArrayList<>(Arrays.asList(triggersStr.split(",")));
                    }
                    this.eventHandlerWrapperMap.put(name, new EventHandlerWrapperDef(className, triggers));
                } else {
                    if ("view".equals(type)) {
                        this.viewHandlerMap.put(name, className);
                    } else {
                        this.eventHandlerMap.put(name, className);
                    }
                }
            }
        }

        protected void loadIncludes(Element rootElement) {
            for (Element includeElement : UtilXml.childElementList(rootElement, "include")) {
                String includeLocation = includeElement.getAttribute("location");
                if (!includeLocation.isEmpty()) {
                    // SCIPIO: support non-recursive
                    boolean recursive = !"no".equals(includeElement.getAttribute("recursive"));
                    boolean optional = "true".equals(includeElement.getAttribute("optional"));
                    String order = includeElement.getAttribute("order");
                    if (includeLocation.startsWith("component://*/")) { // SCIPIO: new case
                        if (!optional) {
                            Debug.logWarning("Include at [" + includeLocation + "] has component wildcard but not marked optional"
                                    + "; you probably want optional=\"true\"",  module);
                        }
                        String path = includeLocation.substring("component://*".length());
                        for(ComponentConfig component : ComponentConfig.getAllComponents()) {
                            if (!component.enabled()) {
                                continue;
                            }
                            loadInclude("component://" + component.getGlobalName() + path, recursive, optional, order, true);
                        }
                    } else {
                        loadInclude(includeLocation, recursive, optional, order, false);
                    }
                }
            }
        }

        private void loadInclude(String includeLocation, boolean recursive, boolean optional, String order, boolean wildcard) { // SCIPIO: refactored from loadIncludes
            try {
                URL urlLocation = FlexibleLocation.resolveLocation(includeLocation);
                Include include = new Include(urlLocation, recursive, optional, order);
                includes.add(include);
                if (include.isPostLocal()) {
                    includesPostLocal.add(include);
                } else {
                    includesPreLocal.add(include);
                }
            } catch (ComponentNotFoundURLException e) { // SCIPIO: 2017-08-03: special case needed for missing component
                if (optional) {
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("Skipping optional processing include at [" + includeLocation + "]: component not found", module);
                    }
                } else {
                    Debug.logError(e, "Error processing include at [" + includeLocation + "]: " + e.toString(), module);
                }
            } catch (MalformedURLException e) {
                Debug.logError(e, "Error processing include at [" + includeLocation + "]: " + e.toString(), module); // SCIPIO: 2017-08-03: typo fix
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
                PRE_LOCAL("pre-local"),
                POST_LOCAL("post-local");

                private final String name;

                private Order(String name) {
                    this.name = name;
                }
                
                public String getName() {
                    return name;
                }

                public static Order fromName(String name) {
                    if (name == null || name.isEmpty() || PRE_LOCAL.name.equals(name)) {
                        return PRE_LOCAL;
                    } else if (POST_LOCAL.name.equals(name)) {
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

            @Override
            public String toString() {
                return "[location=" + location + ", recursive=" + recursive + ", optional=" + optional
                        + ", order=" + order.getName() + "]";
            }
        }
        
        public static class EventHandlerWrapperDef {
            private final String className;
            private final Collection<String> triggers;
            
            public EventHandlerWrapperDef(String className, Collection<String> triggers) {
                this.className = className;
                this.triggers = triggers;
            }

            public String getClassName() {
                return className;
            }

            /**
             * Returns the triggers, or null if meant to apply to all.
             */
            public Collection<String> getTriggers() {
                return triggers;
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

        public ResolvedControllerConfig(ControllerConfig srcConfig) throws WebAppConfigurationException {
            super(srcConfig, true);
        }

        public static class Factory extends ControllerConfigFactory {
            @Override
            public ControllerConfig readControllerConfig(URL url) throws WebAppConfigurationException {
                ControllerConfig cc = new ControllerConfig(url);
                try {
                    return new ResolvedControllerConfig(cc);
                } catch(WebAppConfigurationException e) {
                    if (e.getCause() instanceof java.io.FileNotFoundException) {
                        // SPECIAL: Due to pre-resolving, we must treat FileNotFoundException here as "fatal",
                        // and we can't pass it back down due to readControllerConfig method handling it and hiding errors.
                        // TODO: REVIEW: hackish
                        throw new FatalWebAppConfigurationException(e);
                    } else {
                        throw e;
                    }
                }
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
        public Map<String, Event> getAfterLogoutEventList() throws WebAppConfigurationException {
            return afterLogoutEventList;
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
        public Map<String, EventHandlerWrapperDef> getEventHandlerWrapperMap() throws WebAppConfigurationException { // SCIPIO
            return eventHandlerWrapperMap;
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
        public Map<String, Event> getPreViewRenderEventList() throws WebAppConfigurationException {
            return preViewRenderEventList;
        }

        @Override
        public Map<String, Event> getPostViewRenderEventList() throws WebAppConfigurationException {
            return postViewRenderEventList;
        }

        @Override
        public Map<String, Event> getPreScreenRenderEventList() throws WebAppConfigurationException {
            return preScreenRenderEventList;
        }

        @Override
        public Map<String, Event> getPostScreenRenderEventList() throws WebAppConfigurationException {
            return postScreenRenderEventList;
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

        //@Override
        //public String getStatusCode() throws WebAppConfigurationException {
        //    return statusCode;
        //}

        @Override
        public Integer getStatusCodeNumber() throws WebAppConfigurationException {
            return statusCodeNumber;
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
            // No optimization needed: super only calls getViewAsJsonConfig and does a null check (cheap operation)
            return super.getViewAsJsonConfigOrDefault();
        }

        @Override
        public Boolean getAllowViewSaveDefault() throws WebAppConfigurationException {
            return allowViewSaveDefault;
        }

        @Override
        public List<NameFilter<Boolean>> getAllowViewSaveViewNameFilters() throws WebAppConfigurationException {
            return allowViewSaveViewNameFilters;
        }

        @Override
        public String getDefaultViewLastView() throws WebAppConfigurationException {
            return defaultViewLastView;
        }

        @Override
        public String getDefaultViewAccess() throws WebAppConfigurationException {
            return defaultViewAccess;
        }

        @Override
        public FieldFilter getRequestParamFilter() throws WebAppConfigurationException {
            return requestParamFilter;
        }
    }

    public static class Event {
        public static final List<String> TRIGGERS = UtilMisc.unmodifiableArrayList(
                "firstvisit", "preprocessor", "security-auth", "request", "after-login", "before-logout", "after-logout"); // SCIPIO
        public static final Set<String> TRIGGERS_SET = UtilMisc.unmodifiableHashSetCopy(TRIGGERS); // SCIPIO

        private static final int DEFAULT_TRANSACTION_TIMEOUT = 0; // SCIPIO

        // SCIPIO: 2018-11-07: All fields now final.
        public final String type;
        public final String path;
        public final String invoke;
        public final boolean globalTransaction; // = true;
        public final int transactionTimeout;
        public final Metrics metrics; // = null;
        public final Boolean transaction; // = null; // SCIPIO: A generic transaction flag
        public final String abortTransaction; // = ""; // SCIPIO: Allow aborting transaction
        // SCIPIO
        protected final List<ValueExpr> synchronizedExprList;
        protected final String scriptBody;
        protected Object compiledScript;
        protected final Map<String, Object> staticProperties;
        protected final List<ParamToAttr> paramToAttrList;
        protected final Set<String> paramToAttrNamesSet;

        public Event(Element eventElement) {
            this.type = eventElement.getAttribute("type");
            this.path = eventElement.getAttribute("path");
            this.invoke = eventElement.getAttribute("invoke");
            this.globalTransaction = !"false".equals(eventElement.getAttribute("global-transaction"));
            String tt = eventElement.getAttribute("transaction-timeout");
            int transactionTimeout = DEFAULT_TRANSACTION_TIMEOUT; // SCIPIO: Locals
            if(!tt.isEmpty()) {
                transactionTimeout = Integer.valueOf(tt);
            }
            this.transactionTimeout = transactionTimeout;
            // Get metrics.
            Element metricsElement = UtilXml.firstChildElement(eventElement, "metric");
            if (metricsElement != null) {
                this.metrics = MetricsFactory.getInstance(metricsElement);
            } else { // SCIPIO
                this.metrics = null;
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
            
            List<ValueExpr> synchronizedExprList = null;
            List<? extends Element> synchronizeElementList = UtilXml.childElementList(eventElement, "synchronized");
            if (UtilValidate.isNotEmpty(synchronizeElementList)) {
                synchronizedExprList = new ArrayList<>(synchronizeElementList.size());
                for(Element synchronizeElement : synchronizeElementList) {
                    synchronizedExprList.add(ValueExpr.getInstance(ValueExpr.ensureDelims(synchronizeElement.getAttribute("value"))));
                }
            }
            this.synchronizedExprList = synchronizedExprList;

            // SCIPIO: Script body
            this.scriptBody = UtilXml.childElementValue(eventElement, "script", null);

            Map<String, Object> properties = null;
            List<? extends Element> propertyElements = UtilXml.childElementList(eventElement, "property");
            if (UtilValidate.isNotEmpty(propertyElements)) {
                properties = new HashMap<>();
                for(Element propertyElement : propertyElements) {
                    String name = propertyElement.getAttribute("name");
                    String type = propertyElement.getAttribute("type");
                    String valueStr = propertyElement.getAttribute("value");
                    //String scope = propertyElement.getAttribute("scope"); // TODO?: future
                    Object value = null;
                    if (UtilValidate.isNotEmpty(valueStr)) { // NOTE: empty allowed; means override inherited with null
                        try {
                            //Map<String, Object> propertyCtx = new HashMap<>();
                            // TODO?: Don't support this for static properties for now, so they can be analyzed statically
                            //FlexibleStringExpander expr = FlexibleStringExpander.getInstance(valueStr);
                            //Object result = expr.expand(propertyCtx);
                            Object result = valueStr;
                            if (result != null && UtilValidate.isNotEmpty(type)) {
                                value = ObjectType.simpleTypeConvert(result, type, null, null);
                            } else {
                                value = result;
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Unable to evaluate event property '" + name
                                    + "' for event (will be null)", module);
                        }
                    }
                    properties.put(name, value);
                }
            }
            this.staticProperties = (properties != null) ? Collections.unmodifiableMap(properties) : Collections.emptyMap();

            List<ParamToAttr> paramToAttrList = null;
            List<String> paramToAttrNamesList = null;
            List<? extends Element> paramToAttrElements = UtilXml.childElementList(eventElement, "param-to-attr");
            if (UtilValidate.isNotEmpty(paramToAttrElements)) {
                paramToAttrList = new ArrayList<>();
                paramToAttrNamesList = new ArrayList<>();
                for(Element element : paramToAttrElements) {
                    String nameStr = element.getAttribute("name");
                    if (nameStr.length() == 0) {
                        Debug.logError("param-to-attr: missing name attribute; ignoring directive", module);
                        continue;
                    }
                    String toNameStr = element.getAttribute("to-name");
                    String[] names = nameStr.split(",");
                    String[] toNames = toNameStr.length() > 0 ? toNameStr.split(",") : null;
                    if (toNames != null && toNames.length != names.length) {
                        Debug.logError("param-to-attr: to-name comma-separated list has different number of names than name attribute; ignoring directive", module);
                        continue;
                    }
                    boolean override = UtilMisc.booleanValue(element.getAttribute("override"), false);
                    boolean setIfNull = UtilMisc.booleanValue(element.getAttribute("set-if-null"), true);
                    boolean setIfEmpty = UtilMisc.booleanValue(element.getAttribute("set-if-empty"), true);
                    for(int i = 0; i < names.length; i++) {
                        paramToAttrList.add(new ParamToAttr(names[i], toNames != null && i < toNames.length ? toNames[i] : names[i], override, setIfNull, setIfEmpty));
                        paramToAttrNamesList.add(names[i]);
                    }
                }
            }
            this.paramToAttrList = UtilMisc.unmodifiableOptimizedOrNull(paramToAttrList);
            this.paramToAttrNamesSet = UtilValidate.isNotEmpty(paramToAttrNamesList) ?
                    Collections.unmodifiableSet(new LinkedHashSet<>(paramToAttrNamesList)) : null;
        }

        public Event(String type, String path, String invoke, boolean globalTransaction) {
            this.type = type;
            this.path = path;
            this.invoke = invoke;
            this.globalTransaction = globalTransaction;
            // SCIPIO: Added missing inits
            this.abortTransaction = "";
            this.transaction = null;
            this.transactionTimeout = DEFAULT_TRANSACTION_TIMEOUT;
            this.metrics = null;
            this.synchronizedExprList = null;
            this.scriptBody = null;
            this.staticProperties = Collections.emptyMap();
            this.paramToAttrList = null;
            this.paramToAttrNamesSet = null;
        }

        public Event(String type, String path, String invoke, boolean globalTransaction, Metrics metrics,
                Boolean transaction, String abortTransaction) {
            this.type = type;
            this.path = path;
            this.invoke = invoke;
            this.globalTransaction = globalTransaction;
            this.transaction = transaction;
            this.abortTransaction = abortTransaction;
            // SCIPIO: Added missing inits
            this.transactionTimeout = DEFAULT_TRANSACTION_TIMEOUT;
            this.metrics = null;
            this.synchronizedExprList = null;
            this.scriptBody = null;
            this.staticProperties = Collections.emptyMap();
            this.paramToAttrList = null;
            this.paramToAttrNamesSet = null;
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

        public List<ValueExpr> getSynchronizeExprList() { // SCIPIO
            return synchronizedExprList;
        }

        // SCIPIO: TODO: REVIEW: this violates this xml class design to a small extent...
        public List<Object> getSynchronizeObjList(HttpServletRequest request, HttpServletResponse response) { // SCIPIO
            if (synchronizedExprList == null) {
                return null;
            }
            List<Object> objList = new ArrayList<>(synchronizedExprList.size());
            for(ValueExpr expr : synchronizedExprList) {
                objList.add(expr.getValue(request, response));
            }
            return objList;
        }

        public String getScriptBody() {
            return scriptBody;
        }
        
        public Object getCompiledScript() {
            return compiledScript;
        }

        public void setCompiledScript(Object compiledScript) { // NOTE: no need for thread safety on this field
            this.compiledScript = compiledScript;
        }

        public Map<String, Object> getStaticProperties() {
            return staticProperties;
        }

        /* TODO?: future
        public Map<String, Object> getRuntimeProperties(Map<String, Object> context) {
        }
        public Map<String, Object> getMergedProperties(Map<String, Object> context) {
        }
         */

        public Map<String, Object> getProperties(RequestMap requestMap, HttpServletRequest request, HttpServletResponse response, Map<String, Object> context) {
            return getStaticProperties();
        }

        public List<ParamToAttr> getParamToAttrList() {
            return paramToAttrList;
        }

        public Set<String> getParamToAttrNamesSet() {
            return paramToAttrNamesSet;
        }

        public static class ParamToAttr {
            private final String name;
            private final String toName;
            private final boolean override;
            private final boolean setIfNull;
            private final boolean setIfEmpty;

            public ParamToAttr(String name, String toName, boolean override, boolean setIfNull, boolean setIfEmpty) {
                this.name = name;
                this.toName = UtilValidate.isNotEmpty(toName) ? toName : name;
                this.override = override;
                this.setIfNull = setIfNull;
                this.setIfEmpty = setIfEmpty;
            }

            public String getName() {
                return name;
            }

            public String getToName() {
                return toName;
            }

            public boolean isOverride() {
                return override;
            }

            public boolean isSetIfNull() {
                return setIfNull;
            }

            public boolean isSetIfEmpty() {
                return setIfEmpty;
            }
        }
    }

    public static class RequestMap {
        // SCIPIO
        private static final Set<String> allowedMethods = // NOTE: excludes "all"
                UtilMisc.unmodifiableHashSet("get", "post", "head", "put", "delete", "patch", "options");

        // SCIPIO: 2018-11-07: All fields now final.
        public final String uri;
        public final Set<String> methods; // SCIPIO: Allowed HTTP methods
        public final boolean edit; // = true;
        public final boolean trackVisit; // = true;
        public final boolean trackServerHit; // = true;
        public final String description;
        public final Event event;
        public final boolean securityHttps; // = true;
        public final boolean securityAuth; // = false;
        public final boolean securityCert; // = false;
        public final boolean securityExternalView; // = true;
        public final boolean securityDirectRequest; // = true;
        public final String securityAuthCheckEvent; // SCIPIO: default "checkLogin"
        public final Map<String, RequestResponse> requestResponseMap; // = new HashMap<String, RequestResponse>();
        public final Metrics metrics; // = null
        public final OverrideMode overrideMode;

        // SCIPIO: Special definition-presence flags, needed by merge constructor to determine if should use the base or override settings
        private final boolean methodsSpecified;
        private final boolean editSpecified;
        private final boolean trackVisitSpecified;
        private final boolean trackServerHitSpecified;
        private final boolean securitySpecified;

        public RequestMap(Element requestMapElement) {
            // Get the URI info
            this.uri = requestMapElement.getAttribute("uri");
            // SCIPIO: HTTP methods
            // NOTE: We will be permissive to uppercase for security reasons, but should be written with lowercase in files.
            String methodStr = requestMapElement.getAttribute("method");
            if (methodStr.isEmpty()) {
                this.methods = Collections.emptySet();
                methodsSpecified = false;
            } else if ("all".equalsIgnoreCase(methodStr)) {
                this.methods = Collections.emptySet();
                methodsSpecified = true;
            } else {
                String[] methodList = methodStr.toLowerCase(Locale.getDefault()).split("\\s*,\\s*");
                Set<String> methods = new HashSet<>();
                for(String method : methodList) {
                    if (!allowedMethods.contains(method)) {
                        Debug.logError("request-map '" + this.uri + "' specifies invalid HTTP method: " 
                                + method + " in value '" + methodStr + "' (allowed values: " + allowedMethods + ")", module);
                    } else {
                        methods.add(method);
                    }
                }
                this.methods = methods;
                methodsSpecified = true;
            }
            String editStr = requestMapElement.getAttribute("edit");
            this.edit = !"false".equals(editStr);
            this.editSpecified = UtilValidate.isNotEmpty(editStr); // SCIPIO
            
            String trackServerHitStr = requestMapElement.getAttribute("track-serverhit");
            this.trackServerHit = !"false".equals(trackServerHitStr);
            this.trackServerHitSpecified = UtilValidate.isNotEmpty(trackServerHitStr); // SCIPIO
            String trackVisitStr = requestMapElement.getAttribute("track-visit");
            this.trackVisit = !"false".equals(trackVisitStr);
            this.trackVisitSpecified = UtilValidate.isNotEmpty(trackVisitStr); // SCIPIO
            // Check for security
            boolean securityHttps = true; // SCIPIO: Added locals here
            boolean securityAuth = false;
            boolean securityCert = false;
            boolean securityExternalView = true;
            boolean securityDirectRequest = true;
            String securityAuthCheckEvent = ""; // SCIPIO
            Element securityElement = UtilXml.firstChildElement(requestMapElement, "security");
            if (securityElement != null) {
                if (!UtilProperties.propertyValueEqualsIgnoreCase("url", "no.http", "Y")) {
                    // SCIPIO: 2018-07-09: default is now true
                    //this.securityHttps = "true".equals(securityElement.getAttribute("https"));
                    securityHttps = !"false".equals(securityElement.getAttribute("https"));
                } else {
                    String httpRequestMapList = UtilProperties.getPropertyValue("url", "http.request-map.list");
                    if (UtilValidate.isNotEmpty(httpRequestMapList)) {
                        List<String> reqList = StringUtil.split(httpRequestMapList, ",");
                        if (reqList.contains(this.uri)) {
                            // SCIPIO: 2018-07-09: default is now true
                            //this.securityHttps = "true".equals(securityElement.getAttribute("https"));
                            securityHttps = !"false".equals(securityElement.getAttribute("https"));
                        }
                    }
                }
                securityAuth = "true".equals(securityElement.getAttribute("auth"));
                securityCert = "true".equals(securityElement.getAttribute("cert"));
                securityExternalView = !"false".equals(securityElement.getAttribute("external-view"));
                securityDirectRequest = !"false".equals(securityElement.getAttribute("direct-request"));
                securityAuthCheckEvent = securityElement.getAttribute("auth-check-event");
                this.securitySpecified = true; // SCIPIO
            } else {
                this.securitySpecified = false; // SCIPIO
            }
            this.securityHttps = securityHttps; // SCIPIO: Added locals here
            this.securityAuth = securityAuth;
            this.securityCert = securityCert;
            this.securityExternalView = securityExternalView;
            this.securityDirectRequest = securityDirectRequest;
            // Check for event
            Element eventElement = UtilXml.firstChildElement(requestMapElement, "event");
            if (eventElement != null) {
                this.event = new Event(eventElement);
            } else { // SCIPIO
                this.event = null;
            }
            // Check for description
            this.description = UtilXml.childElementValue(requestMapElement, "description");
            // Get the response(s)
            Map<String, RequestResponse> requestResponseMap = new HashMap<String, RequestResponse>(); // SCIPIO
            for (Element responseElement : UtilXml.childElementList(requestMapElement, "response")) {
                RequestResponse response = new RequestResponse(responseElement);
                requestResponseMap.put(response.name, response);
            }
            this.requestResponseMap = requestResponseMap;
            // Get metrics.
            Element metricsElement = UtilXml.firstChildElement(requestMapElement, "metric");
            if (metricsElement != null) {
                this.metrics = MetricsFactory.getInstance(metricsElement);
            } else { // SCIPIO
                this.metrics = null;
            }
            this.overrideMode = OverrideMode.fromNameAlways(requestMapElement.getAttribute("override-mode"));
            this.securityAuthCheckEvent = securityAuthCheckEvent;
        }

        /**
         * SCIPIO: Merge constructor, for {@link OverrideMode#MERGE}.
         */
        RequestMap(RequestMap baseMap, RequestMap overrideMap) {
            this.uri = overrideMap.uri;

            this.methodsSpecified = overrideMap.methodsSpecified || baseMap.methodsSpecified;
            this.methods = overrideMap.methodsSpecified ? overrideMap.methods : baseMap.methods;
                    
            this.editSpecified = overrideMap.editSpecified || baseMap.editSpecified;
            this.edit = overrideMap.editSpecified ? overrideMap.edit : baseMap.edit;
            
            this.trackVisitSpecified = overrideMap.trackVisitSpecified || baseMap.trackVisitSpecified;
            this.trackVisit = overrideMap.trackVisitSpecified ? overrideMap.trackVisit : baseMap.trackVisit;

            this.trackServerHitSpecified = overrideMap.trackServerHitSpecified || baseMap.trackServerHitSpecified;
            this.trackServerHit = overrideMap.trackVisitSpecified ? overrideMap.trackServerHit : baseMap.trackServerHit;
            
            this.description = (overrideMap.description != null) ? overrideMap.description : baseMap.description;
            this.event = (overrideMap.event != null) ? overrideMap.event : baseMap.event;

            this.securitySpecified = overrideMap.securitySpecified || baseMap.securitySpecified;
            if (overrideMap.securitySpecified) {
                this.securityHttps = overrideMap.securityHttps;
                this.securityAuth = overrideMap.securityAuth;
                this.securityCert = overrideMap.securityCert;
                this.securityExternalView = overrideMap.securityExternalView;
                this.securityDirectRequest = overrideMap.securityDirectRequest;
                this.securityAuthCheckEvent = overrideMap.securityAuthCheckEvent;
            } else {
                this.securityHttps = baseMap.securityHttps;
                this.securityAuth = baseMap.securityAuth;
                this.securityCert = baseMap.securityCert;
                this.securityExternalView = baseMap.securityExternalView;
                this.securityDirectRequest = baseMap.securityDirectRequest;
                this.securityAuthCheckEvent = baseMap.securityAuthCheckEvent;
            }
            
            Map<String, RequestResponse> requestResponseMap = new HashMap<String, RequestResponse>();
            requestResponseMap.putAll(baseMap.requestResponseMap);
            requestResponseMap.putAll(overrideMap.requestResponseMap);
            this.requestResponseMap = requestResponseMap;
            this.metrics = (overrideMap.metrics != null) ? overrideMap.metrics : baseMap.metrics;

            this.overrideMode = OverrideMode.DEFAULT; // This flag should not be transitive
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

        public String getSecurityAuthCheckEvent() { return securityAuthCheckEvent; } // SCIPIO

        public Map<String, RequestResponse> getRequestResponseMap() {
            return requestResponseMap;
        }

        public Metrics getMetrics() {
            return metrics;
        }

        public OverrideMode getOverrideMode() {
            return overrideMode;
        }

        public enum OverrideMode { // SCIPIO
            REPLACE("replace"),
            MERGE("merge");
            
            public static final OverrideMode DEFAULT = REPLACE;
            
            private final String name;
            private OverrideMode(String name) { this.name = name; }

            public String getName() {
                return name;
            }

            public static OverrideMode fromNameAlways(String name) {
                if (UtilValidate.isEmpty(name)) {
                    return DEFAULT;      
                }
                for(OverrideMode mode : OverrideMode.values()) {
                    if (mode.name.equals(name)) {
                        return mode;
                    }
                }
                throw new IllegalArgumentException("Unrecognized controller request-map override-mode: " + name);
            }
        }
    }

    public static class RequestResponse {

        public static RequestResponse createEmptyNoneRequestResponse() {
            /* SCIPIO: 2018-11-07: now a dedicated constructor for this
            RequestResponse requestResponse = new RequestResponse();
            requestResponse.name = "empty-none";
            requestResponse.type = "none";
            // SCIPIO: This is an error; Element.getAttribute returns empty string if missing, so this is not equivalent to
            // rest of code
            //requestResponse.value = null;
            requestResponse.value = "";
            requestResponse.typeEnum = Type.NONE;// SCIPIO
            return requestResponse;
            */
            return new RequestResponse("empty-none", "none", "", Type.NONE);
        }

        // SCIPIO: 2018-11-07: All fields now final.
        public final String name;
        public final String type;
        public final String value;
        public final String statusCode;
        private final Integer statusCodeNumber; // SCIPIO: new (pre-parsed)
        public final boolean saveLastView; // = false;
        public final boolean saveCurrentView; // = false;
        public final boolean saveHomeView; // = false;
        public final Map<String, String> redirectParameterMap; // = new HashMap<String, String>();
        public final Map<String, String> redirectParameterValueMap; // = new HashMap<String, String>();
        public final Set<String> excludeParameterSet; // = null; // SCIPIO: new 2017-04-24
        public final String includeMode; // = "auto"; // SCIPIO: new 2017-04-24
        public final Boolean allowViewSave; // SCIPIO: new 2018-06-12: can be set explicit false to prevent recording this view
        private final Type typeEnum; // SCIPIO: new 2018-06-13
        private final ValueExpr valueExpr; // SCIPIO: 2018-11-19: precompiled value expression
        private final AttributesSpec redirectAttributes; // SCIPIO
        private final String connectionState; // SCIPIO
        private final Boolean allowCacheRedirect; // SCIPIO
        
        /**
         * @deprecated SCIPIO: 2018-11-07: This does nothing useful, all fields are final and should never have been
         * modifiable.
         */
        @Deprecated
        public RequestResponse() {
            this.name = null;
            this.type = null;
            this.value = null;
            this.statusCode = null;
            this.statusCodeNumber = null; // SCIPIO
            this.saveLastView = false;
            this.saveCurrentView = false;
            this.saveHomeView = false;
            this.redirectParameterMap = Collections.emptyMap(); // SCIPIO: use unmodifiable map
            this.redirectParameterValueMap = Collections.emptyMap();
            this.excludeParameterSet = null;
            this.includeMode = "auto";
            this.allowViewSave = null;
            this.typeEnum = null;
            this.valueExpr = null;
            this.redirectAttributes = AttributesSpec.NONE;
            this.connectionState = null;
            this.allowCacheRedirect = null;
        }

        private RequestResponse(String name, String type, String value, Type typeEnum) { // SCIPIO: 2018-11-07
            this.name = name;
            this.type = type;
            this.value = value;
            this.statusCode = null;
            this.statusCodeNumber = null; // SCIPIO
            this.saveLastView = false;
            this.saveCurrentView = false;
            this.saveHomeView = false;
            this.redirectParameterMap = Collections.emptyMap(); // SCIPIO: use unmodifiable map
            this.redirectParameterValueMap = Collections.emptyMap();
            this.excludeParameterSet = null;
            this.includeMode = "auto";
            this.allowViewSave = null;
            this.typeEnum = typeEnum;
            this.valueExpr = null;
            this.redirectAttributes = (typeEnum != null) ? typeEnum.getRedirectAttributesDefault() : AttributesSpec.NONE;
            this.connectionState = null;
            this.allowCacheRedirect = null;
        }

        public RequestResponse(Element responseElement) {
            this.name = responseElement.getAttribute("name");
            this.type = responseElement.getAttribute("type");
            this.value = responseElement.getAttribute("value");
            this.statusCode = responseElement.getAttribute("status-code");
            Integer statusCodeNumber = null; // SCIPIO: Straight Integer
            if (UtilValidate.isNotEmpty(this.statusCode)) {
                try {
                    statusCodeNumber = Integer.parseInt(this.statusCode);
                } catch(NumberFormatException e) {
                    Debug.logError("Invalid status-code (" + this.statusCode + ") for controller request response '" + this.name + "'", module);
                }
            }
            this.statusCodeNumber = statusCodeNumber;
            this.saveLastView = "true".equals(responseElement.getAttribute("save-last-view"));
            this.saveCurrentView = "true".equals(responseElement.getAttribute("save-current-view"));
            this.saveHomeView = "true".equals(responseElement.getAttribute("save-home-view"));
            Map<String, String> redirectParameterMap = new HashMap<String, String>(); // SCIPIO: Locals
            Map<String, String> redirectParameterValueMap = new HashMap<String, String>();
            for (Element redirectParameterElement : UtilXml.childElementList(responseElement, "redirect-parameter")) {
                if (UtilValidate.isNotEmpty(redirectParameterElement.getAttribute("value"))) {
                    redirectParameterValueMap.put(redirectParameterElement.getAttribute("name"), redirectParameterElement.getAttribute("value"));
                } else {
                    String from = redirectParameterElement.getAttribute("from");
                    if (UtilValidate.isEmpty(from))
                        from = redirectParameterElement.getAttribute("name");
                    redirectParameterMap.put(redirectParameterElement.getAttribute("name"), from);
                }
            }
            // SCIPIO: new 2017-04-24
            Set<String> excludeParameterSet = new HashSet<>();
            String includeMode = "auto";
            for (Element redirectParametersElement : UtilXml.childElementList(responseElement, "redirect-parameters")) {
                for (Element redirectParameterElement : UtilXml.childElementList(redirectParametersElement, "param")) {
                    if ("exclude".equals(redirectParameterElement.getAttribute("mode"))) {
                        excludeParameterSet.add(redirectParameterElement.getAttribute("name"));
                    } else {
                        if (UtilValidate.isNotEmpty(redirectParameterElement.getAttribute("value"))) {
                            redirectParameterValueMap.put(redirectParameterElement.getAttribute("name"), redirectParameterElement.getAttribute("value"));
                        } else {
                            String from = redirectParameterElement.getAttribute("from");
                            if (UtilValidate.isEmpty(from))
                                from = redirectParameterElement.getAttribute("name");
                            redirectParameterMap.put(redirectParameterElement.getAttribute("name"), from);
                        }
                    }
                }
                includeMode = redirectParametersElement.getAttribute("include-mode");
            }
            this.includeMode = includeMode;
            this.redirectParameterMap = redirectParameterMap; // SCIPIO: unmodifiable
            this.redirectParameterValueMap = redirectParameterValueMap;
            this.excludeParameterSet = excludeParameterSet;
            Boolean allowViewSave = UtilMisc.booleanValue(responseElement.getAttribute("allow-view-save")); // SCIPIO
            this.allowViewSave = allowViewSave;
            this.typeEnum = Type.fromName(this.type);
            // SCIPIO: precompiled value expression
            this.valueExpr = ValueExpr.getInstance(this.value);
            // SCIPIO: redirect-attributes
            String saveRequestStr = responseElement.getAttribute("save-request");
            Set<String> includeRequestAttributes = null;
            Set<String> excludeRequestAttributes = null;
            for (Element redirectAttributesElement : UtilXml.childElementList(responseElement, "redirect-attributes")) {
                for (Element requestAttributeElement : UtilXml.childElementList(redirectAttributesElement, "request-attribute")) {
                    if ("exclude".equals(requestAttributeElement.getAttribute("mode"))) {
                        if (excludeRequestAttributes == null) {
                            excludeRequestAttributes = new HashSet<>();
                        }
                        excludeRequestAttributes.add(requestAttributeElement.getAttribute("name"));
                    } else {
                        if (includeRequestAttributes == null) {
                            includeRequestAttributes = new HashSet<>();
                        }
                        includeRequestAttributes.add(requestAttributeElement.getAttribute("name"));
                    }
                }
            }
            AttributesSpec spec = this.typeEnum.getRedirectAttributesDefault();
            if (!saveRequestStr.isEmpty() || includeRequestAttributes != null || excludeRequestAttributes != null) {
                spec = AttributesSpec.getSpec(saveRequestStr, includeRequestAttributes, excludeRequestAttributes);
            }
            this.redirectAttributes = spec;
            String connectionState = responseElement.getAttribute("connection-state");
            this.connectionState = connectionState.isEmpty() ? null : connectionState;
            Boolean allowCacheRedirect = UtilMisc.booleanValue(responseElement.getAttribute("allow-cache-redirect")); // SCIPIO
            this.allowCacheRedirect = allowCacheRedirect;
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

        public Integer getStatusCodeNumber() { // SCIPIO
            return statusCodeNumber;
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

        public boolean hasExplicitRedirectParameterSpec() { // SCIPIO
            return !"auto".equals(getIncludeMode()) || (getRedirectParameterMap().size() > 0) || (getRedirectParameterValueMap().size() > 0) || (getExcludeParameterSet().size() > 0);
        }

        public Boolean getAllowViewSave() { // SCIPIO
            return allowViewSave;
        }

        public Type getTypeEnum() { // SCIPIO
            return typeEnum;
        }

        public ValueExpr getValueExpr() { // SCIPIO
            return valueExpr;
        }

        public AttributesSpec getRedirectAttributes() { // SCIPIO
            return redirectAttributes;
        }

        public String getConnectionState() { // SCIPIO
            return connectionState;
        }

        public Boolean getAllowCacheRedirect() { return allowCacheRedirect; }

        public enum Type { // SCIPIO: 2018-06
            NONE("none"),
            VIEW("view"),
            VIEW_LAST("view-last"),
            VIEW_LAST_NOPARAM("view-last-noparam"),
            VIEW_HOME("view-home"),
            REQUEST("request"),
            REQUEST_REDIRECT("request-redirect"),
            REQUEST_REDIRECT_NOPARAM("request-redirect-noparam"),
            REQUEST_REDIRECT_LAST("request-redirect-last"),
            URL("url"),
            CROSS_REDIRECT("cross-redirect");

            /**
             * SCIPIO: Default save-request spec for request-redirect-*.
             * <p>
             * TODO: REVIEW: In theory this should be MESSAGES or NONE; ALL was stock default and flawed...
             * DEV NOTE: don't forget to replace second occurrence below... 
             */
            public static final AttributesSpec DEFAULT_REQUEST_REDIRECT_SPEC = AttributesSpec.ALL;

            private final String name;
            private final boolean redirectType;
            private final boolean requestType;
            private final boolean viewType;
            private final AttributesSpec redirectAttributesDefault;

            private Type(String name) {
                this.name = name;
                this.redirectType = name.contains("redirect") || "url".equals(name);
                this.requestType = "request".equals(name);
                this.viewType = name.contains("view");
                // TODO: REVIEW: In future this might change to default false for all...
                this.redirectAttributesDefault = name.startsWith("request-redirect") ? AttributesSpec.ALL : AttributesSpec.NONE;
            }

            public static Type fromName(String name) {
                for(Type value : Type.values()) { 
                    if (value.getName().equals(name)) {
                        return value;
                    }
                }
                throw new IllegalArgumentException("unrecognized controller request response type: " + name);
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
            
            public AttributesSpec getRedirectAttributesDefault() {
                return redirectAttributesDefault;
            }
        }

        public static abstract class AttributesSpec { // SCIPIO
            public static final String REDIRECT_ATTR = "_SCP_REDIR_ATTRSPEC_"; // may be set by events to override redirect behavior

            public static final AttributesSpec ALL = new AttributesSpec() {
                @Override public boolean isAll() { return true; }
                @Override public boolean includeAttribute(String attributeName) { return true; }
                @Override
                public AttributesSpec mergeIncludes(Set<String> includeAttributes) {
                    return this; // not applicable
                }
            };
            public static final AttributesSpec EVENT_MESSAGES = new AttributesSpec() {
                @Override public boolean includeAttribute(String attributeName) { return EventUtil.getEventErrorMessageAttrNames().contains(attributeName); }
                @Override
                public AttributesSpec mergeIncludes(Set<String> includeAttributes) {
                    if (UtilValidate.isEmpty(includeAttributes)) {
                        return this;
                    }
                    Set<String> newIncludes = new HashSet<>(EventUtil.getEventErrorMessageAttrNames());
                    newIncludes.addAll(includeAttributes);
                    return new IncludeAttributesSpec(newIncludes);
                }
            };
            public static final AttributesSpec NONE = new AttributesSpec() {
                @Override public boolean isNone() { return true; }
                @Override public boolean includeAttribute(String attributeName) { return false; }

                @Override
                public AttributesSpec mergeIncludes(Set<String> includeAttributes) {
                    if (UtilValidate.isEmpty(includeAttributes)) {
                        return this;
                    }
                    return new IncludeAttributesSpec(includeAttributes);
                }
            };

            static AttributesSpec getSpec(String mode, Set<String> includeAttributes, Set<String> excludeAttributes) {
                if ("messages".equals(mode)) {
                    if (includeAttributes == null && excludeAttributes == null) {
                        return EVENT_MESSAGES;
                    }
                    // SPECIAL: here, include messages plus/minus extras
                    Set<String> includeAttr = new HashSet<>(EventUtil.getEventErrorMessageAttrNames());
                    if (includeAttributes != null) {
                        includeAttr.addAll(includeAttributes);
                    }
                    if (excludeAttributes != null) {
                        includeAttr.removeAll(excludeAttributes);
                    }
                    if (includeAttr.equals(EventUtil.getEventErrorMessageAttrNames())) { // avoid needless copies
                        return EVENT_MESSAGES;
                    }
                    return new IncludeAttributesSpec(includeAttr);
                } else if (includeAttributes != null || "none".equals(mode)) {
                    // whitelist mode
                    if (includeAttributes != null && !includeAttributes.isEmpty()) {
                        // none plus extras
                        return new IncludeAttributesSpec(includeAttributes);
                    }
                    return NONE;
                } else {
                    if (excludeAttributes != null && !excludeAttributes.isEmpty()) {
                        // blacklist mode
                        return new ExcludeAttributesSpec(excludeAttributes);
                    }
                    return ALL;
                }
            }

            /**
             * Includes event message by default plus optional extra attribute includes (standard setup) and excludes.
             */
            public static AttributesSpec getMessagesSpec(Set<String> includeAttributes, Set<String> excludeAttributes) {
                return getSpec("messages", includeAttributes, excludeAttributes);
            }

            /**
             * Includes event message by default plus optional extra attribute includes (standard setup).
             */
            public static AttributesSpec getMessagesIncludeSpec(Set<String> includeAttributes) {
                return getSpec("messages", includeAttributes, null);
            }

            public static AttributesSpec getIncludeSpec(Set<String> includeAttributes) {
                return getSpec(null, includeAttributes, null);
            }

            public static AttributesSpec getExcludeSpec(Set<String> excludeAttributes) {
                return getSpec(null, null, excludeAttributes);
            }

            public boolean isNone() { return false; }
            public boolean isAll() { return false; }
            public abstract boolean includeAttribute(String attributeName);

            static class IncludeAttributesSpec extends AttributesSpec {
                private final Set<String> attributes;
                IncludeAttributesSpec(Set<String> attributes) { this.attributes = attributes; }
                @Override public boolean includeAttribute(String attributeName) { return attributes.contains(attributeName); }
                @Override
                public AttributesSpec mergeIncludes(Set<String> includeAttributes) {
                    if (UtilValidate.isEmpty(includeAttributes)) {
                        return this;
                    }
                    Set<String> newIncludes = new HashSet<>(this.attributes);
                    newIncludes.addAll(includeAttributes);
                    return new IncludeAttributesSpec(newIncludes);
                }
            }

            static class ExcludeAttributesSpec extends AttributesSpec {
                private final Set<String> attributes;
                ExcludeAttributesSpec(Set<String> attributes) { this.attributes = attributes; }
                @Override public boolean includeAttribute(String attributeName) { return !attributes.contains(attributeName); }
                @Override
                public AttributesSpec mergeIncludes(Set<String> includeAttributes) {
                    return this; // not applicable
                }
            }

            public abstract AttributesSpec mergeIncludes(Set<String> includeAttributes);
        }
    }

    public static class ViewMap {
        // SCIPIO: 2018-11-07: All fields now final.
        public final String viewMap = null; // SCIPIO: TODO: REVIEW: what is this??
        public final String name;
        public final String page;
        public final String type;
        public final String info;
        public final String contentType;
        public final String encoding;
        public final String xFrameOption;
        public final String strictTransportSecurity;
        public final String description;
        public final boolean noCache; // = false;
        public final String access; // SCIPIO

        public ViewMap(Element viewMapElement) {
            this.name = viewMapElement.getAttribute("name");
            String page = viewMapElement.getAttribute("page"); // SCIPIO: made local
            this.type = viewMapElement.getAttribute("type");
            this.info = viewMapElement.getAttribute("info");
            this.contentType = viewMapElement.getAttribute("content-type");
            this.noCache = "true".equals(viewMapElement.getAttribute("no-cache"));
            this.encoding = viewMapElement.getAttribute("encoding");
            this.xFrameOption = viewMapElement.getAttribute("x-frame-options");
            this.strictTransportSecurity = viewMapElement.getAttribute("strict-transport-security");
            this.description = UtilXml.childElementValue(viewMapElement, "description");
            this.access = UtilValidate.nullIfEmpty(viewMapElement.getAttribute("access"));
            if (UtilValidate.isEmpty(page)) {
                page = this.name;
            }
            this.page = page;
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

        public String getAccess() {
            return access;
        }
    }

    /**
     * SCIPIO: Implements "view-as-json" element in site-conf.xsd.
     * Added 2017-05-15.
     */
    public static class ViewAsJsonConfig {
        // SCIPIO: 2018-11-07: All fields now final.
        private final boolean enabled;
        private final boolean updateSession;
        private final boolean regularLogin;
        private final String jsonRequestUri;

        public ViewAsJsonConfig(Element element) {
            this.enabled = UtilMisc.booleanValue(element.getAttribute("enabled"), false);
            this.updateSession = UtilMisc.booleanValue(element.getAttribute("update-session"), false);
            this.regularLogin = UtilMisc.booleanValue(element.getAttribute("regular-login"), false);
            String jsonRequestUri = element.getAttribute("json-request-uri");
            this.jsonRequestUri = jsonRequestUri.isEmpty() ? null : jsonRequestUri;
        }

        public ViewAsJsonConfig() {
            // all false/null by default
            this.enabled = false;
            this.updateSession = false;
            this.regularLogin = false;
            this.jsonRequestUri = null;
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
                return new PrefixNameFilter<V>(element.getAttribute("prefix"), useValue);
            } else if (element.getAttribute("suffix").length() > 0) {
                return new SuffixNameFilter<V>(element.getAttribute("suffix"), useValue);
            } else if (element.getAttribute("regexp").length() > 0) {
                return new RegexNameFilter<V>(element.getAttribute("regexp"), useValue);
            } else {
                return new FalseNameFilter<V>(useValue);
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

    /**
     * SCIPIO: Precompiles parsable request responses and synchronize values.
     * Similar to FlexibleStringExpander but with some specific expression support for controller.
     */
    public static abstract class ValueExpr {
        protected final String origValue;

        protected ValueExpr(String origValue) {
            this.origValue = origValue;
        }

        public static String ensureDelims(String value) {
            if (value == null) {
                return null;
            }
            if (!value.startsWith("${")) {
                value = "${" + value + "}";
            }
            return value;
        }

        public static ValueExpr getInstance(String value) {
            if (value == null) {
                return NullValueExpr.INSTANCE;
            }
            if (value.startsWith("${") && value.endsWith("}")) {
                String expr = value.substring(2, value.length() - 1).trim();
                int dotIndex = expr.indexOf('.');
                if (dotIndex >= 0) {
                    String scope = expr.substring(0, dotIndex);
                    String name = expr.substring(dotIndex+1);
                    if (scope.length() > 0 && name.length() > 0) {
                        if ("requestAttributes".equals(scope)) {
                            return new ReqAttrValueExpr(value, name);
                        } else if ("requestParameters".equals(scope)) {
                            return new ReqParamValueExpr(value, name);
                        } else if ("requestAttrParam".equals(scope)) {
                            return new ReqAttrParamValueExpr(value, name);
                        } else if ("sessionAttributes".equals(scope)) {
                            return new SessionAttrValueExpr(value, name);
                        } else if ("applicationAttributes".equals(scope)) {
                            return new ApplAttrValueExpr(value, name);
                        }
                    }
                }
                return new FlexibleValueExpr(value);
            }
            return new ConstValueExpr(value);
        }

        public abstract Object getValue(HttpServletRequest request, HttpServletResponse response);

        public String getOrigValue() {
            return origValue;
        }
        
        protected static class NullValueExpr extends ValueExpr {
            public static final NullValueExpr INSTANCE = new NullValueExpr();
            protected NullValueExpr() {
                super("");
            }
            
            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                return null;
            }
        }

        protected static class ConstValueExpr extends ValueExpr {
            protected ConstValueExpr(String value) {
                super(value);
            }
            
            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                return origValue;
            }
        }

        protected static class FlexibleValueExpr extends ValueExpr {
            protected final FlexibleStringExpander exdr;
            protected FlexibleValueExpr(String expr) {
                super(expr);
                this.exdr = FlexibleStringExpander.getInstance(expr);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                Map<String, Object> context = new HashMap<>();
                context.put("request", request);
                context.put("response", response);
                // TODO?: more fields (not too many, otherwise may be slow)...
                return exdr.expand(context);
            }
        }

        protected static abstract class NameBasedValueExpr extends ValueExpr {
            protected final String name;
            protected NameBasedValueExpr(String origValue, String name) {
                super(origValue);
                this.name = name;
            }
        }
        
        protected static class ReqAttrValueExpr extends NameBasedValueExpr {
            protected ReqAttrValueExpr(String origValue, String name) {
                super(origValue, name);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                return request.getAttribute(name);
            }
        }

        protected static class ReqParamValueExpr extends NameBasedValueExpr {
            protected ReqParamValueExpr(String origValue, String name) {
                super(origValue, name);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                return request.getParameter(name);
            }
        }

        protected static class ReqAttrParamValueExpr extends NameBasedValueExpr {
            protected ReqAttrParamValueExpr(String origValue, String name) {
                super(origValue, name);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                Object attrValue = request.getAttribute(name);
                if (attrValue == null) {
                    attrValue = request.getParameter(name);
                }
                return attrValue;
            }
        }

        protected static class SessionAttrValueExpr extends NameBasedValueExpr {
            protected SessionAttrValueExpr(String origValue, String name) {
                super(origValue, name);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                HttpSession session = request.getSession(false);
                if (session != null) {
                    return session.getAttribute(name);
                }
                return null;
            }
        }

        protected static class ApplAttrValueExpr extends NameBasedValueExpr {
            protected ApplAttrValueExpr(String origValue, String name) {
                super(origValue, name);
            }

            @Override
            public Object getValue(HttpServletRequest request, HttpServletResponse response) {
                return request.getServletContext().getAttribute(name);
            }
        }
    }
}
