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
package org.ofbiz.service;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.parsers.ParserConfigurationException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.metrics.def.Metric;
import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.AttributeList;
import com.ilscipio.scipio.service.def.EntityAttributes;
import com.ilscipio.scipio.service.def.EntityAttributesList;
import com.ilscipio.scipio.service.def.Implements;
import com.ilscipio.scipio.service.def.OverrideAttribute;
import com.ilscipio.scipio.service.def.OverrideAttributeList;
import com.ilscipio.scipio.service.def.Permission;
import com.ilscipio.scipio.service.def.PermissionService;
import com.ilscipio.scipio.service.def.Permissions;
import com.ilscipio.scipio.service.def.PermissionsList;
import com.ilscipio.scipio.service.def.Property;
import com.ilscipio.scipio.service.def.PropertyList;
import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.TypeValidate;
import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.config.ResourceHandler;
import org.ofbiz.base.metrics.MetricsFactory;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;
import org.ofbiz.entity.model.ModelFieldType;
import org.ofbiz.service.ModelService.LogLevel;
import org.ofbiz.service.group.GroupModel;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Generic Service - Service Definition Reader
 *
 * <p>SCIPIO: 3.0.0: Modified for annotations support.</p>
 */
@SuppressWarnings("serial")
public class ModelServiceReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final boolean loadValidateHigh = UtilProperties.getPropertyValue("debug", "code.loading.validate.level").contains("high"); // SCIPIO

    /** is either from a URL or from a ResourceLoader (through the ResourceHandler) */
    protected boolean isFromURL;
    protected URL readerURL = null;
    protected ResourceHandler handler = null;
    protected ComponentReflectInfo reflectInfo = null;
    protected Delegator delegator = null;

    public static Map<String, ModelService> getModelServiceMap(URL readerURL, Delegator delegator) {
        if (readerURL == null) {
            Debug.logError("Cannot add reader with a null reader URL", module);
            return null;
        }

        ModelServiceReader reader = new ModelServiceReader(true, readerURL, null, null, delegator);
        return reader.getModelServices();
    }

    public static Map<String, ModelService> getModelServiceMap(ResourceHandler handler, Delegator delegator) {
        ModelServiceReader reader = new ModelServiceReader(false, null, handler, null, delegator);
        return reader.getModelServices();
    }

    public static Map<String, ModelService> getModelServiceMap(ComponentReflectInfo reflectInfo, Delegator delegator) {
        ModelServiceReader reader = new ModelServiceReader(false, null, null, reflectInfo, delegator);
        return reader.getModelServices();
    }

    private ModelServiceReader(boolean isFromURL, URL readerURL, ResourceHandler handler,
                               ComponentReflectInfo reflectInfo, Delegator delegator) {
        this.isFromURL = isFromURL;
        this.readerURL = readerURL;
        this.handler = handler;
        this.reflectInfo = reflectInfo;
        this.delegator = delegator;
    }

    private Map<String, ModelService> getModelServices() {
        UtilTimer utilTimer = new UtilTimer();

        if (reflectInfo != null) { // SCIPIO: 3.0.0: Added for annotations support
            utilTimer.timerString("Before start of service loop in service annotations for component [" +
                    reflectInfo.getComponent().getGlobalName() + "]");
            Map<String, ModelService> modelServices = new LinkedHashMap<>();

            int i = 0;
            for (Class<?> serviceClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Service.class)) {
                Service serviceDef = serviceClass.getAnnotation(Service.class);
                String serviceName = UtilValidate.isNotEmpty(serviceDef.name()) ? serviceDef.name() :
                    serviceClass.getSimpleName().substring(0, 1).toLowerCase() +
                            (serviceClass.getSimpleName().length() > 1 ? serviceClass.getSimpleName().substring(1) : "");

                // check to see if service with same name has already been read
                // SCIPIO: Preserve the previous service temporarily using an alias that will be removed later
                //if (modelServices.containsKey(serviceName)) {
                ModelService overriddenService = modelServices.get(serviceName);
                if (overriddenService != null) {
                    Debug.logInfo("Service " + serviceName + " is defined more than once, " +
                            "most recent will over-write previous definition(s)", module);
                }
                ModelService service = createModelService(serviceName, serviceClass, null, serviceDef, overriddenService);

                // SCIPIO: 2018-09-10: new ability to perform additional validation at load time
                if (loadValidateHigh) {
                    service.validateModel();
                }

                modelServices.put(serviceName, service);
                i++;
            }

            for (Method serviceMethod : reflectInfo.getReflectQuery().getAnnotatedMethods(Service.class)) {
                Service serviceDef = serviceMethod.getAnnotation(Service.class);
                String serviceName = UtilValidate.isNotEmpty(serviceDef.name()) ? serviceDef.name() : serviceMethod.getName();

                // check to see if service with same name has already been read
                // SCIPIO: Preserve the previous service temporarily using an alias that will be removed later
                //if (modelServices.containsKey(serviceName)) {
                ModelService overriddenService = modelServices.get(serviceName);
                if (overriddenService != null) {
                    Debug.logInfo("Service " + serviceName + " is defined more than once, " +
                            "most recent will over-write previous definition(s)", module);
                }
                ModelService service = createModelService(serviceName, null, serviceMethod, serviceDef, overriddenService);

                // SCIPIO: 2018-09-10: new ability to perform additional validation at load time
                if (loadValidateHigh) {
                    service.validateModel();
                }

                modelServices.put(serviceName, service);
                i++;
            }

            utilTimer.timerString("Finished service annotations for component [" +
                    reflectInfo.getComponent().getGlobalName() + "] - Total Services: " + i + " FINISHED");
            Debug.logInfo("Loaded [" + i + "] Services from service annotations for component [" +
                    reflectInfo.getComponent().getGlobalName() + "]", module);
            return modelServices;
        }

        Document document;
        if (this.isFromURL) {
            document = getDocument(readerURL);

            if (document == null) {
                return null;
            }
        } else {
            try {
                document = handler.getDocument();
            } catch (GenericConfigException e) {
                Debug.logError(e, "Error getting XML document from resource", module);
                return null;
            }
        }

        Map<String, ModelService> modelServices = new LinkedHashMap<>(); // SCIPIO: switched to LinkedHashMap: new HashMap<>()

        Element docElement = document.getDocumentElement();
        if (docElement == null) {
            return null;
        }

        docElement.normalize();

        String resourceLocation = handler.getLocation();
        try {
            resourceLocation = handler.getURL().toExternalForm();
        } catch (GenericConfigException e) {
            Debug.logError(e, "Could not get resource URL", module);
        }

        int i = 0;
        Node curChild = docElement.getFirstChild();
        if (curChild != null) {
            if (this.isFromURL) {
                utilTimer.timerString("Before start of service loop in file " + readerURL);
            } else {
                utilTimer.timerString("Before start of service loop in " + handler);
            }

            do {
                if (curChild.getNodeType() == Node.ELEMENT_NODE && "service".equals(curChild.getNodeName())) {
                    i++;
                    Element curServiceElement = (Element) curChild;
                    String serviceName = UtilXml.checkEmpty(curServiceElement.getAttribute("name"));

                    // check to see if service with same name has already been read
                    // SCIPIO: Preserve the previous service temporarily using an alias that will be removed later
                    //if (modelServices.containsKey(serviceName)) {
                    ModelService overriddenService = modelServices.get(serviceName);
                    if (overriddenService != null) {
                        Debug.logInfo("Service " + serviceName + " is defined more than once, " +
                            "most recent will over-write previous definition(s)", module); // SCIPIO: switched from warning to info
                    }
                    ModelService service = createModelService(curServiceElement, resourceLocation, overriddenService); // SCIPIO: Added overriddenService

                    // SCIPIO: 2018-09-10: new ability to perform additional validation at load time
                    if (loadValidateHigh) {
                        service.validateModel();
                    }

                    modelServices.put(serviceName, service);
                }
            } while ((curChild = curChild.getNextSibling()) != null);
        } else {
            Debug.logWarning("No child nodes found.", module);
        }
        if (this.isFromURL) {
            utilTimer.timerString("Finished file " + readerURL + " - Total Services: " + i + " FINISHED");
            Debug.logInfo("Loaded [" + i + "] Services from " + readerURL, module);
        } else {
            utilTimer.timerString("Finished document in " + handler + " - Total Services: " + i + " FINISHED");
            Debug.logInfo("Loaded [" + i + "] Services from " + resourceLocation, module);
        }
        return modelServices;
    }

    /**
     * createModelService.
     *
     * <p>SCIPIO: 2.1.0: Added overriddenService here - NOTE: MAY BE NULL - rechecked in
     * <code>DispatchContext#getGlobalServiceMap()</code></p>
     */
    private ModelService createModelService(Element serviceElement, String resourceLocation, ModelService overriddenService) {
        ModelService service = new ModelService();

        service.name = UtilXml.checkEmpty(serviceElement.getAttribute("name")).intern();
        service.definitionLocation = resourceLocation;
        service.engineName = UtilXml.checkEmpty(serviceElement.getAttribute("engine")).intern();
        service.location = UtilXml.checkEmpty(serviceElement.getAttribute("location")).intern();
        service.invoke = UtilXml.checkEmpty(serviceElement.getAttribute("invoke")).intern();
        service.semaphore = UtilXml.checkEmpty(serviceElement.getAttribute("semaphore")).intern();
        service.defaultEntityName = UtilXml.checkEmpty(serviceElement.getAttribute("default-entity-name")).intern();
        service.fromLoader = isFromURL ? readerURL.toExternalForm() : handler.getLoaderName();

        // SCIPIO: accessor/factory
        service.accessorLocation = UtilValidate.nullIfEmpty(serviceElement.getAttribute("accessor-location"));
        service.accessorInvoke = UtilValidate.nullIfEmpty(serviceElement.getAttribute("accessor-invoke"));

        // SCIPIO: log level
        LogLevel logLevel = LogLevel.fromName(serviceElement.getAttribute("log"), null);
        if (logLevel == null) {
            logLevel = "true".equalsIgnoreCase(serviceElement.getAttribute("debug")) ? LogLevel.DEBUG : LogLevel.NORMAL;
        }
        service.logLevel = logLevel;
        // SCIPIO: eca (default) log level
        LogLevel ecaLogLevel = LogLevel.fromName(serviceElement.getAttribute("log-eca"), null);
        if (ecaLogLevel == null) {
            ecaLogLevel = "true".equalsIgnoreCase(serviceElement.getAttribute("debug")) ? LogLevel.DEBUG : LogLevel.NORMAL;
        }
        service.ecaLogLevel = ecaLogLevel;

        String logTraceExcludeDispatcherRegex = serviceElement.getAttribute("log-trace-exclude-dispatcher-regex");
        if (!logTraceExcludeDispatcherRegex.isEmpty()) {
            service.logTraceExcludeDispatcherRegex = Pattern.compile(logTraceExcludeDispatcherRegex);
        }

        // these default to true; if anything but true, make false
        service.auth = "true".equalsIgnoreCase(serviceElement.getAttribute("auth"));
        service.export = "true".equalsIgnoreCase(serviceElement.getAttribute("export"));
        // SCIPIO: Enhanced above
        //service.debug = "true".equalsIgnoreCase(serviceElement.getAttribute("debug"));
        service.debug = logLevel.isDebug();

        // these defaults to false; if anything but false, make it true
        service.validate = !"false".equalsIgnoreCase(serviceElement.getAttribute("validate"));
        service.useTransaction = !"false".equalsIgnoreCase(serviceElement.getAttribute("use-transaction"));
        service.requireNewTransaction = !"false".equalsIgnoreCase(serviceElement.getAttribute("require-new-transaction"));
        if (service.requireNewTransaction && !service.useTransaction) {
            // requireNewTransaction implies that a transaction is used
            service.useTransaction = true;
            Debug.logWarning("In service definition [" + service.name + "] the value use-transaction has been changed from false to true as required when require-new-transaction is set to true", module);
        }
        service.hideResultInLog = !"false".equalsIgnoreCase(serviceElement.getAttribute("hideResultInLog"));

        // set the semaphore sleep/wait times
        String semaphoreWaitStr = UtilXml.checkEmpty(serviceElement.getAttribute("semaphore-wait-seconds"));
        int semaphoreWait = 300;
        if (UtilValidate.isNotEmpty(semaphoreWaitStr)) {
            try {
                semaphoreWait = Integer.parseInt(semaphoreWaitStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting semaphore-wait to 5 minutes (default)", module);
                semaphoreWait = 300;
            }
        }
        service.semaphoreWait = semaphoreWait;

        String semaphoreSleepStr = UtilXml.checkEmpty(serviceElement.getAttribute("semaphore-sleep"));
        int semaphoreSleep = 500;
        if (UtilValidate.isNotEmpty(semaphoreSleepStr)) {
            try {
                semaphoreSleep = Integer.parseInt(semaphoreSleepStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting semaphore-sleep to 1/2 second (default)", module);
                semaphoreSleep = 500;
            }
        }
        service.semaphoreSleep = semaphoreSleep;

        // set the max retry field
        String maxRetryStr = UtilXml.checkEmpty(serviceElement.getAttribute("max-retry"));
        int maxRetry = -1;
        if (UtilValidate.isNotEmpty(maxRetryStr)) {
            try {
                maxRetry = Integer.parseInt(maxRetryStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting maxRetry to -1 (default)", module);
                maxRetry = -1;
            }
        }
        service.maxRetry = maxRetry;

        // get the timeout and convert to int
        String timeoutStr = UtilXml.checkEmpty(serviceElement.getAttribute("transaction-timeout"), serviceElement.getAttribute("transaction-timout"));
        int timeout = 0;
        if (UtilValidate.isNotEmpty(timeoutStr)) {
            try {
                timeout = Integer.parseInt(timeoutStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting timeout to 0 (default)", module);
                timeout = 0;
            }
        }
        service.transactionTimeout = timeout;

        service.description = getCDATADef(serviceElement, "description");
        this.createNamespace(serviceElement, service);

        // construct the context
        service.contextInfo = new HashMap<>();
        this.createNotification(serviceElement, service);
        this.createPermission(serviceElement, service);
        this.createPermGroups(serviceElement, service);
        this.createGroupDefs(serviceElement, service);
        this.createImplDefs(serviceElement, service);
        this.createAutoAttrDefs(serviceElement, service);
        this.createAttrDefs(serviceElement, service);
        this.createOverrideDefs(serviceElement, service);
        this.createDeprecated(serviceElement, service);
        this.createProperties(serviceElement, service); // SCIPIO: 2018-11-23
        // Get metrics.
        Element metricsElement = UtilXml.firstChildElement(serviceElement, "metric");
        if (metricsElement != null) {
            service.metrics = MetricsFactory.getInstance(metricsElement);
        }

        // SCIPIO
        String loc = service.definitionLocation;
        if (UtilValidate.isNotEmpty(loc)) {
            try {
                loc = UtilURL.getOfbizHomeRelativeLocation(new java.net.URI(loc));
            } catch (Exception e) {
                Debug.logError("Could not get service '" + service.name + "' relative definition location: "
                        + e.toString(), module);
            }
        }
        service.relativeDefinitionLocation = (loc != null) ? loc : "";

        if (service.contextParamList instanceof ArrayList) {
            ((ArrayList<ModelParam>) service.contextParamList).trimToSize();
        }
        if (service.notifications instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelNotification>) service.notifications).trimToSize();
        }
        if (service.permissionGroups instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelPermGroup>) service.permissionGroups).trimToSize();
        }

        if (overriddenService != null) {
            service.updateOverriddenService(overriddenService); // SCIPIO
        }

        // SCIPIO
        String priorityStr = serviceElement.getAttribute("priority");
        Long priority = null;
        if (priorityStr.length() > 0) {
            try {
                priority = Long.parseLong(priorityStr);
            } catch(NumberFormatException e) {
                Debug.logError("Error parsing service definition [" + service.name + "] priority attribute: " + e.toString(), module);
            }
        }
        service.priority = priority;

        service.jobPoolPersist = UtilValidate.nullIfEmpty(serviceElement.getAttribute("job-pool-persist"));

        service.startDelay = UtilMisc.toIntegerObject(UtilValidate.nullIfEmpty(serviceElement.getAttribute("start-delay")));

        return service;
    }

    /**
     * createModelService.
     *
     * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
     */
    private ModelService createModelService(String serviceName, Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService overriddenService) {
        ModelService service = new ModelService();

        service.name = serviceName;
        service.definitionLocation = (serviceMethod != null) ? serviceMethod.getDeclaringClass().getName() :
                (serviceClass.getEnclosingClass() != null ? serviceClass.getEnclosingClass().getName() : serviceClass.getName());
        service.engineName = "java";
        service.location = (serviceMethod != null) ? serviceMethod.getDeclaringClass().getName() : serviceClass.getName();
        service.invoke = (serviceMethod != null) ? serviceMethod.getName() : "exec";
        service.semaphore = serviceDef.semaphore();
        service.defaultEntityName = serviceDef.defaultEntityName();
        service.fromLoader = "annotations";

        // SCIPIO: accessor/factory
        service.accessorLocation = UtilValidate.nullIfEmpty(serviceDef.accessorLocation());
        service.accessorInvoke = UtilValidate.nullIfEmpty(serviceDef.accessorInvoke());

        // SCIPIO: log level
        LogLevel logLevel = null;
        try {
            logLevel = LogLevel.fromName(serviceDef.log(), null);
        } catch (Exception e) {
            Debug.logError(e, "@Service [" + serviceName + "] invalid log() value: " + serviceDef.log(), module);
        }
        if (logLevel == null) {
            logLevel = "true".equalsIgnoreCase(serviceDef.debug()) ? LogLevel.DEBUG : LogLevel.NORMAL;
        }
        service.logLevel = logLevel;
        // SCIPIO: eca (default) log level
        LogLevel ecaLogLevel = null;
        try {
            ecaLogLevel = LogLevel.fromName(serviceDef.logEca(), null);
        } catch (Exception e) {
            Debug.logError(e, "@Service [" + serviceName + "] invalid logEca() value: " + serviceDef.logEca(), module);
        }
        if (ecaLogLevel == null) {
            ecaLogLevel = "true".equalsIgnoreCase(serviceDef.debug()) ? LogLevel.DEBUG : LogLevel.NORMAL;
        }
        service.ecaLogLevel = ecaLogLevel;

        String logTraceExcludeDispatcherRegex = serviceDef.logTraceExcludeDispatcherRegex();
        if (!logTraceExcludeDispatcherRegex.isEmpty()) {
            service.logTraceExcludeDispatcherRegex = Pattern.compile(logTraceExcludeDispatcherRegex);
        }

        // these default to true; if anything but true, make false
        service.auth = "true".equalsIgnoreCase(serviceDef.auth());
        service.export = "true".equalsIgnoreCase(serviceDef.export());
        // SCIPIO: Enhanced above
        //service.debug = "true".equalsIgnoreCase(serviceElement.getAttribute("debug"));
        service.debug = logLevel.isDebug();

        // these defaults to false; if anything but false, make it true
        service.validate = !"false".equalsIgnoreCase(serviceDef.validate());
        service.useTransaction = !"false".equalsIgnoreCase(serviceDef.useTransaction());
        service.requireNewTransaction = !"false".equalsIgnoreCase(serviceDef.requireNewTransaction());
        if (service.requireNewTransaction && !service.useTransaction) {
            // requireNewTransaction implies that a transaction is used
            service.useTransaction = true;
            Debug.logWarning("In service definition [" + service.name + "] the value use-transaction has been changed from false to true as required when require-new-transaction is set to true", module);
        }
        service.hideResultInLog = !"false".equalsIgnoreCase(serviceDef.hideResultInLog());

        // set the semaphore sleep/wait times
        String semaphoreWaitStr = UtilXml.checkEmpty(serviceDef.semaphoreWaitSeconds());
        int semaphoreWait = 300;
        if (UtilValidate.isNotEmpty(semaphoreWaitStr)) {
            try {
                semaphoreWait = Integer.parseInt(semaphoreWaitStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting semaphore-wait to 5 minutes (default)", module);
                semaphoreWait = 300;
            }
        }
        service.semaphoreWait = semaphoreWait;

        String semaphoreSleepStr = UtilXml.checkEmpty(serviceDef.semaphoreSleep());
        int semaphoreSleep = 500;
        if (UtilValidate.isNotEmpty(semaphoreSleepStr)) {
            try {
                semaphoreSleep = Integer.parseInt(semaphoreSleepStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting semaphore-sleep to 1/2 second (default)", module);
                semaphoreSleep = 500;
            }
        }
        service.semaphoreSleep = semaphoreSleep;

        // set the max retry field
        String maxRetryStr = UtilXml.checkEmpty(serviceDef.maxRetry());
        int maxRetry = -1;
        if (UtilValidate.isNotEmpty(maxRetryStr)) {
            try {
                maxRetry = Integer.parseInt(maxRetryStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting maxRetry to -1 (default)", module);
                maxRetry = -1;
            }
        }
        service.maxRetry = maxRetry;

        // get the timeout and convert to int
        String timeoutStr = UtilXml.checkEmpty(serviceDef.transactionTimeout());
        int timeout = 0;
        if (UtilValidate.isNotEmpty(timeoutStr)) {
            try {
                timeout = Integer.parseInt(timeoutStr);
            } catch (NumberFormatException e) {
                Debug.logWarning(e, "Setting timeout to 0 (default)", module);
                timeout = 0;
            }
        }
        service.transactionTimeout = timeout;

        service.description = serviceDef.description();
        this.createNamespace(serviceClass, serviceMethod, serviceDef, service);

        // construct the context
        service.contextInfo = new HashMap<>();
        this.createNotification(serviceClass, serviceMethod, serviceDef, service);
        this.createPermission(serviceClass, serviceMethod, serviceDef, service);
        this.createPermGroups(serviceClass, serviceMethod, serviceDef, service);
        this.createGroupDefs(serviceClass, serviceMethod, serviceDef, service);
        this.createImplDefs(serviceClass, serviceMethod, serviceDef, service);
        this.createAutoAttrDefs(serviceClass, serviceMethod, serviceDef, service);
        this.createAttrDefs(serviceClass, serviceMethod, serviceDef, service);
        this.createOverrideDefs(serviceClass, serviceMethod, serviceDef, service);
        this.createDeprecated(serviceClass, serviceMethod, serviceDef, service);
        this.createProperties(serviceClass, serviceMethod, serviceDef, service); // SCIPIO: 2018-11-23

        // Get metrics.
        if (serviceDef.metrics().length > 0) {
            service.metrics = MetricsFactory.getInstance(serviceDef.metrics()[0]);
        } else {
            Metric metric = (serviceMethod != null) ? serviceMethod.getAnnotation(Metric.class) : serviceClass.getAnnotation(Metric.class);
            if (metric != null) {
                service.metrics = MetricsFactory.getInstance(metric);
            }
        }

        // SCIPIO
        String loc = service.definitionLocation;
        if (UtilValidate.isNotEmpty(loc)) {
            try {
                loc = UtilURL.getOfbizHomeRelativeLocation(new java.net.URI(loc));
            } catch (Exception e) {
                Debug.logError("Could not get service '" + service.name + "' relative definition location: "
                        + e.toString(), module);
            }
        }
        service.relativeDefinitionLocation = (loc != null) ? loc : "";

        if (service.contextParamList instanceof ArrayList) {
            ((ArrayList<ModelParam>) service.contextParamList).trimToSize();
        }
        if (service.notifications instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelNotification>) service.notifications).trimToSize();
        }
        if (service.permissionGroups instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelPermGroup>) service.permissionGroups).trimToSize();
        }

        if (overriddenService != null) {
            service.updateOverriddenService(overriddenService); // SCIPIO
        }

        // SCIPIO
        String priorityStr = serviceDef.priority();
        Long priority = null;
        if (priorityStr.length() > 0) {
            try {
                priority = Long.parseLong(priorityStr);
            } catch(NumberFormatException e) {
                Debug.logError("Error parsing service definition [" + service.name + "] priority attribute: " + e.toString(), module);
            }
        }
        service.priority = priority;

        service.jobPoolPersist = UtilValidate.nullIfEmpty(serviceDef.jobPoolPersist());

        service.startDelay = UtilMisc.toIntegerObject(UtilValidate.nullIfEmpty(serviceDef.startDelay()));

        return service;
    }

    private String getCDATADef(Element baseElement, String tagName) {
        String value = "";
        NodeList nl = baseElement.getElementsByTagName(tagName);

        // if there are more then one decriptions we will use only the first one
        if (nl.getLength() > 0) {
            Node n = nl.item(0);
            NodeList childNodes = n.getChildNodes();

            if (childNodes.getLength() > 0) {
                Node cdata = childNodes.item(0);

                value = UtilXml.checkEmpty(cdata.getNodeValue());
            }
        }
        return value;
    }

    private void createNotification(Element baseElement, ModelService model) {
        List<? extends Element> n = UtilXml.childElementList(baseElement, "notification");
        // default notification groups
        ModelNotification nSuccess = new ModelNotification();
        nSuccess.notificationEvent = "success";
        nSuccess.notificationGroupName = "default.success." + model.fromLoader;
        model.notifications.add(nSuccess);

        ModelNotification nFail = new ModelNotification();
        nFail.notificationEvent = "fail";
        nFail.notificationGroupName = "default.fail." + model.fromLoader;
        model.notifications.add(nFail);

        ModelNotification nError = new ModelNotification();
        nError.notificationEvent = "error";
        nError.notificationGroupName = "default.error." + model.fromLoader;
        model.notifications.add(nError);

        if (n != null) {
            for (Element e: n) {
                ModelNotification notify = new ModelNotification();
                notify.notificationEvent = e.getAttribute("event");
                notify.notificationGroupName = e.getAttribute("group");
                model.notifications.add(notify);
            }
        }
    }

    private void createNotification(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService model) {
        // default notification groups
        ModelNotification nSuccess = new ModelNotification();
        nSuccess.notificationEvent = "success";
        nSuccess.notificationGroupName = "default.success." + model.fromLoader;
        model.notifications.add(nSuccess);

        ModelNotification nFail = new ModelNotification();
        nFail.notificationEvent = "fail";
        nFail.notificationGroupName = "default.fail." + model.fromLoader;
        model.notifications.add(nFail);

        ModelNotification nError = new ModelNotification();
        nError.notificationEvent = "error";
        nError.notificationGroupName = "default.error." + model.fromLoader;
        model.notifications.add(nError);

        /* SCIPIO: TODO?: notification element in services.xsd is not referred, so just leave defaults for now
        if (serviceDef.notifications().length > 0) {
            for (Element e: n) {
                ModelNotification notify = new ModelNotification();
                notify.notificationEvent = e.getAttribute("event");
                notify.notificationGroupName = e.getAttribute("group");
                model.notifications.add(notify);
            }
        }
         */
    }

    private void createPermission(Element baseElement, ModelService model) {
        Element e = UtilXml.firstChildElement(baseElement, "permission-service");
        if (e != null) {
            model.permissionServiceName = e.getAttribute("service-name");
            model.permissionMainAction = e.getAttribute("main-action");
            model.permissionResourceDesc = e.getAttribute("resource-description");
            model.auth = true; // auth is always required when permissions are set
        }
    }

    private void createPermission(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService model) {
        PermissionService permissionService = (serviceMethod != null) ? serviceMethod.getAnnotation(PermissionService.class) : serviceClass.getAnnotation(PermissionService.class);
        if (serviceDef.permissionService().length > 0) {
            if (serviceDef.permissionService().length > 1) {
                Debug.logError("Annotated @Service [" + model.name + "] has more than one permissionService() " +
                                "element; using first one only; please use permissions() (@Permissions) annotations " +
                                "with appropriate joinType() instead", module);
            }
            permissionService = serviceDef.permissionService()[0];
        }
        if (permissionService != null) {
            model.permissionServiceName = permissionService.service();
            model.permissionMainAction = permissionService.mainAction();
            model.permissionResourceDesc = permissionService.resourceDescription();
            model.auth = true; // auth is always required when permissions are set
        }
    }

    private void createPermGroups(Element baseElement, ModelService model) {
        for (Element element: UtilXml.childElementList(baseElement, "required-permissions")) {
            ModelPermGroup group = new ModelPermGroup();
            group.joinType = element.getAttribute("join-type");
            createGroupPermissions(element, group, model);
            model.permissionGroups.add(group);
        }
    }

    private void createPermGroups(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService model) {
        List<Permissions> permissions = new ArrayList<>(Arrays.asList(serviceDef.permissions()));
        // Add in the defined attributes (override the above defaults if specified)
        PermissionsList permissionsList = (serviceMethod != null) ? serviceMethod.getAnnotation(PermissionsList.class) : serviceClass.getAnnotation(PermissionsList.class);
        if (permissionsList != null) {
            permissions.addAll(Arrays.asList(permissionsList.value()));
        }
        for (Permissions permissionsDef : permissions) {
            ModelPermGroup group = new ModelPermGroup();
            group.joinType = permissionsDef.joinType();
            createGroupPermissions(serviceClass, serviceMethod, serviceDef, permissionsDef, group, model);
            model.permissionGroups.add(group);
        }

        // TODO: PermissionList (doesn't exist because PermissionServiceList also does not yet work and joinType becomes too implicit)
        Permission singlePermission = (serviceMethod != null) ? serviceMethod.getAnnotation(Permission.class) : serviceClass.getAnnotation(Permission.class);
        if (singlePermission != null) {
            ModelPermGroup group = new ModelPermGroup();
            group.joinType = "";
            createSimpleGroupPermissions(serviceClass, serviceMethod, serviceDef, singlePermission, group, model);
            model.permissionGroups.add(group);
        }
    }

    private void createGroupPermissions(Element baseElement, ModelPermGroup group, ModelService service) {
        // create the simple permissions
        for (Element element: UtilXml.childElementList(baseElement, "check-permission")) {
            ModelPermission perm = new ModelPermission();
            perm.nameOrRole = element.getAttribute("permission").intern();
            perm.action = element.getAttribute("action").intern();
            if (UtilValidate.isNotEmpty(perm.action)) {
                perm.permissionType = ModelPermission.ENTITY_PERMISSION;
            } else {
                perm.permissionType = ModelPermission.PERMISSION;
            }
            perm.serviceModel = service;
            group.permissions.add(perm);
        }

        // create the role member permissions
        for (Element element: UtilXml.childElementList(baseElement, "check-role-member")) {
            ModelPermission perm = new ModelPermission();
            perm.permissionType = ModelPermission.ROLE_MEMBER;
            perm.nameOrRole = element.getAttribute("role-type").intern();
            perm.serviceModel = service;
            group.permissions.add(perm);
        }
        // Create the permissions based on permission services
        for (Element element : UtilXml.childElementList(baseElement, "permission-service")) {
            ModelPermission perm = new ModelPermission();
            if (baseElement != null) {
                perm.permissionType = ModelPermission.PERMISSION_SERVICE;
                perm.permissionServiceName = element.getAttribute("service-name");
                perm.action = element.getAttribute("main-action");
                perm.permissionResourceDesc = element.getAttribute("resource-description");
                perm.auth = true; // auth is always required when permissions are set
                perm.serviceModel = service;
                group.permissions.add(perm);
            }
        }
        if (group.permissions instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelPermission>) group.permissions).trimToSize();
        }
    }

    private void createSimpleGroupPermissions(Class<?> serviceClass, Method serviceMethod, Service serviceDef, Permission permissionDef,
                                        ModelPermGroup group, ModelService service) {
        // create the simple permissions
        ModelPermission perm = new ModelPermission();
        perm.nameOrRole = permissionDef.permission();
        perm.action = permissionDef.action();
        if (UtilValidate.isNotEmpty(perm.action)) {
            perm.permissionType = ModelPermission.ENTITY_PERMISSION;
        } else {
            perm.permissionType = ModelPermission.PERMISSION;
        }
        perm.serviceModel = service;
        group.permissions.add(perm);

        if (group.permissions instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelPermission>) group.permissions).trimToSize();
        }
    }

    private void createGroupPermissions(Class<?> serviceClass, Method serviceMethod, Service serviceDef, Permissions permissionsDef,
                                        ModelPermGroup group, ModelService service) {
        // create the simple permissions
        for (Permission permissionDef : permissionsDef.permissions()) {
            ModelPermission perm = new ModelPermission();
            perm.nameOrRole = permissionDef.permission();
            perm.action = permissionDef.action();
            if (UtilValidate.isNotEmpty(perm.action)) {
                perm.permissionType = ModelPermission.ENTITY_PERMISSION;
            } else {
                perm.permissionType = ModelPermission.PERMISSION;
            }
            perm.serviceModel = service;
            group.permissions.add(perm);
        }

        // create the role member permissions
        /* SCIPIO: 3.0.0: Was previously non-functional, removed from services.xsd
        for (Element element: UtilXml.childElementList(baseElement, "check-role-member")) {
            ModelPermission perm = new ModelPermission();
            perm.permissionType = ModelPermission.ROLE_MEMBER;
            perm.nameOrRole = element.getAttribute("role-type").intern();
            perm.serviceModel = service;
            group.permissions.add(perm);
        }
         */

        // Create the permissions based on permission services
        for (PermissionService permissionServiceDef : permissionsDef.services()) {
            ModelPermission perm = new ModelPermission();
            perm.permissionType = ModelPermission.PERMISSION_SERVICE;
            perm.permissionServiceName = permissionServiceDef.service();
            perm.action = permissionServiceDef.mainAction();
            perm.permissionResourceDesc = permissionServiceDef.resourceDescription();
            perm.auth = true; // auth is always required when permissions are set
            perm.serviceModel = service;
            group.permissions.add(perm);

        }
        if (group.permissions instanceof ArrayList) { // SCIPIO
            ((ArrayList<ModelPermission>) group.permissions).trimToSize();
        }
    }

    private void createGroupDefs(Element baseElement, ModelService service) {
        List<? extends Element> group = UtilXml.childElementList(baseElement, "group");
        if (UtilValidate.isNotEmpty(group)) {
            Element groupElement = group.get(0);
            groupElement.setAttribute("name", "_" + service.name + ".group");
            service.internalGroup = new GroupModel(groupElement);
            service.invoke = service.internalGroup.getGroupName();
            if (Debug.verboseOn()) {
                Debug.logVerbose("Created INTERNAL GROUP model [" + service.internalGroup + "]", module);
            }
        }
    }

    private void createGroupDefs(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        // SCIPIO: 3.0.0: Not for annotations
    }

    private void createImplDefs(Element baseElement, ModelService service) {
        for (Element implement: UtilXml.childElementList(baseElement, "implements")) {
            String serviceName = UtilXml.checkEmpty(implement.getAttribute("service")).intern();
            boolean optional = UtilXml.checkBoolean(implement.getAttribute("optional"), false);
            if (serviceName.length() > 0) {
                service.implServices.add(new ModelServiceIface(serviceName, optional));
            }
        }
    }

    private void createImplDefs(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        for (Implements implementsDef : serviceDef.implemented()) {
            String serviceName = implementsDef.service();
            boolean optional = UtilXml.checkBoolean(implementsDef.optional(), false);
            if (serviceName.length() > 0) {
                service.implServices.add(new ModelServiceIface(serviceName, optional));
            }
        }
    }

    private void createAutoAttrDefs(Element baseElement, ModelService service) {
        for (Element element: UtilXml.childElementList(baseElement, "auto-attributes")) {
            createAutoAttrDef(element, service);
        }
    }

    private void createAutoAttrDefs(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        List<EntityAttributes> entityAttributes = new ArrayList<>(Arrays.asList(serviceDef.entityAttributes()));
        // Add in the defined attributes (override the above defaults if specified)
        EntityAttributesList entityAttributesList = (serviceMethod != null) ? serviceMethod.getAnnotation(EntityAttributesList.class) : serviceClass.getAnnotation(EntityAttributesList.class);
        if (entityAttributesList != null) {
            entityAttributes.addAll(Arrays.asList(entityAttributesList.value()));
        }
        for (EntityAttributes entityAttributesDef : entityAttributes) {
            createAutoAttrDef(serviceClass, serviceMethod, serviceDef, entityAttributesDef, service);
        }
    }

    private void createAutoAttrDef(Element autoElement, ModelService service) {
        // get the entity name; first from the auto-attributes then from the service def
        String entityName = UtilXml.checkEmpty(autoElement.getAttribute("entity-name"));
        if (UtilValidate.isEmpty(entityName)) {
            entityName = service.defaultEntityName;
            if (UtilValidate.isEmpty(entityName)) {
                Debug.logWarning("Auto-Attribute does not specify an entity-name; not default-entity on service definition", module);
            }
        }

        String prefix = autoElement.getAttribute("prefix"); // SCIPIO: 3.0.0: Added

        // get the include type 'pk|nonpk|all'
        String includeType = UtilXml.checkEmpty(autoElement.getAttribute("include"));
        boolean includePk = "pk".equals(includeType) || "all".equals(includeType);
        boolean includeNonPk = "nonpk".equals(includeType) || "all".equals(includeType);

        if (delegator == null) {
            Debug.logWarning("Cannot use auto-attribute fields with a null delegator", module);
        }

        if (delegator != null && entityName != null) {
            Map<String, ModelParam> modelParamMap = new LinkedHashMap<>(); // SCIPIO: NOTE: keys are unprefixed (to prevent compatibility break with exclude)
            try {
                ModelEntity entity = delegator.getModelEntity(entityName);
                if (entity == null) {
                    throw new GeneralException("Could not find entity with name [" + entityName + "]");
                }
                Iterator<ModelField> fieldsIter = entity.getFieldsIterator();
                while (fieldsIter.hasNext()) {
                    ModelField field = fieldsIter.next();
                    if ((!field.getIsAutoCreatedInternal()) && ((field.getIsPk() && includePk) || (!field.getIsPk() && includeNonPk))) {
                        ModelFieldType fieldType = delegator.getEntityFieldType(entity, field.getType());
                        if (fieldType == null) {
                            throw new GeneralException("Null field type from delegator for entity [" + entityName + "]");
                        }
                        ModelParam param = new ModelParam();
                        param.entityName = entityName;
                        param.fieldName = field.getName();
                        param.name = StringUtil.prefixFieldNameCamelCase(field.getName(), prefix);
                        param.type = fieldType.getJavaType();
                        // this is a special case where we use something different in the service layer than we do in the entity/data layer
                        if ("java.sql.Blob".equals(param.type)) {
                            param.type = "java.nio.ByteBuffer";
                        }
                        param.mode = UtilXml.checkEmpty(autoElement.getAttribute("mode")).intern();
                        param.optional = "true".equalsIgnoreCase(autoElement.getAttribute("optional")); // default to true
                        param.formDisplay = !"false".equalsIgnoreCase(autoElement.getAttribute("form-display")); // default to false
                        param.allowHtml = UtilXml.checkEmpty(autoElement.getAttribute("allow-html"), "none").intern(); // default to none
                        // SCIPIO
                        param.typeConvert = "true".equalsIgnoreCase(autoElement.getAttribute("type-convert")); // SCIPIO: auto type convert flag: default to false
                        String accessStr = autoElement.getAttribute("access");
                        if (!accessStr.isEmpty()) {
                            param.access = ModelService.Access.fromName(accessStr, null);
                        }
                        String eventAccessStr = autoElement.getAttribute("event-access");
                        if (!eventAccessStr.isEmpty()) {
                            param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
                        }
                        modelParamMap.put(field.getName(), param);
                    }
                }

                // get the excludes list; and remove those from the map
                List<? extends Element> excludes = UtilXml.childElementList(autoElement, "exclude");
                if (excludes != null) {
                    for (Element exclude : excludes) {
                        modelParamMap.remove(UtilXml.checkEmpty(exclude.getAttribute("field-name")));
                    }
                }

                // now add in all the remaining params
                for (ModelParam thisParam : modelParamMap.values()) {
                    service.addParam(thisParam);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Problem loading auto-attributes [" + entityName + "] for " + service.name, module);
            } catch (GeneralException e) {
                Debug.logError(e, "Cannot load auto-attributes : " + e.getMessage() + " for " + service.name, module);
            }
        }
    }

    private void createAutoAttrDef(Class<?> serviceClass, Method serviceMethod, Service serviceDef, EntityAttributes entityAttributesDef, ModelService service) {
        // get the entity name; first from the auto-attributes then from the service def
        String entityName = UtilXml.checkEmpty(entityAttributesDef.entityName());
        if (UtilValidate.isEmpty(entityName)) {
            entityName = service.defaultEntityName;
            if (UtilValidate.isEmpty(entityName)) {
                Debug.logWarning("Auto-Attribute does not specify an entity-name; not default-entity on service definition", module);
            }
        }

        String prefix = entityAttributesDef.prefix(); // SCIPIO: 3.0.0: Added

        // get the include type 'pk|nonpk|all'
        String includeType = UtilXml.checkEmpty(entityAttributesDef.include());
        boolean includePk = "pk".equals(includeType) || "all".equals(includeType);
        boolean includeNonPk = "nonpk".equals(includeType) || "all".equals(includeType);

        if (delegator == null) {
            Debug.logWarning("Cannot use auto-attribute fields with a null delegator", module);
        }

        if (delegator != null && entityName != null) {
            Map<String, ModelParam> modelParamMap = new LinkedHashMap<>();
            try {
                ModelEntity entity = delegator.getModelEntity(entityName);
                if (entity == null) {
                    throw new GeneralException("Could not find entity with name [" + entityName + "]");
                }
                Iterator<ModelField> fieldsIter = entity.getFieldsIterator();
                while (fieldsIter.hasNext()) {
                    ModelField field = fieldsIter.next();
                    if ((!field.getIsAutoCreatedInternal()) && ((field.getIsPk() && includePk) || (!field.getIsPk() && includeNonPk))) {
                        ModelFieldType fieldType = delegator.getEntityFieldType(entity, field.getType());
                        if (fieldType == null) {
                            throw new GeneralException("Null field type from delegator for entity [" + entityName + "]");
                        }
                        ModelParam param = new ModelParam();
                        param.entityName = entityName;
                        param.fieldName = field.getName();
                        param.name = StringUtil.prefixFieldNameCamelCase(field.getName(), prefix);
                        param.type = fieldType.getJavaType();
                        // this is a special case where we use something different in the service layer than we do in the entity/data layer
                        if ("java.sql.Blob".equals(param.type)) {
                            param.type = "java.nio.ByteBuffer";
                        }
                        param.mode = UtilXml.checkEmpty(entityAttributesDef.mode()).intern();
                        param.optional = "true".equalsIgnoreCase(entityAttributesDef.optional()); // default to true
                        param.formDisplay = !"false".equalsIgnoreCase(entityAttributesDef.formDisplay()); // default to false
                        param.allowHtml = UtilXml.checkEmpty(entityAttributesDef.allowHtml(), "none").intern(); // default to none
                        // SCIPIO
                        param.typeConvert = "true".equalsIgnoreCase(entityAttributesDef.typeConvert()); // SCIPIO: auto type convert flag: default to false
                        String accessStr = entityAttributesDef.access();
                        if (!accessStr.isEmpty()) {
                            param.access = ModelService.Access.fromName(accessStr, null);
                        }
                        String eventAccessStr = entityAttributesDef.eventAccess();
                        if (!eventAccessStr.isEmpty()) {
                            param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
                        }
                        modelParamMap.put(field.getName(), param);
                    }
                }

                // get the excludes list; and remove those from the map
                String[] excludes = entityAttributesDef.excludeFields();
                if (excludes != null) {
                    for (String exclude : excludes) {
                        modelParamMap.remove(UtilXml.checkEmpty(exclude));
                    }
                }

                // now add in all the remaining params
                for (ModelParam thisParam : modelParamMap.values()) {
                    service.addParam(thisParam);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Problem loading auto-attributes [" + entityName + "] for " + service.name, module);
            } catch (GeneralException e) {
                Debug.logError(e, "Cannot load auto-attributes : " + e.getMessage() + " for " + service.name, module);
            }
        }
    }

    private void createAttrDefs(Element baseElement, ModelService service) {
        // Add in the defined attributes (override the above defaults if specified)
        for (Element attribute: UtilXml.childElementList(baseElement, "attribute")) {
            ModelParam param = new ModelParam();

            param.name = UtilXml.checkEmpty(attribute.getAttribute("name")).intern();
            param.description = getCDATADef(attribute, "description");
            param.type = UtilXml.checkEmpty(attribute.getAttribute("type")).intern();
            param.mode = UtilXml.checkEmpty(attribute.getAttribute("mode")).intern();
            param.entityName = UtilXml.checkEmpty(attribute.getAttribute("entity-name")).intern();
            param.fieldName = UtilXml.checkEmpty(attribute.getAttribute("field-name")).intern();
            param.requestAttributeName = UtilXml.checkEmpty(attribute.getAttribute("request-attribute-name")).intern();
            param.sessionAttributeName = UtilXml.checkEmpty(attribute.getAttribute("session-attribute-name")).intern();
            param.stringMapPrefix = UtilXml.checkEmpty(attribute.getAttribute("string-map-prefix")).intern();
            param.stringListSuffix = UtilXml.checkEmpty(attribute.getAttribute("string-list-suffix")).intern();
            param.formLabel = attribute.hasAttribute("form-label")?attribute.getAttribute("form-label").intern():null;
            param.optional = "true".equalsIgnoreCase(attribute.getAttribute("optional")); // default to true
            param.formDisplay = !"false".equalsIgnoreCase(attribute.getAttribute("form-display")); // default to false
            param.allowHtml = UtilXml.checkEmpty(attribute.getAttribute("allow-html"), "none").intern(); // default to none

            // default value
            String defValue = attribute.getAttribute("default-value");
            if (UtilValidate.isNotEmpty(defValue)) {
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Got a default-value [" + defValue + "] for service attribute [" + service.name + "." + param.name + "]", module);
                }
                param.setDefaultValue(defValue.intern());
            }

            // set the entity name to the default if not specified
            if (param.entityName.length() == 0) {
                param.entityName = service.defaultEntityName;
            }

            // set the field-name to the name if entity name is specified but no field-name
            if (param.fieldName.length() == 0 && param.entityName.length() > 0) {
                param.fieldName = param.name;
            }

            // SCIPIO: auto type convert flag
            param.typeConvert = "true".equalsIgnoreCase(attribute.getAttribute("type-convert")); // default to false

            String accessStr = attribute.getAttribute("access");
            if (!accessStr.isEmpty()) {
                param.access = ModelService.Access.fromName(accessStr, null);
            }
            String eventAccessStr = attribute.getAttribute("event-access");
            if (!eventAccessStr.isEmpty()) {
                param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
            }

            // set the validators
            this.addValidators(attribute, param);
            service.addParam(param);
        }

        createDefaultAttrDefs(service);
    }

    private void createAttrDefs(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        List<Attribute> attributes = new ArrayList<>(Arrays.asList(serviceDef.attributes()));
        // Add in the defined attributes (override the above defaults if specified)
        AttributeList attributeList = (serviceMethod != null) ? serviceMethod.getAnnotation(AttributeList.class) : serviceClass.getAnnotation(AttributeList.class);
        if (attributeList != null) {
            attributes.addAll(Arrays.asList(attributeList.value()));
        }
        for (Attribute attributeDef : attributes) {
            ModelParam param = new ModelParam();

            param.name = attributeDef.name();
            param.description = attributeDef.description();
            param.type = attributeDef.type();
            param.mode = attributeDef.mode();
            param.entityName = attributeDef.entityName();
            param.fieldName = attributeDef.fieldName();
            param.requestAttributeName = attributeDef.requestAttributeName();
            param.sessionAttributeName = attributeDef.sessionAttributeName();
            param.stringMapPrefix = attributeDef.stringMapPrefix();
            param.stringListSuffix = attributeDef.stringListSuffix();
            param.formLabel = UtilValidate.nullIfEmpty(attributeDef.formLabel());
            param.optional = "true".equalsIgnoreCase(attributeDef.optional()); // default to true
            param.formDisplay = !"false".equalsIgnoreCase(attributeDef.formDisplay()); // default to false
            param.allowHtml = UtilXml.checkEmpty(attributeDef.allowHtml(), "none").intern(); // default to none

            // default value
            String defValue = attributeDef.defaultValue();
            if (UtilValidate.isNotEmpty(defValue)) {
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Got a default-value [" + defValue + "] for service attribute [" + service.name + "." + param.name + "]", module);
                }
                param.setDefaultValue(defValue.intern());
            }

            // set the entity name to the default if not specified
            if (param.entityName.length() == 0) {
                param.entityName = service.defaultEntityName;
            }

            // set the field-name to the name if entity name is specified but no field-name
            if (param.fieldName.length() == 0 && param.entityName.length() > 0) {
                param.fieldName = param.name;
            }

            // SCIPIO: auto type convert flag
            param.typeConvert = "true".equalsIgnoreCase(attributeDef.typeConvert()); // default to false

            String accessStr = attributeDef.access();
            if (!accessStr.isEmpty()) {
                param.access = ModelService.Access.fromName(accessStr, null);
            }
            String eventAccessStr = attributeDef.eventAccess();
            if (!eventAccessStr.isEmpty()) {
                param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
            }

            // set the validators
            this.addValidators(serviceClass, serviceMethod, serviceDef, attributeDef, param);
            service.addParam(param);
        }

        createDefaultAttrDefs(service);
    }

    /**
     * Creates default service attribute definitions.
     *
     * <p>SCIPIO: 3.0.0: Refactored for annotations support.</p>
     */
    private void createDefaultAttrDefs(ModelService service) {
        // Add the default optional parameters
        ModelParam def;

        // responseMessage
        def = new ModelParam();
        def.name = ModelService.RESPONSE_MESSAGE;
        def.type = "String";
        def.mode = ModelService.OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // errorMessage
        def = new ModelParam();
        def.name = ModelService.ERROR_MESSAGE;
        def.type = "String";
        def.mode = ModelService.OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // errorMessageList
        def = new ModelParam();
        def.name = ModelService.ERROR_MESSAGE_LIST;
        def.type = "java.util.List";
        def.mode = ModelService.OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // successMessage
        def = new ModelParam();
        def.name = ModelService.SUCCESS_MESSAGE;
        def.type = "String";
        def.mode = ModelService.OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // successMessageList
        def = new ModelParam();
        def.name = ModelService.SUCCESS_MESSAGE_LIST;
        def.type = "java.util.List";
        def.mode = ModelService.OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // userLogin
        def = new ModelParam();
        def.name = "userLogin";
        def.type = "org.ofbiz.entity.GenericValue";
        def.mode = ModelService.IN_OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // login.username
        def = new ModelParam();
        def.name = "login.username";
        def.type = "String";
        def.mode = ModelService.IN_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // login.password
        def = new ModelParam();
        def.name = "login.password";
        def.type = "String";
        def.mode = ModelService.IN_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // Locale
        def = new ModelParam();
        def.name = "locale";
        def.type = "java.util.Locale";
        def.mode = ModelService.IN_OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);

        // timeZone
        def = new ModelParam();
        def.name = "timeZone";
        def.type = "java.util.TimeZone";
        def.mode = ModelService.IN_OUT_PARAM;
        def.optional = true;
        def.internal = true;
        service.addParam(def);
    }

    private void createOverrideDefs(Element baseElement, ModelService service) {
        for (Element overrideElement: UtilXml.childElementList(baseElement, "override")) {
            String name = UtilXml.checkEmpty(overrideElement.getAttribute("name"));
            ModelParam param = service.getParam(name);
            boolean directToParams = true;
            if (param == null) {
                if (!service.inheritedParameters && (service.implServices.size() > 0 || "group".equals(service.engineName))) {
                    // create a temp def to place in the ModelService
                    // this will get read when we read implemented services
                    directToParams = false;
                    param = new ModelParam();
                    param.name = name;
                } else {
                    Debug.logWarning("No parameter found for override parameter named: " + name + " in service " + service.name, module);
                }
            }

            if (param != null) {
                // set only modified values
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("type"))) {
                    param.type = UtilXml.checkEmpty(overrideElement.getAttribute("type")).intern();
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("mode"))) {
                    param.mode = UtilXml.checkEmpty(overrideElement.getAttribute("mode")).intern();
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("entity-name"))) {
                   param.entityName = UtilXml.checkEmpty(overrideElement.getAttribute("entity-name")).intern();
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("field-name"))) {
                    param.fieldName = UtilXml.checkEmpty(overrideElement.getAttribute("field-name")).intern();
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("form-label"))) {
                    param.formLabel = UtilXml.checkEmpty(overrideElement.getAttribute("form-label")).intern();
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("optional"))) {
                    param.optional = "true".equalsIgnoreCase(overrideElement.getAttribute("optional")); // default to true
                    param.overrideOptional = true;
                }
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("form-display"))) {
                    param.formDisplay = !"false".equalsIgnoreCase(overrideElement.getAttribute("form-display")); // default to false
                    param.overrideFormDisplay = true;
                }

                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("allow-html"))) {
                    param.allowHtml = UtilXml.checkEmpty(overrideElement.getAttribute("allow-html")).intern();
                }

                // default value
                String defValue = overrideElement.getAttribute("default-value");
                if (UtilValidate.isNotEmpty(defValue)) {
                    param.setDefaultValue(defValue);
                }

                // SCIPIO: auto type convert flag
                if (UtilValidate.isNotEmpty(overrideElement.getAttribute("type-convert"))) {
                    param.typeConvert = "true".equalsIgnoreCase(overrideElement.getAttribute("type-convert")); // default to true
                }

                String accessStr = overrideElement.getAttribute("access");
                if (!accessStr.isEmpty()) {
                    param.access = ModelService.Access.fromName(accessStr, null);
                }
                String eventAccessStr = overrideElement.getAttribute("event-access");
                if (!eventAccessStr.isEmpty()) {
                    param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
                }

                // override validators
                this.addValidators(overrideElement, param);

                if (directToParams) {
                    service.addParam(param);
                } else {
                    service.overrideParameters.add(param);
                }
            }
        }
    }

    private void createOverrideDefs(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        List<OverrideAttribute> overrideAttributes = new ArrayList<>(Arrays.asList(serviceDef.overrideAttributes()));
        // Add in the defined attributes (override the above defaults if specified)
        OverrideAttributeList overrideAttributeList = (serviceMethod != null) ? serviceMethod.getAnnotation(OverrideAttributeList.class) : serviceClass.getAnnotation(OverrideAttributeList.class);
        if (overrideAttributeList != null) {
            overrideAttributes.addAll(Arrays.asList(overrideAttributeList.value()));
        }
        for (OverrideAttribute overrideAttributeDef : overrideAttributes) {
            String name = UtilXml.checkEmpty(overrideAttributeDef.name());
            ModelParam param = service.getParam(name);
            boolean directToParams = true;
            if (param == null) {
                if (!service.inheritedParameters && (service.implServices.size() > 0 || "group".equals(service.engineName))) {
                    // create a temp def to place in the ModelService
                    // this will get read when we read implemented services
                    directToParams = false;
                    param = new ModelParam();
                    param.name = name;
                } else {
                    Debug.logWarning("No parameter found for override parameter named: " + name + " in service " + service.name, module);
                }
            }

            if (param != null) {
                // set only modified values
                if (UtilValidate.isNotEmpty(overrideAttributeDef.type())) {
                    param.type = UtilXml.checkEmpty(overrideAttributeDef.type()).intern();
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.mode())) {
                    param.mode = UtilXml.checkEmpty(overrideAttributeDef.mode()).intern();
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.entityName())) {
                    param.entityName = UtilXml.checkEmpty(overrideAttributeDef.entityName()).intern();
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.fieldName())) {
                    param.fieldName = UtilXml.checkEmpty(overrideAttributeDef.fieldName()).intern();
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.formLabel())) {
                    param.formLabel = UtilXml.checkEmpty(overrideAttributeDef.formLabel()).intern();
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.optional())) {
                    param.optional = "true".equalsIgnoreCase(overrideAttributeDef.optional()); // default to true
                    param.overrideOptional = true;
                }
                if (UtilValidate.isNotEmpty(overrideAttributeDef.formDisplay())) {
                    param.formDisplay = !"false".equalsIgnoreCase(overrideAttributeDef.formDisplay()); // default to false
                    param.overrideFormDisplay = true;
                }

                if (UtilValidate.isNotEmpty(overrideAttributeDef.allowHtml())) {
                    param.allowHtml = UtilXml.checkEmpty(overrideAttributeDef.allowHtml()).intern();
                }

                // default value
                String defValue = overrideAttributeDef.defaultValue();
                if (UtilValidate.isNotEmpty(defValue)) {
                    param.setDefaultValue(defValue);
                }

                // SCIPIO: auto type convert flag
                if (UtilValidate.isNotEmpty(overrideAttributeDef.typeConvert())) {
                    param.typeConvert = "true".equalsIgnoreCase(overrideAttributeDef.typeConvert()); // default to true
                }

                String accessStr = overrideAttributeDef.access();
                if (!accessStr.isEmpty()) {
                    param.access = ModelService.Access.fromName(accessStr, null);
                }
                String eventAccessStr = overrideAttributeDef.eventAccess();
                if (!eventAccessStr.isEmpty()) {
                    param.eventAccess = ModelService.Access.fromName(eventAccessStr, null);
                }

                // override validators
                this.addValidators(serviceClass, serviceMethod, serviceDef, overrideAttributeDef, param);

                if (directToParams) {
                    service.addParam(param);
                } else {
                    service.overrideParameters.add(param);
                }
            }
        }
    }

    private void createDeprecated(Element baseElement, ModelService service) {
        Element deprecated = UtilXml.firstChildElement(baseElement, "deprecated");
        if (deprecated != null) {
            service.deprecatedUseInstead = deprecated.getAttribute("use-instead");
            service.deprecatedSince = deprecated.getAttribute("since");
            // SCIPIO: trim it
            //service.deprecatedReason = UtilXml.elementValue(deprecated);
            String deprecatedReason = UtilXml.elementValue(deprecated);
            service.deprecatedReason = (deprecatedReason != null) ? deprecatedReason.trim() : null;
            service.informIfDeprecated(false); // SCIPIO: do not log as warning during loading
        }
    }

    private void createDeprecated(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        if (UtilValidate.isNotEmpty(serviceDef.deprecated())) {
            service.deprecatedUseInstead = serviceDef.deprecatedBy();
            service.deprecatedSince = serviceDef.deprecatedSince();
            // SCIPIO: trim it
            //service.deprecatedReason = UtilXml.elementValue(deprecated);
            service.deprecatedReason = serviceDef.deprecated();
            service.informIfDeprecated(false); // SCIPIO: do not log as warning during loading
        }
    }

    /**
     * SCIPIO: Loads custom service properties.
     * Added 2018-11-23.
     */
    private void createProperties(Element baseElement, ModelService service) {
        List<? extends Element> propertyElements = UtilXml.childElementList(baseElement, "property");
        if (UtilValidate.isNotEmpty(propertyElements)) {
            Map<String, Object> properties = new LinkedHashMap<>();
            for(Element propertyElement : propertyElements) {
                String name = propertyElement.getAttribute("name");
                String type = propertyElement.getAttribute("type");
                String valueStr = propertyElement.getAttribute("value");
                properties.put(name, evalProperty(name, type, valueStr, service));
            }
            service.properties = Collections.unmodifiableMap(properties);
            if (Debug.verboseOn() && properties.size() > 0) {
                Debug.logVerbose("Explicit properties for service '" + service.name + "': " + properties, module);
            }
        }
    }

    private void createProperties(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        List<Property> propertiesAll = new ArrayList<>(Arrays.asList(serviceDef.properties()));
        // Add in the defined attributes (override the above defaults if specified)
        PropertyList propertyList = (serviceMethod != null) ? serviceMethod.getAnnotation(PropertyList.class) : serviceClass.getAnnotation(PropertyList.class);
        if (propertyList != null) {
            propertiesAll.addAll(Arrays.asList(propertyList.value()));
        }
        if (propertiesAll.size() > 0) {
            Map<String, Object> properties = new LinkedHashMap<>();
            for (Property propertyDef : propertiesAll) {
                String name = propertyDef.name();
                String type = propertyDef.type();
                String valueStr = propertyDef.value();
                properties.put(name, evalProperty(name, type, valueStr, service));
            }
            service.properties = Collections.unmodifiableMap(properties);
            if (Debug.verboseOn() && properties.size() > 0) {
                Debug.logVerbose("Explicit properties for service '" + service.name + "': " + properties, module);
            }
        }
    }

    private Object evalProperty(String name, String type, String valueStr, ModelService service) {
        Object value = null;
        if (UtilValidate.isNotEmpty(valueStr)) { // NOTE: empty allowed; means override inherited with null
            try {
                Map<String, Object> propertyCtx = new HashMap<>();
                // TODO?: should we make anything available in this context? This runs early during system load.
                FlexibleStringExpander expr = FlexibleStringExpander.getInstance(valueStr);
                Object result = expr.expand(propertyCtx);
                if (result != null && UtilValidate.isNotEmpty(type)) {
                    value = ObjectType.simpleTypeConvert(result, type, null, null);
                } else {
                    value = result;
                }
            } catch (Exception e) {
                Debug.logError(e, "Unable to evaluate service property '" + name
                        + "' for service '" + service.name + " (will be null)", module);
            }
        }
        return value;
    }

    private void createNamespace(Element baseElement, ModelService service) {
        Element namespace = UtilXml.firstChildElement(baseElement, "namespace");
        if (namespace != null) {
            String nameSpace = UtilXml.elementValue(namespace);
            service.nameSpace = (nameSpace != null) ? nameSpace.trim() : null;
            service.nameSpacePrefix = namespace.getAttribute("prefix");
        }
    }

    private void createNamespace(Class<?> serviceClass, Method serviceMethod, Service serviceDef, ModelService service) {
        if (UtilValidate.isNotEmpty(serviceDef.namespace())) {
            service.nameSpace = serviceDef.namespace();
            service.nameSpacePrefix = serviceDef.namespacePrefix();
        }
    }

    private void addValidators(Element attribute, ModelParam param) {
        List<? extends Element> validateElements = UtilXml.childElementList(attribute, "type-validate");
        if (UtilValidate.isNotEmpty(validateElements)) {
            // always clear out old ones; never append
            param.validators = new ArrayList<>(); // SCIPIO: switched to ArrayList

            Element validate = validateElements.get(0);
            String methodName = validate.getAttribute("method").intern();
            String className = validate.getAttribute("class").intern();

            Element fail = UtilXml.firstChildElement(validate, "fail-message");
            if (fail != null) {
                String message = fail.getAttribute("message").intern();
                param.addValidator(className, methodName, message);
            } else {
                fail = UtilXml.firstChildElement(validate, "fail-property");
                if (fail != null) {
                    String resource = fail.getAttribute("resource").intern();
                    String property = fail.getAttribute("property").intern();
                    param.addValidator(className, methodName, resource, property);
                }
            }

            ((ArrayList<ModelParam.ModelParamValidator>) param.validators).trimToSize(); // SCIPIO
        }
    }

    private void addValidators(Class<?> serviceClass, Method serviceMethod, Service serviceDef, Attribute attributeDef, ModelParam param) {
        if (attributeDef.typeValidate().length > 0) {
            // always clear out old ones; never append
            param.validators = new ArrayList<>(); // SCIPIO: switched to ArrayList

            TypeValidate typeValidateDef = attributeDef.typeValidate()[0];
            String methodName = typeValidateDef.method().intern();
            String className = typeValidateDef.className().intern();

            if (UtilValidate.isNotEmpty(typeValidateDef.failMessage())) {
                String message = typeValidateDef.failMessage().intern();
                param.addValidator(className, methodName, message);
            } else {
                if (UtilValidate.isNotEmpty(typeValidateDef.failProperty())) {
                    String resource = typeValidateDef.failResource().intern();
                    String property = typeValidateDef.failProperty().intern();
                    param.addValidator(className, methodName, resource, property);
                }
            }

            ((ArrayList<ModelParam.ModelParamValidator>) param.validators).trimToSize(); // SCIPIO
        }
    }

    private void addValidators(Class<?> serviceClass, Method serviceMethod, Service serviceDef, OverrideAttribute attributeDef, ModelParam param) {
        if (attributeDef.typeValidate().length > 0) {
            // always clear out old ones; never append
            param.validators = new ArrayList<>(); // SCIPIO: switched to ArrayList

            TypeValidate typeValidateDef = attributeDef.typeValidate()[0];
            String methodName = typeValidateDef.method().intern();
            String className = typeValidateDef.className().intern();

            if (UtilValidate.isNotEmpty(typeValidateDef.failMessage())) {
                String message = typeValidateDef.failMessage().intern();
                param.addValidator(className, methodName, message);
            } else {
                if (UtilValidate.isNotEmpty(typeValidateDef.failProperty())) {
                    String resource = typeValidateDef.failResource().intern();
                    String property = typeValidateDef.failProperty().intern();
                    param.addValidator(className, methodName, resource, property);
                }
            }

            ((ArrayList<ModelParam.ModelParamValidator>) param.validators).trimToSize(); // SCIPIO
        }
    }

    private Document getDocument(URL url) {
        if (url == null) {
            return null;
        }
        Document document = null;

        try {
            document = UtilXml.readXmlDocument(url, true, true);
        } catch (SAXException sxe) {
            // Error generated during parsing)
            Exception x = sxe;

            if (sxe.getException() != null) {
                x = sxe.getException();
            }
            Debug.logError(x, module); // SCIPIO: 2018-08-13: remove printStackTrace
        } catch (ParserConfigurationException | IOException e) {
            // Parser with specified options can't be built
            Debug.logError(e, module);
        }

        return document;
    }
}
