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

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

import javax.wsdl.WSDLException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.config.MainResourceHandler;
import org.ofbiz.base.config.ResourceHandler;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityConfException;
import org.ofbiz.entity.config.model.DelegatorElement;
import org.ofbiz.entity.config.model.EntityConfig;
import org.ofbiz.security.Security;
import org.ofbiz.service.config.ServiceConfigUtil;
import org.ofbiz.service.config.model.GlobalServices;
import org.ofbiz.service.eca.ServiceEcaUtil;
import org.w3c.dom.Document;

/**
 * Dispatcher Context.
 *
 * <p>SCIPIO: 3.0.0: Improved {@link #makeValidContext} methods to support IN-SYS, OUT-SYS, INOUT-SYS mode target parameters.</p>
 */
@SuppressWarnings("serial")
public class DispatchContext implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, Map<String, ModelService>> modelServiceMapByModel = UtilCache.createUtilCache("service.ModelServiceMapByModel", 0, 0, false);
    private static final ThreadLocal<ModelService> currentModelService = new ThreadLocal<>(); // SCIPIO: Holds the last executing service model, managed with try-finally

    // these four fields represent the immutable state of a DispatchContext object
    private final String name;
    private final transient ClassLoader loader;
    private final transient LocalDispatcher dispatcher;
    private final String model;

    /**
     * Creates new DispatchContext as an immutable object.
     * The "dispatcher" argument can be null if the "name" argument matches the name of a valid entity model reader.
     * The thread safety of a DispatchContext object is a consequence of its immutability.
     *
     * @param name The immutable name of the DispatchContext
     * @param loader The immutable class loader
     * @param dispatcher The immutable dispatcher associated to the DispatchContext
     *
     */
    public DispatchContext(String name, ClassLoader loader, LocalDispatcher dispatcher) {
        this.name = name;
        this.loader = loader;
        this.dispatcher = dispatcher;
        String modelName = null;
        if (this.dispatcher != null) {
            Delegator delegator = dispatcher.getDelegator();
            if (delegator != null) {
                DelegatorElement delegatorInfo = null;
                try {
                    delegatorInfo = EntityConfig.getInstance().getDelegator(delegator.getDelegatorBaseName());
                } catch (GenericEntityConfException e) {
                    Debug.logWarning(e, "Exception thrown while getting delegator config: ", module);
                }
                if (delegatorInfo != null) {
                    modelName = delegatorInfo.getEntityModelReader();
                }
            }
        }
        if (modelName == null) {
            // if a modelName is not associated to the dispatcher (e.g. dispatcher is null) then use the name
            // of the DispatchContext as the model reader name
            modelName = name;
        }
        this.model = modelName;
        getGlobalServiceMap();
    }

    /**
     * Gets the classloader of this context
     * @return ClassLoader of the context
     */
    public ClassLoader getClassLoader() {
        return this.loader;
    }

    /**
     * Gets the name of the local dispatcher
     * @return String name of the LocalDispatcher object
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the LocalDispatcher used with this context
     * @return LocalDispatcher that was used to create this context
     */
    public LocalDispatcher getDispatcher() {
        return this.dispatcher;
    }

    /**
     * Gets the Delegator associated with this context/dispatcher
     * @return Delegator associated with this context
     */
    public Delegator getDelegator() {
        return dispatcher.getDelegator();
    }

    /**
     * Gets the Security object associated with this dispatcher
     * @return Security object associated with this dispatcher
     */
    public Security getSecurity() {
        return dispatcher.getSecurity();
    }

    // All the methods that follow are helper methods to retrieve service model information from cache (and manage the cache)
    // The cache object is static but most of these methods are not because the same service definition, is used with different
    // DispatchContext objects may result in different in/out attributes: this happens because the DispatchContext is associated to
    // a LocalDispatcher that is associated to a Delegator that is associated to a ModelReader; different ModelReaders could load the
    // same entity name from different files with different fields, and the service definition could automatically get the input/output
    // attributes from an entity.

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName
     * Note: This goes not guarantee the context will be 100% valid, there may be missing fields.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * <p>SCIPIO: 3.0.0: Added options overload.</p>
     *
     * @param service The {@link ModelService} object of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @param options The options
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(ModelService service, String mode, Map<String, ?> context, MakeValidOptions options) throws GenericServiceException {
        return service.makeValid(mode, context, options);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * <p>SCIPIO: 3.0.0: Added options overload.</p>
     *
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @param options The options
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(String serviceName, String mode, Map<String, ?> context, MakeValidOptions options) throws GenericServiceException {
        return makeValidContext(getModelService(serviceName), mode, context, options);
    }

    /**
     * Uses an existing map of name value pairs and extracts the keys which are used in serviceName.
     *
     * <p>NOTE: This goes not guarantee the context will be 100% valid - there may be missing fields.</p>
     *
     * <p>NOTE: The best way to call {@link #makeValidContext} methods is with an inlined
     * "IN"/"OUT"/"INOUT"/"IN-SYS"/"OUT-SYS"/"INOUT-SYS" mode parameter because they are fixed and linking {@link ModelService}
     * adds needless verbosity and imports.</p>
     *
     * @param serviceName The name of the service to obtain parameters for
     * @param mode The mode to use for building the new map (i.e. can be IN or OUT), according to {@link ModelService#PARAM_MODES}
     * @param context The initial set of values to pull from
     * @return Map contains any valid values
     * @throws GenericServiceException
     */
    public Map<String, Object> makeValidContext(String serviceName, String mode, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, mode, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use non-static <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...) or {@link ModelService#makeValid} directly</code>
     */
    @Deprecated
    public static Map<String, Object> makeValidContext(ModelService model, String mode, Map<String, ?> context) throws GenericServiceException {
        return model.makeValid(mode, context);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(String serviceName, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(serviceName, ModelService.IN_OUT_PARAM, context, null);
    }

    /**
     * Uses an existing map of name value pairs and extracts the IN and OUT keys which are used in serviceName.
     * @deprecated Use <code>makeValidContext(..., "IN"/"OUT"/"INOUT", ...)</code>
     */
    @Deprecated
    public Map<String, Object> makeValidInOutContext(ModelService model, Map<String, ?> context) throws GenericServiceException {
        return makeValidContext(model, ModelService.IN_OUT_PARAM, context, null);
    }

    /**
     * Gets the ModelService instance that corresponds to given the name
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelService(String serviceName) throws GenericServiceException {
        Map<String, ModelService> serviceMap = getGlobalServiceMap();
        ModelService retVal = serviceMap.get(serviceName);
        // SCIPIO: 3.0.0: Now done in getGlobalServiceMap() to avoid synchronized contention
        //if (retVal != null && !retVal.inheritedParameters()) {
        //    retVal.interfaceUpdate(this);
        //}
        if (retVal == null) {
            throw new GenericServiceException("Cannot locate service by name (" + serviceName + ")");
        }
        return retVal;
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, throwing IllegalArgumentException if not found.
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelServiceAlways(String serviceName) throws IllegalArgumentException {
        try {
            return getModelService(serviceName);
        } catch (GenericServiceException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * Gets the ModelService instance that corresponds to given the name, returning null and no logging if not found (SCIPIO).
     * @param serviceName Name of the service
     * @return GenericServiceModel that corresponds to the serviceName
     */
    public ModelService getModelServiceOrNull(String serviceName) {
        try {
            return getModelService(serviceName);
        } catch (GenericServiceException e) {
            return null;
        }
    }

    /**
     * Returns the model of the last invoked service, or null if no service executing (SCIPIO).
     *
     * <p>NOTE: After {@link LocalDispatcher#runSync} returns from a call, this will return null as the current-thread
     * current ModelService variable will have been cleared by then.</p>
     *
     * @return the current service model, or null if no service executing
     * @see ServiceContext#service()
     */
    public ModelService getModelService() {
        return currentModelService.get();
    }

    protected void setModelService(ModelService modelService) {
        if (modelService != null) {
            currentModelService.set(modelService);
        } else {
            currentModelService.remove();
        }
    }

    /**
     * SCIPIO: Returns true if the given service exists by name.
     * <p>
     * May be used to test addon service presence.
     * <p>
     * Never throws an exception.
     */
    public boolean isService(String serviceName) {
        try {
            return (getModelService(serviceName) != null);
        } catch (Exception e) {
            return false; // ignore all exceptions in this case
        }
    }

    public Set<String> getAllServiceNames() {
        Set<String> serviceNames = new TreeSet<>();

        Map<String, ModelService> globalServices = modelServiceMapByModel.get(this.model);
        if (globalServices != null) {
            serviceNames.addAll(globalServices.keySet());
        }
        return serviceNames;
    }

    public Document getWSDL(String serviceName, String locationURI) throws GenericServiceException, WSDLException {
        ModelService model = this.getModelService(serviceName);
        return model.toWSDL(locationURI);
    }

    private Callable<Map<String, ModelService>> createServiceReaderCallable(final ResourceHandler handler) {
        return new Callable<Map<String, ModelService>>() {
            public Map<String, ModelService> call() throws Exception {
                // SCIPIO: 3.0.0: Wrapped in unmodifiable to ensure thread safety
                return Collections.unmodifiableMap(ModelServiceReader.getModelServiceMap(handler, DispatchContext.this.getDelegator()));
            }
        };
    }

    /**
     * Creates a per-component service reader for {@link com.ilscipio.scipio.service.def.Service}.
     *
     * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
     */
    private Callable<Map<String, ModelService>> createAnnotationsServiceReaderCallable(ComponentReflectInfo reflectInfo) {
        return new Callable<Map<String, ModelService>>() {
            public Map<String, ModelService> call() throws Exception {
                // SCIPIO: 3.0.0: Wrapped in unmodifiable to ensure thread safety
                return Collections.unmodifiableMap(ModelServiceReader.getModelServiceMap(reflectInfo, DispatchContext.this.getDelegator()));
            }
        };
    }

    private Map<String, ModelService> getGlobalServiceMap() {
        Map<String, ModelService> serviceMap = modelServiceMapByModel.get(this.model);
        if (serviceMap == null) {
            serviceMap = new LinkedHashMap<>(); // SCIPIO: Switched to LinkedHashMap: new HashMap<>()

            List<Future<Map<String, ModelService>>> futures = new ArrayList<>(); // SCIPIO: switched to ArrayList: new LinkedList<>()
            List<GlobalServices> globalServicesList = null;
            try {
                globalServicesList = ServiceConfigUtil.getServiceEngine().getGlobalServices();
            } catch (GenericConfigException e) {
                // FIXME: Refactor API so exceptions can be thrown and caught.
                Debug.logError(e, module);
                throw new RuntimeException(e.getMessage());
            }
            for (GlobalServices globalServices : globalServicesList) {
                ResourceHandler handler = new MainResourceHandler(ServiceConfigUtil.getServiceEngineXmlFileName(), globalServices.getLoader(), globalServices.getLocation());
                futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createServiceReaderCallable(handler)));
            }

            // get all of the component resource model stuff, ie specified in each scipio-component.xml file
            for (ComponentConfig.ServiceResourceInfo componentResourceInfo: ComponentConfig.getAllServiceResourceInfos("model")) {
                futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createServiceReaderCallable(componentResourceInfo.createResourceHandler())));
            }

            // SCIPIO: 3.0.0: Handle @Service annotation definitions
            for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
                futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createAnnotationsServiceReaderCallable(cri)));
            }

            for (Map<String, ModelService> servicesMap: ExecutionPool.getAllFutures(futures)) {
                if (servicesMap != null) {
                    // SCIPIO: 2.1.0: Check duplicates for overriddenService
                    //serviceMap.putAll(servicesMap);
                    for(Map.Entry<String, ModelService> servicesEntry : servicesMap.entrySet()) {
                         String serviceName = servicesEntry.getKey();
                         ModelService modelService = servicesEntry.getValue();
                         ModelService prevModelService = serviceMap.get(serviceName);
                         if (prevModelService != null) {
                             modelService.updateOverriddenService(prevModelService);
                         }
                         serviceMap.put(serviceName, modelService);
                    }
                }
            }

            // SCIPIO: 3.0.0: Now do interfaceUpdate() calls in advance here to avoid contention/threading issues on getModelService() calls
            // Thread safety should be ensured by the following modelServiceMapByModel.putIfAbsentAndGet call and others.
            for (Map.Entry<String, ModelService> serviceEntry : serviceMap.entrySet()) {
                ModelService modelService = serviceEntry.getValue();
                if (!modelService.inheritedParameters()) {
                    try {
                        modelService.interfaceUpdate(serviceMap);
                    } catch (GenericServiceException e) {
                        Debug.logError(e, "Could not update interfaces for service [" + modelService.name + "]", module);
                    }
                }
            }

            Map<String, ModelService> cachedServiceMap = modelServiceMapByModel.putIfAbsentAndGet(this.model, serviceMap);
            if (cachedServiceMap == serviceMap) { // same object: this means that the object created by this thread was actually added to the cache
                ServiceEcaUtil.reloadConfig();
            }
        }
        return serviceMap;
    }
}
