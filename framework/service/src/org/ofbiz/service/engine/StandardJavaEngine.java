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
package org.ofbiz.service.engine;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Map;
import java.util.Objects;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceDispatcher;
import org.ofbiz.service.ServiceHandler;

/**
 * Standard Java Static Method Service Engine
 */
public class StandardJavaEngine extends GenericAsyncEngine {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public StandardJavaEngine(ServiceDispatcher dispatcher) {
        super(dispatcher);
    }

    /**
     * @see org.ofbiz.service.engine.GenericEngine#runSyncIgnore(java.lang.String, org.ofbiz.service.ModelService, java.util.Map)
     */
    @Override
    public void runSyncIgnore(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        runSync(localName, modelService, context);
    }

    /**
     * @see org.ofbiz.service.engine.GenericEngine#runSync(java.lang.String, org.ofbiz.service.ModelService, java.util.Map)
     */
    @Override
    public Map<String, Object> runSync(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        Object result = serviceInvoker(localName, modelService, context);

        if (result == null || !(result instanceof Map<?, ?>)) {
            throw new GenericServiceException("Service [" + modelService.name + "] did not return a Map object");
        }
        return UtilGenerics.checkMap(result);
    }

    // Invoke the static java method service.
    protected Object serviceInvoker(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        // static java service methods should be: public Map<String, Object> methodName(DispatchContext dctx, Map<String, Object> context)
        DispatchContext dctx = dispatcher.getLocalContext(localName);

        if (modelService == null) {
            Debug.logError("ERROR: Null Model Service (localName: " + localName + ")", module);
        }
        if (dctx == null) {
            Debug.logError("ERROR: Null DispatchContext (localName: " + localName + ", service: " + (modelService != null ? modelService.name : null) + ")", module);
        }
        if (context == null) {
            Debug.logError("ERROR: Null Service Context (localName: " + localName + ", service: " + (modelService != null ? modelService.name : null) + ")", module);
        }

        Object result = null;

        // check the package and method names
        if (modelService.location == null || modelService.invoke == null) {
            throw new GenericServiceException("Service [" + modelService.name + "] is missing location and/or invoke values which are required for execution.");
        }

        // get the classloader to use
        ClassLoader cl = null;

        if (dctx == null) {
            cl = this.getClass().getClassLoader();
        } else {
            cl = dctx.getClassLoader();
        }

        try {
            // SCIPIO: 2.1.0: Refactored getMethod vs invoke to prevent problems from NoSuchMethodException from invocations.
            ServiceContext ctx = ServiceContext.from(dctx, context); // SCIPIO: 2.1.0: Added support for ServiceContext.
            ServiceReflectInfo reflectInfo = getServiceReflectInfo(cl, modelService, ctx);
            Object serviceHandler = getHandlerInstance(cl, reflectInfo, modelService, ctx);
            result = invokeHandlerMethod(cl, reflectInfo.serviceClazz, reflectInfo.serviceMethod, serviceHandler, modelService, ctx);
        } catch (ClassNotFoundException cnfe) {
            throw new GenericServiceException("Cannot find service [" + modelService.name + "] location class", cnfe);
        } catch (NoSuchMethodException nsme) {
            throw new GenericServiceException("Service [" + modelService.name + "] specified Java method (invoke attribute) does not exist", nsme);
        } catch (SecurityException se) {
            throw new GenericServiceException("Service [" + modelService.name + "] Access denied", se);
        } catch (IllegalAccessException iae) {
            throw new GenericServiceException("Service [" + modelService.name + "] Method not accessible", iae);
        } catch (IllegalArgumentException iarge) {
            throw new GenericServiceException("Service [" + modelService.name + "] Invalid parameter match", iarge);
        } catch (InvocationTargetException ite) {
            throw new GenericServiceException("Service [" + modelService.name + "] target threw an unexpected exception", ite.getTargetException());
        } catch (NullPointerException npe) {
            throw new GenericServiceException("Service [" + modelService.name + "] ran into an unexpected null object", npe);
        } catch (ExceptionInInitializerError eie) {
            throw new GenericServiceException("Service [" + modelService.name + "] Initialization failed", eie);
        } catch (Throwable th) {
            throw new GenericServiceException("Service [" + modelService.name + "] Error or unknown exception", th);
        }

        return result;
    }

    protected static class ServiceReflectInfo implements Serializable {
        final Class<?> serviceClazz;
        final Constructor<?> serviceConstructor;
        final Method serviceMethod;

        final Class<?> accessorClazz;
        final Constructor<?> accessorConstructor;
        final Method accessorMethod;

        ServiceReflectInfo(Class<?> serviceClazz, Constructor<?> serviceConstructor, Method serviceMethod,
                           Class<?> accessorClazz, Constructor<?> accessorConstructor, Method accessorMethod) {
            this.serviceClazz = serviceClazz;
            this.serviceConstructor = serviceConstructor;
            this.serviceMethod = serviceMethod;
            this.accessorClazz = accessorClazz;
            this.accessorConstructor = accessorConstructor;
            this.accessorMethod = accessorMethod;
        }
    }

    protected ServiceReflectInfo getServiceReflectInfo(ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
        // TODO: OPTIMIZE: we're forced to go through ClassLoader once to make sure we got same Class instance, but shouldn't be needed
        Class<?> serviceClazz = getHandlerClass(cl, this.getLocation(modelService), modelService, ctx);
        ServiceReflectInfo reflectInfo = modelService.getJavaServiceReflectInfo();
        if (reflectInfo != null) {
            if (reflectInfo.serviceClazz == serviceClazz) {
                return reflectInfo;
            } else {
                Debug.logWarning("Loaded class [" + serviceClazz.getName() + "@" + Objects.hashCode(serviceClazz) +
                        "] differs from cached class [" + reflectInfo.serviceClazz.getName() + "@" +
                        Objects.hashCode(reflectInfo.serviceClazz) + "] using classloader: [" + cl.getClass().getName() + "@" +
                        Objects.hashCode(cl) + "]; updating cache", module);
            }
        }
        reflectInfo = makeServiceReflectInfo(cl, serviceClazz, modelService, ctx);
        modelService.setJavaServiceReflectInfo(reflectInfo);
        return reflectInfo;
    }

    protected ServiceReflectInfo makeServiceReflectInfo(ClassLoader cl, Class<?> serviceClazz, ModelService modelService, ServiceContext ctx) throws Throwable {
        Method serviceMethod = getHandlerMethod(cl, serviceClazz, modelService.invoke, modelService, ctx);
        Constructor<?> serviceConstructor = null;
        Class<?> accessorClazz = null;
        Method accessorMethod = null;
        Constructor<?> accessorConstructor = null;
        if (!Modifier.isStatic(serviceMethod.getModifiers())) {
            serviceConstructor = getHandlerConstructor(cl, serviceClazz, modelService, ctx);
            String accessorInvoke = modelService.accessorInvoke;
            if (accessorInvoke != null) {
                String accessorLocation = modelService.accessorLocation;
                accessorClazz = (accessorLocation != null) ? getHandlerClass(cl, getMappedLocation(accessorLocation), modelService, ctx) : serviceClazz;
                accessorMethod = getHandlerMethod(cl, accessorClazz, accessorInvoke, modelService, ctx);
                if (!Modifier.isStatic(accessorMethod.getModifiers())) {
                    accessorConstructor = getHandlerConstructor(cl, accessorClazz, modelService, ctx);
                }
            }
        }
        return new ServiceReflectInfo(serviceClazz, serviceConstructor, serviceMethod, accessorClazz, accessorConstructor, accessorMethod);
    }

    protected final Class<?> getHandlerClass(ClassLoader cl, String location, ModelService modelService, ServiceContext ctx) throws Throwable {
        return cl.loadClass(location);
    }

    protected final Method getHandlerMethod(ClassLoader cl, Class<?> c, String methodName, ModelService modelService, ServiceContext ctx) throws Throwable {
        Method m;
        try {
            m = c.getMethod(methodName, DispatchContext.class, Map.class);
        } catch(NoSuchMethodException e) {
            try {
                m = c.getMethod(methodName, ServiceContext.class);
            } catch(NoSuchMethodException e2) {
                m = c.getMethod(methodName);
            }
        }
        return m;
    }

    protected final Constructor<?> getHandlerConstructor(ClassLoader cl, Class<?> c, ModelService modelService, ServiceContext ctx) throws Throwable {
        Constructor<?> constructor;
        try {
            constructor = c.getConstructor(DispatchContext.class, Map.class);
        } catch(NoSuchMethodException e) {
            try {
                constructor = c.getConstructor(ServiceContext.class);
            } catch(NoSuchMethodException e2) {
                constructor = c.getConstructor();
            }
        }
        return constructor;
    }

    protected final <T> T invokeHandlerMethod(ClassLoader cl, Class<?> c, Method m, Object instance, ModelService modelService, ServiceContext ctx) throws Throwable {
        Object result;
        if (m.getParameterCount() == 0) {
            result = m.invoke(instance);
        } else if (m.getParameterCount() == 1) {
            result = m.invoke(instance, ctx);
        } else {
            result = m.invoke(instance, ctx.dctx(), ctx.context());
        }
        return UtilGenerics.cast(result);
    }

    protected final <T> T invokeHandlerConstructor(ClassLoader cl, Class<?> c, Constructor<?> constructor, ModelService modelService, ServiceContext ctx) throws Throwable {
        if (constructor.getParameterCount() == 1) {
            return UtilGenerics.cast(constructor.newInstance(ctx));
        } else if (constructor.getParameterCount() == 2) {
            return UtilGenerics.cast(constructor.newInstance(ctx.dctx(), ctx.context()));
        } else if (ServiceHandler.Local.class.isAssignableFrom(c)) {
            ServiceHandler.Local inst = UtilGenerics.cast(constructor.newInstance());
            inst.init(ctx);
            return UtilGenerics.cast(inst);
        } else {
            return UtilGenerics.cast(constructor.newInstance());
        }
    }

    protected final <T> T getHandlerInstance(ClassLoader cl, ServiceReflectInfo reflectInfo, ModelService modelService, ServiceContext ctx) throws Throwable {
        Object handlerInstance = null;
        if (reflectInfo.accessorMethod != null) {
            Object accessorInstance = null;
            if (!Modifier.isStatic(reflectInfo.accessorMethod.getModifiers())) {
                accessorInstance = getStandardHandlerInstance(cl, reflectInfo.accessorClazz, reflectInfo.accessorConstructor, modelService, ctx);
            }
            handlerInstance = invokeHandlerMethod(cl, reflectInfo.accessorClazz, reflectInfo.accessorMethod, accessorInstance, modelService, ctx);
        } else {
            if (!Modifier.isStatic(reflectInfo.serviceMethod.getModifiers())) {
                handlerInstance = getStandardHandlerInstance(cl, reflectInfo.serviceClazz, reflectInfo.serviceConstructor, modelService, ctx);
            }
        }
        return UtilGenerics.cast(handlerInstance);
    }

    protected final <T> T getStandardHandlerInstance(ClassLoader cl, Class<?> c, Constructor<?> constructor, ModelService modelService, ServiceContext ctx) throws Throwable {
        if (ServiceHandler.Local.class.isAssignableFrom(c)) {
            return invokeHandlerConstructor(cl, c, constructor, modelService, ctx);
        } else {
            // TODO: better instance management for Shared
            return UtilGenerics.cast(c.getConstructor().newInstance());
        }
    }
}

