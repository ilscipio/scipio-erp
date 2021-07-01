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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Map;

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
            Class<?> c = getHandlerClass(this.getLocation(modelService), cl, modelService, ctx);
            Method m = getHandlerMethod(c, modelService.invoke, cl, modelService, ctx);
            Object serviceHandler = Modifier.isStatic(m.getModifiers()) ? null : getHandler(c, cl, modelService, ctx);
            result = invokeHandler(serviceHandler, c, m, cl, modelService, ctx);
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

    protected Class<?> getHandlerClass(String location, ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
        return cl.loadClass(location);
    }

    protected Method getHandlerMethod(Class<?> c, String methodName, ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
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

    protected <T> T invokeHandler(Object instance, Class<?> c, Method m, ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
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

    protected <T> T getHandler(Class<?> c, ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
        String invoke = modelService.accessorInvoke;
        if (invoke == null) {
            return getDefaultHandler(c, cl, modelService, ctx);
        }

        String location = modelService.accessorLocation;
        if (location != null) {
            c = getHandlerClass(getMappedLocation(location), cl, modelService, ctx);
        }

        Method m = getHandlerMethod(c, invoke, cl, modelService, ctx);
        Object accessorInst = Modifier.isStatic(m.getModifiers()) ? null : getDefaultHandler(c, cl, modelService, ctx);
        return invokeHandler(accessorInst, c, m, cl, modelService, ctx);
    }

    protected <T> T getDefaultHandler(Class<?> c, ClassLoader cl, ModelService modelService, ServiceContext ctx) throws Throwable {
        if (ServiceHandler.Local.class.isAssignableFrom(c)) {
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
            if (constructor.getParameterCount() == 1) {
                return UtilGenerics.cast(constructor.newInstance(ctx));
            } else if (constructor.getParameterCount() == 2) {
                return UtilGenerics.cast(constructor.newInstance(ctx.dctx(), ctx.context()));
            } else {
                ServiceHandler.Local inst = UtilGenerics.cast(constructor.newInstance());
                inst.init(ctx);
                return UtilGenerics.cast(inst);
            }
        } else {
            // TODO: better instance management for Shared
            return UtilGenerics.cast(c.getConstructor().newInstance());
        }
    }
}

