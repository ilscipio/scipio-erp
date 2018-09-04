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
package org.ofbiz.webapp.event;

import java.lang.reflect.Method;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;

/**
 * JavaEventHandler - Static Method Java Event Handler
 */
public class JavaEventHandler implements EventHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /* Cache for event handler classes. */
    private ConcurrentHashMap<String, Class<?>> classes = new ConcurrentHashMap<>();

    /* Return class corresponding to path or null. */
    private static Class<?> loadClass(String path) {
        try {
            ClassLoader l = Thread.currentThread().getContextClassLoader();
            return l.loadClass(path);
        } catch (ClassNotFoundException e) {
            Debug.logError(e, "Error loading class with name: "+ path
                    + ", will not be able to run event...", module);
            return null;
        }
    }

    /**
     * @see org.ofbiz.webapp.event.EventHandler#init(javax.servlet.ServletContext)
     */
    public void init(ServletContext context) throws EventHandlerException {
    }

    /**
     * @see org.ofbiz.webapp.event.EventHandler#invoke(ConfigXMLReader.Event, ConfigXMLReader.RequestMap, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    public String invoke(Event event, RequestMap requestMap,
            HttpServletRequest request, HttpServletResponse response)
                    throws EventHandlerException {
        Class<?> k = classes.computeIfAbsent(event.path, JavaEventHandler::loadClass);
        if (Debug.verboseOn()) Debug.logVerbose("*[[Event invocation]]*", module);
        if (k == null) {
            throw new EventHandlerException("Error invoking event, the class "
                                            + event.path + " was not found");
        }
        if (event.path == null || event.invoke == null) {
            throw new EventHandlerException("Invalid event method or path; call initialize()");
        }

        if (Debug.verboseOn()) Debug.logVerbose("[Processing]: Java Event", module);
        boolean began = false;
        boolean rollback = false; // SCIPIO
        try {
            int timeout = Integer.max(event.transactionTimeout, 0);
            began = TransactionUtil.begin(timeout);
            Method m = k.getMethod(event.invoke, HttpServletRequest.class,
                                   HttpServletResponse.class);
            String ret = (String) m.invoke(null, request, response);
            if (Debug.verboseOn()) Debug.logVerbose("[Event Return]: " + ret, module);

            // SCIPIO: Trigger transaction abort if configured
            if ("error".equals(ret) && ("on-any-error".equals(event.abortTransaction) || "on-error-result".equals(event.abortTransaction))) {
                try {
                    TransactionUtil.rollback(began, "Event returned an error", null);
                } catch (GenericTransactionException e1) {
                    Debug.logError(e1, module);
                }
                rollback = true;
            }

            return ret;
        } catch (java.lang.reflect.InvocationTargetException e) {
            Throwable t = e.getTargetException();

            // SCIPIO: Trigger transaction abort if configured
            if ("on-any-error".equals(event.abortTransaction) || "on-exception".equals(event.abortTransaction)) {
                try {
                    TransactionUtil.rollback(began, "Event exception", t);
                } catch (GenericTransactionException e1) {
                    Debug.logError(e1, module);
                }
                rollback = true;
            }
            
            if (t != null) {
                Debug.logError(t, "Problems Processing Event", module);
                throw new EventHandlerException("Problems processing event: " + t.toString(), t);
            } else {
                Debug.logError(e, "Problems Processing Event", module);
                throw new EventHandlerException("Problems processing event: " + e.toString(), e);
            }
        } catch (Exception e) {
            
            // SCIPIO: Trigger transaction abort if configured
            if ("on-any-error".equals(event.abortTransaction) || "on-exception".equals(event.abortTransaction)) {
                try {
                    TransactionUtil.rollback(began, "Event exception", e);
                } catch (GenericTransactionException e1) {
                    Debug.logError(e1, module);
                }
                rollback = true;
            }
            
            Debug.logError(e, "Problems Processing Event", module);
            throw new EventHandlerException("Problems processing event: " + e.toString(), e);
        } finally {
            if (!rollback) {
                try {
                    TransactionUtil.commit(began);
                } catch (GenericTransactionException e) {
                    Debug.logError(e, module);
                }
            }
        }
    }
}
