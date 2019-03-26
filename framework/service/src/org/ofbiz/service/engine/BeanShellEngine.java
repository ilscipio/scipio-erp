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
package org.ofbiz.service.engine;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceDispatcher;

/**
 * BeanShell Script Service Engine
 * @deprecated SCIPIO: 2018-09-19: Beanshell is gone and this performs a best-effort
 * attempt to invoke the *.bsh file as a groovy script instead. This cannot even use
 * the GroovyLangVariants.BSH emulation so many scripts are highly likely to fail.
 */
@Deprecated
public final class BeanShellEngine extends GroovyEngine {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public BeanShellEngine(ServiceDispatcher dispatcher) {
        super(dispatcher);
    }

    @Override
    public void runSyncIgnore(String localName, ModelService modelService, Map<String, Object> context)
            throws GenericServiceException {
        Debug.logWarning("Deprecated Beanshell (bsh) service invoked (" + modelService.name 
                + "); this is a compatibility mode only (runs as Groovy); please convert to groovy service", module);
        super.runSyncIgnore(localName, modelService, context);
    }

    @Override
    public Map<String, Object> runSync(String localName, ModelService modelService, Map<String, Object> context)
            throws GenericServiceException {
        Debug.logWarning("Deprecated Beanshell (bsh) service invoked (" + modelService.name 
                + "); this is a compatibility mode only (runs as Groovy); please convert to groovy service", module);
        return super.runSync(localName, modelService, context);
    }

    /* OLD CODE
    // Invoke the BeanShell Script.
    private Map<String, Object> serviceInvoker(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        if (UtilValidate.isEmpty(modelService.location)) {
            throw new GenericServiceException("Cannot run Beanshell service with empty location");
        }

        String location = this.getLocation(modelService);
        context.put("dctx", dispatcher.getLocalContext(localName));

        try {
            Object resultObj = BshUtil.runBshAtLocation(location, context);

            if (resultObj != null && resultObj instanceof Map<?, ?>) {
                Debug.logInfo("Got result Map from script return: " + resultObj, module);
                return cast(resultObj);
            } else if (context.get("result") != null && context.get("result") instanceof Map<?, ?>) {
                Debug.logInfo("Got result Map from context: " + resultObj, module);
                return cast(context.get("result"));
            }
        } catch (GeneralException e) {
            throw new GenericServiceException(e);
        }

        return ServiceUtil.returnSuccess();
    }
    */
}
