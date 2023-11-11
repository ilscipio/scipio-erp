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
package org.ofbiz.entityext.eca;

import java.lang.reflect.Method;
import java.util.Map;

import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.ServiceDefUtil;
import com.ilscipio.scipio.service.def.eeca.Eeca;
import com.ilscipio.scipio.service.def.eeca.EecaAction;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceOptions;
import org.ofbiz.service.ServiceUtil;
import org.w3c.dom.Element;

/**
 * EntityEcaAction
 */
@SuppressWarnings("serial")
public final class EntityEcaAction implements java.io.Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final String serviceName;
    private final String serviceMode;
    private final String runAsUser;
    private final String valueAttr;
    private final boolean resultToValue;
    private final boolean abortOnError;
    private final boolean rollbackOnError;
    private final boolean persist;
    protected final Long priority; // SCIPIO
    private final String jobPool; // SCIPIO
    private transient Boolean quiet = null; // SCIPIO: if true, don't log when this gets triggered
    private final boolean reloadValue; // SCIPIO

    public EntityEcaAction(Element action) {
        this.serviceName = action.getAttribute("service");
        this.serviceMode = action.getAttribute("mode");
        // default is true, so anything but false is true
        this.resultToValue = !"false".equals(action.getAttribute("result-to-value"));
        // default is false, so anything but true is false
        this.abortOnError = "true".equals(action.getAttribute("abort-on-error"));
        this.rollbackOnError = "true".equals(action.getAttribute("rollback-on-error"));
        this.persist = "true".equals(action.getAttribute("persist"));
        this.runAsUser = action.getAttribute("run-as-user");
        this.valueAttr = action.getAttribute("value-attr");
        String priorityStr = action.getAttribute("priority");
        Long priority = null;
        if (!priorityStr.isEmpty()) {
            try {
                priority = Long.parseLong(priorityStr);
            } catch (NumberFormatException e) {
                Debug.logError("Invalid job priority on entity ECA service [" + this.serviceName + "]; using default", module);
            }
        }
        this.priority = priority;
        String jobPool = action.getAttribute("job-pool");
        this.jobPool = UtilValidate.isNotEmpty(jobPool) ? jobPool : null;
        this.reloadValue = UtilMisc.booleanValue(action.getAttribute("reload-value"), false);
    }

    /**
     * Annotations constructor.
     *
     * <p>NOTE: serviceClass null when serviceMethod set and vice-versa.</p>
     *
     * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
     */
    public EntityEcaAction(EecaAction actionDef, Eeca secaDef, Service serviceDef, Class<?> serviceClass, Method serviceMethod) {
        this.serviceName = (!actionDef.service().isEmpty()) ? actionDef.service() : ServiceDefUtil.getServiceName(serviceDef, serviceClass, serviceMethod);
        if (UtilValidate.isEmpty(serviceName)) {
            if (serviceClass != null) {
                throw new IllegalArgumentException("Missing Entity ECA action service name on " + EecaAction.class.getSimpleName() +
                        " annotation for service class " + serviceClass.getName());
            } else {
                throw new IllegalArgumentException("Missing Entity ECA action service name on " + EecaAction.class.getSimpleName() +
                        " annotation for service method " + serviceMethod.getDeclaringClass().getName() + "." + serviceMethod.getName());
            }
        }
        this.serviceMode = (!actionDef.mode().isEmpty()) ? actionDef.mode() : "sync";
        // default is true, so anything but false is true
        this.resultToValue = !"false".equals(actionDef.resultToValue());
        // default is false, so anything but true is false
        this.abortOnError = "true".equals(actionDef.abortOnError());
        this.rollbackOnError = "true".equals(actionDef.rollbackOnError());
        this.persist = "true".equals(actionDef.persist());
        this.runAsUser = (!actionDef.runAsUser().isEmpty()) ? actionDef.runAsUser() : "system"; // default "system" - see entity-eca.xsd
        this.valueAttr = actionDef.valueAttr();
        String priorityStr = actionDef.priority();
        Long priority = null;
        if (!priorityStr.isEmpty()) {
            try {
                priority = Long.parseLong(priorityStr);
            } catch (NumberFormatException e) {
                Debug.logError("Invalid job priority on entity ECA service [" + this.serviceName + "]; using default", module);
            }
        }
        this.priority = priority;
        String jobPool = actionDef.jobPool();
        this.jobPool = UtilValidate.isNotEmpty(jobPool) ? jobPool : null;
        this.reloadValue = UtilMisc.booleanValue(actionDef.reloadValue(), false);
    }

    public String getServiceName() {
        return this.serviceName;
    }

    /**
     * Returns true if the service this refers to has log-eca="quiet" by default.
     * <p>SCIPIO: 2.0.0: Added.</p>
     */
    public boolean isQuiet(DispatchContext dctx) {
        Boolean quiet = this.quiet;
        if (quiet == null) {
            try {
                quiet = dctx.getModelService(getServiceName()).isEcaQuiet();
            } catch (Exception e) {
                Debug.logError(e, "Error finding service [" + getServiceName() + "]", module);
                quiet = false;
            }
            this.quiet = quiet;
        }
        return quiet;
    }

    /**
     * Returns true if should reload value from source, for "store" operation.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public boolean isReloadValue() {
        return reloadValue;
    }

    public void runAction(DispatchContext dctx, Map<String, ? extends Object> context, GenericEntity newValue) throws GenericEntityException {
        try {
            // pull out context parameters needed for this service.
            Map<String, Object> actionContext = dctx.getModelService(serviceName).makeValid(context, ModelService.IN_PARAM);
            // if value-attr is specified, insert the value object in that attr name
            if (!valueAttr.isEmpty()) {
                actionContext.put(valueAttr, newValue);
            }

            //Debug.logInfo("Running Entity ECA action service " + this.serviceName + " triggered by entity: " + value.getEntityName(), module);
            //Debug.logInfo("Running Entity ECA action service " + this.serviceName + "; value=" + value + "; actionContext=" + actionContext, module);

            // setup the run-as-user
            GenericValue userLoginToRunAs = null;
            if (!this.runAsUser.isEmpty()) {
                userLoginToRunAs = dctx.getDelegator().findOne("UserLogin", UtilMisc.toMap("userLoginId", this.runAsUser), true);
                if (userLoginToRunAs != null) {
                    actionContext.put("userLogin", userLoginToRunAs);
                }
            }

            LocalDispatcher dispatcher = dctx.getDispatcher();
            if ("sync".equals(this.serviceMode)) {
                Map<String, Object> actionResult = dispatcher.runSync(this.serviceName, actionContext);
                if (ServiceUtil.isError(actionResult)) {
                    throw new GenericServiceException("Error running Entity ECA action service: " + ServiceUtil.getErrorMessage(actionResult));
                }
                // use the result to update the context fields.
                if (resultToValue) {
                    newValue.setNonPKFields(actionResult);
                }
            } else if ("async".equals(this.serviceMode)) {
                dispatcher.runAsync(serviceName, actionContext, ServiceOptions.async(persist).jobPool(jobPool).priority(priority)); // SCIPIO: jobPool
            }
        } catch (GenericServiceException e) {
            // check abortOnError and rollbackOnError
            if (rollbackOnError) {
                String errMsg = "Entity ECA action service failed and rollback-on-error is true, so setting rollback only.";
                Debug.logError(errMsg, module);
                TransactionUtil.setRollbackOnly(errMsg, e);
            }

            if (this.abortOnError) {
                throw new EntityEcaException("Error running Entity ECA action service: " + e.toString(), e);
            } else {
                Debug.logError(e, "Error running Entity ECA action service", module);
            }
        }
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        if (UtilValidate.isNotEmpty(serviceName)) buf.append("[").append(serviceName).append("]");
        if (UtilValidate.isNotEmpty(serviceMode)) buf.append("[").append(serviceMode).append("]");
        if (UtilValidate.isNotEmpty(runAsUser)) buf.append("[").append(runAsUser).append("]");
        if (UtilValidate.isNotEmpty(valueAttr)) buf.append("[").append(valueAttr).append("]");
        if (UtilValidate.isNotEmpty(resultToValue)) buf.append("[").append(resultToValue).append("]");
        if (UtilValidate.isNotEmpty(abortOnError)) buf.append("[").append(abortOnError).append("]");
        if (UtilValidate.isNotEmpty(rollbackOnError)) buf.append("[").append(rollbackOnError).append("]");
        if (UtilValidate.isNotEmpty(persist)) buf.append("[").append(persist).append("]");
        if (UtilValidate.isNotEmpty(jobPool)) buf.append("[").append(jobPool).append("]");
        return buf.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((serviceName == null) ? 0 : serviceName.hashCode());
        result = prime * result + ((serviceMode == null) ? 0 : serviceMode.hashCode());
        result = prime * result + ((runAsUser == null) ? 0 : runAsUser.hashCode());
        result = prime * result + ((valueAttr == null) ? 0 : valueAttr.hashCode());
        result = prime * result + (resultToValue ? 1231 : 1237);
        result = prime * result + (abortOnError ? 1231 : 1237);
        result = prime * result + (rollbackOnError ? 1231 : 1237);
        result = prime * result + (persist ? 1231 : 1237);
        result = prime * result + ((jobPool == null) ? 0 : jobPool.hashCode()); // SCIPIO
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof EntityEcaAction) {
            EntityEcaAction other = (EntityEcaAction) obj;
            if (!UtilValidate.areEqual(this.serviceName, other.serviceName)) return false;
            if (!UtilValidate.areEqual(this.serviceMode, other.serviceMode)) return false;
            if (!UtilValidate.areEqual(this.runAsUser, other.runAsUser)) return false;
            if (!UtilValidate.areEqual(this.valueAttr, other.valueAttr)) return false;
            if (this.resultToValue != other.resultToValue) return false;
            if (this.abortOnError != other.abortOnError) return false;
            if (this.rollbackOnError != other.rollbackOnError) return false;
            if (this.persist != other.persist) return false;
            if (!UtilValidate.areEqual(this.jobPool, other.jobPool)) return false; // SCIPIO
            return true;
        } else {
            return false;
        }
    }
}
