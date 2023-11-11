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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.eeca.Eeca;
import com.ilscipio.scipio.service.def.eeca.EecaAction;
import com.ilscipio.scipio.service.def.eeca.EecaSet;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.w3c.dom.Element;

/**
 * Entity event-condition-action rule.
 */
@SuppressWarnings("serial")
public final class EntityEcaRule implements java.io.Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final String entityName;
    private final String operationName;
    private final String eventName;
    private final boolean runOnError;
    private final List<EntityEcaCondition> conditions;
    private final List<Object> actionsAndSets;
    private boolean enabled = true;
    private final List<String> conditionFieldNames  = new ArrayList<String>();
    protected transient Boolean initEnabled = null;
    private final boolean reloadValue; // SCIPIO

    public EntityEcaRule(Element eca) {
        this.entityName = eca.getAttribute("entity");
        this.operationName = eca.getAttribute("operation");
        this.eventName = eca.getAttribute("event");
        this.runOnError = "true".equals(eca.getAttribute("run-on-error"));
        this.enabled = !"false".equals(eca.getAttribute("enabled"));
        ArrayList<EntityEcaCondition> conditions = new ArrayList<EntityEcaCondition>();
        ArrayList<Object> actionsAndSets = new ArrayList<Object>();
        boolean reloadValue = false;
        for (Element element: UtilXml.childElementList(eca)) {
            // SCIPIO: refactored to EntityEcaCondition
            EntityEcaCondition condition = EntityEcaCondition.getCondition(element);
            if (condition != null) {
                conditions.add(condition);
                conditionFieldNames.addAll(condition.getFieldNames());
            } else if ("action".equals(element.getNodeName())) {
                EntityEcaAction ecaAction = new EntityEcaAction(element);
                if (ecaAction.isReloadValue()) {
                    reloadValue = true;
                }
                actionsAndSets.add(ecaAction);
            } else if ("set".equals(element.getNodeName())) {
                actionsAndSets.add(new EntityEcaSetField(element));
            } else {
                Debug.logWarning("Invalid eca child element " + element.getNodeName(), module);
            }
        }
        conditions.trimToSize();
        this.conditions = Collections.unmodifiableList(conditions);
        actionsAndSets.trimToSize();
        this.actionsAndSets = Collections.unmodifiableList(actionsAndSets);
        this.reloadValue = reloadValue; // SCIPIO
        if (Debug.verboseOn()) {
            Debug.logVerbose("Conditions: " + conditions, module);
            Debug.logVerbose("actions and sets (intermixed): " + actionsAndSets, module);
        }
    }

    /**
     * Annotations constructor.
     *
     * <p>NOTE: serviceClass null when serviceMethod set and vice-versa.</p>
     *
     * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
     */
    public EntityEcaRule(Eeca eecaDef, Service serviceDef, Class<?> serviceClass, Method serviceMethod) {
        this.entityName = eecaDef.entity();
        this.operationName = eecaDef.operation();
        this.eventName = eecaDef.event();
        this.runOnError = "true".equals(eecaDef.runOnError());
        this.enabled = !"false".equals(eecaDef.enabled());

        ArrayList<Object> actionsAndSets = new ArrayList<Object>();
        boolean reloadValue = false;

        // Global assignments
        List<EecaSet> assignments = new ArrayList<>(Arrays.asList(eecaDef.assignments()));
        for (EecaSet assignmentDef : assignments) {
            actionsAndSets.add(new EntityEcaSetField(assignmentDef, eecaDef, serviceDef, serviceClass, serviceMethod));
        }

        // Actions and local assignments
        List<EecaAction> actions = new ArrayList<>(Arrays.asList(eecaDef.actions()));
        if (actions.isEmpty()) {
            actions.add(EecaAction.DefaultType.class.getAnnotation(EecaAction.class));
        }
        for (EecaAction action : actions) {
            for (EecaSet assignmentDef : action.assignments()) {
                actionsAndSets.add(new EntityEcaSetField(assignmentDef, eecaDef, serviceDef, serviceClass, serviceMethod));
            }
            EntityEcaAction ecaAction = new EntityEcaAction(action, eecaDef, serviceDef, serviceClass, serviceMethod);
            if (ecaAction.isReloadValue()) {
                reloadValue = true;
            }
            actionsAndSets.add(ecaAction);
        }

        this.conditions = List.of();
        actionsAndSets.trimToSize();
        this.actionsAndSets = Collections.unmodifiableList(actionsAndSets);
        this.reloadValue = reloadValue;
        if (Debug.verboseOn()) {
            Debug.logVerbose("actions and sets (intermixed): " + actionsAndSets, module);
        }
    }

    public String getEntityName() {
        return this.entityName;
    }

    public String getOperationName() {
        return this.operationName;
    }

    public String getEventName() {
        return this.eventName;
    }

    public boolean getRunOnError() {
        return this.runOnError;
    }

    public List<Object> getActionsAndSets() {
        return this.actionsAndSets;
    }

    public List<EntityEcaCondition> getConditions() {
        return this.conditions;
    }

    /**
     * Returns true if should reload value from source, for "store" operation.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public boolean isReloadValue() {
        return reloadValue;
    }

    public void eval(String currentOperation, DispatchContext dctx, GenericEntity value, boolean isError, Set<String> actionsRun) throws GenericEntityException {
        // SCIPIO: Now incorporated into initEnabled for speed
        //if (!enabled) {
        //    if (Debug.verboseOn()) {
        //        Debug.logVerbose("Entity ECA [" + this.entityName + "] on [" + this.eventName + "] is disabled; not running.", module);
        //    }
        //    return;
        //}
        if (!isEnabled(dctx, Collections.emptyMap())) { // SCIPIO
            return;
        }
        //Debug.logInfo("eval eeca rule: operation=" + currentOperation + ", in event=" + this.eventName + ", on entity=" + this.entityName + ", for value=" + value, module);
        if (isError && !this.runOnError) {
            return;
        }

        if (!"any".equals(this.operationName) && this.operationName.indexOf(currentOperation) == -1) {
            return;
        }
        // Are fields tested in a condition missing? If so, we need to load them
        List<String> fieldsToLoad = new ArrayList<String>();
        for( String conditionFieldName : conditionFieldNames) {
            if( value.get(conditionFieldName) == null) {
                fieldsToLoad.add(conditionFieldName);
            }
        }

        if(!fieldsToLoad.isEmpty()) {
            Delegator delegator = dctx.getDelegator();
            GenericValue oldValue = EntityQuery.use(delegator).from(entityName).where(value.getPrimaryKey()).queryOne();
            if(UtilValidate.isNotEmpty(oldValue)) {
                for (String fieldName : fieldsToLoad) {
                    value.put(fieldName, oldValue.get(fieldName));
                }
            }
        }

        Map<String, Object> context = new HashMap<String, Object>();
        context.putAll(value);

        boolean allCondTrue = true;
        for (EntityEcaCondition ec: conditions) {
            Boolean subResult = ec.eval(dctx, value, context, null); // SCIPIO: null
            if (subResult != null) {
                if (!subResult) {
                    allCondTrue = false;
                    break;
                }
            }
        }

        if (allCondTrue) {
            for (Object actionOrSet: actionsAndSets) {
                if (actionOrSet instanceof EntityEcaAction) {
                    EntityEcaAction ea = (EntityEcaAction) actionOrSet;
                    // in order to enable OR logic without multiple calls to the given service,
                    //only execute a given service name once per service call phase
                    if (actionsRun.add(ea.getServiceName())) {
                        if (!ea.isQuiet(dctx) && Debug.infoOn()) { // SCIPIO: Added quiet check
                            Debug.logInfo("Running Entity ECA Service: " + ea.getServiceName() + ", triggered by rule on Entity: " + value.getEntityName(), module);
                        }
                        ea.runAction(dctx, context, value);
                    }
                } else {
                    EntityEcaSetField sf = (EntityEcaSetField) actionOrSet;
                    sf.eval(context);
                }
            }
        }
    }

    /**
     * @deprecated Not thread-safe, no replacement.
     * @param enabled
     */
    @Deprecated
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isEnabled() {
        return this.enabled;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((entityName == null) ? 0 : entityName.hashCode());
        result = prime * result + ((operationName == null) ? 0 : operationName.hashCode());
        result = prime * result + ((eventName == null) ? 0 : eventName.hashCode());
        result = prime * result + ((actionsAndSets == null) ? 0 : actionsAndSets.hashCode());
        result = prime * result + ((conditions == null) ? 0 : conditions.hashCode());
        result = prime * result + ((conditionFieldNames == null) ? 0 : conditionFieldNames.hashCode());
        // SCIPIO: 2018-10-09: TODO: REVIEW: this is not in equals, can't be here...
        //result = prime * result + (enabled ? 1231 : 1237);
        result = prime * result + (runOnError ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof EntityEcaRule) {
            EntityEcaRule other = (EntityEcaRule) obj;
            if (!UtilValidate.areEqual(this.entityName, other.entityName)) {
                return false;
            }
            if (!UtilValidate.areEqual(this.operationName, other.operationName)) {
                return false;
            }
            if (!UtilValidate.areEqual(this.eventName, other.eventName)) {
                return false;
            }
            if (!this.conditions.equals(other.conditions)) {
                return false;
            }
            if (!this.actionsAndSets.equals(other.actionsAndSets)) {
                return false;
            }
            if (!this.conditionFieldNames.equals(other.conditionFieldNames)) {
                return false;
            }

            if (this.runOnError != other.runOnError) {
                return false;
            }

            return true;
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return "EntityEcaRule:" + this.entityName + ":" + this.operationName + ":" + this.eventName +  ":runOnError=" + this.runOnError + ":enabled=" + this.enabled + ":conditions=" + this.conditions + ":actionsAndSets=" + this.actionsAndSets + ":conditionFieldNames" + this.conditionFieldNames;
    }

    protected final boolean isEnabled(DispatchContext dctx, Map<String, Object> context) { // SCIPIO
        Boolean initEnabled = this.initEnabled;
        if (initEnabled == null) {
            synchronized (this) {
                initEnabled = this.initEnabled;
                if (initEnabled == null) {
                    if (!enabled) {
                        initEnabled = false;
                        if (Debug.infoOn()) {
                            Debug.logInfo("Entity ECA [" + this.entityName + "] on [" + this.eventName + "] is disabled globally.", module);
                        }
                    } else {
                        initEnabled = checkInitConditions(dctx, context, true);
                    }
                    this.initEnabled = initEnabled;
                }
            }
        }
        return initEnabled;
    }

    protected boolean checkInitConditions(DispatchContext dctx, Map<String, Object> context, boolean log) { // SCIPIO
        for(EntityEcaCondition cond : conditions) {
            try {
                Boolean subResult = cond.eval(dctx, dctx.getDelegator().makeValue(entityName), context, "init");
                if (Boolean.FALSE.equals(subResult)) {
                    if (log && Debug.infoOn()) {
                        Debug.logInfo("Entity ECA [" + this.entityName + "] on [" + this.eventName + "] is disabled by init condition: " + cond, module);
                    }
                    return false;
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Could not check eca condition for entity [" + entityName + "] at init scope: " + cond, module);
            }
        }
        return true;
    }
}
