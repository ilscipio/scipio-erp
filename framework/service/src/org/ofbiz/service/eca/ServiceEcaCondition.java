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
package org.ofbiz.service.eca;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.ofbiz.base.GeneralConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.w3c.dom.Element;

/**
 * ServiceEcaCondition
 * SCIPIO: Major refactor to allow better conditions.
 */
@SuppressWarnings("serial")
public abstract class ServiceEcaCondition implements java.io.Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static ServiceEcaCondition getCondition(Element element) { // SCIPIO: moved from EntityEcaRule construction
        if ("condition".equals(element.getNodeName())) {
            return new SingleServiceEcaCondition(element, true, false);
        } else if ("condition-field".equals(element.getNodeName())) {
            return new SingleServiceEcaCondition(element, false, false);
        } else if ("condition-service".equals(element.getNodeName())) {
            return new SingleServiceEcaCondition(element, false, true);
        } else if ("condition-property".equals(element.getNodeName())) {
            return new SingleServiceEcaCondition(element, true, false, true);
        } else if ("condition-property-field".equals(element.getNodeName())) {
            return new SingleServiceEcaCondition(element, false, false, true);
        } else if ("and".equals(element.getNodeName())) {
            return new AndServiceEcaCondition(element);
        } else if ("xor".equals(element.getNodeName())) {
            return new XorServiceEcaCondition(element);
        } else if ("or".equals(element.getNodeName())) {
            return new OrServiceEcaCondition(element);
        } else if ("not".equals(element.getNodeName())) {
            return new NotServiceEcaCondition(element);
        }
        return null;
    }

    public abstract String getShortDisplayDescription(boolean moreDetail);

    public abstract boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException;

    public static abstract class GroupServiceEcaCondition extends ServiceEcaCondition {
        protected final List<ServiceEcaCondition> conditions;

        protected GroupServiceEcaCondition(Element element) {
            List<? extends Element> childElementList = UtilXml.childElementList(element);
            List<ServiceEcaCondition> conditions = new ArrayList<>(childElementList.size());
            for (Element childElement : UtilXml.childElementList(element)) {
                conditions.add(getCondition(childElement));
            }
            if (UtilValidate.isEmpty(conditions)) {
                throw new IllegalArgumentException("Missing condition for service condition group operator: " + element.getNodeName());
            }
            this.conditions = conditions;
        }

        protected abstract String getOperator();

        public String getShortDisplayDescription(boolean moreDetail) {
            List<String> descriptions = new ArrayList<>();
            for(ServiceEcaCondition condition : conditions) {
                descriptions.add(condition.getShortDisplayDescription(moreDetail));
            }
            return "[" + getOperator() + ":" + descriptions + "]";
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            GroupServiceEcaCondition that = (GroupServiceEcaCondition) o;
            return conditions.equals(that.conditions);
        }

        @Override
        public int hashCode() {
            return Objects.hash(conditions);
        }

        @Override
        public String toString() {
            return "[" + getOperator() + ":" + conditions + "]";
        }
    }

    public static class AndServiceEcaCondition extends GroupServiceEcaCondition {
        protected AndServiceEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "and";
        }

        @Override
        public boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException {
            for (ServiceEcaCondition ec: conditions) {
                if (!ec.eval(serviceName, dctx, context)) {
                    return false;
                }
            }
            return true;
        }
    }

    public static class XorServiceEcaCondition extends GroupServiceEcaCondition {
        protected XorServiceEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "xor";
        }

        @Override
        public boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException {
            boolean foundOneTrue = false;
            for (ServiceEcaCondition ec : conditions) {
                if (ec.eval(serviceName, dctx, context)) {
                    if (foundOneTrue) {
                        // now found two true, so return false
                        return false;
                    }
                    foundOneTrue = true;
                }
            }
            return foundOneTrue;
        }
    }

    public static class OrServiceEcaCondition extends GroupServiceEcaCondition {
        protected OrServiceEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "or";
        }

        @Override
        public boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException {
            for (ServiceEcaCondition ec: conditions) {
                if (ec.eval(serviceName, dctx, context)) {
                    return true;
                }
            }
            return false;
        }
    }

    public static class NotServiceEcaCondition extends GroupServiceEcaCondition {
        protected final ServiceEcaCondition condition;
        protected NotServiceEcaCondition(Element element) {
            super(element);
            this.condition = conditions.get(0);
        }

        @Override
        protected String getOperator() {
            return "not";
        }

        @Override
        public boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException {
            return !condition.eval(serviceName, dctx, context);
        }
    }
    
    public static class SingleServiceEcaCondition extends ServiceEcaCondition {
        protected String conditionService = null;
        protected String lhsValueName = null;
        protected String rhsValueName = null;
        protected String lhsMapName = null;
        protected String rhsMapName = null;
        protected String operator = null;
        protected String compareType = null;
        protected String format = null;
        protected boolean isConstant = false;
        protected boolean isService = false;
        protected boolean property = false; // SCIPIO
        protected String propertyResource = null;

        public SingleServiceEcaCondition(Element condition, boolean isConstant, boolean isService, boolean property) { // SCIPIO: added property
            if (isService) {
                this.isService = isService;
                this.conditionService = condition.getAttribute("service-name");
            } else {
                this.lhsValueName = condition.getAttribute("field-name");
                this.lhsMapName = condition.getAttribute("map-name");

                this.isConstant = isConstant;
                if (isConstant) {
                    this.rhsValueName = condition.getAttribute("value");
                    this.rhsMapName = null;
                } else {
                    this.rhsValueName = condition.getAttribute("to-field-name");
                    this.rhsMapName = condition.getAttribute("to-map-name");
                }

                this.operator = condition.getAttribute("operator");
                this.compareType = condition.getAttribute("type");
                this.format = condition.getAttribute("format");
                this.property = property; // SCIPIO
                this.propertyResource = condition.getAttribute("resource");
                if (property) {
                    this.lhsValueName = condition.getAttribute("property-name");
                    int splitIndex = this.lhsValueName.indexOf('#');
                    if (this.propertyResource.isEmpty() && splitIndex >= 1) {
                        this.propertyResource = this.lhsValueName.substring(0, splitIndex);
                        this.lhsValueName = this.lhsValueName.substring(splitIndex + 1);
                    }
                    if (this.propertyResource.isEmpty()) {
                        this.propertyResource = null;
                    }
                }
            }
        }

        public SingleServiceEcaCondition(Element condition, boolean isConstant, boolean isService) { // SCIPIO: added property
            this(condition, isConstant, isService, false);
        }

        @Override
        public String getShortDisplayDescription(boolean moreDetail) {
            StringBuilder buf = new StringBuilder();
            if (isService) {
                buf.append("[").append(conditionService).append("]");
            } else {
                buf.append("[");
                if (UtilValidate.isNotEmpty(lhsMapName)) buf.append(lhsMapName).append(".");
                buf.append(lhsValueName);
                buf.append(":").append(operator).append(":");
                if (UtilValidate.isNotEmpty(rhsMapName)) buf.append(rhsMapName).append(".");
                buf.append(rhsValueName);

                if (moreDetail) {
                    if (UtilValidate.isNotEmpty(compareType)) {
                        buf.append("-");
                        buf.append(compareType);
                    }
                    if (UtilValidate.isNotEmpty(format)) {
                        buf.append(";");
                        buf.append(format);
                    }
                }

                buf.append("]");
            }
            return buf.toString();
        }

        @Override
        public boolean eval(String serviceName, DispatchContext dctx, Map<String, Object> context) throws GenericServiceException {
            if (serviceName == null || dctx == null || context == null || dctx.getClassLoader() == null) {
                throw new GenericServiceException("Cannot have null Service, Context or DispatchContext!");
            }

            if (Debug.verboseOn()) Debug.logVerbose(this.toString() + ", In the context: " + context, module);

            // condition-service; run the service and return the reply result
            if (isService) {
                LocalDispatcher dispatcher = dctx.getDispatcher();
                Map<String, Object> conditionServiceResult = dispatcher.runSync(conditionService,
                        UtilMisc.<String, Object>toMap("serviceContext", context, "serviceName", serviceName,
                                "userLogin", context.get("userLogin")));

                Boolean conditionReply = Boolean.FALSE;
                if (ServiceUtil.isError(conditionServiceResult)) {
                    Debug.logError("Error in condition-service : " +
                            ServiceUtil.getErrorMessage(conditionServiceResult), module);
                } else {
                    conditionReply = (Boolean) conditionServiceResult.get("conditionReply");
                }
                return conditionReply;
            }

            Object lhsValue = null;
            Object rhsValue = null;

            if (property) {
                if (propertyResource != null) {
                    lhsValue = EntityUtilProperties.getPropertyValue(propertyResource, lhsValueName, dctx.getDelegator());
                } else {
                    lhsValue = GeneralConfig.getCommonPropertiesMap().get(lhsValueName);
                    if (lhsValue == null) {
                        Debug.logWarning("Could not find property named '" + lhsValueName
                                + "' in GeneralConfig.getCommonPropertiesMap for eca (invalid common name or missing resource); returning false", module);
                        return false;
                    }
                }
            } else {
                if (UtilValidate.isNotEmpty(lhsMapName)) {
                    try {
                        if (context.containsKey(lhsMapName)) {
                            Map<String, ? extends Object> envMap = UtilGenerics.checkMap(context.get(lhsMapName));
                            lhsValue = envMap.get(lhsValueName);
                        } else {
                            Debug.logInfo("From Map (" + lhsMapName + ") not found in context, defaulting to null.", module);
                        }
                    } catch (ClassCastException e) {
                        throw new GenericServiceException("From Map field [" + lhsMapName + "] is not a Map.", e);
                    }
                } else {
                    if (context.containsKey(lhsValueName)) {
                        lhsValue = context.get(lhsValueName);
                    } else {
                        Debug.logInfo("From Field (" + lhsValueName + ") is not found in context for " + serviceName + ", defaulting to null.", module);
                    }
                }
            }

            if (isConstant) {
                rhsValue = rhsValueName;
            } else if (UtilValidate.isNotEmpty(rhsMapName)) {
                try {
                    if (context.containsKey(rhsMapName)) {
                        Map<String, ? extends Object> envMap = UtilGenerics.checkMap(context.get(rhsMapName));
                        rhsValue = envMap.get(rhsValueName);
                    } else {
                        Debug.logInfo("To Map (" + rhsMapName + ") not found in context for " + serviceName + ", defaulting to null.", module);
                    }
                } catch (ClassCastException e) {
                    throw new GenericServiceException("To Map field [" + rhsMapName + "] is not a Map.", e);
                }
            } else {
                if (context.containsKey(rhsValueName)) {
                    rhsValue = context.get(rhsValueName);
                } else {
                    Debug.logInfo("To Field (" + rhsValueName + ") is not found in context for " + serviceName + ", defaulting to null.", module);
                }
            }

            if (Debug.verboseOn())
                Debug.logVerbose("Comparing : " + lhsValue + " " + operator + " " + rhsValue, module);

            // evaluate the condition & invoke the action(s)
            List<Object> messages = new ArrayList<>(); // SCIPIO: switched to ArrayList
            Boolean cond = ObjectType.doRealCompare(lhsValue, rhsValue, operator, compareType, format, messages, null, dctx.getClassLoader(), isConstant);

            // if any messages were returned send them out
            if (messages.size() > 0 && Debug.warningOn()) {
                for (Object message : messages) {
                    Debug.logWarning(message.toString(), module);
                }
            }
            if (cond != null) {
                return cond;
            } else {
                Debug.logWarning("doRealCompare returned null, returning false", module);
                return false;
            }
        }

        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();

            if (UtilValidate.isNotEmpty(conditionService)) buf.append("[").append(conditionService).append("]");
            if (UtilValidate.isNotEmpty(lhsMapName)) buf.append("[").append(lhsMapName).append("]");
            if (UtilValidate.isNotEmpty(lhsValueName)) buf.append("[").append(lhsValueName).append("]");
            if (UtilValidate.isNotEmpty(operator)) buf.append("[").append(operator).append("]");
            if (UtilValidate.isNotEmpty(rhsMapName)) buf.append("[").append(rhsMapName).append("]");
            if (UtilValidate.isNotEmpty(rhsValueName)) buf.append("[").append(rhsValueName).append("]");
            if (UtilValidate.isNotEmpty(isConstant)) buf.append("[").append(isConstant).append("]");
            if (UtilValidate.isNotEmpty(compareType)) buf.append("[").append(compareType).append("]");
            if (UtilValidate.isNotEmpty(format)) buf.append("[").append(format).append("]");
            return buf.toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((compareType == null) ? 0 : compareType.hashCode());
            result = prime * result + ((conditionService == null) ? 0 : conditionService.hashCode());
            result = prime * result + ((format == null) ? 0 : format.hashCode());
            result = prime * result + (isConstant ? 1231 : 1237);
            result = prime * result + (isService ? 1231 : 1237);
            result = prime * result + ((lhsMapName == null) ? 0 : lhsMapName.hashCode());
            result = prime * result + ((lhsValueName == null) ? 0 : lhsValueName.hashCode());
            result = prime * result + ((operator == null) ? 0 : operator.hashCode());
            result = prime * result + ((rhsMapName == null) ? 0 : rhsMapName.hashCode());
            result = prime * result + ((rhsValueName == null) ? 0 : rhsValueName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof SingleServiceEcaCondition) {
                SingleServiceEcaCondition other = (SingleServiceEcaCondition) obj;

                if (!UtilValidate.areEqual(this.conditionService, other.conditionService)) return false;
                if (!UtilValidate.areEqual(this.lhsValueName, other.lhsValueName)) return false;
                if (!UtilValidate.areEqual(this.rhsValueName, other.rhsValueName)) return false;
                if (!UtilValidate.areEqual(this.lhsMapName, other.lhsMapName)) return false;
                if (!UtilValidate.areEqual(this.rhsMapName, other.rhsMapName)) return false;
                if (!UtilValidate.areEqual(this.operator, other.operator)) return false;
                if (!UtilValidate.areEqual(this.compareType, other.compareType)) return false;
                if (!UtilValidate.areEqual(this.format, other.format)) return false;

                if (this.isConstant != other.isConstant) return false;
                if (this.isService != other.isService) return false;

                return true;
            } else {
                return false;
            }
        }
    }
}
