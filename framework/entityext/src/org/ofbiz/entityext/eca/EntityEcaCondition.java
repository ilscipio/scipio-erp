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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.ofbiz.base.GeneralConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.w3c.dom.Element;

/**
 * EntityEcaCondition
 * SCIPIO: Major refactor to allow better conditions.
 */
@SuppressWarnings("serial")
public abstract class EntityEcaCondition implements java.io.Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static EntityEcaCondition getCondition(Element element) { // SCIPIO: moved from EntityEcaRule construction
        if ("condition".equals(element.getNodeName())) {
            return new SingleEntityEcaCondition(element, true, false);
        } else if ("condition-field".equals(element.getNodeName())) {
            return new SingleEntityEcaCondition(element, false, false);
        } else if ("condition-service".equals(element.getNodeName())) {
            return new SingleEntityEcaCondition(element, false, true);
        } else if ("condition-property".equals(element.getNodeName())) {
            return new SingleEntityEcaCondition(element, true, false, true);
        } else if ("condition-property-field".equals(element.getNodeName())) {
            return new SingleEntityEcaCondition(element, false, false, true);
        } else if ("and".equals(element.getNodeName())) {
            return new AndEntityEcaCondition(element);
        } else if ("xor".equals(element.getNodeName())) {
            return new XorEntityEcaCondition(element);
        } else if ("or".equals(element.getNodeName())) {
            return new OrEntityEcaCondition(element);
        } else if ("not".equals(element.getNodeName())) {
            return new NotEntityEcaCondition(element);
        }
        return null;
    }

    public abstract Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException; // SCIPIO: scope

    public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context) throws GenericEntityException { // SCIPIO: scope
        return eval(dctx, value, context, null);
    }

    protected abstract List<String> getFieldNames();

    public static abstract class GroupEntityEcaCondition extends EntityEcaCondition {
        protected final List<EntityEcaCondition> conditions;

        protected GroupEntityEcaCondition(Element element) {
            List<? extends Element> childElementList = UtilXml.childElementList(element);
            List<EntityEcaCondition> conditions = new ArrayList<>(childElementList.size());
            for (Element childElement : UtilXml.childElementList(element)) {
                conditions.add(getCondition(childElement));
            }
            if (UtilValidate.isEmpty(conditions)) {
                throw new IllegalArgumentException("Missing condition for entity condition group operator: " + element.getNodeName());
            }
            this.conditions = conditions;
        }

        protected abstract String getOperator();

        @Override
        protected List<String> getFieldNames() {
            List<String> fieldNames = new ArrayList<String>();
            for(EntityEcaCondition cond : conditions) {
                fieldNames.addAll(cond.getFieldNames());
            }
            return fieldNames;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            GroupEntityEcaCondition that = (GroupEntityEcaCondition) o;
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

    public static class AndEntityEcaCondition extends GroupEntityEcaCondition {
        protected AndEntityEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "and";
        }

        @Override
        public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException {
            Boolean result = null; // SCIPIO: if all null, returns null; otherwise AND non-nulls
            for (EntityEcaCondition ec: conditions) {
                Boolean subResult = ec.eval(dctx, value, context, scope);
                if (subResult != null) {
                    if (!subResult) {
                        return false;
                    } else {
                        result = true;
                    }
                }
            }
            return result;
        }
    }

    public static class XorEntityEcaCondition extends GroupEntityEcaCondition {
        protected XorEntityEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "xor";
        }

        @Override
        public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException {
            Boolean foundOneTrue = null; // SCIPIO: if all null, returns null; otherwise XOR non-nulls
            for (EntityEcaCondition ec : conditions) {
                Boolean subResult = ec.eval(dctx, value, context, scope);
                if (subResult != null) {
                    if (subResult) {
                        if (foundOneTrue != null && foundOneTrue) {
                            // now found two true, so return false
                            return false;
                        }
                        foundOneTrue = true;
                    } else if (foundOneTrue == null) {
                        foundOneTrue = false;
                    }
                }
            }
            return foundOneTrue;
        }
    }

    public static class OrEntityEcaCondition extends GroupEntityEcaCondition {
        protected OrEntityEcaCondition(Element element) {
            super(element);
        }

        @Override
        protected String getOperator() {
            return "or";
        }

        @Override
        public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException {
            Boolean result = null; // SCIPIO: if all null, returns null; otherwise OR non-nulls
            for (EntityEcaCondition ec: conditions) {
                Boolean subResult = ec.eval(dctx, value, context, scope);
                if (subResult != null) {
                    if (subResult) {
                        return true;
                    } else {
                        result = false;
                    }
                }
            }
            return result;
        }
    }

    public static class NotEntityEcaCondition extends GroupEntityEcaCondition {
        protected final EntityEcaCondition condition;
        protected NotEntityEcaCondition(Element element) {
            super(element);
            this.condition = conditions.get(0);
        }

        @Override
        protected String getOperator() {
            return "not";
        }

        @Override
        public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException {
            Boolean subResult = condition.eval(dctx, value, context, scope); // SCIPIO: null
            return (subResult != null) ? !subResult : null;
        }
    }

    public static class SingleEntityEcaCondition extends EntityEcaCondition { // TODO: split up into subclasses (inefficient)
        protected String lhsValueName = null;
        protected FlexibleMapAccessor<Object> lhsContextValueNameExdr = null; // SCIPIO: 3.0.0: Added
        protected String rhsValue = null;
        protected FlexibleMapAccessor<Object> rhsValueNameExdr = null; // SCIPIO: 3.0.0: Added
        protected FlexibleStringExpander rhsValueExdr = null; // SCIPIO: 3.0.0: Added
        protected String operator = null;
        protected String compareType = null;
        protected String format = null;
        protected boolean isValue = false;
        protected boolean isService = false;
        protected String conditionService = null;
        protected boolean property = false; // SCIPIO
        protected String propertyResource = null;
        protected Set<String> scopes = null; // SCIPIO
        protected String scopeString = null; // SCIPIO

        public SingleEntityEcaCondition(Element condition, boolean isValue, boolean isService, boolean property) { // SCIPIO: added property
            if (isService) {
                this.isService = isService;
                this.conditionService = condition.getAttribute("service-name");
            } else {
                this.lhsValueName = condition.getAttribute("field-name");
                String contextField = condition.getAttribute("context-field");
                if (UtilValidate.isNotEmpty(contextField)) {
                    this.lhsContextValueNameExdr = FlexibleMapAccessor.getInstance(contextField);
                }

                // SCIPIO: 3.0.0: This seems to be pointless; just use present of value vs to-field-name to determine
                //this.constant = constant;
                //if (constant) {
                //    this.rhsValueName = condition.getAttribute("value");
                //} else {
                //    this.rhsValueName = condition.getAttribute("to-field-name");
                //}

                String rhsValue = condition.getAttribute("to-field-name");
                if (UtilValidate.isNotEmpty(rhsValue)) {
                    isValue = false;
                    if (rhsValue.contains(FlexibleStringExpander.openBracket)) {
                        this.rhsValueNameExdr = FlexibleMapAccessor.getInstance(this.rhsValue);
                    }
                } else {
                    isValue = true;
                    rhsValue = condition.getAttribute("value");
                    if (rhsValue.contains(FlexibleStringExpander.openBracket)) {
                        this.rhsValueExdr = FlexibleStringExpander.getInstance(this.rhsValue);
                    }
                }
                this.isValue = isValue;
                this.rhsValue = rhsValue;

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
            this.scopeString = UtilValidate.nullIfEmpty(condition.getAttribute("scope"));
            if (this.scopeString != null) {
                scopes = Collections.unmodifiableSet(StringUtil.splitNames(new HashSet<>(), this.scopeString));
            }
        }

        public SingleEntityEcaCondition(Element condition, boolean isValue, boolean isService) {
            this(condition, isValue, isService, false);
        }

        @Override
        public Boolean eval(DispatchContext dctx, GenericEntity value, Map<String, Object> context, String scope) throws GenericEntityException {
            if (scope == null || "run".equals(scope)) { // SCIPIO
                if (scopes != null && !scopes.contains("run")) {
                    return null; // null means not applicable - skip this condition
                }
            } else {
                if (scopes == null || !scopes.contains(scope)) {
                    return null;
                }
            }

            if (dctx == null || value == null || dctx.getClassLoader() == null) {
                throw new GenericEntityException("Cannot have null Value or DispatchContext!");
            }

            if (Debug.verboseOn()) Debug.logVerbose(this.toString(), module);

            // condition-service; run the service and return the reply result
            if (isService) {
                try {
                    LocalDispatcher dispatcher = dctx.getDispatcher();
                    Map<String, Object> conditionServiceResult = dispatcher.runSync(conditionService,
                            UtilMisc.<String, Object>toMap("serviceContext", context, "userLogin", context.get("userLogin")));

                    Boolean conditionReply = Boolean.FALSE;
                    if (ServiceUtil.isError(conditionServiceResult)) {
                        Debug.logError("Error in condition-service : " +
                                ServiceUtil.getErrorMessage(conditionServiceResult), module);
                    } else {
                        conditionReply = (Boolean) conditionServiceResult.get("conditionReply");
                    }
                    return conditionReply;
                } catch (GenericServiceException gse) {
                    throw new GenericEntityException("Error in calling condition service " + conditionService + ". " + gse.getMessage());
                }
            }

            // SCIPIO
            //Object lhsValue = value.get(lhsValueName);
            Object lhsValue;
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
                if (lhsContextValueNameExdr != null) {
                    lhsValue = lhsContextValueNameExdr.get(context);
                } else {
                    lhsValue = value.get(lhsValueName);
                }
            }

            Object rhsValue;
            if (isValue) {
                if (this.rhsValueExdr != null) {
                    rhsValue = this.rhsValueExdr.expand(context);
                } else {
                    rhsValue = this.rhsValue;
                }
            } else {
                if (this.rhsValueNameExdr != null) {
                    rhsValue = this.rhsValueNameExdr.get(context);
                } else {
                    rhsValue = value.get(this.rhsValue);
                }
            }

            if (Debug.verboseOn())
                Debug.logVerbose("Comparing : " + lhsValue + " " + operator + " " + rhsValue, module);

            // evaluate the condition & invoke the action(s)
            List<Object> messages = new ArrayList<Object>(); // SCIPIO: ArrayList
            Boolean cond = ObjectType.doRealCompare(lhsValue, rhsValue, operator, compareType, format, messages, null, dctx.getClassLoader(), isValue);

            // if any messages were returned send them out
            if (messages.size() > 0) {
                for (Object message : messages) {
                    Debug.logWarning((String) message, module);
                }
            }
            if (cond != null) {
                return cond;
            } else {
                return false;
            }
        }

        public String getLValue() {
            return this.lhsValueName;
        }

        public String getRValue() {
            if (isValue && !rhsValue.isEmpty()) {
                return "\"".concat(this.rhsValue).concat("\"");
            }
            return this.rhsValue;
        }

        public String getOperator() {
            return this.operator;
        }

        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();
            if (UtilValidate.isNotEmpty(conditionService)) buf.append("[").append(conditionService).append("]");
            if (UtilValidate.isNotEmpty(propertyResource)) buf.append("[").append(propertyResource).append("]");
            if (UtilValidate.isNotEmpty(lhsValueName)) buf.append("[").append(lhsValueName).append("]");
            if (UtilValidate.isNotEmpty(operator)) buf.append("[").append(operator).append("]");
            if (UtilValidate.isNotEmpty(rhsValue)) buf.append("[").append(rhsValue).append("]");
            if (UtilValidate.isNotEmpty(isValue)) buf.append("[").append(isValue).append("]");
            if (UtilValidate.isNotEmpty(compareType)) buf.append("[").append(compareType).append("]");
            if (UtilValidate.isNotEmpty(format)) buf.append("[").append(format).append("]");
            if (UtilValidate.isNotEmpty(scopeString)) buf.append("[").append(scopeString).append("]"); // SCIPIO
            return buf.toString();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((lhsValueName == null) ? 0 : lhsValueName.hashCode());
            result = prime * result + ((operator == null) ? 0 : operator.hashCode());
            result = prime * result + ((rhsValue == null) ? 0 : rhsValue.hashCode());
            result = prime * result + (isValue ? 1231 : 1237);
            result = prime * result + ((compareType == null) ? 0 : compareType.hashCode());
            result = prime * result + ((format == null) ? 0 : format.hashCode());
            result = prime * result + ((scopeString == null) ? 0 : scopeString.hashCode());
            // SCIPIO: FIXME: should more fields be here, or is this hashcode intended to differ from equals?? (unusual)
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof SingleEntityEcaCondition) {
                SingleEntityEcaCondition other = (SingleEntityEcaCondition) obj;

                if (!UtilValidate.areEqual(this.conditionService, other.conditionService)) return false;
                if (!UtilValidate.areEqual(this.lhsValueName, other.lhsValueName)) return false;
                if (!UtilValidate.areEqual(this.rhsValue, other.rhsValue)) return false;
                if (!UtilValidate.areEqual(this.operator, other.operator)) return false;
                if (!UtilValidate.areEqual(this.compareType, other.compareType)) return false;
                if (!UtilValidate.areEqual(this.format, other.format)) return false;
                if (this.isValue != other.isValue) return false;
                if (this.isService != other.isService) return false;
                if (this.property != other.property) return false; // SCIPIO
                if (!UtilValidate.areEqual(this.propertyResource, other.propertyResource)) return false; // SCIPIO
                if (!UtilValidate.areEqual(this.scopeString, other.scopeString)) return false; // SCIPIO: FIXME: inaccurate

                return true;
            } else {
                return false;
            }
        }

        @Override
        protected List<String> getFieldNames() {
            List<String> fieldNameList = new ArrayList<String>();
            if (property) {
                return fieldNameList; // SCIPIO
            }
            if (UtilValidate.isNotEmpty(lhsValueName)) {
                fieldNameList.add(lhsValueName);
            }
            if (!isValue && UtilValidate.isNotEmpty(rhsValue)) {
                fieldNameList.add(rhsValue);
            }
            return fieldNameList;
        }
    }
}
