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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

/**
 * SCIPIO: (New) Event utilities and definitions.
 * <p>
 * TODO/FIXME: {@link #runServiceAsEvent} static method should be replaced with a helper invoker instance because there are too many possible options,
 *  so for now only the readable overloads are public (calling code will be unreadable/too complicated)
 */
public final class EventUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String ERROR_MESSAGE_LIST = "_ERROR_MESSAGE_LIST_";
    public static final String ERROR_MESSAGE_MAP = "_ERROR_MESSAGE_MAP_";
    public static final String ERROR_MESSAGE = "_ERROR_MESSAGE_";
    public static final String EVENT_MESSAGE_LIST = "_EVENT_MESSAGE_LIST_";
    public static final String EVENT_MESSAGE = "_EVENT_MESSAGE_";

    private static final Set<String> eventMessageAttrNames = UtilMisc.unmodifiableHashSet(
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);
    private static final Set<String> errorMessageAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE);
    private static final Set<String> eventErrorMessageAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE,
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);

    private static final List<String> eventMessageAttrNamesList = Collections.unmodifiableList(new ArrayList<>(eventMessageAttrNames));
    private static final List<String> errorMessageAttrNamesList = Collections.unmodifiableList(new ArrayList<>(errorMessageAttrNames));
    private static final List<String> eventErrorMessageAttrNamesList = Collections.unmodifiableList(new ArrayList<>(eventErrorMessageAttrNames));

    private EventUtil() {
    }

    /**
     * Returns the standard event regular (success) AND error message attribute names:
     *  _ERROR_MESSAGE_LIST_, _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventErrorMessageAttrNames() {
        return eventErrorMessageAttrNames;
    }

    /**
     * Returns the standard event regular (success) AND error message attribute names as a list:
     *  _ERROR_MESSAGE_LIST_, _EVENT_MESSAGE_LIST_, etc.
     */
    public static List<String> getEventErrorMessageAttrNamesList() {
        return eventErrorMessageAttrNamesList;
    }

    public static boolean isEventErrorMessageAttrName(String attributeName) {
        return eventErrorMessageAttrNames.contains(attributeName);
    }

    public static Map<String, Object> getEventErrorAttributesAsMap(HttpServletRequest request, Map<String, Object> outMap) {
        return UtilHttp.requestAttributesToMap(request, outMap, eventErrorMessageAttrNamesList);
    }

    public static Map<String, Object> getEventErrorAttributesAsMap(HttpServletRequest request) {
        return getEventErrorAttributesAsMap(request, new HashMap<>());
    }

    public static void setEventErrorAttributesFromMap(HttpServletRequest request, Map<String, ?> map) {
        UtilHttp.setRequestAttributesFromMap(request, map, eventErrorMessageAttrNamesList);
    }

    public static void clearEventErrorAttributes(HttpServletRequest request) {
        UtilHttp.setRequestAttributesFromMap(request, Collections.emptyMap(), eventErrorMessageAttrNamesList);
    }

    public static <V> Map<String, V> copyEventErrorMessages(Map<String, ? extends V> srcMap, Map<String, V> dstMap) {
        return UtilMisc.copyKeys(srcMap, dstMap, eventErrorMessageAttrNamesList);
    }

    /**
     * @deprecated use {@link #setEventErrorAttributesFromMap}
     */
    @Deprecated
    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, HttpServletRequest request) {
        setEventErrorAttributesFromMap(request, serviceResult);
    }

    /**
     * @deprecated use {@link #copyEventErrorMessages}
     */
    @Deprecated
    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, Map<String, Object> targetAttributes) {
        copyEventErrorMessages(serviceResult, targetAttributes);
    }

    /**
     * Returns the standard event message attribute names:
     *  _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventMessageAttrNames() {
        return eventMessageAttrNames;
    }

    /**
     * Returns the standard event message attribute names as a list:
     *  _EVENT_MESSAGE_LIST_, etc.
     */
    public static List<String> getEventMessageAttrNamesList() {
        return eventMessageAttrNamesList;
    }

    public static boolean isEventMessageAttrName(String attributeName) {
        return eventMessageAttrNames.contains(attributeName);
    }

    public static Map<String, Object> getEventAttributesAsMap(HttpServletRequest request, Map<String, Object> outMap) {
        return UtilHttp.requestAttributesToMap(request, outMap, eventMessageAttrNamesList);
    }

    public static Map<String, Object> getEventAttributesAsMap(HttpServletRequest request) {
        return getEventAttributesAsMap(request, new HashMap<>());
    }

    public static void setEventAttributesFromMap(HttpServletRequest request, Map<String, ?> map) {
        UtilHttp.setRequestAttributesFromMap(request, map, eventMessageAttrNamesList);
    }

    public static void clearEventAttributes(HttpServletRequest request) {
        UtilHttp.setRequestAttributesFromMap(request, Collections.emptyMap(), eventMessageAttrNamesList);
    }

    public static <V> Map<String, V> copyEventMessages(Map<String, ? extends V> srcMap, Map<String, V> dstMap) {
        return UtilMisc.copyKeys(srcMap, dstMap, eventMessageAttrNamesList);
    }

    /**
     * Returns the standard error message attribute names:
     *  _ERROR_MESSAGE_LIST_, etc.
     */
    public static Set<String> getErrorMessageAttrNames() {
        return errorMessageAttrNames;
    }

    /**
     * Returns the standard error message attribute names as a list:
     *  _ERROR_MESSAGE_LIST_, etc.
     */
    public static List<String> getErrorMessageAttrNamesList() {
        return errorMessageAttrNamesList;
    }

    public static boolean isErrorMessageAttrName(String attributeName) {
        return errorMessageAttrNames.contains(attributeName);
    }

    public static Map<String, Object> getErrorAttributesAsMap(HttpServletRequest request, Map<String, Object> outMap) {
        return UtilHttp.requestAttributesToMap(request, outMap, errorMessageAttrNamesList);
    }

    public static Map<String, Object> getErrorAttributesAsMap(HttpServletRequest request) {
        return getErrorAttributesAsMap(request, new HashMap<>());
    }

    public static void setErrorAttributesFromMap(HttpServletRequest request, Map<String, ?> map) {
        UtilHttp.setRequestAttributesFromMap(request, map, errorMessageAttrNamesList);
    }

    public static void clearErrorAttributes(HttpServletRequest request) {
        UtilHttp.setRequestAttributesFromMap(request, Collections.emptyMap(), errorMessageAttrNamesList);
    }

    public static <V> Map<String, V> copyErrorMessages(Map<String, ? extends V> srcMap, Map<String, V> dstMap) {
        return UtilMisc.copyKeys(srcMap, dstMap, errorMessageAttrNamesList);
    }

    public static boolean hasError(HttpServletRequest request) {
        return hasErrorMessage(request);
    }

    public static boolean hasEventMessage(HttpServletRequest request) {
        return (request.getAttribute(EVENT_MESSAGE) != null) ||
                UtilValidate.isNotEmpty((List<?>) request.getAttribute(EVENT_MESSAGE_LIST));
    }

    public static boolean hasErrorMessage(HttpServletRequest request) {
        return (request.getAttribute(ERROR_MESSAGE) != null) ||
                UtilValidate.isNotEmpty((List<?>) request.getAttribute(ERROR_MESSAGE_LIST)) ||
                UtilValidate.isNotEmpty((Map<?, ?>) request.getAttribute(ERROR_MESSAGE_MAP));
    }

    public static boolean hasAnyMessage(HttpServletRequest request) {
        return hasEventMessage(request) || hasErrorMessage(request);
    }

    public static List<Object> getEventMessageList(HttpServletRequest request) {
        List<Object> eventMessageList = UtilGenerics.cast(request.getAttribute(EVENT_MESSAGE_LIST));
        if (eventMessageList == null) {
            eventMessageList = new ArrayList<>();
            request.setAttribute(EVENT_MESSAGE_LIST, eventMessageList);
        }
        return eventMessageList;
    }

    /**
     * Adds a message to the request event message list attribute.
     */
    public static List<Object> addEventMessage(HttpServletRequest request, Object eventMessage) {
        List<Object> eventMessageList = getEventMessageList(request);
        eventMessageList.add(eventMessage);
        return eventMessageList;
    }

    public static List<Object> getErrorMessageList(HttpServletRequest request) {
        List<Object> errorMessageList = UtilGenerics.cast(request.getAttribute(ERROR_MESSAGE_LIST));
        if (errorMessageList == null) {
            errorMessageList = new ArrayList<>();
            request.setAttribute(ERROR_MESSAGE_LIST, errorMessageList);
        }
        return errorMessageList;
    }

    /**
     * Adds a message to the request error message list attribute.
     */
    public static List<Object> addErrorMessage(HttpServletRequest request, Object errorMessage) {
        List<Object> errorMessageList = getErrorMessageList(request);
        errorMessageList.add(errorMessage);
        return errorMessageList;
    }

    /**
     * Set request attributes and messages from service result, without default success message, overriding existing messages.
     */
    public static void setAttributesFromService(HttpServletRequest request, Map<String, ?> result) {
        setMessagesFromService(request, result);
        setAttributesFromServiceResults(request, result);
    }

    /**
     * Appends request attributes and messages from service result, without default success message.
     */
    public static void appendAttributesFromService(HttpServletRequest request, Map<String, ?> result) {
        appendMessagesFromService(request, result);
        setAttributesFromServiceResults(request, result);
    }

    /**
     * Alternative to {@link ServiceUtil#getMessages(HttpServletRequest, Map, String)} that preserves lists when setting in request.
     * All existing messages are completely replaced, efficiently.
     */
    public static void setMessagesFromService(HttpServletRequest request, Map<String, ? extends Object> serviceResult) {
        ServiceUtil.setRequestMessages(request, serviceResult);
    }

    /**
     * Alternative to {@link ServiceUtil#getMessages(HttpServletRequest, Map, String)} that preserves lists when setting in request.
     * The lists are appended to existing, but single message is replaced.
     */
    public static void appendMessagesFromService(HttpServletRequest request, Map<String, ? extends Object> serviceResult) {
        ServiceUtil.appendRequestMessages(request, serviceResult);
    }

    /**
     * Clears all the event message request attributes.
     */
    public static void clearMessages(HttpServletRequest request) {
        ServiceUtil.clearRequestMessages(request);
    }

    /**
     * Sets default success message for service events if no other messages in service.
     * NOTE: This must be called after setRequestMessagesFromService.
     */
    public static void setDefaultSuccessMessageForService(HttpServletRequest request, Locale locale) {
        if (!EventUtil.hasEventMessage(request) && !EventUtil.hasErrorMessage(request)) {
            String defSuccessMsg = UtilProperties.getMessage("CommonUiLabels", "CommonServiceSuccessMessage", locale);
            request.setAttribute("_EVENT_MESSAGE_", defSuccessMsg);
            request.setAttribute("_DEF_EVENT_MSG_", defSuccessMsg); // See RequestHandler for usage (short lifespan)
        }
    }

    public static void setDefaultSuccessMessageForServiceAsync(HttpServletRequest request, Locale locale, boolean persist, String propertyRef) {
        if (!EventUtil.hasEventMessage(request) && !EventUtil.hasErrorMessage(request)) {
            Map<String, Object> msgCtx = UtilHttp.getAttributeMap(request);
            String defSuccessMsg;
            if (UtilValidate.isNotEmpty(propertyRef)) {
                String[] parts = propertyRef.split("#", 2);
                if (parts.length >= 2) {
                    defSuccessMsg = UtilProperties.getMessage(parts[0], parts[1], msgCtx, locale);
                } else if (parts.length >= 1) {
                    defSuccessMsg = UtilProperties.getMessage("CommonUiLabels", parts[0], msgCtx, locale);
                } else {
                    defSuccessMsg = UtilProperties.getMessage("CommonUiLabels", persist ? "CommonServiceSuccessMessageAsyncPersist" : "CommonServiceSuccessMessageAsyncOnetime", msgCtx, locale);
                }
            } else {
                defSuccessMsg = UtilProperties.getMessage("CommonUiLabels", persist ? "CommonServiceSuccessMessageAsyncPersist" : "CommonServiceSuccessMessageAsyncOnetime", msgCtx, locale);
            }
            request.setAttribute("_EVENT_MESSAGE_", defSuccessMsg);
            request.setAttribute("_DEF_EVENT_MSG_", defSuccessMsg); // See RequestHandler for usage (short lifespan)
        }
    }

    public static void setAttributesFromServiceResults(HttpServletRequest request, Map<String, ?> result) {
        for (Map.Entry<String, ?> rme: result.entrySet()) {
            String resultKey = rme.getKey();
            Object resultValue = rme.getValue();
            // SCIPIO: This is ridiculous
            //if (resultKey != null && !ModelService.RESPONSE_MESSAGE.equals(resultKey) && !ModelService.ERROR_MESSAGE.equals(resultKey) &&
            //        !ModelService.ERROR_MESSAGE_LIST.equals(resultKey) && !ModelService.ERROR_MESSAGE_MAP.equals(resultKey) &&
            //        !ModelService.SUCCESS_MESSAGE.equals(resultKey) && !ModelService.SUCCESS_MESSAGE_LIST.equals(resultKey)) {
            if (resultKey != null && !ModelService.SYS_RESPONSE_FIELDS_SET.contains(resultKey)) {
                request.setAttribute(resultKey, resultValue);
            }
        }
    }

    public static class ServiceParamOptions implements Serializable {
        public static final ServiceParamOptions VALIDATE = new ServiceParamOptions(true, true, true, true) {
            // FIXME: ugly read-only pattern
            @Override public ServiceParamOptions setValidate(boolean validate) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setIncludeSysParams(boolean includeSysParams) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseMultiPartMap(boolean useMultiPartMap) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseSession(boolean useSession) { throw new UnsupportedOperationException(); }
        };
        public static final ServiceParamOptions NO_VALIDATE = new ServiceParamOptions(false, false, true, true) {
            @Override public ServiceParamOptions setValidate(boolean validate) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setIncludeSysParams(boolean includeSysParams) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseMultiPartMap(boolean useMultiPartMap) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseSession(boolean useSession) { throw new UnsupportedOperationException(); }
        };
        public static final ServiceParamOptions MINIMAL = new ServiceParamOptions(false, false, false, false) {
            @Override public ServiceParamOptions setValidate(boolean validate) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setIncludeSysParams(boolean includeSysParams) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseMultiPartMap(boolean useMultiPartMap) { throw new UnsupportedOperationException(); }
            @Override public ServiceParamOptions setUseSession(boolean useSession) { throw new UnsupportedOperationException(); }
        };

        private boolean validate;
        private boolean includeSysParams;
        private boolean useMultiPartMap;
        private boolean useSession; // FIXME: NOT IMPLEMENTED

        private ServiceParamOptions(boolean validate, boolean includeSysParams, boolean useMultiPartMap, boolean useSession) {
            this.validate = validate;
            this.includeSysParams = includeSysParams;
            this.useMultiPartMap = useMultiPartMap;
            this.useSession = useSession;
        }

        private ServiceParamOptions(ServiceParamOptions other) { this(other.validate, other.includeSysParams, other.useMultiPartMap, other.useSession); }

        public static ServiceParamOptions validating() { return new ServiceParamOptions(VALIDATE); }
        public static ServiceParamOptions nonValidating() { return new ServiceParamOptions(NO_VALIDATE); }
        public static ServiceParamOptions minimal() { return new ServiceParamOptions(MINIMAL); }

        public boolean isValidate() { return validate; }
        public ServiceParamOptions setValidate(boolean validate) { this.validate = validate; return this; }
        public boolean isIncludeSysParams() { return includeSysParams; }
        public ServiceParamOptions setIncludeSysParams(boolean includeSysParams) { this.includeSysParams = includeSysParams; return this; }
        public boolean isUseMultiPartMap() { return useMultiPartMap; }
        public ServiceParamOptions setUseMultiPartMap(boolean useMultiPartMap) { this.useMultiPartMap = useMultiPartMap; return this; }
        public boolean isUseSession() { return useSession; }
        public ServiceParamOptions setUseSession(boolean useSession) { this.useSession = useSession; return this; }
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * By default, does NOT perform context validation (makeValid) or add userLogin/locale/timeZone to the context (done by {@link #runServiceAsEvent}).
     * FIXME: useMultiPartMap not currently implemented
     * FIXME: stringListSuffix does not work correctly due to UtilHttp (ofbiz bug).
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, ModelService model, Map<String, ?> extraContext, ServiceParamOptions options, List<? super String> errorMessages) {
        if (options == null) {
            options = ServiceParamOptions.NO_VALIDATE;
        }
        if (extraContext == null) {
            extraContext = Collections.emptyMap();
        }
        Map<String, Object> multiPartMap = options.isUseMultiPartMap() ? Collections.emptyMap() : Collections.emptyMap(); // TODO
        Map<String, Object> rawParametersMap = UtilHttp.getCombinedMap(request);
        //Set<String> urlOnlyParameterNames = UtilHttp.getUrlOnlyParameterMap(request).keySet();
        Map<String, Object> combinedMap = null;

        Locale locale = (Locale) extraContext.get("locale");
        if (locale == null) {
            locale = UtilHttp.getLocale(request);
        }
        TimeZone timeZone = (TimeZone) extraContext.get("timeZone");
        if (timeZone == null && !extraContext.containsKey("timeZone")) {
            timeZone = UtilHttp.getTimeZone(request);
        }

        // we have a service and the model; build the context
        Map<String, Object> serviceContext = new HashMap<String, Object>();
        for (ModelParam modelParam: model.getInModelParamList()) {
            String name = modelParam.name;

            // don't include userLogin, that's taken care of below
            if ("userLogin".equals(name)) continue;
            // don't include locale, that is also taken care of below
            if ("locale".equals(name)) continue;
            // don't include timeZone, that is also taken care of below
            if ("timeZone".equals(name)) continue;

            Object value = null;
            if (UtilValidate.isNotEmpty(modelParam.stringMapPrefix)) {
                if (combinedMap == null) {
                    combinedMap = UtilHttp.getCombinedMap(request);
                    combinedMap.putAll(extraContext);
                }
                Map<String, Object> paramMap = UtilHttp.makeParamMapWithPrefix(combinedMap, multiPartMap, modelParam.stringMapPrefix, null);
                value = paramMap;
            } else if (UtilValidate.isNotEmpty(modelParam.stringListSuffix)) {
                if (combinedMap == null) {
                    combinedMap = UtilHttp.getCombinedMap(request);
                    combinedMap.putAll(extraContext);
                }
                // FIXME: missing UtilHttp overload, inconsistent
                //List<Object> paramList = UtilHttp.makeParamListWithSuffix(combinedMap, multiPartMap, modelParam.stringListSuffix, null);
                List<Object> paramList = UtilHttp.makeParamListWithSuffix(request, multiPartMap, modelParam.stringListSuffix, null);
                value = paramList;
            } else if (extraContext.containsKey(name)) {
                value = extraContext.get(name);
            } else {
                // first check the multi-part map
                value = multiPartMap.get(name);

                // next check attributes; do this before parameters so that attribute which can be changed by code can override parameters which can't
                if (UtilValidate.isEmpty(value)) {
                    Object tempVal = request.getAttribute(UtilValidate.isEmpty(modelParam.requestAttributeName) ? name : modelParam.requestAttributeName);
                    if (tempVal != null) {
                        value = tempVal;
                    }
                }

                // check the request parameters
                if (UtilValidate.isEmpty(value)) {
                    // TODO? not possible here for now...
                    //ServiceEventHandler.checkSecureParameter(requestMap, urlOnlyParameterNames, name, session, serviceName, dctx.getDelegator());

                    // if the service modelParam has allow-html="any" then get this direct from the request instead of in the parameters Map so there will be no canonicalization possibly messing things up
                    if ("any".equals(modelParam.allowHtml)) {
                        value = request.getParameter(name);
                    } else {
                        // use the rawParametersMap from UtilHttp in order to also get pathInfo parameters, do canonicalization, etc
                        value = rawParametersMap.get(name);
                    }

                    // make any composite parameter data (e.g., from a set of parameters {name_c_date, name_c_hour, name_c_minutes})
                    if (value == null) {
                        value = UtilHttp.makeParamValueFromComposite(request, name, locale);
                    }
                }

                // then session
                if (UtilValidate.isEmpty(value)) {
                    Object tempVal = request.getSession().getAttribute(UtilValidate.isEmpty(modelParam.sessionAttributeName) ? name : modelParam.sessionAttributeName);
                    if (tempVal != null) {
                        value = tempVal;
                    }
                }

                // no field found
                if (value == null) {
                    //still null, give up for this one
                    continue;
                }

                if (value instanceof String && ((String) value).length() == 0) {
                    // interpreting empty fields as null values for each in back end handling...
                    value = null;
                }
            }
            // set even if null so that values will get nulled in the db later on
            serviceContext.put(name, value);
        }

        if (options.isValidate()) {
            serviceContext = model.makeValid(serviceContext, ModelService.IN_PARAM, true, UtilGenerics.checkList(errorMessages), timeZone, locale);
        }

        if (options.isIncludeSysParams()) {
            // include the UserLogin value object
            GenericValue userLogin = extraContext.containsKey("userLogin") ? (GenericValue) extraContext.get("userLogin") : UtilHttp.getSessionAttr(request, "userLogin");
            if (userLogin != null) {
                serviceContext.put("userLogin", userLogin);
            }

            // include the Locale object
            if (locale != null) {
                serviceContext.put("locale", locale);
            }

            // include the TimeZone object
            if (timeZone != null) {
                serviceContext.put("timeZone", timeZone);
            }
        }

        return serviceContext;
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * By default, does NOT perform context validation (makeValid) or add userLogin/locale/timeZone to the context (done by {@link #runServiceAsEvent}).
     * FIXME: useMultiPartMap not currently implemented
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, String serviceName, Map<String, ?> extraContext, ServiceParamOptions options, List<? super String> errorMessages) {
        ModelService model;
        try {
            model = ((LocalDispatcher) request.getAttribute("dispatcher")).getModelService(serviceName);
        } catch (GenericServiceException e) {
            throw new IllegalArgumentException(e);
        }
        return getServiceEventParamMap(request, model, extraContext, options, errorMessages);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * By default, does NOT perform context validation (makeValid) or add userLogin/locale/timeZone to the context (done by {@link #runServiceAsEvent}).
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, String serviceName, ServiceParamOptions options) {
        return getServiceEventParamMap(request, serviceName, null, options, null);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * Does NOT perform context validation (makeValid) or add userLogin/locale/timeZone to the context (done by {@link #runServiceAsEvent}).
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, String serviceName) {
        return getServiceEventParamMap(request, serviceName, null, ServiceParamOptions.NO_VALIDATE, null);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority), also performs validation.
     * This overload performs context validation (makeValid) and adds userLogin/locale/timeZone to the context.
     */
    public static Map<String, Object> getValidServiceEventParamMap(HttpServletRequest request, String serviceName, Map<String, ?> extraContext, List<? super String> errorMessages) {
        return getServiceEventParamMap(request, serviceName, extraContext, ServiceParamOptions.VALIDATE, errorMessages);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority), also performs validation.
     * This overload performs context validation (makeValid) and adds userLogin/locale/timeZone to the context.
     */
    public static Map<String, Object> getValidServiceEventParamMap(HttpServletRequest request, String serviceName, List<? super String> errorMessages) {
        return getServiceEventParamMap(request, serviceName, null, ServiceParamOptions.VALIDATE, errorMessages);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority), also performs validation.
     * This overload performs context validation (makeValid) and adds userLogin/locale/timeZone to the context.
     */
    public static Map<String, Object> getValidServiceEventParamMap(HttpServletRequest request, String serviceName) {
        return getServiceEventParamMap(request, serviceName, null, ServiceParamOptions.VALIDATE, null);
    }

    /**
     * May be used in a pure event to emulate the service event handler return (abstracted method);
     * sets request messages and returns the event result code.
     * NOTE: This is meant to be a lightweight version of what ServiceEventHandler returns, and so
     * other fixes may be added to this (subject to change).
     * Added 2019-02-05.
     * @return the event error response
     */
    public static String returnServiceAsEvent(HttpServletRequest request, Map<String, ? extends Object> serviceResult) {
        return returnServiceAsEvent(request, serviceResult, true, true, true);
    }

    // FIXME: currently private because there are too many possible options - this needs a helper class or a helper invoker class, because too many booleans, so caller events will become unreadable...
    //  so for now we will simply emulate the full service event handler (all true by default) since that should be the most common usage
    private static String returnServiceAsEvent(HttpServletRequest request, Map<String, ? extends Object> serviceResult, boolean messagesToRequest, boolean useDefaultSuccessMessage, boolean resultToRequest) {
        if (messagesToRequest) {
            appendMessagesFromService(request, serviceResult);
        }
        if (useDefaultSuccessMessage) {
            if (ServiceUtil.isSuccess(serviceResult)) {
                EventUtil.setDefaultSuccessMessageForService(request, UtilHttp.getLocale(request));
            }
        }
        if (resultToRequest) {
            setAttributesFromServiceResults(request, serviceResult);
        }
        // NOTE: Usually this is success/error/fail, so we don't have to convert it for now...
        String responseMessage =  (String) serviceResult.get(ModelService.RESPONSE_MESSAGE);
        return UtilValidate.isNotEmpty(responseMessage) ? responseMessage : "success";
    }

    private static String runServiceAsEvent(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext, Boolean async, boolean validate, boolean messagesToRequest, boolean useDefaultSuccessMessage, boolean resultToRequest) throws GenericServiceException {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        try {
            ModelService model = dispatcher.getModelService(serviceName);

            GenericValue userLogin = (GenericValue) serviceContext.get("userLogin");
            if (!UtilMisc.containsKey(serviceContext, "userLogin", userLogin)) {
                userLogin = UtilHttp.getSessionAttr(request, "userLogin");
            }
            Locale locale = (Locale) serviceContext.get("locale");
            if (!UtilMisc.containsKey(serviceContext, "locale", locale)) {
                locale = UtilHttp.getLocale(request);
            }
            TimeZone timeZone = (TimeZone) serviceContext.get("timeZone");
            if (!UtilMisc.containsKey(serviceContext, "timeZone", timeZone)) {
                timeZone = UtilHttp.getTimeZone(request);
            }

            if (validate) {
                List<Object> errorMessages = new ArrayList<>();
                serviceContext = model.makeValid(serviceContext, ModelService.IN_PARAM, true, errorMessages, timeZone, locale);
                if (errorMessages.size() > 0) {
                    request.setAttribute("_ERROR_MESSAGE_LIST_", errorMessages);
                    return "error";
                }
            }

            if (userLogin != null) {
                serviceContext.put("userLogin", UtilGenerics.cast(userLogin));
            }
            if (locale != null) {
                serviceContext.put("locale", UtilGenerics.cast(locale));
            }
            if (timeZone != null) {
                serviceContext.put("timeZone", UtilGenerics.cast(timeZone));
            }

            if (async) {
                dispatcher.runAsync(serviceName, serviceContext, false);
                return "success";
            } else {
                Map<String, Object> servResult = dispatcher.runSync(serviceName, serviceContext);
                return returnServiceAsEvent(request, servResult, messagesToRequest, useDefaultSuccessMessage, resultToRequest);
            }
        } catch (ServiceAuthException e) {
            // not logging since the service engine already did
            request.setAttribute("_ERROR_MESSAGE_", e.getNonNestedMessage());
            return "error";
        } catch (ServiceValidationException e) {
            // not logging since the service engine already did
            request.setAttribute("serviceValidationException", e);
            if (e.getMessageList() != null) {
                request.setAttribute("_ERROR_MESSAGE_LIST_", e.getMessageList());
            } else {
                request.setAttribute("_ERROR_MESSAGE_", e.getNonNestedMessage());
            }
            return "error";
        } catch (GenericServiceException e) {
            Debug.logError(e, "Service invocation error", module);
            throw e;
        }
    }

    /**
     * Implements an approximation of the ServiceEventHandler to call the given service, and simply
     * call the given service with the given context after making it valid, finally returning an appropriate event response.
     * (similar but not equivalent to groovy and ftl's runService function).
     * <p>
     * This is geared toward small events that are basically just service wrappers.
     * <p>
     * Automatically sets the userLogin, locale and timeZone IF their keys are not present in the passed map.
     */
    public static String runServiceAsEvent(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext) throws GenericServiceException {
        return runServiceAsEvent(request, response, serviceName, serviceContext, false, true, true, true ,true);
    }

    /**
     * Implements an approximation of the ServiceEventHandler to call the given service, and simply
     * call the given service with the given context after making it valid, finally returning an appropriate event response.
     * (similar but not equivalent to groovy and ftl's runService function).
     * <p>
     * This is geared toward small events that are basically just service wrappers.
     * <p>
     * Automatically sets the userLogin, locale and timeZone IF their keys are not present in the passed map.
     */
    public static String runServiceAsEvent(HttpServletRequest request, HttpServletResponse response, String serviceName,
                                           Map<String, ?> serviceContext, Boolean validate) throws GenericServiceException {
        return runServiceAsEvent(request, response, serviceName, serviceContext, false, !Boolean.TRUE.equals(validate), true, true ,true);
    }

    /**
     * Implements an approximation of the ServiceEventHandler to call the given service, and simply
     * call the given service with the given context after making it valid, finally returning an appropriate event response.
     * (similar but not equivalent to groovy and ftl's runService function).
     * <p>
     * This is geared toward small events that are basically just service wrappers.
     * <p>
     * Automatically sets the userLogin, locale and timeZone IF their keys are not present in the passed map.
     */
    public static String runAsyncServiceAsEvent(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext) throws GenericServiceException {
        return runServiceAsEvent(request, response, serviceName, serviceContext, true, true, true, true ,true);
    }

    /**
     * Implements an approximation of the ServiceEventHandler to call the given service, and simply
     * call the given service with the given context after making it valid, finally returning an appropriate event response.
     * (similar but not equivalent to groovy and ftl's runService function).
     * <p>
     * This version skips the makeValid call; can be used if the context was already passed through makeValid.
     * <p>
     * This is geared toward small events that are basically just service wrappers.
     * <p>
     * Automatically sets the userLogin, locale and timeZone IF their keys are not present in the passed map.
     */
    public static String runServiceAsEventNoValid(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext) throws GenericServiceException {
        return runServiceAsEvent(request, response, serviceName, serviceContext, false, false, true, true, true);
    }

    /**
     * Implements an approximation of the ServiceEventHandler to call the given service, and simply
     * call the given service with the given context after making it valid, finally returning an appropriate event response.
     * (similar but not equivalent to groovy and ftl's runService function).
     * <p>
     * This version skips the makeValid call; can be used if the context was already passed through makeValid.
     * <p>
     * This is geared toward small events that are basically just service wrappers.
     * <p>
     * Automatically sets the userLogin, locale and timeZone IF their keys are not present in the passed map.
     */
    public static String runAsyncServiceAsEventNoValid(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext) throws GenericServiceException {
        return runServiceAsEvent(request, response, serviceName, serviceContext, true, false, true, true, true);
    }

    /**
     * Emulates ServiceMultiEventHandler, to a limited extent.
     * NOTE: You can use this for non-service but you must at least define a service interface, this helps to define the required parameters anyway.
     * WARN: Callers must enforce POST themselves if any GET params considered a security issue.
     * <p>
     * TODO: de-duplicate logic with ServiceMultiEventHandler
     * TODO: implement useMultiPartMap
     * @return a LinkedHashMap mapping parameter row submit indexes to their service contexts
     */
    public static Map<Integer, Map<String, Object>> getMultiSubmitServiceContexts(HttpServletRequest request, String serviceName, ServiceParamOptions options) throws GenericServiceException {
        return getMultiSubmitServiceContexts(request, serviceName, options, null, null, null);
    }

    private static Map<Integer, Map<String, Object>> getMultiSubmitServiceContexts(HttpServletRequest request, String serviceName, ServiceParamOptions options,
                                                                                  Integer rowCount, Boolean useRowSubmit, Boolean checkGlobalScope) throws GenericServiceException {
        if (options == null) {
            options = ServiceParamOptions.NO_VALIDATE;
        }
        Map<Integer, Map<String, Object>> contexts = new LinkedHashMap<>();

        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        if (dispatcher == null) {
            throw new IllegalStateException("The local service dispatcher is null");
        }
        DispatchContext dctx = dispatcher.getDispatchContext();
        if (dctx == null) {
            throw new IllegalStateException("Dispatch context cannot be found");
        }
        ModelService modelService = dctx.getModelService(serviceName);

        Locale locale = UtilHttp.getLocale(request);
        TimeZone timeZone = UtilHttp.getTimeZone(request);
        HttpSession session = options.isUseSession() ? request.getSession() : null;
        GenericValue userLogin = UtilHttp.getSessionUserLogin(request);

        if (useRowSubmit == null) {
            useRowSubmit = request.getParameter("_useRowSubmit") == null ? false :
                    "Y".equalsIgnoreCase(request.getParameter("_useRowSubmit"));
        }
        if (checkGlobalScope == null) {
            checkGlobalScope = request.getParameter("_checkGlobalScope") == null ? true :
                    !"N".equalsIgnoreCase(request.getParameter("_checkGlobalScope"));
        }
        if (rowCount == null) {
            rowCount = UtilHttp.getMultiFormRowCount(request);
            if (rowCount < 1) {
                return contexts;
            }
        }

        Set<String> urlOnlyParameterNames = UtilHttp.getUrlOnlyParameterMap(request).keySet();

        // now loop throw the rows and prepare/invoke the service for each
        for (int i = 0; i < rowCount; i++) {
            String curSuffix = UtilHttp.getMultiRowDelimiter() + i;
            boolean rowSelected = false;
            if (UtilValidate.isNotEmpty(request.getAttribute(UtilHttp.getRowSubmitPrefix() + i))) {
                rowSelected = request.getAttribute(UtilHttp.getRowSubmitPrefix() + i) == null ? false :
                        "Y".equalsIgnoreCase((String) request.getAttribute(UtilHttp.getRowSubmitPrefix() + i));
            } else {
                rowSelected = request.getParameter(UtilHttp.getRowSubmitPrefix() + i) == null ? false :
                        "Y".equalsIgnoreCase(request.getParameter(UtilHttp.getRowSubmitPrefix() + i));
            }

            // make sure we are to process this row
            if (useRowSubmit && !rowSelected) {
                continue;
            }

            // build the context
            Map<String, Object> serviceContext = new HashMap<String, Object>();
            for (ModelParam modelParam : modelService.getInModelParamList()) {
                String paramName = modelParam.name;

                // Debug.logInfo("In ServiceMultiEventHandler processing input parameter [" + modelParam.name + (modelParam.optional?"(optional):":"(required):") + modelParam.mode + "] for service [" + serviceName + "]", module);

                // don't include userLogin, that's taken care of below
                if ("userLogin".equals(paramName)) continue;
                // don't include locale, that is also taken care of below
                if ("locale".equals(paramName)) continue;
                // don't include timeZone, that is also taken care of below
                if ("timeZone".equals(paramName)) continue;

                Object value = null;
                if (UtilValidate.isNotEmpty(modelParam.stringMapPrefix)) {
                    Map<String, Object> paramMap = UtilHttp.makeParamMapWithPrefix(request, modelParam.stringMapPrefix, curSuffix);
                    value = paramMap;
                } else if (UtilValidate.isNotEmpty(modelParam.stringListSuffix)) {
                    List<Object> paramList = UtilHttp.makeParamListWithSuffix(request, modelParam.stringListSuffix, null);
                    value = paramList;
                } else {
                    // check attributes; do this before parameters so that attribute which can be changed by code can override parameters which can't
                    value = request.getAttribute(paramName + curSuffix);

                    // first check for request parameters
                    if (value == null) {
                        String name = paramName + curSuffix;

                        // TODO: REVIEW: This check is not possible here, it rarely did any good anyway, caller can check POST
                        //ServiceEventHandler.checkSecureParameter(requestMap, urlOnlyParameterNames, name, session, serviceName, dctx.getDelegator());

                        String[] paramArr = request.getParameterValues(name);
                        if (paramArr != null) {
                            if (paramArr.length > 1) {
                                value = Arrays.asList(paramArr);
                            } else {
                                value = paramArr[0];
                            }
                        }
                    }

                    // if the parameter wasn't passed and no other value found, check the session
                    if (options.isUseSession() && value == null) {
                        value = session.getAttribute(paramName + curSuffix);
                    }

                    // now check global scope
                    if (value == null) {
                        if (checkGlobalScope) {
                            String[] gParamArr = request.getParameterValues(paramName);
                            if (gParamArr != null) {
                                if (gParamArr.length > 1) {
                                    value = Arrays.asList(gParamArr);
                                } else {
                                    value = gParamArr[0];
                                }
                            }
                            if (value == null) {
                                value = request.getAttribute(paramName);
                            }
                            if (options.isUseSession() && value == null) {
                                value = session.getAttribute(paramName);
                            }
                        }
                    }

                    // make any composite parameter data (e.g., from a set of parameters {name_c_date, name_c_hour, name_c_minutes})
                    if (value == null) {
                        value = UtilHttp.makeParamValueFromComposite(request, paramName + curSuffix, locale);
                    }

                    if (value == null) {
                        // still null, give up for this one
                        continue;
                    }

                    if (value instanceof String && ((String) value).length() == 0) {
                        // interpreting empty fields as null values for each in back end handling...
                        value = null;
                    }
                }
                // set even if null so that values will get nulled in the db later on
                serviceContext.put(paramName, value);

                // Debug.logInfo("In ServiceMultiEventHandler got value [" + value + "] for input parameter [" + paramName + "] for service [" + serviceName + "]", module);
            }

            // get only the parameters for this service - converted to proper type
            if (options.isValidate()) {
                serviceContext = modelService.makeValid(serviceContext, ModelService.IN_PARAM, true, null, timeZone, locale);
            }

            if (options.isIncludeSysParams()) {
                // include the UserLogin value object
                if (userLogin != null) {
                    serviceContext.put("userLogin", userLogin);
                }

                // include the Locale object
                if (locale != null) {
                    serviceContext.put("locale", locale);
                }

                // include the TimeZone object
                if (timeZone != null) {
                    serviceContext.put("timeZone", timeZone);
                }
            }

            contexts.put(i, serviceContext);
        }
        return contexts;
    }
}