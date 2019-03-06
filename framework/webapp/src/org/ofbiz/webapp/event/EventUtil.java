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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

/**
 * SCIPIO: (New) Event utilities and definitions.
 */
public final class EventUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String ERROR_MESSAGE_LIST = "_ERROR_MESSAGE_LIST_";
    public static final String ERROR_MESSAGE_MAP = "_ERROR_MESSAGE_MAP_";
    public static final String ERROR_MESSAGE = "_ERROR_MESSAGE_";
    public static final String EVENT_MESSAGE_LIST = "_EVENT_MESSAGE_LIST_";
    public static final String EVENT_MESSAGE = "_EVENT_MESSAGE_";

    private static final Set<String> eventMsgAttrNames = UtilMisc.unmodifiableHashSet(
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);

    private static final Set<String> errorMsgAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE);

    private static final Set<String> eventErrorMsgAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE,
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);

    private EventUtil() {
    }

    /**
     * Returns the standard event regular (success) AND error message attribute names:
     *  _ERROR_MESSAGE_LIST_, _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventErrorMsgAttrNames() {
        return eventErrorMsgAttrNames;
    }

    public static boolean isEventErrorMsgAttrName(String attributeName) {
        return eventErrorMsgAttrNames.contains(attributeName);
    }

    /**
     * Returns the standard event message attribute names:
     *  _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventMsgAttrNames() {
        return eventMsgAttrNames;
    }

    public static boolean isEventMsgAttrName(String attributeName) {
        return eventMsgAttrNames.contains(attributeName);
    }

    /**
     * Returns the standard error message attribute names:
     *  _ERROR_MESSAGE_LIST_, etc.
     */
    public static Set<String> getErrorMsgAttrNames() {
        return errorMsgAttrNames;
    }

    public static boolean isErrorMsgAttrName(String attributeName) {
        return errorMsgAttrNames.contains(attributeName);
    }

    public static boolean hasError(HttpServletRequest request) {
        return hasErrorMsg(request);
    }

    public static boolean hasEventMsg(HttpServletRequest request) {
        return (request.getAttribute(EVENT_MESSAGE) != null) ||
                UtilValidate.isNotEmpty((List<?>) request.getAttribute(EVENT_MESSAGE_LIST));
    }

    public static boolean hasErrorMsg(HttpServletRequest request) {
        return (request.getAttribute(ERROR_MESSAGE) != null) ||
                UtilValidate.isNotEmpty((List<?>) request.getAttribute(ERROR_MESSAGE_LIST)) ||
                UtilValidate.isNotEmpty((Map<?, ?>) request.getAttribute(ERROR_MESSAGE_MAP));
    }

    public static boolean hasAnyMsg(HttpServletRequest request) {
        return hasEventMsg(request) || hasErrorMsg(request);
    }

    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, Map<String, Object> targetAttributes) {
        targetAttributes.put(ERROR_MESSAGE_LIST, serviceResult.get(ModelService.ERROR_MESSAGE_LIST));
        targetAttributes.put(ERROR_MESSAGE_MAP, serviceResult.get(ModelService.ERROR_MESSAGE_MAP));
        targetAttributes.put(ERROR_MESSAGE, serviceResult.get(ModelService.ERROR_MESSAGE));

        targetAttributes.put(EVENT_MESSAGE_LIST, serviceResult.get(ModelService.SUCCESS_MESSAGE_LIST));
        targetAttributes.put(EVENT_MESSAGE, serviceResult.get(ModelService.SUCCESS_MESSAGE));
    }

    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, HttpServletRequest targetRequest) {
        targetRequest.setAttribute(ERROR_MESSAGE_LIST, serviceResult.get(ModelService.ERROR_MESSAGE_LIST));
        targetRequest.setAttribute(ERROR_MESSAGE_MAP, serviceResult.get(ModelService.ERROR_MESSAGE_MAP));
        targetRequest.setAttribute(ERROR_MESSAGE, serviceResult.get(ModelService.ERROR_MESSAGE));

        targetRequest.setAttribute(EVENT_MESSAGE_LIST, serviceResult.get(ModelService.SUCCESS_MESSAGE_LIST));
        targetRequest.setAttribute(EVENT_MESSAGE, serviceResult.get(ModelService.SUCCESS_MESSAGE));
    }

    /**
     * Alternative to {@link ServiceUtil#getMessages(HttpServletRequest, Map, String)} that preserves lists when setting in request.
     * The lists are appended to existing, but single message is replaced.
     */
    public static void appendRequestMessagesFromService(HttpServletRequest request, Map<String, ? extends Object> serviceResult) {
        ServiceUtil.appendRequestMessages(request, serviceResult);
    }

    /**
     * Alternative to {@link ServiceUtil#getMessages(HttpServletRequest, Map, String)} that preserves lists when setting in request.
     * Unlike {@link #appendMessageLists(HttpServletRequest, Map)}, all existing messages are completely replaced, efficiently.
     */
    public static void setRequestMessagesFromService(HttpServletRequest request, Map<String, ? extends Object> serviceResult) {
        ServiceUtil.setRequestMessages(request, serviceResult);
    }

    /**
     * Clears all the event message request attributes.
     */
    public static void clearRequestMessages(HttpServletRequest request) {
        ServiceUtil.clearRequestMessages(request);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * NOTE: 2019-02-05: Support is only partial at this time; will be improved.
     * NOTE: 2019-02-05: This does NOT perform a makeValid check; the runService* methods cover it here...
     * NOTE: 2019-02-05: Currently skips the multiPartMap (TODO?).
     * Added 2019-02-05.
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, ModelService model,
            boolean validate, List<Object> errorMessages, GenericValue userLogin, Locale locale, TimeZone timeZone) {
        Map<String, Object> multiPartMap = Collections.emptyMap(); // TODO?
        Map<String, Object> rawParametersMap = UtilHttp.getCombinedMap(request);
        //Set<String> urlOnlyParameterNames = UtilHttp.getUrlOnlyParameterMap(request).keySet();

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
                Map<String, Object> paramMap = UtilHttp.makeParamMapWithPrefix(request, multiPartMap, modelParam.stringMapPrefix, null);
                value = paramMap;
                if (Debug.verboseOn()) Debug.logVerbose("Set [" + modelParam.name + "]: " + paramMap, module);
            } else if (UtilValidate.isNotEmpty(modelParam.stringListSuffix)) {
                List<Object> paramList = UtilHttp.makeParamListWithSuffix(request, multiPartMap, modelParam.stringListSuffix, null);
                value = paramList;
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

        if (validate) {
            serviceContext = model.makeValid(serviceContext, ModelService.IN_PARAM, true, errorMessages, timeZone, locale);
        }
        return serviceContext;
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * NOTE: 2019-02-05: Support is only partial at this time; will be improved.
     * NOTE: 2019-02-05: This does NOT perform a makeValid check; the runService* methods cover it here...
     * NOTE: 2019-02-05: Currently skips the multiPartMap (TODO?).
     * Added 2019-02-05.
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, String serviceName,
            boolean validate, List<Object> errorMessages, GenericValue userLogin, Locale locale, TimeZone timeZone) {
        ModelService model;
        try {
            model = ((LocalDispatcher) request.getAttribute("dispatcher")).getModelService(serviceName);
        } catch (GenericServiceException e) {
            throw new IllegalArgumentException(e);
        }
        return getServiceEventParamMap(request, model, validate, errorMessages, userLogin, locale, timeZone);
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority).
     * NOTE: 2019-02-05: Support is only partial at this time; will be improved.
     * NOTE: 2019-02-05: This does NOT perform a makeValid check; the runService* methods cover it here...
     * NOTE: 2019-02-05: Currently skips the multiPartMap (TODO?).
     * Added 2019-02-05.
     */
    public static Map<String, Object> getServiceEventParamMap(HttpServletRequest request, String serviceName) {
        return getServiceEventParamMap(request, serviceName, false, null, UtilHttp.getSessionAttr(request, "userLogin"),
                UtilHttp.getLocale(request), UtilHttp.getTimeZone(request));
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority), also performs validation.
     * NOTE: 2019-02-05: Support is only partial at this time; will be improved.
     * NOTE: 2019-02-05: This does NOT perform a makeValid check; the runService* methods cover it here...
     * NOTE: 2019-02-05: Currently skips the multiPartMap (TODO?).
     * Added 2019-02-05.
     */
    public static Map<String, Object> getValidServiceEventParamMap(HttpServletRequest request, String serviceName, List<Object> errorMessages) {
        return getServiceEventParamMap(request, serviceName, true, errorMessages, UtilHttp.getSessionAttr(request, "userLogin"),
                UtilHttp.getLocale(request), UtilHttp.getTimeZone(request));
    }

    /**
     * Emulates ServiceEventHandler's parameter handling by preparing a service context from request attributes,
     * request parameters and session attributes (in that order of priority), also performs validation.
     * NOTE: 2019-02-05: Support is only partial at this time; will be improved.
     * NOTE: 2019-02-05: This does NOT perform a makeValid check; the runService* methods cover it here...
     * NOTE: 2019-02-05: Currently skips the multiPartMap (TODO?).
     * Added 2019-02-05.
     */
    public static Map<String, Object> getValidServiceEventParamMap(HttpServletRequest request, String serviceName) {
        return getValidServiceEventParamMap(request, serviceName, null);
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
        appendRequestMessagesFromService(request, serviceResult);
        // NOTE: Usually this is success/error/fail, so we don't have to convert it for now...
        String responseMessage =  (String) serviceResult.get(ModelService.RESPONSE_MESSAGE);
        return UtilValidate.isNotEmpty(responseMessage) ? responseMessage : "success";
    }

    private static String runServiceAsEvent(HttpServletRequest request, HttpServletResponse response, String serviceName,
            Map<String, ?> serviceContext, boolean async, boolean validate) throws GenericServiceException {
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
                return returnServiceAsEvent(request, servResult);
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
        return runServiceAsEvent(request, response, serviceName, serviceContext, false, true);
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
        return runServiceAsEvent(request, response, serviceName, serviceContext, true, true);
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
        return runServiceAsEvent(request, response, serviceName, serviceContext, false, false);
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
        return runServiceAsEvent(request, response, serviceName, serviceContext, true, false);
    }
}
