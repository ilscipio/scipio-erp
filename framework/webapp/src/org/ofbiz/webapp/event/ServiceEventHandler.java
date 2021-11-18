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

import static org.ofbiz.base.util.UtilGenerics.checkList;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.fileupload.FileUploadException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceValidationException;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.control.ControlActivationEventListener;

/**
 * ServiceEventHandler - Service Event Handler
 */
public class ServiceEventHandler implements EventHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String SYNC = "sync";
    public static final String ASYNC = "async";
    // SCIPIO: new options
    public static final String ASYNC_PERSIST = "async-persist";
    public static final String ASYNC_ONETIME = "async-onetime";
    public static final Set<String> SERVICE_MODES = UtilMisc.unmodifiableLinkedHashSet(SYNC, ASYNC, ASYNC_PERSIST, ASYNC_ONETIME);

    /**
     * @see org.ofbiz.webapp.event.EventHandler#init(javax.servlet.ServletContext)
     */
    public void init(ServletContext context) throws EventHandlerException {
    }

    /**
     * @see org.ofbiz.webapp.event.EventHandler#invoke(ConfigXMLReader.Event, ConfigXMLReader.RequestMap, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    public String invoke(Event event, RequestMap requestMap, HttpServletRequest request, HttpServletResponse response) throws EventHandlerException {
        // make sure we have a valid reference to the Service Engine
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        if (dispatcher == null) {
            throw new EventHandlerException("The local service dispatcher is null");
        }
        DispatchContext dctx = dispatcher.getDispatchContext();
        if (dctx == null) {
            throw new EventHandlerException("Dispatch context cannot be found");
        }

        // get the details for the service(s) to call
        String mode = SYNC;
        String serviceName = null;

        if (UtilValidate.isEmpty(event.path)) {
            mode = SYNC;
        } else {
            mode = event.path;
        }

        // SCIPIO: support mode-parameter
        Map<String, Object> eventProperties = event.getProperties(requestMap, request, response, null);
        String modeParameter = (String) eventProperties.get("mode-parameter");
        if (UtilValidate.isNotEmpty(modeParameter)) {
            Object modeParameterValue = UtilHttp.getRequestAttrParam(request, modeParameter);
            if (SERVICE_MODES.contains(modeParameterValue)) {
                mode = (String) modeParameterValue;
            }
        }

        // make sure we have a defined service to call
        serviceName = event.invoke;
        if (serviceName == null) {
            throw new EventHandlerException("Service name (eventMethod) cannot be null");
        }
        if (Debug.verboseOn()) Debug.logVerbose("[Set mode/service]: " + mode + "/" + serviceName, module);

        // some needed info for when running the service
        Locale locale = UtilHttp.getLocale(request);
        TimeZone timeZone = UtilHttp.getTimeZone(request);
        HttpSession session = request.getSession();
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");

        // get the service model to generate context
        ModelService model = null;

        try {
            model = dctx.getModelService(serviceName);
        } catch (GenericServiceException e) {
            throw new EventHandlerException("Problems getting the service model", e);
        }

        if (model == null) {
            throw new EventHandlerException("Problems getting the service model");
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("[Processing]: SERVICE Event", module);
            Debug.logVerbose("[Using delegator]: " + dispatcher.getDelegator().getDelegatorName(), module);
        }

        // SCIPIO: refactored multiPartMap reading
        Map<String, Object> multiPartMap = getMultiPartMap(request);

        // SCIPIO: application/json request body parameters
        Map<String, Object> requestBodyMap = RequestBodyMapHandlerFactory.getRequestBodyMap(request);

        Map<String, Object> rawParametersMap = UtilHttp.getCombinedMap(request);
        Set<String> urlOnlyParameterNames = UtilHttp.getUrlOnlyParameterMap(request).keySet();

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
            boolean publicParam = modelParam.getEffectiveEventAccess().isPublic(); // SCIPIO: exclude parameters when public
            if (UtilValidate.isNotEmpty(modelParam.stringMapPrefix)) {
                Map<String, Object> paramMap = publicParam ? UtilHttp.makeParamMapWithPrefix(request, multiPartMap, modelParam.stringMapPrefix, null)
                        : UtilHttp.makeParamMapWithPrefix(UtilHttp.getAllAttributeMap(request), multiPartMap, modelParam.stringMapPrefix, null);
                value = paramMap;
                if (Debug.verboseOn()) Debug.logVerbose("Set [" + modelParam.name + "]: " + paramMap, module);
            } else if (UtilValidate.isNotEmpty(modelParam.stringListSuffix)) {
                List<Object> paramList = publicParam ? UtilHttp.makeParamListWithSuffix(request, multiPartMap, modelParam.stringListSuffix, null) :
                        UtilHttp.makeParamListWithSuffix(UtilHttp.getAllAttributeMap(request), multiPartMap, modelParam.stringListSuffix, null);
                value = paramList;
            } else {
                // SCIPIO: NO: Always check parameters after attributes, otherwise other event can't override
                //// first check the multi-part map
                //value = multiPartMap.get(name);

                // next check attributes; do this before parameters so that attribute which can be changed by code can override parameters which can't
                //if (UtilValidate.isEmpty(value)) {
                { //if (value == null) {
                    Object tempVal = request.getAttribute(UtilValidate.isEmpty(modelParam.requestAttributeName) ? name : modelParam.requestAttributeName);
                    if (tempVal != null) {
                        value = tempVal;
                    }
                }

                // SCIPIO: NOTE: These have been changed to null checks due to inconsistency with ServiceMultiEventHandler and logic
                //if (UtilValidate.isEmpty(value)) {
                if (value == null && publicParam) {
                    value = multiPartMap.get(name); // SCIPIO: only get this after attributes
                }

                if (value == null && publicParam) {
                    value = requestBodyMap.get(name); // SCIPIO: application/json request body parameters
                }

                // check the request parameters
                //if (UtilValidate.isEmpty(value)) {
                if (value == null && publicParam) {
                    ServiceEventHandler.checkSecureParameter(requestMap, urlOnlyParameterNames, name, session, serviceName, dctx.getDelegator());

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
                //if (UtilValidate.isEmpty(value)) {
                if (value == null) {
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

        // get only the parameters for this service - converted to proper type
        // TODO: pass in a list for error messages, like could not convert type or not a proper X, return immediately with messages if there are any
        List<Object> errorMessages = new LinkedList<>();
        serviceContext = model.makeValid(serviceContext, ModelService.IN_PARAM, true, errorMessages, timeZone, locale);
        if (errorMessages.size() > 0) {
            // uh-oh, had some problems...
            request.setAttribute("_ERROR_MESSAGE_LIST_", errorMessages);
            return "error";
        }

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

        // invoke the service
        Map<String, Object> result = null;
        try {
            /* SCIPIO: 2018-11-23: Refactored
            if (ASYNC.equalsIgnoreCase(mode)) {
                dispatcher.runAsync(serviceName, serviceContext);
            } else {
                result = dispatcher.runSync(serviceName, serviceContext);
            }
            */
            result = invokeService(dispatcher, model, serviceName, serviceContext, mode,
                    event, requestMap, request, response);
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
            throw new EventHandlerException("Service invocation error", e.getNested());
        }

        String responseString = null;

        if (result == null) {
            responseString = ModelService.RESPOND_SUCCESS;

            // SCIPIO: TODO: REVIEW: Due to compatibility issues, we can only set message here if explicit -onetime or -persist
            //          was requested...
            Object asyncSuccessMsgPropertyRef = eventProperties.get("default-success-property");
            if (!(asyncSuccessMsgPropertyRef instanceof String)) {
                asyncSuccessMsgPropertyRef = null;
            }
            if (ASYNC_ONETIME.equals(mode)) {
                EventUtil.setDefaultSuccessMessageForServiceAsync(request, locale, false, (String) asyncSuccessMsgPropertyRef);
            } else if (ASYNC_PERSIST.equals(mode)) {
                EventUtil.setDefaultSuccessMessageForServiceAsync(request, locale, true, (String) asyncSuccessMsgPropertyRef);
            }
        } else {

            if (!result.containsKey(ModelService.RESPONSE_MESSAGE)) {
                responseString = ModelService.RESPOND_SUCCESS;
            } else {
                responseString = (String) result.get(ModelService.RESPONSE_MESSAGE);
            }

            // set the messages in the request; this will be picked up by messages.ftl and displayed
            EventUtil.setMessagesFromService(request, result); // SCIPIO: Factored out

            // SCIPIO: Some services don't set any result messages, either because they aren't explicitly set in the service logic (minilang, groovy, java...)
            // or because the service is just a direct DB operation
            // 2019-03-06: We now also don't add this if there was already an error message from a prior service.
            // NOTE: See also org.ofbiz.webapp.control.RequestHandler#cleanupEventMessages for the code that handles _DEF_EVENT_MSG_; it unsets
            // _EVENT_MESSAGE_ if an error message exists and _EVENT_MESSAGE_ is still set to _DEF_EVENT_MSG_ by the time the events are done.
            if (ModelService.RESPOND_SUCCESS.equals(responseString)) {
                EventUtil.setDefaultSuccessMessageForService(request, locale);
            }

            // set the results in the request
            EventUtil.setAttributesFromServiceResults(request, result);
        }

        if (Debug.verboseOn()) Debug.logVerbose("[Event Return]: " + responseString, module);
        return responseString;
    }

    /**
     * SCIPIO: Core service invocation, overridable.
     * Refactored from {@link #invoke(Event, RequestMap, HttpServletRequest, HttpServletResponse)}.
     * Added 2018-11-23.
     */
    protected Map<String, Object> invokeService(LocalDispatcher dispatcher, ModelService modelService, String serviceName, Map<String, Object> serviceContext, String mode,
            Event event, RequestMap requestMap, HttpServletRequest request, HttpServletResponse response) throws ServiceAuthException, ServiceValidationException, GenericServiceException {
        Map<String, Object> result = null;
        if (ASYNC.equalsIgnoreCase(mode)) {
            dispatcher.runAsync(serviceName, serviceContext);
        } else if (ASYNC_PERSIST.equalsIgnoreCase(mode)) {
            dispatcher.runAsync(serviceName, serviceContext, true);
        } else if (ASYNC_ONETIME.equalsIgnoreCase(mode)) {
            dispatcher.runAsync(serviceName, serviceContext, false);
        } else {
            result = dispatcher.runSync(serviceName, serviceContext);
        }
        return result;
    }

    public static void checkSecureParameter(RequestMap requestMap, Set<String> urlOnlyParameterNames, String name, HttpSession session, String serviceName, Delegator delegator) throws EventHandlerException {
        // special case for security: if this is a request-map defined as secure in controller.xml then only accept body parameters coming in, ie don't allow the insecure URL parameters
        // NOTE: the RequestHandler will check the HttpSerletRequest security to make sure it is secure if the request-map -> security -> https=true,
        // but we can't just look at the request.isSecure() method here because it is allowed to send secure requests for request-map with https=false
        if (requestMap != null && requestMap.securityHttps) {
            if (urlOnlyParameterNames.contains(name)) {
                // SCIPIO: Too verbose/inappropriate, and do not want people turning off service.http.parameters.require.encrypted just to get rid of this.
//                String errMsg = "Found URL parameter [" + name + "] passed to secure (https) request-map with uri ["
//                    + requestMap.uri + "] with an event that calls service ["
//                    + serviceName + "]; this is not allowed for security reasons! The data should be encrypted by making it part of the request body "
//                    + "(a form field) instead of the request URL."
//                    + " Moreover it would be kind if you could create a Jira sub-task of https://issues.apache.org/jira/browse/OFBIZ-2330 "
//                    + "(check before if a sub-task for this error does not exist)."
//                    + " If you are not sure how to create a Jira issue please have a look before at https://cwiki.apache.org/confluence/display/OFBIZ/OFBiz+Contributors+Best+Practices"
//                    + " Thank you in advance for your help.";
//                Debug.logError("=============== " + errMsg + "; In session " + ControlActivationEventListener.getSessionIdForLog(session) + "; Note that this can be changed using the service.http.parameters.require.encrypted property in the url.properties file", module);
                String errMsg = "Security: Found URL parameter [" + name + "] passed to secure (https) request-map with uri ["
                        + requestMap.uri + "] with an event that calls service ["
                        + serviceName + "]; event service parameters for https must be sent via POST";
                Debug.logError(errMsg + "; in session " + ControlActivationEventListener.getSessionIdForLog(session), module);

                // the default here is true, so anything but N/n is true
                boolean requireEncryptedServiceWebParameters = !EntityUtilProperties.propertyValueEqualsIgnoreCase("url", "service.http.parameters.require.encrypted", "N", delegator);

                // NOTE: this forces service call event parameters to be in the body and not in the URL! can be issues with existing links, like Delete links or whatever, and those need to be changed to forms!
                if (requireEncryptedServiceWebParameters) {
                    throw new EventHandlerException(errMsg);
                }
            }
            // NOTTODO: may want to allow parameters that map to entity PK fields to be in the URL, but that might be a big security hole since there are certain security sensitive entities that are made of only PK fields, or that only need PK fields to function (like UserLoginSecurityGroup)
            // NOTTODO: we could allow URL parameters when it is not a POST (ie when !request.getMethod().equalsIgnoreCase("POST")), but that would open a security hole where sensitive parameters can be passed on the URL in a GET/etc and bypass this security constraint
        }
    }

    public static Map<String, Object> getMultiPartMap(HttpServletRequest request) throws EventHandlerException { // SCIPIO: Refactored
        Map<String, Object> multiPartMap = UtilGenerics.cast(request.getAttribute("multiPartMap"));
        if (multiPartMap == null) {
            try {
                // SCIPIO: Prevent double-processing in chained requests
                multiPartMap = UtilHttp.readMultiPartParameterMap(request);
                if (multiPartMap == null) {
                    multiPartMap = Collections.emptyMap();
                }
            } catch (IOException e) {
                throw new EventHandlerException(e.getCause() instanceof FileUploadException ? e.getCause() : e);
            }
            // store the multi-part map as an attribute so we can access the parameters
            request.setAttribute("multiPartMap", multiPartMap);
        }
        return multiPartMap;
    }
}
