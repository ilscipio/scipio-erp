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

/**
 * SCIPIO: This script emulates the execution of an event service call
 * from controller event, but using groovy so it can be run from screen.
 * 
 * It will run the service in a separate transaction to avoid having errors
 * halt the screen render transaction.
 * 
 * There are differences, such as the context variables used for error messages
 * instead of request attributes. It appends to existing event/error list,
 * not replace.
 * 
 * Derived from:
 * org.ofbiz.widget.renderer.ScreenRenderer.populateContextForRequest
 * org.ofbiz.webapp.event.ServiceEventHandler.invoke
 * 
 * FIXME:
 * * Does not support multi-part-map or string-map-prefix!
 * 
 * KNOWN ISSUES:
 * * Does not check for GET params on HTTPS requests; usually we want to allow here
 *   anyway because this is mainly intended for queries.
 */

import java.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.service.*;
import org.ofbiz.webapp.event.EventHandlerException;
import org.ofbiz.webapp.event.ServiceEventHandler;

final module = "runServiceLikeEvent.groovy";

doExec = context.rsleArgs.doExec; // boolean for easy conditional execution
if (doExec == null) {
    doExec = true;
}

serviceName = context.rsleArgs.serviceName;

serviceCtxOverrides = context.rsleArgs.serviceCtxOverrides;

serviceParams = context.rsleArgs.serviceParams;

serviceExtraParams = context.rsleArgs.serviceExtraParams;

updateCtx = context.rsleArgs.updateCtx;
if (updateCtx == null) {
    updateCtx = true;
}

updateReqAttr = context.rsleArgs.updateReqAttr;
if (updateReqAttr == null) {
    updateReqAttr = true;
}

updateParamsMap = context.rsleArgs.updateParamsMap;
if (updateParamsMap == null) {
    updateParamsMap = true;
}

context.remove("rsleArgs");

eventMessageList = [];
errorMessageList = [];

rsleRes = [:];
rsleRes.errorType = null;
// convenience bools
rsleRes.isExec = false;
rsleRes.isError = false;
rsleRes.isServiceFailure = false;
rsleRes.isServiceError = false;
rsleRes.isServiceSuccess = false;

if (doExec) {
    rsleRes.isExec = true;
    try {
        DispatchContext dctx = dispatcher.getDispatchContext();
        if (dctx == null) {
            throw new EventHandlerException("Dispatch context cannot be found");
        }
        
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
        
        if (serviceParams == null) {
        
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
                //if (UtilValidate.isNotEmpty(modelParam.stringMapPrefix)) {
                //    Map<String, Object> paramMap = UtilHttp.makeParamMapWithPrefix(request, multiPartMap, modelParam.stringMapPrefix, null);
                //    value = paramMap;
                //    if (Debug.verboseOn()) Debug.logVerbose("Set [" + modelParam.name + "]: " + paramMap, module);
                //} else if (UtilValidate.isNotEmpty(modelParam.stringListSuffix)) {
                //    List<Object> paramList = UtilHttp.makeParamListWithSuffix(request, multiPartMap, modelParam.stringListSuffix, null);
                //    value = paramList;
                //} else {
                if (true) {
                    // first check the multi-part map
                    //value = multiPartMap.get(name); // SCIPIO: TODO
    
                    // next check attributes; do this before parameters so that attribute which can be changed by code can override parameters which can't
                    if (UtilValidate.isEmpty(value)) {
                        Object tempVal = request.getAttribute(UtilValidate.isEmpty(modelParam.requestAttributeName) ? name : modelParam.requestAttributeName);
                        if (tempVal != null) {
                            value = tempVal;
                        }
                    }
    
                    // check the request parameters
                    if (UtilValidate.isEmpty(value)) {
                        // SCIPIO: WARN: this skips the standard POST security check, but almost no other screens check for this
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
            
            serviceParams = serviceContext;
        }
        
        if (serviceExtraParams) {
            serviceParams.putAll(serviceExtraParams);
        }
        
        servCtx = model.makeValid(serviceParams, ModelService.IN_PARAM);
        servCtx.put("userLogin", context.userLogin);
        servCtx.put("locale", context.locale);
        servCtx.put("timeZone", context.timeZone);
        if (serviceCtxOverrides != null) {
            servCtx.putAll(serviceCtxOverrides);
        }
        // NOTE: ALWAYS require a new transaction so errors don't mess up the screen render transaction
        servRes = dispatcher.runSync(serviceName, servCtx, -1, true);
        rsleRes.serviceResult = servRes;
        
        // set the results in the request AND parameters map
        for (Map.Entry<String, Object> rme: servRes.entrySet()) {
            String resultKey = rme.getKey();
            Object resultValue = rme.getValue();
    
            if (resultKey != null && !ModelService.RESPONSE_MESSAGE.equals(resultKey) && !ModelService.ERROR_MESSAGE.equals(resultKey) &&
                    !ModelService.ERROR_MESSAGE_LIST.equals(resultKey) && !ModelService.ERROR_MESSAGE_MAP.equals(resultKey) &&
                    !ModelService.SUCCESS_MESSAGE.equals(resultKey) && !ModelService.SUCCESS_MESSAGE_LIST.equals(resultKey)) {
                if (updateReqAttr) {
                    request.setAttribute(resultKey, resultValue);
                }
                if (updateParamsMap) {
                    parameters.put(resultKey, resultValue);
                }
            }
        }
        
        // DON'T put these in request attributes; they would have been removed by the ScreenRenderer
        servMsgs = [:];
        servMsgs.put("_ERROR_MESSAGE_LIST_", servRes.get(ModelService.ERROR_MESSAGE_LIST));
        servMsgs.put("_ERROR_MESSAGE_MAP_", servRes.get(ModelService.ERROR_MESSAGE_MAP));
        servMsgs.put("_ERROR_MESSAGE_", servRes.get(ModelService.ERROR_MESSAGE));
    
        servMsgs.put("_EVENT_MESSAGE_LIST_", servRes.get(ModelService.SUCCESS_MESSAGE_LIST));
        servMsgs.put("_EVENT_MESSAGE_", servRes.get(ModelService.SUCCESS_MESSAGE));
        
        if (servMsgs._EVENT_MESSAGE_) {
            eventMessageList.add(servMsgs._EVENT_MESSAGE_);
        }
        if (servMsgs._EVENT_MESSAGE_LIST_) {
            eventMessageList.addAll(servMsgs._EVENT_MESSAGE_LIST_);
        }
        if (servMsgs._ERROR_MESSAGE_) {
            errorMessageList.add(servMsgs._ERROR_MESSAGE_);
        }
        if (servMsgs._ERROR_MESSAGE_LIST_) {
            errorMessageList.addAll(servMsgs._ERROR_MESSAGE_LIST_);
        }
        
        rsleRes.isServiceFailure = ServiceUtil.isFailure(servRes);
        rsleRes.isServiceError = ServiceUtil.isError(servRes);
        rsleRes.isServiceSuccess = ServiceUtil.isSuccess(servRes);
    
        if (rsleRes.isServiceFailure) {
            rsleRes.errorType = "failure";
            rsleRes.isError = true;
        } else if (rsleRes.isServiceError) {
            rsleRes.errorType = "error";
            rsleRes.isError = true;
        }
    } catch (ServiceAuthException e) {
        errorMessageList.add(e.getNonNestedMessage());
        rsleRes.isError = true;
        rsleRes.errorType = "auth";
    } catch (ServiceValidationException e) {
        if (e.getMessageList() != null) {
            errorMessageList.addAll(e.getMessageList());
        } else {
            errorMessageList.add(e.getNonNestedMessage());
        }
        rsleRes.isError = true;
        rsleRes.errorType = "validate";
    } catch(GenericServiceException e) {
        Debug.logError(e, "Exception trying to run " + serviceName + " service from groovy", module);
        errorMessageList.add(UtilProperties.getMessage("CommonUiLabels", "CommonError", locale)); // TODO: better message
        rsleRes.isError = true;
        rsleRes.errorType = "general";
    }
    
    if (updateCtx) {
        if (context.eventMessageList == null) {
            context.eventMessageList = eventMessageList;
        } else {
            context.eventMessageList.addAll(eventMessageList);
        }
        
        if (context.errorMessageList == null) {
            context.errorMessageList = errorMessageList;
        } else {
            context.errorMessageList.addAll(errorMessageList);
        }
        
        if (errorMessageList) {
            context.isError = true;
        }
    }
}

rsleRes.eventMessageList = eventMessageList;
rsleRes.errorMessageList = errorMessageList;

context.rsleRes = rsleRes;

