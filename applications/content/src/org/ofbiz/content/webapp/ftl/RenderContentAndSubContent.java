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
package org.ofbiz.content.webapp.ftl;

import java.io.IOException;
import java.io.Writer;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import freemarker.template.*;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

import freemarker.core.Environment;

/**
 * RenderContentAndSubContent - Freemarker Transform for Content rendering
 * This transform cannot be called recursively (at this time).
 */
public class RenderContentAndSubContent implements TemplateDirectiveModel { // SCIPIO: Refactored as TemplateDirectiveModel

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map params, TemplateModel[] loopVars, TemplateDirectiveBody body) throws TemplateException, IOException {
        LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
        Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
        HttpServletRequest request = FreeMarkerWorker.getWrappedObject("request", env);
        HttpServletResponse response = FreeMarkerWorker.getWrappedObject("response", env); // SCIPIO
        Map<String, Object> envMap = FreeMarkerWorker.createEnvironmentMap(env);
        MapStack<String> templateRoot = RenderMapStack.createRenderContext(); // SCIPIO: Dedicated context class: MapStack.create();
        templateRoot.push(envMap);
        try { // SCIPIO: Added try/finally block
            if (Debug.verboseOn()) {
                Debug.logVerbose("in RenderContentAndSubContent, contentId(0):" + templateRoot.get("contentId"), module);
            }
            FreeMarkerWorker.getSiteParameters(request, templateRoot);
            FreeMarkerWorker.overrideWithArgs(templateRoot, params);
            renderSubContent(env.getOut(), delegator, dispatcher, request, response, templateRoot);
        } finally {
            templateRoot.pop(); // SCIPIO: Added pop()
        }
    }

    protected void renderSubContent(Writer out, Delegator delegator, LocalDispatcher dispatcher, HttpServletRequest request, HttpServletResponse response,
                                    MapStack<String> templateRoot) throws IOException {
        String mimeTypeId = (String) templateRoot.get("mimeTypeId");
        Object localeObject = templateRoot.get("locale");
        Locale locale = null;
        if (localeObject == null) {
            locale = UtilHttp.getLocale(request);
        } else {
            locale = UtilMisc.ensureLocale(localeObject);
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("in RenderContentAndSubContent, contentId(2):" + templateRoot.get("contentId"), module);
        }
        if (Debug.verboseOn()) {
            Debug.logVerbose("in RenderContentAndSubContent, subContentId(2):" + templateRoot.get("subContentId"), module);
        }
        try {
            String contentId = (String)templateRoot.get("contentId");
            String mapKey = (String)templateRoot.get("mapKey");
            String contentAssocTypeId = (String)templateRoot.get("contentAssocTypeId");
            if (UtilValidate.isNotEmpty(mapKey) || UtilValidate.isNotEmpty(contentAssocTypeId)) {
                String txt = ContentWorker.renderSubContentAsText(dispatcher, delegator, contentId, mapKey, templateRoot, locale, mimeTypeId, true);
                out.write(txt);
            } else if (contentId != null) {
                ContentWorker.renderContentAsText(dispatcher, delegator, contentId, out, templateRoot, locale, mimeTypeId, null, null, true);
            }
        } catch (GeneralException e) {
            String errMsg = "Error rendering thisContentId:" + (String)templateRoot.get("contentId") + " msg:" + e.toString();
            Debug.logError(e, errMsg, module);
            // just log a message and don't return anything: throw new IOException();
        }
    }
}
