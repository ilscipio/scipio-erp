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

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import freemarker.template.*;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.RequestHandler;

import freemarker.core.Environment;

/**
 * RenderContentAsText - Freemarker Transform for Content rendering
 * This transform cannot be called recursively (at this time).
 */
public class RenderContentTransform implements TemplateDirectiveModel { // SCIPIO: Refactored as TemplateDirectiveModel

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map params, TemplateModel[] loopVars, TemplateDirectiveBody body) throws TemplateException, IOException {
        Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
        LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
        HttpServletRequest request = FreeMarkerWorker.getWrappedObject("request", env);
        HttpServletResponse response = FreeMarkerWorker.getWrappedObject("response", env);
        MapStack<String> templateRoot = RenderMapStack.createRenderContext(FreeMarkerWorker.createEnvironmentMap(env)); // SCIPIO: Dedicated context class: MapStack.create(FreeMarkerWorker.createEnvironmentMap(env));
        templateRoot.push(params);
        try { // SCIPIO: Added try/finally block
            String xmlEscape = (String) templateRoot.get("xmlEscape");
            String thisContentId = (String) templateRoot.get("contentId");
            renderSubContent(env.getOut(), delegator, dispatcher, request, response, templateRoot, xmlEscape, thisContentId);
        } finally {
            templateRoot.pop(); // SCIPIO: Added pop()
        }
    }

    protected void renderSubContent(Writer out, Delegator delegator, LocalDispatcher dispatcher, HttpServletRequest request, HttpServletResponse response,
                                 MapStack<String> templateRoot, String xmlEscape, String thisContentId) throws IOException {
        String mimeTypeId = (String) templateRoot.get("mimeTypeId");
        Object localeObject = templateRoot.get("locale");
        Locale locale = null;
        if (localeObject == null) {
            locale = UtilHttp.getLocale(request);
        } else {
            locale = UtilMisc.ensureLocale(localeObject);
        }

        String editRequestName = (String)templateRoot.get("editRequestName");
        if (UtilValidate.isNotEmpty(editRequestName)) {
            String editStyle = getEditStyle(templateRoot);
            openEditWrap(out, editStyle);
        }

        try {
            String txt = null;

            String mapKey = (String)templateRoot.get("mapKey");
            if (UtilValidate.isEmpty(mapKey)) {
                txt = ContentWorker.renderContentAsText(dispatcher, delegator, thisContentId, templateRoot, locale, mimeTypeId, true);
            } else {
                txt = ContentWorker.renderSubContentAsText(dispatcher, delegator, thisContentId, mapKey, templateRoot, locale, mimeTypeId, true);
            }
            if ("true".equals(xmlEscape)) {
                txt = UtilFormatOut.encodeXmlValue(txt);
            }

            out.write(txt);
        } catch (GeneralException e) {
            String errMsg = "Error rendering thisContentId:" + thisContentId + " msg:" + e.toString();
            Debug.logError(e, errMsg, module);
            // just log a message and don't return anything: throw new IOException();
        }
        if (UtilValidate.isNotEmpty(editRequestName)) {
            closeEditWrap(out, request, response, thisContentId, editRequestName);
        }

    }

    private void openEditWrap(Writer out, String editStyle) throws IOException {
        String divStr = "<div class=\"" + editStyle + "\">";
        out.write(divStr);
    }

    private void closeEditWrap(Writer out, HttpServletRequest request, HttpServletResponse response, String thisContentId, String editRequestName) throws IOException {
        String fullRequest = editRequestName;
        String delim = "?";
        if (UtilValidate.isNotEmpty(thisContentId)) {
            fullRequest += delim + "contentId=" + thisContentId;
            delim = "&";
        }
        out.write("<a href=\"");
        ServletContext servletContext = request.getServletContext(); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
        RequestHandler rh = (RequestHandler) servletContext.getAttribute("_REQUEST_HANDLER_");
        out.append(rh.makeLink(request, response, "/" + fullRequest, false, null, true)); // SCIPIO: 2018-07-09: changed secure to null
        out.write("\">Edit</a>");
        out.write("</div>");
    }

    private String getEditStyle(MapStack<String> templateRoot) {
        String editStyle = (String) templateRoot.get("editStyle");
        if (UtilValidate.isEmpty(editStyle)) {
            editStyle = UtilProperties.getPropertyValue("content", "defaultEditStyle");
        }
        if (UtilValidate.isEmpty(editStyle)) {
            editStyle = "buttontext";
        }
        return editStyle;
    }
}
