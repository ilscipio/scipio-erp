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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.content.content.ContentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.RequestHandler;

import freemarker.core.Environment;
import freemarker.template.TemplateTransformModel;

/**
 * RenderContentAsText - Freemarker Transform for Content rendering
 * This transform cannot be called recursively (at this time).
 */
public class RenderContentAsText implements TemplateTransformModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String [] upSaveKeyNames = {"globalNodeTrail"};
    public static final String [] saveKeyNames = {"contentId", "subContentId", "subDataResourceTypeId", "mimeTypeId", "whenMap", "locale",  "wrapTemplateId", "encloseWrapText", "nullThruDatesOnly", "globalNodeTrail"};

    @Override
    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, @SuppressWarnings("rawtypes") Map args) {
        final Environment env = FreeMarkerWorker.getCurrentEnvironment();
        final LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
        final Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
        final HttpServletRequest request = FreeMarkerWorker.getWrappedObject("request", env);
        final HttpServletResponse response = FreeMarkerWorker.getWrappedObject("response", env);
        final Map<String, Object> templateRoot = FreeMarkerWorker.createEnvironmentMap(env);
        if (Debug.verboseOn()) {
            Debug.logVerbose("in RenderSubContent, contentId(0):" + templateRoot.get("contentId"), module);
        }
        FreeMarkerWorker.getSiteParameters(request, templateRoot);
        final Map<String, Object> savedValuesUp = new HashMap<>();
        FreeMarkerWorker.saveContextValues(templateRoot, upSaveKeyNames, savedValuesUp);
        FreeMarkerWorker.overrideWithArgs(templateRoot, args);
        if (Debug.verboseOn()) {
            Debug.logVerbose("in RenderSubContent, contentId(2):" + templateRoot.get("contentId"), module);
        }
        final String thisContentId =  (String)templateRoot.get("contentId");
        final String xmlEscape =  (String)templateRoot.get("xmlEscape");
        final boolean directAssocMode = UtilValidate.isNotEmpty(thisContentId) ? true : false;
        if (Debug.verboseOn()) {
            Debug.logVerbose("in Render(0), directAssocMode ." + directAssocMode , module);
        }

        final Map<String, Object> savedValues = new HashMap<>();

        return new Writer(out) {

            @Override
            public void write(char cbuf[], int off, int len) {
            }

            @Override
            public void flush() throws IOException {
                out.flush();
            }

            @Override
            public void close() throws IOException {
                List<Map<String, ? extends Object>> globalNodeTrail = UtilGenerics.checkList(templateRoot.get("globalNodeTrail"));
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Render close, globalNodeTrail(2a):" + ContentWorker.nodeTrailToCsv(globalNodeTrail), "");
                }
                renderSubContent();
            }

            public void renderSubContent() throws IOException {
                String mimeTypeId = (String) templateRoot.get("mimeTypeId");
                Object localeObject = templateRoot.get("locale");
                Locale locale = null;
                if (localeObject == null) {
                    locale = UtilHttp.getLocale(request);
                } else {
                    locale = UtilMisc.ensureLocale(localeObject);
                }

                String editRequestName = (String)templateRoot.get("editRequestName");
                if (Debug.verboseOn()) {
                    Debug.logVerbose("in Render(3), editRequestName ." + editRequestName , module);
                }

                if (UtilValidate.isNotEmpty(editRequestName)) {
                    String editStyle = getEditStyle();
                    openEditWrap(out, editStyle);
                }

                if (Debug.verboseOn()) {
                    Debug.logVerbose("in RenderSubContent, contentId(2):" + templateRoot.get("contentId"), module);
                    Debug.logVerbose("in RenderSubContent, subContentId(2):" + templateRoot.get("subContentId"), module);
                }
                FreeMarkerWorker.saveContextValues(templateRoot, saveKeyNames, savedValues);
                    try {
                        String txt = ContentWorker.renderContentAsText(dispatcher, delegator, thisContentId, templateRoot, locale, mimeTypeId, true);
                        if ("true".equals(xmlEscape)) {
                            txt = UtilFormatOut.encodeXmlValue(txt);
                        }

                        out.write(txt);

                    } catch (GeneralException e) {
                        String errMsg = "Error rendering thisContentId:" + thisContentId + " msg:" + e.toString();
                        Debug.logError(e, errMsg, module);
                        // just log a message and don't return anything: throw new IOException();
                    }
                //}
                FreeMarkerWorker.reloadValues(templateRoot, savedValuesUp, env);
                FreeMarkerWorker.reloadValues(templateRoot, savedValues, env);
                if (UtilValidate.isNotEmpty(editRequestName)) {
                    closeEditWrap(out, editRequestName);
                }

            }

            public void openEditWrap(Writer out, String editStyle) throws IOException {
                String divStr = "<div class=\"" + editStyle + "\">";
                out.write(divStr);
            }

            public void closeEditWrap(Writer out, String editRequestName) throws IOException {
                if (Debug.infoOn()) {
                    Debug.logInfo("in RenderSubContent, contentId(3):" + templateRoot.get("contentId"), module);
                    Debug.logInfo("in RenderSubContent, subContentId(3):" + templateRoot.get("subContentId"), module);
                }
                String fullRequest = editRequestName;
                String contentId = null;
                contentId = (String)templateRoot.get("subContentId");
                String delim = "?";
                if (UtilValidate.isNotEmpty(contentId)) {
                    fullRequest += delim + "contentId=" + contentId;
                    delim = "&";
                }

                out.write("<a href=\"");
                ServletContext servletContext = request.getServletContext(); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
                RequestHandler rh = (RequestHandler) servletContext.getAttribute("_REQUEST_HANDLER_");
                out.append(rh.makeLink(request, response, "/" + fullRequest, false, null, true)); // SCIPIO: 2018-07-09: changed secure to null
                out.write("\">Edit</a>");
                out.write("</div>");
            }

            public String getEditStyle() {
                String editStyle = (String)templateRoot.get("editStyle");
                if (UtilValidate.isEmpty(editStyle)) {
                    editStyle = UtilProperties.getPropertyValue("content", "defaultEditStyle");
                }
                if (UtilValidate.isEmpty(editStyle)) {
                    editStyle = "buttontext";
                }
                return editStyle;
            }
        };
    }
}
