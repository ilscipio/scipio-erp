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
package org.ofbiz.widget.renderer.macro;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.WeakHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.taglib.ContentUrlTag;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.content.WidgetContentWorker;
import org.ofbiz.widget.content.WidgetDataResourceWorker;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ModelScreenWidget;
import org.ofbiz.widget.model.ModelScreenWidget.Column;
import org.ofbiz.widget.model.ModelScreenWidget.ColumnContainer;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.model.ScreenFactory;
import org.ofbiz.widget.renderer.FormStringRenderer;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.ofbiz.widget.renderer.Paginator;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.html.HtmlScreenRenderer.ScreenletMenuRenderer;
import org.xml.sax.SAXException;

import freemarker.core.Environment;
import freemarker.template.Template;
import freemarker.template.TemplateException;

public class MacroScreenRenderer implements ScreenStringRenderer {

    public static final String module = MacroScreenRenderer.class.getName();
    private Template macroLibrary;
    private WeakHashMap<Appendable, Environment> environments = new WeakHashMap<Appendable, Environment>();
    private String rendererName;
    private int elementId = 999;
    protected boolean widgetCommentsEnabled = false;
    private static final String formrenderer = UtilProperties.getPropertyValue("widget", "screen.formrenderer");
    private int screenLetsIdCounter = 1;

    private MapStack<String> initialContext = null; // Scipio: context reference as received at beginning of screen render
    //private Map<String, Object> initialContextCopy = null; // Scipio: not needed for now
    
    /**
     * Scipio: Saves the initial screen context to use in populating a context upon creation
     * of Environments used to render macros, to provide context var support to Ofbiz widget macros.
     * <p>
     * 2016-01-06: Previously this class only saved and passed a few major global values from the
     * initial context (request, locale, globalContext, etc.). Now, we exploit the fact that the
     * initial context is a MapStack which is only a reference to other maps and simply pass this stack 
     * to the Environment which means it will automatically know of the last changes to it.
     * <p>
     * This is a delicate hack based on MapStack's implementation, but it works for now.
     * It has 2 major problems:
     * 1) we can't reliably add new keys to the map - but this is okay since stock code barely adds anything.
     * 2) the macros will not actually receive the REAL current context received by the java methods - that context
     *    may be something other than the global MapStack context. we simply ignore this for now
     *    because it is rare and probably not a problem.
     *    WARN: it could still be a problem in complex rendering cases.
     * <p>   
     * TODO?: The real solution to all of this is to modify all MacroXxxRenderer methods to pass context
     * to macro calls. However it's major change and may not even work with the current Freemarker calls used.
     */
    static class ContextHandler {

        private final String rendererLabel;
        private MacroScreenRenderer screenRenderer = null;

        public ContextHandler(String rendererLabel) {
            super();
            this.rendererLabel = rendererLabel;
        }

        private void registerScreenRenderer(Appendable writer, Map<String, Object> context) throws IOException {
            if (screenRenderer == null && context.get("screens") != null) {
                org.ofbiz.widget.renderer.ScreenRenderer screens = (org.ofbiz.widget.renderer.ScreenRenderer) context.get("screens");
                screenRenderer = (MacroScreenRenderer) screens.getScreenStringRenderer();
            }
        }
        
        /**
         * Registers the context as an initial context for the render thread.
         * Call from methods where the original context is expected to be passed at some point.
         */
        public void registerInitialContext(Appendable writer, Map<String, Object> context) throws IOException {
            registerScreenRenderer(writer, context);
            if (screenRenderer != null) {
                if (screenRenderer.initialContext == null) {
                    if (context instanceof MapStack) {
                        screenRenderer.initialContext = (MapStack<String>) context;
                        //screenRenderer.initialContextCopy = new HashMap<String, Object>(context);
                    }
                    else {
                        throw new IllegalStateException("Scipio: Expected initial screen context to be a MapStack, "
                                + "but was of type: " + context.getClass().getName());
                    }
                }
            }
            else {
                Debug.logError("macro " + rendererLabel + " renderer template environment initial context register "
                        + "could not retrieve macro screen renderer instance", module);
            }
        }
        
        /**
         * Registers/saves elements from context as needed.
         * Call from any *StringRenderer method that may be important.
         */
        public void registerContext(Appendable writer, Map<String, Object> context) throws IOException {
            registerScreenRenderer(writer, context);
        }
        
        /**
         * Creates a new context as source for data model for a new Environment, populated with given
         * initial values, in addition to anything we deem needed.
         * <p>
         * FIXME?: This does not handle the real, "current" context. To minimize intrusive changes
         * we currently (still) try to emulate as best as possible using an initial context.
         * otherwise have to rewrite nearly all renderer methods (may have to...).
         */
        public Map<String, Object> createRenderContext(Appendable writer, Map<String, Object> currentContext, Map<String, Object> extraValues) throws IOException {
            if (currentContext != null) {
                throw new UnsupportedOperationException("Not expecting to receive current context in current implementation");
            }
            else if (screenRenderer != null) {
                MapStack<String> initContext = screenRenderer.initialContext;
                if (initContext != null) {
                    return createRenderContextFromInitial(initContext, extraValues);
                }
                else {
                    Debug.logError("macro " + rendererLabel + " renderer template environment initial context absent", module);
                    return new HashMap<String, Object>(extraValues);
                }
            }
            else {
                Debug.logError("macro " + rendererLabel + " renderer template environment initial context populate "
                        + "could not retrieve macro screen renderer instance", module);
                return new HashMap<String, Object>(extraValues);
            }
        }
        
        private Map<String, Object> createRenderContextFromInitial(MapStack<String> initContext, Map<String, Object> extraValues) throws IOException {

            // FIXME: here we'll cheat and just dump this into the context. it's a terrible idea but won't matter.
            initContext.putAll(extraValues);
            
            return initContext;
        }
    }
    
    private ContextHandler contextHandler = new ContextHandler("screen");
    
    
    public MacroScreenRenderer(String name, String macroLibraryPath) throws TemplateException, IOException {
        macroLibrary = FreeMarkerWorker.getTemplate(macroLibraryPath);
        rendererName = name;
    }

    @Deprecated
    public MacroScreenRenderer(String name, String macroLibraryPath, Appendable writer) throws TemplateException, IOException {
        this(name, macroLibraryPath);
    }

    /**
     * Scipio: Returns macro library path used for this renderer. 
     */
    public String getMacroLibraryPath() {
        return macroLibrary.getName();
    }
    
    private String getNextElementId() {
        elementId++;
        return "hsr" + elementId;
    }

    private void executeMacro(Appendable writer, String macro) throws IOException {
        try {
            Environment environment = getEnvironment(writer);
            Reader templateReader = new StringReader(macro);
            // FIXME: I am using a Date as an hack to provide a unique name for the template...
            Template template = new Template((new java.util.Date()).toString(), templateReader, FreeMarkerWorker.getDefaultOfbizConfig());
            templateReader.close();
            FreeMarkerWorker.includeTemplate(template, environment);
        } catch (TemplateException e) {
            Debug.logError(e, "Error rendering screen macro [" + macro + "] thru ftl", module);
        } catch (IOException e) {
            Debug.logError(e, "Error rendering screen macro [" + macro + "] thru ftl", module);
        }
    }

    private void executeMacro(Appendable writer, String macroName, Map<String, Object> parameters) throws IOException {
        StringBuilder sb = new StringBuilder("<@");
        sb.append(macroName);
        if (parameters != null) {
            for (Map.Entry<String, Object> parameter : parameters.entrySet()) {
                sb.append(' ');
                sb.append(parameter.getKey());
                sb.append("=");
                Object value = parameter.getValue();
                if (value instanceof String) {
                    sb.append('"');
                    sb.append(((String) value).replaceAll("\"", "\\\\\""));
                    sb.append('"');
                } else {
                    sb.append(value);
                }
            }
        }
        sb.append(" />");
        executeMacro(writer, sb.toString());
    }

    private Environment getEnvironment(Appendable writer) throws TemplateException, IOException {
        Environment environment = environments.get(writer);
        if (environment == null) {
            // Scipio: custom render context
            Map<String, Object> input = contextHandler.createRenderContext(writer, null, UtilMisc.toMap("key", null));
            environment = FreeMarkerWorker.renderTemplate(macroLibrary, input, writer);
            environments.put(writer, environment);
        }
        return environment;
    }

    public String getRendererName() {
        return rendererName;
    }

    public void renderScreenBegin(Appendable writer, Map<String, Object> context) throws IOException {
        contextHandler.registerInitialContext(writer, context);
        executeMacro(writer, "renderScreenBegin", null);
    }

    public void renderScreenEnd(Appendable writer, Map<String, Object> context) throws IOException {
        executeMacro(writer, "renderScreenEnd", null);
    }

    public void renderSectionBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Section section) throws IOException {
        if (section.isMainSection()) {
            this.widgetCommentsEnabled = ModelWidget.widgetBoundaryCommentsEnabled(context);
        }
        if (this.widgetCommentsEnabled) {
            Map<String, Object> parameters = new HashMap<String, Object>();
            StringBuilder sb = new StringBuilder("Begin ");
            sb.append(section.isMainSection() ? "Screen " : "Section Widget ");
            sb.append(section.getBoundaryCommentName());
            parameters.put("boundaryComment", sb.toString());
            executeMacro(writer, "renderSectionBegin", parameters);
        }
    }
    public void renderSectionEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Section section) throws IOException {
        if (this.widgetCommentsEnabled) {
            Map<String, Object> parameters = new HashMap<String, Object>();
            StringBuilder sb = new StringBuilder();
            sb.append("End ");
            sb.append(section.isMainSection() ? "Screen " : "Section Widget ");
            sb.append(section.getBoundaryCommentName());
            parameters.put("boundaryComment", sb.toString());
            executeMacro(writer, "renderSectionEnd", parameters);
        }
    }

    public void renderContainerBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Container container) throws IOException {
        String containerId = container.getId(context);
        String autoUpdateTarget = container.getAutoUpdateTargetExdr(context);
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        String autoUpdateLink = "";
        if (UtilValidate.isNotEmpty(autoUpdateTarget) && UtilHttp.isJavaScriptEnabled(request)) {
            if (UtilValidate.isEmpty(containerId)) {
                containerId = getNextElementId();
            }
            HttpServletResponse response = (HttpServletResponse) context.get("response");
            ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
            RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
            autoUpdateLink = rh.makeLink(request, response, autoUpdateTarget);
        }
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("id", containerId);
        parameters.put("style", container.getStyle(context));
        parameters.put("autoUpdateLink", autoUpdateLink);
        parameters.put("autoUpdateInterval", container.getAutoUpdateInterval(context));
        executeMacro(writer, "renderContainerBegin", parameters);
    }

    public void renderContainerEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Container container) throws IOException {
        executeMacro(writer, "renderContainerEnd", null);
    }

    public void renderLabel(Appendable writer, Map<String, Object> context, ModelScreenWidget.Label label) throws IOException {
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("text", label.getText(context));
        parameters.put("id", label.getId(context));
        parameters.put("style", label.getStyle(context));
        executeMacro(writer, "renderLabel", parameters);
    }

    public void renderHorizontalSeparator(Appendable writer, Map<String, Object> context, ModelScreenWidget.HorizontalSeparator separator) throws IOException {
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("id", separator.getId(context));
        parameters.put("style", separator.getStyle(context));
        executeMacro(writer, "renderHorizontalSeparator", parameters);
    }

    public void renderLink(Appendable writer, Map<String, Object> context, ModelScreenWidget.ScreenLink link) throws IOException {
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        HttpServletRequest request = (HttpServletRequest) context.get("request");

        String targetWindow = link.getTargetWindow(context);
        String target = link.getTarget(context);

        String uniqueItemName = link.getModelScreen().getName() + "_LF_" + UtilMisc.<String>addToBigDecimalInMap(context, "screenUniqueItemIndex", BigDecimal.ONE);

        String linkType = WidgetWorker.determineAutoLinkType(link.getLinkType(), target, link.getUrlMode(), request);
        String linkUrl = "";
        String actionUrl = "";
        StringBuilder parameters=new StringBuilder();
        String width = link.getWidth();
        if (UtilValidate.isEmpty(width)) {
            width = "300";
        }
        String height = link.getHeight();
        if (UtilValidate.isEmpty(height)) {
            height = "200";
        }
        if ("hidden-form".equals(linkType) || "ajax-window".equals(linkType)) {
            StringBuilder sb = new StringBuilder();
            WidgetWorker.buildHyperlinkUrl(sb, target, link.getUrlMode(), null, link.getPrefix(context),
                    link.getFullPath(), link.getSecure(), link.getEncode(), request, response, context);
            actionUrl = sb.toString();
            parameters.append("[");
            for (Map.Entry<String, String> parameter: link.getParameterMap(context).entrySet()) {
                if (parameters.length() >1) {
                    parameters.append(",");
                }
                parameters.append("{'name':'");
                parameters.append(parameter.getKey());
                parameters.append("'");
                parameters.append(",'value':'");
                parameters.append(parameter.getValue());
                parameters.append("'}");
            }
            parameters.append("]");

        }
        String id = link.getId(context);
        String style = link.getStyle(context);
        String name = link.getName(context);
        String text = link.getText(context);
        if (UtilValidate.isNotEmpty(target)) {
            if (!"hidden-form".equals(linkType)) {
                StringBuilder sb = new StringBuilder();
                WidgetWorker.buildHyperlinkUrl(sb, target, link.getUrlMode(), link.getParameterMap(context), link.getPrefix(context),
                        link.getFullPath(), link.getSecure(), link.getEncode(), request, response, context);
                linkUrl = sb.toString();
            }
        }
        String imgStr = "";
        ModelScreenWidget.ScreenImage img = link.getImage();
        if (img != null) {
            StringWriter sw = new StringWriter();
            renderImage(sw, context, img);
            imgStr = sw.toString();
        }
        StringWriter sr = new StringWriter();
        sr.append("<@renderLink ");
        sr.append("parameterList=");
        sr.append(parameters.length()==0?"\"\"":parameters.toString());
        sr.append(" targetWindow=\"");
        sr.append(targetWindow);
        sr.append("\" target=\"");
        sr.append(target);
        sr.append("\" uniqueItemName=\"");
        sr.append(uniqueItemName);
        sr.append("\" linkType=\"");
        sr.append(linkType);
        sr.append("\" actionUrl=\"");
        sr.append(actionUrl);
        sr.append("\" id=\"");
        sr.append(id);
        sr.append("\" style=\"");
        sr.append(style);
        sr.append("\" name=\"");
        sr.append(name);
        sr.append("\" width=\"");
        sr.append(width);
        sr.append("\" height=\"");
        sr.append(height);
        sr.append("\" linkUrl=\"");
        sr.append(linkUrl);
        sr.append("\" text=\"");
        sr.append(text);
        sr.append("\" imgStr=\"");
        sr.append(imgStr.replaceAll("\"", "\\\\\""));
        sr.append("\" />");
        executeMacro(writer, sr.toString());
    }

    public void renderImage(Appendable writer, Map<String, Object> context, ModelScreenWidget.ScreenImage image) throws IOException {
        if (image == null)
            return ;
        String src = image.getSrc(context);

        String urlMode = image.getUrlMode();
        Boolean fullPath = null; // Scipio: changed from boolean to Boolean
        Boolean secure = null; // Scipio: changed from boolean to Boolean
        Boolean encode = false; // Scipio: changed from boolean to Boolean
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        String urlString = "";
        if (urlMode != null && urlMode.equalsIgnoreCase("intra-app")) {
            if (request != null && response != null) {
                ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
                RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
                urlString = rh.makeLink(request, response, src, fullPath, secure, encode);
            } else {
                urlString = src;
            }
        } else  if (urlMode != null && urlMode.equalsIgnoreCase("content")) {
            if (request != null && response != null) {
                StringBuilder newURL = new StringBuilder();
                ContentUrlTag.appendContentPrefix(request, newURL);
                newURL.append(src);
                urlString = newURL.toString();
            }
        } else {
            urlString = src;
        }
        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("src", src);
        parameters.put("id", image.getId(context));
        parameters.put("style", image.getStyle(context));
        parameters.put("wid", image.getWidth(context));
        parameters.put("hgt", image.getHeight(context));
        parameters.put("border", image.getBorder(context));
        parameters.put("alt", image.getAlt(context));
        parameters.put("urlString", urlString);
        executeMacro(writer, "renderImage", parameters);
    }

    public void renderContentBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException {
         String editRequest = content.getEditRequest(context);
         String enableEditName = content.getEnableEditName(context);
         String enableEditValue = (String)context.get(enableEditName);

         if (Debug.verboseOn()) Debug.logVerbose("directEditRequest:" + editRequest, module);

         Map<String, Object> parameters = new HashMap<String, Object>();
         parameters.put("editRequest", editRequest);
         parameters.put("enableEditValue", enableEditValue == null ? "" : enableEditValue);
         parameters.put("editContainerStyle", content.getEditContainerStyle(context));
         executeMacro(writer, "renderContentBegin", parameters);
    }

    public void renderContentBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException {
        Locale locale = UtilMisc.ensureLocale(context.get("locale"));
        //Boolean nullThruDatesOnly = Boolean.valueOf(false);
        String mimeTypeId = "text/html";
        String expandedContentId = content.getContentId(context);
        String expandedDataResourceId = content.getDataResourceId(context);
        String renderedContent = null;
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        Delegator delegator = (Delegator) context.get("delegator");

        // make a new map for content rendering; so our current map does not get clobbered
        Map<String, Object> contentContext = new HashMap<String, Object>();
        contentContext.putAll(context);
        String dataResourceId = (String)contentContext.get("dataResourceId");
        if (Debug.verboseOn()) Debug.logVerbose("expandedContentId:" + expandedContentId, module);

        try {
            if (UtilValidate.isNotEmpty(dataResourceId)) {
                if (WidgetDataResourceWorker.dataresourceWorker != null) {
                    renderedContent = WidgetDataResourceWorker.dataresourceWorker.renderDataResourceAsTextExt(delegator, dataResourceId, contentContext, locale, mimeTypeId, false);
                } else {
                    Debug.logError("Not rendering content, WidgetDataResourceWorker.dataresourceWorker not found.", module);
                }
            } else if (UtilValidate.isNotEmpty(expandedContentId)) {
                if (WidgetContentWorker.contentWorker != null) {
                    renderedContent = WidgetContentWorker.contentWorker.renderContentAsTextExt(dispatcher, delegator, expandedContentId, contentContext, locale, mimeTypeId, true);
                } else {
                    Debug.logError("Not rendering content, WidgetContentWorker.contentWorker not found.", module);
                }
            } else if (UtilValidate.isNotEmpty(expandedDataResourceId)) {
                if (WidgetDataResourceWorker.dataresourceWorker != null) {
                    renderedContent = WidgetDataResourceWorker.dataresourceWorker.renderDataResourceAsTextExt(delegator, expandedDataResourceId, contentContext, locale, mimeTypeId, false);
                } else {
                    Debug.logError("Not rendering content, WidgetDataResourceWorker.dataresourceWorker not found.", module);
                }
            }
            if (UtilValidate.isEmpty(renderedContent)) {
                String editRequest = content.getEditRequest(context);
                if (UtilValidate.isNotEmpty(editRequest)) {
                    if (WidgetContentWorker.contentWorker != null) {
                        WidgetContentWorker.contentWorker.renderContentAsTextExt(dispatcher, delegator, "NOCONTENTFOUND", writer, contentContext, locale, mimeTypeId, true);
                    } else {
                        Debug.logError("Not rendering content, WidgetContentWorker.contentWorker not found.", module);
                    }
                }
            } else {
                if (content.xmlEscape()) {
                    renderedContent = UtilFormatOut.encodeXmlValue(renderedContent);
                }

                writer.append(renderedContent);
            }

        } catch (GeneralException e) {
            String errMsg = "Error rendering included content with id [" + expandedContentId + "] : " + e.toString();
            Debug.logError(e, errMsg, module);
            //throw new RuntimeException(errMsg);
        } catch (IOException e2) {
            String errMsg = "Error rendering included content with id [" + expandedContentId + "] : " + e2.toString();
            Debug.logError(e2, errMsg, module);
            //throw new RuntimeException(errMsg);
        }
    }

    public void renderContentEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException {
        String expandedContentId = content.getContentId(context);
        String editMode = "Edit";
        String editRequest = content.getEditRequest(context);
        String enableEditName = content.getEnableEditName(context);
        String enableEditValue = (String)context.get(enableEditName);
        String urlString = "";
        if (editRequest != null && editRequest.toUpperCase().indexOf("IMAGE") > 0) {
            editMode += " Image";
        }

        if (UtilValidate.isNotEmpty(editRequest) && "true".equals(enableEditValue)) {
            HttpServletResponse response = (HttpServletResponse) context.get("response");
            HttpServletRequest request = (HttpServletRequest) context.get("request");
            if (request != null && response != null) {
                if (editRequest.indexOf("?") < 0)  editRequest += "?";
                else editRequest += "&amp;";
                editRequest += "contentId=" + expandedContentId;
                ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
                RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
                urlString = rh.makeLink(request, response, editRequest, false, false, false);
            }

            Map<String, Object> parameters = new HashMap<String, Object>();
            parameters.put("urlString", urlString);
            parameters.put("editMode", editMode);
            parameters.put("editContainerStyle", content.getEditContainerStyle(context));
            parameters.put("editRequest", editRequest);
            parameters.put("enableEditValue", enableEditValue);
            executeMacro(writer, "renderContentEnd", parameters);
        }
    }

    public void renderContentFrame(Appendable writer, Map<String, Object> context, ModelScreenWidget.Content content) throws IOException {
        String dataResourceId = content.getDataResourceId(context);
        String urlString = "/ViewSimpleContent?dataResourceId=" + dataResourceId;
        String fullUrlString = "";
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        if (request != null && response != null) {
            ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
            RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
            fullUrlString = rh.makeLink(request, response, urlString, true, false, false);
        }

        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("fullUrl", fullUrlString);
        parameters.put("width", content.getWidth());
        parameters.put("height", content.getHeight());
        parameters.put("border", content.getBorder());
        executeMacro(writer, "renderContentFrame", parameters);
    }

    public void renderSubContentBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException {
         String enableEditName = content.getEnableEditName(context);
         String enableEditValue = (String)context.get(enableEditName);

         Map<String, Object> parameters = new HashMap<String, Object>();
         parameters.put("editContainerStyle", content.getEditContainerStyle(context));
         parameters.put("editRequest", content.getEditRequest(context));
         parameters.put("enableEditValue", enableEditValue == null ? "" : enableEditValue);
         executeMacro(writer, "renderSubContentBegin", parameters);
    }

    public void renderSubContentBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException {
         Locale locale = UtilMisc.ensureLocale(context.get("locale"));
         String mimeTypeId = "text/html";
         String expandedContentId = content.getContentId(context);
         String expandedMapKey = content.getMapKey(context);
         String renderedContent = "";
         LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
         Delegator delegator = (Delegator) context.get("delegator");

         // create a new map for the content rendering; so our current context does not get overwritten!
         Map<String, Object> contentContext = new HashMap<String, Object>();
         contentContext.putAll(context);

         try {
             if (WidgetContentWorker.contentWorker != null) {
                 renderedContent = WidgetContentWorker.contentWorker.renderSubContentAsTextExt(dispatcher, delegator, expandedContentId, expandedMapKey, contentContext, locale, mimeTypeId, true);
                 //Debug.logInfo("renderedContent=" + renderedContent, module);
             } else {
                 Debug.logError("Not rendering content, WidgetContentWorker.contentWorker not found.", module);
             }
             if (UtilValidate.isEmpty(renderedContent)) {
                 String editRequest = content.getEditRequest(context);
                 if (UtilValidate.isNotEmpty(editRequest)) {
                     if (WidgetContentWorker.contentWorker != null) {
                         WidgetContentWorker.contentWorker.renderContentAsTextExt(dispatcher, delegator, "NOCONTENTFOUND", writer, contentContext, locale, mimeTypeId, true);
                     } else {
                         Debug.logError("Not rendering content, WidgetContentWorker.contentWorker not found.", module);
                     }
                 }
             } else {
                 if (content.xmlEscape()) {
                     renderedContent = UtilFormatOut.encodeXmlValue(renderedContent);
                 }

                 writer.append(renderedContent);
             }

         } catch (GeneralException e) {
             String errMsg = "Error rendering included content with id [" + expandedContentId + "] : " + e.toString();
             Debug.logError(e, errMsg, module);
             //throw new RuntimeException(errMsg);
         } catch (IOException e2) {
             String errMsg = "Error rendering included content with id [" + expandedContentId + "] : " + e2.toString();
             Debug.logError(e2, errMsg, module);
             //throw new RuntimeException(errMsg);
         }
    }

    public void renderSubContentEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.SubContent content) throws IOException {
         String editMode = "Edit";
         String editRequest = content.getEditRequest(context);
         String enableEditName = content.getEnableEditName(context);
         String enableEditValue = (String)context.get(enableEditName);
         String expandedContentId = content.getContentId(context);
         String expandedMapKey = content.getMapKey(context);
         String urlString = "";
         if (editRequest != null && editRequest.toUpperCase().indexOf("IMAGE") > 0) {
             editMode += " Image";
         }
         if (UtilValidate.isNotEmpty(editRequest) && "true".equals(enableEditValue)) {
             HttpServletResponse response = (HttpServletResponse) context.get("response");
             HttpServletRequest request = (HttpServletRequest) context.get("request");
             if (request != null && response != null) {
                 if (editRequest.indexOf("?") < 0)  editRequest += "?";
                 else editRequest += "&amp;";
                 editRequest += "contentId=" + expandedContentId;
                 if (UtilValidate.isNotEmpty(expandedMapKey)) {
                     editRequest += "&amp;mapKey=" + expandedMapKey;
                 }
                 ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
                 RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
                 urlString = rh.makeLink(request, response, editRequest, false, false, false);
             }
         }

         Map<String, Object> parameters = new HashMap<String, Object>();
         parameters.put("urlString", urlString);
         parameters.put("editMode", editMode);
         parameters.put("editContainerStyle", content.getEditContainerStyle(context));
         parameters.put("editRequest", editRequest);
         parameters.put("enableEditValue", enableEditValue == null ? "" : enableEditValue);
         executeMacro(writer, "renderSubContentEnd", parameters);
    }


    public void renderScreenletBegin(Appendable writer, Map<String, Object> context, boolean collapsed, ModelScreenWidget.Screenlet screenlet) throws IOException {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        boolean javaScriptEnabled = UtilHttp.isJavaScriptEnabled(request);
        ModelScreenWidget.Menu tabMenu = screenlet.getTabMenu();
        if (tabMenu != null) {
            tabMenu.renderWidgetString(writer, context, this);
        }

        String title = screenlet.getTitle(context);
        String titleStyle = screenlet.getTitleStyle(context);
        boolean collapsible = screenlet.collapsible();
        ModelScreenWidget.Menu navMenu = screenlet.getNavigationMenu();
        ModelScreenWidget.Form navForm = screenlet.getNavigationForm();
        String expandToolTip = "";
        String collapseToolTip = "";
        String fullUrlString = "";
        String menuString = "";
        boolean showMore = false;
        String menuRole = "";
        if (UtilValidate.isNotEmpty(title) || navMenu != null || navForm != null || collapsible) {
            showMore = true;
            if (collapsible) {
                this.getNextElementId();
                Map<String, Object> uiLabelMap = UtilGenerics.checkMap(context.get("uiLabelMap"));
                Map<String, Object> paramMap = UtilGenerics.checkMap(context.get("requestParameters"));
                Map<String, Object> requestParameters = new HashMap<String, Object>(paramMap);
                if (uiLabelMap != null) {
                    expandToolTip = (String) uiLabelMap.get("CommonExpand");
                    collapseToolTip = (String) uiLabelMap.get("CommonCollapse");
                }
                if (!javaScriptEnabled) {
                    requestParameters.put(screenlet.getPreferenceKey(context) + "_collapsed", collapsed ? "false" : "true");
                    String queryString = UtilHttp.urlEncodeArgs(requestParameters);
                    fullUrlString = request.getRequestURI() + "?" + queryString;
                }
            }
            StringWriter sb = new StringWriter();
            if (navMenu != null) {
                // Scipio: use the macro renderer for this
                //MenuStringRenderer savedRenderer = (MenuStringRenderer) context.get("menuStringRenderer");
                //MenuStringRenderer renderer = new ScreenletMenuRenderer(request, response);
                //context.put("menuStringRenderer", renderer);
                Map<String, Object> menuRenderArgs = UtilMisc.getMapFromMap(context, "menuRenderArgs");
                menuRenderArgs.put("inlineEntries", Boolean.TRUE);
                menuRenderArgs.put("menuCtxRole", "screenlet-nav-menu");
                navMenu.renderWidgetString(sb, context, this);
                //context.put("menuStringRenderer", savedRenderer);
                menuRole = "nav-menu";
            } else if (navForm != null) {
                renderScreenletPaginateMenu(sb, context, navForm);
                menuRole = "paginate-menu";
            }
            menuString = sb.toString();
        }

        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("title", title);
        parameters.put("titleStyle", titleStyle);
        parameters.put("collapsible", collapsible);
        parameters.put("saveCollapsed", screenlet.saveCollapsed());
        if (UtilValidate.isNotEmpty (screenlet.getId(context))) {
            parameters.put("id", screenlet.getId(context));
            parameters.put("collapsibleAreaId", screenlet.getId(context) + "_col");
        } else {
            parameters.put("id", "screenlet_" + screenLetsIdCounter);
            parameters.put("collapsibleAreaId","screenlet_" + screenLetsIdCounter + "_col");
            screenLetsIdCounter++;
        }
        parameters.put("expandToolTip", expandToolTip);
        parameters.put("collapseToolTip", collapseToolTip);
        parameters.put("fullUrlString", fullUrlString);
        parameters.put("padded", screenlet.padded());
        parameters.put("menuString", menuString);
        parameters.put("showMore", showMore);
        parameters.put("collapsed", collapsed);
        parameters.put("javaScriptEnabled", javaScriptEnabled);
        parameters.put("menuRole", menuRole);
        executeMacro(writer, "renderScreenletBegin", parameters);
    }

    public void renderScreenletSubWidget(Appendable writer, Map<String, Object> context, ModelScreenWidget subWidget, ModelScreenWidget.Screenlet screenlet) throws GeneralException, IOException  {
        if (subWidget.equals(screenlet.getNavigationForm())) {
            HttpServletRequest request = (HttpServletRequest) context.get("request");
            HttpServletResponse response = (HttpServletResponse) context.get("response");
            if (request != null && response != null) {
                Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                globalCtx.put("NO_PAGINATOR", true);
                FormStringRenderer savedRenderer = (FormStringRenderer) context.get("formStringRenderer");
                MacroFormRenderer renderer = null;
                try {
                    renderer = new MacroFormRenderer(formrenderer, request, response);
                } catch (TemplateException e) {
                    Debug.logError("Not rendering content, error on MacroFormRenderer creation.", module);
                }
                renderer.setRenderPagination(false);
                context.put("formStringRenderer", renderer);
                subWidget.renderWidgetString(writer, context, this);
                context.put("formStringRenderer", savedRenderer);
            }
        } else {
            subWidget.renderWidgetString(writer, context, this);
        }
    }
    public void renderScreenletEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.Screenlet screenlet) throws IOException {
        executeMacro(writer, "renderScreenletEnd", null);
    }

    protected void renderScreenletPaginateMenu(Appendable writer, Map<String, Object> context, ModelScreenWidget.Form form) throws IOException {
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        ModelForm modelForm;
        try {
            modelForm = form.getModelForm(context);
        } catch (Exception e) {
            throw new IOException(e);
        }
        modelForm.runFormActions(context);
        Paginator.preparePager(modelForm, context);
        String targetService = modelForm.getPaginateTarget(context);
        if (targetService == null) {
            targetService = "${targetService}";
        }

        // get the parametrized pagination index and size fields
        int paginatorNumber = WidgetWorker.getPaginatorNumber(context);
        String viewIndexParam = modelForm.getMultiPaginateIndexField(context);
        String viewSizeParam = modelForm.getMultiPaginateSizeField(context);

        int viewIndex = Paginator.getViewIndex(modelForm, context);
        int viewSize = Paginator.getViewSize(modelForm, context);
        int listSize = Paginator.getListSize(context);

        int highIndex = Paginator.getHighIndex(context);
        int actualPageSize = Paginator.getActualPageSize(context);

        // if this is all there seems to be (if listSize < 0, then size is unknown)
        if (actualPageSize >= listSize && listSize >= 0) return;

        // needed for the "Page" and "rows" labels
        Map<String, String> uiLabelMap = UtilGenerics.cast(context.get("uiLabelMap"));
        String ofLabel = "";
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        } else {
            ofLabel = uiLabelMap.get("CommonOf");
            ofLabel = ofLabel.toLowerCase();
        }

        // for legacy support, the viewSizeParam is VIEW_SIZE and viewIndexParam is VIEW_INDEX when the fields are "viewSize" and "viewIndex"
        if (viewIndexParam.equals("viewIndex" + "_" + paginatorNumber)) viewIndexParam = "VIEW_INDEX" + "_" + paginatorNumber;
        if (viewSizeParam.equals("viewSize" + "_" + paginatorNumber)) viewSizeParam = "VIEW_SIZE" + "_" + paginatorNumber;

        ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
        RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");

        Map<String, Object> inputFields = UtilGenerics.toMap(context.get("requestParameters"));
        // strip out any multi form fields if the form is of type multi
        if (modelForm.getType().equals("multi")) {
            inputFields = UtilHttp.removeMultiFormParameters(inputFields);
        }
        String queryString = UtilHttp.urlEncodeArgs(inputFields);
        // strip legacy viewIndex/viewSize params from the query string
        queryString = UtilHttp.stripViewParamsFromQueryString(queryString, "" + paginatorNumber);
        // strip parametrized index/size params from the query string
        HashSet<String> paramNames = new HashSet<String>();
        paramNames.add(viewIndexParam);
        paramNames.add(viewSizeParam);
        queryString = UtilHttp.stripNamedParamsFromQueryString(queryString, paramNames);

        String anchor = "";
        String paginateAnchor = modelForm.getPaginateTargetAnchor();
        if (paginateAnchor != null) anchor = "#" + paginateAnchor;

        // preparing the link text, so that later in the code we can reuse this and just add the viewIndex
        String prepLinkText = "";
        prepLinkText = targetService;
        if (prepLinkText.indexOf("?") < 0) {
            prepLinkText += "?";
        } else if (!prepLinkText.endsWith("?")) {
            prepLinkText += "&amp;";
        }
        if (!UtilValidate.isEmpty(queryString) && !queryString.equals("null")) {
            prepLinkText += queryString + "&amp;";
        }
        prepLinkText += viewSizeParam + "=" + viewSize + "&amp;" + viewIndexParam + "=";

        String linkText;


        // The current screenlet title bar navigation syling requires rendering
        // these links in reverse order
        // Last button
        String lastLinkUrl = "";
        if (highIndex < listSize) {
            int lastIndex = UtilMisc.getViewLastIndex(listSize, viewSize);
            linkText = prepLinkText + lastIndex + anchor;
            lastLinkUrl = rh.makeLink(request, response, linkText);
        }
        String nextLinkUrl = "";
        if (highIndex < listSize) {
            linkText = prepLinkText + (viewIndex + 1) + anchor;
            // - make the link
            nextLinkUrl = rh.makeLink(request, response, linkText);
        }
        String previousLinkUrl = "";
        if (viewIndex > 0) {
            linkText = prepLinkText + (viewIndex - 1) + anchor;
            previousLinkUrl = rh.makeLink(request, response, linkText);
        }
        String firstLinkUrl = "";
        if (viewIndex > 0) {
            linkText = prepLinkText + 0 + anchor;
            firstLinkUrl = rh.makeLink(request, response, linkText);
        }

        Map<String, Object> parameters = new HashMap<String, Object>();
        parameters.put("lowIndex", Paginator.getLowIndex(context));
        parameters.put("actualPageSize", actualPageSize);
        parameters.put("ofLabel", ofLabel);
        parameters.put("listSize", listSize);
        parameters.put("paginateLastStyle", modelForm.getPaginateLastStyle());
        parameters.put("lastLinkUrl", lastLinkUrl);
        parameters.put("paginateLastLabel", modelForm.getPaginateLastLabel(context));
        parameters.put("paginateNextStyle", modelForm.getPaginateNextStyle());
        parameters.put("nextLinkUrl", nextLinkUrl);
        parameters.put("paginateNextLabel", modelForm.getPaginateNextLabel(context));
        parameters.put("paginatePreviousStyle", modelForm.getPaginatePreviousStyle());
        parameters.put("paginatePreviousLabel", modelForm.getPaginatePreviousLabel(context));
        parameters.put("previousLinkUrl", previousLinkUrl);
        parameters.put("paginateFirstStyle", modelForm.getPaginateFirstStyle());
        parameters.put("paginateFirstLabel", modelForm.getPaginateFirstLabel(context));
        parameters.put("firstLinkUrl", firstLinkUrl);
        executeMacro(writer, "renderScreenletPaginateMenu", parameters);
    }

    public void renderPortalPageBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage) throws GeneralException, IOException {
        String portalPageId = portalPage.getActualPortalPageId(context);
        String originalPortalPageId = portalPage.getOriginalPortalPageId(context);
        String confMode = portalPage.getConfMode(context);

        Map<String, String> uiLabelMap = UtilGenerics.cast(context.get("uiLabelMap"));
        String addColumnLabel = "";
        String addColumnHint = "";
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        } else {
            addColumnLabel = uiLabelMap.get("CommonAddColumn");
            addColumnHint = uiLabelMap.get("CommonAddAColumnToThisPortalPage");
        }

        Integer columnCount = (Integer) context.get("portletColumnCount");
        if (columnCount == null) {
            columnCount = 1;
        }
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPageBegin ");
        sr.append("originalPortalPageId=\"");
        sr.append(originalPortalPageId);
        sr.append("\" portalPageId=\"");
        sr.append(portalPageId);
        sr.append("\" confMode=\"");
        sr.append(confMode);
        sr.append("\" addColumnLabel=\"");
        sr.append(addColumnLabel);
        sr.append("\" addColumnHint=\"");
        sr.append(addColumnHint);
        sr.append("\" columnCount=");
        sr.append(columnCount.toString());
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }

    public void renderPortalPageEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage) throws GeneralException, IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPageEnd/>");
        executeMacro(writer, sr.toString());
    }

    public void renderPortalPageColumnBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPageColumn) throws GeneralException, IOException {
        String portalPageId = portalPage.getActualPortalPageId(context);
        String originalPortalPageId = portalPage.getOriginalPortalPageId(context);
        String columnSeqId = portalPageColumn.getString("columnSeqId");
        String columnWidthPercentage = portalPageColumn.getString("columnWidthPercentage");
        String columnWidthPixels = portalPageColumn.getString("columnWidthPixels");
        String confMode = portalPage.getConfMode(context);

        Map<String, String> uiLabelMap = UtilGenerics.cast(context.get("uiLabelMap"));
        String delColumnLabel = "";
        String delColumnHint = "";
        String addPortletLabel = "";
        String addPortletHint = "";
        String colWidthLabel = "";
        String setColumnSizeHint = "";
        
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        } else {
            delColumnLabel = uiLabelMap.get("CommonDeleteColumn");
            delColumnHint = uiLabelMap.get("CommonDeleteThisColumn");

            addPortletLabel = uiLabelMap.get("CommonAddAPortlet");
            addPortletHint = uiLabelMap.get("CommonAddPortletToPage");
            colWidthLabel = uiLabelMap.get("CommonWidth");
            setColumnSizeHint = uiLabelMap.get("CommonSetColumnWidth");
        }

        Integer columnCount = (Integer) context.get("portletColumnCount");
        if (columnCount == null) {
            columnCount = 1;
        }
        
        Integer columnIndex = (Integer) context.get("portletColumnIndex");
        if (columnIndex == null) {
            columnIndex = 0;
        }
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPageColumnBegin ");
        sr.append("originalPortalPageId=\"");
        sr.append(originalPortalPageId);
        sr.append("\" portalPageId=\"");
        sr.append(portalPageId);
        sr.append("\" columnSeqId=\"");
        sr.append(columnSeqId);
        sr.append("\" ");
        if (UtilValidate.isNotEmpty(columnWidthPixels)) {
            sr.append("width=\"");
            sr.append(columnWidthPixels);
            sr.append("px\"");
        } else if (UtilValidate.isNotEmpty(columnWidthPercentage)) {
            sr.append("width=\"");
            sr.append(columnWidthPercentage);
            sr.append("%\"");
        }
        sr.append(" confMode=\"");
        sr.append(confMode);
        sr.append("\" delColumnLabel=\"");
        sr.append(delColumnLabel);
        sr.append("\" delColumnHint=\"");
        sr.append(delColumnHint);
        sr.append("\" addPortletLabel=\"");
        sr.append(addPortletLabel);
        sr.append("\" addPortletHint=\"");
        sr.append(addPortletHint);
        sr.append("\" colWidthLabel=\"");
        sr.append(colWidthLabel);
        sr.append("\" setColumnSizeHint=\"");
        sr.append(setColumnSizeHint);
        sr.append("\" columnCount=");
        sr.append(columnCount.toString());
        sr.append(" columnIndex=");
        sr.append(columnIndex.toString());
        sr.append(" />");
        executeMacro(writer, sr.toString());
    }   

    public void renderPortalPageColumnEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPageColumn) throws GeneralException, IOException {
        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPageColumnEnd/>");
        executeMacro(writer, sr.toString());
    }

    public void renderPortalPagePortletBegin(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException {
        String portalPageId = portalPage.getActualPortalPageId(context);
        String originalPortalPageId = portalPage.getOriginalPortalPageId(context);
        String portalPortletId = portalPortlet.getString("portalPortletId");
        String portletSeqId = portalPortlet.getString("portletSeqId");
        String columnSeqId = portalPortlet.getString("columnSeqId");
        String confMode = portalPage.getConfMode(context);
        String editFormName = portalPortlet.getString("editFormName");
        String editFormLocation = portalPortlet.getString("editFormLocation");

        String prevPortletId = (String) context.get("prevPortletId");
        String prevPortletSeqId = (String) context.get("prevPortletSeqId");
        String nextPortletId = (String) context.get("nextPortletId");
        String nextPortletSeqId = (String) context.get("nextPortletSeqId");
        String prevColumnSeqId = (String) context.get("prevColumnSeqId");
        String nextColumnSeqId = (String) context.get("nextColumnSeqId");

        String columnWidthPercentage = (String) context.get("columnWidthPercentage");
        String columnWidthPixels = (String) context.get("columnWidthPixels");
        
        Map<String, String> uiLabelMap = UtilGenerics.cast(context.get("uiLabelMap"));
        String delPortletHint = "";
        String editAttributeHint = "";
        if (uiLabelMap == null) {
            Debug.logWarning("Could not find uiLabelMap in context", module);
        } else {
            delPortletHint = uiLabelMap.get("CommonDeleteThisPortlet");
            editAttributeHint = uiLabelMap.get("CommonEditPortletAttributes");
        }

        Integer columnCount = (Integer) context.get("portletColumnCount");
        if (columnCount == null) {
            columnCount = 1;
        }
        
        Integer columnIndex = (Integer) context.get("portletColumnIndex");
        if (columnIndex == null) {
            columnIndex = 0;
        }
        
        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPagePortletBegin ");
        sr.append("originalPortalPageId=\"");
        sr.append(originalPortalPageId);
        sr.append("\" portalPageId=\"");
        sr.append(portalPageId);
        sr.append("\" portalPortletId=\"");
        sr.append(portalPortletId);
        sr.append("\" portletSeqId=\"");
        sr.append(portletSeqId);
        sr.append("\" prevPortletId=\"");
        sr.append(prevPortletId);
        sr.append("\" prevPortletSeqId=\"");
        sr.append(prevPortletSeqId);
        sr.append("\" nextPortletId=\"");
        sr.append(nextPortletId);
        sr.append("\" nextPortletSeqId=\"");
        sr.append(nextPortletSeqId);
        sr.append("\" columnSeqId=\"");
        sr.append(columnSeqId);
        sr.append("\" prevColumnSeqId=\"");
        sr.append(prevColumnSeqId);
        sr.append("\" nextColumnSeqId=\"");
        sr.append(nextColumnSeqId);
        sr.append("\" delPortletHint=\"");
        sr.append(delPortletHint);
        sr.append("\" editAttributeHint=\"");
        sr.append(editAttributeHint);
        sr.append("\" confMode=\"");
        sr.append(confMode);
        sr.append("\" columnCount=");
        sr.append(columnCount.toString());
        sr.append(" columnIndex=");
        sr.append(columnIndex.toString());
        sr.append("");
        if (UtilValidate.isNotEmpty(columnWidthPixels)) {
            sr.append(" width=\"");
            sr.append(columnWidthPixels);
            sr.append("px\"");
        } else if (UtilValidate.isNotEmpty(columnWidthPercentage)) {
            sr.append(" width=\"");
            sr.append(columnWidthPercentage);
            sr.append("%\"");
        }
        if (UtilValidate.isNotEmpty(editFormName) && UtilValidate.isNotEmpty(editFormLocation)) {
            sr.append(" editAttribute=\"true\"");
        }
        sr.append("/>");
        executeMacro(writer, sr.toString());
    }

    public void renderPortalPagePortletEnd(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException {
        String confMode = portalPage.getConfMode(context);

        StringWriter sr = new StringWriter();
        sr.append("<@renderPortalPagePortletEnd ");
        sr.append(" confMode=\"");
        sr.append(confMode);
        sr.append("\" />");
        executeMacro(writer, sr.toString());
    }

    public void renderPortalPagePortletBody(Appendable writer, Map<String, Object> context, ModelScreenWidget.PortalPage portalPage, GenericValue portalPortlet) throws GeneralException, IOException {
        String portalPortletId = portalPortlet.getString("portalPortletId");
        String screenName = portalPortlet.getString("screenName");
        String screenLocation = portalPortlet.getString("screenLocation");

        ModelScreen modelScreen = null;
        if (UtilValidate.isNotEmpty(screenName) && UtilValidate.isNotEmpty(screenLocation)) {
            try {
                modelScreen = ScreenFactory.getScreenFromLocation(screenLocation, screenName);
            } catch (IOException e) {
                String errMsg = "Error rendering portlet ID [" + portalPortletId + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (SAXException e) {
                String errMsg = "Error rendering portlet ID [" + portalPortletId + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (ParserConfigurationException e) {
                String errMsg = "Error rendering portlet ID [" + portalPortletId + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }
        modelScreen.renderScreenString(writer, context, this);
    }

    @Override
    public void renderColumnContainer(Appendable writer, Map<String, Object> context, ColumnContainer columnContainer) throws IOException {
        String id = columnContainer.getId(context);
        String style = columnContainer.getStyle(context);
        StringBuilder sb = new StringBuilder("<@renderColumnContainerBegin");
        sb.append(" id=\"");
        sb.append(id);
        sb.append("\" style=\"");
        sb.append(style);
        sb.append("\" />");
        executeMacro(writer, sb.toString());
        for (Column column : columnContainer.getColumns()) {
            id = column.getId(context);
            style = column.getStyle(context);
            sb = new StringBuilder("<@renderColumnBegin");
            sb.append(" id=\"");
            sb.append(id);
            sb.append("\" style=\"");
            sb.append(style);
            sb.append("\" />");
            executeMacro(writer, sb.toString());
            for (ModelScreenWidget subWidget : column.getSubWidgets()) {
                try {
                    subWidget.renderWidgetString(writer, context, this);
                } catch (GeneralException e) {
                    throw new IOException(e);
                }
            }
            executeMacro(writer, "<@renderColumnEnd />");
        }
        executeMacro(writer, "<@renderColumnContainerEnd />");
    }
}
