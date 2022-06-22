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
import java.io.Writer;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestHandlerException;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.event.EventHandlerException;
import org.ofbiz.webapp.view.AbstractViewHandler;
import org.ofbiz.webapp.view.ViewHandler;
import org.ofbiz.webapp.view.ViewHandlerException;
import org.ofbiz.widget.renderer.FormStringRenderer;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.TreeStringRenderer;
import org.ofbiz.widget.renderer.VisualThemeWorker;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr;
import org.xml.sax.SAXException;

import freemarker.template.TemplateException;
import freemarker.template.utility.StandardCompress;

public class MacroScreenViewHandler extends AbstractViewHandler implements ViewHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected ServletContext servletContext = null;

    public void init(ServletContext context) throws ViewHandlerException {
        this.servletContext = context;
    }

    /**
     * SCIPIO: Reusable version of the original ofbiz loadRenderers method.
     */
    public static ScreenStringRenderer loadRenderers(HttpServletRequest request, HttpServletResponse response,
            String name, Map<String, Object> context, Writer writer) throws GeneralException, TemplateException, IOException {
        // SCIPIO: need this name early, check if html
        String screenRendererName = UtilProperties.getPropertyValue("widget", name + ".name");

        String screenMacroLibraryPath = UtilProperties.getPropertyValue("widget", name + ".screenrenderer");
        String formMacroLibraryPath = UtilProperties.getPropertyValue("widget", name + ".formrenderer");
        String treeMacroLibraryPath = UtilProperties.getPropertyValue("widget", name + ".treerenderer");
        String menuMacroLibraryPath = UtilProperties.getPropertyValue("widget", name + ".menurenderer");

        Map<String, List<String>> themeResources = VisualThemeWorker.getVisualThemeResources(context);
        if (themeResources != null) {
            // SCIPIO: all these lookups modified to go through platform and expression checks
            String macroLibraryPath;

            macroLibraryPath = VisualThemeWorker.getMacroLibraryLocationStaticFromResources(screenRendererName, themeResources, "VT_SCRN_MACRO_LIB");
            if (macroLibraryPath != null) {
                screenMacroLibraryPath = macroLibraryPath;
            }

            macroLibraryPath = VisualThemeWorker.getMacroLibraryLocationStaticFromResources(screenRendererName, themeResources, "VT_FORM_MACRO_LIB");
            if (macroLibraryPath != null) {
                formMacroLibraryPath = macroLibraryPath;
            }

            macroLibraryPath = VisualThemeWorker.getMacroLibraryLocationStaticFromResources(screenRendererName, themeResources, "VT_TREE_MACRO_LIB");
            if (macroLibraryPath != null) {
                treeMacroLibraryPath = macroLibraryPath;
            }

            macroLibraryPath = VisualThemeWorker.getMacroLibraryLocationStaticFromResources(screenRendererName, themeResources, "VT_MENU_MACRO_LIB");
            if (macroLibraryPath != null) {
                menuMacroLibraryPath = macroLibraryPath;
            }
        }

        // SCIPIO: 2016-09-15: in addition, dump the renderers into the request attributes,
        // for some cases where only request is available
        ScreenStringRenderer screenStringRenderer = new MacroScreenRenderer(screenRendererName, screenMacroLibraryPath);
        if (UtilValidate.isNotEmpty(formMacroLibraryPath)) {
            FormStringRenderer formStringRenderer = new MacroFormRenderer(screenRendererName, formMacroLibraryPath, request, response);
            context.put("formStringRenderer", formStringRenderer);
            request.setAttribute("formStringRenderer", formStringRenderer);
        }
        if (UtilValidate.isNotEmpty(treeMacroLibraryPath)) {
            @SuppressWarnings("deprecation")
            TreeStringRenderer treeStringRenderer = new MacroTreeRenderer(screenRendererName, treeMacroLibraryPath, writer);
            context.put("treeStringRenderer", treeStringRenderer);
            request.setAttribute("treeStringRenderer", treeStringRenderer);
        }
        if (UtilValidate.isNotEmpty(menuMacroLibraryPath)) {
            MenuStringRenderer menuStringRenderer = new MacroMenuRenderer(screenRendererName, menuMacroLibraryPath, request, response);
            context.put("menuStringRenderer", menuStringRenderer);
            request.setAttribute("menuStringRenderer", menuStringRenderer);
        }
        return screenStringRenderer;
    }

    private ScreenStringRenderer loadRenderers(HttpServletRequest request, HttpServletResponse response,
            Map<String, Object> context, Writer writer) throws GeneralException, TemplateException, IOException {
        return loadRenderers(request, response, getName(), context, writer);
    }

    // SCIPIO: 2017-05-01: factored out Writer for reuse
    public void render(ViewRenderContext vrctx) throws ViewHandlerException {
        // SCIPIO: 2.1.0: pre-screen-render event
        if (vrctx.controllerConfig() != null) {
            try {
                RequestHandler.runEvents(vrctx, "pre-screen-render", vrctx.controllerConfig().getPreScreenRenderEventList(), false);
            } catch (GeneralException e) {
                Debug.logError(e, "Exception thrown reading/running pre-screen-render events: ", module);
                throw new ViewHandlerException(e);
            }
        }

        String name = vrctx.name();
        String page = vrctx.page();
        String info = vrctx.info();
        String contentType = vrctx.contentType();
        String encoding = vrctx.encoding();
        HttpServletRequest request = vrctx.request();
        HttpServletResponse response = vrctx.response();
        Writer writer = vrctx.writer();

        try {
            Delegator delegator = (Delegator) request.getAttribute("delegator");
            // compress output if configured to do so
            if (UtilValidate.isEmpty(encoding)) {
                encoding = EntityUtilProperties.getPropertyValue("widget", getName() + ".default.encoding", "none", delegator);
            }
            boolean compressOutput = "compressed".equals(encoding);
            if (!compressOutput) {
                compressOutput = "true".equals(EntityUtilProperties.getPropertyValue("widget", getName() + ".compress", delegator));
            }
            if (!compressOutput && this.servletContext != null) {
                compressOutput = "true".equals(this.servletContext.getAttribute("compressHTML"));
            }
            if (compressOutput) {
                // StandardCompress defaults to a 2k buffer. That could be increased
                // to speed up output.
                writer = new StandardCompress().getWriter(writer, null);
            }
            MapStack<String> context = RenderMapStack.createRenderContext(); // SCIPIO: Dedicated context class: MapStack.create()
            ScreenRenderer.populateContextForRequest(context, null, request, response, servletContext);
            try { // SCIPIO: Added try/finally block
                // SCIPIO: 2017-05-09: targeted rendering prep. NOTE: populateContextForRequest call set up the RenderTargetState object.
                writer = WidgetRenderTargetExpr.getRenderTargetState(context).prepareWriter(writer, context);

                ScreenStringRenderer screenStringRenderer = loadRenderers(request, response, context, writer);
                ScreenRenderer screens = ScreenRenderer.makeWithEnvAwareFetching(writer, context, screenStringRenderer);
                context.put("screens", screens);
                // SCIPIO: 2016-09-15: in addition, dump the screens renderer into the request attributes,
                // for some cases where only request is available
                request.setAttribute("screens", screens);
                // SCIPIO: new early encoder
                UtilCodec.SimpleEncoder simpleEncoder = UtilCodec.getEncoder(UtilProperties.getPropertyValue("widget", getName() + ".encoder"));
                context.put("simpleEncoder", simpleEncoder);
                UtilCodec.SimpleEncoder simpleEarlyEncoder = UtilCodec.getEncoder(UtilProperties.getPropertyValue("widget", getName() + ".earlyEncoder"));
                context.put("simpleEarlyEncoder", (simpleEarlyEncoder != null) ? simpleEarlyEncoder : simpleEncoder);
                screenStringRenderer.renderScreenBegin(writer, context);
                screens.render(page);
                screenStringRenderer.renderScreenEnd(writer, context);
                writer.flush();
            } finally {
                context.pop(); // SCIPIO: Added pop()
                if (context.stackSize() > 1) { // SCIPIO
                    Debug.logWarning("Unmatched push() calls at render end: stack size (" + context.stackSize()
                            + ") greater than expected (1)", module);
                }
            }
        } catch (TemplateException e) {
            Debug.logError(e, "Error initializing screen renderer", module);
            throw new ViewHandlerException(e.getMessage());
        } catch (IOException e) {
            throw new ViewHandlerException("Error in the response writer/output stream: " + e.toString(), e);
        } catch (SAXException | ParserConfigurationException e) {
            throw new ViewHandlerException("XML Error rendering page: " + e.toString(), e);
        } catch (GeneralException e) {
            throw new ViewHandlerException("Lower level error rendering page: " + e.toString(), e);
        }

        // SCIPIO: 2.1.0: post-screen-render event
        if (vrctx.controllerConfig() != null) {
            try {
                RequestHandler.runEvents(vrctx, "post-screen-render", vrctx.controllerConfig().getPostScreenRenderEventList(), false);
            } catch (GeneralException e) {
                Debug.logError(e, "Exception thrown reading/running post-screen-render events: ", module);
                throw new ViewHandlerException(e);
            }
        }
    }

    public static void runPreScreenRenderEvents(ViewRenderContext vrctx) throws ViewHandlerException {
        HttpServletRequest request = vrctx.request();
        HttpServletResponse response = vrctx.response();
        ConfigXMLReader.ControllerConfig controllerConfig = vrctx.controllerConfig();
        ConfigXMLReader.RequestMap requestMap = vrctx.requestMap();
        RequestHandler requestHandler = vrctx.requestHandler();
        if (requestHandler == null) {
            return;
        }

        // SCIPIO: 2.1.0: pre-screen-render event
        try {
            for (ConfigXMLReader.Event event: controllerConfig.getPreScreenRenderEventList().values()) {
                try {
                    String returnString = requestHandler.runEvent(request, response, event, requestMap, "pre-screen-render");
                    if (returnString != null && !"success".equalsIgnoreCase(returnString)) {
                        throw new EventHandlerException("Pre-Screen-Render event did not return 'success'.");
                    }
                } catch (EventHandlerException e) {
                    Debug.logError(e, module);
                }
            }
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            throw new ViewHandlerException(e);
        }
    }

    public static void runPostScreenRenderEvents(ViewRenderContext vrctx) throws ViewHandlerException {
        HttpServletRequest request = vrctx.request();
        HttpServletResponse response = vrctx.response();
        ConfigXMLReader.ControllerConfig controllerConfig = vrctx.controllerConfig();
        ConfigXMLReader.RequestMap requestMap = vrctx.requestMap();
        RequestHandler requestHandler = vrctx.requestHandler();
        if (requestHandler == null) {
            return;
        }

        // SCIPIO: 2.1.0: post-screen-render event
        try {
            for (ConfigXMLReader.Event event: controllerConfig.getPostScreenRenderEventList().values()) {
                try {
                    String returnString = requestHandler.runEvent(request, response, event, requestMap, "post-screen-render");
                    if (returnString != null && !"success".equalsIgnoreCase(returnString)) {
                        throw new EventHandlerException("Post-Screen-Render event did not return 'success'.");
                    }
                } catch (EventHandlerException e) {
                    Debug.logError(e, module);
                }
            }
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            throw new ViewHandlerException(e);
        }
    }






}
