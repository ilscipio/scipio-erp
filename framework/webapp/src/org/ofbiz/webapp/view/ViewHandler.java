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
package org.ofbiz.webapp.view;

import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.RequestHandler;

import javax.annotation.Nonnull;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.Writer;

/**
 * ViewHandler - View Handler Interface
 */
public interface ViewHandler {

    /**
     * Sets the name of the view handler as declared in the controller configuration file.
     * @param name String The name of the view handler as declared in the controller configuration file.
     */
    public void setName(String name);

    /**
     * Gets the name of the view handler as declared in the controller configuration file.
     * @return name String The name of the view handler as declared in the controller configuration file.
     */
    public String getName();

    /**
     * Initializes the handler. Since handlers use the singleton pattern this method should only be called
     * the first time the handler is used.
     *
     * @param context ServletContext This may be needed by the handler in order to lookup properties or XML
     * definition files for rendering pages or handler options.
     * @throws ViewHandlerException
     */
    public void init(ServletContext context) throws ViewHandlerException;

    /**
     * Render the page to the specified writer (preferred overload).
     * <p>SCIPIO: 2.1.0: Added overload with arbitrary extra parameters.</p>
     *
     * @param vrctx The view render context.
     * @throws ViewHandlerException
     */
    public default void render(ViewRenderContext vrctx) throws ViewHandlerException {
        render(vrctx.name(), vrctx.page(), vrctx.info(), vrctx.contentType(), vrctx.encoding(), vrctx.request(), vrctx.response());
    }

    /**
     * Render the page (legacy overload).
     *
     * @param name The name of the view.
     * @param page The source of the view; could be a page, url, etc depending on the type of handler.
     * @param info An info string attached to this view
     * @param request The HttpServletRequest object used when requesting this page.
     * @param response The HttpServletResponse object to be used to present the page.
     * @throws ViewHandlerException
     */
    public default void render(String name, String page, String info, String contentType, String encoding, HttpServletRequest request, HttpServletResponse response) throws ViewHandlerException {
        render(new ViewRenderContext(name, page, info, contentType, encoding, request, response, null, null, null, null, null));
    }

    /**
     * Render the page to the specified writer (legacy Scipio overload).
     * <p>SCIPIO: 2017-05-01: Added.</p>
     * <p>SCIPIO: 2.1.0: Integrated into base interface.</p>
     *
     * @param name The name of the view.
     * @param page The source of the view; could be a page, url, etc depending on the type of handler.
     * @param info An info string attached to this view
     * @param request The HttpServletRequest object used when requesting this page.
     * @param response The HttpServletResponse object to be used to present the page.
     * @param writer the writer to print to (must NOT use response object)
     * @throws ViewHandlerException
     */
    public default void render(String name, String page, String info, String contentType, String encoding, HttpServletRequest request, HttpServletResponse response, Writer writer) throws ViewHandlerException {
        render(new ViewRenderContext(name, page, info, contentType, encoding, request, response, writer, null, null, null, null));
    }

    class ViewRenderContext {
        protected String name;
        protected String page;
        protected String info;
        protected String contentType;
        protected String encoding;
        protected HttpServletRequest request;
        protected HttpServletResponse response;
        protected Writer writer;
        protected ConfigXMLReader.ControllerConfig controllerConfig;
        protected ConfigXMLReader.RequestMap requestMap;
        protected ConfigXMLReader.ViewMap viewMap;
        protected RequestHandler requestHandler;

        public ViewRenderContext(String name, String page, String info, String contentType, String encoding,
                                 HttpServletRequest request, HttpServletResponse response, Writer writer,
                                 ConfigXMLReader.ControllerConfig controllerConfig, ConfigXMLReader.RequestMap requestMap,
                                 ConfigXMLReader.ViewMap viewMap, RequestHandler requestHandler) {
            this.name = name;
            this.page = page;
            this.info = info;
            this.contentType = contentType;
            this.encoding = encoding;
            this.request = request;
            this.response = response;
            this.writer = writer;
            this.controllerConfig = controllerConfig;
            this.requestMap = requestMap;
            this.viewMap = viewMap;
            this.requestHandler = requestHandler;
        }

        public String name() {
            return name;
        }

        public ViewRenderContext name(String name) {
            this.name = name;
            return this;
        }

        public String page() {
            return page;
        }

        public ViewRenderContext page(String page) {
            this.page = page;
            return this;
        }

        public String info() {
            return info;
        }

        public ViewRenderContext info(String info) {
            this.info = info;
            return this;
        }

        public String contentType() {
            return contentType;
        }

        public ViewRenderContext contentType(String contentType) {
            this.contentType = contentType;
            return this;
        }

        public String encoding() {
            return encoding;
        }

        public ViewRenderContext encoding(String encoding) {
            this.encoding = encoding;
            return this;
        }

        public HttpServletRequest request() {
            return request;
        }

        public ViewRenderContext request(HttpServletRequest request) {
            this.request = request;
            return this;
        }

        public HttpServletResponse response() {
            return response;
        }

        public ViewRenderContext response(HttpServletResponse response) {
            this.response = response;
            return this;
        }

        public Writer writer() {
            return writer;
        }

        public ViewRenderContext writer(Writer writer) {
            this.writer = writer;
            return this;
        }

        @Nonnull
        public Writer renderWriter() throws ViewHandlerException {
            Writer writer = this.writer;
            if (writer == null) {
                try {
                    writer = response().getWriter();
                } catch (IOException e) {
                    throw new ViewHandlerException("Error getting the response writer/output stream: " + e, e);
                }
                this.writer = writer;
            }
            return writer;
        }

        public ConfigXMLReader.ControllerConfig controllerConfig() {
            return controllerConfig;
        }

        public ViewRenderContext controllerConfig(ConfigXMLReader.ControllerConfig controllerConfig) {
            this.controllerConfig = controllerConfig;
            return this;
        }

        public ConfigXMLReader.RequestMap requestMap() {
            return requestMap;
        }

        public ViewRenderContext requestMap(ConfigXMLReader.RequestMap requestMap) {
            this.requestMap = requestMap;
            return this;
        }

        public ConfigXMLReader.ViewMap viewMap() {
            return viewMap;
        }

        public ViewRenderContext viewMap(ConfigXMLReader.ViewMap viewMap) {
            this.viewMap = viewMap;
            return this;
        }

        public RequestHandler requestHandler() {
            return requestHandler;
        }

        public ViewRenderContext requestHandler(RequestHandler requestHandler) {
            this.requestHandler = requestHandler;
            return this;
        }
    }
}
