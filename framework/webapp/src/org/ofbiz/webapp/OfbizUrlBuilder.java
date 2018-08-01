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
package org.ofbiz.webapp;

import java.io.IOException;
import java.net.URL;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.xml.sax.SAXException;

/**
 * OFBiz URL builder.
 * <p>
 * SCIPIO: Some noteworthy changes:
 * <ul>
 * <li>Controller is now optional (no exceptions for webapps with no controller) (added 2017-11-18).</li>
 * </ul>
 */
public final class OfbizUrlBuilder {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Returns an <code>OfbizUrlBuilder</code> instance.
     * 
     * @param request
     * @throws GenericEntityException
     * @throws WebAppConfigurationException
     */
    public static OfbizUrlBuilder from(HttpServletRequest request) throws GenericEntityException, WebAppConfigurationException {
        Assert.notNull("request", request);
        OfbizUrlBuilder builder = (OfbizUrlBuilder) request.getAttribute("_OFBIZ_URL_BUILDER_");
        if (builder == null) {
            WebSiteProperties webSiteProps = WebSiteProperties.from(request);
            URL url = ConfigXMLReader.getControllerConfigURL(request.getServletContext());
            ControllerConfig config = (url != null) ? ConfigXMLReader.getControllerConfig(url, true) : null; // SCIPIO: 2017-11-18: controller now fully optional (2 change)
            String servletPath = (String) request.getAttribute("_CONTROL_PATH_");
            String contextPath = request.getContextPath();
            builder = new OfbizUrlBuilder(config, webSiteProps, servletPath, contextPath);
            request.setAttribute("_OFBIZ_URL_BUILDER_", builder);
        }
        return builder;
    }

    /**
     * Returns an <code>OfbizUrlBuilder</code> instance. Use this method when you
     * don't have a <code>HttpServletRequest</code> object - like in scheduled jobs.
     * 
     * @param webAppInfo Optional - if <code>null</code>, the builder can only build the host part,
     * and that will be based only on the settings in <code>url.properties</code> (the WebSite
     * entity will be ignored).
     * @param delegator
     * @throws WebAppConfigurationException
     * @throws IOException
     * @throws SAXException
     * @throws GenericEntityException
     */
    public static OfbizUrlBuilder from(WebappInfo webAppInfo, Delegator delegator) throws WebAppConfigurationException, IOException, SAXException, GenericEntityException {
        WebSiteProperties webSiteProps = null;
        ControllerConfig config = null;
        String servletPath = null;
        String contextPath = null;
        if (webAppInfo != null) {
            Assert.notNull("delegator", delegator);
            String webSiteId = WebAppUtil.getWebSiteId(webAppInfo);
            if (webSiteId != null) {
                GenericValue webSiteValue = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
                if (webSiteValue != null) {
                    webSiteProps = WebSiteProperties.from(webSiteValue);
                }
            }
            config = ConfigXMLReader.getControllerConfig(webAppInfo, true); // SCIPIO: 2017-11-18: controller now optional
            servletPath = WebAppUtil.getControlServletPath(webAppInfo, true); // SCIPIO: 2017-11-18: ControlServlet now optional
            contextPath = webAppInfo.getContextRoot();
        }
        if (webSiteProps == null) {
            webSiteProps = WebSiteProperties.defaults(delegator);
        }
        return new OfbizUrlBuilder(config, webSiteProps, servletPath, contextPath);
    }
    
    /**
     * SCIPIO: Returns an <code>OfbizUrlBuilder</code> instance. Mixed method that allows
     * using WebSiteProperties different than the WebappInfo instance.
     * <p>
     * This is needed because not every webapp has its own webSiteId, which means
     * another source for WebSiteProperties must be used in its place.
     * 
     * @param webAppInfo Optional - if <code>null</code>, the builder can only build the host part,
     * and that will be based only on the settings in <code>url.properties</code> (the WebSite
     * entity will be ignored).
     * @param delegator
     * @throws WebAppConfigurationException
     * @throws IOException
     * @throws SAXException
     * @throws GenericEntityException
     */
    public static OfbizUrlBuilder from(WebappInfo webAppInfo, WebSiteProperties webSiteProps, Delegator delegator) throws WebAppConfigurationException, IOException, SAXException, GenericEntityException {
        ControllerConfig config = null;
        String servletPath = null;
        String contextPath = null;
        if (webAppInfo != null) {
            Assert.notNull("delegator", delegator);
            config = ConfigXMLReader.getControllerConfig(webAppInfo, true); // SCIPIO: 2017-11-18: controller now optional
            servletPath = WebAppUtil.getControlServletPath(webAppInfo, true); // SCIPIO: 2017-11-18: ControlServlet now optional
            contextPath = webAppInfo.getContextRoot();
        }
        if (webSiteProps == null) {
            webSiteProps = WebSiteProperties.defaults(delegator);
        }
        return new OfbizUrlBuilder(config, webSiteProps, servletPath, contextPath);
    }
    
    /**
     * SCIPIO: Returns an <code>OfbizUrlBuilder</code> instance using the given webSiteId.
     * Added 2017-11.
     * 
     * @param webSiteId Optional - if <code>null</code>, the builder can only build the host part,
     * and that will be based only on the settings in <code>url.properties</code> (the WebSite
     * entity will be ignored).
     * @param delegator
     * @throws WebAppConfigurationException
     * @throws IOException
     * @throws SAXException
     * @throws GenericEntityException
     */
    public static OfbizUrlBuilder fromWebSiteId(String webSiteId, Delegator delegator) throws WebAppConfigurationException, 
        IOException, SAXException, GenericEntityException, IllegalArgumentException {
        WebappInfo webAppInfo = null;
        WebSiteProperties webSiteProps = null;
        if (webSiteId != null && !webSiteId.isEmpty()) {
            webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            webSiteProps = WebSiteProperties.from(delegator, webSiteId);
        }
        if (webSiteProps == null) {
            webSiteProps = WebSiteProperties.defaults(delegator);
        }
        return from(webAppInfo, webSiteProps, delegator);
    }

    private final ControllerConfig config;
    private final WebSiteProperties webSiteProps;
    private final String servletPath;
    private final String contextPath;   // SCIPIO: this class should record the context path (webapp mount-point)

    private OfbizUrlBuilder(ControllerConfig config, WebSiteProperties webSiteProps, String servletPath, String contextPath) {
        this.config = config;
        this.webSiteProps = webSiteProps;
        this.servletPath = servletPath;
        this.contextPath = contextPath;
    }

    /**
     * Builds a full URL - including scheme, host, servlet path and resource.
     * 
     * @param buffer
     * @param url
     * @param useSSL Default value to use - will be replaced by request-map setting
     * if one is found.
     * @return <code>true</code> if the URL uses https
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public boolean buildFullUrl(Appendable buffer, String url, boolean useSSL) throws WebAppConfigurationException, IOException {
        boolean makeSecure = buildHostPart(buffer, url, useSSL);
        buildPathPart(buffer, url);
        return makeSecure;
    }

    /**
     * SCIPIO: Builds a full URL - including scheme, host, context root and resource (custom servlet).
     * Added 2018-08-01.
     */
    public boolean buildFullUrlWithContextRoot(Appendable buffer, String url, boolean useSSL) throws WebAppConfigurationException, IOException {
        boolean makeSecure = buildHostPart(buffer, url, useSSL);
        buildPathPartWithContextRoot(buffer, url);
        return makeSecure;
    }

    /**
     * SCIPIO: Builds a full URL - including scheme, host, webapp path prefix and resource (custom context root).
     * Added 2018-08-01.
     */
    public boolean buildFullUrlWithWebappPathPrefix(Appendable buffer, String url, boolean useSSL) throws WebAppConfigurationException, IOException {
        boolean makeSecure = buildHostPart(buffer, url, useSSL);
        buildPathPartWithWebappPathPrefix(buffer, url);
        return makeSecure;
    }

    /**
     * Builds a partial URL - including the scheme and host, but not the servlet path or resource.
     * <p>
     * SCIPIO: Modified to support omitting controller lookup. Also supports Boolean instead of boolean.
     * 
     * @param buffer
     * @param url
     * @param useSSL Default value to use - will be replaced by request-map setting
     * if one is found with security=true set.
     * @param controller
     * @return <code>true</code> if the URL uses https
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public boolean buildHostPart(Appendable buffer, String url, Boolean useSSL, Boolean controller) throws WebAppConfigurationException, IOException {
        // SCIPIO: support Boolean
        useSSL = Boolean.TRUE.equals(useSSL); // default false
        controller = !Boolean.FALSE.equals(controller); // default true // SCIPIO: re-fixed 2017-11-17
        
        boolean makeSecure = useSSL;
        String requestMapUri = null;
        if (UtilValidate.isNotEmpty(url)) { // SCIPIO: added null check
            String[] pathElements = url.split("/");
            requestMapUri = pathElements[0];
            int queryIndex = requestMapUri.indexOf("?");
            if (queryIndex != -1) {
                requestMapUri = requestMapUri.substring(0, queryIndex);
            }
        }
        RequestMap requestMap = null;
        // SCIPIO: only lookup if controller lookup requested
        if (controller) {
            if (config != null) {
                requestMap = config.getRequestMapMap().get(requestMapUri);
            }
        }
        if (!makeSecure && requestMap != null) { // if the request has security="true" then use it
            makeSecure = requestMap.securityHttps;
        }
        makeSecure = webSiteProps.getEnableHttps() & makeSecure;
        if (makeSecure) {
            String server = webSiteProps.getHttpsHost();
            if (server.isEmpty()) {
                server = "localhost";
            }
            buffer.append("https://");
            buffer.append(server);
            if (!webSiteProps.getHttpsPort().isEmpty()) {
                // SCIPIO: only append port if it's not default, otherwise not required and ugly
                if (!"443".equals(webSiteProps.getHttpsPort())) {
                    buffer.append(":").append(webSiteProps.getHttpsPort());
                }
            }
        } else {
            String server = webSiteProps.getHttpHost();
            if (server.isEmpty()) {
                server = "localhost";
            }
            buffer.append("http://");
            buffer.append(server);
            if (!webSiteProps.getHttpPort().isEmpty()) {
                // SCIPIO: only append port if it's not default, otherwise not required and ugly
                if (!"80".equals(webSiteProps.getHttpsPort())) {
                    buffer.append(":").append(webSiteProps.getHttpPort());
                }
            }
        }
        return makeSecure;
    }
    
    /**
     * Builds a partial URL - including the scheme and host, but not the servlet path or resource.
     * <p>
     * SCIPIO: Version that assumes controller is to be used. Also accepts Boolean instead of boolean.
     * 
     * @param buffer
     * @param url
     * @param useSSL Default value to use - will be replaced by request-map setting
     * if one is found with security=true set.
     * @return <code>true</code> if the URL uses https
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public boolean buildHostPart(Appendable buffer, String url, Boolean useSSL) throws WebAppConfigurationException, IOException {
        return buildHostPart(buffer, url, useSSL, true);
    }

    /**
     * SCIPIO: Builds a partial URL - including the scheme and host, but not the servlet path or resource.
     * Does NOT consult controller. useSSL false by default.
     * Added 2017-11-17.
     */
    public boolean buildHostPart(Appendable buffer, Boolean useSSL) throws WebAppConfigurationException, IOException {
        return buildHostPart(buffer, null, useSSL, false);
    }
    
    /**
     * Builds a partial URL - including the servlet path and resource, but not the scheme or host.
     * <p>
     * SCIPIO: 2018-08-01: If url is null, this only appends up to the servlet path, with no trailing slash.
     * If url is empty string, does the same but appends trailing slash.
     * @param buffer
     * @param url
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public void buildPathPart(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        if (servletPath == null) {
            throw new IllegalStateException("Servlet path is unknown");
        }
        buffer.append(webSiteProps.getWebappPathPrefix()); // SCIPIO: 2018-07-27
        if (url != null) {
            buffer.append(servletPath);
            appendPathPart(buffer, url); // SCIPIO
        } else {
            buffer.append(servletPath.endsWith("/") ? servletPath.substring(0, servletPath.length() - 1) : servletPath);
        }
    }

    /**
     * SCIPIO: Builds path part up to servlet path.
     * Alias for {@link #buildPathPart(Appendable, String)}.
     * Added 2018-08-01.
     */
    public void buildPathPartWithServletPath(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        buildPathPart(buffer, url);
    }

    /**
     * SCIPIO: Builds path part up to servlet path, with no trailing slash.
     * Added 2018-08-01.
     */
    public void buildPathPartWithServletPath(Appendable buffer) throws WebAppConfigurationException, IOException {
        buildPathPart(buffer, null);
    }
    
    /**
     * SCIPIO: Adds a path and a url to the buffer, handling slash (/).
     * WARN: This assumes the buffer is a StringBuilder, StringWriter, or other whose toString()
     * returns the url, and not some other type of Writer.
     * Added 2018-07-09. 
     */
    private static void appendPathPart(Appendable buffer, String part) throws IOException {
        if (buffer.toString().endsWith("/")) {
            if (part.startsWith("/")) {
                buffer.append(part.substring(1));
            } else {
                buffer.append(part);
            }
        } else {
            if (!part.startsWith("/")) {
                buffer.append("/");
            }
            buffer.append(part);
        }
    }

    /**
     * SCIPIO: Builds a partial URL - including the context path, but not the scheme or host or servlet.
     * <p>
     * 2018-08-01: If url is null, this only appends up to the context root, with no trailing slash.
     * If url is empty string, does the same but appends trailing slash.
     * @param buffer
     * @param url
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public void buildPathPartWithContextRoot(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        if (contextPath == null) {
            throw new IllegalStateException("Context path is unknown");
        }
        buffer.append(webSiteProps.getWebappPathPrefix()); // SCIPIO: 2018-07-27
        if (url != null) {
            buffer.append(contextPath);
            appendPathPart(buffer, url);
        } else {
            buffer.append(contextPath.endsWith("/") ? contextPath.substring(0, contextPath.length() - 1) : contextPath);
        }
    }

    /**
     * SCIPIO: Builds path part up to webapp context root, with no trailing slash.
     * Added 2018-08-01.
     */
    public void buildPathPartWithContextRoot(Appendable buffer) throws WebAppConfigurationException, IOException {
        buildPathPartWithContextRoot(buffer, null);
    }

    /**
     * SCIPIO: Builds a partial URL - including the webapp path prefix, but not the context path.
     * <p>
     * 2018-08-01: If url is null, this only appends up to the webapp path prefix, with no trailing slash.
     * If url is empty string, does the same but appends trailing slash.
     * @param buffer
     * @param url
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public void buildPathPartWithWebappPathPrefix(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        buffer.append(webSiteProps.getWebappPathPrefix()); // SCIPIO: 2018-07-27
        if (url != null) {
            appendPathPart(buffer, url); // SCIPIO
        }
    }

    /**
     * SCIPIO: Builds path part up to webapp path prefix, with no trailing slash.
     * Added 2018-08-01.
     */
    public void buildPathPartWithWebappPathPrefix(Appendable buffer) throws WebAppConfigurationException, IOException {
        buildPathPartWithWebappPathPrefix(buffer, null);
    }

    /**
     * SCIPIO: Get servlet path including the webapp context path, but NOT
     * including the webappPathPrefix ({@link #getWebappPathPrefix()});
     * <p>
     * NOTE: 2018-08-01: Excludes terminating slash.
     */
    public String getContextAndServletPath() throws WebAppConfigurationException, IOException {
        return servletPath.endsWith("/") ? servletPath.substring(0, servletPath.length() - 1) : servletPath;
    }
    
    /**
     * SCIPIO: Get webapp context path, but NOT
     * including the webappPathPrefix ({@link #getWebappPathPrefix()});
     * <p>
     * NOTE: 2018-08-01: Excludes terminating slash.
     */
    public String getContextPath() throws WebAppConfigurationException, IOException {
        return contextPath.endsWith("/") ? contextPath.substring(0, contextPath.length() - 1) : contextPath;
    }

    /**
     * SCIPIO: Get the webapp path prefix (comes from WebSiteProperties webappPathPrefix).
     * Added 2018-07-27.
     */
    public String getWebappPathPrefix() throws WebAppConfigurationException, IOException {
        return webSiteProps.getWebappPathPrefix();
    }
}
