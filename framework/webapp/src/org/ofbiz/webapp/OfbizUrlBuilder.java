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
import org.ofbiz.base.util.UtilMisc;
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
 */
public final class OfbizUrlBuilder {

    public static final String module = OfbizUrlBuilder.class.getName();

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
            ControllerConfig config = ConfigXMLReader.getControllerConfig(url);
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
            config = ConfigXMLReader.getControllerConfig(webAppInfo);
            servletPath = WebAppUtil.getControlServletPath(webAppInfo);
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
            config = ConfigXMLReader.getControllerConfig(webAppInfo);
            servletPath = WebAppUtil.getControlServletPath(webAppInfo);
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
        if (url != null) { // SCIPIO: added null check
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
     * 
     * @param buffer
     * @param url
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public void buildPathPart(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        if (servletPath == null) {
            throw new IllegalStateException("Servlet path is unknown");
        }
        buffer.append(servletPath);
        // SCIPIO: added check to make sure servletPath doesn't already end with "/"
        // FIXME: we should really check buffer instead of servletPath, but we can't because Appendable...
        if (!servletPath.endsWith("/") && !url.startsWith("/")) {
            buffer.append("/");
        }
        buffer.append(url);
    }
    
    /**
     * SCIPIO: Builds a partial URL - including the context path, but not the scheme or host or servlet.
     * 
     * @param buffer
     * @param url
     * @throws WebAppConfigurationException
     * @throws IOException
     */
    public void buildPathPartWithContextRoot(Appendable buffer, String url) throws WebAppConfigurationException, IOException {
        if (contextPath == null) {
            throw new IllegalStateException("Context path is unknown");
        }
        buffer.append(contextPath);
        // SCIPIO: added check to make sure contextPath doesn't already end with "/"
        // FIXME: we should really check buffer instead of contextPath, but we can't because Appendable...
        if (!contextPath.endsWith("/") && !url.startsWith("/")) {
            buffer.append("/");
        }
        buffer.append(url);
    }    
    
    
    /**
     * SCIPIO: Get serlvet path.
     */
    public String getServletPath() throws WebAppConfigurationException, IOException {
        return servletPath;
    }
    
    /**
     * SCIPIO: Get context path.
     */
    public String getContextPath() throws WebAppConfigurationException, IOException {
        return contextPath;
    }
}
