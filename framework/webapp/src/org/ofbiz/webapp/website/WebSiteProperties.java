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
package org.ofbiz.webapp.website;

import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.lang.ThreadSafe;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.RequestLinkUtil;

/**
 * Web site properties.
 */
@ThreadSafe
public final class WebSiteProperties {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the <code>url.properties</code> file.
     */
    public static WebSiteProperties defaults(Delegator delegator) {
        return new WebSiteProperties(delegator);
    }

    /**
     * SCIPIO: Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the <code>url.properties</code> file, with caching in the given map.
     * Added 2018-07-31.
     */
    public static WebSiteProperties defaults(Delegator delegator, Map<String, WebSiteProperties> cache) {
        if (cache == null) return defaults(delegator);
        WebSiteProperties webSiteProps = cache.get("_defaults_");
        if (webSiteProps == null) {
            webSiteProps = new WebSiteProperties(delegator);
            cache.put("_defaults_", webSiteProps);
        }
        return webSiteProps;
    }

    /**
     * SCIPIO: Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the <code>url.properties</code> file, with request scope caching.
     * Added 2018-07-31.
     */
    public static WebSiteProperties defaults(HttpServletRequest request) {
        Assert.notNull("request", request);
        WebSiteProperties webSiteProps = (WebSiteProperties) request.getAttribute("_DEF_WEBSITE_PROPS_");
        if (webSiteProps == null) {
            Delegator delegator = (Delegator) request.getAttribute("delegator");
            webSiteProps = new WebSiteProperties(delegator);
            request.setAttribute("_DEF_WEBSITE_PROPS_", webSiteProps);
        }
        return webSiteProps;
    }

    /**
     * Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the application's WebSite entity value. If the application does not have a
     * WebSite entity value then the instance is initialized to the settings found
     * in the <code>url.properties</code> file.
     * 
     * @param request
     * @throws GenericEntityException
     */
    public static WebSiteProperties from(HttpServletRequest request) throws GenericEntityException {
        Assert.notNull("request", request);
        WebSiteProperties webSiteProps = (WebSiteProperties) request.getAttribute("_WEBSITE_PROPS_");
        if (webSiteProps == null) {

            // SCIPIO: now delegates
            webSiteProps = newFrom(request, WebSiteWorker.getWebSiteId(request));
            
            request.setAttribute("_WEBSITE_PROPS_", webSiteProps);
        }
        return webSiteProps;
    }
    
    private static WebSiteProperties newFrom(HttpServletRequest request, String webSiteId) throws GenericEntityException {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        
        // SCIPIO: This code section is restructured for optional request overrides and other fixes.
        
        boolean overrideRequestHostPort = "Y".equalsIgnoreCase(EntityUtilProperties.getPropertyValue("url.properties", "override.request.host.port", delegator));
        boolean requestOverridesStatic = !overrideRequestHostPort;
        boolean requestOverridesStaticHttpPort = requestOverridesStatic;
        boolean requestOverridesStaticHttpHost = requestOverridesStatic;
        boolean requestOverridesStaticHttpsPort = requestOverridesStatic;
        boolean requestOverridesStaticHttpsHost = requestOverridesStatic;
        boolean requestOverridesStaticWebappPathPrefix = requestOverridesStatic;
        
        WebSiteProperties defaults = new WebSiteProperties(delegator);
        
        String httpPort = defaults.getHttpPort();
        String httpHost = defaults.getHttpHost();
        String httpsPort = defaults.getHttpsPort();
        String httpsHost = defaults.getHttpsHost();
        boolean enableHttps = defaults.getEnableHttps();
        // SCIPIO: new
        String webappPathPrefix = defaults.getWebappPathPrefix();
        String webappPathPrefixHeader = defaults.getWebappPathPrefixHeader();

        if (delegator != null) {
            if (webSiteId != null) {
                GenericValue webSiteValue = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
                if (webSiteValue != null) {
                    if (webSiteValue.get("httpPort") != null) {
                        httpPort = webSiteValue.getString("httpPort");
                        requestOverridesStaticHttpPort = false;
                    }
                    if (webSiteValue.get("httpHost") != null) {
                        httpHost = webSiteValue.getString("httpHost");
                        requestOverridesStaticHttpHost = false;
                    }
                    if (webSiteValue.get("httpsPort") != null) {
                        httpsPort = webSiteValue.getString("httpsPort");
                        requestOverridesStaticHttpsPort = false;
                    }
                    if (webSiteValue.get("httpsHost") != null) {
                        httpsHost = webSiteValue.getString("httpsHost");
                        requestOverridesStaticHttpsHost = false;
                    }
                    if (webSiteValue.get("enableHttps") != null) {
                        enableHttps = webSiteValue.getBoolean("enableHttps");
                    }

                    // SCIPIO: new
                    if (webSiteValue.get("webappPathPrefix") != null) {
                        webappPathPrefix = webSiteValue.getString("webappPathPrefix");
                        requestOverridesStaticWebappPathPrefix = false;
                    }
                }
            }
        }
        
        // SCIPIO: NOTE: this has been factored and moved to before the request value lookups.
        httpPort = adjustPort(delegator, httpPort);
        httpsPort = adjustPort(delegator, httpsPort);      
        
        boolean isSecure = RequestLinkUtil.isEffectiveSecure(request); // SCIPIO: 2018: replace request.isSecure()
        
        // SCIPIO: this may override the url.properties settings, though not the WebSite settings
        if ((requestOverridesStaticHttpPort || httpPort.isEmpty()) && !isSecure) {
            httpPort = String.valueOf(request.getServerPort());
        }
        if (requestOverridesStaticHttpHost || httpHost.isEmpty()) {
            httpHost = request.getServerName();
        }
        if ((requestOverridesStaticHttpsPort || httpsPort.isEmpty()) && isSecure) {
            httpsPort = String.valueOf(request.getServerPort());
        }
        if (requestOverridesStaticHttpsHost || httpsHost.isEmpty()) {
            httpsHost = request.getServerName();
        }

        if ((requestOverridesStaticWebappPathPrefix || webappPathPrefix.isEmpty()) && !webappPathPrefixHeader.isEmpty()) {
            webappPathPrefix = request.getHeader(webappPathPrefixHeader);
            if (webappPathPrefix == null) webappPathPrefix = "";
        }
        webappPathPrefix = normalizeWebappPathPrefix(webappPathPrefix);

        return new WebSiteProperties(httpPort, httpHost, httpsPort, httpsHost, enableHttps,
                webappPathPrefix, webappPathPrefixHeader);
    }
    
    /**
     * SCIPIO: Returns web site properties for the given web site; any host or port fields
     * not specified are taken from request instead, as would be returned by {@link #from(HttpServletRequest)}.
     * 
     * @param webSiteValue
     */
    public static WebSiteProperties from(HttpServletRequest request, GenericValue webSiteValue) throws GenericEntityException {
        Assert.notNull("webSiteValue", webSiteValue);
        if (!"WebSite".equals(webSiteValue.getEntityName())) {
            throw new IllegalArgumentException("webSiteValue is not a WebSite entity value");
        }
        
        WebSiteProperties defaults = from(request);
                
        String httpPort;
        boolean adjustHttpPort;
        if (webSiteValue.get("httpPort") != null) {
            httpPort = webSiteValue.getString("httpPort");
            adjustHttpPort = true;
        } else {
            httpPort = defaults.getHttpPort();
            adjustHttpPort = false;
        }
        String httpHost = (webSiteValue.get("httpHost") != null) ? webSiteValue.getString("httpHost") : defaults.getHttpHost();
        String httpsPort;
        boolean adjustHttpsPort;
        if (webSiteValue.get("httpsPort") != null) {
            httpsPort = webSiteValue.getString("httpsPort");
            adjustHttpsPort = true;
        } else {
            httpsPort = defaults.getHttpsPort();
            adjustHttpsPort = false;
        }
        String httpsHost = (webSiteValue.get("httpsHost") != null) ? webSiteValue.getString("httpsHost") : defaults.getHttpsHost();
        boolean enableHttps = (webSiteValue.get("enableHttps") != null) ? webSiteValue.getBoolean("enableHttps") : defaults.getEnableHttps();

        if (adjustHttpPort) {
            httpPort = adjustPort(webSiteValue.getDelegator(), httpPort);
        }
        if (adjustHttpsPort) {
            httpsPort = adjustPort(webSiteValue.getDelegator(), httpsPort);
        }

        // SCIPIO: new
        String webappPathPrefix = (webSiteValue.get("webappPathPrefix") != null) ? normalizeWebappPathPrefix(webSiteValue.getString("webappPathPrefix")) : defaults.getWebappPathPrefix();
        String webappPathPrefixHeader = defaults.getWebappPathPrefixHeader();

        return new WebSiteProperties(httpPort, httpHost, httpsPort, httpsHost, enableHttps, 
                webappPathPrefix, webappPathPrefixHeader);
    }
    
    /**
     * SCIPIO: Returns web site properties for the given webSiteId, or for any fields missing,
     * the values for the current request (or system defaults).
     * 
     * @param webSiteValue
     */
    public static WebSiteProperties from(HttpServletRequest request, String webSiteId) throws GenericEntityException {
        Assert.notNull("webSiteId", webSiteId);
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue webSiteValue = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
        if (webSiteValue != null) {
            return from(request, webSiteValue);
        } else {
            throw new GenericEntityException("Scipio: Could not find WebSite for webSiteId '" + webSiteId + "'");
        }
    }
    
    
    /**
     * Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the WebSite entity value.
     * 
     * @param webSiteValue
     */
    public static WebSiteProperties from(GenericValue webSiteValue) {
        Assert.notNull("webSiteValue", webSiteValue);
        if (!"WebSite".equals(webSiteValue.getEntityName())) {
            throw new IllegalArgumentException("webSiteValue is not a WebSite entity value");
        }
        WebSiteProperties defaults = new WebSiteProperties(webSiteValue.getDelegator());
        String httpPort = (webSiteValue.get("httpPort") != null) ? webSiteValue.getString("httpPort") : defaults.getHttpPort();
        String httpHost = (webSiteValue.get("httpHost") != null) ? webSiteValue.getString("httpHost") : defaults.getHttpHost();
        String httpsPort = (webSiteValue.get("httpsPort") != null) ? webSiteValue.getString("httpsPort") : defaults.getHttpsPort();
        String httpsHost = (webSiteValue.get("httpsHost") != null) ? webSiteValue.getString("httpsHost") : defaults.getHttpsHost();
        boolean enableHttps = (webSiteValue.get("enableHttps") != null) ? webSiteValue.getBoolean("enableHttps") : defaults.getEnableHttps();

        // SCIPIO: factored out
        httpPort = adjustPort(webSiteValue.getDelegator(), httpPort);
        httpsPort = adjustPort(webSiteValue.getDelegator(), httpsPort);            

        // SCIPIO: new
        String webappPathPrefix = (webSiteValue.get("webappPathPrefix") != null) ? normalizeWebappPathPrefix(webSiteValue.getString("webappPathPrefix")) : defaults.getWebappPathPrefix();
        String webappPathPrefixHeader = defaults.getWebappPathPrefixHeader();

        return new WebSiteProperties(httpPort, httpHost, httpsPort, httpsHost, enableHttps, 
                webappPathPrefix, webappPathPrefixHeader);
    }
    
    /**
     * SCIPIO: Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the WebSite entity value for the given webSiteId.
     * 
     * @param delegator
     * @param webSiteId
     */
    public static WebSiteProperties from(Delegator delegator, String webSiteId) throws GenericEntityException {
        Assert.notNull("webSiteId", webSiteId);
        GenericValue webSiteValue = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
        if (webSiteValue != null) {
            return from(webSiteValue);
        } else {
            throw new GenericEntityException("Scipio: Could not find WebSite for webSiteId '" + webSiteId + "'");
        }
    }
    
    /**
     * SCIPIO: Returns a <code>WebSiteProperties</code> instance initialized to the settings found
     * in the WebSite entity value for the given webSiteId, using the given cache.
     * 
     * @param delegator
     * @param webSiteId
     */
    public static WebSiteProperties from(Delegator delegator, String webSiteId, Map<String, WebSiteProperties> cache) throws GenericEntityException {
        if (cache == null) return from(delegator, webSiteId);
        WebSiteProperties webSiteProps = cache.get(webSiteId);
        if (webSiteProps == null) {
            webSiteProps = from(delegator, webSiteId);
            cache.put(webSiteId, webSiteProps);
        }
        return webSiteProps;
    }

    /**
     * SCIPIO: A specialized method to lookup WebSiteProperties for the current app,
     * which never throws exceptions and tries best-effort to get the webappPathPrefix
     * no matter what.
     * <p>
     * Added 2018-08-31.
     */
    public static WebSiteProperties fromRequestFilterSafe(HttpServletRequest request) {
        WebSiteProperties webSiteProps = (WebSiteProperties) request.getAttribute("_WEBSITE_PROPS_");
        if (webSiteProps != null) return webSiteProps;
        if (request.getAttribute("delegator") != null) {
            // If the delegator was set in request attributes,
            // it means we passed the ContextFilter, so we can call the regular
            // factory method and let it cache this WebSiteProperties for us;
            // We do not need to clear "_WEBSITE_PROPS_".
            try {
                return from(request);
            } catch (Exception e) {
                Debug.logError(e, "Could not get WebSiteProperties from request; treating webappPathPrefix as not set", module);
                return null;
            }
        } else {
            // If the delegator was not yet set, it means we're being called from
            // somewhere like a very early filter before ContextFilter;
            // we must prevent caching the results to avoid influencing the app with our state.
            request.setAttribute("delegator", WebAppUtil.getDelegatorFilterSafe(request));
            try {
                return from(request);
            } catch (Exception e) {
                Debug.logError(e, "Could not get WebSiteProperties from request using fallback delegator; treating webappPathPrefix as not set", module);
                return null;
            } finally {
                // Do not let filter state affect rest of request
                request.removeAttribute("_WEBSITE_PROPS_");
                request.removeAttribute("delegator");
            }
        }
    }

    private final String httpPort;
    private final String httpHost;
    private final String httpsPort;
    private final String httpsHost;
    private final boolean enableHttps;

    private final String webappPathPrefix; // SCIPIO: added 2018-07-27
    private final String webappPathPrefixHeader; // SCIPIO: added 2018-07-27

    private WebSiteProperties(Delegator delegator) {
        this.httpPort = EntityUtilProperties.getPropertyValue("url", "port.http", delegator);
        this.httpHost = EntityUtilProperties.getPropertyValue("url", "force.http.host", delegator);
        this.httpsPort = EntityUtilProperties.getPropertyValue("url", "port.https", delegator);
        this.httpsHost = EntityUtilProperties.getPropertyValue("url", "force.https.host", delegator);
        this.enableHttps = EntityUtilProperties.propertyValueEqualsIgnoreCase("url", "port.https.enabled", "Y", delegator);

        this.webappPathPrefix = normalizeWebappPathPrefix(EntityUtilProperties.getPropertyValue("url", "webapp.url.path.prefix", delegator)); // SCIPIO
        this.webappPathPrefixHeader = EntityUtilProperties.getPropertyValue("url", "webapp.url.path.prefix.httpHeader", delegator); // SCIPIO
    }

    private WebSiteProperties(String httpPort, String httpHost, String httpsPort, String httpsHost, boolean enableHttps,
            String webappPathPrefix, String webappPathPrefixHeader) { // SCIPIO: new fields
        this.httpPort = httpPort;
        this.httpHost = httpHost;
        this.httpsPort = httpsPort;
        this.httpsHost = httpsHost;
        this.enableHttps = enableHttps;

        this.webappPathPrefix = webappPathPrefix;
        this.webappPathPrefixHeader = webappPathPrefixHeader;
    }

    /**
     * Returns the configured http port, or an empty <code>String</code> if not configured.
     */
    public String getHttpPort() {
        return httpPort;
    }

    /**
     * Returns the configured http host, or an empty <code>String</code> if not configured.
     */
    public String getHttpHost() {
        return httpHost;
    }

    /**
     * Returns the configured https port, or an empty <code>String</code> if not configured.
     */
    public String getHttpsPort() {
        return httpsPort;
    }

    /**
     * Returns the configured https host, or an empty <code>String</code> if not configured.
     */
    public String getHttpsHost() {
        return httpsHost;
    }

    /**
     * Returns <code>true</code> if https is enabled.
     */
    public boolean getEnableHttps() {
        return enableHttps;
    }

    /**
     * SCIPIO: Returns the webapp/navigation URL path prefix.
     * <p>
     * DEV NOTE: Prefer using {@link org.ofbiz.webapp.OfbizUrlBuilder} methods over calling this.
     */
    public String getWebappPathPrefix() {
        return webappPathPrefix;
    }

    /**
     * SCIPIO: Returns the webapp/navigation URL path prefix HTTP header name.
     * TODO: REVIEW: protected for now; probably no reason for any other class to use.
     */
    protected String getWebappPathPrefixHeader() {
        return webappPathPrefixHeader;
    }

    private static final boolean defaultWebappPathPrefixUrlBuild = UtilProperties.getPropertyAsBoolean("url",
            "webapp.url.path.prefix.urlBuild", true); // SCIPIO

    /**
     * SCIPIO: If true, the webappPathPrefix should be included in URL building
     * code by default; if false, it is left up to URL rewriting to append it.
     * <p>
     * NOTE: 2018-08-03: At current time this setting is stored only in url.properties
     * and web.xml, NOT the WebSite entity, to reflect its coded nature.
     * <p>
     * DEV NOTE: Prefer using {@link org.ofbiz.webapp.OfbizUrlBuilder} methods over calling this.
     */
    public boolean isWebappPathPrefixUrlBuild(ServletContext servletContext) {
        return UtilMisc.booleanValue(servletContext.getAttribute("scpWebappPathPrefixUrlBuild"), defaultWebappPathPrefixUrlBuild);
    }
    
    /**
     * SCIPIO: If true, the webappPathPrefix should be included in URL building
     * code by default; if false, it is left up to URL rewriting to append it.
     * <p>
     * NOTE: 2018-08-03: At current time this setting is stored only in url.properties
     * and web.xml, NOT the WebSite entity, to reflect its coded nature.
     * <p>
     * DEV NOTE: Prefer using {@link org.ofbiz.webapp.OfbizUrlBuilder} methods over calling this.
     */
    public boolean isWebappPathPrefixUrlBuild(Map<String, String> contextParams) {
        return UtilMisc.booleanValue(contextParams.get("scpWebappPathPrefixUrlBuild"), defaultWebappPathPrefixUrlBuild);
    }
    
    /**
     * SCIPIO: If true, the webappPathPrefix should be included in URL building
     * code by default; if false, it is left up to URL rewriting to append it.
     * <p>
     * NOTE: 2018-08-03: At current time this setting is stored only in url.properties
     * and web.xml, NOT the WebSite entity, to reflect its coded nature.
     * <p>
     * DEV NOTE: Prefer using {@link org.ofbiz.webapp.OfbizUrlBuilder} methods over calling this.
     */
    public boolean isWebappPathPrefixUrlBuildDefault() {
        return defaultWebappPathPrefixUrlBuild;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{httpPort=");
        sb.append(httpPort).append(", ");
        sb.append("httpHost=").append(httpHost).append(", ");
        sb.append("httpsPort=").append(httpsPort).append(", ");
        sb.append("httpsHost=").append(httpsHost).append(", ");
        // SCIPIO
        //sb.append("enableHttps=").append(enableHttps).append("}");
        sb.append("enableHttps=").append(enableHttps).append(", ");
        sb.append("webappPathPrefix=").append(webappPathPrefix).append(", ");
        sb.append("webappPathPrefixHeader=").append(webappPathPrefixHeader);
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * SCIPIO: Returns true if and only if all fields in this object match 
     * the ones in the other WebSiteProperties.
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object other) {
        return equalsProtoHostPort(other) &&
               sameFields(this.webappPathPrefix, ((WebSiteProperties) other).webappPathPrefix) &&
               sameFields(this.webappPathPrefixHeader, ((WebSiteProperties) other).webappPathPrefixHeader);
    }
    
    /**
     * SCIPIO: Returns true if and only if all fields in this object match 
     * the ones in the other WebSiteProperties. Fields which are missing, 
     * such as hosts or ports, are substituted with hardcoded Ofbiz defaults when 
     * performing the comparison.
     * <p>
     * Currently, the hard defaults are "localhost" for host fields, "80" for httpPort
     * and "443" for httpsPort. 
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equalsWithHardDefaults(Object other) {
        return equalsProtoHostPortWithHardDefaults(other) &&
               sameFields(this.webappPathPrefix, ((WebSiteProperties) other).webappPathPrefix) &&
               sameFields(this.webappPathPrefixHeader, ((WebSiteProperties) other).webappPathPrefixHeader);
    }

    /**
     * SCIPIO: Returns true if and only if all host-related fields in this object match 
     * the ones in the other WebSiteProperties.
     */
    public boolean equalsProtoHostPort(Object other) {
        if (this == other) {
            return true;
        } else if (other == null) {
            return false;
        } else if (!(other instanceof WebSiteProperties)) {
            return false;
        }
        WebSiteProperties o = (WebSiteProperties) other;
        return sameFields(this.httpHost, o.httpHost) &&
               sameFields(this.httpPort, o.httpPort) &&
               sameFields(this.httpsHost, o.httpsHost) &&
               sameFields(this.httpsPort, o.httpsPort) &&
               (this.enableHttps == o.enableHttps);
    }

    /**
     * SCIPIO: Returns true if and only if all host-related fields in this object match 
     * the ones in the other WebSiteProperties. Fields which are missing, 
     * such as hosts or ports, are substituted with hardcoded Ofbiz defaults when 
     * performing the comparison.
     * <p>
     * Currently, the hard defaults are "localhost" for host fields, "80" for httpPort
     * and "443" for httpsPort. 
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equalsProtoHostPortWithHardDefaults(Object other) {
        if (this == other) {
            return true;
        } else if (other == null) {
            return false;
        } else if (!(other instanceof WebSiteProperties)) {
            return false;
        }
        WebSiteProperties o = (WebSiteProperties) other;
        return sameFields(this.httpHost, o.httpHost, "localhost") &&
               sameFields(this.httpPort, o.httpPort, "80") &&
               sameFields(this.httpsHost, o.httpsHost, "localhost") &&
               sameFields(this.httpsPort, o.httpsPort, "443") &&
               (this.enableHttps == o.enableHttps);
    }
    
    private static boolean sameFields(String first, String second) {
        // SCIPIO: treat null and empty the same, just to be safe
        if (first != null && !first.isEmpty()) {
            return first.equals(second);
        } else {
            return (second == null || second.isEmpty());
        }
    }
    
    private static boolean sameFields(String first, String second, String defaultVal) {
        if (first == null || first.isEmpty()) {
            first = defaultVal;
        }
        if (second == null || second.isEmpty()) {
            second = defaultVal;
        }
        return first.equals(second);
    }
    
    /**
     * SCIPIO: Adjusts the given port value (as string) by the port offset configuration value, if applicable.
     */
    public static String adjustPort(Delegator delegator, String port) {
        if (port != null && !port.isEmpty() && Start.getInstance().getConfig().portOffset != 0) {
            Integer portValue = Integer.valueOf(port);
            portValue += Start.getInstance().getConfig().portOffset;
            return portValue.toString();
        } else {
            return port;
        }
    }
    
    /**
     * SCIPIO: Adjusts the given port value by the port offset configuration value, if applicable.
     */
    public static Integer adjustPort(Delegator delegator, Integer port) {
        if (port != null && Start.getInstance().getConfig().portOffset != 0) {
            return port + Start.getInstance().getConfig().portOffset;
        } else {
            return port;
        }
    }
    
    private static String normalizeWebappPathPrefix(String path) { // SCIPIO: 2018-07-27
        if ("/".equals(path)) return "";
        // nobody will do this
        //else if (path.endsWith("/")) return path.substring(0, path.length() - 1);
        else return path;
    }
}
