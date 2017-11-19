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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.tomcat.util.digester.Digester;
import org.apache.tomcat.util.descriptor.DigesterFactory;
import org.apache.tomcat.util.descriptor.web.ServletDef;
import org.apache.tomcat.util.descriptor.web.WebRuleSet;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Assert;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilXml.LocalErrorHandler;
import org.ofbiz.base.util.UtilXml.LocalResolver;
import org.ofbiz.base.util.cache.UtilCache;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Web application utilities.
 * <p>This class reuses some of the Tomcat/Catalina classes for convenience, but
 * OFBiz does not need to be running on Tomcat for this to work.</p>
 */
public final class WebAppUtil {

    public static final String module = WebAppUtil.class.getName();
    private static final String webAppFileName = "/WEB-INF/web.xml";
    private static final UtilCache<String, WebXml> webXmlCache = UtilCache.createUtilCache("webapp.WebXml");

    /**
     * SCIPIO: Fast, light homemache cache to optimize control servlet path lookups.
     */
    private static final Map<String, String> controlServletPathWebappInfoCache = new ConcurrentHashMap<String, String>();
    
    /**
     * SCIPIO: Fast, light homemache cache to optimize WebappInfo lookups by webSiteId.
     */
    private static final Map<String, WebappInfo> webappInfoWebSiteIdCache = new ConcurrentHashMap<String, WebappInfo>();

    /**
     * SCIPIO: Fast, light homemache cache to optimize WebappInfo lookups by exact context path.
     */
    private static final Map<String, WebappInfo> webappInfoContextPathCache = new ConcurrentHashMap<String, WebappInfo>();
    
    private static final Pattern contextPathDelimPat = Pattern.compile("[/?;#&]");
    
    /**
     * Returns the control servlet path. The path consists of the web application's mount-point
     * specified in the <code>ofbiz-component.xml</code> file and the servlet mapping specified
     * in the web application's <code>web.xml</code> file.
     * 
     * @param webAppInfo
     * @param optional SCIPIO: if true, return null if not found; otherwise throw IllegalArgumentException (added 2017-11-18)
     * @throws IOException
     * @throws SAXException
     */
    public static String getControlServletPath(WebappInfo webAppInfo, boolean optional) throws IOException, SAXException {
        Assert.notNull("webAppInfo", webAppInfo);
        // SCIPIO: Go through cache first. No need to synchronize, doesn't matter.
        String res = controlServletPathWebappInfoCache.get(webAppInfo.getContextRoot()); // key on context root (global unique)
        if (res != null) {
            // We take empty string to mean lookup found nothing
            if (res.isEmpty()) {
                if (optional) return null; // SCIPIO
                else throw new IllegalArgumentException("org.ofbiz.webapp.control.ControlServlet mapping not found in " + webAppInfo.getLocation() + webAppFileName);
            }
            else {
                return res;
            }
        }
        else {
            String servletMapping = null;
            WebXml webXml = getWebXml(webAppInfo);
            for (ServletDef servletDef : webXml.getServlets().values()) {
                if ("org.ofbiz.webapp.control.ControlServlet".equals(servletDef.getServletClass())) {
                    String servletName = servletDef.getServletName();
                    // Catalina servlet mappings: key = url-pattern, value = servlet-name.
                    for (Entry<String, String> entry : webXml.getServletMappings().entrySet()) {
                        if (servletName.equals(entry.getValue())) {
                            servletMapping = entry.getKey();
                            break;
                        }
                    }
                    break;
                }
            }
            if (servletMapping == null) {
                // SCIPIO: empty string means we did lookup and failed
                controlServletPathWebappInfoCache.put(webAppInfo.getContextRoot(), "");
                if (optional) return null; // SCIPIO
                else throw new IllegalArgumentException("org.ofbiz.webapp.control.ControlServlet mapping not found in " + webAppInfo.getLocation() + webAppFileName);
            }
            servletMapping = servletMapping.replace("*", "");
            String servletPath = webAppInfo.contextRoot.concat(servletMapping);
            // SCIPIO: save result
            controlServletPathWebappInfoCache.put(webAppInfo.getContextRoot(), servletPath);
            return servletPath;
        }
    }
    
    /**
     * Returns the control servlet path. The path consists of the web application's mount-point
     * specified in the <code>ofbiz-component.xml</code> file and the servlet mapping specified
     * in the web application's <code>web.xml</code> file.
     * 
     * @param webAppInfo
     * @throws IOException
     * @throws SAXException
     */
    public static String getControlServletPath(WebappInfo webAppInfo) throws IOException, SAXException, IllegalArgumentException {
        return getControlServletPath(webAppInfo, false);
    }
    
    /**
     * SCIPIO: Returns the control servlet path with no exceptions generated and with a terminating slash,
     * or null. The path consists of the web application's mount-point
     * specified in the <code>ofbiz-component.xml</code> file and the servlet mapping specified
     * in the web application's <code>web.xml</code> file.
     * 
     * @param webAppInfo
     * @throws IOException
     * @throws SAXException
     */
    public static String getControlServletPathSafeSlash(WebappInfo webAppInfo) {
        String controlPath = null;
        try {
            controlPath = WebAppUtil.getControlServletPath(webAppInfo);
        } catch (Exception e) {
            ; // Control servlet may not exist; don't treat as error
        }
        
        if (controlPath != null && controlPath.startsWith("/")) {
            if (!controlPath.endsWith("/")) {
                controlPath += "/"; // Important
            }
        }
        return controlPath;
    }

    /**
     * SCIPIO: Gets the control servlet mapping for given webappInfo, WITHOUT the
     * webapp context root. There is never a terminating slash, except if root,
     * where it will be "/".
     */
    public static String getControlServletOnlyPath(WebappInfo webAppInfo) throws IOException, SAXException {
        String controlPath = WebAppUtil.getControlServletPath(webAppInfo);
        if (controlPath != null) {
            if (webAppInfo.contextRoot != null && !webAppInfo.contextRoot.isEmpty() && !"/".equals(webAppInfo.contextRoot)) {
                controlPath = controlPath.substring(webAppInfo.contextRoot.length());
            }
            if (controlPath.length() > 1 && controlPath.endsWith("/")) {
                controlPath = controlPath.substring(0, controlPath.length() - 1);
            }
            if (controlPath.length() == 0) {
                controlPath = "/";
            }
            return controlPath;
        } else {
            return null;
        }
    }
    
    /**
     * SCIPIO: Gets the control servlet mapping for given webappInfo, WITHOUT the
     * webapp context root, throwing no exceptions. There is never a terminating slash, 
     * except if root, where it will be "/".
     */
    public static String getControlServletOnlyPathSafe(WebappInfo webAppInfo) {
        String controlPath = null;
        try {
            controlPath = WebAppUtil.getControlServletOnlyPath(webAppInfo);
        } catch (Exception e) {
            ; // Control servlet may not exist; don't treat as error
        }
        return controlPath;
    }

    /**
     * Returns the <code>WebappInfo</code> instance associated to the specified web site ID.
     * Throws <code>IllegalArgumentException</code> if the web site ID was not found.
     * 
     * @param webSiteId
     * @throws IOException
     * @throws SAXException
     */
    public static WebappInfo getWebappInfoFromWebsiteId(String webSiteId) throws IOException, SAXException {
        Assert.notNull("webSiteId", webSiteId);
        // SCIPIO: Go through cache first. No need to synchronize, doesn't matter.
        WebappInfo res = webappInfoWebSiteIdCache.get(webSiteId);
        if (res != null) {
            return res;
        }
        else {
            for (WebappInfo webAppInfo : ComponentConfig.getAllWebappResourceInfos()) {
                if (webSiteId.equals(WebAppUtil.getWebSiteId(webAppInfo))) {
                    webappInfoWebSiteIdCache.put(webSiteId, webAppInfo); // SCIPIO: save in cache
                    return webAppInfo;
                }
            }
        }
        throw new IllegalArgumentException("Web site ID '" + webSiteId + "' not found.");
    }
    
    /**
     * SCIPIO: Returns the <code>WebappInfo</code> instance that has the same mount-point prefix as
     * the given path.
     * <p>
     * <strong>WARN:</strong> Webapp mounted on root (/*) will usually cause a catch-all here.
     * 
     * @param webSiteId
     * @throws IOException
     * @throws SAXException
     */
    public static WebappInfo getWebappInfoFromPath(String path) throws IOException, SAXException {
        Assert.notNull("path", path);
        // Must be absolute
        if (!path.startsWith("/")) {
            throw new IllegalArgumentException("Scipio: Web app for path '" + path + "' not found (must be absolute path).");
        }
        
        String contextPath = path;
        // TODO: version without regexp... 
        // TODO: version that supports multiple slashes, but too complicated for now
        Matcher m = contextPathDelimPat.matcher(path.substring(1)); 
        if (m.find()) {
            contextPath = "/" + path.substring(1, m.start() + 1);
        }
        
        WebappInfo webappInfo;
        try {
            webappInfo = getWebappInfoFromContextPath(contextPath);
        }
        catch(IllegalArgumentException e) {
            try {
                // If there was no exact match, assume we're covered by the root mount-point
                webappInfo = getWebappInfoFromContextPath("/");
            }
            catch(IllegalArgumentException e2) {
                throw new IllegalArgumentException("Scipio: Web app for path '" + path + "' not found.");
            }
        }
        return webappInfo;
    }
    
    /**
     * SCIPIO: Returns the <code>WebappInfo</code> instance that the given exact context path as mount-point
     * <p>
     * <strong>WARN:</strong> Webapp mounted on root (/*) will usually cause a catch-all here.
     * 
     * @param webSiteId
     * @throws IOException
     * @throws SAXException
     */
    public static WebappInfo getWebappInfoFromContextPath(String contextPath) throws IOException, SAXException {
        Assert.notNull("contextPath", contextPath);
        
        // SCIPIO: Go through cache first. No need to synchronize, doesn't matter.
        WebappInfo res = webappInfoContextPathCache.get(contextPath);
        if (res != null) {
            return res;
        }
        else {
            for (WebappInfo webAppInfo : ComponentConfig.getAllWebappResourceInfos()) {
                if (contextPath.equals(webAppInfo.getContextRoot())) {
                    webappInfoContextPathCache.put(contextPath, webAppInfo); // SCIPIO: save in cache
                    return webAppInfo;
                }
            }
        }
        throw new IllegalArgumentException("Web app for context path '" + contextPath + "' not found.");
    }
    
    /**
     * SCIPIO: Returns the <code>WebappInfo</code> instance for the current request's webapp.
     * 
     * @param webSiteId
     * @throws IOException
     * @throws SAXException
     */
    public static WebappInfo getWebappInfoFromRequest(HttpServletRequest request) throws IOException, SAXException {
        Assert.notNull("request", request);
        String contextPath = request.getContextPath();
        return getWebappInfoFromContextPath(contextPath);
    }    

    /**
     * Returns the web site ID - as configured in the web application's <code>web.xml</code> file,
     * or <code>null</code> if no web site ID was found.
     * 
     * @param webAppInfo
     * @throws IOException
     * @throws SAXException
     */
    public static String getWebSiteId(WebappInfo webAppInfo) throws IOException, SAXException {
        Assert.notNull("webAppInfo", webAppInfo);
        WebXml webXml = getWebXml(webAppInfo);
        return webXml.getContextParams().get("webSiteId");
    }

    /**
     * Returns a <code>WebXml</code> instance that models the web application's <code>web.xml</code> file.
     * 
     * @param webAppInfo
     * @throws IOException
     * @throws SAXException
     */
    public static WebXml getWebXml(WebappInfo webAppInfo) throws IOException, SAXException {
        Assert.notNull("webAppInfo", webAppInfo);
        String webXmlFileLocation = webAppInfo.getLocation().concat(webAppFileName);
        // SCIPIO: TEMPORARILY CHANGED THIS TO NON-VALIDATING
        // FIXME: RETURN THIS TO VALIDATING ONCE ALL web.xml VALIDATION ISSUES ARE MERGED FROM UPSTREAM
        // The ofbiz team neglected to do it in this part of code, probably
        // because stock doesn't use it much yet... but we rely on it
        // NOTE: it's also possible this code is missing something that is done in CatalinaContainer
        // but not here... don't know... wait for upstream
        //return parseWebXmlFile(webXmlFileLocation, true);
        return parseWebXmlFile(webXmlFileLocation, false);
    }

    /**
     * Parses the specified <code>web.xml</code> file into a <code>WebXml</code> instance.
     * 
     * @param webXmlFileLocation
     * @param validate
     * @throws IOException
     * @throws SAXException
     */
    public static WebXml parseWebXmlFile(String webXmlFileLocation, boolean validate) throws IOException, SAXException {
        Assert.notEmpty("webXmlFileLocation", webXmlFileLocation);
        WebXml result = webXmlCache.get(webXmlFileLocation);
        if (result == null) {
            File file = new File(webXmlFileLocation);
            if (!file.exists()) {
                throw new IllegalArgumentException(webXmlFileLocation + " does not exist.");
            }
            boolean namespaceAware = true;
            InputStream is = new FileInputStream(file);
            result = new WebXml();
            LocalResolver lr = new LocalResolver(new DefaultHandler());
            ErrorHandler handler = new LocalErrorHandler(webXmlFileLocation, lr);
            Digester digester = DigesterFactory.newDigester(validate, namespaceAware, new WebRuleSet(), false);
            digester.getParser();
            digester.push(result);
            digester.setErrorHandler(handler);
            try {
                digester.parse(new InputSource(is));
            } finally {
                digester.reset();
                if (is != null) {
                    try {
                        is.close();
                    } catch (Throwable t) {
                        Debug.logError(t, "Exception thrown while parsing " + webXmlFileLocation + ": ", module);
                    }
                }
            }
            result = webXmlCache.putIfAbsentAndGet(webXmlFileLocation, result);
        }
        return result;
    }

    /**
     * SCIPIO: Returns the web.xml context-params for webappInfo.
     */
    public static Map<String, String> getWebappContextParams(WebappInfo webappInfo) {
        WebXml webXml;
        try {
            webXml = WebAppUtil.getWebXml(webappInfo);
            Map<String, String> contextParams = webXml.getContextParams();
            return contextParams != null ? contextParams : Collections.<String, String> emptyMap();
        } catch (Exception e) {
            throw new IllegalArgumentException("Web app xml definition for webapp with context root '" + webappInfo.contextRoot + "' not found.", e);
        }
    }
    
    /**
     * SCIPIO: Returns the web.xml context-params for webappInfo, with no exceptions thrown if anything missing.
     */
    public static Map<String, String> getWebappContextParamsSafe(WebappInfo webappInfo) {
        try {
            return getWebappContextParams(webappInfo);
        } catch (Exception e) {
            return Collections.<String, String> emptyMap();
        }
    }
    
    /**
     * SCIPIO: Returns the web.xml context-params for webSiteId.
     */
    public static Map<String, String> getWebappContextParams(String webSiteId) {
        WebappInfo webappInfo;
        WebXml webXml;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            webXml = WebAppUtil.getWebXml(webappInfo);
            Map<String, String> contextParams = webXml.getContextParams();
            return contextParams != null ? contextParams : Collections.<String, String> emptyMap();
        } catch (Exception e) {
            throw new IllegalArgumentException("Web app xml definition for webSiteId '" + webSiteId + "' not found.", e);
        }
    }
    
    /**
     * SCIPIO: Returns the web.xml context-params for webSiteId, with no exceptions thrown if anything missing.
     */
    public static Map<String, String> getWebappContextParamsSafe(String webSiteId) {
        try {
            return getWebappContextParams(webSiteId);
        } catch (Exception e) {
            return Collections.<String, String> emptyMap();
        }
    }
    
    private WebAppUtil() {}
}
