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
package org.ofbiz.widget.renderer;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.LoginWorker;
import org.ofbiz.webapp.website.WebSiteWorker;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.cache.GenericWidgetOutput;
import org.ofbiz.widget.cache.ScreenCache;
import org.ofbiz.widget.cache.WidgetContextCacheKey;
import org.ofbiz.widget.model.AbstractModelAction;
import org.ofbiz.widget.model.ModelAction;
import org.ofbiz.widget.model.ModelLocation;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ModelScreenSettings;
import org.ofbiz.widget.model.ModelScreens;
import org.ofbiz.widget.model.ScreenFactory;
import org.xml.sax.SAXException;

import freemarker.ext.jsp.TaglibFactory;
import freemarker.ext.servlet.HttpRequestHashModel;
import freemarker.ext.servlet.HttpSessionHashModel;
import freemarker.ext.servlet.ServletContextHashModel;

/**
 * Widget Library - Screen model class
 */
public class ScreenRenderer {

    public static final String module = ScreenRenderer.class.getName();

    // SCIPIO: these get set in context to indicate render init has been run for the context.
    public static final String WEBAPP_RENDER_INIT_GUARD = "_scpRdrInitWebappRun"; // stored as Boolean
    public static final String LOCAL_RENDER_INIT_GUARDS = "_scpRdrInitLocalRuns"; // stored as a Set
    
    protected Appendable writer;
    protected MapStack<String> context;
    protected ScreenStringRenderer screenStringRenderer;
    protected int renderFormSeqNumber = 0;

    public ScreenRenderer(Appendable writer, MapStack<String> context, ScreenStringRenderer screenStringRenderer) {
        this.writer = writer;
        this.context = context;
        if (this.context == null) this.context = MapStack.create();
        this.screenStringRenderer = screenStringRenderer;
    }

    /**
     * Renders the named screen using the render environment configured when this ScreenRenderer was created.
     * <p>
     * SCIPIO: modified for asString boolean which returns as string instead of going straight to writer.
     * 
     * @param combinedName A combination of the resource name/location for the screen XML file and the name of the screen within that file, separated by a pound sign ("#"). This is the same format that is used in the view-map elements on the controller.xml file.
     * @param asString If true, returns content as string; otherwise goes direct to writer (stock behavior)
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String render(String combinedName, boolean asString) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        String resourceName = ScreenFactory.getResourceNameFromCombined(combinedName);
        String screenName = ScreenFactory.getScreenNameFromCombined(combinedName);
        // SCIPIO: return proper
        return this.render(resourceName, screenName, asString);
        //return "";
    }
    
    /**
     * Renders the named screen using the render environment configured when this ScreenRenderer was created.
     * <p>
     * SCIPIO: now delegating. Renders directly to writer.
     * 
     * @param combinedName A combination of the resource name/location for the screen XML file and the name of the screen within that file, separated by a pound sign ("#"). This is the same format that is used in the view-map elements on the controller.xml file.
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String render(String combinedName) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        String resourceName = ScreenFactory.getResourceNameFromCombined(combinedName);
        String screenName = ScreenFactory.getScreenNameFromCombined(combinedName);
        // SCIPIO: return proper
        return this.render(resourceName, screenName, false);
        //return "";
    }
    
    /**
     * SCIPIO: Renders the named screen using the render environment configured when this ScreenRenderer was created,
     * and returns the result as a string (instead of directly to writer).
     * 
     * @param combinedName A combination of the resource name/location for the screen XML file and the name of the screen within that file, separated by a pound sign ("#"). This is the same format that is used in the view-map elements on the controller.xml file.
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String renderAsString(String combinedName) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        String resourceName = ScreenFactory.getResourceNameFromCombined(combinedName);
        String screenName = ScreenFactory.getScreenNameFromCombined(combinedName);
        // SCIPIO: return proper
        return this.render(resourceName, screenName, true);
        //return "";
    }

    /**
     * Renders the named screen using the render environment configured when this ScreenRenderer was created.
     * <p>
     * SCIPIO: now delegating. Renders directly to writer.
     *
     * @param resourceName The name/location of the resource to use, can use "component://[component-name]/" and "ofbiz://" and other special OFBiz style URLs
     * @param screenName The name of the screen within the XML file specified by the resourceName.
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */    
    public String render(String resourceName, String screenName) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        return render(resourceName, screenName, false);
    }
    
    /**
     * SCIPIO: Renders the named screen using the render environment configured when this ScreenRenderer was created,
     * and returns the result as a string (instead of directly to writer).
     *
     * @param resourceName The name/location of the resource to use, can use "component://[component-name]/" and "ofbiz://" and other special OFBiz style URLs
     * @param screenName The name of the screen within the XML file specified by the resourceName.
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */    
    public String renderAsString(String resourceName, String screenName) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        return render(resourceName, screenName, true);
    }
        
    /**
     * Renders the named screen using the render environment configured when this ScreenRenderer was created.
     * <p>
     * SCIPIO: modified for asString boolean which returns as string instead of going straight to writer.
     *
     * @param resourceName The name/location of the resource to use, can use "component://[component-name]/" and "ofbiz://" and other special OFBiz style URLs
     * @param screenName The name of the screen within the XML file specified by the resourceName.
     * @param asString If true, returns content as string; otherwise goes direct to writer (stock behavior)
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String render(String resourceName, String screenName, boolean asString) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        ModelScreen modelScreen = ScreenFactory.getScreenFromLocation(resourceName, screenName);
        if (modelScreen.getUseCache()) {
            // if in the screen definition use-cache is set to true
            // then try to get an already built screen output from the cache:
            // 1) if we find it then we get it and attach it to the passed in writer
            // 2) if we can't find one, we create a new StringWriter,
            //    and pass it to the renderScreenString;
            //    then we wrap its content and put it in the cache;
            //    and we attach it to the passed in writer
            WidgetContextCacheKey wcck = new WidgetContextCacheKey(context);
            String screenCombinedName = resourceName + ":" + screenName;
            ScreenCache screenCache = new ScreenCache();
            GenericWidgetOutput gwo = screenCache.get(screenCombinedName, wcck);
            if (gwo == null) {
                checkRunRenderInit(resourceName); // SCIPIO: new hooks
                Writer sw = new StringWriter();
                modelScreen.renderScreenString(sw, context, screenStringRenderer);
                gwo = new GenericWidgetOutput(sw.toString());
                screenCache.put(screenCombinedName, wcck, gwo);
                // SCIPIO: may render to string
                if (asString) {
                    return gwo.toString();
                } else {
                    writer.append(gwo.toString());
                }
            } else {
                // SCIPIO: may render to string
                if (asString) {
                    return gwo.toString();
                } else {
                    writer.append(gwo.toString());
                }
            }
        } else {
            checkRunRenderInit(resourceName); // SCIPIO: new hooks
            context.put("renderFormSeqNumber", String.valueOf(renderFormSeqNumber));
            // SCIPIO: may render to string
            if (asString) {
                Writer sw = new StringWriter();
                modelScreen.renderScreenString(sw, context, screenStringRenderer);
                return sw.toString();
            } else {
                modelScreen.renderScreenString(writer, context, screenStringRenderer);
            }
        }
        return "";
    }
    
    /**
     * SCIPIO: Renders the named screen using the render environment configured when this ScreenRenderer was created,
     * and (unlike the other methods) by default protects the context by pushing it before/after the render,
     * with optional boolean to bypass it.
     * <p>
     * Additionally, this method automatically handles the case where screenName is empty and included in resourceName.
     *
     * @param resourceName The name/location of the resource to use, can use "component://[component-name]/" and "ofbiz://" and other special OFBiz style URLs
     * @param screenName The name of the screen within the XML file specified by the resourceName. THIS METHOD ONLY: if empty, extracts it from resource
     * @param asString If true, returns content as string; otherwise goes direct to writer (stock behavior)
     * @parma shareScope If false (default for this method - only), protects scope by pushing the context stack
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String renderScoped(String resourceName, String screenName, Boolean asString, Boolean shareScope) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        if (asString == null) {
            asString = Boolean.FALSE;
        }
        if (!Boolean.TRUE.equals(shareScope)) { // default is FALSE for this method (only!)
            context.push();
            try {
                if (UtilValidate.isNotEmpty(screenName)) {
                    return render(resourceName, screenName, asString);
                } else {
                    return render(resourceName, asString);
                }
            } finally {
                context.pop();
            }
        } else { // if (shareScope == Boolean.TRUE)
            if (UtilValidate.isNotEmpty(screenName)) {
                return render(resourceName, screenName, asString);
            } else {
                return render(resourceName, asString);
            }
        }
    }
    
    /**
     * SCIPIO: Renders the named screen using the render environment configured when this ScreenRenderer was created,
     * and (unlike the other methods) by default protects the context by pushing it before/after the render,
     * with optional boolean to bypass it, generic type/ftl friendly version.
     * <p>
     * Additionally, this method automatically handles the case where screenName is empty and included in resourceName.
     * <p>
     * This overload accepts Objects instead of Booleans, for FTL support.
     *
     * @param resourceName The name/location of the resource to use, can use "component://[component-name]/" and "ofbiz://" and other special OFBiz style URLs
     * @param screenName The name of the screen within the XML file specified by the resourceName. THIS METHOD ONLY: if empty, extracts it from resource
     * @param asString If true, returns content as string; otherwise goes direct to writer (stock behavior)
     * @parma shareScope If false (default for this method - only), protects scope by pushing the context stack
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public String renderScopedGen(String resourceName, String screenName, Object asString, Object shareScope) throws GeneralException, IOException, SAXException, ParserConfigurationException {
        return renderScoped(resourceName, screenName, UtilMisc.booleanValue(asString), UtilMisc.booleanValue(shareScope));
    }
    
    /**
     * SCIPIO: SPECIAL INITIAL/TOP SCREEN INITIALIZATION. 
     * <p>
     * Should be run at the beginning of every screen render, just at the moment the top screen
     * render is about to start.
     */
    public void checkRunRenderInit(String resourceName) {
        if (!Boolean.TRUE.equals(context.get(WEBAPP_RENDER_INIT_GUARD))) {
            // run webapp init first (more general)
            runWebappRenderInitActions(context, resourceName);
            
            context.put(WEBAPP_RENDER_INIT_GUARD, Boolean.TRUE);
        }
        
        Set<String> localRunGuards = UtilGenerics.checkSet(context.get(LOCAL_RENDER_INIT_GUARDS));
        if (localRunGuards == null || !localRunGuards.contains(resourceName)) {
            // run local screens init last (more specific)
            runLocalRenderInitActions(context, resourceName);
            
            if (localRunGuards == null) {
                localRunGuards = new HashSet<>();
                localRunGuards.add(resourceName);
                context.put(LOCAL_RENDER_INIT_GUARDS, localRunGuards);
            }
        }
    }
     

    public void setRenderFormUniqueSeq (int renderFormSeqNumber) {
        this.renderFormSeqNumber = renderFormSeqNumber;
    }

    public ScreenStringRenderer getScreenStringRenderer() {
        return this.screenStringRenderer;
    }

    public void populateBasicContext(Map<String, Object> parameters, Delegator delegator, LocalDispatcher dispatcher, Security security, Locale locale, GenericValue userLogin) {
        populateBasicContext(context, this, parameters, delegator, dispatcher, security, locale, userLogin);
    }

    public static void populateBasicContext(MapStack<String> context, ScreenRenderer screens, Map<String, Object> parameters, Delegator delegator, LocalDispatcher dispatcher, Security security, Locale locale, GenericValue userLogin) {
        // ========== setup values that should always be in a screen context
        // include an object to more easily render screens
        context.put("screens", screens);

        // make a reference for high level variables, a global context
        context.put("globalContext", context.standAloneStack());

        // make sure the "nullField" object is in there for entity ops; note this is nullField and not null because as null causes problems in FreeMarker and such...
        context.put("nullField", GenericEntity.NULL_FIELD);

        context.put("parameters", parameters);
        context.put("delegator", delegator);
        context.put("dispatcher", dispatcher);
        context.put("security", security);
        context.put("locale", locale);
        context.put("userLogin", userLogin);
        context.put("nowTimestamp", UtilDateTime.nowTimestamp());
        try {
            Map<String, Object> result = dispatcher.runSync("getUserPreferenceGroup", UtilMisc.toMap("userLogin", userLogin, "userPrefGroupTypeId", "GLOBAL_PREFERENCES"));
            context.put("userPreferences", result.get("userPrefMap"));
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error while getting user preferences: ", module);
        }
    }

    /**
     * This method populates the context for this ScreenRenderer based on the HTTP Request and Response objects and the ServletContext.
     * It leverages various conventions used in other places, namely the ControlServlet and so on, of OFBiz to get the different resources needed.
     *
     * @param request
     * @param response
     * @param servletContext
     */
    public void populateContextForRequest(HttpServletRequest request, HttpServletResponse response, ServletContext servletContext) {
        populateContextForRequest(context, this, request, response, servletContext);
    }

    @SuppressWarnings("rawtypes")
    public static void populateContextForRequest(MapStack<String> context, ScreenRenderer screens, HttpServletRequest request, HttpServletResponse response, ServletContext servletContext) {
        HttpSession session = request.getSession();

        // attribute names to skip for session and application attributes; these are all handled as special cases, duplicating results and causing undesired messages
        Set<String> attrNamesToSkip = UtilMisc.toSet("delegator", "dispatcher", "security", "webSiteId",
                "org.apache.catalina.jsp_classpath");
        Map<String, Object> parameterMap = UtilHttp.getCombinedMap(request, attrNamesToSkip);

        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");

        populateBasicContext(context, screens, parameterMap, (Delegator) request.getAttribute("delegator"),
                (LocalDispatcher) request.getAttribute("dispatcher"),
                (Security) request.getAttribute("security"), UtilHttp.getLocale(request), userLogin);

        context.put("autoUserLogin", session.getAttribute("autoUserLogin"));
        context.put("person", session.getAttribute("person"));
        context.put("partyGroup", session.getAttribute("partyGroup"));

        // some things also seem to require this, so here it is:
        request.setAttribute("userLogin", userLogin);

        // set up the user's time zone
        context.put("timeZone", UtilHttp.getTimeZone(request));

        // ========== setup values that are specific to OFBiz webapps

        context.put("request", request);
        context.put("response", response);
        context.put("session", session);
        context.put("application", servletContext);
        if (session != null) {
            context.put("webappName", session.getAttribute("_WEBAPP_NAME_"));
        }
        if (servletContext != null) {
            String rootDir = (String) context.get("rootDir");
            String webSiteId = (String) context.get("webSiteId");
            String https = (String) context.get("https");
            if (UtilValidate.isEmpty(rootDir)) {
                rootDir = servletContext.getRealPath("/");
                context.put("rootDir", rootDir);
            }
            if (UtilValidate.isEmpty(webSiteId)) {
                webSiteId = WebSiteWorker.getWebSiteId(request);
                context.put("webSiteId", webSiteId);
            }
            if (UtilValidate.isEmpty(https)) {
                https = (String) servletContext.getAttribute("https");
                context.put("https", https);
            }
        }
        context.put("javaScriptEnabled", Boolean.valueOf(UtilHttp.isJavaScriptEnabled(request)));

        // these ones are FreeMarker specific and will only work in FTL templates, mainly here for backward compatibility
        context.put("sessionAttributes", new HttpSessionHashModel(session, FreeMarkerWorker.getDefaultOfbizWrapper()));
        context.put("requestAttributes", new HttpRequestHashModel(request, FreeMarkerWorker.getDefaultOfbizWrapper()));
        TaglibFactory JspTaglibs = new TaglibFactory(servletContext);
        context.put("JspTaglibs", JspTaglibs);
        context.put("requestParameters",  UtilHttp.getParameterMap(request));
       
        ServletContextHashModel ftlServletContext = (ServletContextHashModel) request.getAttribute("ftlServletContext");
        context.put("Application", ftlServletContext);
        context.put("Request", context.get("requestAttributes"));
 
        // this is a dummy object to stand-in for the JPublish page object for backward compatibility
        context.put("page", new HashMap());

        // some information from/about the ControlServlet environment
        context.put("controlPath", request.getAttribute("_CONTROL_PATH_"));
        context.put("contextRoot", request.getAttribute("_CONTEXT_ROOT_"));
        context.put("serverRoot", request.getAttribute("_SERVER_ROOT_URL_"));
        context.put("checkLoginUrl", LoginWorker.makeLoginUrl(request));
        String externalLoginKey = LoginWorker.getExternalLoginKey(request);
        String externalKeyParam = externalLoginKey == null ? "" : "&amp;externalLoginKey=" + externalLoginKey;
        context.put("externalLoginKey", externalLoginKey);
        context.put("externalKeyParam", externalKeyParam);

        // setup message lists
        List<String> eventMessageList = UtilGenerics.toList(request.getAttribute("eventMessageList"));
        if (eventMessageList == null) eventMessageList = new LinkedList<String>();
        List<String> errorMessageList = UtilGenerics.toList(request.getAttribute("errorMessageList"));
        if (errorMessageList == null) errorMessageList = new LinkedList<String>();

        if (request.getAttribute("_EVENT_MESSAGE_") != null) {
            eventMessageList.add(UtilFormatOut.replaceString((String) request.getAttribute("_EVENT_MESSAGE_"), "\n", "<br/>"));
            request.removeAttribute("_EVENT_MESSAGE_");
        }
        List<String> msgList = UtilGenerics.toList(request.getAttribute("_EVENT_MESSAGE_LIST_"));
        if (msgList != null) {
            eventMessageList.addAll(msgList);
            request.removeAttribute("_EVENT_MESSAGE_LIST_");
        }
        if (request.getAttribute("_ERROR_MESSAGE_") != null) {
            errorMessageList.add(UtilFormatOut.replaceString((String) request.getAttribute("_ERROR_MESSAGE_"), "\n", "<br/>"));
            request.removeAttribute("_ERROR_MESSAGE_");
        }
        if (session.getAttribute("_ERROR_MESSAGE_") != null) {
            errorMessageList.add(UtilFormatOut.replaceString((String) session.getAttribute("_ERROR_MESSAGE_"), "\n", "<br/>"));
            session.removeAttribute("_ERROR_MESSAGE_");
        }
        msgList = UtilGenerics.toList(request.getAttribute("_ERROR_MESSAGE_LIST_"));
        if (msgList != null) {
            errorMessageList.addAll(msgList);
            request.removeAttribute("_ERROR_MESSAGE_LIST_");
        }
        context.put("eventMessageList", eventMessageList);
        context.put("errorMessageList", errorMessageList);

        if (request.getAttribute("serviceValidationException") != null) {
            context.put("serviceValidationException", request.getAttribute("serviceValidationException"));
            request.removeAttribute("serviceValidationException");
        }

        // if there was an error message, this is an error
        context.put("isError", errorMessageList.size() > 0 ? Boolean.TRUE : Boolean.FALSE);
        // if a parameter was passed saying this is an error, it is an error
        if ("true".equals(parameterMap.get("isError"))) {
            context.put("isError", Boolean.TRUE);
        }

        // SCIPIO: set the request method for easy access. it is UPPERCASE.
        context.put("requestMethod", request.getMethod().toUpperCase());
        
        // SCIPIO: ensure rendererVisualThemeResources has been set (only other central place for this call would be render() method)
        VisualThemeWorker.getVisualThemeResources(context);
        
        // SCIPIO: General context scripts and per-webapp context scripts
        populateContextScripts(context);
        populateWebappContextScripts(context, request, response, servletContext);
        
        // to preserve these values, push the MapStack
        context.push();
    }
    
    public Map<String, Object> getContext() {
        return context;
    }

    public void populateContextForService(DispatchContext dctx, Map<String, Object> serviceContext) {
        this.populateBasicContext(serviceContext, dctx.getDelegator(), dctx.getDispatcher(),
                dctx.getSecurity(), (Locale) serviceContext.get("locale"), (GenericValue) serviceContext.get("userLogin"));
        
        // SCIPIO: ensure rendererVisualThemeResources has been set (only other central place for this call would be render() method)
        VisualThemeWorker.getVisualThemeResources(context);
        
        populateContextScripts(serviceContext);
    }
    
    /**
     * SCIPIO: Calls scripts defined in widgetContextScripts.properties to help populate root context.
     * <p>
     * Should be called after rest of context populated and scripts will fish out what they need out of the context.
     */
    public static void populateContextScripts(Map<String, Object> context) {
        // SCIPIO: runs scripts on initial render context to help populate
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        Enumeration<URL> resources;
        try {
            resources = loader.getResources("widgetContextScripts.properties");
        } catch (IOException e) {
            Debug.logError(e, "Could not load list of widgetContextScripts.properties", module);
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
        while (resources.hasMoreElements()) {
            URL propertyURL = resources.nextElement();
            if (Debug.infoOn()) {
                Debug.logInfo("loading properties: " + propertyURL, module);
            }
            Properties props = UtilProperties.getProperties(propertyURL);
            if (UtilValidate.isNotEmpty(props)) {
                for (Iterator<Object> i = props.keySet().iterator(); i.hasNext();) {
                    String key = (String) i.next();
                    String scriptLocation = props.getProperty(key);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("Running widget context script " + scriptLocation, module);
                    }
                    
                    String location = WidgetWorker.getScriptLocation(scriptLocation);
                    String method = WidgetWorker.getScriptMethodName(scriptLocation);
                    ScriptUtil.executeScript(location, method, context);
                }
            }
        }
    }

    /**
     * SCIPIO: Calls the script named by widgetContextScriptLocation init-parameter in web.xml.
     * <p>
     * Should be called after rest of context populated and scripts will fish out what they need out of the context,
     * and after #populateContextScripts, because these are more specific.
     */
    public static void populateWebappContextScripts(Map<String, Object> context, 
            HttpServletRequest request, HttpServletResponse response, ServletContext servletContext) {
        // first run generic groovy/minilang script
        String fullLoc = getWebappContextScriptLocation(request, servletContext);
        if (fullLoc != null) {
            String location = WidgetWorker.getScriptLocation(fullLoc);
            String method = WidgetWorker.getScriptMethodName(fullLoc);
            ScriptUtil.executeScript(location, method, context);
        }
    }
    
    /**
     * SCIPIO: Return webapp-specific context script location based on web.xml configuration:
     * renderContextScriptLocation init-parameter.
     */
    public static String getWebappContextScriptLocation(HttpServletRequest request, ServletContext servletContext) {
        String res = (String) servletContext.getAttribute("widgetContextScriptLocation");
        if (res != null && !res.isEmpty()) {
            return res;
        }
        return null;
    }
    
    /**
     * SCIPIO: Runs webapp-specific render init actions.
     */
    public static void runWebappRenderInitActions(Map<String, Object> context, String resource) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        ServletContext servletContext = (ServletContext) context.get("servletContext");
        if (request == null) {
            return;
        }
        if (servletContext == null) {
            servletContext = request.getServletContext(); // NOTE: new in Servlet API 3.0
        }
        
        // next run screen-based one
        ModelLocation modelLoc = getRenderInitScriptScreenLocation(request, servletContext);        
        if (modelLoc.hasResource()) {
            ModelScreen scriptScreen;
            try {
                scriptScreen = ScreenFactory.getScreenFromLocationOrNull(modelLoc.getResource(), 
                        modelLoc.getName());
            } catch (Exception e) {
                Debug.logError(e, "Could not resolve render init script screen location: " + modelLoc, module);
                scriptScreen = null;
            }
            if (scriptScreen != null) {
                try {
                    AbstractModelAction.runSubActions(scriptScreen.getSection().getActions(), context);
                } catch (Exception e) {
                    Debug.logError(e, "Error running render init screen actions [" + modelLoc.toString() + "]", module);
                }
            }
        }
    }
    
    /**
     * SCIPIO: Return render init script screen location based on web.xml configuration:
     * renderInitScriptScreenLocation init-parameter, with fallback on mainDecoratorLocation.
     * The default screen name is "webapp-common-actions".
     */
    public static ModelLocation getRenderInitScriptScreenLocation(HttpServletRequest request, ServletContext servletContext) {
        ModelLocation modelLoc = (ModelLocation) servletContext.getAttribute("_RIS_screenModelLoc");
        if (modelLoc != null) { // NOTE: no need for synchronization; getAttribute is thread safe and races don't matter
            return modelLoc;
        }
        
        String strLoc = (String) servletContext.getAttribute("renderInitScriptScreenLocation");
        if (strLoc != null && !strLoc.isEmpty()) {
            if (!strLoc.contains("#")) { // is name only
                String mainDec = (String) servletContext.getAttribute("mainDecoratorLocation");
                if (mainDec != null && !mainDec.isEmpty()) {
                    modelLoc = ModelLocation.fromResAndName(mainDec, strLoc);
                }
            } else {
                modelLoc = ModelLocation.fromAddress(strLoc);
            }
        } else {
            strLoc = (String) servletContext.getAttribute("mainDecoratorLocation");
            if (strLoc != null && !strLoc.isEmpty()) {
                modelLoc = ModelLocation.fromResAndName(strLoc, "webapp-common-actions");
            }
        }
        
        if (modelLoc == null) {
            modelLoc = ModelLocation.EMPTY_LOCATION;
        }
        // cache it
        servletContext.setAttribute("_RIS_screenModelLoc", modelLoc);
        return modelLoc;
    }
    
    /**
     * SCIPIO: Runs local render init actions. NOTE: can be more than one of these run in a request if
     * screens from multiple webapps are involved.
     */
    public static void runLocalRenderInitActions(Map<String, Object> context, String resource) {
        List<ModelAction> actions;
        try {
            actions = ScreenFactory.getScreensFromLocation(resource).getEffectiveSettings().getLocalRenderInitActions();
        } catch (Exception e) {
            Debug.logError(e, "Could not resolve render init script screen location from "
                    + "web.xml renderInitScriptScreenLocation/mainDecoratorLocation config: " + resource, module);
            actions = null;
        }
        if (actions != null) {
            try {
                AbstractModelAction.runSubActions(actions, context);
            } catch (Exception e) {
                Debug.logError(e, "Error running local render init screen actions for [" + resource + "]", module);
            }
        }
    }
}
