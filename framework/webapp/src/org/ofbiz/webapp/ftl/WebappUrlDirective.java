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

package org.ofbiz.webapp.ftl;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.Debug.OfbizLogger;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.FtlTransformFactory;
import org.ofbiz.catalina.container.ScipioConnectorInfo;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.renderer.RenderEnvType;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: Standard library implementations (excluding CMS) for pageUrl, appUrl, servletUrl
 * and their function equivalents.
 * <p>
 * Note that "Webapp" in this class's name refers to the general webapp concept as well
 * as the package it's in.
 * <p>
 * This replaces the removed <code>OfbizUrlTransform</code> class.
 * <p>
 * Added 2019-01-28.
 */
public abstract class WebappUrlDirective implements TemplateDirectiveModel {
    public static final DefaultParams DEFAULT_PARAMS = DefaultParams.NULL;

    static final TemplateModel[] NO_LOOP_VARS = new TemplateModel[] {}; // Do not modify.
    
    private WebappUrlDirective() {}

    public static class DefaultParams {
        public static final DefaultParams NULL = new DefaultParams();

        Boolean rawParams;
        Boolean strict;
        Boolean interWebapp;
        Boolean controller;
        Boolean absPath;
        Boolean extLoginKey;
        String paramDelim;
        Boolean emptyIfMissing;


        public DefaultParams(DefaultParams other) {
            this.rawParams = other.rawParams;
            this.strict = other.strict;
            this.interWebapp = other.interWebapp;
            this.controller = other.controller;
            this.absPath = other.absPath;
            this.extLoginKey = other.extLoginKey;
            this.paramDelim = other.paramDelim;
            this.emptyIfMissing = other.emptyIfMissing;
        }
        public DefaultParams() { }

        public Boolean getRawParams() { return rawParams; }
        public DefaultParams setRawParams(Boolean rawParams) { this.rawParams = rawParams; return this; }
        public Boolean getStrict() { return strict; }
        public DefaultParams setStrict(Boolean strict) { this.strict = strict; return this; }
        public Boolean getInterWebapp() { return interWebapp; }
        public DefaultParams setInterWebapp(Boolean interWebapp) { this.interWebapp = interWebapp; return this; }
        public Boolean getController() { return controller; }
        public DefaultParams setController(Boolean controller) { this.controller = controller; return this; }
        public Boolean getAbsPath() { return absPath; }
        public DefaultParams setAbsPath(Boolean absPath) { this.absPath = absPath; return this; }
        public Boolean getExtLoginKey() { return extLoginKey; }
        public DefaultParams setExtLoginKey(Boolean extLoginKey) { this.extLoginKey = extLoginKey; return this; }
        public String getParamDelim() { return paramDelim; }
        public DefaultParams setParamDelim(String paramDelim) { this.paramDelim = paramDelim; return this; }
        public Boolean getEmptyIfMissing() { return emptyIfMissing; }
        public DefaultParams setEmptyIfMissing(Boolean emptyIfMissing) { this.emptyIfMissing = emptyIfMissing; return this; }
    }

    public abstract DefaultParams getDefaultParams();
    protected abstract Debug.OfbizLogger getModule();

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body) throws TemplateException, IOException {
        execute(env, args, loopVars, body, env.getOut(), null, null, getDefaultParams());
    }

    protected void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body,
            Writer out, TemplateModel uriModel, TemplateModel webSiteIdModel, DefaultParams defaultParams) throws TemplateException, IOException {
        String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // new
        Boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams"); // new
        if (rawParams == null) {
            rawParams = (defaultParams.rawParams != null) ? defaultParams.rawParams :
                UtilValidate.isNotEmpty(escapeAs); // if we're post-escaping, we can assume we should get rawParams
        }
        Boolean strict = TransformUtil.getBooleanArg(args, "strict"); // new
        if (strict == null) {
            strict = (defaultParams.strict != null) ? defaultParams.strict : UtilValidate.isNotEmpty(escapeAs); // if we're post-escaping, we can assume we want strict handling
        }

        // We now support a "uri" arg as alternative to #nested
        String uri = (uriModel != null) ? TransformUtil.getStringArg(uriModel, rawParams)
                : TransformUtil.getStringArg(args, "uri", rawParams);

        // more new parameters
        String type = TransformUtil.getStringArg(args, "type", rawParams);
        Boolean absPath = TransformUtil.getBooleanArg(args, "absPath", defaultParams.absPath);
        Boolean interWebapp = TransformUtil.getBooleanArg(args, "interWebapp", defaultParams.interWebapp); // Alias for type="inter-webapp"
        Boolean controller = TransformUtil.getBooleanArg(args, "controller", defaultParams.controller);
        boolean extLoginKey = TransformUtil.getBooleanArg(args, "extLoginKey",
                defaultParams.extLoginKey != null ? defaultParams.extLoginKey : false);
        // NOTE: the default for paramDelim is highly heuristic... for now just follow rawParams (even though it's not its exact meaning)
        String paramDelimDefault = rawParams ? "&" : "&amp;";
        String paramDelim = TransformUtil.getStringArg(args, "paramDelim", paramDelimDefault, true, true);
        String paramStr = TransformUtil.getParamString(args, "params", paramDelim, rawParams);
        Boolean emptyIfMissing = TransformUtil.getBooleanArg(args, "emptyIfMissing", defaultParams.emptyIfMissing);

        try {
            if (body != null) {
                StringWriter buf = new StringWriter();
                body.render(buf);
                uri = buf.toString();
            } else if (uri == null) {
                throw new TemplateException("Cannot build URL: missing path/uri (null)", env);
            }
            uri = TemplateFtlUtil.appendParamString(uri, paramStr);
            // TODO: Map support
            //Object paramsObj = args.get("params");
            //if (paramsObj instanceof String) {
            //    uri = TemplateFtlUtil.appendParamString(uri, (String) paramsObj);
            //} else if (paramsObj instanceof Map) {
            //    uri = appendUrlParams(uri, UtilGenerics.cast(paramsObj), paramDelim);
            //}

            HttpServletRequest request = FreeMarkerWorker.unwrap(env.getVariable("request"));
            HttpServletResponse response = FreeMarkerWorker.unwrap(env.getVariable("response"));
            RenderEnvType renderEnvType = ContextFtlUtil.getRenderEnvType(env, request);
            FullWebappInfo.Cache webappInfoCache = ContextFtlUtil.getWebappInfoCacheAndCurrent(env, request, renderEnvType);
            Delegator delegator = ContextFtlUtil.getDelegator(request, env);

            Boolean fullPath = UrlTransformUtil.determineFullPath(TransformUtil.getBooleanArg(args, "fullPath"), renderEnvType, env);
            Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // modified to remove default; leave centralized
            Boolean encode = TransformUtil.getBooleanArg(args, "encode"); // modified to remove default; leave centralized
            String webSiteId = (webSiteIdModel != null) ? TransformUtil.getStringArg(webSiteIdModel, rawParams) :
                    TransformUtil.getStringArg(args, "webSiteId", rawParams);

            Boolean interWebappEff = interWebapp;
            if (interWebappEff == null) {
                if (type == null || type.isEmpty()) {
                    ; // leave it to method
                } else if ("intra-webapp".equals(type)) {
                    interWebappEff = false;
                } else if ("inter-webapp".equals(type)) {
                    interWebappEff = true;
                }
            }

            String requestUrl = uri;
            if (request != null) {
                // If requested, add external login key
                if (extLoginKey) {
                    requestUrl = RequestLinkUtil.checkAddExternalLoginKey(requestUrl, request, paramDelim);
                }
                // Now use more advanced method
                //RequestHandler rh = (RequestHandler) request.getServletContext().getAttribute("_REQUEST_HANDLER_"); // reworked
                //out.write(rh.makeLink(request, response, requestUrl, fullPath, secure, encode));
                String link = RequestHandler.makeLinkAuto(request, response, requestUrl, absPath, interWebappEff, webSiteId, controller, fullPath, secure, encode);
                if (link != null && !link.isEmpty()) {
                    link = checkForceHost(link, args, secure,false);
                    output(UrlTransformUtil.escapeGeneratedUrl(link, escapeAs, strict, env), out);
                } else if (!Boolean.TRUE.equals(emptyIfMissing)) {
                    // SCIPIO 3.0.0: Output link name as fallback by default; previously left empty for templates to catch but this is rarely needed anyway
                    output(UrlTransformUtil.escapeGeneratedUrl(requestUrl, escapeAs, strict, env), out);
                }
            } else if (webSiteId != null || webappInfoCache.getCurrentWebappWebSiteId() != null) {
                Locale locale = TransformUtil.getOfbizLocaleArgOrCurrent(args, "locale", env);
                String link = RequestHandler.makeLinkAuto(ContextFtlUtil.getContext(env), delegator, locale, webSiteId, requestUrl, absPath,
                        interWebappEff, controller, fullPath, secure, encode);
                if (link != null && !link.isEmpty()) {
                    link = checkForceHost(link, args, secure, false);
                    output(UrlTransformUtil.escapeGeneratedUrl(link, escapeAs, strict, env), out);
                } else if (!Boolean.TRUE.equals(emptyIfMissing)) {
                    // SCIPIO 3.0.0: Output link name as fallback by default; previously left empty for templates to catch but this is rarely needed anyway
                    output(UrlTransformUtil.escapeGeneratedUrl(requestUrl, escapeAs, strict, env), out);
                }
            } else {
                // DEPRECATED - TODO: REMOVE
                // Handle prefix.
                String prefixString = TransformUtil.getStringNonEscapingArg(args, "urlPrefix", "");
                if (prefixString.isEmpty() && request == null) {
                    // for emails only: check for urlPrefix in the environment
                    prefixString = TransformUtil.getStringNonEscapingArg(env.getVariable("urlPrefix"), "");
                }
                if (!prefixString.isEmpty()) {
                    String bufString = requestUrl;
                    boolean prefixSlash = prefixString.endsWith("/");
                    boolean bufSlash = bufString.startsWith("/");
                    if (prefixSlash && bufSlash) {
                        bufString = bufString.substring(1);
                    } else if (!prefixSlash && !bufSlash) {
                        bufString = "/" + bufString;
                    }
                    output(prefixString + bufString, out);
                    Debug.logWarning("Using DEPRECATED fallback URL building mode because context is incomplete (url: "
                            + prefixString + bufString + ")", getModule());
                } else {
                    Debug.logWarning("Using DEPRECATED fallback URL building mode because context is incomplete (url: "
                            + requestUrl + ")", getModule());
                }
            }
        } catch (EnvOutIOException e) {
            Debug.logWarning("Exception in URL transform (uri=" + uri + "): " + e.ex.toString(), getModule());
            throw e.ex;
        } catch (TemplateException e) {
            Debug.logWarning("Exception in URL transform (uri=" + uri + "): " + e.toString(), getModule());
            throw e;
        } catch (Exception e) {
            Debug.logWarning("Exception in URL transform (uri=" + uri + "): " + e.toString(), getModule());
            throw new TemplateException(e, env);
        }
    }

    public String makeLinkForContext(Map<String, Object> context, Map<String, Object> args) {
        return makeLinkForContext(context, args, null, null, null, getDefaultParams());
    }

    public String makeLinkForContext(Map<String, Object> context, Map<String, Object> args, String uri, String webSiteId, Map<String, Object> baseParams) {
        return makeLinkForContext(context, args, uri, webSiteId, baseParams, getDefaultParams());
    }

    protected String makeLinkForContext(Map<String, Object> context, Map<String, Object> args, String uri, String webSiteId, Map<String, Object> baseParams, DefaultParams defaultParams) {
        if (uri == null) {
            uri = (String) args.get("uri");
            if (uri == null) {
                throw new IllegalArgumentException("Cannot build URL: missing path/uri (null)");
            }
        }
        String type = (String) args.get("type");
        Boolean absPath = (Boolean) args.get("absPath");
        if (absPath == null) {
            absPath = defaultParams.absPath;
        }
        Boolean interWebapp = (Boolean) args.get("interWebapp"); // Alias for type="inter-webapp"
        if (interWebapp == null) {
            interWebapp = defaultParams.interWebapp;
        }
        Boolean controller = (Boolean) args.get("controller");
        if (controller == null) {
            controller = defaultParams.controller;
        }
        Boolean extLoginKey = (Boolean) args.get("extLoginKey");
        if (extLoginKey == null) {
            extLoginKey = defaultParams.extLoginKey != null ? defaultParams.extLoginKey : false;
        }
        // NOTE: the default for paramDelim is highly heuristic... for now just follow rawParams (even though it's not its exact meaning)
        String paramDelim = (String) args.get("paramDelim");
        if (UtilValidate.isEmpty(paramDelim)) {
            paramDelim = "&";
        }
        Object paramsObj = args.get("params");
        if (paramsObj instanceof String) {
            // FIXME: this doesn't merge string params with the baseParams properly
            if (UtilValidate.isNotEmpty(baseParams)) {
                uri = appendUrlParams(uri, baseParams, paramDelim);
            }
            uri = TemplateFtlUtil.appendParamString(uri, (String) paramsObj);
        } else if (paramsObj instanceof Map) {
            Map<String, Object> argsParams = UtilGenerics.cast(paramsObj);
            Map<String, Object> params;
            if (UtilValidate.isNotEmpty(baseParams)) {
                if (UtilValidate.isNotEmpty(argsParams)) {
                    params = new LinkedHashMap<>(baseParams);
                    params.putAll(argsParams);
                } else {
                    params = UtilGenerics.cast(baseParams);
                }
            } else {
                params = argsParams;
            }
            uri = appendUrlParams(uri, params, paramDelim);
        }

        HttpServletRequest request = (HttpServletRequest) context.get("request");
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        RenderEnvType renderEnvType = RenderEnvType.fromRequestOrContext(request, context);
        FullWebappInfo.Cache webappInfoCache = ContextFtlUtil.getWebappInfoCacheAndCurrent(request, context, renderEnvType);
        Delegator delegator = (Delegator) ((request != null) ? request.getAttribute("delegator") : context.get("delegator"));

        Boolean fullPath = UrlTransformUtil.determineFullPath((Boolean) args.get( "fullPath"), renderEnvType);
        Boolean secure = (Boolean) args.get("secure");
        Boolean encode = (Boolean) args.get("encode");
        if (webSiteId == null) {
            webSiteId = (String) args.get("webSiteId");
        }

        Boolean interWebappEff = interWebapp;
        if (interWebappEff == null) {
            if (type == null || type.isEmpty()) {
                ; // leave it to method
            } else if ("intra-webapp".equals(type)) {
                interWebappEff = false;
            } else if ("inter-webapp".equals(type)) {
                interWebappEff = true;
            }
        }

        String requestUrl = uri;
        if (request != null) {
            // If requested, add external login key
            if (extLoginKey) {
                requestUrl = RequestLinkUtil.checkAddExternalLoginKey(requestUrl, request, paramDelim);
            }
            // Now use more advanced method
            //RequestHandler rh = (RequestHandler) request.getServletContext().getAttribute("_REQUEST_HANDLER_"); // reworked
            //out.write(rh.makeLink(request, response, requestUrl, fullPath, secure, encode));
            String link = RequestHandler.makeLinkAuto(request, response, requestUrl, absPath, interWebappEff, webSiteId, controller, fullPath, secure, encode);
            if (link != null) {
                link = checkForceHost(link, (Boolean) args.get("localhost"), secure,false);
            }
            return link;
        } else if (webSiteId != null || webappInfoCache.getCurrentWebappWebSiteId() != null) {
            Locale locale = null;
            Object localeObj = args.get("locale");
            if (localeObj != null) {
                if (localeObj instanceof Locale) {
                    locale = (Locale) localeObj;
                } else {
                    locale = UtilMisc.parseLocale((String) localeObj);
                }
            } else {
                locale = (Locale) context.get("locale");
            }
            String link = RequestHandler.makeLinkAuto(context, delegator, locale, webSiteId, requestUrl, absPath,
                    interWebappEff, controller, fullPath, secure, encode);
            if (link != null) {
                link = checkForceHost(link, (Boolean) args.get("localhost"), secure, false);
            }
            return link;
        } else {
            return null;
        }
    }

    public static String appendUrlParams(String url, Map<String, Object> params, String delim) { // TODO: move, client code should not call
        if (UtilValidate.isEmpty(params)) {
            return url;
        }
        if (UtilValidate.isEmpty(delim)) {
            delim = "&";
        }
        StringBuilder sb = new StringBuilder(url);
        String effDelim = url.contains("?") ? delim : "?";
        for(Map.Entry<String, Object> entry : params.entrySet()) {
            String name = entry.getKey();
            if (entry.getValue() instanceof Collection) {
                for(Object value : UtilGenerics.<Collection<?>>cast(entry.getValue())) {
                    sb.append(effDelim);
                    sb.append(UtilCodec.getUrlEncoder().encode(name));
                    sb.append('=');
                    if (value != null) {
                        sb.append(UtilCodec.getUrlEncoder().encode(value.toString()));
                    }
                    effDelim = delim;
                }
            } else {
                Object value = entry.getValue();
                sb.append(effDelim);
                sb.append(UtilCodec.getUrlEncoder().encode(name));
                sb.append('=');
                if (value != null) {
                    sb.append(UtilCodec.getUrlEncoder().encode(value.toString()));
                }
                effDelim = delim;
            }
        }
        return sb.toString();
    }

    private static void output(String url, Writer out) throws EnvOutIOException {
        try {
            out.write(url);
        } catch(IOException e) {
            throw new EnvOutIOException(e);
        }
    }

    /**
     * SCIPIO: NOTE: This workaround wrapper exception is here to identify only the out.write
     * calls that throw IOException, because nothing else is supposed to throw it, per TemplateDirectiveModel interface.
     */
    @SuppressWarnings("serial")
    private static class EnvOutIOException extends IOException {
        private final IOException ex;
        EnvOutIOException(IOException ex) { this.ex = ex; }
    }

    public static abstract class WebappUrlMethod implements TemplateMethodModelEx {
        public static final DefaultParams DEFAULT_PARAMS = new DefaultParams(DefaultParams.NULL)
                .setRawParams(true).setStrict(true);
        @Override
        public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
            return exec(WebappUrlDirective.getArgsMapOrNull(args), args);
        }

        public Object exec(Map<String, ? extends TemplateModel> args, List<?> posArgs) throws TemplateModelException {
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            StringWriter sw = new StringWriter();
            TemplateModel uriModel = null;
            TemplateModel webSiteIdModel = null;
            if (args == null) {
                args = Collections.emptyMap();
                // Positional parameter(s) (limited support)
                if (posArgs.size() >= 1) {
                    uriModel = (TemplateModel) posArgs.get(0); 
                    if (posArgs.size() >= 2) {
                        webSiteIdModel = (TemplateModel) posArgs.get(1);
                    }
                }
            }
            try {
                getDirective().execute(env, args, NO_LOOP_VARS, null, sw, uriModel, webSiteIdModel, getDefaultParams());
            } catch (IOException | TemplateException e) {
                throw new TemplateModelException(e);
            }
            return new SimpleScalar(sw.toString()); // NOTE: Do not return unwrapped string, due to escaping
        }

        protected abstract DefaultParams getDefaultParams();
        protected abstract Debug.OfbizLogger getModule();
        protected abstract WebappUrlDirective getDirective();
    }

    /**
     * Freemarker Transform for creating OFBiz URLs (links).
     * <p>This transform accepts several arguments:<br>
     * <ul>
     * <li><b>fullPath</b> (true/false) - generate a full URL including scheme and host, defaults to false.</li>
     * <li><b>secure</b> (true/false) - generate a secure (https) URL, defaults to false. Server settings will
     * override this argument.</li>
     * <li><b>encode</b> (true/false) - encode the URL, defaults to true. Encoding is UTF-8.</li>
     * <li><b>webSiteId</b> - generate a full URL using the web site settings found in the WebSite entity.</li>
     * </ul></p>
     * <p>In addition, this transform accepts an environment variable - <b>urlPrefix</b>. If the variable
     * exists, it is prepended to the contents of the transform (the part between
     * <code>&lt;@pageUrl&gt;</code> and <code>&lt;/@pageUrl&gt;</code>), and all transform arguments are
     * ignored.</p>
     */
    public static class PageUrlDirective extends WebappUrlDirective { // Upgraded interface (2019-01-28): //implements TemplateTransformModel
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlDirective.DEFAULT_PARAMS));
        public static final DefaultParams configureParams(DefaultParams defaultParams) {
            // 2019-01-28: For now, nothing to do because the WebappUrlDirective core implementation
            // was simply taken from pageUrl.
            return defaultParams; 
        }
        public static class Factory implements FtlTransformFactory { // SCIPIO
            private static final PageUrlDirective DEFAULT = new PageUrlDirective();
            @Override
            public TemplateModel getTransform(ClassLoader loader) {
                return DEFAULT;
            }
        }
        public static PageUrlDirective getInstance() { // SCIPIO
            return Factory.DEFAULT;
        }
        public static PageUrlDirective create() { // See CMS's PageUrlDirective
            return new PageUrlDirective();
        }
        @Override
        public DefaultParams getDefaultParams() {
            return DEFAULT_PARAMS;
        }
        @Override
        protected OfbizLogger getModule() {
            return module;
        }

        public static class Method extends WebappUrlMethod {
            private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
            public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlMethod.DEFAULT_PARAMS));
            public static class Factory implements FtlTransformFactory { // SCIPIO
                private static final Method DEFAULT = new Method();
                @Override
                public TemplateModel getTransform(ClassLoader loader) {
                    return DEFAULT;
                }
            }
            public static Method getInstance() {
                return Factory.DEFAULT;
            }
            public static Method create() { // See CMS's PageUrlDirective
                return new Method();
            }
            @Override
            protected DefaultParams getDefaultParams() {
                return DEFAULT_PARAMS;
            }
            @Override
            protected OfbizLogger getModule() {
                return module;
            }
            @Override
            protected PageUrlDirective getDirective() {
                return PageUrlDirective.getInstance();
            }
        }
    }

    public static class AppUrlDirective extends WebappUrlDirective {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlDirective.DEFAULT_PARAMS));
        public static final DefaultParams configureParams(DefaultParams defaultParams) {
            return defaultParams.setAbsPath(false).setController(false);
            // This is now determined based on webSiteId presence, in makeLinkAuto: .setInterWebapp(false);
        }
        public static class Factory implements FtlTransformFactory { // SCIPIO
            private static final AppUrlDirective DEFAULT = new AppUrlDirective();
            @Override
            public TemplateModel getTransform(ClassLoader loader) {
                return DEFAULT;
            }
        }
        public static AppUrlDirective getInstance() { // SCIPIO
            return Factory.DEFAULT;
        }
        public static AppUrlDirective create() {
            return new AppUrlDirective();
        }
        @Override
        public DefaultParams getDefaultParams() {
            return DEFAULT_PARAMS;
        }
        @Override
        protected OfbizLogger getModule() {
            return module;
        }

        public static class Method extends WebappUrlMethod {
            private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
            public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlMethod.DEFAULT_PARAMS));
            public static class Factory implements FtlTransformFactory { // SCIPIO
                private static final Method DEFAULT = new Method();
                @Override
                public TemplateModel getTransform(ClassLoader loader) {
                    return DEFAULT;
                }
            }
            public static Method getInstance() {
                return Factory.DEFAULT;
            }
            public static Method create() { // See CMS's PageUrlDirective
                return new Method();
            }
            @Override
            protected DefaultParams getDefaultParams() {
                return DEFAULT_PARAMS;
            }
            @Override
            protected OfbizLogger getModule() {
                return module;
            }
            @Override
            protected AppUrlDirective getDirective() {
                return AppUrlDirective.getInstance();
            }
        }
    }

    public static class ServerUrlDirective extends WebappUrlDirective {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlDirective.DEFAULT_PARAMS));
        public static final DefaultParams configureParams(DefaultParams defaultParams) {
            return defaultParams.setInterWebapp(true);
        }
        public static class Factory implements FtlTransformFactory {
            private static final ServerUrlDirective DEFAULT = new ServerUrlDirective();
            @Override
            public TemplateModel getTransform(ClassLoader loader) {
                return DEFAULT;
            }
        }
        public static ServerUrlDirective getInstance() {
            return Factory.DEFAULT;
        }
        public static ServerUrlDirective create() {
            return new ServerUrlDirective();
        }
        @Override
        public DefaultParams getDefaultParams() {
            return DEFAULT_PARAMS;
        }
        @Override
        protected OfbizLogger getModule() {
            return module;
        }

        public static class Method extends WebappUrlMethod {
            private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
            public static final DefaultParams DEFAULT_PARAMS = configureParams(new DefaultParams(WebappUrlMethod.DEFAULT_PARAMS));
            public static class Factory implements FtlTransformFactory {
                private static final Method DEFAULT = new Method();
                @Override
                public TemplateModel getTransform(ClassLoader loader) {
                    return DEFAULT;
                }
            }
            public static Method getInstance() {
                return Factory.DEFAULT;
            }
            public static Method create() {
                return new Method();
            }
            @Override
            protected DefaultParams getDefaultParams() {
                return DEFAULT_PARAMS;
            }
            @Override
            protected OfbizLogger getModule() {
                return module;
            }
            @Override
            protected ServerUrlDirective getDirective() {
                return ServerUrlDirective.getInstance();
            }
        }
    }

    // For reuse in implementations
    public static Map<String, TemplateModel> getArgsMapOrNull(List<?> args) throws TemplateModelException {
        TemplateModel firstArg = (TemplateModel) args.get(0);
        if (LangFtlUtil.isObjectType("map", firstArg)) {
            // Copy not needed for us (we won't modify it)
            //linkArgs = LangFtlUtil.makeModelMap((TemplateHashModelEx) firstArg);
            return LangFtlUtil.getWrappedOrAdaptAsMap((TemplateHashModelEx) firstArg);
        }
        return null;
    }

    public static String checkForceHost(String url, Map<String, TemplateModel> args, Boolean secure, boolean isContentUrl) throws TemplateModelException {
        return checkForceHost(url, TransformUtil.getBooleanArg(args, "localhost"), secure, isContentUrl);
    }

    /** Workaround for force localhost integration. WARN: subject to change */
    public static String checkForceHost(String url, Boolean forceLocal, Boolean secure, boolean isContentUrl) {
        if (UtilValidate.isEmpty(url) || !Boolean.TRUE.equals(forceLocal)) {
            return url;
        }
        StringBuilder sb;
        int hostEnd = -1;
        int port;
        if (url.startsWith("https://")) {
            hostEnd = url.indexOf('/', "https://".length());
            sb = new StringBuilder("https://");
            ScipioConnectorInfo httpsConnectorInfo = ScipioConnectorInfo.getWebContainer(true);
            port = (httpsConnectorInfo != null) ? httpsConnectorInfo.getPort() : 443;
        } else if (url.startsWith("http://")) {
            hostEnd = url.indexOf('/', "http://".length());
            sb = new StringBuilder("http://");
            ScipioConnectorInfo httpsConnectorInfo = ScipioConnectorInfo.getWebContainer(false);
            port = (httpsConnectorInfo != null) ? httpsConnectorInfo.getPort() : 80;
        } else if (url.startsWith("//")) {
            hostEnd = url.indexOf('/', "//".length());
            sb = new StringBuilder("//");
            ScipioConnectorInfo httpsConnectorInfo = ScipioConnectorInfo.getWebContainer(true);
            port = (httpsConnectorInfo != null) ? httpsConnectorInfo.getPort() : 443;
        } else {
            if (url.contains("://")) { // TODO: REVIEW: don't do others because we can't get a port
                return url;
            }
            if (Boolean.FALSE.equals(secure)) {
                sb = new StringBuilder("http://");
                hostEnd = 0;
                ScipioConnectorInfo httpsConnectorInfo = ScipioConnectorInfo.getWebContainer(false);
                port = (httpsConnectorInfo != null) ? httpsConnectorInfo.getPort() : 80;
            } else {
                sb = new StringBuilder("https://");
                hostEnd = 0;
                ScipioConnectorInfo httpsConnectorInfo = ScipioConnectorInfo.getWebContainer(true);
                port = (httpsConnectorInfo != null) ? httpsConnectorInfo.getPort() : 443;
            }
        }
        if (port != 80 && port != 443) {
            sb.append("localhost:");
            sb.append(port);
        } else {
            sb.append("localhost");
        }
        if (hostEnd >= 0) {
            sb.append(url.substring(hostEnd));
        }
        return sb.toString();
    }
}
