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
import java.io.Writer;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.renderer.RenderEnvType;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateTransformModel;

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
 * <code>&lt;@ofbizUrl&gt;</code> and <code>&lt;/@ofbizUrl&gt;</code>), and all transform arguments are
 * ignored.</p>
 *
 */
public class OfbizUrlTransform implements TemplateTransformModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    @SuppressWarnings("rawtypes")
    public Writer getWriter(final Writer out, final Map args) throws TemplateModelException {
        final StringBuilder buf = new StringBuilder();

        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new

        // SCIPIO: We now support a "uri" arg as alternative to #nested
        final String uriArg = TransformUtil.getStringArg(args, "uri", rawParams);
        // SCIPIO: more new parameters
        final String type = TransformUtil.getStringArg(args, "type", rawParams);
        final Boolean absPath = TransformUtil.getBooleanArg(args, "absPath");
        final Boolean interWebapp = TransformUtil.getBooleanArg(args, "interWebapp"); // Alias for type="inter-webapp"
        final Boolean controller = TransformUtil.getBooleanArg(args, "controller");
        final boolean extLoginKey = TransformUtil.getBooleanArg(args, "extLoginKey", false);
        // NOTE: the default for paramDelim is highly heuristic... for now just follow rawParams (even though it's not its exact meaning)
        final String paramDelimDefault = rawParams ? "&" : "&amp;";
        final String paramDelim = TransformUtil.getStringArg(args, "paramDelim", paramDelimDefault, true, true);

        return new Writer(out) {

            @Override
            public void close() throws IOException {
                try {
                    // SCIPIO: can use uri instead of buffer
                    if (uriArg != null) {
                        buf.append(uriArg);
                    }

                    Environment env = FreeMarkerWorker.getCurrentEnvironment();
                    HttpServletRequest request = FreeMarkerWorker.unwrap(env.getVariable("request"));
                    HttpServletResponse response = FreeMarkerWorker.unwrap(env.getVariable("response"));
                    RenderEnvType renderEnvType = ContextFtlUtil.getRenderEnvType(env, request);
                    FullWebappInfo.Cache webappInfoCache = ContextFtlUtil.getWebappInfoCacheAndCurrent(env, request, renderEnvType);
                    Delegator delegator = ContextFtlUtil.getDelegator(request, env);

                    final Boolean fullPath = UrlTransformUtil.determineFullPath(TransformUtil.getBooleanArg(args, "fullPath"), renderEnvType, env);
                    final Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO: modified to remove default; leave centralized
                    final Boolean encode = TransformUtil.getBooleanArg(args, "encode"); // SCIPIO: modified to remove default; leave centralized
                    final String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams);

                    Boolean interWebappEff = interWebapp;
                    if (interWebappEff == null) {
                        if (type == null || type.isEmpty()) {
                            ; // leave it to method
                        }
                        else if ("intra-webapp".equals(type)) {
                            interWebappEff = false;
                        }
                        else if ("inter-webapp".equals(type)) {
                            interWebappEff = true;
                        }
                    }

                    String requestUrl = buf.toString();
                    if (request != null) {
                        // SCIPIO: If requested, add external login key
                        if (extLoginKey) {
                            requestUrl = RequestLinkUtil.checkAddExternalLoginKey(requestUrl, request, paramDelim);
                        }
                        // SCIPIO: Now use more advanced method
                        //RequestHandler rh = (RequestHandler) request.getServletContext().getAttribute("_REQUEST_HANDLER_"); // SCIPIO: reworked
                        //out.write(rh.makeLink(request, response, requestUrl, fullPath, secure, encode));
                        String link = RequestHandler.makeLinkAuto(request, response, requestUrl, absPath, interWebappEff, webSiteId, controller, fullPath, secure, encode);
                        if (link != null) {
                            out.write(UrlTransformUtil.escapeGeneratedUrl(link, escapeAs, strict, env));
                        } else {
                            // SCIPIO: If link is null, it means there was an error building link; write nothing, so that
                            // it's possible for templates to catch this case if they need to.
                            //out.write(requestUrl);
                        }
                    } else if (webSiteId != null || webappInfoCache.getCurrentWebappWebSiteId() != null) {
                        Locale locale = TransformUtil.getOfbizLocaleArgOrContextOrRequest(args, "locale", env);
                        String link = RequestHandler.makeLinkAuto(ContextFtlUtil.getContext(env), delegator, locale, webSiteId, requestUrl, absPath,
                                interWebappEff, controller, fullPath, secure, encode);
                        if (link != null) {
                            out.write(UrlTransformUtil.escapeGeneratedUrl(link, escapeAs, strict, env));
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
                            String bufString = buf.toString();
                            boolean prefixSlash = prefixString.endsWith("/");
                            boolean bufSlash = bufString.startsWith("/");
                            if (prefixSlash && bufSlash) {
                                bufString = bufString.substring(1);
                            } else if (!prefixSlash && !bufSlash) {
                                bufString = "/" + bufString;
                            }
                            out.write(prefixString + bufString);
                            return;
                        }
                    }
                } catch (Exception e) {
                    Debug.logWarning(e, "Exception thrown while running ofbizUrl transform", module);
                    throw new IOException(e);
                }
            }

            @Override
            public void flush() throws IOException {
                out.flush();
            }

            @Override
            public void write(char cbuf[], int off, int len) {
                buf.append(cbuf, off, len);
            }
        };
    }

    /**
     * Gets boolean arg.
     * <p>
     * @deprecated SCIPIO: use {@link com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil#getBooleanArg} instead.
     */
    @Deprecated
    public static Boolean checkBooleanArg(Map<?, ?> args, String key, Boolean defaultValue) {
        try {
            return TransformUtil.getBooleanArg(args, key, defaultValue);
        } catch (TemplateModelException e) {
            Debug.logError(e, module);
            return defaultValue;
        }
    }

    /**
     * Gets string arg.
     * <p>
     * @deprecated SCIPIO: use {@link com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil#getStringArg} instead.
     */
    @Deprecated
    public static String checkStringArg(Map<?, ?> args, String key, String defaultValue) {
        try {
            return TransformUtil.getStringArg(args, key, defaultValue);
        } catch (TemplateModelException e) {
            Debug.logError(e, module);
            return defaultValue;
        }
    }
}
