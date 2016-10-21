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
import java.net.URLEncoder;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
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

    public final static String module = OfbizUrlTransform.class.getName();

    @Override
    @SuppressWarnings("rawtypes")
    public Writer getWriter(final Writer out, final Map args) throws TemplateModelException {
        final StringBuilder buf = new StringBuilder();
        
        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final boolean strict = TransformUtil.getBooleanArg(args, "strict", rawParamsDefault); // SCIPIO: new
        
        final Boolean fullPath = TransformUtil.getBooleanArg(args, "fullPath"); // SCIPIO: modified to remove default; leave centralized
        final Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO: modified to remove default; leave centralized
        final Boolean encode = TransformUtil.getBooleanArg(args, "encode"); // SCIPIO: modified to remove default; leave centralized
        final String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams);
        // SCIPIO: We now support a "uri" arg as alternative to #nested
        final String uriArg = TransformUtil.getStringArg(args, "uri", rawParams);
        // SCIPIO: more new parameters
        final String type = TransformUtil.getStringArg(args, "type", rawParams);
        final Boolean absPath = TransformUtil.getBooleanArg(args, "absPath"); 
        final Boolean interWebapp = TransformUtil.getBooleanArg(args, "interWebapp"); // Alias for type="inter-webapp"
        final Boolean controller = TransformUtil.getBooleanArg(args, "controller");
        final boolean extLoginKey = TransformUtil.getBooleanArg(args, "extLoginKey", false);
        
        return new Writer(out) {

            @Override
            public void close() throws IOException {
                try {
                    // SCIPIO: can use uri instead of buffer
                    if (uriArg != null) {
                        buf.append(uriArg);
                    }
                    
                    Environment env = FreeMarkerWorker.getCurrentEnvironment();
                    // Handle prefix.
                    String prefixString = TransformUtil.getStringArg(args, "urlPrefix", "");
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
                    HttpServletRequest request = FreeMarkerWorker.unwrap(env.getVariable("request"));
                    /* SCIPIO: This part is limited and incomplete. Instead, delegate to our improved makeLink* method(s).
                    // Handle web site ID.
                    if (!webSiteId.isEmpty()) {
                        Delegator delegator = FreeMarkerWorker.unwrap(env.getVariable("delegator"));
                        if (request != null && delegator == null) {
                            delegator = (Delegator) request.getAttribute("delegator");
                        }
                        if (delegator == null) {
                            throw new IllegalStateException("Delegator not found");
                        }
                        WebappInfo webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
                        StringBuilder newUrlBuff = new StringBuilder(250);
                        OfbizUrlBuilder builder = OfbizUrlBuilder.from(webAppInfo, delegator);
                        builder.buildFullUrl(newUrlBuff, buf.toString(), secure);
                        String newUrl = newUrlBuff.toString();
                        if (encode) {
                            // SCIPIO: This was invalid! This is not what the "encode" boolean is supposed to mean!
                            // It means pass through response.encodeURL.
                            //newUrl = URLEncoder.encode(newUrl, "UTF-8");
                            HttpServletResponse response = FreeMarkerWorker.unwrap(env.getVariable("response"));
                            if (response != null) {
                                newUrl = response.encodeURL(newUrl);
                            }
                        }
                        out.write(newUrl);
                        return;
                    }
                    */
                    if (request != null) {
                        ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
                        HttpServletResponse response = FreeMarkerWorker.unwrap(env.getVariable("response"));
                        String requestUrl = buf.toString();
                        // SCIPIO: If requested, add external login key
                        if (extLoginKey) {
                            requestUrl = RequestLinkUtil.checkAddExternalLoginKey(requestUrl, request, true);
                        }
                        RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
                        // SCIPIO: Now use more advanced method
                        //out.write(rh.makeLink(request, response, requestUrl, fullPath, secure, encode));
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
                        String link = rh.makeLinkAuto(request, response, requestUrl, absPath, interWebappEff, webSiteId, controller, fullPath, secure, encode);
                        if (link != null) {
                            out.write(TransformUtil.escapeGeneratedUrl(link, escapeAs, strict, env));
                        }
                        else {
                            // SCIPIO: If link is null, it means there was an error building link; write nothing, so that
                            // it's possible for templates to catch this case if they need to.
                            //out.write(requestUrl);
                        }
                    } else {
                        out.write(TransformUtil.escapeGeneratedUrl(buf.toString(), escapeAs, strict, env));
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
