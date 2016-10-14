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
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

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

    @SuppressWarnings("rawtypes")
    public static Boolean checkBooleanArg(Map args, String key, Boolean defaultValue) { // SCIPIO: NOTE: can now return null
        Object o = args.get(key);
        // SCIPIO: NOTE (2016-02): we now support real booleans. 
        // In addition, SimpleScalar was a bad type to use.
        //if (o instanceof SimpleScalar) {
        //    SimpleScalar s = (SimpleScalar) o;
        //    return "true".equalsIgnoreCase(s.getAsString());
        //}
        if (o instanceof TemplateBooleanModel) {
            try {
                return ((TemplateBooleanModel) o).getAsBoolean();
            } catch (TemplateModelException e) {
                Debug.logError(e, "Could not get boolean arg for ofbizUrl (as boolean model)", module);
            }
        }
        else if (o instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) o;
            try {
                String val = s.getAsString();
                // SCIPIO: empty check is desirable and makes it so caller can request default by specifying ""
                if (!val.isEmpty()) {
                    return "true".equalsIgnoreCase(s.getAsString());
                }
            } catch (TemplateModelException e) {
                Debug.logError(e, "Could not get boolean arg for ofbizUrl (from string model)", module);
            }
        }
        return defaultValue;
    }

    // SCIPIO: new (wrapper)
    public static String checkStringArg(Map args, String key, String defaultValue) {
        return getStringArg(args, key, defaultValue);
    }
    
    private static String getStringArg(Map args, String key, String defaultValue) {
        Object o = args.get(key);
        if (o instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) o;
            try {
                // SCIPIO: 2016-10-14: this must bypass screen auto-escaping!
                return LangFtlUtil.getAsStringNonEscaping(s);
                //return s.getAsString();
            } catch (TemplateModelException e) {
                Debug.logError(e, "Invalid uri arg for ofbizUrl", module);
            }
        }
        return defaultValue;
    }

    
    private static String convertToString(Object o) {
        String result = "";
        if (o != null) {
            if (Debug.verboseOn())
                Debug.logVerbose("Arg Object : " + o.getClass().getName(), module);
            if (o instanceof TemplateScalarModel) {
                TemplateScalarModel s = (TemplateScalarModel) o;
                try {
                    // SCIPIO: FIXME: This should probably bypass auto-escaping,
                    // but currently we cannot do this because in some cases there
                    // may be security risks in existing templates, largely because 
                    // HTML and javascript escaping are not really done properly.
                    // This applies only to the "uri" argument (the others would be mostly safe)
                    //result = LangFtlUtil.getAsStringNonEscaping(s);
                    result = s.getAsString();
                } catch (TemplateModelException e) {
                    Debug.logError(e, "Template Exception", module);
                }
            } else {
                result = o.toString();
            }
        }
        return result;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Writer getWriter(final Writer out, Map args) {
        final StringBuilder buf = new StringBuilder();
        final Boolean fullPath = checkBooleanArg(args, "fullPath", null); // SCIPIO: modified to remove default; leave centralized
        final Boolean secure = checkBooleanArg(args, "secure", null); // SCIPIO: modified to remove default; leave centralized
        final Boolean encode = checkBooleanArg(args, "encode", null); // SCIPIO: modified to remove default; leave centralized
        final String webSiteId = convertToString(args.get("webSiteId"));
        // SCIPIO: We now support a "uri" arg as alternative to #nested
        final String uriArg = getStringArg(args, "uri", null);
        // SCIPIO: more new parameters
        final String type = getStringArg(args, "type", null);
        final Boolean absPath = checkBooleanArg(args, "absPath", null); 
        final Boolean interWebapp = checkBooleanArg(args, "interWebapp", null); // Alias for type="inter-webapp"
        final Boolean controller = checkBooleanArg(args, "controller", null);
        final boolean extLoginKey = checkBooleanArg(args, "extLoginKey", false);
        
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
                    String prefixString = convertToString(env.getVariable("urlPrefix"));
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
                            out.write(link);
                        }
                        else {
                            // SCIPIO: If link is null, it means there was an error building link; write nothing, so that
                            // it's possible for templates to catch this case if they need to.
                            //out.write(requestUrl);
                        }
                    } else {
                        out.write(buf.toString());
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
}
