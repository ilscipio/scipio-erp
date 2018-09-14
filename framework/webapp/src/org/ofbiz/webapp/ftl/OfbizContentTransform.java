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
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.content.ContentRequestWorker;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateTransformModel;

/**
 * OfbizContentTransform - Freemarker Transform for content links
 * <p>
 * SCIPIO: added:
 * <ul>
 * <li><code>uri</code> (alternative to nested)
 * <li><code>urlDecode</code> boolean which is changed to <code>false</code> by default
 *   (would have been <code>true</code> default in stock Ofbiz).
 * <li><code>ctxPrefix</code> overridable URL prefix (or boolean true, which looks up contentPathPrefix in globals)
 * </ul>
 */
public class OfbizContentTransform implements TemplateTransformModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public Writer getWriter(final Writer out, @SuppressWarnings("rawtypes") Map args) throws TemplateModelException {
        final StringBuilder buf = new StringBuilder();
        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final Boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new

        final String imgSize = TransformUtil.getStringArg(args, "variant", rawParams);
        final String uri = TransformUtil.getStringArg(args, "uri", rawParams); // SCIPIO: uri as alternative to nested
        final Boolean urlDecode = TransformUtil.getBooleanArg(args, "urlDecode"); // SCIPIO: new
        final Object ctxPrefixObj = TransformUtil.getBooleanOrStringArg(args, "ctxPrefix", null, false, rawParams); // SCIPIO: new

        // SCIPIO: autoVariant params: added 2017-08-08
        final String autoVariant = TransformUtil.getStringNonEscapingArg(args, "autoVariant");
        final Integer imgWidth = TransformUtil.getIntegerArg(args, "width");
        final Integer imgHeight = TransformUtil.getIntegerArg(args, "height");
        final String imgVariantCfg = TransformUtil.getStringNonEscapingArg(args, "variantCfg");

        final String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams);
        final Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO
        final String type = TransformUtil.getStringNonEscapingArg(args, "type");

        return new Writer(out) {
            @Override
            public void write(char cbuf[], int off, int len) {
                buf.append(cbuf, off, len);
            }

            @Override
            public void flush() throws IOException {
                out.flush();
            }

            @Override
            public void close() throws IOException {
                try {
                    Environment env = FreeMarkerWorker.getCurrentEnvironment();
                    HttpServletRequest request = ContextFtlUtil.getRequest(env);
                    HttpServletResponse response = ContextFtlUtil.getResponse(env);

                    String ctxPrefix = getContentPathPrefix(ctxPrefixObj, rawParams, env); // SCIPIO: new

                    String srcUri = UtilValidate.isNotEmpty(uri) ? uri : buf.toString();

                    // SCIPIO: delegated to our new methods
                    String url;
                    if (request != null) {
                        url = ContentRequestWorker.makeContentLink(request, response, srcUri, imgSize, webSiteId,
                            ctxPrefix, urlDecode, strict, secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
                    } else {
                        Map<String, Object> context = ContextFtlUtil.getContext(env);
                        if (context != null) {
                            url = ContentRequestWorker.makeContentLink(context, srcUri, imgSize, webSiteId, ctxPrefix, urlDecode, strict,
                                    secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
                        } else {
                            Debug.logWarning("@ofbizContentUrl: no request or render context available - unusual context!", module);
                            // try anyway (original request-based method in ContentUrlTag will catch it)
                            url = ContentRequestWorker.makeContentLink(null, null, srcUri, imgSize, webSiteId, ctxPrefix, urlDecode, strict,
                                    secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
                        }
                    }

                    out.write(UrlTransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                }
            }
        };
    }

    /**
     * Returns the contextPathPrefix from the environment or request
     * or null if not found. May be empty string.
     * @throws TemplateModelException
     */
    public static String getContentPathPrefix(boolean nonEscaping, Environment env) throws TemplateModelException {
        TemplateModel model = TransformUtil.getFtlContextGlobalVar("contentPathPrefix", env);
        if (model instanceof TemplateScalarModel) {
            return LangFtlUtil.getAsString((TemplateScalarModel) model, nonEscaping);
        }
        HttpServletRequest request = ContextFtlUtil.getRequest(env);
        if (request != null) {
            Object res = request.getAttribute("contentPathPrefix");
            if (res instanceof String) {
                return (String) res;
            }
        }
        return null;
    }

    /**
     * Returns the string passed in ctxPrefix or the contextPathPrefix from the environment or request,
     * bypassing auto screen escaping,
     * or null if not found. May be empty string.
     * @throws TemplateModelException
     */
    public static String getContentPathPrefix(Object ctxPrefixObj, boolean nonEscaping, Environment env) throws TemplateModelException {
        String ctxPrefix = null;
        if (ctxPrefixObj instanceof String) {
            ctxPrefix = (String) ctxPrefixObj;
        } else if (Boolean.TRUE.equals(ctxPrefixObj)) {
            ctxPrefix = getContentPathPrefix(nonEscaping, env);
        }
        return ctxPrefix;
    }

}
