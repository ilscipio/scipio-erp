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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.FtlTransformFactory;
import org.ofbiz.webapp.content.ContentRequestWorker;
import org.ofbiz.webapp.ftl.WebappUrlDirective.DefaultParams;
import org.ofbiz.webapp.ftl.WebappUrlDirective.WebappUrlMethod;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * ContentDirective - Freemarker Transform for content links
 * <p>
 * SCIPIO: added:
 * <ul>
 * <li><code>uri</code> (alternative to nested)
 * <li><code>urlDecode</code> boolean which is changed to <code>false</code> by default
 *   (would have been <code>true</code> default in stock Ofbiz).
 * <li><code>ctxPrefix</code> overridable URL prefix (or boolean true, which looks up contentPathPrefix in globals)
 * </ul>
 */
public class ContentUrlDirective implements TemplateDirectiveModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // FIXME: this class should have its own DefaultParams class
    private static final DefaultParams DEFAULT_PARAMS = new DefaultParams(DefaultParams.NULL);
    public static class Factory implements FtlTransformFactory { // SCIPIO
        private static final ContentUrlDirective DEFAULT = new ContentUrlDirective();
        @Override
        public TemplateModel getTransform(ClassLoader loader) {
            return DEFAULT;
        }
    }
    public static ContentUrlDirective getInstance() {
        return Factory.DEFAULT;
    }
    public static ContentUrlDirective create() {
        return new ContentUrlDirective();
    }

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        execute(env, args, loopVars, body, env.getOut(), null, null, DEFAULT_PARAMS);
    }

    private void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body,
            Writer out, TemplateModel uriModel, TemplateModel variantModel, DefaultParams defaultParams) throws TemplateException, IOException {
        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // new
        Boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams"); // new
        if (rawParams == null) {
            rawParams = (defaultParams.rawParams != null) ? defaultParams.rawParams :
                UtilValidate.isNotEmpty(escapeAs) ? true : false; // if we're post-escaping, we can assume we should get rawParams
        }
        Boolean strict = TransformUtil.getBooleanArg(args, "strict"); // new
        if (strict == null) {
            strict = (defaultParams.strict != null) ? defaultParams.strict : UtilValidate.isNotEmpty(escapeAs) ? true : false; // if we're post-escaping, we can assume we want strict handling
        }

        // We now support a "uri" arg as alternative to #nested
        String uri = (uriModel != null) ? TransformUtil.getStringArg(uriModel, rawParams)
                : TransformUtil.getStringArg(args, "uri", rawParams);
        
        String imgSize = (variantModel != null) ? TransformUtil.getStringArg(variantModel, rawParams) :
            TransformUtil.getStringArg(args, "variant", rawParams);
        
        Boolean urlDecode = TransformUtil.getBooleanArg(args, "urlDecode"); // SCIPIO: new
        Object ctxPrefixObj = TransformUtil.getBooleanOrStringArg(args, "ctxPrefix", null, false, rawParams); // SCIPIO: new

        // SCIPIO: autoVariant params: added 2017-08-08
        String autoVariant = TransformUtil.getStringNonEscapingArg(args, "autoVariant");
        Integer imgWidth = TransformUtil.getIntegerArg(args, "width");
        Integer imgHeight = TransformUtil.getIntegerArg(args, "height");
        String imgVariantCfg = TransformUtil.getStringNonEscapingArg(args, "variantCfg");

        String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams);
        Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO
        String type = TransformUtil.getStringNonEscapingArg(args, "type");

        String url;
        try {
            HttpServletRequest request = ContextFtlUtil.getRequest(env);
            HttpServletResponse response = ContextFtlUtil.getResponse(env);

            if (body != null) {
                final StringWriter buf = new StringWriter();
                body.render(buf);
                uri = buf.toString();
            } else if (uri == null) {
                throw new TemplateException("Cannot build URL: missing path/uri (null)", env);
            }
            
            String ctxPrefix = getContentPathPrefix(ctxPrefixObj, rawParams, env); // SCIPIO: new

            // SCIPIO: delegated to our new methods
            if (request != null) {
                url = ContentRequestWorker.makeContentLink(request, response, uri, imgSize, webSiteId,
                    ctxPrefix, urlDecode, strict, secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
            } else {
                Map<String, Object> context = ContextFtlUtil.getContext(env);
                if (context != null) {
                    url = ContentRequestWorker.makeContentLink(context, uri, imgSize, webSiteId, ctxPrefix, urlDecode, strict,
                            secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
                } else {
                    Debug.logWarning("@contentUrl: no request or render context available - unusual context! (path: " + uri + ")", module);
                    // try anyway (original request-based method in ContentUrlTag will catch it)
                    url = ContentRequestWorker.makeContentLink(null, null, uri, imgSize, webSiteId, ctxPrefix, urlDecode, strict,
                            secure, type, autoVariant, imgWidth, imgHeight, imgVariantCfg);
                }
            }

            url = UrlTransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env);
        } catch (TemplateException e) {
            throw e;
        } catch (Exception e) {
            throw new TemplateException(e, env);
        }
        out.write(url);
    }

    public static class Method implements TemplateMethodModelEx {
        //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        private static final DefaultParams DEFAULT_PARAMS = new DefaultParams(WebappUrlMethod.DEFAULT_PARAMS);
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
        private DefaultParams getDefaultParams() {
            return DEFAULT_PARAMS;
        }
        @Override
        public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
            return exec(WebappUrlDirective.getArgsMapOrNull(args), args);
        }
        
        public Object exec(Map<String, ? extends TemplateModel> args, List<?> posArgs) throws TemplateModelException {
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            StringWriter sw = new StringWriter();
            TemplateModel uriModel = null;
            TemplateModel variantModel = null;
            if (args == null) {
                args = Collections.emptyMap();
                // Positional parameter(s) (limited support)
                if (posArgs.size() >= 1) {
                    uriModel = (TemplateModel) posArgs.get(0); 
                    if (posArgs.size() >= 2) {
                        variantModel = (TemplateModel) posArgs.get(1);
                    }
                }
            }
            try {
                ContentUrlDirective.getInstance().execute(env, args, WebappUrlDirective.NO_LOOP_VARS,
                        null, sw, uriModel, variantModel, getDefaultParams());
            } catch (IOException | TemplateException e) {
                throw new TemplateModelException(e);
            }
            return new SimpleScalar(sw.toString()); // NOTE: Do not return unwrapped string, due to escaping
        }
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
