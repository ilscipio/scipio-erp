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
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.content.ContentRequestWorker;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.taglib.ContentUrlTag;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.TemplateBooleanModel;
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

    public final static String module = OfbizContentTransform.class.getName();

    @SuppressWarnings("unchecked")
    public static String getStringArg(Map args, String key) { // SCIPIO: renamed and made public for reuse
        String  result = "";
        Object obj = args.get(key);
        if (obj != null) {
            if (Debug.verboseOn()) Debug.logVerbose("Arg Object : " + obj.getClass().getName(), module);
            if (obj instanceof TemplateScalarModel) {
                TemplateScalarModel s = (TemplateScalarModel) obj;
                try {
                    // SCIPIO: can't enable this yet... too many templates are misusing encoding currently
                    //result = LangFtlUtil.getAsStringNonEscaping(s);
                    result = s.getAsString();
                } catch (TemplateModelException e) {
                    Debug.logError(e, "Template Exception", module);
                }
            } else {
              result = obj.toString();
            }
        }
        return result;
    }
    
    @SuppressWarnings("unchecked")
    public static Object getArgBoolOrStringNonEscaping(Map args, String key) { // SCIPIO: new
        String  result = "";
        Object obj = args.get(key);
        if (obj != null) {
            if (Debug.verboseOn()) Debug.logVerbose("Arg Object : " + obj.getClass().getName(), module);
            if (obj instanceof TemplateBooleanModel) {
                try {
                    return ((TemplateBooleanModel) obj).getAsBoolean();
                } catch (TemplateModelException e) {
                    Debug.logError(e, "Template Exception", module);
                }
            }
            if (obj instanceof TemplateScalarModel) {
                TemplateScalarModel s = (TemplateScalarModel) obj;
                try {
                    result = LangFtlUtil.getAsStringNonEscaping(s);
                } catch (TemplateModelException e) {
                    Debug.logError(e, "Template Exception", module);
                }
            } else {
              result = obj.toString();
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, Map args) {
        final StringBuilder buf = new StringBuilder();
        final String imgSize = OfbizContentTransform.getStringArg(args, "variant");
        final String uri = OfbizContentTransform.getStringArg(args, "uri"); // SCIPIO: uri as alternative to nested
        final Boolean urlDecode = OfbizUrlTransform.checkBooleanArg(args, "urlDecode", null); // SCIPIO: new
        final Object ctxPrefixObj = getArgBoolOrStringNonEscaping(args, "ctxPrefix"); // SCIPIO: new
        final Boolean strict = OfbizUrlTransform.checkBooleanArg(args, "strict", null); // SCIPIO: new
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
                    BeanModel req = (BeanModel)env.getVariable("request");
                    HttpServletRequest request = req == null ? null : (HttpServletRequest) req.getWrappedObject();

                    // SCIPIO: delegated to our new method
                    BeanModel resp = (BeanModel) env.getVariable("response");
                    HttpServletResponse response = resp == null ? null : (HttpServletResponse) resp.getWrappedObject();
                    String ctxPrefix = getContentPathPrefixNonEscaping(ctxPrefixObj, env); // SCIPIO: new
                    String url = ContentRequestWorker.makeContentLink(request, response, UtilValidate.isNotEmpty(uri) ? uri : buf.toString(), imgSize, null, ctxPrefix, urlDecode, strict);
                            
                    out.write(url);
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                }
            }
        };
    }
    
    /**
     * Returns the contextPathPrefix from the environment or request, bypassing auto screen escaping,
     * or null if not found. May be empty string.
     * @throws TemplateModelException 
     */
    public static String getContentPathPrefixNonEscaping(Environment env) throws TemplateModelException {
        TemplateModel model = ContextFtlUtil.getFtlContextGlobalVar("contentPathPrefix", env);
        if (model instanceof TemplateScalarModel) {
            return LangFtlUtil.getAsStringNonEscaping((TemplateScalarModel) model);
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
    public static String getContentPathPrefixNonEscaping(Object ctxPrefixObj, Environment env) throws TemplateModelException {
        String ctxPrefix = null;
        if (ctxPrefixObj instanceof String) {
            ctxPrefix = (String) ctxPrefixObj;
        } else if (Boolean.TRUE.equals(ctxPrefixObj)) {
            ctxPrefix = getContentPathPrefixNonEscaping(env);
        }
        return ctxPrefix;
    }

}
