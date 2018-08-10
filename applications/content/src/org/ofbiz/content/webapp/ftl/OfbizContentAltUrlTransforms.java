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

package org.ofbiz.content.webapp.ftl;

import java.io.IOException;
import java.io.Writer;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.content.content.ContentUrlFilter;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.renderer.RenderEnvType;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.UrlTransformUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateTransformModel;

/**
 * Content Alt URL transform.
 * <p>
 * SCIPIO: Added <code>urlDecode</code> boolean param.
 * and changed the logical default to <code>false</code> compared to stock Ofbiz (where
 * it would have been <code>true</code>).
 */
public class OfbizContentAltUrlTransforms implements TemplateTransformModel {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    @Override
    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, final Map args)
            throws TemplateModelException, IOException {
        final StringBuilder buf = new StringBuilder();
        return new Writer(out) {
            
            @Override
            public void write(char[] cbuf, int off, int len) throws IOException {
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
                    HttpServletRequest request = ContextFtlUtil.getRequest(env); // SCIPIO
                    HttpServletResponse response = ContextFtlUtil.getResponse(env);
                    RenderEnvType renderEnvType = ContextFtlUtil.getRenderEnvType(env, request);
                    //FullWebappInfo.Cache webappInfoCache = ContextFtlUtil.getWebappInfoCacheAndCurrent(env, request, renderEnvType);
                    
                    final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
                    boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
                    boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
                    boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
                    final Boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new
                    
                    Boolean fullPath = UrlTransformUtil.determineFullPath(TransformUtil.getBooleanArg(args, "fullPath"), renderEnvType, env); // SCIPIO: changed from boolean to Boolean
                    Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO: changed from boolean to Boolean
                    Boolean encode = TransformUtil.getBooleanArg(args, "encode"); // SCIPIO: new flag
                    Object urlParams = TransformUtil.getStringArg(args, "params", rawParams); // SCIPIO: new; TODO: support map (but needs special handling to respect rawParams)

                    String contentId = TransformUtil.getStringArg(args, "contentId", rawParams);
                    String viewContent = TransformUtil.getStringArg(args, "viewContent", rawParams);
                    Boolean urlDecode = TransformUtil.getBooleanArg(args, "urlDecode");
     
                    String url = "";
                    if (UtilValidate.isNotEmpty(contentId)) {
                        Locale locale = TransformUtil.getOfbizLocaleArgOrContextOrRequest(args, "locale", env); // SCIPIO
                        if (request != null) {
                            // SCIPIO: replaced
                            //url = ContentUrlFilter.makeContentAltUrl(request, response, contentId, viewContent, urlDecode);
                            url = ContentUrlFilter.makeContentAltLink(request, response, locale, contentId, viewContent, urlDecode, urlParams, fullPath, secure, encode);
                        } else {
                            // TODO: when from static context...
                            Debug.logWarning("@ofbizContentAltUrl transform is not implemented for non-webapp render contexts", module);
                        }
                        out.write(UrlTransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
                    }
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                } catch (WebAppConfigurationException e) { // SCIPIO
                    throw new IOException(e.getMessage());
                }
            }
        };
    }
}
