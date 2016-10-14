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

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateTransformModel;

/**
 * OfbizContentTransform - Freemarker Transform for content links
 */
public class OfbizContentTransform implements TemplateTransformModel {

    public final static String module = OfbizContentTransform.class.getName();

    @SuppressWarnings("unchecked")
    private static String getArg(Map args, String key) {
        String  result = "";
        Object obj = args.get(key);
        if (obj != null) {
            if (Debug.verboseOn()) Debug.logVerbose("Arg Object : " + obj.getClass().getName(), module);
            if (obj instanceof TemplateScalarModel) {
                TemplateScalarModel s = (TemplateScalarModel) obj;
                try {
                    // SCIPIO: 2016-10-14: this must bypass screen auto-escaping!
                    //result = s.getAsString();
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
        final String imgSize = OfbizContentTransform.getArg(args, "variant");
        final String uri = OfbizContentTransform.getArg(args, "uri"); // SCIPIO: uri as alternative to nested
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
                    String url = ContentRequestWorker.makeContentLink(request, response, UtilValidate.isNotEmpty(uri) ? uri : buf.toString(), imgSize);
                            
                    out.write(url);
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                }
            }
        };
    }
}
