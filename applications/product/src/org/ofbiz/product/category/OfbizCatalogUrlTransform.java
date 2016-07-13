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
package org.ofbiz.product.category;

import java.io.IOException;
import java.io.Writer;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.ftl.OfbizUrlTransform;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.ext.beans.StringModel;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateTransformModel;

public class OfbizCatalogUrlTransform implements TemplateTransformModel {
    public final static String module = OfbizCatalogUrlTransform.class.getName();
    
    @SuppressWarnings("unchecked")
    public String getStringArg(Map args, String key) {
        Object o = args.get(key);
        if (o instanceof SimpleScalar) {
            return ((SimpleScalar) o).getAsString();
        } else if (o instanceof StringModel) {
            return ((StringModel) o).getAsString();
        }
        return null;
    }

    // Scipio: Added and modified to support Boolean
    @SuppressWarnings("unchecked")
    private static Boolean checkBooleanArg(Map args, String key, Boolean defaultValue) { // Scipio: NOTE: can now return null
        return OfbizUrlTransform.checkBooleanArg(args, key, defaultValue);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, final Map args) throws TemplateModelException, IOException {
        final StringBuilder buf = new StringBuilder();
        
        // Scipio: new flags
        final Boolean fullPath = checkBooleanArg(args, "fullPath", null);
        final Boolean secure = checkBooleanArg(args, "secure", null);
        final Boolean encode = checkBooleanArg(args, "encode", null);
        
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
                    BeanModel req = (BeanModel) env.getVariable("request");
                    
                    String productId = getStringArg(args, "productId");
                    String currentCategoryId = getStringArg(args, "currentCategoryId");
                    String previousCategoryId = getStringArg(args, "previousCategoryId");
                    
                    // SCIPIO: webSiteId
                    String webSiteId = getStringArg(args, "webSiteId");
                    
                    String prefix = getStringArg(args, "prefix");
                    
                    if (req != null) {
                        HttpServletRequest request = (HttpServletRequest) req.getWrappedObject();
                        
                        // SCIPIO: now delegated to our new reusable method, and also support fullPath and secure flags
                        BeanModel resp = (BeanModel) env.getVariable("response");
                        HttpServletResponse response = (HttpServletResponse) resp.getWrappedObject();
                        
                        String url = CatalogUrlServlet.makeCatalogLink(request, response, webSiteId, prefix, productId, currentCategoryId, previousCategoryId, 
                                fullPath, secure, encode);

                        // SCIPIO: no null
                        if (url != null) {
                            out.write(url);
                        }
                    } else if (webSiteId != null || prefix != null) {
                        // SCIPIO: New: Handle non-request cases
                        Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
                        LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
                        Locale locale = (Locale) args.get("locale");
                        
                        String url = CatalogUrlServlet.makeCatalogLink(delegator, dispatcher, locale, webSiteId, prefix, productId, currentCategoryId, previousCategoryId, 
                                fullPath, secure);

                        // SCIPIO: no null
                        if (url != null) {
                            out.write(url);
                        }
                    }
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                } catch (WebAppConfigurationException e) {
                    throw new IOException(e.getMessage());
                }
            }
        };
    }
}