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

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.control.WebAppConfigurationException;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.ext.beans.NumberModel;
import freemarker.ext.beans.StringModel;
import freemarker.template.SimpleNumber;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateTransformModel;

public class OfbizCatalogAltUrlTransform implements TemplateTransformModel {
    public final static String module = OfbizCatalogUrlTransform.class.getName();

    @SuppressWarnings("unchecked")
    public String getStringArg(Map args, String key) {
        Object o = args.get(key);
        if (o instanceof SimpleScalar) {
            return ((SimpleScalar) o).getAsString();
        } else if (o instanceof StringModel) {
            return ((StringModel) o).getAsString();
        } else if (o instanceof SimpleNumber) {
            return ((SimpleNumber) o).getAsNumber().toString();
        } else if (o instanceof NumberModel) {
            return ((NumberModel) o).getAsNumber().toString();
        }
        return null;
    }

    // Cato: Modified to support Boolean
    @SuppressWarnings("unchecked")
    public Boolean checkArg(Map args, String key, Boolean defaultValue) {
        if (!args.containsKey(key)) {
            return defaultValue;
        } else {
            Object o = args.get(key);
            if (o instanceof SimpleScalar) {
                SimpleScalar s = (SimpleScalar) o;
                if ("true".equalsIgnoreCase(s.getAsString())) {
                    return true;
                } else if ("false".equalsIgnoreCase(s.getAsString())) { // Cato: require explicit false
                    return false;
                }
                else {
                    return defaultValue;
                }
            }
            return defaultValue;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, final Map args)
            throws TemplateModelException, IOException {
        final StringBuilder buf = new StringBuilder();
        final Boolean fullPath = checkArg(args, "fullPath", null); // Cato: changed from boolean to Boolean
        final Boolean secure = checkArg(args, "secure", null); // Cato: changed from boolean to Boolean
        final Boolean encode = checkArg(args, "encode", null); // Cato: new flag

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
                    String previousCategoryId = getStringArg(args, "previousCategoryId");
                    String productCategoryId = getStringArg(args, "productCategoryId");
                    String productId = getStringArg(args, "productId");
                    String url = "";
                    
                    Object prefix = env.getVariable("urlPrefix");
                    String viewSize = getStringArg(args, "viewSize");
                    String viewIndex = getStringArg(args, "viewIndex");
                    String viewSort = getStringArg(args, "viewSort");
                    String searchString = getStringArg(args, "searchString");
                    if (req != null) {
                        HttpServletRequest request = (HttpServletRequest) req.getWrappedObject();
                        //StringBuilder newURL = new StringBuilder();
                        
                        // CATO: now delegated to our new reusable method
                        BeanModel resp = (BeanModel) env.getVariable("response");
                        HttpServletResponse response = (HttpServletResponse) resp.getWrappedObject();
                        url = CatalogUrlFilter.makeCatalogAltLink(request, response, productCategoryId, productId, previousCategoryId, 
                                fullPath, secure, encode, viewSize, viewIndex, viewSort, searchString);

                        out.write(url);
                    } else if (prefix != null) {
                        Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
                        LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
                        Locale locale = (Locale) args.get("locale");
                        
                        // CATO: now delegated to our new reusable method, and add "webSiteId" support because no way to know it
                        String prefixStr = ((StringModel) prefix).getAsString();
                        String webSiteId = getStringArg(args, "webSiteId");
                        url = CatalogUrlFilter.makeCatalogAltLink(delegator, dispatcher, locale, webSiteId, prefixStr, productCategoryId, 
                                productId, previousCategoryId, fullPath, secure, encode, viewSize, viewIndex, viewSort, searchString);
                        
                        out.write(url);
                    } else {
                        out.write(buf.toString());
                    }
                } catch (TemplateModelException e) {
                    throw new IOException(e.getMessage());
                //} catch (GenericEntityException e) {
                //    throw new IOException(e.getMessage());
                } catch (WebAppConfigurationException e) {
                    throw new IOException(e.getMessage());
                }
            }
        };
    }
}
