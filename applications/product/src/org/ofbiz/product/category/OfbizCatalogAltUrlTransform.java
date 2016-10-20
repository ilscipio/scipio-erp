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

import org.ofbiz.base.util.Debug;
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
import org.ofbiz.webapp.ftl.OfbizUrlTransform;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.ext.beans.NumberModel;
import freemarker.ext.beans.StringModel;
import freemarker.template.SimpleNumber;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateTransformModel;
import freemarker.template.utility.DeepUnwrap;

/**
 * Catalog URL Alt Transform.
 * <p>
 * Accepts the following arguments (see CatalogUrlFilter for their definition):
 * <ul>
 * <li>productId</li>
 * <li>productCategoryId</li>
 * <li>previousCategoryId</li>
 * </ul>
 * <p>
 * SCIPIO: This transform is augmented to support the following parameters:
 * <ul>
 * <li>fullPath (boolean)</li>
 * <li>secure (boolean)</li>
 * <li>encode (boolean)</li>
 * </ul>
 * <p>
 * In addition, it now supports inter-webapp links. If either of the parameters
 * <ul>
 * <li>webSiteId</li>
 * <li>prefix</li>
 * </ul>
 * are specified, it enables inter-webapp mode, where no session information
 * is used and a purely static link is built instead.
 * For staticly-rendered templates such as emails, webSiteId or prefix is always required.
 * <p>
 * It is also now possible to specify a string of parameters (with or without starting "?") using:
 * <ul>
 * <li>params (TODO: support map of parameters)</li>
 * </ul>
 */
public class OfbizCatalogAltUrlTransform implements TemplateTransformModel {
    public final static String module = OfbizCatalogUrlTransform.class.getName();

    @Override
    @SuppressWarnings("unchecked")
    public Writer getWriter(final Writer out, final Map args)
            throws TemplateModelException, IOException {
        final StringBuilder buf = new StringBuilder();
        // SCIPIO: various changes here
        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final Boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new
        
        final Boolean fullPath = TransformUtil.getBooleanArg(args, "fullPath"); // SCIPIO: changed from boolean to Boolean
        final Boolean secure = TransformUtil.getBooleanArg(args, "secure"); // SCIPIO: changed from boolean to Boolean
        final Boolean encode = TransformUtil.getBooleanArg(args, "encode"); // SCIPIO: new flag

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
                    String previousCategoryId = TransformUtil.getStringArg(args, "previousCategoryId", rawParams);
                    String productCategoryId = TransformUtil.getStringArg(args, "productCategoryId", rawParams);
                    String productId = TransformUtil.getStringArg(args, "productId", rawParams);
                    String url = "";
                    
                    String viewSize = TransformUtil.getStringArg(args, "viewSize", rawParams);
                    String viewIndex = TransformUtil.getStringArg(args, "viewIndex", rawParams);
                    String viewSort = TransformUtil.getStringArg(args, "viewSort", rawParams);
                    String searchString = TransformUtil.getStringArg(args, "searchString", rawParams);
                    
                    // SCIPIO: webSiteId
                    String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams);
                    
                    String prefix = TransformUtil.getStringArg(args, "prefix", rawParams);
                    
                    Object urlParams = TransformUtil.getStringArg(args, "params", rawParams); // SCIPIO: new; TODO: support map (but needs special handling to respect rawParams)
                    
                    if (req != null) {
                        HttpServletRequest request = (HttpServletRequest) req.getWrappedObject();
                        //StringBuilder newURL = new StringBuilder();
                        
                        // SCIPIO: now delegated to our new reusable method
                        BeanModel resp = (BeanModel) env.getVariable("response");
                        HttpServletResponse response = (HttpServletResponse) resp.getWrappedObject();
                        url = CatalogUrlFilter.makeCatalogAltLink(request, response, productCategoryId, productId, previousCategoryId, urlParams, webSiteId, 
                                prefix, fullPath, secure, encode, viewSize, viewIndex, viewSort, searchString);

                        // SCIPIO: no null
                        if (url != null) {
                            out.write(TransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
                        }
                    } else if (webSiteId != null || prefix != null) {
                        Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
                        LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
                        Locale locale = (Locale) args.get("locale");
                        
                        // SCIPIO: now delegated to our new reusable method
                        // NOTE: here webSiteId is usually required!
                        url = CatalogUrlFilter.makeCatalogAltLink(delegator, dispatcher, locale, productCategoryId, productId, previousCategoryId, 
                                urlParams, webSiteId, prefix, fullPath, secure, viewSize, viewIndex, viewSort, searchString);
                        
                        // SCIPIO: no null
                        if (url != null) {
                            out.write(TransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
                        }
                    } else {
                        out.write(TransformUtil.escapeGeneratedUrl(buf.toString(), escapeAs, strict, env));
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
