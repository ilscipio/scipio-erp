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
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.ftl.OfbizUrlTransform;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateScalarModel;
import freemarker.template.utility.DeepUnwrap;

/**
 * CatalogUrlDirective - Freemarker Template Directive for generating URLs suitable for use by the CatalogUrlServlet
 * <p>
 * Accepts the following arguments (see CatalogUrlServlet for their definition):
 * <ul>
 * <li>productId</li>
 * <li>currentCategoryId</li>
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
 * <li>params</li>
 * </ul>
 */
public class CatalogUrlDirective implements TemplateDirectiveModel {

    public final static String module = CatalogUrlDirective.class.getName();

    @Override
    public void execute(Environment env, Map args, TemplateModel[] loopVars, TemplateDirectiveBody body) throws TemplateException, IOException {
        Map<String, TemplateModel> params = UtilGenerics.checkMap(args);
        // SCIPIO: various changes here
        final String escapeAs = TransformUtil.getStringArg(args, "escapeAs"); // SCIPIO: new
        boolean rawParamsDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we should get rawParams
        final boolean rawParams = TransformUtil.getBooleanArg(args, "rawParams", rawParamsDefault); // SCIPIO: new
        boolean strictDefault = UtilValidate.isNotEmpty(escapeAs) ? true : false; // SCIPIO: if we're post-escaping, we can assume we want strict handling
        final Boolean strict = TransformUtil.getBooleanArg(args, "strict", strictDefault); // SCIPIO: new
        
        String productId = TransformUtil.getStringArg(args, "productId", rawParams);
        String currentCategoryId = TransformUtil.getStringArg(args, "currentCategoryId", rawParams);
        String previousCategoryId = TransformUtil.getStringArg(args, "previousCategoryId", rawParams);

        BeanModel req = (BeanModel) env.getVariable("request");

        // SCIPIO: new flags
        
        Boolean fullPath = TransformUtil.getBooleanArg(args, "fullPath");
        Boolean secure = TransformUtil.getBooleanArg(args, "secure");
        Boolean encode = TransformUtil.getBooleanArg(args, "encode");

        String webSiteId = TransformUtil.getStringArg(args, "webSiteId", rawParams); // SCIPIO: webSiteId
        
        String prefix = TransformUtil.getStringArg(args, "prefix", rawParams);
        
        Object urlParams = DeepUnwrap.unwrap(params.get("params"));
        
        if (req != null) {
            HttpServletRequest request = (HttpServletRequest) req.getWrappedObject();
            
            // SCIPIO: now delegated to our new reusable method, and also support fullPath and secure flags
            BeanModel resp = (BeanModel) env.getVariable("response");
            HttpServletResponse response = (HttpServletResponse) resp.getWrappedObject();
            
            //String url = CatalogUrlServlet.makeCatalogUrl(request, productId, currentCategoryId, previousCategoryId);
            String url = null;
            try {
                url = CatalogUrlServlet.makeCatalogLink(request, response, productId, currentCategoryId, previousCategoryId, urlParams, webSiteId, 
                        prefix, fullPath, secure, encode);
            } catch (WebAppConfigurationException e) {
                throw new IOException(e.getMessage());
            }
            
            // SCIPIO: no null
            if (url != null) {
                env.getOut().write(TransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
            }
        } else if (webSiteId != null || prefix != null) {
            // SCIPIO: New: Handle non-request cases
            Delegator delegator = FreeMarkerWorker.getWrappedObject("delegator", env);
            LocalDispatcher dispatcher = FreeMarkerWorker.getWrappedObject("dispatcher", env);
            Locale locale = (Locale) args.get("locale");
            
            String url;
            try {
                url = CatalogUrlServlet.makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, urlParams, webSiteId, 
                        prefix, fullPath, secure);
            } catch (WebAppConfigurationException e) {
                throw new IOException(e.getMessage());
            }
            
            // SCIPIO: no null
            if (url != null) {
                env.getOut().write(TransformUtil.escapeGeneratedUrl(url, escapeAs, strict, env));
            }
        }
    }
}
