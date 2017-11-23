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
package com.ilscipio.scipio.cms.template.ftl;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: PageLinkMethod - <code>makeCmsPageUrl</code> function wrapper around <code>@cmsPageUrl</code>.
 * <p>
 * Two variants supported:
 * <ul>
 * <li><code>makeCmsPageUrl(pageName)</code> - single string page name arg, no other options.
 * <li><code>makeCmsPageUrl({"name":pageName, "escapeAs":"html", ...})</code> - single map arg, 
 *     with each entry an option to <code>@cmsPageUrl</code>.
 * </ul>
 * Unlike the macro, by default, this uses <code>rawParams=true</code> and <code>strict=true</code> (NOTE: strict
 * may or may not be honored; see ofbizUrl docs).
 */
public class PageLinkMethod implements TemplateMethodModelEx {

    public static final String module = PageLinkMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        Environment env = Environment.getCurrentEnvironment();
        
        Map<String, TemplateModel> linkArgs;
        
        TemplateModel firstArg = (TemplateModel) args.get(0);
        if (LangFtlUtil.isObjectType("map", firstArg)) {
            linkArgs = LangFtlUtil.makeModelMap((TemplateHashModelEx) firstArg);
            if (LangFtlUtil.isNullOrEmptyString(linkArgs.get("rawParams"))) {
                linkArgs.put("linkArgs", TemplateBooleanModel.TRUE);
            }
            if (LangFtlUtil.isNullOrEmptyString(linkArgs.get("strict"))) {
                linkArgs.put("strict", TemplateBooleanModel.TRUE);
            }
        } else {
            // single-arg form assumes page name (ID will be unreadable number in practice).
            linkArgs = new HashMap<>();
            linkArgs.put("name", firstArg);
            linkArgs.put("rawParams", TemplateBooleanModel.TRUE);
            linkArgs.put("strict", TemplateBooleanModel.TRUE);
        }
        
        String output;
        try {
            output = PageLinkDirective.makeLinkFromFtl(env, linkArgs, null, null);
        } catch (TemplateException e) {
            throw new TemplateModelException(e);
        } catch (IOException e) {
            throw new TemplateModelException(e);
        }
        return output != null ? output : "";
    }

}
