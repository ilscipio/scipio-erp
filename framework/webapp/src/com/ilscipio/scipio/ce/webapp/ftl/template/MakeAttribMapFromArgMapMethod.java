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
package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.OfbizFtlObjectType;

import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: MakeAttribMapFromArgMapMethod - Freemarker Method for getting an attribs map from an args map.
 */
public class MakeAttribMapFromArgMapMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }
        
        ObjectWrapper objectWrapper = CommonFtlUtil.getCurrentEnvironment().getObjectWrapper();

        TemplateHashModelEx argsMap = (TemplateHashModelEx) args.get(0);
        
        // caller-supplied excludes
        TemplateModel excludesModel = (args.size() >=2) ? (TemplateModel) args.get(1) : null;
        Set<String> excludes;
        if (excludesModel != null) {
            excludes = LangFtlUtil.getAsStringSet(excludesModel);
        } else {
            excludes = new HashSet<>();
        }
        
        SimpleHash res = null;
        
        final Boolean useExclude = Boolean.FALSE;

        // put attribs from explicit attribs map first, if any
        TemplateModel attribsModel = argsMap.get("attribs");
        if (attribsModel != null && OfbizFtlObjectType.isObjectType(OfbizFtlObjectType.MAP, attribsModel)) {
            if (OfbizFtlObjectType.isObjectType(OfbizFtlObjectType.COMPLEXMAP, attribsModel)) {
                attribsModel = LangFtlUtil.toSimpleMap(attribsModel, false, objectWrapper);
            }
            res = LangFtlUtil.copyMapToSimple((TemplateHashModel) attribsModel, excludes, useExclude, objectWrapper);
        }

        // to get inline attribs, add list of all arg names to excludes as well as the lists themselves
        TemplateModel allArgNamesModel = argsMap.get("allArgNames");
        if (allArgNamesModel != null) {
            excludes.addAll(LangFtlUtil.getAsStringSet(allArgNamesModel));
        }
        excludes.add("allArgNames");
        excludes.add("localArgNames");

        // add the inline attribs over the attribs map (if any)
        if (res == null) {
            res = LangFtlUtil.copyMapToSimple(argsMap, excludes, useExclude, objectWrapper);
        } else {
            LangFtlUtil.putAll(res, argsMap, excludes, useExclude, objectWrapper);
        }
        
        return res;
    }

}
