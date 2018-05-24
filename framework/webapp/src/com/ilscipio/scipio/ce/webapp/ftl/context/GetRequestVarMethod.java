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
package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.List;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: GetRequestVarMethod - Freemarker Method for getting request-scope variables
 * with fallback to globals.
 * <p>
 * Should only be used to read values set by {@link SetRequestVarMethod}.
 */
public class GetRequestVarMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object res = ContextFtlUtil.getRequestVar(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), env);
        
        ObjectWrapper objectWrapper = getResultObjectWrapper(env);
        return LangFtlUtil.wrap(res, objectWrapper);
    }

    /**
     * Returns the appropriate result object wrapper for getRequestVar and analogous
     * methods that return results from the scipio request var map.
     * <p>
     * CURRENTLY (2016-04-20) This returns a NON-escaping wrapper. The values are generally
     * stored in the request map as TemplateModels so this wrapper has no effect, and
     * in general we define the interface such that what you put into the map is what you
     * get out.
     * <p>
     * This differs from other calls such as request.getAttribute() which perform auto-escaping.
     */
    public static ObjectWrapper getResultObjectWrapper(Environment env) {
        return LangFtlUtil.getNonEscapingObjectWrapper(env);
        //return LangFtlUtil.getCurrentObjectWrapper(env);
    }
}
