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
import java.util.Map;

import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SetGlobalContextFieldMethod - Freemarker Method for setting global context fields
 * <p>
 * Derived from {@link org.ofbiz.webapp.ftl.SetContextFieldTransform}.
 */
public class SetGlobalContextFieldMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() != 2)
            throw new TemplateModelException("Invalid number of arguments");
        if (!(args.get(0) instanceof TemplateScalarModel))
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel");
        // SCIPIO: This is too limiting
        //if (!(args.get(1) instanceof BeanModel) && !(args.get(1) instanceof TemplateNumberModel) && !(args.get(1) instanceof TemplateScalarModel))
        //    throw new TemplateModelException("Second argument not an instance of BeanModel nor TemplateNumberModel nor TemplateScalarModel");

        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        BeanModel globalContextModel = (BeanModel) env.getVariable("globalContext");
        Map<String, Object> globalContext = (Map<String, Object>) globalContextModel.getWrappedObject();

        String name = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(0)));
        Object valueModel = args.get(1);
        Object value = null;
        // SCIPIO: Let DeepUnwrap handle this...
        //if (args.get(1) instanceof TemplateScalarModel)
        //    value = ((TemplateScalarModel) args.get(1)).getAsString();
        //if (args.get(1) instanceof TemplateNumberModel)
        //    value = ((TemplateNumberModel) args.get(1)).getAsNumber();
        //if (args.get(1) instanceof BeanModel)
        //    value = ((BeanModel) args.get(1)).getWrappedObject();
        // SCIPIO: NOTE: Unlike this above, this call will avoid the auto-escaping as implemented by Ofbiz (sensitive to DeepUnwrap implementation)
        value = LangFtlUtil.unwrapAlwaysUnlessNull(valueModel);
        
        globalContext.put(name, value);
        return new SimpleScalar("");
    }

}
