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
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: SetRequestVarMethod - Freemarker Method for setting request-scope variables
 * with fallback to globals.
 * <p>
 * Values set by this transform should only be read using {@link GetRequestVarMethod}.
 * 
 * @see com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil#setRequestVar
 */
public class SetRequestVarMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() < 2 || args.size() > 3) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2-3)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);
        
        Boolean unwrap = null;
        if (args.size() >= 3) {
            TemplateModel modeModel = (TemplateModel) args.get(2);
            if (modeModel != null) {
                String mode = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) modeModel));
                if ("u".equals(mode)) {
                    unwrap = Boolean.TRUE;
                }
                else if ("w".equals(mode)) {
                    unwrap = Boolean.FALSE;
                }
            }
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object value = valueModel;
        if (unwrap == Boolean.TRUE) {
            value = LangFtlUtil.unwrapPermissive(valueModel);
        }
        ContextFtlUtil.setRequestVar(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), value, env);

        return new SimpleScalar("");
    }

}
