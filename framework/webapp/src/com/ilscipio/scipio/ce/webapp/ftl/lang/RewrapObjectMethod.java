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
package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.List;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: RewrapObjectMethod - Rewraps objects with different Freemarker wrappers.
 */
public class RewrapObjectMethod implements TemplateMethodModelEx {

    public static final String module = RewrapObjectMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 3 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-3)");
        }
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        TemplateModel object = (TemplateModel) args.get(0);
        
        String wrapperStr = null;
        String modeStr = null;
        if (args.size() >= 2) {        
            TemplateScalarModel wrapperModel = (TemplateScalarModel) args.get(1);
            if (wrapperModel != null) {
                wrapperStr = wrapperModel.getAsString();
            }
            
            if (args.size() >= 3) {
                TemplateScalarModel modeModel = (TemplateScalarModel) args.get(2);
                if (modeModel != null) {
                    modeStr = modeModel.getAsString();
                }
            }
        }

        Object res = LangFtlUtil.rewrapObject(object, WrappingOptions.makeOptions(wrapperStr, modeStr, env), env);
        return res;
    }
    
}
