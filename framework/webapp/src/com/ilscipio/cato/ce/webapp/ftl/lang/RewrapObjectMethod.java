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
package com.ilscipio.cato.ce.webapp.ftl.lang;

import java.util.List;

import com.ilscipio.cato.ce.webapp.ftl.CommonFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * Cato: RewrapObjectMethod - Rewraps objects with different Freemarker wrappers.
 */
public class RewrapObjectMethod implements TemplateMethodModelEx {

    public static final String module = RewrapObjectMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        TemplateModel object = (TemplateModel) args.get(0);
        
        String modeStr = null;
        TemplateScalarModel modeModel = (TemplateScalarModel) args.get(1);
        if (modeModel != null) {
            modeStr = modeModel.getAsString();
        }
        RewrapMode mode;
        if (modeStr == null || modeStr.length() <= 0) {
            mode = RewrapMode.SIMPLE;
        } else {
            mode = RewrapMode.fromString(modeStr);
            if (mode == null) {
                throw new TemplateModelException("Unrecognized mode: " + modeStr);
            }
        }

        return LangFtlUtil.rewrapObject(object, mode, env, LangFtlUtil.getCurrentObjectWrapper());
    }
    
}
