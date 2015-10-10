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
package com.ilscipio.cato.webapp.ftl;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * Cato: SetRequestVarMethod - Freemarker Method for setting request-scope variables
 * with fallback to globals.
 * <p>
 * Values set by this transform should only be read using {@link GetRequestVarMethod}.
 */
public class SetRequestVarMethod implements TemplateMethodModelEx {

    public static final String module = SetRequestVarMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);

        String name = ((TemplateScalarModel) nameModel).getAsString();
        
        Environment env = FtlTransformUtil.getCurrentEnvironment();
        HttpServletRequest request = FtlTransformUtil.getRequest(env);

        if (request != null) {
            request.setAttribute(name, FtlTransformUtil.unwrap(valueModel));
        }
        else {
            // check if globalContext exists
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(env);
            if (globalContext != null) {
                globalContext.put(name, FtlTransformUtil.unwrap(valueModel));
            }
            else {
                // all we have is global FTL vars
                env.setGlobalVariable(name, valueModel);
            }
        }
        
        return new SimpleScalar("");
    }

}
