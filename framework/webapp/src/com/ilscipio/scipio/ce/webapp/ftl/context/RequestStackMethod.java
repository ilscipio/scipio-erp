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
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: RequestStackMethod - Freemarker Method base class providing support for a stack
 * structure having request scope, with fallback to globals.
 */
public abstract class RequestStackMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @SuppressWarnings("unchecked")
    protected Object execPush(List args, boolean setLast) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        ContextFtlUtil.pushRequestStack(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), valueModel, setLast, env);
        
        return new SimpleScalar("");
    }

    @SuppressWarnings("unchecked")
    protected Object execRead(List args, boolean pop) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object res = ContextFtlUtil.readRequestStack(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), pop, env);
 
        ObjectWrapper objectWrapper = GetRequestVarMethod.getResultObjectWrapper(env);
        return LangFtlUtil.wrap(res, objectWrapper);
    }
    
    @SuppressWarnings("unchecked")
    protected Object execGetAsList(List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        String stackName = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel));
        
        boolean origList = false;
        
        if (args.size() >= 2) {
            TemplateModel listTypeModel = (TemplateModel) args.get(1);
            String listType = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) listTypeModel));
            if ("orig".equals(listType)) {
                origList = true;
            }
        }
        
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        ObjectWrapper objectWrapper = GetRequestVarMethod.getResultObjectWrapper(env);
        Object res = ContextFtlUtil.getRequestStackAsList(stackName, 
                (origList ? null : LangFtlUtil.TemplateValueTargetType.SIMPLEMODEL), env, objectWrapper);
        return LangFtlUtil.wrap(res, objectWrapper);
    }    
    
    @SuppressWarnings("unchecked")
    protected Object execGetSize(List args) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        String stackName = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel));
        
        return ContextFtlUtil.getRequestStackSize(stackName, CommonFtlUtil.getCurrentEnvironment());
    }    
    
}
