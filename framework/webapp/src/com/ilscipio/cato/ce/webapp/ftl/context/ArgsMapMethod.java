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
package com.ilscipio.cato.ce.webapp.ftl.context;

import java.util.List;

import com.ilscipio.cato.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.cato.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * Cato: ArgsMapMethod - base class for methods that deal with Cato macro arg patterns.
 */
public abstract class ArgsMapMethod implements TemplateMethodModelEx {

    public static final String module = ArgsMapMethod.class.getName();
    
    @SuppressWarnings("unchecked")
    protected Object execMergeArgMaps(List methodArgs, boolean recordArgNames) throws TemplateModelException {
        return execMergeArgMaps(methodArgs, recordArgNames, CommonFtlUtil.getCurrentEnvironment());
    }    
    
    @SuppressWarnings("unchecked")
    protected Object execMergeArgMaps(List methodArgs, boolean recordArgNames, Environment env) throws TemplateModelException {
        TemplateHashModelEx args = null;
        if (methodArgs.size() >= 1) {
            args = (TemplateHashModelEx) methodArgs.get(0);
        }
        TemplateHashModelEx inlineArgs = null;
        if (methodArgs.size() >= 2) {
            inlineArgs = (TemplateHashModelEx) methodArgs.get(1);
        }
        TemplateHashModelEx defaultArgs = null;
        if (methodArgs.size() >= 3) {
            defaultArgs = (TemplateHashModelEx) methodArgs.get(2);
        }
        TemplateHashModelEx overrideArgs = null;
        if (methodArgs.size() >= 4) {
            overrideArgs = (TemplateHashModelEx) methodArgs.get(3);
        }
        
        return ContextFtlUtil.mergeArgMaps(args, inlineArgs, defaultArgs, overrideArgs, recordArgNames, env);
    }    
    
    @SuppressWarnings("unchecked")
    protected Object execProcessArgMaps(List methodArgs, boolean recordArgNames) throws TemplateModelException {
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        
        SimpleHash resArgs = (SimpleHash) execMergeArgMaps(methodArgs, recordArgNames, env);
        
        LangFtlUtil.localsPutAll(resArgs, env);
        
        resArgs.put("args", resArgs);
        resArgs.put("origArgs", resArgs);
        
        return new SimpleScalar(""); 
    }    
    
}
