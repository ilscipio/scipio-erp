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
import freemarker.template.SimpleHash;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: ArgsMapMethod - base class for methods that deal with Scipio macro arg patterns.
 */
public abstract class ArgMapMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected Object execMergeArgMaps(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames) throws TemplateModelException {
        return execMergeArgMaps(methodArgs, recordArgNames, CommonFtlUtil.getCurrentEnvironment());
    }    
    
    protected Object execMergeArgMaps(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames, Environment env) throws TemplateModelException {
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
        
        // NOTE: Here the choice of wrapper makes no real difference.
        ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper(env);
        return ContextFtlUtil.mergeArgMaps(args, inlineArgs, defaultArgs, overrideArgs, recordArgNames, env, objectWrapper);
    }    
    
    protected Object execMergeArgMapsToLocals(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames) throws TemplateModelException {
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        
        SimpleHash resArgs = (SimpleHash) execMergeArgMaps(methodArgs, recordArgNames, env);
        
        LangFtlUtil.localsPutAll(resArgs, env);
        env.setLocalVariable("args", resArgs);
        
        return new SimpleScalar(""); 
    }    
    
}
