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

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.OfbizFtlObjectType;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker.InvokeOptions;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker.InvokeOptions.InvokeMode;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker.WrapperModel;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateSource;

import freemarker.core.Environment;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: InterpretStdMethod - Template interpretation method, alternative to <code>?interpret</code> built-in
 * and tailored to Scipio needs.
 */
public class InterpretStdMethod implements TemplateMethodModelEx {

    public static final String module = InterpretStdMethod.class.getName();

    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execTyped(args);
    }
    
    public Object execTyped(List<TemplateModel> args) throws TemplateModelException {
        if (args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel arg = args.get(0);
        
        String str = null;
        String location = null;
        String invokeModeStr = null;
        Boolean pushCtx = null;
        String modelStr = null;
        
        if (OfbizFtlObjectType.MAP.isObjectType(arg)) {
            // NOTE: the unwrap call should have bypassed auto-escaping automatically...
            Map<String, Object> argMap = UtilGenerics.checkMap(LangFtlUtil.unwrapAlways(arg));
            
            str = (String) argMap.get("str");
            location = (String) argMap.get("location");
            pushCtx = (Boolean) argMap.get("pushCtx");
            invokeModeStr = (String) argMap.get("invokeMode");
            modelStr = (String) argMap.get("model");
        } else if (arg instanceof TemplateScalarModel) {
            str = LangFtlUtil.getAsStringNonEscaping((TemplateScalarModel) arg);
        } else {
            throw new TemplateModelException("Invalid arg type (expected string or hash): " + arg.getClass());
        }
        InvokeMode invokeMode;
        WrapperModel model;
        try {
            invokeMode = UtilValidate.isNotEmpty(invokeModeStr) ? 
                InvokeMode.fromNameAlways(invokeModeStr) : null;
            model = UtilValidate.isNotEmpty(modelStr) ? 
                    WrapperModel.fromNameAlways(modelStr) : null;
        } catch(Exception e) {
            throw new TemplateModelException(e);
        }
        
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        // TODO?: currently don't support specifying the configuration, can only reuse current
        // (ambiguous how would be specified)
        Configuration config = env.getConfiguration();
        UtilCache<String, Template> cache = null;
        
        TemplateSource templateSource;
        try {
            if (UtilValidate.isNotEmpty(location)) {
                cache = TemplateSource.getTemplateLocationCacheForConfig(config, env);
                if (cache == null) {
                    Debug.logWarning("Scipio: #interpretStd: could not determine"
                            + " a location template cache to use; not using cache", module);
                }
                templateSource = TemplateSource.getForLocation(location, cache, config);
            } else if (str != null) {
                cache = TemplateSource.getTemplateInlineSelfCacheForConfig(config, env);
                if (cache == null) {
                    Debug.logWarning("Scipio: #interpretStd: could not determine"
                            + " an inline template cache to use; not using cache", module);
                } 
                templateSource = TemplateSource.getForInlineSelfCache(str, cache, config);
            } else {
                throw new TemplateModelException("Expected a 'location' or inline 'str' in interpretStd args map, but none passed");
            }
        } catch (TemplateException e) {
            throw new TemplateModelException(e);
        } catch (IOException e) {
            throw new TemplateModelException(e);
        }

        TemplateInvoker invoker;
        try {
            // NOTE: must get StringInvoker so BeansWrapper's StringModel can invoke toString()
            invoker = TemplateInvoker.getStringInvoker(templateSource, new InvokeOptions(invokeMode, pushCtx), model);
        } catch (TemplateException e) {
            throw new TemplateModelException(e);
        } catch (IOException e) {
            throw new TemplateModelException(e);
        }
        
        // TODO?: in the future the wrap logic should be handled in a custom ObjectWrapper,
        // but the scope of such change is too big for now.
        // note this means the selection of this wrapper will be lost on subsequent unwrap+rewrap.
        // NOTE: currently we encourage simple StringModel wrapper here (stock ftl)
        return TemplateInvoker.wrap(invoker, env.getObjectWrapper());
    }

}
