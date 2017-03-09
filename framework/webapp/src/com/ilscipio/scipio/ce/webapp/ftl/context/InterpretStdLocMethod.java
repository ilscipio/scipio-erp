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
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: InterpretStdLocMethod - Template interpretation method, alternative to <code>?interpret</code> built-in
 * and tailored to Scipio needs.
 */
public class InterpretStdLocMethod extends InterpretStdMethod {

    public static final String module = InterpretStdLocMethod.class.getName();

    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execTyped(args, true);
    }
    
}
