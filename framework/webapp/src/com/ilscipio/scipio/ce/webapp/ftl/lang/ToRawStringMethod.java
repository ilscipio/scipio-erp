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

import java.util.Collection;
import java.util.List;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: ToRawStringMethod - Prevents Ofbiz auto-escaping of string.
 */
public class ToRawStringMethod implements TemplateMethodModelEx {

    public static final String module = ToRawStringMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        List<TemplateModel> values = UtilGenerics.cast(args);
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        if (args.size() == 1) {
            return LangFtlUtil.toRawString(values.get(0), env);
        } else if (args.size() > 1) {
            return LangFtlUtil.toRawString(values, env);
        } else {
            throw new TemplateModelException("Invalid number of parameters passed to toRawString/rawString (none passed)");
        }
    }
    

    
}
