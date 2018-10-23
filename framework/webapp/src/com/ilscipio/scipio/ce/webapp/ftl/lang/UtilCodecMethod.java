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

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: UtilCodecMethod - Wrappers around UtilCodec helper methods.
 */
public abstract class UtilCodecMethod implements TemplateMethodModelEx {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        Environment env = FreeMarkerWorker.getCurrentEnvironment();

        String value = LangFtlUtil.toRawJavaString((TemplateModel) args.get(0), env);
        String lang = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1)));

        return new SimpleScalar(langExec(value, lang));
    }

    protected abstract String langExec(String value, String lang);

    public static class DecodeMethod extends UtilCodecMethod {
        @Override
        protected String langExec(String value, String lang) {
            return UtilCodec.decode(value, lang);
        }
    }

    public static class EncodeMethod extends UtilCodecMethod {
        @Override
        protected String langExec(String value, String lang) {
            return UtilCodec.encode(value, lang);
        }
    }

    public static class SanitizeMethod extends UtilCodecMethod {
        @Override
        protected String langExec(String value, String lang) {
            return UtilCodec.sanitize(value, lang);
        }
    }
}
