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

import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;

/**
 * SCIPIO: MaskValueLeftMethod - Helper method to mask a value.
 */
public abstract class MaskValueMethod implements TemplateMethodModelEx {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final char DEFAULT_MASK_CHAR = '*';

    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 2 || args.size() > 3) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2-3)");
        }
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        String str = LangFtlUtil.toRawJavaString((TemplateModel) args.get(0), env);
        int maskLength = ((TemplateNumberModel) args.get(1)).getAsNumber().intValue();
        char maskChar = DEFAULT_MASK_CHAR;
        if (args.size() >= 3) {
            String maskCharStr = LangFtlUtil.toRawJavaString(((TemplateModel) args.get(2)), env);
            if (maskCharStr != null && !maskCharStr.isEmpty()) {
                maskChar = maskCharStr.charAt(0);
            }
        }
        return doMask(str, maskLength, maskChar);
    }

    protected abstract String doMask(String str, int maskLength, char maskChar);

    public static class MaskValueLeftMethod extends MaskValueMethod {
        @Override
        protected String doMask(String str, int maskLength, char maskChar) {
            return StringUtil.maskLeft(str, maskLength, maskChar);
        }
    }

    public static class MaskValueRightMethod extends MaskValueMethod {
        @Override
        protected String doMask(String str, int maskLength, char maskChar) {
            return StringUtil.maskRight(str, maskLength, maskChar);
        }
    }
}
