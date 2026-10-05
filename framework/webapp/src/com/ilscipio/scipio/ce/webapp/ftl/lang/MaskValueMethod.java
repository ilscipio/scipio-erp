/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
