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
