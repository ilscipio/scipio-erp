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
package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.List;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: SetRequestVarMethod - Freemarker Method for setting request-scope variables
 * with fallback to globals.
 * <p>
 * Values set by this transform should only be read using {@link GetRequestVarMethod}.
 *
 * @see com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil#setRequestVar
 */
public class SetRequestVarMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 2 || args.size() > 3) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2-3)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);

        Boolean unwrap = null;
        if (args.size() >= 3) {
            TemplateModel modeModel = (TemplateModel) args.get(2);
            if (modeModel != null) {
                String mode = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) modeModel));
                if ("u".equals(mode)) {
                    unwrap = Boolean.TRUE;
                } else if ("w".equals(mode)) {
                    unwrap = Boolean.FALSE;
                }
            }
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object value = valueModel;
        if (Boolean.TRUE.equals(unwrap)) {
            value = LangFtlUtil.unwrapPermissive(valueModel);
        }
        ContextFtlUtil.setRequestVar(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), value, env);

        return new SimpleScalar("");
    }

}
