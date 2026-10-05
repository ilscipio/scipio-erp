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

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: CopyObjectMethod - Helper method to clone (shallow copy) a map or list.
 */
public class CopyMapMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 3) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-3)");
        }
        TemplateModel hashObjModel = (TemplateModel) args.get(0);
        if (!(hashObjModel instanceof TemplateHashModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateHashModel");
        }
        TemplateHashModel hashModel = (TemplateHashModel) hashObjModel;

        String mode = null;
        if (args.size() >= 2) {
            mode = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1)));
        }

        TemplateModel keysModel = null;
        if (args.size() >= 3) {
            keysModel = (TemplateModel) args.get(2);
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();

        Boolean include = null;
        if (mode != null && !mode.isEmpty()) {
            if (mode.contains("i")) {
                include = Boolean.TRUE;
            }
            else if (mode.contains("e")) {
                include = Boolean.FALSE;
            }
        }
        ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper(env);
        return LangFtlUtil.copyMap(hashModel, LangFtlUtil.getAsStringSet(keysModel), include,
                LangFtlUtil.TemplateValueTargetType.SIMPLEMODEL, objectWrapper);
    }

}
