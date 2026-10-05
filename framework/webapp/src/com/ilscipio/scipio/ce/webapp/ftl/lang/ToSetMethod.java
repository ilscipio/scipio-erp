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

import java.util.HashSet;
import java.util.List;

import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: ToSetMethod - Converts a sequence or collection to bean-wrapped set.
 */
public class ToSetMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 0 || args.size() > 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 0-1)");
        }
        //Environment env = CommonFtlUtil.getCurrentEnvironment();

        if (args.size() > 0) {
            TemplateModel object = (TemplateModel) args.get(0);

            //ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper(env);
            return LangFtlUtil.toSet(object);
        }
        else {
            return new HashSet<Object>();
        }
    }

}
