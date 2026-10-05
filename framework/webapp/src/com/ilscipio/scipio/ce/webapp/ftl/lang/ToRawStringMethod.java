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

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: ToRawStringMethod - Prevents Ofbiz auto-escaping of string.
 */
public class ToRawStringMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
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
