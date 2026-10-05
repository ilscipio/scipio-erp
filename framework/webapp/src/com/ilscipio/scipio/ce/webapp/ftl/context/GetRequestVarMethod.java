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
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: GetRequestVarMethod - Freemarker Method for getting request-scope variables
 * with fallback to globals.
 * <p>
 * Should only be used to read values set by {@link SetRequestVarMethod}.
 */
public class GetRequestVarMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object res = ContextFtlUtil.getRequestVar(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), env);

        ObjectWrapper objectWrapper = getResultObjectWrapper(env);
        return LangFtlUtil.wrap(res, objectWrapper);
    }

    /**
     * Returns the appropriate result object wrapper for getRequestVar and analogous
     * methods that return results from the scipio request var map.
     * <p>
     * CURRENTLY (2016-04-20) This returns a NON-escaping wrapper. The values are generally
     * stored in the request map as TemplateModels so this wrapper has no effect, and
     * in general we define the interface such that what you put into the map is what you
     * get out.
     * <p>
     * This differs from other calls such as request.getAttribute() which perform auto-escaping.
     */
    public static ObjectWrapper getResultObjectWrapper(Environment env) {
        return LangFtlUtil.getNonEscapingObjectWrapper(env);
        //return LangFtlUtil.getCurrentObjectWrapper(env);
    }
}
