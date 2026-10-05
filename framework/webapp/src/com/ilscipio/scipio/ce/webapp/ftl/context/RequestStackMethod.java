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
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: RequestStackMethod - Freemarker Method base class providing support for a stack
 * structure having request scope, with fallback to globals.
 */
public abstract class RequestStackMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected Object execPush(@SuppressWarnings("rawtypes") List args, boolean setLast) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        ContextFtlUtil.pushRequestStack(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), valueModel, setLast, env);

        return new SimpleScalar("");
    }

    protected Object execRead(@SuppressWarnings("rawtypes") List args, boolean pop) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        Object res = ContextFtlUtil.readRequestStack(LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel)), pop, env);

        ObjectWrapper objectWrapper = GetRequestVarMethod.getResultObjectWrapper(env);
        return LangFtlUtil.wrap(res, objectWrapper);
    }

    protected Object execGetAsList(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        String stackName = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel));

        boolean origList = false;

        if (args.size() >= 2) {
            TemplateModel listTypeModel = (TemplateModel) args.get(1);
            String listType = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) listTypeModel));
            if ("orig".equals(listType)) {
                origList = true;
            }
        }

        Environment env = CommonFtlUtil.getCurrentEnvironment();
        ObjectWrapper objectWrapper = GetRequestVarMethod.getResultObjectWrapper(env);
        Object res = ContextFtlUtil.getRequestStackAsList(stackName,
                (origList ? null : LangFtlUtil.TemplateValueTargetType.SIMPLEMODEL), env, objectWrapper);
        return LangFtlUtil.wrap(res, objectWrapper);
    }

    protected Object execGetSize(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 1) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        String stackName = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) nameModel));

        return ContextFtlUtil.getRequestStackSize(stackName, CommonFtlUtil.getCurrentEnvironment());
    }

}
