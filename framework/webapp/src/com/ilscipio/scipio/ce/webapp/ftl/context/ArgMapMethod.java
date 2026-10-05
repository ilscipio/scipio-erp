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
import freemarker.template.SimpleHash;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: ArgsMapMethod - base class for methods that deal with Scipio macro arg patterns.
 */
public abstract class ArgMapMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected Object execMergeArgMaps(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames) throws TemplateModelException {
        return execMergeArgMaps(methodArgs, recordArgNames, CommonFtlUtil.getCurrentEnvironment());
    }

    protected Object execMergeArgMaps(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames, Environment env) throws TemplateModelException {
        TemplateHashModelEx args = null;
        if (methodArgs.size() >= 1) {
            args = (TemplateHashModelEx) methodArgs.get(0);
        }
        TemplateHashModelEx inlineArgs = null;
        if (methodArgs.size() >= 2) {
            inlineArgs = (TemplateHashModelEx) methodArgs.get(1);
        }
        TemplateHashModelEx defaultArgs = null;
        if (methodArgs.size() >= 3) {
            defaultArgs = (TemplateHashModelEx) methodArgs.get(2);
        }
        TemplateHashModelEx overrideArgs = null;
        if (methodArgs.size() >= 4) {
            overrideArgs = (TemplateHashModelEx) methodArgs.get(3);
        }

        // NOTE: Here the choice of wrapper makes no real difference.
        ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper(env);
        return ContextFtlUtil.mergeArgMaps(args, inlineArgs, defaultArgs, overrideArgs, recordArgNames, env, objectWrapper);
    }

    protected Object execMergeArgMapsToLocals(@SuppressWarnings("rawtypes") List methodArgs, boolean recordArgNames) throws TemplateModelException {
        Environment env = CommonFtlUtil.getCurrentEnvironment();

        SimpleHash resArgs = (SimpleHash) execMergeArgMaps(methodArgs, recordArgNames, env);

        LangFtlUtil.localsPutAll(resArgs, env);
        env.setLocalVariable("args", resArgs);

        return new SimpleScalar("");
    }

}
