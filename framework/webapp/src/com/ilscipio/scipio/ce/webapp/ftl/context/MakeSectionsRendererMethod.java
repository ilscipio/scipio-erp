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

import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.ftl.FtlSectionsRenderer;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: MakeSectionsRendererMethod - Makes a SectionsRenderer.
 */
public class MakeSectionsRendererMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @SuppressWarnings("unchecked")
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        return execTyped(args);
    }

    public Object execTyped(List<TemplateModel> args) throws TemplateModelException {
        if (args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }

        Environment env = FreeMarkerWorker.getCurrentEnvironment();

        TemplateModel arg1 = args.get(0);
        if (!(arg1 instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument (type) was not a string");
        }
        String type = LangFtlUtil.getAsStringNonEscaping((TemplateScalarModel) arg1);

        TemplateModel arg2 = args.get(1);
        if (!(arg2 instanceof TemplateHashModel)) {
            throw new TemplateModelException("Second argument (sectionsMap) was not a map");
        }
        TemplateHashModelEx sectionsMapModel = (TemplateHashModelEx) LangFtlUtil.toSimpleMap(arg2, false, env.getObjectWrapper());

        if ("ftl".equals(type)) {
            FtlSectionsRenderer sections = FtlSectionsRenderer.create(sectionsMapModel);
            return sections;
        } else if ("screen".equals(type)) {
            // TODO: "screen": WARN: due to build dependencies we won't be able to invoke widget renderer from here
            // may be forced to use reflection (dirty)...
            throw new TemplateModelException("First argument (type) currently only supports: ftl (screen type not yet implemented)");
        } else {
            throw new TemplateModelException("First argument (type) currently only supports: ftl");
        }
    }

}
