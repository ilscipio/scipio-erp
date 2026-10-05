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
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: GetPropertyValueEntityMethod - Freemarker Method for getting properties
 */
public class GetEntityPropertyValueMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 2)
            throw new TemplateModelException("Invalid number of arguements");
        if (!(args.get(0) instanceof TemplateScalarModel))
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        if (!(args.get(1) instanceof TemplateScalarModel))
            throw new TemplateModelException("Second argument not an instance of TemplateScalarModel (string)");

        String resource = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(0)));
        String name = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1)));
        Delegator delegator = ContextFtlUtil.getDelegator(FreeMarkerWorker.getCurrentEnvironment());
        
        String res = EntityUtilProperties.getPropertyValue(resource, name, delegator);

        // here we do opposite of UtilProperties and return null if empty, so ! operator can work
        if (res != null && res.isEmpty()) {
            res = null;
        }
        // Prevent escaping by EscapingObjectWrapper - values from properties are always coded
        // by trusted people
        return LangFtlUtil.wrapNonEscaping(res);
    }

}
