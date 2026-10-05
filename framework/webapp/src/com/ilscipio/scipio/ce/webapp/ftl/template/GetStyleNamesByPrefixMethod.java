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
package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: GetStyleNamesByPrefixMethod - Freemarker Method for extracting style names by prefix from a style string.
 */
public class GetStyleNamesByPrefixMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        String styleString = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(0)));
        styleString = TemplateFtlUtil.getPlainClassArgNames(styleString);

        String prefix = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1)));

        String[] names = StringUtils.split(styleString, ' ');
        // NOTE: For emergency/safety reasons, use the current wrapper, which MAY be escaping.
        // style strings contain only simple characters anyway.
        ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper();
        SimpleSequence res = new SimpleSequence(names.length, objectWrapper);

        for(String name : names) {
            if (name.startsWith(prefix)) {
                res.add(name);
            }
        }

        // redundant
        //return LangFtlUtil.wrap(res, objectWrapper);
        return res;
    }

}
