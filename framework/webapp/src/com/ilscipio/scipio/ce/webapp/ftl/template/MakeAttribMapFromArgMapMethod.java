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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.OfbizFtlObjectType;

import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateSequenceModel;

/**
 * SCIPIO: MakeAttribMapFromArgMapMethod - Freemarker Method for getting an attribs map from an args map.
 */
public class MakeAttribMapFromArgMapMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }

        ObjectWrapper objectWrapper = CommonFtlUtil.getCurrentEnvironment().getObjectWrapper();

        // support empty list (ignore, treat as empty hash)
        TemplateModel argsObj = (TemplateModel) args.get(0);
        if (argsObj instanceof TemplateSequenceModel) {
            TemplateSequenceModel argsSeq = (TemplateSequenceModel) argsObj;
            if (argsSeq.size() == 0) {
                argsObj = TemplateHashModelEx.NOTHING;
            } else {
                throw new TemplateModelException("Invalid argument type (sequence) - expected hash");
            }
        }

        TemplateHashModelEx argsMap = (TemplateHashModelEx) argsObj;

        // caller-supplied excludes
        TemplateModel excludesModel = (args.size() >=2) ? (TemplateModel) args.get(1) : null;
        Set<String> excludes;
        if (excludesModel != null) {
            excludes = LangFtlUtil.getAsStringSet(excludesModel);
        } else {
            excludes = new HashSet<>();
        }

        SimpleHash res = null;

        final Boolean useExclude = Boolean.FALSE;

        // put attribs from explicit attribs map first, if any
        TemplateModel attribsModel = argsMap.get("attribs");
        if (attribsModel != null && OfbizFtlObjectType.isObjectType(OfbizFtlObjectType.MAP, attribsModel)) {
            if (OfbizFtlObjectType.isObjectType(OfbizFtlObjectType.COMPLEXMAP, attribsModel)) {
                attribsModel = LangFtlUtil.toSimpleMap(attribsModel, false, objectWrapper);
            }
            res = LangFtlUtil.copyMapToSimple((TemplateHashModel) attribsModel, excludes, useExclude, objectWrapper);
        }

        // to get inline attribs, add list of all arg names to excludes as well as the lists themselves
        TemplateModel allArgNamesModel = argsMap.get("allArgNames");
        if (allArgNamesModel != null) {
            excludes.addAll(LangFtlUtil.getAsStringSet(allArgNamesModel));
        }
        excludes.add("allArgNames");
        excludes.add("localArgNames");
        excludes.add("attribs"); // 2020-02-12: in most cases this was automatically added by makeArgMaps from default args list, but custom usages need this now

        // add the inline attribs over the attribs map (if any)
        if (res == null) {
            res = LangFtlUtil.copyMapToSimple(argsMap, excludes, useExclude, objectWrapper);
        } else {
            LangFtlUtil.putAll(res, argsMap, excludes, useExclude, objectWrapper);
        }

        return res;
    }

}
