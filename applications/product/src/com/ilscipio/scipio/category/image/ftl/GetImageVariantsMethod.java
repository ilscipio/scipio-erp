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
package com.ilscipio.scipio.category.image.ftl;

import com.ilscipio.scipio.category.image.CategoryImageVariants;
import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.content.image.ContentImageVariants;
import freemarker.core.Environment;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: GetImageVariantsMethod.
 */
public class GetImageVariantsMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        return exec(UtilGenerics.cast(args), CommonFtlUtil.getCurrentEnvironment());
    }

    protected Object exec(@SuppressWarnings("rawtypes") List<TemplateModel> args, Environment env) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 4) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-4)");
        }
        TemplateModel firstArg = args.get(0);
        Map<String, TemplateModel> mapArgs = Collections.emptyMap();
        Map<String, Object> unwrappedMapArgs = Collections.emptyMap();

        String type = null;
        String contentId = null;
        String productCategoryId = null;
        String productCatContentTypeId = null;
        boolean useCache = true;

        if (firstArg instanceof TemplateScalarModel) {
            type = TransformUtil.getStringArg(firstArg);
            if ("content".equals(type)) {
                contentId = TransformUtil.getStringArg(args, 1);
                useCache = TransformUtil.getBooleanArg(args, 2, true);
            } else if ("category".equals(type)) {
                productCategoryId = TransformUtil.getStringArg(args, 1);
                productCatContentTypeId = TransformUtil.getStringArg(args, 2);
                useCache = TransformUtil.getBooleanArg(args, 3, true);
            } else {
                throw new TemplateModelException("Invalid image variants type. Supported: content, product");
            }
        } else if (firstArg instanceof TemplateHashModelEx) {
            mapArgs = LangFtlUtil.adaptAsMap((TemplateHashModelEx) args.get(0));
            unwrappedMapArgs = UtilGenerics.cast(LangFtlUtil.unwrapPermissive((TemplateHashModelEx) args.get(0)));
            //type = TransformUtil.getStringArg(mapArgs, "type");
            contentId = TransformUtil.getStringArg(mapArgs, "contentId");
            productCategoryId = TransformUtil.getStringArg(mapArgs, "productCategoryId");
            productCatContentTypeId = TransformUtil.getStringArg(mapArgs, "prodCatContentTypeId");
            useCache = TransformUtil.getBooleanArg(mapArgs, "useCache", true);
        } else {
            throw new TemplateModelException("First arg not instance of string or hash");
        }

        Delegator delegator = ContextFtlUtil.getDelegator(env);
        LocalDispatcher dispatcher = ContextFtlUtil.getDispatcher(env);
        Locale locale = TransformUtil.getOfbizLocaleArgOrCurrent(mapArgs, "locale", env);

        if (UtilValidate.isNotEmpty(productCategoryId)) {
            if (UtilValidate.isEmpty(productCatContentTypeId)) { // SCIPIO: 3.0.0: Default to ORIGINAL_IMAGE_URL
                productCatContentTypeId = "ORIGINAL_IMAGE_URL";
            }
            return CategoryImageVariants.from(productCategoryId, productCatContentTypeId, true, delegator, dispatcher, locale, useCache, unwrappedMapArgs);
        } else if (UtilValidate.isNotEmpty(contentId)) {
            boolean useUtilCache = TransformUtil.getBooleanArg(args, "useUtilCache", 2, true);
            return ContentImageVariants.from(contentId, delegator, dispatcher, locale, useUtilCache, unwrappedMapArgs);
        } else {
            // Behave nicely to simplify usage
            //throw new TemplateModelException("Invalid parameters: missing contentId or (productId + productContentTypeId)");
            return null;
        }
    }
}
