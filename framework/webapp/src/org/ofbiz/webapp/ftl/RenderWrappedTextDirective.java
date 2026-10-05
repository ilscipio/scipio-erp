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
package org.ofbiz.webapp.ftl;

import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

/**
 * RenderWrappedTextTransform - Freemarker Transform for URLs (links)
 * <p>
 * SCIPIO: 2019-02-05: Reimplemented as TemplateDirectiveModel (was previously: RenderWrappedTextTransform)
 */
public class RenderWrappedTextDirective implements TemplateDirectiveModel {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        Map<String, Object> ctx = UtilGenerics.checkMap(FreeMarkerWorker.getWrappedObject("context", env), String.class, Object.class);
        String wrappedFTL = FreeMarkerWorker.getArg(UtilGenerics.checkMap(args, String.class, Object.class), "wrappedFTL", ctx);
        if (UtilValidate.isNotEmpty(wrappedFTL)) {
            env.getOut().write(wrappedFTL);
        } else {
            if (Debug.verboseOn()) {
                Debug.logVerbose("wrappedFTL was empty. skipping write.", module);
            }
        }
    }
}
