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

import freemarker.template.*;
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;

import java.util.*;

/**
 * SCIPIO: #runScript function implementation
 */
public class RunScriptMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        // TODO: support more args
        String scriptLocation = (args != null) ? TransformUtil.getStringArg(args, 0) : null;
        if (UtilValidate.isEmpty(scriptLocation)) {
            throw new TemplateModelException("Missing or invalid script location argument for #dateScript Freemarker function");
        }
        try {
            return executeScriptAtLocation(scriptLocation, ContextFtlUtil.getContext(FreeMarkerWorker.getCurrentEnvironment()));
        } catch (GeneralException e) {
            // TODO: REVIEW: it should at least be an option to catch errors here (but not forced always)...
            throw new TemplateModelException(e);
        }
    }

    // FIXME: duplicated due to dependency issues from: org.ofbiz.widget.model.AbstractModelAction.Script.executeScriptAtLocation
    private static Object executeScriptAtLocation(String scriptLocation, Map<String, Object> context) throws GeneralException { // SCIPIO: refactored from runAction()
        String location = getScriptLocation(scriptLocation);
        String method = getScriptMethodName(scriptLocation);

        if (location.endsWith(".xml")) {
            Map<String, Object> localContext = new HashMap<>();
            localContext.putAll(context);
            DispatchContext ctx = ((LocalDispatcher) context.get("dispatcher")).getDispatchContext();
            MethodContext methodContext = new MethodContext(ctx, localContext, null);
            try {
                Object result = SimpleMethod.runSimpleMethod(location, method, methodContext);
                context.putAll(methodContext.getResults());
                return result; // SCIPIO: return result
            } catch (MiniLangException e) {
                throw new GeneralException("Error running simple method at location [" + location + "]", e);
            }
        } else {
            return ScriptUtil.executeScript(location, method, context);
        }
    }

    private static String getScriptLocation(String combinedName) { // FIXME: duplicated from WidgetWorker
        int pos = combinedName.lastIndexOf('#');
        if (pos == -1) {
            return combinedName;
        }
        return combinedName.substring(0, pos);
    }

    private static String getScriptMethodName(String combinedName) { // FIXME: duplicated from WidgetWorker
        int pos = combinedName.lastIndexOf('#');
        if (pos == -1) {
            return null;
        }
        return combinedName.substring(pos + 1);
    }
}
