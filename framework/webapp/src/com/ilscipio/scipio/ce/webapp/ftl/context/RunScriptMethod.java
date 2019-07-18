/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
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
