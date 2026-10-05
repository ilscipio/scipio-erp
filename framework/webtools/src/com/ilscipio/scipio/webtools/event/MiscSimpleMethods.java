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
package com.ilscipio.scipio.webtools.event;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://webtools/script/com/ilscipio/scipio/webtools/MiscSimpleMethods.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class MiscSimpleMethods {

    private static final String MODULE = MiscSimpleMethods.class.getName();


    /**
     * bsh backward-compatibility
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testBshCompat(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("groovy:\n            import org.ofbiz.base.util.*;\n            final module = \"testSimpleMethod1.groovy\";\n            x = null;\n            try {\n                x = somemissingpropertyname;\n                Debug.logError(\"testSimpleMethod1: Standard groovy: Missing property was improperly detected!\", module);\n            } catch(groovy.lang.MissingPropertyException e) {\n                Debug.logInfo(\"testSimpleMethod1: Standard groovy: Missing property was triggered and caught successfully.\", module);\n            }\n            context.testSmOutVar1 = \"hello from groovy\";", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        result.put("testSmOutVar1", context.get("testSmOutVar1"));
        // TODO: Convert <call-bsh> element
        result.put("testSmOutVar2", context.get("testSmOutVar2"));
        Object testBshVar1 = GroovyUtil.eval("org.ofbiz.base.util.Debug.logInfo('testSimpleMethod1: running from deprecated bsh scriptlet', 'testSimpleMethod1.groovy')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("bsh:\n            org.ofbiz.base.util.Debug.logInfo('testSimpleMethod1: running from deprecated bsh script block', 'testSimpleMethod1.groovy')\n            org.ofbiz.base.util.string.FlexibleStringExpander.getInstance(\n                \"\\${bsh: org.ofbiz.base.util.Debug.logInfo('testSimpleMethod1: running from deprecated bsh flexible expression', 'testSimpleMethod1.groovy')}\"\n            ).expandString(context);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }

        return "success";
    }

}
