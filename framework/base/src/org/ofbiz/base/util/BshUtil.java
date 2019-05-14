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
package org.ofbiz.base.util;

import java.util.Map;

/**
 * BshUtil - BeanShell Utilities
 * @deprecated SCIPIO: 2018-09-19: This now simply delegates to GroovyUtil as best-effort compatibility and 
 * will eventually be removed. WARN: Even the best-effort compatibility is incomplete from this file!
 */
@Deprecated
public final class BshUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Evaluate a BSH condition or expression
     * @deprecated SCIPIO: NOTE: this was always a poorly interfaced method, even its Groovy analog - do not use
     * @param expression The expression to evaluate
     * @param context The context to use in evaluation (re-written)
     * @return Object The result of the evaluation
     */
    @Deprecated
    public static final Object eval(String expression, Map<String, Object> context) {
        Debug.logWarning("Deprecated Beanshell expression eval called; "
                + "this is a compatibility mode only (runs as Groovy); please convert to Groovy script", module); 
        Object o = null;
        if (expression == null || expression.equals("")) {
            Debug.logError("BSH Evaluation error. Empty expression", module);
            return null;
        }

        if (Debug.verboseOn())
            Debug.logVerbose("Evaluating -- " + expression, module);
        if (Debug.verboseOn())
            Debug.logVerbose("Using Context -- " + context, module);

        try {
            // SCIPIO: WARN: this doesn't do full compatibility, but likely no one has used this at all
            o = GroovyUtil.eval(expression, context);
        } catch (Exception e) {
            Debug.logError(e, "BSH Evaluation error.", module);
        }
        return o;
    }

    public static Object makeInterpreter(Map<String, ? extends Object> context) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Beanshell is deprecated, cannot make an interpreter; please switch to Groovy");
    }

    public static Object getMasterInterpreter(ClassLoader classLoader) throws UnsupportedOperationException {
        throw new UnsupportedOperationException("Beanshell is deprecated, cannot make an interpreter; please switch to Groovy");
    }

    public static Object runBshAtLocation(String location, Map<String, ? extends Object> context) throws GeneralException {
        // SCIPIO: WARN: this doesn't do full compatibility, but likely no one has used this at all
        Debug.logWarning("Deprecated Beanshell script file invoked (" + location + "); "
                + "this is a compatibility mode only (runs as Groovy); please convert to Groovy script", module);
        return GroovyUtil.runScriptAtLocation(location, UtilGenerics.cast(context));
    }
}
