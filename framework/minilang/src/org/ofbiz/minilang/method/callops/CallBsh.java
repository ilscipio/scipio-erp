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
package org.ofbiz.minilang.method.callops;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

import org.codehaus.groovy.control.CompilationFailedException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.GroovyUtil.GroovyLangVariant;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.MiniLangUtil;
import org.ofbiz.minilang.MiniLangValidate;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.minilang.method.MethodOperation;
import org.w3c.dom.Element;

/**
 * Implements the &lt;call-bsh&gt; element.
 * @deprecated SCIPIO: 2018-09-19: The "script" element should be used instead of this
 *  everywhere from now on; the class remains purely for backward-compatibility for very old code.
 *
 * @see <a href="https://cwiki.apache.org/confluence/display/OFBADMIN/Mini-language+Reference#Mini-languageReference-{{%3Ccallbsh%3E}}">Mini-language Reference</a>
 */
@Deprecated
public final class CallBsh extends MethodOperation {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // This method is needed only during the v1 to v2 transition
    private static boolean autoCorrect(Element element) {
        boolean elementModified = false;
        String errorListAttr = element.getAttribute("error-list-name");
        if (errorListAttr.length() > 0) {
            element.removeAttribute("error-list-name");
            elementModified = true;
        }
        return elementModified;
    }

    private final String inline;
    private final String resource;

    public CallBsh(Element element, SimpleMethod simpleMethod) throws MiniLangException {
        super(element, simpleMethod);
        if (MiniLangValidate.validationOn()) {
            MiniLangValidate.handleError("<call-bsh> element is deprecated (use <script>)", simpleMethod, element);
            MiniLangValidate.attributeNames(simpleMethod, element, "resource");
            MiniLangValidate.constantAttributes(simpleMethod, element, "resource");
            MiniLangValidate.noChildElements(simpleMethod, element);
        }
        boolean elementModified = autoCorrect(element);
        if (elementModified && MiniLangUtil.autoCorrectOn()) {
            MiniLangUtil.flagDocumentAsCorrected(element);
        }
        this.inline = StringUtil.convertOperatorSubstitutions(UtilXml.elementValue(element));
        this.resource = element.getAttribute("resource");
        // SCIPIO: 2018-09-19: Print this even if validation not enabled, people need to remove this from their code
        Debug.logWarning("Deprecated <call-bsh> element found in simple-method '" 
                + simpleMethod.getLocationAndName() + "'; this is a compatibility mode only (runs as Groovy)"
                + "; please update code to use <script> with groovy", module);
    }

    @Override
    public boolean exec(MethodContext methodContext) throws MiniLangException {
        //bsh.setClassLoader(methodContext.getLoader());
        try {
            //// setup environment
            //for (Map.Entry<String, Object> entry : methodContext.getEnvMap().entrySet()) {
            //    bsh.set(entry.getKey(), entry.getValue());
            //}
            // run external, from resource, first if resource specified
            if (UtilValidate.isNotEmpty(this.resource)) {
                InputStream is = methodContext.getLoader().getResourceAsStream(this.resource);
                if (is == null) {
                    this.simpleMethod.addErrorMessage(methodContext, "Could not find bsh resource: " + this.resource);
                } else {
                    BufferedReader reader = null;
                    try {
                        reader = new BufferedReader(new InputStreamReader(is));
                        StringBuilder outSb = new StringBuilder();
                        String tempStr = null;
                        while ((tempStr = reader.readLine()) != null) {
                            outSb.append(tempStr);
                            outSb.append('\n');
                        }
                        // SCIPIO: 2018-09-19: evaluate as a BSH-emulated groovy block
                        //Object resourceResult = bsh.eval(outSb.toString());
                        Object resourceResult = GroovyUtil.evalBlock(outSb.toString(), methodContext.getEnvMap(), 
                                GroovyLangVariant.BSH, true);
                        // if map is returned, copy values into env
                        if ((resourceResult != null) && (resourceResult instanceof Map<?, ?>)) {
                            methodContext.putAllEnv(UtilGenerics.<String, Object> checkMap(resourceResult));
                        }
                    } catch (IOException e) {
                        this.simpleMethod.addErrorMessage(methodContext, "IO error loading bsh resource: " + e.getMessage());
                    } finally {
                        if (reader != null) {
                            try {
                                reader.close();
                            } catch (IOException e) {
                                this.simpleMethod.addErrorMessage(methodContext, "IO error closing BufferedReader: " + e.getMessage());
                            }
                        }
                    }
                }
            }
            if (Debug.verboseOn())
                Debug.logVerbose("Running inline BSH script: " + inline, module);
            // run inlined second to it can override the one from the property
            // SCIPIO: 2018-09-19: evaluate as a BSH-emulated groovy block
            //Object inlineResult = bsh.eval(inline);
            Object inlineResult = GroovyUtil.evalBlock(inline, methodContext.getEnvMap(), 
                    GroovyLangVariant.BSH, true);
            if (Debug.verboseOn())
                Debug.logVerbose("Result of inline BSH script: " + inlineResult, module);
            // if map is returned, copy values into env
            if ((inlineResult != null) && (inlineResult instanceof Map<?, ?>)) {
                methodContext.putAllEnv(UtilGenerics.<String, Object> checkMap(inlineResult));
            }
        } catch (CompilationFailedException e) {
            Debug.logWarning(e, "Groovy execution (through deprecated <call-bsh>) caused an error", module);
            this.simpleMethod.addErrorMessage(methodContext, "Groovy execution caused an error: " + e.getMessage());
        }
        // always return true, error messages just go on the error list
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("<set ");
        if (this.resource.length() > 0) {
            sb.append("resource=\"").append(this.resource).append("\" ");
        }
        sb.append("/>");
        return sb.toString();
    }

    /**
     * A factory for the &lt;call-bsh&gt; element.
     */
    public static final class CallBshFactory implements Factory<CallBsh> {
        @Override
        public CallBsh createMethodOperation(Element element, SimpleMethod simpleMethod) throws MiniLangException {
            return new CallBsh(element, simpleMethod);
        }

        @Override
        public String getName() {
            return "call-bsh";
        }
    }
}
