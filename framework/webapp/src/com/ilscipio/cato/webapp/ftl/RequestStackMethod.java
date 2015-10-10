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
package com.ilscipio.cato.webapp.ftl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;

import freemarker.core.Environment;
import freemarker.template.SimpleScalar;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * Cato: RequestStackMethod - Freemarker Method base class providing support for a stack
 * structure having request scope, with fallback to globals.
 */
public abstract class RequestStackMethod implements TemplateMethodModelEx {

    public static final int INITIAL_CAPACITY = 10;
    
    public static final String module = RequestStackMethod.class.getName();

    protected Object execPush(List args) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        TemplateModel valueModel = (TemplateModel) args.get(1);

        String name = ((TemplateScalarModel) nameModel).getAsString();
        
        Environment env = FtlTransformUtil.getCurrentEnvironment();
        HttpServletRequest request = FtlTransformUtil.getRequest(env);

        if (request != null) {
            List<Object> stack;
            Object stackObj = request.getAttribute(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            else {
                if (stackObj != null) {
                    Debug.logWarning("Overriding request attribute with new stack (name: " + name + ")", module);
                }
                stack = new ArrayList<Object>(INITIAL_CAPACITY);
            }
            
            // WARNING: currently I don't see any need to unwrap this, 
            // so don't do it for performance reasons, but in future it could be needed.
            //stack.add(FtlTransformUtil.unwrap(valueModel));
            stack.add(valueModel);

            request.setAttribute(name, stack);
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(env);
            if (globalContext != null) {   
                List<Object> stack;
                Object stackObj = globalContext.get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                else {
                    if (stackObj != null) {
                        Debug.logWarning("Overriding globalContext var with new stack (name: " + name + ")", module);
                    }
                    stack = new ArrayList<Object>(INITIAL_CAPACITY);
                }
                
                // WARNING: currently I don't see any need to unwrap this, 
                // so don't do it for performance reasons, but in future it could be needed.
                //stack.add(FtlTransformUtil.unwrap(valueModel));
                stack.add(valueModel);
                
                globalContext.put(name, stack);
            }
            else {
                SimpleSequence stack;
                Object stackObj = env.getGlobalVariable(name);
                if (stackObj instanceof SimpleSequence) {
                    stack = (SimpleSequence) stackObj;
                }
                else {
                    if (stackObj != null) {
                        Debug.logWarning("Overriding FTL globals var with new stack (name: " + name + ")", module);
                    }
                    stack = new SimpleSequence(INITIAL_CAPACITY, env.getObjectWrapper());
                }
                
                // WARN: this sort of violates freemarker language by modifying list in-place,
                // but no one should ever be accessing this list anyway
                stack.add(valueModel);
                
                env.setGlobalVariable(name, stack);
            }
        }
        
        return new SimpleScalar("");
    }

    protected Object execRead(List args, boolean pop) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-2)");
        }
        TemplateModel nameModel = (TemplateModel) args.get(0);
        if (!(nameModel instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        }
        // the default value is a throwback to the #function definition version; doesn't hurt
        TemplateModel defaultValModel = (args.size() >= 2) ? (TemplateModel) args.get(1) : null;

        String name = ((TemplateScalarModel) nameModel).getAsString();
        
        Environment env = FtlTransformUtil.getCurrentEnvironment();
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        
        Object res = null;
        
        if (request != null) {
            List<Object> stack = null;
            Object stackObj = request.getAttribute(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            if (stack != null && !stack.isEmpty()) {
                res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
                if (pop) {
                    request.setAttribute(name, stack); // for correctness
                }
            }
            else if (pop) {
                Debug.logError("Trying to pop empty request attrib stack (name: " + name + ")", module);
            }
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(env);
            if (globalContext != null) {   
                List<Object> stack = null;
                Object stackObj = globalContext.get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                if (stack != null && !stack.isEmpty()) {
                    res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
                    if (pop) {
                        globalContext.put(name, stack); // for correctness
                    }
                }
                else if (pop) {
                    Debug.logError("Trying to pop empty globalContext stack (name: " + name + ")", module);
                }
            }
            else {
                SimpleSequence stack = null;
                Object stackObj = env.getGlobalVariable(name);
                if (stackObj instanceof SimpleSequence) {
                    stack = (SimpleSequence) stackObj;
                }
                if (stack != null && stack.size() >= 1) {
                    res = stack.get(stack.size() - 1);
                    if (pop) {
                        if (stack.size() <= 1) {
                            env.setGlobalVariable(name, null);
                        }
                        else {
                            // unfortunately this part is poor performance, but it's the only slow op
                            // in all of this (apart from recursive wrapping/unwrapping), so not big deal
                            SimpleSequence newStack = new SimpleSequence(INITIAL_CAPACITY, env.getObjectWrapper());
                            for(int i=0; i < (stack.size() - 1); i++) {
                                newStack.add(stack.get(i));
                            }
                            env.setGlobalVariable(name, newStack);
                        }
                    }
                }
                else if (pop) {
                    Debug.logError("Trying to pop empty FTL globals stack (name: " + name + ")", module);
                }
            }
        }
 
        return FtlTransformUtil.getDefaultIfNull(res, defaultValModel); // NOTE: result automatically wrapped as needed by freemarker
    }
    
}
