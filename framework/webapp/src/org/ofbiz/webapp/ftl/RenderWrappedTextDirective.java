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
