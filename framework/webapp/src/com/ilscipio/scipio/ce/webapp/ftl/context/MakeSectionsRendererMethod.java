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

import java.util.List;

import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.ftl.FtlSectionsRenderer;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: MakeSectionsRendererMethod - Makes a SectionsRenderer.
 */
public class MakeSectionsRendererMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execTyped(args);
    }
    
    public Object execTyped(List<TemplateModel> args) throws TemplateModelException {
        if (args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        
        TemplateModel arg1 = args.get(0);
        if (!(arg1 instanceof TemplateScalarModel)) {
            throw new TemplateModelException("First argument (type) was not a string");
        }
        String type = LangFtlUtil.getAsStringNonEscaping((TemplateScalarModel) arg1);
        
        TemplateModel arg2 = args.get(1);
        if (!(arg2 instanceof TemplateHashModel)) {
            throw new TemplateModelException("Second argument (sectionsMap) was not a map");
        }
        TemplateHashModelEx sectionsMapModel = (TemplateHashModelEx) LangFtlUtil.toSimpleMap(arg2, false, env.getObjectWrapper());

        if ("ftl".equals(type)) {
            FtlSectionsRenderer sections = FtlSectionsRenderer.create(sectionsMapModel);
            return sections;
        } else if ("screen".equals(type)) {
            // TODO: "screen": WARN: due to build dependencies we won't be able to invoke widget renderer from here
            // may be forced to use reflection (dirty)...
            throw new TemplateModelException("First argument (type) currently only supports: ftl (screen type not yet implemented)");
        } else {
            throw new TemplateModelException("First argument (type) currently only supports: ftl");
        }
    }
    
}
