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
package com.ilscipio.cato.ce.webapp.ftl.context;

import java.util.List;

import org.ofbiz.base.util.UtilProperties;

import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * GetPropertyValue - Freemarker Method for getting properties
 */
public class GetPropertyValue implements TemplateMethodModelEx {

    public static final String module = GetPropertyValue.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() != 2)
            throw new TemplateModelException("Invalid number of arguements");
        if (!(args.get(0) instanceof TemplateScalarModel))
            throw new TemplateModelException("First argument not an instance of TemplateScalarModel (string)");
        if (!(args.get(1) instanceof TemplateScalarModel))
            throw new TemplateModelException("Second argument not an instance of TemplateScalarModel (string)");

        String resource = ((TemplateScalarModel) args.get(0)).getAsString();
        String name = ((TemplateScalarModel) args.get(1)).getAsString();
        
        String res = UtilProperties.getPropertyValue(resource, name);

        // here we do opposite of UtilProperties and return null if empty, so ! operator can work
        if (res != null && res.isEmpty()) {
            res = null;
        }
        return res;
    }

}
