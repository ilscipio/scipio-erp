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
package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: GetStyleNamesByPrefixMethod - Freemarker Method for extracting style names by prefix from a style string.
 */
public class GetStyleNamesByPrefixMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() != 2) {
            throw new TemplateModelException("Invalid number of arguments (expected: 2)");
        }
        String styleString = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(0)));
        styleString = TemplateFtlUtil.getPlainClassArgNames(styleString);

        String prefix = LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1)));

        String[] names = StringUtils.split(styleString, ' ');
        // NOTE: For emergency/safety reasons, use the current wrapper, which MAY be escaping.
        // style strings contain only simple characters anyway.
        ObjectWrapper objectWrapper = LangFtlUtil.getCurrentObjectWrapper();
        SimpleSequence res = new SimpleSequence(names.length, objectWrapper);

        for(String name : names) {
            if (name.startsWith(prefix)) {
                res.add(name);
            }
        }

        // redundant
        //return LangFtlUtil.wrap(res, objectWrapper);
        return res;
    }

}
