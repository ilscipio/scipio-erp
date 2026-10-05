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
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */
package org.ofbiz.minilang.operation;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import java.util.regex.PatternSyntaxException;
import java.util.regex.Pattern;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.PatternFactory;
import org.w3c.dom.Element;

/**
 * Validates the current field using a regular expression
 */
public class Regexp extends SimpleMapOperation {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private Pattern pattern = null;
    String expr;

    public Regexp(Element element, SimpleMapProcess simpleMapProcess) {
        super(element, simpleMapProcess);
        expr = element.getAttribute("expr");
        try {
            pattern = PatternFactory.createOrGetPerl5CompiledPattern(expr, true);
        } catch (PatternSyntaxException e) {
            Debug.logError(e, module);
        }
    }

    @Override
    public void exec(Map<String, Object> inMap, Map<String, Object> results, List<Object> messages, Locale locale, ClassLoader loader) {
        Object obj = inMap.get(fieldName);
        String fieldValue = null;
        try {
            fieldValue = (String) ObjectType.simpleTypeConvert(obj, "String", null, locale);
        } catch (GeneralException e) {
            messages.add("Could not convert field value for comparison: " + e.getMessage());
            return;
        }
        if (pattern == null) {
            messages.add("Could not compile regular expression \"" + expr + "\" for validation");
            return;
        }
        if (!pattern.matcher(fieldValue).matches()) {
            addMessage(messages, loader, locale);
        }
    }
}
