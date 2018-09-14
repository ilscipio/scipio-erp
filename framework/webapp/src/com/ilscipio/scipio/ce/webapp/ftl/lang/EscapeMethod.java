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
package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.List;

import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: EscapeMethods - impl for utilities.ftl escapeVal, escapeFull, etc. functions.
 */
public class EscapeMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 3 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-3)");
        }
        throw new UnsupportedOperationException("TODO - not implemented");
    }

    public static class EscapeValMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeFullMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeFullUrlMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeMsgMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeEventMsgMethod extends EscapeMethod {
        // TODO
    }
}
