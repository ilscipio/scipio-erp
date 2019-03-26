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

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

/**
 * ContentDirective - Freemarker Transform for content links
 * @deprecated SCIPIO: This is replaced by {@link ContentUrlDirective}.
 */
@Deprecated
public class OfbizContentTransform {
    
    /**
     * Returns the contextPathPrefix from the environment or request
     * or null if not found. May be empty string.
     * @throws TemplateModelException
     */
    public static String getContentPathPrefix(boolean nonEscaping, Environment env) throws TemplateModelException {
        return ContentUrlDirective.getContentPathPrefix(nonEscaping, env);
    }

    /**
     * Returns the string passed in ctxPrefix or the contextPathPrefix from the environment or request,
     * bypassing auto screen escaping,
     * or null if not found. May be empty string.
     * @throws TemplateModelException
     */
    public static String getContentPathPrefix(Object ctxPrefixObj, boolean nonEscaping, Environment env) throws TemplateModelException {
        return ContentUrlDirective.getContentPathPrefix(ctxPrefixObj, nonEscaping, env);
    }

}
