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
package org.ofbiz.content.content;

/**
 * ContentWrapper Interface
 */

public interface ContentWrapper {

    /**
     * Gets a content field by ID.
     * <p>
     * SCIPIO: NOTE: This method's parameters' meanings and implementation is modified from stock Ofbiz.
     * Encoding is generally NOT DONE by this method anymore, with a few possible exceptions (such as url parameter encoding).
     * <p>
     * NOTE: 2016-10-14: Currently there is no encoding done by this method at all,
     * with possible URL parameter encoding to be added (TODO? see 
     * {@link ContentLangUtil#getContentWrapperSanitizer(String)}).
     * <p>
     * In most cases, the language can be omitted and {@link #get(String)} can be used instead once core,
     * while the caller can (and often should) handle the languages.
     * <p>
     * In overwhelming majority of cases it is the templates' and screens' responsibility to encode 
     * in the output document language at the proper point of use; usually done automatically by 
     * screen html auto-escaping for html (which can be bypassed for non-html by caller).
     * <p>
     * The contentLang parameter here logically replaces encoderType and it denotes the intrinsic language
     * elements that may be contained in the specified content; but does NOT imply encoding
     * will be done in the named language.
     * <p>
     * In other words, "html" means the value may contain HTML markup, and "url" means
     * the value is a URL. It does not mean the markup will be html-escaped, although this
     * happens automatically by the screen automatic html-escaping when this is called
     * from freemarker or passed from groovy/screens to freemarker (so there are no added
     * security issues from removing the previous HTML encodings).
     * <p>
     * Currently, most code in SCIPIO no longer specifies "html" to this method and
     * uses {@link #get(String)} instead; only a few "url" are left.
     * <p>
     * For URL, individual parameters may get encoded, but unlike stock ofbiz, the URL
     * will not get slaughtered and is still subject to HTML and other encoding by caller/screens.
     * <p>
     * The implementations should make use of {@link ContentLangUtil}.
     * See {@link ContentLangUtil} for automatic encoding behavior.
     * 
     * @see ContentLangUtil#getContentWrapperSanitizer
     * @see #get(String)
     */
    //public StringUtil.StringWrapper get(String contentTypeId, String encoderType); 
    public String get(String contentTypeId, String contentLang); 

    /**
     * SCIPIO: Gets a content field by ID, with content language type general/handled by caller.
     * <p>
     * NEVER performs encoding.
     * 
     * @see #get(String, String)
     */
    public String get(String contentTypeId); 

}
