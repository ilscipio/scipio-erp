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
package org.ofbiz.webapp.taglib;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * ContentUrlTag - Creates a URL string prepending the content prefix from url.properties
 */
public class ContentUrlTag {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: NOTE: Orig Ofbiz method.
     */
    public static void appendContentPrefix(HttpServletRequest request, StringBuilder urlBuffer) {
        try {
            appendContentPrefix(request, (Appendable) urlBuffer);
        } catch (IOException e) {
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
    }
    
    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: Version that supports optional explicit webSiteId.
     */
    public static void appendContentPrefix(HttpServletRequest request, StringBuilder urlBuffer, String webSiteId) {
        try {
            appendContentPrefix(request, (Appendable) urlBuffer, webSiteId);
        } catch (IOException e) {
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
    }
    
    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: Modified to support an explicit webSiteId
     */
    public static void appendContentPrefix(HttpServletRequest request, Appendable urlBuffer, String webSiteId) throws IOException {
        if (request == null) {
            Debug.logWarning("Request was null in appendContentPrefix; this probably means this was used where it shouldn't be, like using ofbizContentUrl in a screen rendered through a service; using best-bet behavior: standard prefix from url.properties (no WebSite or security setting known)", module);
            String prefix = UtilProperties.getPropertyValue("url", "content.url.prefix.standard");
            if (prefix != null) {
                urlBuffer.append(prefix.trim());
            }
            return;
        }
        // SCIPIO: if webSiteId, get that specific one
        //GenericValue webSite = WebSiteWorker.getWebSite(request);
        GenericValue webSite;
        if (webSiteId != null && webSiteId.length() > 0) {
            webSite = WebSiteWorker.findWebSite((Delegator) request.getAttribute("delegator"), webSiteId);
        } else {
            webSite = WebSiteWorker.getWebSite(request);
        }
        // SCIPIO: 2017-11-18: Factored out dispersed secure checks into RequestLinkUtil:
        appendContentPrefix(webSite, RequestLinkUtil.isEffectiveSecure(request), urlBuffer);
    }
    
    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: NOTE: Orig Ofbiz signature.
     */
    public static void appendContentPrefix(HttpServletRequest request, Appendable urlBuffer) throws IOException {
        appendContentPrefix(request, urlBuffer, null);
    }    

    // SCIPIO: Modified to support Boolean
    public static void appendContentPrefix(GenericValue webSite, Boolean secure, Appendable urlBuffer) throws IOException {
        // SCIPIO: WARN: Don't have request, can't determine sane default when secure null, so assume false
        secure = Boolean.TRUE.equals(secure); // default false 
        if (secure) {
            if (webSite != null && UtilValidate.isNotEmpty(webSite.getString("secureContentPrefix"))) {
                urlBuffer.append(webSite.getString("secureContentPrefix").trim());
            } else {
                String prefix = UtilProperties.getPropertyValue("url", "content.url.prefix.secure");
                if (prefix != null) {
                    urlBuffer.append(prefix.trim());
                }
            }
        } else {
            if (webSite != null && UtilValidate.isNotEmpty(webSite.getString("standardContentPrefix"))) {
                urlBuffer.append(webSite.getString("standardContentPrefix").trim());
            } else {
                String prefix = UtilProperties.getPropertyValue("url", "content.url.prefix.standard");
                if (prefix != null) {
                    urlBuffer.append(prefix.trim());
                }
            }
        }
    }

    public static String getContentPrefix(HttpServletRequest request) {
        StringBuilder buf = new StringBuilder();
        ContentUrlTag.appendContentPrefix(request, buf);
        return buf.toString();
    }
}
