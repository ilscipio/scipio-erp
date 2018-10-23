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
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.renderer.RendererInfo;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * ContentUrlTag - Creates a URL string prepending the content prefix from url.properties
 * <p>
 * SCIPIO: TODO: prefix caches, maybe integrate with FullWebappInfo
 */
public class ContentUrlTag {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: static vars
    private static final boolean useSysProp = UtilProperties.getPropertyAsBoolean("url", "content.url.settings.entitySysProp", true);
    private static final String defaultStandardContentPrefix = UtilProperties.getPropertyValue("url", "content.url.prefix.standard");
    private static final String defaultSecureContentPrefix = UtilProperties.getPropertyValue("url", "content.url.prefix.secure");

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: NOTE: Orig Ofbiz method.
     */
    public static void appendContentPrefix(HttpServletRequest request, StringBuilder urlBuffer) {
        try {
            appendContentPrefix(request, (Appendable) urlBuffer, null, null, null);
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
            appendContentPrefix(request, (Appendable) urlBuffer, webSiteId, null, null);
        } catch (IOException e) {
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
    }

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: Version that supports optional explicit webSiteId.
     */
    public static void appendContentPrefix(HttpServletRequest request, StringBuilder urlBuffer, String webSiteId, Boolean secure, String type) {
        try {
            appendContentPrefix(request, (Appendable) urlBuffer, webSiteId, secure, type);
        } catch (IOException e) {
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
    }

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: Version that supports optional explicit webSiteId.
     */
    public static void appendContentPrefix(Map<String, Object> context, StringBuilder urlBuffer, String webSiteId, Boolean secure, String type) {
        try {
            appendContentPrefix(context, (Appendable) urlBuffer, webSiteId, secure, type);
        } catch (IOException e) {
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
    }

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: NOTE: Orig Ofbiz signature.
     */
    public static void appendContentPrefix(HttpServletRequest request, Appendable urlBuffer) throws IOException {
        appendContentPrefix(request, urlBuffer, null, null, null);
    }

    /**
     * SCIPIO: Appends content prefix to buffer, with optional webSiteId.
     */
    public static void appendContentPrefix(HttpServletRequest request, Appendable urlBuffer, String webSiteId) throws IOException {
        appendContentPrefix(request, urlBuffer, webSiteId, null, null);
    }

    /**
     * Appends content prefix to buffer.
     * <p>
     * SCIPIO: Modified to support an explicit webSiteId and secure flag
     */
    public static void appendContentPrefix(HttpServletRequest request, Appendable urlBuffer, String webSiteId, Boolean secure, String type) throws IOException {
        if (request == null) {
            Debug.logWarning("appendContentPrefix: Request is null; this probably means this was used where it"
                    + " shouldn't be; falling back to prefix from url.properties if available (no WebSite, delegator or security setting known)", module);
            if (checkDoLocalContentPrefix(null, urlBuffer, true, type, false)) {
                return;
            }
            appendContentPrefix((GenericValue) null, true, urlBuffer);
            return;
        }
        // SCIPIO: FOP and local content request detection
        boolean isXslFo = isXslFo(RendererInfo.fromRequest(request));
        if (secure == null) {
            if (isXslFo) {
                // SPECIAL: FOP external-graphic cannot reach via HTTPS at current time, and such is very slow anyway
                secure = false;
            } else {
                secure = RequestLinkUtil.isEffectiveSecure(request);
            }
        }
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (checkDoLocalContentPrefix(delegator, urlBuffer, secure, type, isXslFo)) {
            return;
        }
        // SCIPIO: if webSiteId, get that specific one
        //GenericValue webSite = WebSiteWorker.getWebSite(request);
        GenericValue webSite;
        if (UtilValidate.isNotEmpty(webSiteId)) {
            webSite = WebSiteWorker.findWebSite(delegator, webSiteId);
        } else {
            webSiteId = WebSiteWorker.getWebSiteId(request);
            webSite = (webSiteId != null) ? WebSiteWorker.findWebSite(delegator, webSiteId) : null;
        }
        // SCIPIO: 2017-11-18: Factored out dispersed secure checks into RequestLinkUtil:
        appendContentPrefix(webSite, secure, urlBuffer);
    }

    /**
     * SCIPIO: Appends content prefix to buffer, using a render context.
     */
    public static void appendContentPrefix(Map<String, Object> context, Appendable urlBuffer, String webSiteId, Boolean secure, String type) throws IOException {
        // SCIPIO: FOP and local content request detection
        boolean isXslFo = isXslFo(RendererInfo.fromContext(context));
        if (secure == null) {
            if (isXslFo) {
                // SPECIAL: FOP external-graphic cannot reach via HTTPS at current time, and such is very slow anyway
                secure = false;
            } else {
                secure = true;
            }
        }
        Delegator delegator = (Delegator) context.get("delegator");
        if (checkDoLocalContentPrefix(delegator, urlBuffer, secure, type, isXslFo)) {
            return;
        }

        // SCIPIO: if webSiteId, get that specific one
        //GenericValue webSite = WebSiteWorker.getWebSite(request);
        GenericValue webSite;
        if (UtilValidate.isNotEmpty(webSiteId)) {
            webSite = WebSiteWorker.findWebSite(delegator, webSiteId);
        } else {
            webSiteId = WebSiteWorker.getWebSiteIdFromContext(context);
            webSite = (webSiteId != null) ? WebSiteWorker.findWebSite(delegator, webSiteId) : null;
        }
        // SCIPIO: 2017-11-18: Factored out dispersed secure checks into RequestLinkUtil:
        StringBuilder sb = new StringBuilder();
        appendContentPrefix(webSite, secure, sb);
        urlBuffer.append(sb.toString());
        if (sb.length() == 0) {
            Debug.logWarning("appendContentPrefix: We appear to be rendering from a non-webapp"
                    + " render context, but no content prefix is defined in url.properties; cannot"
                    + " create a full link", module);
        }
    }

    private static boolean isXslFo(RendererInfo renderInfo) {
        return (renderInfo != null && "xsl-fo".equals(renderInfo.getRendererName()));
    }

    private static boolean checkDoLocalContentPrefix(Delegator delegator, Appendable urlBuffer, boolean secure, String type, boolean isXslFo) throws IOException {
        if (type == null || type.isEmpty()) {
            type = isXslFo ? "local" : null;
        }
        if (type != null) {
            if (secure) {
                String prefix = (useSysProp && delegator != null) ?
                        EntityUtilProperties.getPropertyValue("url", type+".content.url.prefix.secure", delegator) :
                        UtilProperties.getPropertyValue("url", type+".content.url.prefix.secure");
                if (!prefix.isEmpty()) {
                    if (!"/".equals(prefix)) { // SCIPIO: use empty slash as "no-prefix" identifier
                        urlBuffer.append(prefix);
                    }
                    return true;
                }
            } else {
                String prefix = (useSysProp && delegator != null) ?
                        EntityUtilProperties.getPropertyValue("url", type+".content.url.prefix.standard", delegator) :
                        UtilProperties.getPropertyValue("url", type+".content.url.prefix.standard");
                if (!prefix.isEmpty()) {
                    if (!"/".equals(prefix)) {
                        urlBuffer.append(prefix);
                    }
                    return true;
                }
            }
        }
        return false;
    }

    // SCIPIO: Modified to support Boolean, heavily modified
    public static void appendContentPrefix(GenericValue webSite, Boolean secure, Appendable urlBuffer) throws IOException {
        // SCIPIO: WARN: Don't have request, can't determine sane default when secure null, so assume false
        secure = !Boolean.FALSE.equals(secure); // default TRUE (2018-08)
        if (secure) {
            String prefix;
            if (webSite != null && UtilValidate.isNotEmpty(webSite.getString("secureContentPrefix"))) {
                prefix = webSite.getString("secureContentPrefix").trim();
            } else {
                if (useSysProp && webSite != null) { // SCIPIO: can now check through EntityUtilProperties, if enabled
                    prefix = EntityUtilProperties.getPropertyValue("url", "content.url.prefix.secure", webSite.getDelegator());
                } else {
                    prefix = defaultSecureContentPrefix;
                }
            }
            if (!"/".equals(prefix)) {
                urlBuffer.append(prefix);
            }
        } else {
            String prefix;
            if (webSite != null && UtilValidate.isNotEmpty(webSite.getString("standardContentPrefix"))) {
                prefix = webSite.getString("standardContentPrefix").trim();
            } else {
                if (useSysProp && webSite != null) {
                    prefix = EntityUtilProperties.getPropertyValue("url", "content.url.prefix.standard", webSite.getDelegator());
                } else {
                    prefix = defaultStandardContentPrefix;
                }
            }
            if (!"/".equals(prefix)) {
                urlBuffer.append(prefix);
            }
        }
    }

    public static String getContentPrefix(HttpServletRequest request) {
        StringBuilder buf = new StringBuilder();
        ContentUrlTag.appendContentPrefix(request, buf, null, null, null);
        return buf.toString();
    }
}
