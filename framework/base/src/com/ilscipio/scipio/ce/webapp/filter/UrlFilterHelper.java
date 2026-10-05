/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.ce.webapp.filter;

import org.ofbiz.base.util.Debug;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.lang.reflect.Method;

/**
 * Classloader-aware proxy for UrlFilterHelperImpl.
 *
 * <p>This proxy class uses the same fully qualified name as the original UrlFilterHelper
 * so that urlrewrite.xml files don't need to be changed. The urlrewrite library is loaded
 * by the base module classloader, which finds this proxy class. The proxy then uses the
 * thread context classloader to delegate to the real UrlFilterHelperImpl in the webapp module.</p>
 *
 * <p>SCIPIO: 4.0.0: Added to fix ClassNotFoundException when urlrewrite library tries to load
 * UrlFilterHelper using its own classloader which doesn't have access to platform JARs.</p>
 */
public class UrlFilterHelper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String URL_FILTER_HELPER_IMPL_CLASS = "com.ilscipio.scipio.ce.webapp.filter.UrlFilterHelperImpl";

    // Keep the same public constants as the original for backwards compatibility
    /**
     * When rendering from static render contexts (emails, etc.),
     * name of a request attribute (in the emulated HttpServletRequest)
     * containing a context map of the static render context.
     */
    public static final String SOURCE_CONTEXT = "scpUrlReSrcCtx";

    /**
     * When doing inter-webapp URL rewriting, name of a request attribute
     * containing the original HttpServletRequest of the source webapp.
     */
    public static final String SOURCE_REQUEST = "scpUrlReSrcReq";

    /**
     * For outbound rules, name of request attribute containing the FullWebappInfo
     * for the webapp to which the urlrewrite.xml file is associated.
     */
    public static final String URLREWRITE_CONF_WEBAPP = "scpUrlReCnfWebapp";

    /**
     * Name of optionally-present request attribute containing the FullWebappInfo
     * for the link currently being processed by outbound-rules.
     */
    public static final String OUT_URL_WEBAPP = "scpUrlOutWebapp";

    private static volatile Object helperInstance;
    private static volatile Class<?> helperClass;
    private static volatile Method setCommonAttrOutMethod;
    private static volatile Method setCommonAttrInMethod;
    private static volatile Method doInterWebappUrlRewriteMethod;

    /**
     * Initializes the helper using thread context classloader.
     */
    private static synchronized void ensureInitialized() {
        if (helperClass == null) {
            try {
                ClassLoader tccl = Thread.currentThread().getContextClassLoader();
                if (tccl == null) {
                    tccl = UrlFilterHelper.class.getClassLoader();
                }
                helperClass = tccl.loadClass(URL_FILTER_HELPER_IMPL_CLASS);
                helperInstance = helperClass.getDeclaredConstructor().newInstance();
                setCommonAttrOutMethod = helperClass.getMethod("setCommonAttrOut", HttpServletRequest.class, HttpServletResponse.class);
                setCommonAttrInMethod = helperClass.getMethod("setCommonAttrIn", HttpServletRequest.class, HttpServletResponse.class);
                doInterWebappUrlRewriteMethod = helperClass.getMethod("doInterWebappUrlRewrite", HttpServletRequest.class, HttpServletResponse.class);
                Debug.logInfo("UrlFilterHelper: Successfully loaded " + URL_FILTER_HELPER_IMPL_CLASS + " using thread context classloader", module);
            } catch (Exception e) {
                Debug.logError(e, "UrlFilterHelper: Failed to load " + URL_FILTER_HELPER_IMPL_CLASS + ": " + e.getMessage(), module);
                throw new RuntimeException("Failed to load UrlFilterHelperImpl: " + e.getMessage(), e);
            }
        }
    }

    /**
     * Delegates to UrlFilterHelperImpl.setCommonAttrOut().
     * Called from urlrewrite.xml outbound-rules.
     */
    public void setCommonAttrOut(HttpServletRequest request, HttpServletResponse response) {
        ensureInitialized();
        try {
            setCommonAttrOutMethod.invoke(helperInstance, request, response);
        } catch (Exception e) {
            Debug.logError(e, "UrlFilterHelper: Error invoking setCommonAttrOut: " + e.getMessage(), module);
        }
    }

    /**
     * Delegates to UrlFilterHelperImpl.setCommonAttrIn().
     * Called from urlrewrite.xml inbound-rules.
     */
    public void setCommonAttrIn(HttpServletRequest request, HttpServletResponse response) {
        ensureInitialized();
        try {
            setCommonAttrInMethod.invoke(helperInstance, request, response);
        } catch (Exception e) {
            Debug.logError(e, "UrlFilterHelper: Error invoking setCommonAttrIn: " + e.getMessage(), module);
        }
    }

    /**
     * Delegates to UrlFilterHelperImpl.doInterWebappUrlRewrite().
     * Called from urlrewrite.xml outbound-rules for inter-webapp URL rewriting.
     */
    public void doInterWebappUrlRewrite(HttpServletRequest request, HttpServletResponse response) {
        ensureInitialized();
        try {
            doInterWebappUrlRewriteMethod.invoke(helperInstance, request, response);
        } catch (Exception e) {
            Debug.logError(e, "UrlFilterHelper: Error invoking doInterWebappUrlRewrite: " + e.getMessage(), module);
        }
    }
}
