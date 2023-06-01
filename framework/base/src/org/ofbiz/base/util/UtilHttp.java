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
package org.ofbiz.base.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.nio.ByteBuffer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

import javax.net.ssl.SSLContext;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;

import com.ilscipio.scipio.base.util.AttrHandler;
import com.ilscipio.scipio.base.util.AttrAccessOp;
import com.ilscipio.scipio.ce.util.servlet.FieldFilter;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContexts;
import org.apache.oro.text.regex.MalformedPatternException;
import org.apache.oro.text.regex.Pattern;
import org.apache.oro.text.regex.PatternMatcher;
import org.apache.oro.text.regex.Perl5Matcher;

import com.ibm.icu.util.Calendar;

/**
 * HttpUtil - Misc HTTP Utility Functions
 *
 * <p>SCIPIO: 3.0.0: Rewrote {@link #getLocale}, {@link #getTimeZone}, {@link #getCurrencyUom} to use {@link AttrHandler}; misc.</p>
 */
public final class UtilHttp {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String MULTI_ROW_DELIMITER = "_o_"; // SCIPIO: 2018-08: keeping public for backward-compat
    public static final String ROW_SUBMIT_PREFIX = "_rowSubmit_o_";
    public static final String COMPOSITE_DELIMITER = "_c_";
    public static final int MULTI_ROW_DELIMITER_LENGTH = MULTI_ROW_DELIMITER.length();
    public static final int ROW_SUBMIT_PREFIX_LENGTH = ROW_SUBMIT_PREFIX.length();
    public static final int COMPOSITE_DELIMITER_LENGTH = COMPOSITE_DELIMITER.length();

    /**
     * SCIPIO: The name of a session attribute that contains an object to synchronize on for
     * whole-session synchronization. Should be accessed using {@link #getSessionSyncObject(HttpServletRequest)}.
     */
    public static final String SESSION_SYNCOBJ = "scpSessSyncObj";
    @Deprecated
    public static final String SESSION_SYNC_OBJECT = SESSION_SYNCOBJ;

    /**
     * SCIPIO: The name of a servlet context attribute that contains an object to synchronize on for
     * whole-session synchronization. Should be accessed using {@link #getServletContextSyncObject(HttpServletRequest)}.
     */
    public static final String SERVLETCONTEXT_SYNCOBJ = "scpApplSyncObj";
    @Deprecated
    public static final String SERVLETCONTEXT_SYNC_OBJECT = SERVLETCONTEXT_SYNCOBJ;

    /**
     * SCIPIO: The name of a servlet context attribute that contains names of session attributes which
     * should not be persisted. Should be accessed using {@link #getServletContextSyncObject(HttpServletRequest)}.
     */
    public static final String SESSION_NOPERSIST_ATTRLIST = "scpSessAttrNoPersist";

    private static final UtilHttp INSTANCE = new UtilHttp(); // SCIPIO: This is for FreeMarkerWorker (only!)

    private UtilHttp () {}

    /**
     * Create a combined map from servlet context, session, attributes and parameters
     */
    public static Map<String, Object> getCombinedMap(HttpServletRequest request) {
        return getCombinedMap(request, null);
    }

    /**
     * Create a combined map from servlet context, session, attributes and parameters
     * -- this method will only use the skip names for session and servlet context attributes.
     */
    public static Map<String, Object> getCombinedMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        return getCombinedMap(request, namesToSkip, null);
    }

    /**
     * Create a combined map from servlet context, session, attributes and parameters
     * -- this method will only use the skip names for session and servlet context attributes.
     * <p>SCIPIO: 2.1.0: This method now excludes parameters defined in controller request-parameter-filter to prevent
     * unintended manipulation of screens when corresponding request attributes are missing.</p>
     * <p>SCIPIO: 2.1.0: This method now supports preventing servlet context map attributes from being included
     * that are named by scpExclApplParams.</p>
     */
    public static Map<String, Object> getCombinedMap(HttpServletRequest request, Set<? extends String> namesToSkip, Boolean readBody) {
        Map<String, Object> combinedMap = new HashMap<>();
        combinedMap.putAll(getParameterMap(request, null, null, readBody, true));                   // parameters override nothing
        Set<? extends String> namesToSkipAppl;
        Set<? extends String> scpExclApplParams = UtilHttp.getContextParamAttrAsNameSet(request.getServletContext(), "scpExclApplParams", true);
        if (UtilValidate.isNotEmpty(scpExclApplParams)) {
            if (UtilValidate.isNotEmpty(namesToSkip)) {
                namesToSkipAppl = new HashSet<String>(namesToSkip);
                namesToSkipAppl.addAll(UtilGenerics.cast(scpExclApplParams));
            } else {
                namesToSkipAppl = scpExclApplParams;
            }
        } else {
            namesToSkipAppl = namesToSkip;
        }
        combinedMap.putAll(getServletContextMap(request, namesToSkipAppl)); // bottom level application attributes
        combinedMap.putAll(getSessionMap(request, namesToSkip));        // session overrides application
        combinedMap.putAll(getAttributeMap(request));                   // attributes trump them all
        return combinedMap;
    }

    /**
     * SCIPIO: Checks the following in the given order and returns the first non-null named attribute or parameter:
     * request attributes -> session attributes -> servlet context attributes -> request parameters.
     * Uses same combining rules as {@link #getCombinedMap(HttpServletRequest)}.
     */
    public static Object getCombinedAttrParam(HttpServletRequest request, String name) {
        Object value = getAllAttr(request, name);
        if (value != null) {
            return value;
        }
        return getRequestParam(request, name);
    }
    
    /**
     * SCIPIO: Create a combined map from request attributes and request parameters and session attributes, with
     * request attributes having priority, followed by request parameters, and finally session attributes.
     */
    public static Map<String, Object> getRequestSessionAttrParamMap(HttpServletRequest request) {
        return getRequestSessionAttrParamMap(request, null);
    }

    /**
     * SCIPIO: Create a combined map from request attributes and request parameters and session attributes, with
     * request attributes having priority, followed by request parameters, and finally session attributes
     * -- this method will only use the skip names for session and servlet context attributes.
     */
    public static Map<String, Object> getRequestSessionAttrParamMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> combinedMap = new HashMap<>();
        combinedMap.putAll(getSessionMap(request, namesToSkip));        // session overrides application
        combinedMap.putAll(getParameterMap(request));                   // parameters override nothing
        combinedMap.putAll(getAttributeMap(request));                   // attributes trump them all
        return combinedMap;
    }

    /**
     * SCIPIO: Checks the following in the given order and returns the first non-null named attribute:
     * request attributes -> session attributes -> servlet context attributes.
     * Uses same combining rules as {@link #getRequestSessionAttrParamMap(HttpServletRequest)}.
     */
    public static Object getRequestSessionAttrParam(HttpServletRequest request, String name) {
        Object value = getRequestAttr(request, name);
        if (value != null) {
            return value;
        }
        value = getRequestParam(request, name);
        if (value != null) {
            return value;
        }
        return getSessionAttr(request, name);
    }

    /**
     * SCIPIO: Create a combined map from request, session and servlet context attributes, in that order of priority.
     * Does NOT contain request parameters.
     */
    public static Map<String, Object> getAllAttributeMap(HttpServletRequest request) {
        return getRequestSessionAttrParamMap(request, null);
    }

    /**
     * SCIPIO: Create a combined map from request, session and servlet context attributes, in that order of priority.
     * -- this method will only use the skip names for session and servlet context attributes.
     *  Does NOT contain request parameters.
     */
    public static Map<String, Object> getAllAttributeMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> combinedMap = new HashMap<>();
        combinedMap.putAll(getServletContextMap(request, namesToSkip)); // bottom level application attributes
        combinedMap.putAll(getSessionMap(request, namesToSkip));        // session overrides application
        combinedMap.putAll(getAttributeMap(request));                   // attributes trump them all
        return combinedMap;
    }

    /**
     * SCIPIO: Checks the following in the given order and returns the first non-null named attribute:
     * request attributes -> session attributes -> servlet context attributes.
     * Uses same combining rules as {@link #getRequestSessionAttrParamMap(HttpServletRequest)}.
     * Does NOT check request parameters.
     */
    public static Object getAllAttr(HttpServletRequest request, String name) {
        Object value = getRequestAttr(request, name);
        if (value != null) {
            return value;
        }
        value = getSessionAttr(request, name);
        if (value != null) {
            return value;
        }
        return getServletContextAttr(request, name);
    }
    
    /**
     * SCIPIO: Create a combined map from request attributes and request parameters only, with attributes having priority.
     * Added 2019-02-05.
     * @return The resulting Map
     */
    public static Map<String, Object> getRequestAttrParamMap(HttpServletRequest request) {
        return getRequestAttrParamMap(request, null);
    }

    /**
     * SCIPIO: Create a combined map from request attributes and request parameters, with attributes having priority.
     * -- this method will only use the skip names for session and servlet context attributes.
     * Added 2019-02-05.
     * @return The resulting Map
     */
    public static Map<String, Object> getRequestAttrParamMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> combinedMap = new HashMap<>();
        combinedMap.putAll(getParameterMap(request));                   // parameters override nothing
        combinedMap.putAll(getAttributeMap(request));                   // attributes trump them all
        return combinedMap;
    }

    /**
     * SCIPIO: Checks the following in the given order and returns the first non-null named attribute or parameter:
     * request attributes -> request parameters.
     * Uses same combining rules as {@link #getRequestAttrParamMap(HttpServletRequest)}.
     */
    public static Object getRequestAttrParam(HttpServletRequest request, String name) {
        Object value = getRequestAttr(request, name);
        if (value != null) {
            return value;
        }
        return getRequestParam(request, name);
    }
    
    /**
     * Create a map from a HttpServletRequest (parameters) object
     * @return The resulting Map
     */
    public static Map<String, Object> getParameterMap(HttpServletRequest request) {
        return getParameterMap(request, null, null);
    }

    public static Map<String, Object> getParameterMap(HttpServletRequest request, Set<? extends String> nameSet) {
        return getParameterMap(request, nameSet, null);
    }


    /**
     * Create a map from a HttpServletRequest (parameters) object
     * @param onlyIncludeOrSkip If true only include, if false skip, the named parameters in the nameSet. If this is null and nameSet is not null, default to skip.
     * @return The resulting Map
     */
    public static Map<String, Object> getParameterMap(HttpServletRequest request, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip) {
        return getParameterMap(request, nameSet, onlyIncludeOrSkip, null);
    }

    /**
     * Create a map from a HttpServletRequest (parameters) object
     * @param onlyIncludeOrSkip If true only include, if false skip, the named parameters in the nameSet. If this is null and nameSet is not null, default to skip.
     * @return The resulting Map
     */
    public static Map<String, Object> getParameterMap(HttpServletRequest request, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip, Boolean readBody) {
        return getParameterMap(request, nameSet, onlyIncludeOrSkip, readBody, false);
    }

    /**
     * Create a map from a HttpServletRequest (parameters) object
     * <p>SCIPIO: 2.1.0: Added overload with paramFilter, designed for controller input-output-filters (see common-controller.xml).</p>
     * @param onlyIncludeOrSkip If true only include, if false skip, the named parameters in the nameSet. If this is null and nameSet is not null, default to skip.
     * @param paramFilter Either Boolean.TRUE to apply webapp filter, Boolean.FALSE to prevent or FieldFilter instance (SCIPIO)
     * @return The resulting Map
     */
    public static Map<String, Object> getParameterMap(HttpServletRequest request, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip, Boolean readBody,
                                                      Object paramFilter) {
        boolean onlyIncludeOrSkipPrim = onlyIncludeOrSkip == null ? true : onlyIncludeOrSkip;
        Map<String, Object> paramMap = new HashMap<>();
        FieldFilter.SectionFilter sectionFilter = getSectionFilter(request, paramFilter); // SCIPIO

        // add all the actual HTTP request parameters
        Enumeration<String> e = UtilGenerics.cast(request.getParameterNames());
        while (e.hasMoreElements()) {
            String name = e.nextElement();
            if (!isIncludeParam(name, nameSet, onlyIncludeOrSkipPrim, sectionFilter)) {
                continue;
            }

            Object value = null;
            String[] paramArr = request.getParameterValues(name);
            if (paramArr != null) {
                if (paramArr.length != 1) { // SCIPIO: For correctness, switched this: (paramArr.length > 1)
                    value = Arrays.asList(paramArr);
                } else {
                    value = paramArr[0];
                    // does the same thing basically, nothing better about it as far as I can see: value = request.getParameter(name);
                }
            }
            paramMap.put(name, value);
        }

        paramMap.putAll(getPathInfoOnlyParameterMap(request, nameSet, onlyIncludeOrSkip, paramFilter));

        // SCIPIO: Always put anything found in the multi-part map in case anything else received along with it, otherwise consistency issues.
        //if (paramMap.size() == 0) {
        {
            // nothing found in the parameters; maybe we read the stream instead
            // SCIPIO: TODO: REVIEW: This is not an option due to file upload implementations and other things reading
            //  the body, so leave it to service events still for now...
            //Map<String, Object> multiPartMap = getMultiPartParameterMap(request);
            Map<String, Object> multiPartMap = UtilGenerics.checkMap(request.getAttribute("multiPartMap"));
            if (UtilValidate.isNotEmpty(multiPartMap)) {
                for(Map.Entry<String, Object> entry : multiPartMap.entrySet()) {
                    if (!isIncludeParam(entry.getKey(), nameSet, onlyIncludeOrSkipPrim, sectionFilter)) {
                        continue;
                    }
                    paramMap.put(entry.getKey(), entry.getValue());
                }
            }
        }

        // SCIPIO: Include JSON body parameters
        Map<String, Object> requestBodyMap = !Boolean.FALSE.equals(readBody) ? getRequestBodyMap(request) : UtilGenerics.cast(request.getAttribute("requestBodyMap"));
        if (UtilValidate.isNotEmpty(requestBodyMap)) {
            for(Map.Entry<String, Object> entry : requestBodyMap.entrySet()) {
                if (!isIncludeParam(entry.getKey(), nameSet, onlyIncludeOrSkipPrim, sectionFilter)) {
                    continue;
                }
                paramMap.put(entry.getKey(), entry.getValue());
            }
        }

        // SCIPIO: Include override request params
        Map<String, Object> overrideRequestParams = UtilGenerics.cast(request.getAttribute("scpReqParamsOvrd"));
        if (UtilValidate.isNotEmpty(overrideRequestParams)) {
            for(Map.Entry<String, Object> entry : overrideRequestParams.entrySet()) {
                if (!isIncludeParam(entry.getKey(), nameSet, onlyIncludeOrSkipPrim, sectionFilter)) {
                    continue;
                }
                paramMap.put(entry.getKey(), entry.getValue());
            }
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Made Request Parameter Map with [" + paramMap.size() + "] Entries", module);
        }

        return canonicalizeParameterMap(paramMap);
    }

    private static boolean isIncludeParam(String name, Set<? extends String> nameSet, boolean onlyIncludeOrSkipPrim, FieldFilter.SectionFilter sectionFilter) {
        // Explicit blacklists have priority
        if (nameSet != null && !onlyIncludeOrSkipPrim && nameSet.contains(name)) {
            return false;
        } else if (sectionFilter != null && sectionFilter.deniesExplicit(name)) {
            return false;
        }

        // Whitelists will do a logical OR because one is logically extending the whitelist of the other
        if (nameSet != null && onlyIncludeOrSkipPrim) { // nameSet whitelist overrides sectionFilter
            // NOTE: don't consult sectionFilter getDefaultAccess because onlyIncludeOrSkipPrim logically overrides it
            return nameSet.contains(name) || (sectionFilter != null && sectionFilter.allowsExplicit(name));
        }
        return sectionFilter == null || sectionFilter.allowsExplicit(name) || "allow".equals(sectionFilter.getDefaultAccess());
    }

    private static FieldFilter.SectionFilter getSectionFilter(HttpServletRequest request, Object paramFilter) { // SCIPIO
        FieldFilter.SectionFilter sectionFilter = null;
        if (paramFilter != null) {
            if (paramFilter instanceof FieldFilter.SectionFilter) {
                sectionFilter = (FieldFilter.SectionFilter) paramFilter;
            } else if (paramFilter instanceof FieldFilter) {
                sectionFilter = ((FieldFilter) paramFilter).getInputFilter();
            } else if (Boolean.TRUE.equals(paramFilter)) {
                FieldFilter fieldFilter = getWebappRequestParamFilter(request);
                if (fieldFilter != null) {
                    sectionFilter = fieldFilter.getInputFilter();
                }
            }
        }
        return sectionFilter;
    }

    private static Method getWebappRequestParamFilterMethod = null;
    private static FieldFilter getWebappRequestParamFilter(HttpServletRequest request) {
        Method method = getWebappRequestParamFilterMethod;
        if (method == null) {
            try {
                Class<?> requestHandlerCls = UtilHttp.class.getClassLoader().loadClass("org.ofbiz.webapp.control.RequestHandler");
                method = requestHandlerCls.getMethod("getWebappRequestParamFilter", HttpServletRequest.class);
            } catch(Exception e) {
                throw new IllegalStateException(e);
            }
            getWebappRequestParamFilterMethod = method;
        }
        try {
            return (FieldFilter) method.invoke(null, request);
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * SCIPIO: Returns the named request parameter, using the same canonicalization rules as {@link #getParameterMap(HttpServletRequest)}.
     * <p>NOTE: This method is low level and does not consult the same body maps and exceptions and getParameterMap;
     * meant for implementations rather than screen use.</p>
     */
    public static Object getRequestParam(HttpServletRequest request, String name) {
        Object value = getPathInfoOnlyParam(request, name);
        if (value == null) {
            String[] paramArr = request.getParameterValues(name);
            if (paramArr != null) {
                if (paramArr.length != 1) { // SCIPIO: For correctness, switched this: (paramArr.length > 1)
                    value = Arrays.asList(paramArr);
                } else {
                    value = paramArr[0];
                    // does the same thing basically, nothing better about it as far as I can see: value = request.getParameter(name);
                }
            }
        }
        return canonicalizeParameter(value);
    }
    
    public static Map<String, Object> getQueryStringOnlyParameterMap(String queryString) {
        Map<String, Object> paramMap = new HashMap<>();
        if (UtilValidate.isNotEmpty(queryString)) {
            StringTokenizer queryTokens = new StringTokenizer(queryString, "&");
            while (queryTokens.hasMoreTokens()) {
                String token = queryTokens.nextToken();
                if (token.startsWith("amp;")) {
                    // this is most likely a split value that had an &amp; in it, so don't consider this a name; note that some old code just stripped the "amp;" and went with it
                    continue;
                }
                int equalsIndex = token.indexOf("=");
                if (equalsIndex > 0) {
                    String name = token.substring(0, equalsIndex);
                    paramMap.put(name, token.substring(equalsIndex + 1));
                }
            }
        }
        return canonicalizeParameterMap(paramMap);
    }

    private static ScipioMethod getMultiPartParameterMapMethod = ScipioMethod.from("org.ofbiz.webapp.util.UtilHttpWeb",
            "getMultiPartParameterMap", HttpServletRequest.class);
    /**
     * Reads to gets the last parsed multiPartMap.
     * NOTE: This method swallows upload errors, so use {@link #readMultiPartParameterMap(HttpServletRequest)} for those locations
     * that can handle them, otherwise reported errors may be incorrect.
     */
    public static Map<String, Object> getMultiPartParameterMap(HttpServletRequest request) { // SCIPIO: modified
        return getMultiPartParameterMapMethod.invokeRuntime(null, request);
    }

    private static ScipioMethod readMultiPartParameterMapMethod = ScipioMethod.from("org.ofbiz.webapp.util.UtilHttpWeb",
            "readMultiPartParameterMap", HttpServletRequest.class);
    public static Map<String, Object> readMultiPartParameterMap(HttpServletRequest request) throws IOException { // SCIPIO: modified
        return readMultiPartParameterMapMethod.invokeRuntime(null, request);
    }

    private static ScipioMethod getRequestBodyMapMethod = ScipioMethod.from("org.ofbiz.webapp.event.RequestBodyMapHandlerFactory",
            "getRequestBodyMap", ServletRequest.class);
    /**
     * Returns the request body map, checking request attribute requestBodyMap to see if already parsed (SCIPIO).
     */
    public static Map<String, Object> getRequestBodyMap(ServletRequest request) {
        return getRequestBodyMapMethod.invokeRuntime(null, request);
    }

    private static ScipioMethod extractMapFromRequestBodyMethod = ScipioMethod.from("org.ofbiz.webapp.event.RequestBodyMapHandlerFactory",
            "extractMapFromRequestBody", ServletRequest.class);
    /**
     * Returns the request body map without going through any request attributes, with a new read (SCIPIO).
     */
    public static Map<String, Object> extractMapFromRequestBody(ServletRequest request) throws IOException {
        try {
            return extractMapFromRequestBodyMethod.invoke(null, request);
        } catch (IOException e) {
            throw e;
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * SCIPIO: Returns the named request parameter, using the same rules as {@link #getQueryStringOnlyParameterMap(String)}.
     * WARN: At current time (2019-02), this implementation is slow; if you check for than one param, use the Map method.
     */
    public static Object getQueryStringOnlyParam(String queryString, String name) {
        return getQueryStringOnlyParameterMap(queryString).get(name); // TODO: Optimize
    }

    public static Map<String, Object> getPathInfoOnlyParameterMap(HttpServletRequest request, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip) {
        return getPathInfoOnlyParameterMap(request.getPathInfo(), nameSet, onlyIncludeOrSkip, false);
    }

    public static Map<String, Object> getPathInfoOnlyParameterMap(HttpServletRequest request, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip, Object paramFilter) {
        if (Boolean.TRUE.equals(paramFilter)) {
            paramFilter = getWebappRequestParamFilter(request);
            if (paramFilter != null) {
                paramFilter = ((FieldFilter) paramFilter).getInputFilter();
            }
        }
        return getPathInfoOnlyParameterMap(request.getPathInfo(), nameSet, onlyIncludeOrSkip, paramFilter);
    }

    public static Map<String, Object> getPathInfoOnlyParameterMap(String pathInfoStr, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip) {
        return getPathInfoOnlyParameterMap(pathInfoStr, nameSet, onlyIncludeOrSkip, false);
    }

    public static Map<String, Object> getPathInfoOnlyParameterMap(String pathInfoStr, Set<? extends String> nameSet, Boolean onlyIncludeOrSkip, Object paramFilter) {
        boolean onlyIncludeOrSkipPrim = onlyIncludeOrSkip == null ? true : onlyIncludeOrSkip;
        Map<String, Object> paramMap = new HashMap<>();
        FieldFilter.SectionFilter sectionFilter = (paramFilter instanceof FieldFilter.SectionFilter) ? (FieldFilter.SectionFilter) paramFilter : null;

        // now add in all path info parameters /~name1=value1/~name2=value2/
        // note that if a parameter with a given name already exists it will be put into a list with all values

        if (UtilValidate.isNotEmpty(pathInfoStr)) {
            // make sure string ends with a trailing '/' so we get all values
            if (!pathInfoStr.endsWith("/")) {
                pathInfoStr += "/";
            }

            int current = pathInfoStr.indexOf('/');
            int last = current;
            while ((current = pathInfoStr.indexOf('/', last + 1)) != -1) {
                String element = pathInfoStr.substring(last + 1, current);
                last = current;
                if (element.charAt(0) == '~' && element.indexOf('=') > -1) {
                    String name = element.substring(1, element.indexOf('='));
                    if (!isIncludeParam(name, nameSet, onlyIncludeOrSkipPrim, sectionFilter)) {
                        continue;
                    }

                    String value = element.substring(element.indexOf('=') + 1);
                    Object curValue = paramMap.get(name);
                    if (curValue != null) {
                        List<String> paramList = null;
                        if (curValue instanceof List<?>) {
                            paramList = UtilGenerics.checkList(curValue);
                            paramList.add(value);
                        } else {
                            String paramString = (String) curValue;
                            paramList = new LinkedList<>();
                            paramList.add(paramString);
                            paramList.add(value);
                        }
                        paramMap.put(name, paramList);
                    } else {
                        paramMap.put(name, value);
                    }
                }
            }
        }

        return canonicalizeParameterMap(paramMap);
    }

    /**
     * SCIPIO: Returns the named request parameter, using the same rules as {@link #getPathInfoOnlyParameterMap}.
     * WARN: At current time (2019-02), this implementation is slow; if you check for than one param, use the Map method.
     */
    public static Object getPathInfoOnlyParam(HttpServletRequest request, String name) {
        return getPathInfoOnlyParameterMap(request.getPathInfo(), null, null).get(name); // TODO: Optimize
    }

    public static Map<String, Object> getUrlOnlyParameterMap(HttpServletRequest request) {
        // NOTE: these have already been through canonicalizeParameterMap, so not doing it again here
        Map<String, Object> paramMap = getQueryStringOnlyParameterMap(request.getQueryString());
        paramMap.putAll(getPathInfoOnlyParameterMap(request.getPathInfo(), null, null));
        return paramMap;
    }

    /**
     * SCIPIO: Returns the named request parameter, using the same rules as {@link #getUrlOnlyParameterMap(HttpServletRequest)}.
     * WARN: At current time (2019-02), this implementation is slow; if you check for than one param, use the Map method.
     */
    public static Object getUrlOnlyParam(HttpServletRequest request, String name) {
        return getUrlOnlyParameterMap(request).get(name); // TODO: Optimize
    }

    public static Map<String, Object> canonicalizeParameterMap(Map<String, Object> paramMap) {
        for (Map.Entry<String, Object> paramEntry: paramMap.entrySet()) {
            if (paramEntry.getValue() instanceof String) {
                paramEntry.setValue(canonicalizeParameter((String) paramEntry.getValue()));
            } else if (paramEntry.getValue() instanceof Collection<?>) {
                Collection<String> collection = UtilGenerics.<String>checkCollection(paramEntry.getValue());
                List<String> newList = new ArrayList<>(collection.size()); // SCIPIO: Switched to ArrayList
                for (String listEntry : collection) {
                    newList.add(canonicalizeParameter(listEntry));
                }
                paramEntry.setValue(newList);
            }
        }
        return paramMap;
    }

    public static Object canonicalizeParameter(Object paramValue) { // SCIPIO
        if (paramValue instanceof String) {
            paramValue = canonicalizeParameter((String) paramValue);
        } else if (paramValue instanceof Collection<?>) {
            List<String> newList = new ArrayList<>();
            for (String listEntry: UtilGenerics.<String>checkCollection(paramValue)) {
                newList.add(canonicalizeParameter(listEntry));
            }
            paramValue = newList;
        }
        return paramValue;
    }

    public static String canonicalizeParameter(String paramValue) {
        try {
            /** calling canonicalize with strict flag set to false so we only get warnings about double encoding, etc; can be set to true for exceptions and more security */
            String cannedStr = UtilCodec.canonicalize(paramValue, false);
            if (Debug.verboseOn()) {
                Debug.logVerbose("Canonicalized parameter with " + (cannedStr.equals(paramValue) ? "no " : "") + "change: original [" + paramValue + "] canned [" + cannedStr + "]", module);
            }
            return cannedStr;
        } catch (Exception e) {
            Debug.logError(e, "Error in canonicalize parameter value [" + paramValue + "]: " + e.toString(), module);
            return paramValue;
        }
    }

    /**
     * Create a map from a HttpRequest (attributes) object used in JSON requests
     * @return The resulting Map
     */
    public static Map<String, Object> getJSONAttributeMap(HttpServletRequest request) {
        // SCIPIO: delegating
        return transformJSONAttributeMap(getAttributeMap(request));
    }

    /**
     * Create a map from a HttpRequest (attributes) object used in JSON requests, with attribute filter (SCIPIO).
     * @return The resulting Map
     */
    public static Map<String, Object> getJSONAttributeMap(HttpServletRequest request, AttributeFilter attributeFilter) {
        // SCIPIO: delegating
        return transformJSONAttributeMap(getAttributeMap(request), attributeFilter);
    }

    /**
     * Create a map from a HttpRequest (attributes) object used in JSON requests, with attribute filter (SCIPIO).
     * @return The resulting Map
     */
    public static Map<String, Object> getJSONAttributeMap(Map<String, Object> returnMap, HttpServletRequest request, AttributeFilter attributeFilter) {
        return transformJSONAttributeMap(returnMap, getAttributeMap(request), attributeFilter);
    }

    /**
     * Filters JSON attribute map; filter returns true to force include, false to exclude, null to auto-determine based on type.
     * <p>SCIPIO: 2017-05-01: Factored out from getJSONAttributeMap.</p>
     */
    public static Map<String, Object> transformJSONAttributeMap(Map<String, Object> returnMap, Map<String, Object> attrMap, AttributeFilter attributeFilter) {
        if (attributeFilter == null) {
            attributeFilter = DefaultAttributeFilter.INSTANCE;
        }
        for (Map.Entry<String, Object> entry : attrMap.entrySet()) {
            String key = entry.getKey();
            Object val = entry.getValue();
            Boolean include = attributeFilter.includeAttribute(key, val);
            if (Boolean.FALSE.equals(include)) {
                continue;
            }
            if (val instanceof java.sql.Timestamp) {
                val = val.toString();
            }
            if (Boolean.TRUE.equals(include) || val instanceof String || val instanceof Number || val instanceof Map<?, ?> || val instanceof List<?> || val instanceof Boolean) {
                if (Debug.verboseOn()) {
                    Debug.logVerbose("Adding attribute to JSON output: " + key, module);
                }
                returnMap.put(key, val);
            }
        }
        return returnMap;
    }

    /**
     * SCIPIO: factored out from getJSONAttributeMap.
     * Added 2017-05-01.
     */
    public static Map<String, Object> transformJSONAttributeMap(Map<String, Object> attrMap, AttributeFilter attributeFilter) {
        return transformJSONAttributeMap(new HashMap<>(), attrMap, attributeFilter);
    }

    /**
     * SCIPIO: factored out from getJSONAttributeMap.
     * Added 2017-05-01.
     */
    public static Map<String, Object> transformJSONAttributeMap(Map<String, Object> attrMap) {
        return transformJSONAttributeMap(attrMap, null);
    }

    public interface AttributeFilter { // SCIPIO
        /** Return null for default behavior (apply type filters), false to prevent include, true to force include. */
        Boolean includeAttribute(String key, Object value);
    }

    public static class DefaultAttributeFilter implements AttributeFilter { // SCIPIO
        public static final DefaultAttributeFilter INSTANCE = new DefaultAttributeFilter();
        @Override
        public Boolean includeAttribute(String key, Object value) { return null; }
    }

    /**
     * Create a map from a HttpRequest (attributes) object
     * @return The resulting Map
     */
    public static Map<String, Object> getAttributeMap(HttpServletRequest request) {
        return getAttributeMap(request, null);
    }

    /**
     * Create a map from a HttpRequest (attributes) object
     * @return The resulting Map
     */
    public static Map<String, Object> getAttributeMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> attributeMap = new HashMap<>();

        // look at all request attributes
        Enumeration<String> requestAttrNames = UtilGenerics.cast(request.getAttributeNames());
        while (requestAttrNames.hasMoreElements()) {
            String attrName = requestAttrNames.nextElement();
            if (namesToSkip != null && namesToSkip.contains(attrName)) {
                continue;
            }

            Object attrValue = request.getAttribute(attrName);
            attributeMap.put(attrName, attrValue);
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Made Request Attribute Map with [" + attributeMap.size() + "] Entries", module);
            Debug.logVerbose("Request Attribute Map Entries: " + System.getProperty("line.separator") + UtilMisc.printMap(attributeMap), module);
        }

        return attributeMap;
    }

    /**
     * SCIPIO: Returns the named request attribute.
     */
    public static Object getRequestAttr(HttpServletRequest request, String name) {
        return request.getAttribute(name);
    }

    /**
     * Create a map from a HttpSession object
     * @return The resulting Map
     */
    public static Map<String, Object> getSessionMap(HttpServletRequest request) {
        return getSessionMap(request, null);
    }

    /**
     * Create a map from a HttpSession object
     * @return The resulting Map
     */
    public static Map<String, Object> getSessionMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> sessionMap = new HashMap<>();
        HttpSession session = request.getSession();

        // look at all the session attributes
        Enumeration<String> sessionAttrNames = UtilGenerics.cast(session.getAttributeNames());
        while (sessionAttrNames.hasMoreElements()) {
            String attrName = sessionAttrNames.nextElement();
            if (namesToSkip != null && namesToSkip.contains(attrName)) {
                continue;
            }

            Object attrValue = session.getAttribute(attrName);
            sessionMap.put(attrName, attrValue);
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Made Session Attribute Map with [" + sessionMap.size() + "] Entries", module);
            Debug.logVerbose("Session Attribute Map Entries: " + System.getProperty("line.separator") + UtilMisc.printMap(sessionMap), module);
        }

        return sessionMap;
    }

    /**
     * SCIPIO: Returns the named session attribute, IF a session exists. Does not force session creation.
     */
    public static <T> T getSessionAttr(HttpServletRequest request, String name) {
        HttpSession session = request.getSession(false); // Do not create.
        return (session != null) ? UtilGenerics.cast(session.getAttribute(name)) : null;
    }

    /**
     * Create a map from a ServletContext object
     * @return The resulting Map
     */
    public static Map<String, Object> getServletContextMap(HttpServletRequest request) {
        return getServletContextMap(request, null);
    }

    /**
     * Create a map from a ServletContext object
     * @return The resulting Map
     */
    public static Map<String, Object> getServletContextMap(HttpServletRequest request, Set<? extends String> namesToSkip) {
        Map<String, Object> servletCtxMap = new HashMap<>();

        // look at all servlet context attributes
        ServletContext servletContext = request.getServletContext(); // SCIPIO: get context using servlet API 3.0
        Enumeration<String> applicationAttrNames = UtilGenerics.cast(servletContext.getAttributeNames());
        while (applicationAttrNames.hasMoreElements()) {
            String attrName = applicationAttrNames.nextElement();
            if (namesToSkip != null && namesToSkip.contains(attrName)) {
                continue;
            }

            Object attrValue = servletContext.getAttribute(attrName);
            servletCtxMap.put(attrName, attrValue);
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Made ServletContext Attribute Map with [" + servletCtxMap.size() + "] Entries", module);
            Debug.logVerbose("ServletContext Attribute Map Entries: " + System.getProperty("line.separator") + UtilMisc.printMap(servletCtxMap), module);
        }

        return servletCtxMap;
    }

    /**
     * SCIPIO: Returns the named servlet context attribute.
     */
    public static <T> T getServletContextAttr(HttpServletRequest request, String name) {
        return UtilGenerics.cast(request.getServletContext().getAttribute(name));
    }

    public static Map<String, Object> makeParamMapWithPrefix(HttpServletRequest request, String prefix, String suffix) {
        return makeParamMapWithPrefix(request, null, prefix, suffix);
    }

    public static Map<String, Object> makeParamMapWithPrefix(HttpServletRequest request, Map<String, ? extends Object> additionalFields, String prefix, String suffix) {
        return makeParamMapWithPrefix(getCombinedMap(request), additionalFields, prefix, suffix);
    }

    public static Map<String, Object> makeParamMapWithPrefix(Map<String, ? extends Object> context, String prefix, String suffix) {
        return makeParamMapWithPrefix(context, null, prefix, suffix);
    }

    public static Map<String, Object> makeParamMapWithPrefix(Map<String, ? extends Object> context, Map<String, ? extends Object> additionalFields, String prefix, String suffix) {
        Map<String, Object> paramMap = new HashMap<>();
        for (Map.Entry<String, ? extends Object> entry: context.entrySet()) {
            String parameterName = entry.getKey();
            if (parameterName.startsWith(prefix)) {
                if (UtilValidate.isNotEmpty(suffix)) {
                    if (parameterName.endsWith(suffix)) {
                        String key = parameterName.substring(prefix.length(), parameterName.length() - (suffix.length()));
                        if (entry.getValue() instanceof ByteBuffer) {
                            ByteBuffer value = (ByteBuffer) entry.getValue();
                            paramMap.put(key, value);
                        } else {
                            String value = (String) entry.getValue();
                            paramMap.put(key, value);
                        }
                    }
                } else {
                    String key = parameterName.substring(prefix.length());
                    if (context.get(parameterName) instanceof ByteBuffer) {
                        ByteBuffer value = (ByteBuffer) entry.getValue();
                        paramMap.put(key, value);
                    } else {
                        String value = (String) entry.getValue();
                        paramMap.put(key, value);
                    }
                }
            }
        }
        if (additionalFields != null) {
            for (Map.Entry<String, ? extends Object> entry: additionalFields.entrySet()) {
                String fieldName = entry.getKey();
                if (fieldName.startsWith(prefix)) {
                    if (UtilValidate.isNotEmpty(suffix)) {
                        if (fieldName.endsWith(suffix)) {
                            String key = fieldName.substring(prefix.length(), fieldName.length() - (suffix.length() - 1));
                            Object value = entry.getValue();
                            paramMap.put(key, value);

                            // check for image upload data
                            if (!(value instanceof String)) {
                                String nameKey = "_" + key + "_fileName";
                                Object nameVal = additionalFields.get("_" + fieldName + "_fileName");
                                if (nameVal != null) {
                                    paramMap.put(nameKey, nameVal);
                                }

                                String typeKey = "_" + key + "_contentType";
                                Object typeVal = additionalFields.get("_" + fieldName + "_contentType");
                                if (typeVal != null) {
                                    paramMap.put(typeKey, typeVal);
                                }

                                String sizeKey = "_" + key + "_size";
                                Object sizeVal = additionalFields.get("_" + fieldName + "_size");
                                if (sizeVal != null) {
                                    paramMap.put(sizeKey, sizeVal);
                                }
                            }
                        }
                    } else {
                        String key = fieldName.substring(prefix.length());
                        Object value = entry.getValue();
                        paramMap.put(key, value);

                        // check for image upload data
                        if (!(value instanceof String)) {
                            String nameKey = "_" + key + "_fileName";
                            Object nameVal = additionalFields.get("_" + fieldName + "_fileName");
                            if (nameVal != null) {
                                paramMap.put(nameKey, nameVal);
                            }

                            String typeKey = "_" + key + "_contentType";
                            Object typeVal = additionalFields.get("_" + fieldName + "_contentType");
                            if (typeVal != null) {
                                paramMap.put(typeKey, typeVal);
                            }

                            String sizeKey = "_" + key + "_size";
                            Object sizeVal = additionalFields.get("_" + fieldName + "_size");
                            if (sizeVal != null) {
                                paramMap.put(sizeKey, sizeVal);
                            }
                        }
                    }
                }
            }
        }
        return paramMap;
    }

    public static List<Object> makeParamListWithSuffix(HttpServletRequest request, String suffix, String prefix) {
        return makeParamListWithSuffix(request, null, suffix, prefix);
    }

    // SCIPIO: TODO: this should use getCombinedMap as above by default and corresponding overload
    public static List<Object> makeParamListWithSuffix(HttpServletRequest request, Map<String, ? extends Object> additionalFields, String suffix, String prefix) {
        /* SCIPIO: use combined map for consistency with makeParamMapWithPrefix
        List<Object> paramList = new ArrayList<>();
        Enumeration<String> parameterNames = UtilGenerics.cast(request.getParameterNames());
        while (parameterNames.hasMoreElements()) {
            String parameterName = parameterNames.nextElement();
            if (parameterName.endsWith(suffix)) {
                if (UtilValidate.isNotEmpty(prefix)) {
                    if (parameterName.startsWith(prefix)) {
                        String value = request.getParameter(parameterName);
                        paramList.add(value);
                    }
                } else {
                    String value = request.getParameter(parameterName);
                    paramList.add(value);
                }
            }
        }
        if (additionalFields != null) {
            for (Map.Entry<String, ? extends Object> entry: additionalFields.entrySet()) {
                String fieldName = entry.getKey();
                if (fieldName.endsWith(suffix)) {
                    if (UtilValidate.isNotEmpty(prefix)) {
                        if (fieldName.startsWith(prefix)) {
                            paramList.add(entry.getValue());
                        }
                    } else {
                        paramList.add(entry.getValue());
                    }
                }
            }
        }
        return paramList;
        */
        return makeParamListWithSuffix(getCombinedMap(request), additionalFields, suffix, prefix);
    }

    public static List<Object> makeParamListWithSuffix(Map<String, ? extends Object> context, String suffix, String prefix) {
        return makeParamListWithSuffix(context, null, suffix, prefix);
    }

    // SCIPIO: version that accepts context map instead of HttpServletRequest
    public static List<Object> makeParamListWithSuffix(Map<String, ? extends Object> context, Map<String, ? extends Object> additionalFields, String suffix, String prefix) {
        List<Object> paramList = new ArrayList<>();
        for (Map.Entry<String, ? extends Object> entry: context.entrySet()) {
            String parameterName = entry.getKey();
            if (parameterName.endsWith(suffix)) {
                if (UtilValidate.isNotEmpty(prefix)) {
                    if (parameterName.startsWith(prefix)) {
                        paramList.add(entry.getValue());
                    }
                } else {
                    paramList.add(entry.getValue());
                }
            }
        }
        if (additionalFields != null) {
            for (Map.Entry<String, ? extends Object> entry: additionalFields.entrySet()) {
                String fieldName = entry.getKey();
                if (fieldName.endsWith(suffix)) {
                    if (UtilValidate.isNotEmpty(prefix)) {
                        if (fieldName.startsWith(prefix)) {
                            paramList.add(entry.getValue());
                        }
                    } else {
                        paramList.add(entry.getValue());
                    }
                }
            }
        }
        return paramList;
    }

    /**
     * Given a request, returns the application name or "root" if deployed on root
     * @param request An HttpServletRequest to get the name info from
     * @return String
     */
    public static String getApplicationName(HttpServletRequest request) {
        String appName = "root";
        if (request.getContextPath().length() > 1) {
            appName = request.getContextPath().substring(1);
        }
        // SCIPIO: Prevent slashes in "application name", only bad things can come of them (see also LoginWorker)
        //return appName
        return appName.replaceAll("/", "_");
    }

    public static void setInitialRequestInfo(HttpServletRequest request) {
        HttpSession session = request.getSession();
        if (UtilValidate.isNotEmpty(session.getAttribute("_WEBAPP_NAME_"))) {
            // oops, info already in place...
            return;
        }

        String fullRequestUrl = getFullRequestUrl(request);

        session.setAttribute("_WEBAPP_NAME_", getApplicationName(request));
        session.setAttribute("_CLIENT_LOCALE_", request.getLocale());
        session.setAttribute("_CLIENT_REQUEST_", fullRequestUrl);
        session.setAttribute("_CLIENT_USER_AGENT_", request.getHeader("User-Agent") != null ? request.getHeader("User-Agent") : "");
        session.setAttribute("_CLIENT_REFERER_", request.getHeader("Referer") != null ? request.getHeader("Referer") : "");

        session.setAttribute("_CLIENT_FORWARDED_FOR_", request.getHeader("X-Forwarded-For"));
        session.setAttribute("_CLIENT_REMOTE_ADDR_", request.getRemoteAddr());
        session.setAttribute("_CLIENT_REMOTE_HOST_", request.getRemoteHost());
        session.setAttribute("_CLIENT_REMOTE_USER_", request.getRemoteUser());
    }

    /**
     * Put request parameters in request object as attributes.
     * @param request
     */
    public static void parametersToAttributes(HttpServletRequest request) {
        java.util.Enumeration<String> e = UtilGenerics.cast(request.getParameterNames());
        while (e.hasMoreElements()) {
            String name = e.nextElement();
            request.setAttribute(name, request.getParameter(name));
        }
    }


    private static StringBuilder prepareServerRootUrl(HttpServletRequest request) {
        StringBuilder requestUrl = new StringBuilder();
        requestUrl.append(request.getScheme());
        requestUrl.append("://" + request.getServerName());
        if (request.getServerPort() != 80 && request.getServerPort() != 443) {
            requestUrl.append(":" + request.getServerPort());
        }
        return requestUrl;
    }

    public static String getServerRootUrl(HttpServletRequest request) {
        return prepareServerRootUrl(request).toString();
    }

    public static String getFullRequestUrl(HttpServletRequest request) {
        StringBuilder requestUrl = prepareServerRootUrl(request);
        requestUrl.append(request.getRequestURI());
        if (request.getQueryString() != null) {
            requestUrl.append("?" + request.getQueryString());
        }
        return requestUrl.toString();
    }

    /**
     * <p>Returns (guarantees) locale object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>NOTE: This method may use accept headers from browser (or not, as configured).</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static Locale getLocale(HttpServletRequest request) {
        return AttrHandler.from(request).getSessionLocale(request, null);
    }

    /**
     * <p>Returns (guarantees) locale object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>NOTE: This method may use accept headers from browser (or not, as configured).</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static Locale getLocale(HttpServletRequest request, AttrAccessOp accessOp) {
        return AttrHandler.from(request).getSessionLocale(request, accessOp);
    }

    /**
     * <p>Returns (guarantees) locale object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getLocale(HttpServletRequest)} instead.</p>
     *
     * <p>NOTE: Unless already cached in session, this method may not use accept headers from browser.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static Locale getLocale(HttpSession session) {
        return AttrHandler.from(session).getSessionLocale(session, null);
    }

    /**
     * <p>Returns (guarantees) locale object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getLocale(HttpServletRequest)} instead.</p>
     *
     * <p>NOTE: Unless already cached in session, this method may not use accept headers from browser.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static Locale getLocale(HttpSession session, AttrAccessOp accessOp) {
        return AttrHandler.from(session).getSessionLocale(session, accessOp);
    }

    /**
     * @deprecated SCIPIO: 3.0.0: appDefaultLocale must be coded into the applications using {@link AttrHandler}
     */
    @Deprecated
    public static Locale getLocale(HttpServletRequest request, HttpSession session, Object appDefaultLocale) {
        Debug.logWarning("DEPRECATED: getLocale called with appDefaultLocale; unused", module);
        return (request != null) ? getLocale(request) : getLocale(session);
    }

    /**
     * Returns the servlet request content accepted locales, minus the default locale server via heuristic if possible.
     *
     * <p>NOTE: This is needed because the servlet API returns Locale.getDefault() as part of the results
     * and it typically has to be filtered out to use the default product store locale instead, which this method
     * attempts to do via heuristic.</p>
     *
     * <p>If excludeDefault false, it doesn't guarantee that it is included (see {@link ServletRequest#getLocales()}).</p>
     *
     * <p>FIXME: Because we sometimes need to include a different default than the server default locale (Locale.getDefault()),
     *     and instead use the product store defaultLocaleString as default, the servlet API makes it impossible for the
     *     client to request the single server default locale as a request, and in that case will be ignored; however,
     *     in most cases the browser will send en_US,en as two locales so the default case will usually be avoided.
     *     This is a servlet API limitation.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static List<Locale> getClientRequestLocales(HttpServletRequest request, boolean excludeDefault) {
        List<Locale> locales = Collections.list(request.getLocales());
        return (!excludeDefault || locales.size() != 1 || !Objects.equals(locales.get(0), Locale.getDefault())) ?
                locales : Collections.emptyList();
    }

    /**
     * Get the Locale object from a session variable; if not found use the browser's default;
     * if the session does not exist, does NOT create it (unlike {@link #getLocale(HttpServletRequest)}).
     *
     * <p>SCIPIO: 2018-07-30: Added.</p>
     *
     * @param request HttpServletRequest object to use for lookup
     * @return Locale The current Locale to use
     */
    public static Locale getLocaleExistingSession(HttpServletRequest request) {
        return getLocale(request, AttrAccessOp.GET);
    }

    /**
     * Sets session locale.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setLocale(HttpServletRequest request, Locale locale) {
        setLocale(request.getSession(), locale);
    }

    /**
     * Sets session locale.
     */
    public static void setLocale(HttpServletRequest request, String localeString) {
        setLocale(request.getSession(), localeString);
    }

    /**
     * Sets session locale.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setLocale(HttpSession session, Locale locale) {
        if (session != null) {
            session.setAttribute("locale", locale);
        }
    }

    /**
     * Sets session locale.
     */
    public static void setLocale(HttpSession session, String localeString) {
        if (session != null) {
            setLocale(session, UtilMisc.parseLocale(localeString));
        }
    }

    /**
     * Sets session locale, only if none set.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setLocaleIfNone(HttpServletRequest request, Locale locale) {
        setLocaleIfNone(request.getSession(), locale);
    }

    /**
     * Sets session locale, only if none set.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setLocaleIfNone(HttpServletRequest request, String localeString) {
        setLocaleIfNone(request.getSession(), localeString);
    }

    /**
     * Sets session locale, only if none set.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setLocaleIfNone(HttpSession session, Locale locale) {
        if (locale != null && session != null && session.getAttribute("locale") == null) {
            setLocale(session, locale);
        }
    }

    /**
     * Sets session locale, only if none set.
     *
     * <p>SCIPIO: 1.x.x: Added.</p>
     */
    public static void setLocaleIfNone(HttpSession session, String localeString) {
        if (UtilValidate.isNotEmpty(localeString) && session != null && session.getAttribute("locale") == null) {
            setLocale(session, UtilMisc.parseLocale(localeString));
        }
    }

    public static void setTimeZone(HttpServletRequest request, TimeZone timeZone) {
        setTimeZone(request.getSession(), timeZone);
    }

    public static void setTimeZone(HttpServletRequest request, String tzId) {
        setTimeZone(request.getSession(), tzId);
    }

    public static void setTimeZone(HttpSession session, TimeZone timeZone) {
        if (session != null) {
            session.setAttribute("timeZone", timeZone);
        }
    }

    public static void setTimeZone(HttpSession session, String tzId) {
        if (session != null) {
            session.setAttribute("timeZone", UtilDateTime.toTimeZone(tzId));
        }
    }

    public static void setTimeZoneIfNone(HttpServletRequest request, TimeZone timeZone) {
        setTimeZoneIfNone(request.getSession(), timeZone);
    }

    public static void setTimeZoneIfNone(HttpServletRequest request, String timeZoneString) {
        setTimeZoneIfNone(request.getSession(), timeZoneString);
    }

    public static void setTimeZoneIfNone(HttpSession session, TimeZone timeZone) {
        if (timeZone != null && session != null && session.getAttribute("timeZone") == null) {
            UtilHttp.setTimeZone(session, timeZone);
        }
    }

    public static void setTimeZoneIfNone(HttpSession session, String timeZoneString) {
        if (UtilValidate.isNotEmpty(timeZoneString) && session != null && session.getAttribute("timeZone") == null) {
            UtilHttp.setTimeZone(session, UtilDateTime.toTimeZone(timeZoneString));
        }
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static TimeZone getTimeZone(HttpServletRequest request) {
        return AttrHandler.from(request).getSessionTimeZone(request, null);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static TimeZone getTimeZone(HttpServletRequest request, AttrAccessOp accessOp) {
        return AttrHandler.from(request).getSessionTimeZone(request, accessOp);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getTimeZone(HttpServletRequest)} instead.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static TimeZone getTimeZone(HttpSession session) {
        return AttrHandler.from(session).getSessionTimeZone(session, null);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getTimeZone(HttpServletRequest)} instead.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static TimeZone getTimeZone(HttpSession session, AttrAccessOp accessOp) {
        return AttrHandler.from(session).getSessionTimeZone(session, accessOp);
    }

    /**
     * @deprecated SCIPIO: 3.0.0: appDefaultLocale must be coded into the applications using {@link AttrHandler}
     */
    @Deprecated
    public static TimeZone getTimeZone(HttpServletRequest request, HttpSession session, String appDefaultTimeZoneString) {
        return (request != null) ? getTimeZone(request) : getTimeZone(session);
    }

    /**
     * SCIPIO: Returns the current user login from "userLogin" request attribute or, if not set,
     * the "userLogin" session attribute; <strong>may</strong> also update the request attribute
     * from the session attribute (not guaranteed).
     * <p>
     * NOTE: This method does not really belong in a base package utility like UtilHttp because
     * it violates build dependencies, but because getLocale and getTimeZone are on this class,
     * this is simply where everyone expects them to be.
     * <p>
     * NOTE: This method can also be found in {@link org.ofbiz.webapp.WebAppUtil#getUserLogin(HttpServletRequest)}.
     * Added 2019-02-12.
     */
    @SuppressWarnings("unchecked")
    public static <T extends Map<String, Object>> T getUserLogin(HttpServletRequest request) {
        T userLogin = (T) request.getAttribute("userLogin");
        /* TODO: REVIEW: This will likely be enabled in the future!
        if (userLogin == null) {
            userLogin = getUserLogin(request.getSession(false));
            if (userLogin != null) {
                request.setAttribute("userLogin", userLogin);
            }
        }
        return userLogin;
        */
        return (userLogin != null) ? userLogin : getUserLogin(request.getSession(false));
    }

    /**
     * SCIPIO: Returns the current user login from "userLogin" session attribute or null otherwise
     * or if the session is null.
     * <p>
     * NOTE: This method does not really belong in a base package utility like UtilHttp because
     * it violates build dependencies, but because getLocale and getTimeZone are on this class,
     * this is simply where everyone expects them to be.
     * <p>
     * NOTE: This method can also be found in {@link org.ofbiz.webapp.WebAppUtil#getUserLogin(HttpSession)}.
     * Added 2019-02-12.
     */
    @SuppressWarnings("unchecked")
    public static <T extends Map<String, Object>> T getUserLogin(HttpSession session) {
        return getSessionUserLogin(session);
    }

    /**
     * SCIPIO: Returns the current user login from "userLogin" session attribute or null otherwise
     * or if the session is null.
     * <p>
     * NOTE: This method does not really belong in a base package utility like UtilHttp because
     * it violates build dependencies, but because getLocale and getTimeZone are on this class,
     * this is simply where everyone expects them to be.
     * <p>
     * NOTE: This method can also be found in {@link org.ofbiz.webapp.WebAppUtil#getUserLogin(HttpSession)}.
     * Added 2019-02-12.
     */
    @SuppressWarnings("unchecked")
    public static <T extends Map<String, Object>> T getSessionUserLogin(HttpServletRequest request) {
        return getSessionUserLogin(request.getSession(false));
    }

    /**
     * SCIPIO: Returns the current user login from "userLogin" session attribute or null otherwise
     * or if the session is null.
     * <p>
     * NOTE: This method does not really belong in a base package utility like UtilHttp because
     * it violates build dependencies, but because getLocale and getTimeZone are on this class,
     * this is simply where everyone expects them to be.
     * <p>
     * NOTE: This method can also be found in {@link org.ofbiz.webapp.WebAppUtil#getUserLogin(HttpSession)}.
     * Added 2019-02-12.
     */
    @SuppressWarnings("unchecked")
    public static <T extends Map<String, Object>> T getSessionUserLogin(HttpSession session) {
        return (session != null) ? (T) session.getAttribute("userLogin") : null;
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static String getCurrencyUom(HttpServletRequest request) {
        return AttrHandler.from(request).getSessionCurrencyUom(request, null);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static String getCurrencyUom(HttpServletRequest request, AttrAccessOp accessOp) {
        return AttrHandler.from(request).getSessionCurrencyUom(request, accessOp);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getCurrencyUom(HttpServletRequest)} instead.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static String getCurrencyUom(HttpSession session) {
        return AttrHandler.from(session).getSessionCurrencyUom(session, null);
    }

    /**
     * <p>Returns (guarantees) time zone object from session (cache) or resolution via {@link AttrHandler}.
     *
     * <p>WARN: When request is available, always use {@link #getCurrencyUom(HttpServletRequest)} instead.</p>
     *
     * <p>SCIPIO: 3.0.0: Rewritten for {@link AttrHandler}.</p>
     */
    public static String getCurrencyUom(HttpSession session, AttrAccessOp accessOp) {
        return AttrHandler.from(session).getSessionCurrencyUom(session, accessOp);
    }

    /**
     * @deprecated SCIPIO: 3.0.0: appDefaultLocale must be coded into the applications using {@link AttrHandler}
     */
    @Deprecated
    public static String getCurrencyUom(HttpSession session, String appDefaultCurrencyUom) {
        return getCurrencyUom(session);
    }

    /**
     * Set the user's per-session currency uom value.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setCurrencyUom(HttpServletRequest request, String currencyUom) {
        setCurrencyUom(request.getSession(), currencyUom);
    }

    /**
     * Set the user's per-session currency uom value.
     */
    public static void setCurrencyUom(HttpSession session, String currencyUom) {
        if (session != null) {
            session.setAttribute("currencyUom", currencyUom);
        }
    }

    /**
     * Set the user's per-session currency uom value if not already set.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static void setCurrencyUomIfNone(HttpServletRequest request, String currencyUom) {
        setCurrencyUomIfNone(request.getSession(), currencyUom);
    }

    /**
     * Set the user's per-session currency uom value if not already set.
     *
     * <p>SCIPIO: 1.x.x: Added.</p>
     */
    public static void setCurrencyUomIfNone(HttpSession session, String currencyUom) {
        if (UtilValidate.isNotEmpty(currencyUom) && session != null && session.getAttribute("currencyUom") == null) {
            session.setAttribute("currencyUom", currencyUom);
        }
    }

    /** URL Encodes a Map of arguements */
    public static String urlEncodeArgs(Map<String, ? extends Object> args) {
        return urlEncodeArgs(args, true);
    }

    /** URL Encodes a Map of arguements */
    public static String urlEncodeArgs(Map<String, ? extends Object> args, boolean useExpandedEntites) {
        StringBuilder buf = new StringBuilder();
        if (args != null) {
            for (Map.Entry<String, ? extends Object> entry: args.entrySet()) {
                String name = entry.getKey();
                Object value = entry.getValue();
                String valueStr = null;
                if (name == null || value == null) {
                    continue;
                }

                Collection<?> col;
                if (value instanceof String) {
                    col = Arrays.asList(value);
                } else if (value instanceof Collection) {
                    col = UtilGenerics.cast(value);
                } else if (value.getClass().isArray()) {
                    col = Arrays.asList((Object[]) value);
                } else {
                    col = Arrays.asList(value);
                }
                for (Object colValue: col) {
                    if (colValue instanceof String) {
                        valueStr = (String) colValue;
                    } else if (colValue == null) {
                        continue;
                    } else {
                        valueStr = colValue.toString();
                    }

                    if (UtilValidate.isNotEmpty(valueStr)) {
                        if (buf.length() > 0) {
                            if (useExpandedEntites) {
                                buf.append("&amp;");
                            } else {
                                buf.append("&");
                            }
                        }
                        buf.append(UtilCodec.getEncoder("url").encode(name));
                        buf.append('=');
                        buf.append(UtilCodec.getEncoder("url").encode(valueStr));
                    }
                }
            }
        }
        return buf.toString();
    }

    public static String getRequestUriFromTarget(String target) {
        if (UtilValidate.isEmpty(target)) {
            return null;
        }
        int endOfRequestUri = target.length();
        if (target.indexOf('?') > 0) {
            endOfRequestUri = target.indexOf('?');
        }
        int slashBeforeRequestUri = target.lastIndexOf('/', endOfRequestUri);
        String requestUri = null;
        if (slashBeforeRequestUri < 0) {
            requestUri = target.substring(0, endOfRequestUri);
        } else {
            requestUri = target.substring(slashBeforeRequestUri, endOfRequestUri);
        }
        return requestUri;
    }

    /** Returns the query string contained in a request target - basically everything
     * after and including the ? character.
     * @param target The request target
     * @return The query string
     */
    public static String getQueryStringFromTarget(String target) {
        if (UtilValidate.isEmpty(target)) {
            return "";
        }
        int queryStart = target.indexOf('?');
        if (queryStart != -1) {
            return target.substring(queryStart);
        }
        return "";
    }

    /** Removes the query string from a request target - basically everything
     * after and including the ? character.
     * @param target The request target
     * @return The request target string
     */
    public static String removeQueryStringFromTarget(String target) {
        if (UtilValidate.isEmpty(target)) {
            return null;
        }
        int queryStart = target.indexOf('?');
        if (queryStart < 0) {
            return target;
        }
        return target.substring(0, queryStart);
    }

    public static String getWebappMountPointFromTarget(String target) {
        int firstChar = 0;
        if (UtilValidate.isEmpty(target)) {
            return null;
        }
        if (target.charAt(0) == '/') {
            firstChar = 1;
        }
        int pathSep = target.indexOf('/', 1);
        String webappMountPoint = null;
        if (pathSep > 0) {
            // if not then no good, supposed to be a inter-app, but there is no path sep! will do general search with null and treat like an intra-app
            webappMountPoint = target.substring(firstChar, pathSep);
        }
        return webappMountPoint;
    }

    public static String encodeAmpersands(String htmlString) {
        StringBuilder htmlBuffer = new StringBuilder(htmlString);
        int ampLoc = -1;
        while ((ampLoc = htmlBuffer.indexOf("&", ampLoc + 1)) != -1) {
            //NOTE: this should work fine, but if it doesn't could try making sure all characters between & and ; are letters, that would qualify as an entity

            // found ampersand, is it already and entity? if not change it to &amp;
            int semiLoc = htmlBuffer.indexOf(";", ampLoc);
            if (semiLoc != -1) {
                // found a semi colon, if it has another & or an = before it, don't count it as an entity, otherwise it may be an entity, so skip it
                int eqLoc = htmlBuffer.indexOf("=", ampLoc);
                int amp2Loc = htmlBuffer.indexOf("&", ampLoc + 1);
                if ((eqLoc == -1 || eqLoc > semiLoc) && (amp2Loc == -1 || amp2Loc > semiLoc)) {
                    continue;
                }
            }

            // at this point not an entity, no substitute with a &amp;
            htmlBuffer.insert(ampLoc + 1, "amp;");
        }
        return htmlBuffer.toString();
    }

    public static String encodeBlanks(String htmlString) {
        return htmlString.replaceAll(" ", "%20");
    }

    public static String setResponseBrowserProxyNoCache(HttpServletRequest request, HttpServletResponse response) {
        setResponseBrowserProxyNoCache(response);
        return "success";
    }

    public static void setResponseBrowserProxyNoCache(HttpServletResponse response) {
        long nowMillis = System.currentTimeMillis();
        response.setDateHeader("Expires", nowMillis);
        response.setDateHeader("Last-Modified", nowMillis); // always modified
        response.setHeader("Cache-Control", "no-store, no-cache, must-revalidate, private"); // HTTP/1.1
        response.setHeader("Pragma", "no-cache"); // HTTP/1.0
    }

    public static void setResponseBrowserProxyNoCacheRedirect(HttpServletResponse response) { // SCIPIO
        setResponseBrowserProxyNoCache(response);
    }

    public static String getContentTypeByFileName(String fileName) {
        FileNameMap mime = URLConnection.getFileNameMap();
        return mime.getContentTypeFor(fileName);
    }

    /**
     * Stream an array of bytes to the browser
     * This method will close the ServletOutputStream when finished
     *
     * @param response HttpServletResponse object to get OutputStream from
     * @param bytes Byte array of content to stream
     * @param contentType The content type to pass to the browser
     * @param fileName the fileName to tell the browser we are downloading
     * @throws IOException
     */
    public static void streamContentToBrowser(HttpServletResponse response, byte[] bytes, String contentType, String fileName) throws IOException {
        // tell the browser not the cache
        setResponseBrowserProxyNoCache(response);

        // set the response info
        response.setContentLength(bytes.length);
        if (contentType != null) {
            response.setContentType(contentType);
        }
        if (fileName != null) {
            setContentDisposition(response, fileName);
        }

        // create the streams

        // stream the content
        try (OutputStream out = response.getOutputStream();
                InputStream in = new ByteArrayInputStream(bytes)) {
            streamContent(out, in, bytes.length);
            out.flush();
        } catch (IOException e) {
            throw e;
        }
    }

    public static void streamContentToBrowser(HttpServletResponse response, byte[] bytes, String contentType) throws IOException {
        streamContentToBrowser(response, bytes, contentType, null);
    }

    /**
     * Streams content from InputStream to the ServletOutputStream
     * This method will close the ServletOutputStream when finished
     * This method does not close the InputSteam passed
     *
     * @param response HttpServletResponse object to get OutputStream from
     * @param in InputStream of the actual content
     * @param length Size (in bytes) of the content
     * @param contentType The content type to pass to the browser
     * @throws IOException
     */
    public static void streamContentToBrowser(HttpServletResponse response, InputStream in, int length, String contentType, String fileName) throws IOException {
        // tell the browser not the cache
        setResponseBrowserProxyNoCache(response);

        // set the response info
        response.setContentLength(length);
        if (contentType != null) {
            response.setContentType(contentType);
        }
        if (fileName != null) {
            setContentDisposition(response, fileName);
        }

        // stream the content
        try (OutputStream out = response.getOutputStream()) {
            streamContent(out, in, length);
            out.flush();
        } catch (IOException e) {
            throw e;
        }
    }

    public static void streamContentToBrowser(HttpServletResponse response, InputStream in, int length, String contentType) throws IOException {
        streamContentToBrowser(response, in, length, contentType, null);
    }

    /**
     * Stream binary content from InputStream to OutputStream
     * This method does not close the streams passed
     *
     * @param out OutputStream content should go to
     * @param in InputStream of the actual content
     * @param length Size (in bytes) of the content
     * @throws IOException
     */
    public static void streamContent(OutputStream out, InputStream in, int length) throws IOException {
        // make sure we have something to write to
        if (out == null) {
            throw new IOException("Attempt to write to null output stream");
        }

        // make sure we have something to read from
        if (in == null) {
            throw new IOException("Attempt to read from null input stream");
        }

        // make sure we have some content
        if (length == 0) {
            throw new IOException("Attempt to write 0 bytes of content to output stream");
        }

        // initialize the buffered streams
        // SCIPIO: 2018-08-30: not available from base at current time (FIXME?)
        //int bufferSize = EntityUtilProperties.getPropertyAsInteger("content", "stream.buffersize", 8192);
        int bufferSize = UtilProperties.getPropertyAsInteger("content", "stream.buffersize", 8192);
        byte[] buffer = new byte[bufferSize];
        int read = 0;
        try (BufferedOutputStream bos = new BufferedOutputStream(out, bufferSize);
                BufferedInputStream bis = new BufferedInputStream(in, bufferSize)) {
            while ((read = bis.read(buffer, 0, buffer.length)) != -1) {
                bos.write(buffer, 0, read);
            }
        } catch (IOException e) {
            Debug.logError(e, "Problem reading/writing buffers", module);
            throw e;
        }
    }

    public static String stripViewParamsFromQueryString(String queryString) {
        return stripViewParamsFromQueryString(queryString, null);
    }

    public static String stripViewParamsFromQueryString(String queryString, String paginatorNumber) {
        Set<String> paramNames = new HashSet<>();
        if (UtilValidate.isNotEmpty(paginatorNumber)) {
            paginatorNumber = "_" + paginatorNumber;
        }
        paramNames.add("VIEW_INDEX" + paginatorNumber);
        paramNames.add("VIEW_SIZE" + paginatorNumber);
        paramNames.add("viewIndex" + paginatorNumber);
        paramNames.add("viewSize" + paginatorNumber);
        return stripNamedParamsFromQueryString(queryString, paramNames);
    }

    public static String stripNamedParamsFromQueryString(String queryString, Collection<String> paramNames) {
        String retStr = null;
        if (UtilValidate.isNotEmpty(queryString)) {
            StringTokenizer queryTokens = new StringTokenizer(queryString, "&");
            StringBuilder cleanQuery = new StringBuilder();
            while (queryTokens.hasMoreTokens()) {
                String token = queryTokens.nextToken();
                if (token.startsWith("amp;")) {
                    token = token.substring(4);
                }
                int equalsIndex = token.indexOf("=");
                String name = token;
                if (equalsIndex > 0) {
                    name = token.substring(0, equalsIndex);
                }
                if (!paramNames.contains(name)) {
                    if (cleanQuery.length() > 0) {
                        cleanQuery.append("&");
                    }
                    cleanQuery.append(token);
                }
            }
            retStr = cleanQuery.toString();
        }
        return retStr;
    }

    /**
     * Given multi form data with the ${param}_o_N notation, creates a Collection
     * of Maps for the submitted rows. Each Map contains the key/value pairs
     * of a particular row. The keys will be stripped of the _o_N suffix.
     * There is an additionaly key "row" for each Map that holds the
     * index of the row.
     */
    public static Collection<Map<String, Object>> parseMultiFormData(Map<String, Object> parameters) {
        Map<Integer, Map<String, Object>> rows = new HashMap<>(); // stores the rows keyed by row number

        // first loop through all the keys and create a hashmap for each ${ROW_SUBMIT_PREFIX}${N} = Y
        for (Map.Entry<String, Object> entry : parameters.entrySet()) {
            String key = entry.getKey();
            // skip everything that is not ${ROW_SUBMIT_PREFIX}N
            if (key == null || key.length() <= ROW_SUBMIT_PREFIX_LENGTH) {
                continue;
            }
            if (key.indexOf(MULTI_ROW_DELIMITER) <= 0) {
                continue;
            }
            if (!key.substring(0, ROW_SUBMIT_PREFIX_LENGTH).equals(ROW_SUBMIT_PREFIX)) {
                continue;
            }
            if (!"Y".equals(entry.getValue())) {
                continue;
            }

            // decode the value of N and create a new map for it
            Integer n = Integer.decode(key.substring(ROW_SUBMIT_PREFIX_LENGTH, key.length()));
            Map<String, Object> m = new HashMap<>();
            m.put("row", n); // special "row" = N tuple
            rows.put(n, m); // key it to N
        }

        // next put all parameters with matching N in the right map
        for (Map.Entry<String, Object> entry : parameters.entrySet()) {
            String key = entry.getKey();
            // skip keys without DELIMITER and skip ROW_SUBMIT_PREFIX
            if (key == null) {
                continue;
            }
            int index = key.indexOf(MULTI_ROW_DELIMITER);
            if (index <= 0) {
                continue;
            }
            if (key.length() > ROW_SUBMIT_PREFIX_LENGTH && key.substring(0, ROW_SUBMIT_PREFIX_LENGTH).equals(ROW_SUBMIT_PREFIX)) {
                continue;
            }

            // get the map with index N
            Integer n = Integer.decode(key.substring(index + MULTI_ROW_DELIMITER_LENGTH, key.length())); // N from ${param}${DELIMITER}${N}
            Map<String, Object> map = rows.get(n);
            if (map == null) {
                continue;
            }

            // get the key without the <DELIMITER>N suffix and store it and its value
            String newKey = key.substring(0, index);
            map.put(newKey, entry.getValue());
        }
        // return only the values, which is the list of maps
        return rows.values();
    }

    /**
     * Returns a new map containing all the parameters from the input map except for the
     * multi form parameters (usually named according to the ${param}_o_N notation).
     */
    public static <V> Map<String, V> removeMultiFormParameters(Map<String, V> parameters) {
        Map<String, V> filteredParameters = new HashMap<>();
        for (Map.Entry<String, V> entry : parameters.entrySet()) {
            String key = entry.getKey();
            if (key != null && (key.indexOf(MULTI_ROW_DELIMITER) != -1 || key.indexOf("_useRowSubmit") != -1 || key.indexOf("_rowCount") != -1)) {
                continue;
            }

            filteredParameters.put(key, entry.getValue());
        }
        return filteredParameters;
    }

    /**
     * Utility to make a composite parameter from the given prefix and suffix.
     * The prefix should be a regular parameter name such as meetingDate. The
     * suffix is the composite field, such as the hour of the meeting. The
     * result would be meetingDate_${COMPOSITE_DELIMITER}_hour.
     *
     * @param prefix
     * @param suffix
     * @return the composite parameter
     */
    public static String makeCompositeParam(String prefix, String suffix) {
        return prefix + COMPOSITE_DELIMITER + suffix;
    }

    /**
     * Given the prefix of a composite parameter, recomposes a single Object from
     * the composite according to compositeType. For example, consider the following
     * form widget field,
     *
     * <pre>
     * {@code
     * <field name="meetingDate">
     *     <date-time type="timestamp" input-method="time-dropdown">
     * </field>
     * }
     * </pre>
     *
     * The result in HTML is three input boxes to input the date, hour and minutes separately.
     * The parameter names are named meetingDate_c_date, meetingDate_c_hour, meetingDate_c_minutes.
     * Additionally, there will be a field named meetingDate_c_compositeType with a value of "Timestamp".
     * where _c_ is the COMPOSITE_DELIMITER. These parameters will then be recomposed into a Timestamp
     * object from the composite fields.
     *
     * @param request
     * @param prefix
     * @return Composite object from data or null if not supported or a parsing error occurred.
     */
    public static Object makeParamValueFromComposite(HttpServletRequest request, String prefix, Locale locale) {
        String compositeType = request.getParameter(makeCompositeParam(prefix, "compositeType"));
        if (UtilValidate.isEmpty(compositeType)) {
            return null;
        }

        // collect the composite fields into a map
        Map<String, String> data = new HashMap<>();
        for (Enumeration<String> names = UtilGenerics.cast(request.getParameterNames()); names.hasMoreElements();) {
            String name = names.nextElement();
            if (!name.startsWith(prefix + COMPOSITE_DELIMITER)) {
                continue;
            }

            // extract the suffix of the composite name
            String suffix = name.substring(name.indexOf(COMPOSITE_DELIMITER) + COMPOSITE_DELIMITER_LENGTH);

            // and the value of this parameter
            String value = request.getParameter(name);

            // key = suffix, value = parameter data
            data.put(suffix, value);
        }
        if (Debug.verboseOn()) { Debug.logVerbose("Creating composite type with parameter data: " + data.toString(), module); }

        // handle recomposition of data into the compositeType
        if ("Timestamp".equals(compositeType)) {
            String date = data.get("date");
            String hour = data.get("hour");
            String minutes = data.get("minutes");
            String ampm = data.get("ampm");
            if (date == null || date.length() < 10) {
                return null;
            }
            if (UtilValidate.isEmpty(hour)) {
                return null;
            }
            if (UtilValidate.isEmpty(minutes)) {
                return null;
            }
            boolean isTwelveHour = UtilValidate.isNotEmpty(ampm);

            // create the timestamp from the data
            try {
                int h = Integer.parseInt(hour);
                Timestamp timestamp = Timestamp.valueOf(date.substring(0, 10) + " 00:00:00.000");
                Calendar cal = Calendar.getInstance(locale);
                cal.setTime(timestamp);
                if (isTwelveHour) {
                    boolean isAM = ("AM".equals(ampm) ? true : false);
                    if (isAM && h == 12) {
                        h = 0;
                    }
                    if (!isAM && h < 12) {
                        h += 12;
                    }
                }
                cal.set(Calendar.HOUR_OF_DAY, h);
                cal.set(Calendar.MINUTE, Integer.parseInt(minutes));
                return new Timestamp(cal.getTimeInMillis());
            } catch (IllegalArgumentException e) {
                Debug.logWarning("User input for composite timestamp was invalid: " + e.getMessage(), module);
                return null;
            }
        }

        // we don't support any other compositeTypes (yet)
        return null;
    }

    /**
     * Obtains the session ID from the request, or "[none]" if no session present.
     * <p>
     * SCIPIO: 2018-12-11: This no longer forces session creation and now returns "[none]" instead of "unknown",
     * which was inaccurate. Note that this was primarily used for log display, and previously this was miscoded
     * such that it could never actually return "unknown"; therefore, if any code was checking the string "unknown",
     * it never did anything anyway.
     */
    public static String getSessionId(HttpServletRequest request) {
        HttpSession session = request.getSession(false); // SCIPIO: use false!
        return (session == null ? "[none]" : session.getId());
    }

    /**
     * SCIPIO: Obtains the session ID from the request, or "[none]" if no session present.
     * @deprecated 2018-12-11: This is now handled by {@link #getSessionId(HttpServletRequest)}.
     */
    @Deprecated
    public static String getSessionIdIfSet(HttpServletRequest request) {
        return getSessionId(request);
    }

    /**
     * checks, if the current request comes from a searchbot
     *
     * @param request
     * @return whether the request is from a web searchbot
     */
    public static boolean checkURLforSpiders(HttpServletRequest request) {
        boolean result = false;

        String spiderRequest = (String) request.getAttribute("_REQUEST_FROM_SPIDER_");
        if (UtilValidate.isNotEmpty(spiderRequest)) {
            if ("Y".equals(spiderRequest)) {
                return true;
            }
            return false;
        }
        String initialUserAgent = request.getHeader("User-Agent") != null ? request.getHeader("User-Agent") : "";
        List<String> spiderList = StringUtil.split(UtilProperties.getPropertyValue("url", "link.remove_lsessionid.user_agent_list"), ",");

        if (UtilValidate.isNotEmpty(spiderList)) {
            for (String spiderNameElement : spiderList) {
                Pattern pattern = null;
                try {
                    pattern = PatternFactory.createOrGetPerl5CompiledPattern(spiderNameElement, false);
                } catch (MalformedPatternException e) {
                    Debug.logError(e, module);
                }
                PatternMatcher matcher = new Perl5Matcher();
                if (matcher.contains(initialUserAgent, pattern)) {
                    request.setAttribute("_REQUEST_FROM_SPIDER_", "Y");
                    result = true;
                    break;
                }
            }
        }

        if (!result) {
            request.setAttribute("_REQUEST_FROM_SPIDER_", "N");
        }

        return result;
    }

    /** Returns true if the user has JavaScript enabled.
     * @param request
     * @return whether javascript is enabled
     */
    public static boolean isJavaScriptEnabled(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            Boolean javaScriptEnabled = (Boolean) session.getAttribute("javaScriptEnabled");
            if (javaScriptEnabled != null) {
                return javaScriptEnabled;
            }
        }
        // SCIPIO: 2018-06-07: javascript should be assumed enabled by default in Scipio, unless detected as off
        //return false;
        return true;
    }

    /** Returns the number or rows submitted by a multi form.
     */
    public static int getMultiFormRowCount(HttpServletRequest request) {
        return getMultiFormRowCount(getParameterMap(request));
    }
    /** Returns the number or rows submitted by a multi form.
     */
    public static int getMultiFormRowCount(Map<String, ?> requestMap) {
        // The number of multi form rows is computed selecting the maximum index
        int rowCount = 0;
        String maxRowIndex = "";
        int rowDelimiterLength = MULTI_ROW_DELIMITER.length();
        for (String parameterName: requestMap.keySet()) {
            int rowDelimiterIndex = (parameterName != null? parameterName.indexOf(MULTI_ROW_DELIMITER): -1);
            if (rowDelimiterIndex > 0) {
                String thisRowIndex = parameterName.substring(rowDelimiterIndex + rowDelimiterLength);
                if (thisRowIndex.indexOf("_") > -1) {
                    thisRowIndex = thisRowIndex.substring(0, thisRowIndex.indexOf("_"));
                }
                if (maxRowIndex.length() < thisRowIndex.length()) {
                    maxRowIndex = thisRowIndex;
                } else if (maxRowIndex.length() == thisRowIndex.length() && maxRowIndex.compareTo(thisRowIndex) < 0) {
                    maxRowIndex = thisRowIndex;
                }
            }
        }
        if (UtilValidate.isNotEmpty(maxRowIndex)) {
            try {
                rowCount = Integer.parseInt(maxRowIndex);
                rowCount++; // row indexes are zero based
            } catch (NumberFormatException e) {
                Debug.logWarning("Invalid value for row index found: " + maxRowIndex, module);
            }
        }
        return rowCount;
    }

    public static String stashParameterMap(HttpServletRequest request) {
        HttpSession session = request.getSession();
        Map<String, Map<String, Object>> paramMapStore = UtilGenerics.checkMap(session.getAttribute("_PARAM_MAP_STORE_"));
        if (paramMapStore == null) {
            // SCIPIO: Synchronize the creation of this map to prevent lost entries, and
            // make sure it's thread-safe
            //paramMapStore = new HashMap<>();
            //session.setAttribute("_PARAM_MAP_STORE_", paramMapStore);
            synchronized (getSessionSyncObject(request)) {
                paramMapStore = UtilGenerics.checkMap(session.getAttribute("_PARAM_MAP_STORE_"));
                if (paramMapStore == null) {
                    paramMapStore = new ConcurrentHashMap<>();
                    session.setAttribute("_PARAM_MAP_STORE_", paramMapStore);
                }
            }
        }
        Map<String, Object> parameters = getParameterMap(request);
        // SCIPIO: Just in case of conflict, loop this
        //String paramMapId = RandomStringUtils.randomAlphanumeric(10);
        //paramMapStore.put(paramMapId, parameters);
        while(true) {
            String paramMapId = RandomStringUtils.randomAlphanumeric(10);
            if (paramMapStore.putIfAbsent(paramMapId, parameters) == null) {
                return paramMapId;
            }
        }
    }

    public static Map<String, Object> getStashedParameterMap(HttpServletRequest request, String paramMapId) { // SCIPIO: New method
        HttpSession session = request.getSession(false);
        if (session == null) {
            return null;
        }
        Map<String, Map<String, Object>> paramMapStore = UtilGenerics.checkMap(session.getAttribute("_PARAM_MAP_STORE_"));
        if (paramMapStore != null) {
            return paramMapStore.get(paramMapId);
        }
        return null;
    }
    
    public static Map<String, Object> removeStashedParameterMap(HttpServletRequest request, String paramMapId) { // SCIPIO: New method
        HttpSession session = request.getSession(false);
        if (session == null) {
            return null;
        }
        Map<String, Map<String, Object>> paramMapStore = UtilGenerics.checkMap(session.getAttribute("_PARAM_MAP_STORE_"));
        if (paramMapStore != null) {
            return paramMapStore.remove(paramMapId);
        }
        return null;
    }

    public static void restoreStashedParameterMap(HttpServletRequest request, Map<String, ?> paramMap) { // SCIPIO: New method
        // SCIPIO: Refactored from the original restoreStashedParameterMap
        if (paramMap != null) {
            //paramMapStore.remove(paramMapId);
            for (Map.Entry<String, ?> paramEntry : paramMap.entrySet()) {
                if (request.getAttribute(paramEntry.getKey()) != null) {
                    Debug.logWarning("Skipped loading parameter [" + paramEntry.getKey() + "] because it would have overwritten a request attribute" , module);
                    continue;
                }
                request.setAttribute(paramEntry.getKey(), paramEntry.getValue());
            }
        }
    }

    public static void restoreStashedParameterMap(HttpServletRequest request, String paramMapId) { // SCIPIO: NOTE: Original stock method, refactored + delegated
        restoreStashedParameterMap(request, removeStashedParameterMap(request, paramMapId));
    }

    public static void restoreStashedParameterMapNoRemove(HttpServletRequest request, String paramMapId) { // SCIPIO: New method
        restoreStashedParameterMap(request, getStashedParameterMap(request, paramMapId));
    }

    /**
     * Returns a unique Id for the current request
     * @param request An HttpServletRequest to get the name info from
     * @return String
     */
    public static String getNextUniqueId(HttpServletRequest request) {
        Integer uniqueIdNumber= (Integer)request.getAttribute("UNIQUE_ID");
        if (uniqueIdNumber == null) {
            uniqueIdNumber = 1;
        }

        request.setAttribute("UNIQUE_ID", uniqueIdNumber + 1);
        return "autoId_" + uniqueIdNumber;
    }

    public static void setContentDisposition(final HttpServletResponse response, final String filename) {
        String dispositionType = UtilProperties.getPropertyValue("requestHandler", "content-disposition-type", "attachment");
        response.setHeader("Content-Disposition", String.format("%s; filename=\"%s\"", dispositionType, filename));
    }

    public static CloseableHttpClient getAllowAllHttpClient() {
        return getAllowAllHttpClient("component://base/config/ofbizssl.jks", "changeit");
    }

    public static CloseableHttpClient getAllowAllHttpClient(String jksStoreFileName, String jksStorePassword) {
        try {
            // Trust own CA and all self-signed certs
            SSLContext sslContext = SSLContexts.custom()
                    .loadTrustMaterial(FileUtil.getFile(jksStoreFileName), jksStorePassword.toCharArray(),
                            new TrustSelfSignedStrategy())
                    .build();
            // No host name verifier
            SSLConnectionSocketFactory sslsf = new SSLConnectionSocketFactory(
                    sslContext,
                    NoopHostnameVerifier.INSTANCE);
            CloseableHttpClient httpClient = HttpClients.custom()
                    .setSSLSocketFactory(sslsf)
                    .build();
            return httpClient;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            return HttpClients.createDefault();
        }
    }

    public static String getMultiRowDelimiter() {
        return MULTI_ROW_DELIMITER;
    }

    public static String getRowSubmitPrefix() {
        return ROW_SUBMIT_PREFIX;
    }

    /**
     * SCIPIO: Checks if the given uri is a full URL, with strict test.
     * <p>
     * It is a full URL if it starts with a protocol such as "http://", "https://",
     * or simply "//".
     * <p>
     * TODO?: add other protocols as needed...
     */
    public static boolean isFullUrl(String uri) {
        return (uri.startsWith("http://") || uri.startsWith("https://") || uri.startsWith("//"));
    }

    /**
     * SCIPIO: Checks if the given uri is a full URL, permissively, attempting to allow
     * some types of language encodings.
     * <p>
     * <strong>WARN</strong>: This method is imprecise and should be avoided where
     * possible by proper design and using post-escaping instead of pre-escaping, to avoid
     * escaped strings being encountered in such logic as isFullUrl calls.
     * <p>
     * In other words, isFullUrlPerm is currently needed for cases in Ofbiz where escaping happens too early.
     * <p>
     * Currently (2016-10-19), this only handles HTML and Javascript (based on behavior of
     * UtilCodec and Freemarker builtins).
     * <p>
     * FIXME: does nothing for CSS
     */
    public static boolean isFullUrlPerm(String uri) {
        for(String prefix : fullUrlPermPrefixes) {
            if (uri.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    /**
     * SCIPIO: builds a list of prefixes used for checking full URL presence in permissive fashion.
     * <p>
     * WARN/FIXME?: Imprecise, bad, potential security risk, does not handle CSS
     *
     * @see #isFullUrlPerm
     */
    private static List<String> makeFullUrlPermPrefixes() {
        Set<String> prefixes = new LinkedHashSet<>(); // the Set will eliminate duplicates
        prefixes.add("http://");
        prefixes.add("https://");
        prefixes.add("//");

        for(String encoderName : new String[] { "html" }) {
            UtilCodec.SimpleEncoder encoder = UtilCodec.getEncoder(encoderName);
            prefixes.add(encoder.encode("http://"));
            prefixes.add(encoder.encode("https://"));
            prefixes.add(encoder.encode("//"));
        }

        // SPECIAL CASE: the encoders don't deal with javascript properly, where the frontslashes
        // may be escaped by backslashes. even worse, in Freemarker, only the first slash is escaped...
        prefixes.add("\\/\\/");
        prefixes.add("\\//");

        // FIXME: Doesn't handle CSS!

        ArrayList<String> prefixList = new ArrayList<>(prefixes);
        prefixList.trimToSize();
        return prefixList;
    }

    /**
     * SCIPIO: pre-built list of static full URL permissive prefixes
     */
    private static final List<String> fullUrlPermPrefixes = makeFullUrlPermPrefixes();

    /**
     * SCIPIO: Returns all the [paramNameSuffix] suffixes from parameters named "[paramNamePrefix][paramNameSuffix]" for which
     * the parameter value is in the specified values.
     */
    public static List<String> getParameterNamesWithValue(HttpServletRequest request, Collection<String> values, String paramNamePrefix) {
        List<String> result = new ArrayList<>();
        Enumeration<String> e = UtilGenerics.cast(request.getParameterNames());
        if (paramNamePrefix == null) {
            paramNamePrefix = "";
        }
        while (e.hasMoreElements()) {
            String name = e.nextElement();
            if (name.startsWith(paramNamePrefix) && values.contains(request.getParameter(name))) {
                result.add(name.substring(paramNamePrefix.length()));
            }
        }
        return result;
    }

    /**
     * SCIPIO: Returns all the [paramNameSuffix] suffixes from parameters named "[paramNamePrefix][paramNameSuffix]" for which
     * the parameter value is in the specified values.
     */
    public static List<String> getParameterNamesWithValue(HttpServletRequest request, String value, String paramNamePrefix) {
        return getParameterNamesWithValue(request, UtilMisc.toSet(value), paramNamePrefix);
    }

    /**
     * SCIPIO: Gets the request attribute names into the specified collection and returns the collection.
     */
    public static <C extends Collection<? super String>> C getRequestAttrNames(HttpServletRequest request, C out, Collection<String> namesToSkip) {
        Enumeration<String> names = request.getAttributeNames();
        while(names.hasMoreElements()) {
            String name = names.nextElement();
            if (namesToSkip == null || !namesToSkip.contains(name)) {
                out.add(name);
            }
        }
        return out;
    }

    /**
     * SCIPIO: Gets the request attribute names into the specified collection and returns the collection.
     */
    public static <C extends Collection<? super String>> C getRequestAttrNames(HttpServletRequest request, C out) {
        Enumeration<String> names = request.getAttributeNames();
        while(names.hasMoreElements()) {
            out.add(names.nextElement());
        }
        return out;
    }

    /**
     * SCIPIO: Gets the request attribute names into the specified collection and returns the collection.
     */
    public static Set<String> getRequestAttrNamesAsSet(HttpServletRequest request, Collection<String> namesToSkip) {
        return getRequestAttrNames(request, new HashSet<>(), namesToSkip);
    }

    /**
     * SCIPIO: Gets the request attribute names into the specified collection and returns the collection.
     */
    public static Set<String> getRequestAttrNamesAsSet(HttpServletRequest request) {
        return getRequestAttrNames(request, new HashSet<>());
    }

    /**
     * SCIPIO: Get the given session attribute if the session exists, otherwise null.
     */
    public static <T> T getSessionAttribute(HttpServletRequest request, String attrName) {
        return getSessionAttribute(request.getSession(false), attrName);
    }

    /**
     * SCIPIO: Get the given session attribute if the session exists, otherwise null.
     */
    public static <T> T getSessionAttribute(HttpSession session, String attrName) {
        return (session != null) ? UtilGenerics.cast(session.getAttribute(attrName)) : null;
    }

    /**
     * SCIPIO: Remove the given session attribute if the session exists.
     */
    public static void removeSessionAttribute(HttpServletRequest request, String attrName) {
        removeSessionAttribute(request.getSession(false), attrName);
    }

    /**
     * SCIPIO: Remove the given session attribute if the session is non-null.
     */
    public static void removeSessionAttribute(HttpSession session, String attrName) {
        if (session != null) {
            session.removeAttribute(attrName);
        }
    }

    /**
     * SCIPIO: Transfers the named request attribute to the given map, efficiently.
     */
    public static Map<String, Object> requestAttributesToMap(HttpServletRequest request, Map<String, Object> outMap, Collection<String> attrNames) {
        for(String attrName : attrNames) {
            outMap.put(attrName, request.getAttribute(attrName));
        }
        return outMap;
    }

    /**
     * SCIPIO: Transfers the named request attribute to the given map, efficiently.
     */
    public static Map<String, Object> requestAttributesToMap(HttpServletRequest request, Map<String, Object> outMap, String... attrNames) {
        for(String attrName : attrNames) {
            outMap.put(attrName, request.getAttribute(attrName));
        }
        return outMap;
    }

    /**
     * SCIPIO: Transfers the named request attribute to the given map, efficiently.
     */
    public static Map<String, Object> requestAttributesToMapNoNull(HttpServletRequest request, Map<String, Object> outMap, Collection<String> attrNames) {
        for(String attrName : attrNames) {
            Object attrValue = request.getAttribute(attrName);
            if (attrValue != null) {
                outMap.put(attrName, attrValue);
            }
        }
        return outMap;
    }

    /**
     * SCIPIO: Transfers the named request attribute to the given map, efficiently.
     */
    public static Map<String, Object> requestAttributesToMapNoNull(HttpServletRequest request, Map<String, Object> outMap, String... attrNames) {
        for(String attrName : attrNames) {
            Object attrValue = request.getAttribute(attrName);
            if (attrValue != null) {
                outMap.put(attrName, attrValue);
            }
        }
        return outMap;
    }

    /**
     * SCIPIO: Sets request attributes from named map keys, efficiently.
     */
    public static void setRequestAttributesFromMap(HttpServletRequest request, Map<String, ?> attrMap, Collection<String> attrNames) {
        for(String attrName : attrNames) {
            request.setAttribute(attrName, attrMap.get(attrName));
        }
    }

    /**
     * SCIPIO: Sets request attributes from named map keys, efficiently.
     */
    public static void setRequestAttributesFromMap(HttpServletRequest request, Map<String, ?> attrMap, String... attrNames) {
        for(String attrName : attrNames) {
            request.setAttribute(attrName, attrMap.get(attrName));
        }
    }

    /**
     * SCIPIO: Sets request attributes from all map entries, efficiently.
     */
    public static void setRequestAttributesFromMap(HttpServletRequest request, Map<String, ?> attrMap) {
        for(Map.Entry<String, ?> entry : attrMap.entrySet()) {
            request.setAttribute(entry.getKey(), entry.getValue());
        }
    }

    /**
     * SCIPIO: Returns all headers in the request.
     */
    public static Map<String, List<String>> getHeaderMap(HttpServletRequest request) {
        Map<String, List<String>> headerMap = new LinkedHashMap<>();
        Enumeration<String> headerNames = request.getHeaderNames();
        if (headerNames != null) { // NOTE: null never happens with tomcat but API allows null
            while (headerNames.hasMoreElements()) {
                String headerName = headerNames.nextElement();
                Enumeration<String> headers = request.getHeaders(headerName);
                if (headers != null) {
                    List<String> headerList = Collections.list(headers);
                    if (!headerList.isEmpty()) { // NOTE: This shouldn't happen either, just in case
                        headerMap.put(headerName, headerList);
                    }
                }
            }
        }
        return headerMap;
    }

    /**
     * SCIPIO: Returns an object which can be used for full session synchronization (never null).
     * <p>
     * NOTE: This must be used instead of synchronizing directly on the session. It is not supported
     * by the servlet API to synchronize on the HttpSession object itself, and it also prevents
     * session facades from being used in projects.
     * <p>
     * The sync object is normally set on session creation by {@link SessionSyncEventListener}
     * which is automatically added to all webapps (since 2018-12-03) by CatalinaContainer.
     * This method will print a warning if it's missing, because such case can cause a race.
     * <p>
     * Added 2018-12-03.
     */
    public static Object getSessionSyncObject(HttpSession session) {
        Object syncObj = session.getAttribute(SESSION_SYNCOBJ);
        if (syncObj != null) {
            return syncObj;
        }
        // The sync object should always be there, but if for some reason it got removed, add one...
        // NOTE: For BEST-EFFORT emergency reasons, we'll lock on HttpSession here, but it is likely to do nothing.
        synchronized(session) {
            syncObj = session.getAttribute(SESSION_SYNCOBJ);
            if (syncObj != null) {
                return syncObj;
            }
            syncObj = createSessionSyncObject();
            session.setAttribute(SESSION_SYNCOBJ, syncObj);
            Debug.logWarning("getSessionSyncObject: Session synchronization object (" + SESSION_SYNCOBJ
                    + ") not found in session attributes for context "  + session.getServletContext().getContextPath() + "; creating", module); // log after to minimize exposure
        }
        return syncObj;
    }

    /**
     * SCIPIO: Returns an object which can be used for full session synchronization (never null).
     * <p>
     * NOTE: This must be used instead of synchronizing directly on the session. It is not supported
     * by the servlet API to synchronize on the HttpSession object itself, and it also prevents
     * session facades from being used in projects.
     * <p>
     * The sync object is normally set on session creation by {@link SessionSyncEventListener}
     * which is automatically added to all webapps (since 2018-12-03) by CatalinaContainer.
     * This method will print a warning if it's missing, because such case can cause a race.
     * <p>
     * Added 2018-12-03.
     */
    public static Object getSessionSyncObject(HttpServletRequest request) {
        return getSessionSyncObject(request.getSession());
    }

    @SuppressWarnings("serial")
    public static Object createSessionSyncObject() { // SCIPIO
        return new java.io.Serializable(){};
    }

    /**
     * SCIPIO: Special listener to initialize the session sync object.
     * Automatically added to all webapps by CatalinaContainer.
     * Added 2018-12-03.
     */
    public static class SessionSyncEventListener implements javax.servlet.http.HttpSessionListener {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        private static final SessionSyncEventListener INSTANCE = new SessionSyncEventListener();
        public static javax.servlet.http.HttpSessionListener getInstance() { return INSTANCE; }
        @Override
        public void sessionCreated(HttpSessionEvent se) {
            if (Debug.verboseOn()) {
                Debug.logVerbose("Initialized session sync object for context: " + se.getSession().getServletContext().getContextPath(), module);
            }
            se.getSession().setAttribute(SESSION_SYNCOBJ, createSessionSyncObject());
        }
        @Override
        public void sessionDestroyed(HttpSessionEvent se) {
        }
    }

    /**
     * SCIPIO: Returns an object which can be used for full servlet context synchronization (never null).
     * <p>
     * NOTE: This must be used instead of synchronizing directly on the servlet context. It is not supported
     * by the servlet API to synchronize on the HttpServletContext object itself, and it also prevents
     * servlet context facades from being used in projects.
     * <p>
     * The sync object is normally set on servlet context creation by {@link ServletContextSyncEventListener}
     * which is automatically added to all webapps (since 2018-12-03) by CatalinaContainer.
     * This method will print a warning if it's missing, because such case can cause a race.
     * <p>
     * Added 2018-12-03.
     */
    public static Object getServletContextSyncObject(ServletContext context) {
        Object syncObj = context.getAttribute(SERVLETCONTEXT_SYNCOBJ);
        if (syncObj != null) {
            return syncObj;
        }
        // The sync object should always be there, but if for some reason it got removed, add one...
        // NOTE: For BEST-EFFORT emergency reasons, we'll lock on HttpServletContext here, but it is likely to do nothing.
        synchronized(context) {
            syncObj = context.getAttribute(SERVLETCONTEXT_SYNCOBJ);
            if (syncObj != null) {
                return syncObj;
            }
            syncObj = createServletContextSyncObject();
            context.setAttribute(SERVLETCONTEXT_SYNCOBJ, syncObj);
            Debug.logWarning("getServletContextSyncObject: ServletContext synchronization object (" + SERVLETCONTEXT_SYNCOBJ
                    + ") not found in servlet context attributes for context " + context.getContextPath() + "; creating", module); // log after to minimize exposure
        }
        return syncObj;
    }

    /**
     * SCIPIO: Returns an object which can be used for full servlet context synchronization (never null).
     * <p>
     * NOTE: This must be used instead of synchronizing directly on the servlet context. It is not supported
     * by the servlet API to synchronize on the HttpServletContext object itself, and it also prevents
     * servlet context facades from being used in projects.
     * <p>
     * The sync object is normally set on servlet context creation by {@link ServletContextSyncEventListener}
     * which is automatically added to all webapps (since 2018-12-03) by CatalinaContainer.
     * This method will print a warning if it's missing, because such case can cause a race.
     * <p>
     * Added 2018-12-03.
     */
    public static Object getServletContextSyncObject(HttpServletRequest request) {
        return getServletContextSyncObject(request.getServletContext());
    }

    @SuppressWarnings("serial")
    public static Object createServletContextSyncObject() { // SCIPIO
        return new java.io.Serializable(){};
    }

    /**
     * SCIPIO: Special listener to initialize the servlet context sync object.
     * Automatically added to all webapps by CatalinaContainer.
     * Added 2018-12-03.
     */
    public static class ServletContextSyncEventListener implements javax.servlet.ServletContextListener {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        private static final ServletContextSyncEventListener INSTANCE = new ServletContextSyncEventListener();
        public static javax.servlet.ServletContextListener getInstance() { return INSTANCE; }
        @Override
        public void contextInitialized(ServletContextEvent sce) {
            sce.getServletContext().setAttribute(SERVLETCONTEXT_SYNCOBJ, createServletContextSyncObject());
            if (Debug.infoOn()) { // Log this one as info, because important to know Tomcat in fact picked up this listener
                Debug.logInfo("Initialized servlet context sync object for context: " + sce.getServletContext().getContextPath(), module);
            }
        }
        @Override
        public void contextDestroyed(ServletContextEvent sce) {
        }
    }

    /**
     * SCIPIO: DO NOT USE: Returns a "dummy" static instance, for use by <code>FreeMarkerWorker</code>.
     * Subject to change without notice.
     * Added 2019-01-31.
     */
    public static UtilHttp getStaticInstance() {
        return INSTANCE;
    }

    /**
     * Returns the web.xml context-param as a name set.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static Set<String> getContextParamAttrAsNameSet(ServletContext servletContext, String attrName, boolean useAttrCache) {
        Object value = servletContext.getAttribute(attrName);
        if (value == null) {
            value = servletContext.getInitParameter(attrName);
            if (value == null) {
                value = UtilProperties.getPropertyValueOrNull("scipioWebapp", "webapp.contextParam." + attrName);
            }
        }
        Set<String> values = null;
        if (value instanceof Set) {
            values = UtilGenerics.cast(value);
        } else if (value instanceof Collection) {
            values = new LinkedHashSet<>(UtilGenerics.cast(value));
        } else if (value instanceof String && !((String) value).isEmpty()) {
            values = new LinkedHashSet<>(Arrays.asList(((String) value).split("[:,;]")));
            if (useAttrCache) {
                servletContext.setAttribute(attrName, values);
            }
        }
        return values;
    }

    /**
     * Checks if map, list or other contains only serializable objects (best-effort).
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static void checkSerializable(Object value, String keyDesc, int logLevel) {
        if (value instanceof Map) {
            if (keyDesc == null) {
                keyDesc = "(map)";
            }
            for(Map.Entry<?, ?> entry : (UtilGenerics.<Map<?, ?>>cast(value)).entrySet()) {
                if (entry.getValue() == null) {
                    continue;
                }
                String nextKeyDesc = keyDesc + ".";
                if (entry.getKey() instanceof String || entry.getKey() instanceof CharSequence || entry.getKey() instanceof Number) {
                    nextKeyDesc += entry.getKey().toString();
                } else {
                    nextKeyDesc += "(unknown)";
                }
                checkSerializable(entry.getValue(), nextKeyDesc, logLevel);
            }
        } else if (value instanceof Collection) {
            if (keyDesc == null) {
                keyDesc = "(collection)";
            }
            int index = 0;
            for(Object entry : (UtilGenerics.<Collection<?>>cast(value))) {
                if (entry == null) {
                    continue;
                }
                String nextKeyDesc = keyDesc + "." + index;
                checkSerializable(entry, nextKeyDesc, logLevel);
                index++;
            }
        } else if (value instanceof java.lang.StackTraceElement) {
            if (keyDesc == null) {
                keyDesc = "(stacktrace)";
            }
            Debug.log(logLevel, "checkSerializable: [" + keyDesc + "] value is non-serializable [java.lang.StackTraceElement]", module);
        } else if (value != null && !(value instanceof java.io.Serializable)) {
            if (keyDesc == null) {
                keyDesc = "(unknown)";
            }
            Debug.log(logLevel, "checkSerializable: [" + keyDesc + "] value is non-serializable [" + value.getClass().getName() + "]", module);
        }
    }

    /**
     * Returns ServletContext from request, session or servlet context.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static ServletContext getServletContext(Object servletObject) {
        if (servletObject instanceof ServletContext) {
            return (ServletContext) servletObject;
        } else if (servletObject instanceof ServletRequest) {
            return ((ServletRequest) servletObject).getServletContext();
        } else if (servletObject instanceof HttpSession) {
            return ((HttpSession) servletObject).getServletContext();
        } else {
            throw new IllegalArgumentException("Cannot get ServletContext from type: " + (servletObject != null ? servletObject.getClass().getName() : "null"));
        }
    }

    /**
     * Returns ServletContext from request, session or servlet context, or null if already null.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static ServletContext getServletContextSafe(Object servletObject) {
        return (servletObject != null) ? getServletContext(servletObject) : null;
    }

}
