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
package com.ilscipio.scipio.product.seo;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;

/**
 * SCIPIO: SEO Catalog URL util
 */
public class SeoUrlUtil {
    
    /**
     * @deprecated this method did not preserve order of charFilters (though it could have
     * with LinkedHashMap, method has no control) - use {@link UrlProcessors.CharFilter} instead.
     */
    @Deprecated
    public static String replaceSpecialCharsUrl(String url, Map<String, String> charFilters) {
        if (charFilters == null) return url;
        if (UtilValidate.isEmpty(url)) {
            url = "";
        }
        for (String characterPattern : charFilters.keySet()) {
            url = url.replaceAll(characterPattern, charFilters.get(characterPattern));
        }
        return url;
    }

    /**
     * @deprecated does not properly handle delimiters.
     */
    @Deprecated
    public static String removeContextPath(String uri, String contextPath) {
        if (UtilValidate.isEmpty(contextPath) || UtilValidate.isEmpty(uri)) {
            return uri;
        }
        if (uri.length() > contextPath.length() && uri.startsWith(contextPath)) {
            return uri.substring(contextPath.length());
        }
        return uri;
    }
}
