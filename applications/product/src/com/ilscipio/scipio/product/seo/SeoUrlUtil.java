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
package com.ilscipio.scipio.product.seo;

import java.util.Map;

import org.ofbiz.base.util.UtilValidate;

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
