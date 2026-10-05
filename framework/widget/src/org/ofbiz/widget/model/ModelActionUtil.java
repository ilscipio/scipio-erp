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
package org.ofbiz.widget.model;

import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;

public class ModelActionUtil {

    /**
     * @param context
     * @param result
     * @param resultMapNameAcsr
     */
    protected static void contextPutQueryStringOrAllResult(Map<String, Object> context, Map<String, Object> result, FlexibleMapAccessor<Map<String, Object>> resultMapNameAcsr) {
        if (!resultMapNameAcsr.isEmpty()) {
            resultMapNameAcsr.put(context, result);
            String queryString = (String) result.get("queryString");
            context.put("queryString", queryString);
            context.put("queryStringMap", result.get("queryStringMap"));
            if (UtilValidate.isNotEmpty(queryString)) {
                String queryStringEncoded = queryString.replaceAll("&", "%26");
                context.put("queryStringEncoded", queryStringEncoded);
            }
        } else {
            context.putAll(result);
        }
    }
}
