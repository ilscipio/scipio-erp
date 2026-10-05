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
package @component-package@.@component-name@.service;

import java.util.Map;

import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * Implementation of the @component-resource-name@ services declared by {@code @component-resource-name@Services}.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template. Replace with real business logic.</p>
 */
public class @component-resource-name@ServiceImpl {

    private @component-resource-name@ServiceImpl() {}

    /** Creates one @component-resource-name@ record. Replace with real logic (e.g. delegator.makeValue + create). */
    public static Map<String, Object> create@component-resource-name@(DispatchContext dctx, Map<String, ? extends Object> context) {
        String name = (String) context.get("name");
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("name", name);
        return result;
    }
}
