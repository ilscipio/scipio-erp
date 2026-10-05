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
package org.ofbiz.webapp.event;

import java.io.IOException;
import java.util.Map;

import javax.servlet.ServletRequest;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.UtilGenerics;

/**
 * An implementation of <code>RequestBodyMapHandler</code> that can extract a <code>Map&lt;String, Object&gt;</code> from the JSON data in the request body.
 */
public class JSONRequestBodyMapHandler implements RequestBodyMapHandler {
    public Map<String, Object> extractMapFromRequestBody(ServletRequest request) throws IOException {
        return UtilGenerics.<Map<String, Object>>cast(JSON.from(request.getInputStream()).toObject(Map.class));
    }
}
