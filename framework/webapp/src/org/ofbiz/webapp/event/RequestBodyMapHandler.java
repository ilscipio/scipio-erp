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

/**
 * A handler that can extract a Map (typically used as a service input map) from the data in the body of a <code>ServletRequest</code>.
 */
public interface RequestBodyMapHandler {
    /** Extracts from the data in the body of the <code>ServletRequest</code> an instance of <code>Map&lt;String, Object&gt;</code>.
     *
     * @param request the request with the data in its body
     * @return an instance of <code>Map&lt;String, Object&gt;</code> that represents the data in the request body
     */
    public Map<String, Object> extractMapFromRequestBody(ServletRequest request) throws IOException;
}
