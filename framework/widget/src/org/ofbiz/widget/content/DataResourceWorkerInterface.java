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
package org.ofbiz.widget.content;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;

/**
 * ContentWorkerInterface
 */
public interface DataResourceWorkerInterface {
    public String renderDataResourceAsTextExt(Delegator delegator, String dataResourceId, Map<String, Object> templateContext,
            Locale locale, String targetMimeTypeId, boolean cache) throws GeneralException, IOException;

    public void renderDataResourceAsTextExt(Delegator delegator, String dataResourceId, Appendable out, Map<String, Object> templateContext,
            Locale locale, String targetMimeTypeId, boolean cache) throws GeneralException, IOException;
}
