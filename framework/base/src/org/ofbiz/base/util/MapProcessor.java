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
package org.ofbiz.base.util;

import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Basic interface for a map processor.
 * <p>
 * Generalization of the logical minilang SimpleMapProcessor interface; processors may be written in any language.
 * <p>
 * This goes further and supports a map of lists to associate error messages with param names in addition to
 * non-specific error message lists.
 */
public interface MapProcessor {

    /**
     * Validates a map and for each entry that does not validate, adds a corresponding
     * message in the entryErrorMessages/generalErrorMessages map for the field.
     * <p>
     * May associate errors with fields using entryErrorMessages or (if not supported) specify generic errorMessages list.
     */
    public void process(Map<String, Object> inMap, Map<String, Object> results, Map<String, List<String>> entryErrorMessages,
            List<String> generalErrorMessages, Locale locale) throws GeneralException;

}