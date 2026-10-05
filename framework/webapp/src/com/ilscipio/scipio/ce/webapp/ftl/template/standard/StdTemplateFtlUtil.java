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
package com.ilscipio.scipio.ce.webapp.ftl.template.standard;

import java.util.List;
import java.util.Map;

/**
 * SCIPIO: Standard template markup FTL utils.
 * <p>
 * These are utilities used to assist the markup implementations found under:
 * <code>component://framework/common/webcommon/includes/scipio/lib/standard</code>
 * <p>
 * These are theme-, styling-framework- and platform-aware.
 * <p>
 * DEV NOTE: these could be further divided but I doubt there will be many.
 */
public abstract class StdTemplateFtlUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected StdTemplateFtlUtil() {
    }


    /**
     * Heuristically calculates container grid size factors from lists of parent container sizes.
     * <p>
     * TODO: Not implemented
     */
    public static Map<String, Object> evalAbsContainerSizeFactors(List<Map<String, Object>> sizesList, Object maxSizes,
            List<Map<String, Object>> cachedFactorsList) {
        // TODO
        throw new UnsupportedOperationException("Not implemented");
        /*
        Map<String, Object> res = new HashMap<>();
        res.put("large", 1F);
        res.put("medium", 1F);
        res.put("small", 1F);
        return res; */
    }

}
