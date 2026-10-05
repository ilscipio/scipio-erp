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
package com.ilscipio.scipio.widget.def.condition.impl;

import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a screen section is empty.
 *
 * <p>Params: [sectionName]</p>
 *
 * <p>This is screen-specific. Returns true if the named section has not been rendered
 * or has no content.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class EmptySection implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String sectionName;
    private FlexibleStringExpander sectionExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("EmptySection requires 1 parameter: [sectionName]");
        }
        this.sectionName = params[0];
        this.sectionExpander = FlexibleStringExpander.getInstance(sectionName);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            String expandedName = sectionExpander.expandString(context);

            // Check if this section name is in the set of non-empty sections
            // The screen renderer tracks which sections have content
            Set<String> nonEmptySections = UtilGenerics.cast(context.get("_SCIPIO_NON_EMPTY_SECTIONS_"));
            if (nonEmptySections != null && nonEmptySections.contains(expandedName)) {
                return false; // Section is NOT empty
            }

            // Also check the standard OFBiz way
            Map<String, Object> sections = UtilGenerics.cast(context.get("sections"));
            if (sections != null) {
                Object sectionContent = sections.get(expandedName);
                if (sectionContent != null) {
                    if (sectionContent instanceof String) {
                        return UtilValidate.isEmpty((String) sectionContent);
                    }
                    return false; // Has content
                }
            }

            // Section not found or empty
            return true;

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating EmptySection condition for [" + sectionName + "]", module);
            return true; // Assume empty on error
        }
    }
}
