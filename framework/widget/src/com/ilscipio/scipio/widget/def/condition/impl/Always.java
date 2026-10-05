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

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that always evaluates to true.
 *
 * <p>This is used as a fallback when complex conditions (like nested or/xor)
 * cannot be directly represented in annotations.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class Always implements WidgetCondition {

    @Override
    public void init(String[] params) {
        // No parameters needed - always returns true
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        return true;
    }
}
