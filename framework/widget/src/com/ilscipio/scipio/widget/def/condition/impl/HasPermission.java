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

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if the user has a specific permission.
 *
 * <p>Params: [permission, action?]</p>
 * <ul>
 *   <li>permission (required): The permission to check (e.g., "PARTYMGR")</li>
 *   <li>action (optional): The action to check (e.g., "_ADMIN", "_CREATE")</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class HasPermission implements WidgetCondition {

    private String permission;
    private String action;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("HasPermission requires at least 1 parameter: [permission]");
        }
        this.permission = params[0];
        this.action = params.length > 1 ? params[1] : null;
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        Security security = (Security) context.get("security");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (security == null || userLogin == null) {
            return false;
        }

        if (UtilValidate.isNotEmpty(action)) {
            // Check entity permission with action
            return security.hasEntityPermission(permission, action, userLogin);
        }
        // Check simple permission
        return security.hasPermission(permission, userLogin);
    }
}
