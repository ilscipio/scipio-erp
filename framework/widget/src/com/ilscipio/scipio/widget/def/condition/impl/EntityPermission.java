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

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks entity-level permission.
 *
 * <p>Params: [entityName, entityId, targetOperation]</p>
 * <ul>
 *   <li>entityName (required): The entity name</li>
 *   <li>entityId (required): The entity ID field or expression</li>
 *   <li>targetOperation (required): The operation to check (e.g., _VIEW, _UPDATE, _DELETE, _CREATE)</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class EntityPermission implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String entityName;
    private String entityId;
    private String targetOperation;

    private FlexibleStringExpander entityIdExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 3) {
            throw new IllegalArgumentException("EntityPermission requires 3 parameters: [entityName, entityId, targetOperation]");
        }
        this.entityName = params[0];
        this.entityId = params[1];
        this.targetOperation = params[2];

        this.entityIdExpander = FlexibleStringExpander.getInstance(entityId);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            Security security = (Security) context.get("security");
            GenericValue userLogin = (GenericValue) context.get("userLogin");

            if (security == null) {
                Debug.logWarning("EntityPermission: security not found in context", module);
                return false;
            }

            if (userLogin == null) {
                Debug.logVerbose("EntityPermission: userLogin not found in context", module);
                return false;
            }

            // Expand the entity ID expression
            String expandedEntityId = entityIdExpander.expandString(context);

            // Check entity permission
            // The permission pattern is typically: ENTITYNAME_OPERATION (e.g., PARTYMGR_VIEW)
            String permission = entityName + targetOperation;

            return security.hasEntityPermission(entityName, targetOperation, userLogin);

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating EntityPermission condition for entity [" + entityName + "]", module);
            return false;
        }
    }
}
