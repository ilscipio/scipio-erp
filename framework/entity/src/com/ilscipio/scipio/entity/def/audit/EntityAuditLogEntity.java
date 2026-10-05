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
package com.ilscipio.scipio.entity.def.audit;

import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.Index;
import com.ilscipio.scipio.entity.def.IndexField;
import com.ilscipio.scipio.entity.def.PrimaryKey;

/**
 * EntityAuditLog entity - tracks changes to entity fields for auditing purposes.
 *
 * <p>SCIPIO: 4.0.0: Migrated from XML to annotation-based entity definition.</p>
 */
@Entity(
    name = "EntityAuditLog",
    packageName = "org.ofbiz.entity.audit",
    title = "Entity Audit Log"
)
@Field(name = "auditHistorySeqId", type = "id-ne",
       description = "Sequenced primary key")
@Field(name = "changedEntityName", type = "long-varchar")
@Field(name = "changedFieldName", type = "long-varchar")
@Field(name = "pkCombinedValueText", type = "long-varchar")
@Field(name = "oldValueText", type = "long-varchar")
@Field(name = "newValueText", type = "long-varchar")
@Field(name = "changedDate", type = "date-time")
@Field(name = "changedByInfo", type = "long-varchar",
       description = "This should contain whatever information is available about the user or system that changed the value. This could be a userLoginId, but could be something else too, so there is no foreign key.")
@Field(name = "changedSessionInfo", type = "long-varchar",
       description = "This should contain whatever information is available about the session during which the value was changed. This could be a visitId, but could be something else too, so there is no foreign key.")
@PrimaryKey(field = "auditHistorySeqId")
@Index(
    name = "ENTITY_AUDIT_LOG_DATE",
    fields = {
        @IndexField(name = "changedDate"),
        @IndexField(name = "changedEntityName"),
        @IndexField(name = "pkCombinedValueText")
    }
)
@Index(
    name = "ENTITY_AUDIT_LOG_TIMESTMP",
    fields = {
        @IndexField(name = "lastUpdatedStamp"),
        @IndexField(name = "changedEntityName"),
        @IndexField(name = "changedFieldName")
    }
)
public interface EntityAuditLogEntity {
    // Marker interface for entity definition
}
