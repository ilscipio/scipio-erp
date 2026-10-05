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
package com.ilscipio.scipio.solr.def;

import com.ilscipio.scipio.entity.def.Entity;
import com.ilscipio.scipio.entity.def.Field;
import com.ilscipio.scipio.entity.def.KeyMap;
import com.ilscipio.scipio.entity.def.PrimaryKey;
import com.ilscipio.scipio.entity.def.Relation;
import com.ilscipio.scipio.entity.def.RelationType;

/**
 * SolrStatus entity - tracks SOLR data synchronization status.
 *
 * <p>SCIPIO: 4.0.0: Migrated from XML to annotation-based entity definition.</p>
 */
@Entity(
    name = "SolrStatus",
    packageName = "com.ilscipio.scipio.solr",
    title = "SOLR Status",
    neverCache = true
)
@Field(name = "solrId", type = "id-ne")
@Field(name = "dataStatusId", type = "id")
@Field(name = "dataCfgVersion", type = "value",
       description = "Last config version used (by rebuildSolrIndex) - from solrconfig.properties/solr.config.version[.custom]")
@PrimaryKey(field = "solrId")
@Relation(
    type = RelationType.ONE,
    relEntityName = "StatusItem",
    fkName = "SOLR_DATA_STTS",
    keyMaps = @KeyMap(fieldName = "dataStatusId", relFieldName = "statusId")
)
public interface SolrStatusEntity {
    // Marker interface for entity definition
}
