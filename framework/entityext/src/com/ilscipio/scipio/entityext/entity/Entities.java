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
package com.ilscipio.scipio.entityext.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Entities {

    /**
     * Entity Grouping
     */
    @Entity(
        name = "EntityGroup",
        packageName = "org.ofbiz.entity.group",
        title = "Entity Grouping",
        fields = {
            @Field(name = "entityGroupId", type = "id-ne"),
            @Field(name = "entityGroupName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "entityGroupId")
        }
    )
    public interface EntityGroupEntity {}

    /**
     * Entity Grouping
     */
    @Entity(
        name = "EntityGroupEntry",
        packageName = "org.ofbiz.entity.group",
        title = "Entity Grouping",
        fields = {
            @Field(name = "entityGroupId", type = "id-ne"),
            @Field(name = "entityOrPackage", type = "long-varchar"),
            @Field(name = "applEnumId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "entityGroupId"),
            @PrimaryKey(field = "entityOrPackage")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EntityGroup",
                fkName = "ENTGRP_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "entityGroupId")
                }
            )
        }
    )
    public interface EntityGroupEntryEntity {}

    /**
     * Entity Synchronization
     */
    @Entity(
        name = "EntitySync",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization",
        fields = {
            @Field(name = "entitySyncId", type = "id-ne"),
            @Field(name = "runStatusId", type = "id-ne"),
            @Field(name = "lastSuccessfulSynchTime", type = "date-time"),
            @Field(name = "lastHistoryStartDate", type = "date-time"),
            @Field(name = "preOfflineSynchTime", type = "date-time"),
            @Field(name = "offlineSyncSplitMillis", type = "numeric"),
            @Field(name = "syncSplitMillis", type = "numeric"),
            @Field(name = "syncEndBufferMillis", type = "numeric"),
            @Field(name = "maxRunningNoUpdateMillis", type = "numeric"),
            @Field(name = "targetServiceName", type = "long-varchar"),
            @Field(name = "targetDelegatorName", type = "long-varchar"),
            @Field(name = "keepRemoveInfoHours", type = "floating-point"),
            @Field(name = "forPullOnly", type = "indicator"),
            @Field(name = "forPushOnly", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "entitySyncId")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "EntitySyncInclGrpDetailView",
                keyMaps = {
                    @KeyMap(fieldName = "entitySyncId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EntitySyncHistory",
                title = "Last",
                keyMaps = {
                    @KeyMap(fieldName = "entitySyncId"),
                    @KeyMap(fieldName = "lastHistoryStartDate", relFieldName = "startDate")
                }
            )
        }
    )
    public interface EntitySyncEntity {}

    /**
     * Entity Synchronization History
     */
    @Entity(
        name = "EntitySyncHistory",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization History",
        fields = {
            @Field(name = "entitySyncId", type = "id-ne"),
            @Field(name = "startDate", type = "date-time"),
            @Field(name = "runStatusId", type = "id-ne"),
            @Field(name = "beginningSynchTime", type = "date-time"),
            @Field(name = "lastSuccessfulSynchTime", type = "date-time"),
            @Field(name = "lastCandidateEndTime", type = "date-time"),
            @Field(name = "lastSplitStartTime", type = "numeric"),
            @Field(name = "toCreateInserted", type = "numeric"),
            @Field(name = "toCreateUpdated", type = "numeric"),
            @Field(name = "toCreateNotUpdated", type = "numeric"),
            @Field(name = "toStoreInserted", type = "numeric"),
            @Field(name = "toStoreUpdated", type = "numeric"),
            @Field(name = "toStoreNotUpdated", type = "numeric"),
            @Field(name = "toRemoveDeleted", type = "numeric"),
            @Field(name = "toRemoveAlreadyDeleted", type = "numeric"),
            @Field(name = "totalRowsExported", type = "numeric"),
            @Field(name = "totalRowsToCreate", type = "numeric"),
            @Field(name = "totalRowsToStore", type = "numeric"),
            @Field(name = "totalRowsToRemove", type = "numeric"),
            @Field(name = "totalSplits", type = "numeric"),
            @Field(name = "totalStoreCalls", type = "numeric"),
            @Field(name = "runningTimeMillis", type = "numeric"),
            @Field(name = "perSplitMinMillis", type = "numeric"),
            @Field(name = "perSplitMaxMillis", type = "numeric"),
            @Field(name = "perSplitMinItems", type = "numeric"),
            @Field(name = "perSplitMaxItems", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "entitySyncId"),
            @PrimaryKey(field = "startDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EntitySync",
                fkName = "ENTSYNC_HSTSNC",
                keyMaps = {
                    @KeyMap(fieldName = "entitySyncId")
                }
            )
        }
    )
    public interface EntitySyncHistoryEntity {}

    /**
     * Entity Synchronization Include
     */
    @Entity(
        name = "EntitySyncInclude",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization Include",
        fields = {
            @Field(name = "entitySyncId", type = "id-ne"),
            @Field(name = "entityOrPackage", type = "long-varchar"),
            @Field(name = "applEnumId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "entitySyncId"),
            @PrimaryKey(field = "entityOrPackage")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EntitySync",
                fkName = "ENTSYNC_INCSNC",
                keyMaps = {
                    @KeyMap(fieldName = "entitySyncId")
                }
            )
        }
    )
    public interface EntitySyncIncludeEntity {}

    /**
     * Entity Synchronization Include Entity Group
     */
    @Entity(
        name = "EntitySyncIncludeGroup",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization Include Entity Group",
        fields = {
            @Field(name = "entitySyncId", type = "id-ne"),
            @Field(name = "entityGroupId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "entitySyncId"),
            @PrimaryKey(field = "entityGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EntityGroup",
                fkName = "ENTSNCGU_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "entityGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EntitySync",
                fkName = "ENTSNCGU_SNC",
                keyMaps = {
                    @KeyMap(fieldName = "entitySyncId")
                }
            )
        }
    )
    public interface EntitySyncIncludeGroupEntity {}

    /**
     * Entity Synchronization Remove
     */
    @Entity(
        name = "EntitySyncRemove",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization Remove",
        fields = {
            @Field(name = "entitySyncRemoveId", type = "id-ne"),
            @Field(name = "primaryKeyRemoved", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "entitySyncRemoveId")
        }
    )
    public interface EntitySyncRemoveEntity {}

    /**
     * Entity Synchronization Include Entity Group Detail View
     */
    @ViewEntity(
        name = "EntitySyncInclGrpDetailView",
        packageName = "org.ofbiz.entity.synchronization",
        title = "Entity Synchronization Include Entity Group Detail View",
        members = {
            @MemberEntity(entityAlias = "ESIG", entityName = "EntitySyncIncludeGroup"),
            @MemberEntity(entityAlias = "EGE", entityName = "EntityGroupEntry")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "ESIG"),
            @AliasAll(entityAlias = "EGE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ESIG",
                relEntityAlias = "EGE",
                keyMaps = {
                    @KeyMap(fieldName = "entityGroupId")
                }
            )
        }
    )
    public interface EntitySyncInclGrpDetailViewView {}

}
