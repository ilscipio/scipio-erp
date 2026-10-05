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
package com.ilscipio.scipio.workeffort.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OldEntities {

    /**
     * Work Effort Assignment Rate Entity, now depreciated and replaced by the RateAmount
     */
    @Entity(
        name = "OldWorkEffortAssignmentRate",
        packageName = "org.ofbiz.workeffort.timesheet",
        tableName = "WORK_EFFORT_ASSIGNMENT_RATE",
        title = "Work Effort Assignment Rate Entity, now depreciated and replaced by the RateAmount",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "rate", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "rateTypeId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WEFF_ASRT_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RateType",
                fkName = "WEFF_ASRT_RATETP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "WEFF_ASRT_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface OldWorkEffortAssignmentRateEntity {}

    /**
     * Old WorkEffort Contact Mechanism Entity, now depreciated and replaced by the WorkEffortContactMech
     */
    @Entity(
        name = "OldWorkEffortContactMech",
        packageName = "org.ofbiz.workeffort.workeffort",
        tableName = "WORK_EFFORT_CONTACT_MECH",
        title = "Old WorkEffort Contact Mechanism Entity, now depreciated and replaced by the WorkEffortContactMech",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "OLWKEF_CMECH_WKEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "OLWKEF_CMECH_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface OldWorkEffortContactMechEntity {}

}
