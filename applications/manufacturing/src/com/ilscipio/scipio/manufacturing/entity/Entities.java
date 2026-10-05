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
package com.ilscipio.scipio.manufacturing.entity;

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
     * Product To Part Rule
     */
    @Entity(
        name = "ProductManufacturingRule",
        packageName = "org.ofbiz.manufacturing.bom",
        title = "Product To Part Rule",
        fields = {
            @Field(name = "ruleId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productIdFor", type = "id"),
            @Field(name = "productIdIn", type = "id-ne"),
            @Field(name = "ruleSeqId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "productIdInSubst", type = "id"),
            @Field(name = "productFeature", type = "id"),
            @Field(name = "ruleOperator", type = "id"),
            @Field(name = "quantity", type = "floating-point"),
            @Field(name = "description", type = "description"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "ruleId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODUCT_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "ProductFor",
                fkName = "PRODUCT_FOR",
                keyMaps = {
                    @KeyMap(fieldName = "productIdFor", relFieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "ProductIn",
                fkName = "PRODUCT_IN",
                keyMaps = {
                    @KeyMap(fieldName = "productIdIn", relFieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "ProductSubst",
                fkName = "PRODUCT_SUBST",
                keyMaps = {
                    @KeyMap(fieldName = "productIdInSubst", relFieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "PRODUCT_FEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeature", relFieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductManufacturingRuleEntity {}

    /**
     * Calendar
     * Used to defined the availability of the machines, this entity define the Id and the week definition.       The Id is used in the exception calendar table as reference       
     */
    @Entity(
        name = "TechDataCalendar",
        packageName = "org.ofbiz.manufacturing.techdata",
        title = "Calendar",
        description = "Used to defined the availability of the machines, this entity define the Id and the week definition.\n      The Id is used in the exception calendar table as reference\n      ",
        defaultResourceName = "ManufacturingEntityLabels",
        fields = {
            @Field(name = "calendarId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "calendarWeekId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "calendarId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TechDataCalendarWeek",
                fkName = "CALENDAR_WEEK",
                keyMaps = {
                    @KeyMap(fieldName = "calendarWeekId")
                }
            )
        }
    )
    public interface TechDataCalendarEntity {}

    /**
     * Calendar Exception Day
     * Used to defined some days which differ from the normal day definition in the weekId associated in the calendar.       
     */
    @Entity(
        name = "TechDataCalendarExcDay",
        packageName = "org.ofbiz.manufacturing.techdata",
        title = "Calendar Exception Day",
        description = "Used to defined some days which differ from the normal day definition in the weekId associated in the calendar.\n      ",
        fields = {
            @Field(name = "calendarId", type = "id-ne"),
            @Field(name = "exceptionDateStartTime", type = "date-time"),
            @Field(name = "exceptionCapacity", type = "fixed-point"),
            @Field(name = "usedCapacity", type = "fixed-point"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "calendarId"),
            @PrimaryKey(field = "exceptionDateStartTime")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TechDataCalendar",
                fkName = "EXC_DAY_CALENDAR",
                keyMaps = {
                    @KeyMap(fieldName = "calendarId")
                }
            )
        }
    )
    public interface TechDataCalendarExcDayEntity {}

    /**
     * Calendar Exception Week
     * Used to defined some weeks which differ from the normal week defined in the calendar.
     */
    @Entity(
        name = "TechDataCalendarExcWeek",
        packageName = "org.ofbiz.manufacturing.techdata",
        title = "Calendar Exception Week",
        description = "Used to defined some weeks which differ from the normal week defined in the calendar.",
        fields = {
            @Field(name = "calendarId", type = "id-ne"),
            @Field(name = "exceptionDateStart", type = "date"),
            @Field(name = "calendarWeekId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "calendarId"),
            @PrimaryKey(field = "exceptionDateStart")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TechDataCalendar",
                fkName = "EXC_WEEK_CALENDAR",
                keyMaps = {
                    @KeyMap(fieldName = "calendarId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TechDataCalendarWeek",
                fkName = "EXC_WEEK_WEEK",
                keyMaps = {
                    @KeyMap(fieldName = "calendarWeekId")
                }
            )
        }
    )
    public interface TechDataCalendarExcWeekEntity {}

    /**
     * Week definition
     * Used to defined the week definition disponibility for machine
     */
    @Entity(
        name = "TechDataCalendarWeek",
        packageName = "org.ofbiz.manufacturing.techdata",
        title = "Week definition",
        description = "Used to defined the week definition disponibility for machine",
        defaultResourceName = "ManufacturingEntityLabels",
        fields = {
            @Field(name = "calendarWeekId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "mondayStartTime", type = "time"),
            @Field(name = "mondayCapacity", type = "floating-point"),
            @Field(name = "tuesdayStartTime", type = "time"),
            @Field(name = "tuesdayCapacity", type = "floating-point"),
            @Field(name = "wednesdayStartTime", type = "time"),
            @Field(name = "wednesdayCapacity", type = "floating-point"),
            @Field(name = "thursdayStartTime", type = "time"),
            @Field(name = "thursdayCapacity", type = "floating-point"),
            @Field(name = "fridayStartTime", type = "time"),
            @Field(name = "fridayCapacity", type = "floating-point"),
            @Field(name = "saturdayStartTime", type = "time"),
            @Field(name = "saturdayCapacity", type = "floating-point"),
            @Field(name = "sundayStartTime", type = "time"),
            @Field(name = "sundayCapacity", type = "floating-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "calendarWeekId")
        }
    )
    public interface TechDataCalendarWeekEntity {}

    /**
     * MRP Event Type
     */
    @Entity(
        name = "MrpEventType",
        packageName = "org.ofbiz.manufacturing.mrp",
        title = "MRP Event Type",
        defaultResourceName = "ManufacturingEntityLabels",
        fields = {
            @Field(name = "mrpEventTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "mrpEventTypeId")
        }
    )
    public interface MrpEventTypeEntity {}

    /**
     * MRP Event
     */
    @Entity(
        name = "MrpEvent",
        packageName = "org.ofbiz.manufacturing.mrp",
        title = "MRP Event",
        fields = {
            @Field(name = "mrpId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "eventDate", type = "date-time"),
            @Field(name = "mrpEventTypeId", type = "id-ne"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "quantity", type = "floating-point"),
            @Field(name = "eventName", type = "very-long"),
            @Field(name = "isLate", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "mrpId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "eventDate"),
            @PrimaryKey(field = "mrpEventTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "MRPEV_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MrpEventType",
                fkName = "MRPEV_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "mrpEventTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "MRPEV_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "MrpRun",
                keyMaps = {
                    @KeyMap(fieldName = "mrpId")
                }
            )
        }
    )
    public interface MrpEventEntity {}

    /**
     * SCIPIO: MRP Run header: one row per executeMrp call with status, counts and errors.
     */
    @Entity(
        name = "MrpRun",
        packageName = "org.ofbiz.manufacturing.mrp",
        title = "MRP Run",
        fields = {
            @Field(name = "mrpId", type = "id-ne"),
            @Field(name = "mrpName", type = "name"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "facilityGroupId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "startDate", type = "date-time"),
            @Field(name = "finishDate", type = "date-time"),
            @Field(name = "eventCount", type = "numeric"),
            @Field(name = "proposedProductionRuns", type = "numeric"),
            @Field(name = "proposedPurchases", type = "numeric"),
            @Field(name = "errorCount", type = "numeric"),
            @Field(name = "runByUserLoginId", type = "id-vlong"),
            @Field(name = "message", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "mrpId")
        },
        relations = {
            @Relation(type = RelationType.ONE, relEntityName = "Facility", fkName = "MRPRUN_FAC", keyMaps = {@KeyMap(fieldName = "facilityId")}),
            @Relation(type = RelationType.ONE, relEntityName = "FacilityGroup", fkName = "MRPRUN_FACGRP", keyMaps = {@KeyMap(fieldName = "facilityGroupId")}),
            @Relation(type = RelationType.ONE, relEntityName = "StatusItem", fkName = "MRPRUN_STTS", keyMaps = {@KeyMap(fieldName = "statusId")}),
            @Relation(type = RelationType.MANY, relEntityName = "MrpEvent", keyMaps = {@KeyMap(fieldName = "mrpId")})
        }
    )
    public interface MrpRunEntity {}

    /**
     * SCIPIO: Production run reject (scrap) record: quantity rejected on a task with a reason.
     */
    @Entity(
        name = "ProductionRunReject",
        packageName = "org.ofbiz.manufacturing.jobshopmgt",
        title = "Production Run Reject",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "rejectSeqId", type = "id-ne"),
            @Field(name = "productionRunId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "lotId", type = "id"),
            @Field(name = "rejectDate", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "userLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "rejectSeqId")
        },
        relations = {
            @Relation(type = RelationType.ONE, relEntityName = "WorkEffort", fkName = "PRUNREJ_WE", keyMaps = {@KeyMap(fieldName = "workEffortId")}),
            @Relation(type = RelationType.ONE, title = "ProductionRun", relEntityName = "WorkEffort", fkName = "PRUNREJ_PRUN", keyMaps = {@KeyMap(fieldName = "productionRunId", relFieldName = "workEffortId")}),
            @Relation(type = RelationType.ONE, relEntityName = "Product", fkName = "PRUNREJ_PROD", keyMaps = {@KeyMap(fieldName = "productId")}),
            @Relation(type = RelationType.ONE, title = "Reason", relEntityName = "Enumeration", fkName = "PRUNREJ_ENUM", keyMaps = {@KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")})
        }
    )
    public interface ProductionRunRejectEntity {}

    /**
     * MRP Event View
     */
    @ViewEntity(
        name = "MrpEventView",
        packageName = "org.ofbiz.manufacturing.mrp",
        title = "MRP Event View",
        members = {
            @MemberEntity(entityAlias = "MEV", entityName = "MrpEvent"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "MEV")
        },
        aliases = {
            @Alias(name = "billOfMaterialLevel", entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "MEV",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface MrpEventViewView {}


    /**
     * SCIPIO: a lot (inventory item) reserved for a production run task. The whole lot is held for the
     * task: its ATP is taken down while it is reserved, and released on release or on run close.
     */
    @Entity(
        name = "ProductionRunLotReservation",
        packageName = "org.ofbiz.manufacturing.jobshopmgt",
        title = "Production Run Lot Reservation",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "productionRunId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "lotId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "quantityReserved", type = "fixed-point"),
            @Field(name = "quantityUomId", type = "id"),
            @Field(name = "reservedDate", type = "date-time"),
            @Field(name = "releasedDate", type = "date-time"),
            @Field(name = "userLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
            @Relation(type = RelationType.ONE, relEntityName = "WorkEffort", fkName = "PRUNLOTRES_WE", keyMaps = {@KeyMap(fieldName = "workEffortId")}),
            @Relation(type = RelationType.ONE, relEntityName = "InventoryItem", fkName = "PRUNLOTRES_INV", keyMaps = {@KeyMap(fieldName = "inventoryItemId")}),
            @Relation(type = RelationType.ONE, relEntityName = "Product", fkName = "PRUNLOTRES_PROD", keyMaps = {@KeyMap(fieldName = "productId")})
        }
    )
    public interface ProductionRunLotReservation {}

    /**
     * SCIPIO: one barcode or QR scan on the shop floor: which task, which code, which action was taken.
     */
    @Entity(
        name = "ProductionRunScan",
        packageName = "org.ofbiz.manufacturing.jobshopmgt",
        title = "Production Run Scan",
        fields = {
            @Field(name = "scanId", type = "id-ne"),
            @Field(name = "productionRunId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "scanCode", type = "long-varchar"),
            @Field(name = "scanAction", type = "id"),
            @Field(name = "lotId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "scanDate", type = "date-time"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "scanId")
        },
        relations = {
            @Relation(type = RelationType.ONE, relEntityName = "WorkEffort", fkName = "PRUNSCAN_WE", keyMaps = {@KeyMap(fieldName = "workEffortId")})
        }
    )
    public interface ProductionRunScan {}
}
