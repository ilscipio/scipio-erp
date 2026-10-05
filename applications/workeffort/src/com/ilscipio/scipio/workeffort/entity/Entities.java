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
public class Entities {

    /**
     * Time Entry
     */
    @Entity(
        name = "TimeEntry",
        packageName = "org.ofbiz.workeffort.timesheet",
        title = "Time Entry",
        fields = {
            @Field(name = "timeEntryId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "rateTypeId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "timesheetId", type = "id"),
            @Field(name = "invoiceId", type = "id"),
            @Field(name = "invoiceItemSeqId", type = "id"),
            @Field(name = "hours", type = "floating-point"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "timeEntryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "TIME_ENT_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RateType",
                fkName = "TIME_ENT_RTTP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "TIME_ENT_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Timesheet",
                fkName = "TIME_ENT_TSHT",
                keyMaps = {
                    @KeyMap(fieldName = "timesheetId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "TIME_ENT_INVIT",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface TimeEntryEntity {}

    /**
     * Timesheet
     */
    @Entity(
        name = "Timesheet",
        packageName = "org.ofbiz.workeffort.timesheet",
        title = "Timesheet",
        fields = {
            @Field(name = "timesheetId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "clientPartyId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "approvedByUserLoginId", type = "id-vlong"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "timesheetId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "TIMESHEET_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Client",
                fkName = "TIMESHEET_CPTY",
                keyMaps = {
                    @KeyMap(fieldName = "clientPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "TIMESHEET_STS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ApprovedBy",
                fkName = "TIMESHEET_AB_UL",
                keyMaps = {
                    @KeyMap(fieldName = "approvedByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface TimesheetEntity {}

    /**
     * Timesheet Role
     */
    @Entity(
        name = "TimesheetRole",
        packageName = "org.ofbiz.workeffort.timesheet",
        title = "Timesheet Role",
        fields = {
            @Field(name = "timesheetId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "timesheetId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Timesheet",
                fkName = "TIMESHTRL_TSHT",
                keyMaps = {
                    @KeyMap(fieldName = "timesheetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "TIMESHTRL_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "TIMESHTRL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface TimesheetRoleEntity {}

    /**
     * WorkEffort Application Sandbox
     */
    @Entity(
        name = "ApplicationSandbox",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Application Sandbox",
        fields = {
            @Field(name = "applicationId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "runtimeDataId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "applicationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortPartyAssignment",
                fkName = "APP_SNDBX_WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RuntimeData",
                fkName = "APP_SNDBX_RNTMDTA",
                keyMaps = {
                    @KeyMap(fieldName = "runtimeDataId")
                }
            )
        }
    )
    public interface ApplicationSandboxEntity {}

    /**
     * Communication Event Work Effort
     */
    @Entity(
        name = "CommunicationEventWorkEff",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Communication Event Work Effort",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "COMEV_WEFF_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COMEV_WEFF_CMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventWorkEffEntity {}

    /**
     * Deliverable
     */
    @Entity(
        name = "Deliverable",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Deliverable",
        fields = {
            @Field(name = "deliverableId", type = "id-ne"),
            @Field(name = "deliverableTypeId", type = "id"),
            @Field(name = "deliverableName", type = "name"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "deliverableId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DeliverableType",
                fkName = "DELIVERABLE_DLTYP",
                keyMaps = {
                    @KeyMap(fieldName = "deliverableTypeId")
                }
            )
        }
    )
    public interface DeliverableEntity {}

    /**
     * Deliverable Type
     */
    @Entity(
        name = "DeliverableType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Deliverable Type",
        fields = {
            @Field(name = "deliverableTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "deliverableTypeId")
        }
    )
    public interface DeliverableTypeEntity {}

    /**
     * Work Effort
     */
    @Entity(
        name = "WorkEffort",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "workEffortTypeId", type = "id"),
            @Field(name = "currentStatusId", type = "id"),
            @Field(name = "lastStatusUpdate", type = "date-time"),
            @Field(name = "workEffortPurposeTypeId", type = "id"),
            @Field(name = "workEffortParentId", type = "id", description = "The primary parent (or the like); it should be one of the parent WorkEfforts already setup in WorkEffortAssoc"),
            @Field(name = "scopeEnumId", type = "id"),
            @Field(name = "priority", type = "numeric"),
            @Field(name = "percentComplete", type = "numeric"),
            @Field(name = "workEffortName", type = "name"),
            @Field(name = "showAsEnumId", type = "id"),
            @Field(name = "sendNotificationEmail", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "locationDesc", type = "description"),
            @Field(name = "estimatedStartDate", type = "date-time"),
            @Field(name = "estimatedCompletionDate", type = "date-time"),
            @Field(name = "actualStartDate", type = "date-time"),
            @Field(name = "actualCompletionDate", type = "date-time"),
            @Field(name = "estimatedMilliSeconds", type = "floating-point"),
            @Field(name = "estimatedSetupMillis", type = "floating-point"),
            @Field(name = "estimateCalcMethod", type = "id"),
            @Field(name = "actualMilliSeconds", type = "floating-point"),
            @Field(name = "actualSetupMillis", type = "floating-point"),
            @Field(name = "totalMilliSecondsAllowed", type = "floating-point"),
            @Field(name = "totalMoneyAllowed", type = "currency-amount"),
            @Field(name = "moneyUomId", type = "id"),
            @Field(name = "specialTerms", type = "long-varchar"),
            @Field(name = "timeTransparency", type = "numeric", description = "Deprecated - use the availabilityStatusId field in the assignment entities instead"),
            @Field(name = "universalId", type = "short-varchar"),
            @Field(name = "sourceReferenceId", type = "id-long"),
            @Field(name = "fixedAssetId", type = "id", description = "Deprecated - use the WorkEffortFixedAssetAssign entity instead"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "infoUrl", type = "long-varchar"),
            @Field(name = "recurrenceInfoId", type = "id", description = "Deprecated - use the tempExprId field instead"),
            @Field(name = "tempExprId", type = "id"),
            @Field(name = "runtimeDataId", type = "id"),
            @Field(name = "noteId", type = "id"),
            @Field(name = "serviceLoaderName", type = "name"),
            @Field(name = "quantityToProduce", type = "fixed-point"),
            @Field(name = "quantityProduced", type = "fixed-point"),
            @Field(name = "quantityRejected", type = "fixed-point"),
            @Field(name = "reservPersons", type = "fixed-point", description = "the number of persons renting the attached asset"),
            @Field(name = "reserv2ndPPPerc", type = "fixed-point", description = "reservationSecondPersonPricePercentage: percentage of the end price for the 2nd person renting this asset connected to the workEffort"),
            @Field(name = "reservNthPPPerc", type = "fixed-point", description = "reservationNthPersonPricePercentage: percentage of the end price for the Nth (2+) person renting this asset connected to the workEffort"),
            @Field(name = "accommodationMapId", type = "id"),
            @Field(name = "accommodationSpotId", type = "id"),
            @Field(name = "revisionNumber", type = "numeric"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortType",
                fkName = "WK_EFFRT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortPurposeType",
                fkName = "WK_EFFRT_PRPTYP",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortPurposeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "Parent",
                fkName = "WK_EFFRT_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortParentId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Current",
                fkName = "WK_EFFRT_CURSTTS",
                keyMaps = {
                    @KeyMap(fieldName = "currentStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Scope",
                fkName = "WK_EFFRT_SC_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "scopeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "WK_EFFRT_FXDASST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "WK_EFFRT_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Money",
                fkName = "WK_EFFRT_MON_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "moneyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceInfo",
                fkName = "WK_EFFRT_RECINFO",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TemporalExpression",
                fkName = "WK_EFFRT_TEMPEXPR",
                keyMaps = {
                    @KeyMap(fieldName = "tempExprId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RuntimeData",
                fkName = "WK_EFFRT_RNTMDTA",
                keyMaps = {
                    @KeyMap(fieldName = "runtimeDataId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "WK_EFFRT_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "WK_EFFRT_CUS_MET",
                keyMaps = {
                    @KeyMap(fieldName = "estimateCalcMethod", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationMap",
                fkName = "WK_EFFRT_ACC_MAP",
                keyMaps = {
                    @KeyMap(fieldName = "accommodationMapId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationSpot",
                fkName = "WK_EFFRT_ACC_SPOT",
                keyMaps = {
                    @KeyMap(fieldName = "accommodationSpotId")
                }
            )
        }
    )
    public interface WorkEffortEntity {}

    /**
     * Work Effort Association
     */
    @Entity(
        name = "WorkEffortAssoc",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association",
        fields = {
            @Field(name = "workEffortIdFrom", type = "id-ne"),
            @Field(name = "workEffortIdTo", type = "id-ne"),
            @Field(name = "workEffortAssocTypeId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortIdFrom"),
            @PrimaryKey(field = "workEffortIdTo"),
            @PrimaryKey(field = "workEffortAssocTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortAssocType",
                fkName = "WK_EFFRTASSC_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssocTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "From",
                fkName = "WK_EFFRTASSC_FWE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdFrom", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "To",
                fkName = "WK_EFFRTASSC_TWE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdTo", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAssocEntity {}

    /**
     * Work Effort Association Attribute
     */
    @Entity(
        name = "WorkEffortAssocAttribute",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association Attribute",
        fields = {
            @Field(name = "workEffortIdFrom", type = "id-ne"),
            @Field(name = "workEffortIdTo", type = "id-ne"),
            @Field(name = "workEffortAssocTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortIdFrom"),
            @PrimaryKey(field = "workEffortIdTo"),
            @PrimaryKey(field = "workEffortAssocTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortAssoc",
                fkName = "WK_EFFRTASSC_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdFrom"),
                    @KeyMap(fieldName = "workEffortIdTo"),
                    @KeyMap(fieldName = "workEffortAssocTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssocTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface WorkEffortAssocAttributeEntity {}

    /**
     * Work Effort Association Type
     */
    @Entity(
        name = "WorkEffortAssocType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association Type",
        defaultResourceName = "WorkEffortEntityLabels",
        fields = {
            @Field(name = "workEffortAssocTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortAssocTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortAssocType",
                title = "Parent",
                fkName = "WK_EFFRTASSC_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "workEffortAssocTypeId")
                }
            )
        }
    )
    public interface WorkEffortAssocTypeEntity {}

    /**
     * Work Effort Association Type Attribute
     */
    @Entity(
        name = "WorkEffortAssocTypeAttr",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association Type Attribute",
        defaultResourceName = "WorkEffortEntityLabels",
        fields = {
            @Field(name = "workEffortAssocTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortAssocTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortAssocType",
                fkName = "WK_EFFRTASSC_TATR",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssocAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortAssocTypeId")
                }
            )
        }
    )
    public interface WorkEffortAssocTypeAttrEntity {}

    /**
     * Work Effort Attribute
     */
    @Entity(
        name = "WorkEffortAttribute",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Attribute",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WK_EFFRT_ATTR_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface WorkEffortAttributeEntity {}

    /**
     * Work Effort Billing
     */
    @Entity(
        name = "WorkEffortBilling",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Billing",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "percentage", type = "floating-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WK_EFFBLNG_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "WK_EFFBLNG_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface WorkEffortBillingEntity {}

    /**
     * WorkEffort Contact Mechanism
     */
    @Entity(
        name = "WorkEffortContactMech",
        packageName = "org.ofbiz.workeffort.workeffort",
        tableName = "WORK_EFFORT_CONTACT_MECH_NEW",
        title = "WorkEffort Contact Mechanism",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_CMECH_WKEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "WKEFF_CMECH_CMECH",
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
    public interface WorkEffortContactMechEntity {}

    /**
     * WorkEffort Content
     */
    @Entity(
        name = "WorkEffortContent",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Content",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "workEffortContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "workEffortContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_CNT_WKEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "WKEFF_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortContentType",
                fkName = "WKEFF_CNT_WCTP",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortContentTypeId")
                }
            )
        }
    )
    public interface WorkEffortContentEntity {}

    /**
     * WorkEffort Content Type
     */
    @Entity(
        name = "WorkEffortContentType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Content Type",
        fields = {
            @Field(name = "workEffortContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortContentType",
                title = "Parent",
                fkName = "WEFFCTP_TP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "workEffortContentTypeId")
                }
            )
        }
    )
    public interface WorkEffortContentTypeEntity {}

    /**
     * Work Effort Deliverable Produced
     */
    @Entity(
        name = "WorkEffortDeliverableProd",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Deliverable Produced",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "deliverableId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "deliverableId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_DELPRD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Deliverable",
                fkName = "WKEFF_DELPRD_DEL",
                keyMaps = {
                    @KeyMap(fieldName = "deliverableId")
                }
            )
        }
    )
    public interface WorkEffortDeliverableProdEntity {}

    /**
     * Work Effort Event Reminder
     */
    @Entity(
        name = "WorkEffortEventReminder",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Event Reminder",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "sequenceId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "partyId", type = "id", description = "The party this reminder is assigned to"),
            @Field(name = "reminderDateTime", type = "date-time"),
            @Field(name = "repeatCount", type = "numeric"),
            @Field(name = "repeatInterval", type = "numeric", description = "The millisecond interval between reminder repeats"),
            @Field(name = "currentCount", type = "numeric"),
            @Field(name = "reminderOffset", type = "numeric", description = "The millisecond offset from the event to activate a reminder"),
            @Field(name = "localeId", type = "id"),
            @Field(name = "timeZoneId", type = "id-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "sequenceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WE_EVENT_REMIND_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "WE_EVENT_REMIND_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "WE_EVENT_REMIND_PY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface WorkEffortEventReminderEntity {}

    /**
     * Work Effort Fixed Asset Assignment
     */
    @Entity(
        name = "WorkEffortFixedAssetAssign",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Fixed Asset Assignment",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "availabilityStatusId", type = "id", description = "Points to StatusItem value with statusTypeId=\"WEFA_AVAILABILITY\""),
            @Field(name = "allocatedCost", type = "currency-amount"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_FXDAA_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "WKEFF_FXDAA_FXAS",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "WKEFF_FXDAA_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Availability",
                fkName = "WKEFF_FXDAA_AVAIL",
                keyMaps = {
                    @KeyMap(fieldName = "availabilityStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortFixedAssetAssignEntity {}

    /**
     * Work Effort Fixed Asset Standard
     */
    @Entity(
        name = "WorkEffortFixedAssetStd",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Fixed Asset Standard",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "fixedAssetTypeId", type = "id-ne"),
            @Field(name = "estimatedQuantity", type = "floating-point"),
            @Field(name = "estimatedDuration", type = "floating-point"),
            @Field(name = "estimatedCost", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "fixedAssetTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_FASTD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetType",
                fkName = "WKEFF_FASTD_FAT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            )
        }
    )
    public interface WorkEffortFixedAssetStdEntity {}

    /**
     * Work Effort Good Standard
     */
    @Entity(
        name = "WorkEffortGoodStandard",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Good Standard",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "workEffortGoodStdTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "estimatedQuantity", type = "floating-point"),
            @Field(name = "estimatedCost", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "workEffortGoodStdTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_GDSTD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortGoodStandardType",
                fkName = "WKEFF_GDSTD_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortGoodStdTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "WKEFF_GDSTD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "WKEFF_GDSTD_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortGoodStandardEntity {}

    /**
     * Work Effort Good Standard Type
     */
    @Entity(
        name = "WorkEffortGoodStandardType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Good Standard Type",
        fields = {
            @Field(name = "workEffortGoodStdTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortGoodStdTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortGoodStandardType",
                title = "Parent",
                fkName = "WKEFF_GDSTD_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "workEffortGoodStdTypeId")
                }
            )
        }
    )
    public interface WorkEffortGoodStandardTypeEntity {}

    /**
     * Work Effort iCalendar Data
     */
    @Entity(
        name = "WorkEffortIcalData",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort iCalendar Data",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "icalData", type = "very-long", description = "iCalender Data")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_ICAL_DATA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortIcalDataEntity {}

    /**
     * Work Effort Inventory Assignment
     */
    @Entity(
        name = "WorkEffortInventoryAssign",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Inventory Assignment",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "quantity", type = "floating-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_INVAS_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "WKEFF_INVAS_INVIT",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "WKEFF_INVAS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortInventoryAssignEntity {}

    /**
     * Work Effort Inventory Produced
     */
    @Entity(
        name = "WorkEffortInventoryProduced",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Inventory Produced",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_INVPD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "WKEFF_INVPD_INVIT",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface WorkEffortInventoryProducedEntity {}

    /**
     * Work Effort Cost Calculation
     */
    @Entity(
        name = "WorkEffortCostCalc",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Cost Calculation",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "costComponentTypeId", type = "id-ne"),
            @Field(name = "costComponentCalcId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "costComponentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WK_EFFRT_COS_WEF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentType",
                fkName = "WK_EFFRT_COS_CCT",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentCalc",
                fkName = "WK_EFFRT_COS_CCC",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentCalcId")
                }
            )
        }
    )
    public interface WorkEffortCostCalcEntity {}

    /**
     * WorkEffort Keyword
     */
    @Entity(
        name = "WorkEffortKeyword",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Keyword",
        neverCache = true,
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "keyword", type = "short-varchar"),
            @Field(name = "relevancyWeight", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "keyword")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WEFF_KWD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        },
        indexes = {
            @Index(
                name = "WEFF_KWD_KWD",
                fields = {
                    @IndexField(name = "keyword")
                }
            )
        }
    )
    public interface WorkEffortKeywordEntity {}

    /**
     * Work Effort Note
     */
    @Entity(
        name = "WorkEffortNote",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Note",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne"),
            @Field(name = "internalNote", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_NTE_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "WKEFF_NTE_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface WorkEffortNoteEntity {}

    /**
     * Work Effort Party Assignment
     */
    @Entity(
        name = "WorkEffortPartyAssignment",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Party Assignment",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "assignedByUserLoginId", type = "id-vlong"),
            @Field(name = "statusId", type = "id", description = "Point to StatusItem value with statusTypeId=\"PRTYASGN_STATUS\""),
            @Field(name = "statusDateTime", type = "date-time"),
            @Field(name = "expectationEnumId", type = "id", description = "Point to Enumeration value with enumTypeId=\"WORK_EFF_EXPECT\""),
            @Field(name = "delegateReasonEnumId", type = "id", description = "Point to Enumeration value with enumTypeId=\"WORK_EFF_DEL_REAS\""),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "mustRsvp", type = "indicator"),
            @Field(name = "availabilityStatusId", type = "id", description = "Points to StatusItem value with statusTypeId=\"WEPA_AVAILABILITY\"")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_PA_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "WKEFF_PA_PRTY_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "AssignedBy",
                fkName = "WKEFF_PA_ABUSRLOG",
                keyMaps = {
                    @KeyMap(fieldName = "assignedByUserLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Assignment",
                fkName = "WKEFF_PA_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Expectation",
                fkName = "WKEFF_PA_EXP_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "expectationEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "DelegateReason",
                fkName = "WKEFF_PA_DELR_ENM",
                keyMaps = {
                    @KeyMap(fieldName = "delegateReasonEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "WKEFF_PA_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Availability",
                fkName = "WKEFF_PA_AVSTTS",
                keyMaps = {
                    @KeyMap(fieldName = "availabilityStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortPartyAssignmentEntity {}

    /**
     * Work Effort Purpose Type
     */
    @Entity(
        name = "WorkEffortPurposeType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Purpose Type",
        defaultResourceName = "WorkEffortEntityLabels",
        fields = {
            @Field(name = "workEffortPurposeTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortPurposeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortPurposeType",
                title = "Parent",
                fkName = "WK_EFFRT_PTYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "workEffortPurposeTypeId")
                }
            )
        }
    )
    public interface WorkEffortPurposeTypeEntity {}

    /**
     * WorkEffort Review
     */
    @Entity(
        name = "WorkEffortReview",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Review",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "reviewDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "postedAnonymous", type = "indicator"),
            @Field(name = "rating", type = "floating-point"),
            @Field(name = "reviewText", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "reviewDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WEFF_REVIEW_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "WEFF_REVIEW_UL",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "WEFF_REVIEW_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortReviewEntity {}

    /**
     * WorkEffort Search Result Constraint
     */
    @Entity(
        name = "WorkEffortSearchConstraint",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Search Result Constraint",
        neverCache = true,
        fields = {
            @Field(name = "workEffortSearchResultId", type = "id-ne"),
            @Field(name = "constraintSeqId", type = "id-ne"),
            @Field(name = "constraintName", type = "long-varchar"),
            @Field(name = "infoString", type = "long-varchar"),
            @Field(name = "includeSubWorkEfforts", type = "indicator"),
            @Field(name = "isAnd", type = "indicator"),
            @Field(name = "anyPrefix", type = "indicator"),
            @Field(name = "anySuffix", type = "indicator"),
            @Field(name = "removeStems", type = "indicator"),
            @Field(name = "lowValue", type = "short-varchar"),
            @Field(name = "highValue", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortSearchResultId"),
            @PrimaryKey(field = "constraintSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortSearchResult",
                fkName = "WEFF_SCHRSI_RES",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortSearchResultId")
                }
            )
        }
    )
    public interface WorkEffortSearchConstraintEntity {}

    /**
     * WorkEffort Search Result
     */
    @Entity(
        name = "WorkEffortSearchResult",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Search Result",
        neverCache = true,
        fields = {
            @Field(name = "workEffortSearchResultId", type = "id-ne"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "orderByName", type = "long-varchar"),
            @Field(name = "isAscending", type = "indicator"),
            @Field(name = "numResults", type = "numeric"),
            @Field(name = "secondsTotal", type = "floating-point"),
            @Field(name = "searchDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortSearchResultId")
        }
    )
    public interface WorkEffortSearchResultEntity {}

    /**
     * Work Effort Skill Standard
     */
    @Entity(
        name = "WorkEffortSkillStandard",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Skill Standard",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "skillTypeId", type = "id-ne"),
            @Field(name = "estimatedNumPeople", type = "floating-point"),
            @Field(name = "estimatedDuration", type = "floating-point"),
            @Field(name = "estimatedCost", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "skillTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_SKLSTD_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                fkName = "WKEFF_SKLSTD_SKTP",
                keyMaps = {
                    @KeyMap(fieldName = "skillTypeId")
                }
            )
        }
    )
    public interface WorkEffortSkillStandardEntity {}

    /**
     * Work Effort Status
     */
    @Entity(
        name = "WorkEffortStatus",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Status",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusDatetime", type = "date-time"),
            @Field(name = "setByUserLogin", type = "id-vlong"),
            @Field(name = "reason", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "statusDatetime")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_STTS_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "WKEFF_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "SetBy",
                fkName = "WKEFF_STTS_SB_UL",
                keyMaps = {
                    @KeyMap(fieldName = "setByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface WorkEffortStatusEntity {}

    /**
     * Work Effort Transition Box
     */
    @Entity(
        name = "WorkEffortTransBox",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Transition Box",
        fields = {
            @Field(name = "processWorkEffortId", type = "id-ne"),
            @Field(name = "toActivityId", type = "id-long-ne"),
            @Field(name = "transitionId", type = "id-long-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "processWorkEffortId"),
            @PrimaryKey(field = "toActivityId"),
            @PrimaryKey(field = "transitionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEFF_TXBX_WE",
                keyMaps = {
                    @KeyMap(fieldName = "processWorkEffortId", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortTransBoxEntity {}

    /**
     * Work Effort Type
     */
    @Entity(
        name = "WorkEffortType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Type",
        defaultResourceName = "WorkEffortEntityLabels",
        fields = {
            @Field(name = "workEffortTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortType",
                title = "Parent",
                fkName = "WK_EFFRT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "workEffortTypeId")
                }
            )
        }
    )
    public interface WorkEffortTypeEntity {}

    /**
     * Work Effort Type Attribute
     */
    @Entity(
        name = "WorkEffortTypeAttr",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Type Attribute",
        fields = {
            @Field(name = "workEffortTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffortType",
                fkName = "WK_EFFRT_TYPE_ATR",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            )
        }
    )
    public interface WorkEffortTypeAttrEntity {}

    /**
     * Work Effort Survey Appl
     */
    @Entity(
        name = "WorkEffortSurveyAppl",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Survey Appl",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "WKEF_SURVAPL_SVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WKEF_SURVAPL_WKE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreSurveyAppl",
                fkName = "WKEF_SURVAPL_PSSA",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId", relFieldName = "productStoreSurveyId")
                }
            )
        }
    )
    public interface WorkEffortSurveyApplEntity {}

}
