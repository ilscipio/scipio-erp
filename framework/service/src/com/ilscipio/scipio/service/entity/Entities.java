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
package com.ilscipio.scipio.service.entity;

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
     * Job Scheduler Sandbox
     */
    @Entity(
        name = "JobSandbox",
        packageName = "org.ofbiz.service.schedule",
        title = "Job Scheduler Sandbox",
        sequenceBankSize = 100,
        neverCache = true,
        fields = {
            @Field(name = "jobId", type = "id-ne"),
            @Field(name = "jobName", type = "name"),
            @Field(name = "runTime", type = "date-time"),
            @Field(name = "priority", type = "numeric"),
            @Field(name = "poolId", type = "name"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "parentJobId", type = "id"),
            @Field(name = "previousJobId", type = "id"),
            @Field(name = "serviceName", type = "name"),
            @Field(name = "loaderName", type = "name"),
            @Field(name = "maxRetry", type = "numeric"),
            @Field(name = "currentRetryCount", type = "numeric"),
            @Field(name = "authUserLoginId", type = "id-vlong"),
            @Field(name = "runAsUser", type = "id-vlong"),
            @Field(name = "runtimeDataId", type = "id"),
            @Field(name = "recurrenceInfoId", type = "id", description = "Deprecated - use tempExprId instead"),
            @Field(name = "tempExprId", type = "id", description = "Temporal expression id"),
            @Field(name = "currentRecurrenceCount", type = "numeric"),
            @Field(name = "maxRecurrenceCount", type = "numeric"),
            @Field(name = "runByInstanceId", type = "id"),
            @Field(name = "startDateTime", type = "date-time"),
            @Field(name = "finishDateTime", type = "date-time"),
            @Field(name = "cancelDateTime", type = "date-time"),
            @Field(name = "jobResult", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "jobId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceInfo",
                fkName = "JOB_SNDBX_RECINFO",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TemporalExpression",
                fkName = "JOB_SNDBX_TEMPEXPR",
                keyMaps = {
                    @KeyMap(fieldName = "tempExprId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RuntimeData",
                fkName = "JOB_SNDBX_RNTMDTA",
                keyMaps = {
                    @KeyMap(fieldName = "runtimeDataId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "Auth",
                fkName = "JOB_SNDBX_AUSRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "authUserLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "RunAs",
                fkName = "JOB_SNDBX_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "runAsUser", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "JOB_SNDBX_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        indexes = {
            @Index(
                name = "JOB_SNDBX_RUNSTAT",
                fields = {
                    @IndexField(name = "runByInstanceId"),
                    @IndexField(name = "statusId")
                }
            )
        }
    )
    public interface JobSandboxEntity {}

    /**
     * Recurrence Info
     */
    @Entity(
        name = "RecurrenceInfo",
        packageName = "org.ofbiz.service.schedule",
        title = "Recurrence Info",
        fields = {
            @Field(name = "recurrenceInfoId", type = "id-ne"),
            @Field(name = "startDateTime", type = "date-time"),
            @Field(name = "exceptionDateTimes", type = "very-long"),
            @Field(name = "recurrenceDateTimes", type = "very-long"),
            @Field(name = "exceptionRuleId", type = "id-ne"),
            @Field(name = "recurrenceRuleId", type = "id-ne"),
            @Field(name = "recurrenceCount", type = "numeric", description = "Not recommended - more than one process could be using this RecurrenceInfo")
        },
        primaryKeys = {
            @PrimaryKey(field = "recurrenceInfoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceRule",
                fkName = "REC_INFO_RCRLE",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceRule",
                title = "Exception",
                fkName = "REC_INFO_EX_RCRLE",
                keyMaps = {
                    @KeyMap(fieldName = "exceptionRuleId", relFieldName = "recurrenceRuleId")
                }
            )
        }
    )
    public interface RecurrenceInfoEntity {}

    /**
     * Recurrence Rule
     */
    @Entity(
        name = "RecurrenceRule",
        packageName = "org.ofbiz.service.schedule",
        title = "Recurrence Rule",
        fields = {
            @Field(name = "recurrenceRuleId", type = "id-ne"),
            @Field(name = "frequency", type = "short-varchar"),
            @Field(name = "untilDateTime", type = "date-time"),
            @Field(name = "countNumber", type = "numeric"),
            @Field(name = "intervalNumber", type = "numeric"),
            @Field(name = "bySecondList", type = "very-long"),
            @Field(name = "byMinuteList", type = "very-long"),
            @Field(name = "byHourList", type = "very-long"),
            @Field(name = "byDayList", type = "very-long"),
            @Field(name = "byMonthDayList", type = "very-long"),
            @Field(name = "byYearDayList", type = "very-long"),
            @Field(name = "byWeekNoList", type = "very-long"),
            @Field(name = "byMonthList", type = "very-long"),
            @Field(name = "bySetPosList", type = "very-long"),
            @Field(name = "weekStart", type = "short-varchar"),
            @Field(name = "xName", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "recurrenceRuleId")
        }
    )
    public interface RecurrenceRuleEntity {}

    /**
     * Runtime Data
     */
    @Entity(
        name = "RuntimeData",
        packageName = "org.ofbiz.service.schedule",
        title = "Runtime Data",
        sequenceBankSize = 100,
        fields = {
            @Field(name = "runtimeDataId", type = "id-ne"),
            @Field(name = "runtimeInfo", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "runtimeDataId")
        }
    )
    public interface RuntimeDataEntity {}

    /**
     * Temporal Expression
     */
    @Entity(
        name = "TemporalExpression",
        packageName = "org.ofbiz.service.schedule",
        title = "Temporal Expression",
        fields = {
            @Field(name = "tempExprId", type = "id-ne"),
            @Field(name = "tempExprTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "date1", type = "date-time"),
            @Field(name = "date2", type = "date-time"),
            @Field(name = "integer1", type = "numeric"),
            @Field(name = "integer2", type = "numeric"),
            @Field(name = "string1", type = "id"),
            @Field(name = "string2", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "tempExprId")
        }
    )
    public interface TemporalExpressionEntity {}

    /**
     * Temporal Expression Association
     */
    @Entity(
        name = "TemporalExpressionAssoc",
        packageName = "org.ofbiz.service.schedule",
        title = "Temporal Expression Association",
        fields = {
            @Field(name = "fromTempExprId", type = "id-ne", description = "The \"parent\" expression"),
            @Field(name = "toTempExprId", type = "id-ne", description = "The \"child\" expression"),
            @Field(name = "exprAssocType", type = "id", description = "Expression association type.\n         When applied to DIFFERENCE expression types, valid values are INCLUDE or EXCLUDE.\n         When applied to SUBSTITUTION expression types, valid values are INCLUDE, EXCLUDE, or SUBSTITUTE.\n         ")
        },
        primaryKeys = {
            @PrimaryKey(field = "fromTempExprId"),
            @PrimaryKey(field = "toTempExprId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TemporalExpression",
                title = "From",
                fkName = "TEMP_EXPR_FROM",
                keyMaps = {
                    @KeyMap(fieldName = "fromTempExprId", relFieldName = "tempExprId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TemporalExpression",
                title = "To",
                fkName = "TEMP_EXPR_TO",
                keyMaps = {
                    @KeyMap(fieldName = "toTempExprId", relFieldName = "tempExprId")
                }
            )
        }
    )
    public interface TemporalExpressionAssocEntity {}

    /**
     * Lock Job Manager Scheduler
     */
    @Entity(
        name = "JobManagerLock",
        packageName = "org.ofbiz.service.schedule",
        title = "Lock Job Manager Scheduler",
        fields = {
            @Field(name = "instanceId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "instanceId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Reason",
                fkName = "JOBLK_ENUM_REAS",
                keyMaps = {
                    @KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface JobManagerLockEntity {}

    /**
     * Semaphore Lock
     */
    @Entity(
        name = "ServiceSemaphore",
        packageName = "org.ofbiz.service.semaphore",
        title = "Semaphore Lock",
        sequenceBankSize = 100,
        fields = {
            @Field(name = "serviceName", type = "name"),
            @Field(name = "lockedByInstanceId", type = "id"),
            @Field(name = "lockThread", type = "name"),
            @Field(name = "lockTime", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "serviceName")
        }
    )
    public interface ServiceSemaphoreEntity {}

    /**
     * Temporal Expression Children View
     */
    @ViewEntity(
        name = "TemporalExpressionChild",
        packageName = "org.ofbiz.service.schedule",
        title = "Temporal Expression Children View",
        members = {
            @MemberEntity(entityAlias = "TEA", entityName = "TemporalExpressionAssoc"),
            @MemberEntity(entityAlias = "TE", entityName = "TemporalExpression")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TEA", excludes = {"toTempExprId"}),
            @AliasAll(entityAlias = "TE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TEA",
                relEntityAlias = "TE",
                keyMaps = {
                    @KeyMap(fieldName = "toTempExprId", relFieldName = "tempExprId")
                }
            )
        }
    )
    public interface TemporalExpressionChildView {}

    @ExtendEntity(
        name = "JobSandbox",
        fields = {
            @Field(name = "eventId", type = "id", description = "SCIPIO: Identifies the event at which the job should be triggered, or in other words\n                the event which will limit when the job can be run.")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Event",
                fkName = "JOB_SNDBX_EVENT",
                keyMaps = {
                    @KeyMap(fieldName = "eventId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface JobSandboxExtension {}

}
