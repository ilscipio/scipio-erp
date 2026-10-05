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
package com.ilscipio.scipio.humanres.entity;

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
     * Party Qualification
     */
    @Entity(
        name = "PartyQual",
        packageName = "org.ofbiz.humanres.ability",
        title = "Party Qualification",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "partyQualTypeId", type = "id-ne"),
            @Field(name = "qualificationDesc", type = "id-long"),
            @Field(name = "title", type = "id-long", description = "Title of degree or job"),
            @Field(name = "statusId", type = "id", description = "Status e.g. completed, part-time etc."),
            @Field(name = "verifStatusId", type = "id", description = "Verification done for this entry if any"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "partyQualTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_QUAL_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyQualType",
                fkName = "PARTY_QUAL_PQTYP",
                keyMaps = {
                    @KeyMap(fieldName = "partyQualTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PARTY_QUAL_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Verification",
                fkName = "PARTY_QUAL_VERIF",
                keyMaps = {
                    @KeyMap(fieldName = "verifStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface PartyQualEntity {}

    /**
     * Party Qualification Type
     */
    @Entity(
        name = "PartyQualType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Party Qualification Type",
        fields = {
            @Field(name = "partyQualTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyQualTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyQualType",
                title = "Parent",
                fkName = "PARTY_QUAL_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyQualTypeId")
                }
            )
        }
    )
    public interface PartyQualTypeEntity {}

    /**
     * Resume
     */
    @Entity(
        name = "PartyResume",
        packageName = "org.ofbiz.humanres.ability",
        title = "Resume",
        fields = {
            @Field(name = "resumeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "contentId", type = "id"),
            @Field(name = "resumeDate", type = "date-time"),
            @Field(name = "resumeText", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "resumeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_RSME_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Content",
                fkName = "PARTY_RSME_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface PartyResumeEntity {}

    /**
     * Party Skill
     */
    @Entity(
        name = "PartySkill",
        packageName = "org.ofbiz.humanres.ability",
        title = "Party Skill",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "skillTypeId", type = "id-ne"),
            @Field(name = "yearsExperience", type = "numeric"),
            @Field(name = "rating", type = "numeric"),
            @Field(name = "skillLevel", type = "numeric"),
            @Field(name = "startedUsingDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "skillTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_SKLL_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                fkName = "PARTY_SKLL_SKTP",
                keyMaps = {
                    @KeyMap(fieldName = "skillTypeId")
                }
            )
        }
    )
    public interface PartySkillEntity {}

    /**
     * Performance Rating Type
     */
    @Entity(
        name = "PerfRatingType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Performance Rating Type",
        fields = {
            @Field(name = "perfRatingTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "perfRatingTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PerfRatingType",
                title = "Parent",
                fkName = "PERF_RATNG_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "perfRatingTypeId")
                }
            )
        }
    )
    public interface PerfRatingTypeEntity {}

    /**
     * Employee Performance Review
     */
    @Entity(
        name = "PerfReview",
        packageName = "org.ofbiz.humanres.ability",
        title = "Employee Performance Review",
        fields = {
            @Field(name = "employeePartyId", type = "id-ne"),
            @Field(name = "employeeRoleTypeId", type = "id-ne"),
            @Field(name = "perfReviewId", type = "id-ne"),
            @Field(name = "managerPartyId", type = "id"),
            @Field(name = "managerRoleTypeId", type = "id"),
            @Field(name = "paymentId", type = "id"),
            @Field(name = "emplPositionId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "employeePartyId"),
            @PrimaryKey(field = "employeeRoleTypeId"),
            @PrimaryKey(field = "perfReviewId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Employee",
                fkName = "PERF_REV_EPTY",
                keyMaps = {
                    @KeyMap(fieldName = "employeePartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "Employee",
                fkName = "PERF_REV_EPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "employeePartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "employeeRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Manager",
                fkName = "PERF_REV_MPTY",
                keyMaps = {
                    @KeyMap(fieldName = "managerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "Manager",
                fkName = "PERF_REV_MPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "managerPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "managerRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PERF_REV_PMNT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmplPosition",
                fkName = "PERF_REV_PSTN",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            )
        }
    )
    public interface PerfReviewEntity {}

    /**
     * Performance Review Item
     */
    @Entity(
        name = "PerfReviewItem",
        packageName = "org.ofbiz.humanres.ability",
        title = "Performance Review Item",
        fields = {
            @Field(name = "employeePartyId", type = "id-ne"),
            @Field(name = "employeeRoleTypeId", type = "id"),
            @Field(name = "perfReviewId", type = "id-ne"),
            @Field(name = "perfReviewItemSeqId", type = "id-ne"),
            @Field(name = "perfReviewItemTypeId", type = "id"),
            @Field(name = "perfRatingTypeId", type = "id"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "employeePartyId"),
            @PrimaryKey(field = "employeeRoleTypeId"),
            @PrimaryKey(field = "perfReviewId"),
            @PrimaryKey(field = "perfReviewItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PerfReview",
                fkName = "PERF_RVITM_PFRV",
                keyMaps = {
                    @KeyMap(fieldName = "employeePartyId"),
                    @KeyMap(fieldName = "employeeRoleTypeId"),
                    @KeyMap(fieldName = "perfReviewId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Employee",
                fkName = "PERF_RVITM_EPTY",
                keyMaps = {
                    @KeyMap(fieldName = "employeePartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "Employee",
                fkName = "PERF_RVITM_EPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "employeePartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "employeeRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PerfRatingType",
                fkName = "PERF_RVITM_PRTTP",
                keyMaps = {
                    @KeyMap(fieldName = "perfRatingTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PerfReviewItemType",
                fkName = "PERF_RVITM_PRITTP",
                keyMaps = {
                    @KeyMap(fieldName = "perfReviewItemTypeId")
                }
            )
        }
    )
    public interface PerfReviewItemEntity {}

    /**
     * Performance Review Item Type
     */
    @Entity(
        name = "PerfReviewItemType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Performance Review Item Type",
        fields = {
            @Field(name = "perfReviewItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "perfReviewItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PerfReviewItemType",
                title = "Parent",
                fkName = "PERF_REV_ITM_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "perfReviewItemTypeId")
                }
            )
        }
    )
    public interface PerfReviewItemTypeEntity {}

    /**
     * Performance Note
     */
    @Entity(
        name = "PerformanceNote",
        packageName = "org.ofbiz.humanres.ability",
        title = "Performance Note",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "communicationDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PERF_NOTE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "PERF_NOTE_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface PerformanceNoteEntity {}

    /**
     * Person Training
     */
    @Entity(
        name = "PersonTraining",
        packageName = "org.ofbiz.humanres.ability",
        title = "Person Training",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "trainingRequestId", type = "id-ne"),
            @Field(name = "trainingClassTypeId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "approverId", type = "id-ne"),
            @Field(name = "approvalStatus", type = "short-varchar"),
            @Field(name = "reason", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "trainingClassTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PERS_TRNG_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Person",
                title = "Approver",
                fkName = "PERS_TRNG_APPR",
                keyMaps = {
                    @KeyMap(fieldName = "approverId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrainingClassType",
                fkName = "PERS_TRNG_TCTP",
                keyMaps = {
                    @KeyMap(fieldName = "trainingClassTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "PERS_TRNG_WREF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrainingRequest",
                fkName = "PERS_TRNG_TRNRQ",
                keyMaps = {
                    @KeyMap(fieldName = "trainingRequestId")
                }
            )
        }
    )
    public interface PersonTrainingEntity {}

    /**
     * Responsibility Type
     */
    @Entity(
        name = "ResponsibilityType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Responsibility Type",
        fields = {
            @Field(name = "responsibilityTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "responsibilityTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ResponsibilityType",
                title = "Parent",
                fkName = "RESPON_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "responsibilityTypeId")
                }
            )
        }
    )
    public interface ResponsibilityTypeEntity {}

    /**
     * Skill Type
     */
    @Entity(
        name = "SkillType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Skill Type",
        fields = {
            @Field(name = "skillTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "skillTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                title = "Parent",
                fkName = "PARNT_SKILL_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "skillTypeId")
                }
            )
        }
    )
    public interface SkillTypeEntity {}

    /**
     * Training Class Type
     */
    @Entity(
        name = "TrainingClassType",
        packageName = "org.ofbiz.humanres.ability",
        title = "Training Class Type",
        fields = {
            @Field(name = "trainingClassTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "trainingClassTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrainingClassType",
                title = "Parent",
                fkName = "TRAIN_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "trainingClassTypeId")
                }
            )
        }
    )
    public interface TrainingClassTypeEntity {}

    /**
     * Benefit Type
     */
    @Entity(
        name = "BenefitType",
        packageName = "org.ofbiz.humanres.employment",
        title = "Benefit Type",
        fields = {
            @Field(name = "benefitTypeId", type = "id-ne"),
            @Field(name = "benefitName", type = "name"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "employerPaidPercentage", type = "floating-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "benefitTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BenefitType",
                title = "Parent",
                fkName = "BEN_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "benefitTypeId")
                }
            )
        }
    )
    public interface BenefitTypeEntity {}

    /**
     * Employment
     */
    @Entity(
        name = "Employment",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employment",
        fields = {
            @Field(name = "roleTypeIdFrom", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "terminationReasonId", type = "id"),
            @Field(name = "terminationTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeIdFrom"),
            @PrimaryKey(field = "roleTypeIdTo"),
            @PrimaryKey(field = "partyIdFrom"),
            @PrimaryKey(field = "partyIdTo"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "EMPLMNT_TPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "To",
                fkName = "EMPLMNT_TPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "EMPLMNT_FPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "From",
                fkName = "EMPLMNT_FPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TerminationReason",
                fkName = "EMPLMNT_TNRN",
                keyMaps = {
                    @KeyMap(fieldName = "terminationReasonId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TerminationType",
                fkName = "EMPLMNT_TNTP",
                keyMaps = {
                    @KeyMap(fieldName = "terminationTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Agreement",
                fkName = "EMPLMNT_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "agreementId"),
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "agreementId")
                }
            )
        }
    )
    public interface EmploymentEntity {}

    /**
     * Employment Application
     */
    @Entity(
        name = "EmploymentApp",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employment Application",
        fields = {
            @Field(name = "applicationId", type = "id-ne"),
            @Field(name = "emplPositionId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "employmentAppSourceTypeId", type = "id"),
            @Field(name = "applyingPartyId", type = "id"),
            @Field(name = "referredByPartyId", type = "id"),
            @Field(name = "applicationDate", type = "date-time"),
            @Field(name = "approverPartyId", type = "id"),
            @Field(name = "jobRequisitionId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "applicationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmplPosition",
                fkName = "EMPLMNT_APP_POS",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                fkName = "EMPLMNT_APP_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmploymentAppSourceType",
                fkName = "EMPLMNT_APP_EAST",
                keyMaps = {
                    @KeyMap(fieldName = "employmentAppSourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "Applying",
                fkName = "EMPLMNT_APP_APTY",
                keyMaps = {
                    @KeyMap(fieldName = "applyingPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "ReferredBy",
                fkName = "EMPLMNT_APP_RBPTY",
                keyMaps = {
                    @KeyMap(fieldName = "referredByPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Approver",
                fkName = "EMPLMNT_APP_APER",
                keyMaps = {
                    @KeyMap(fieldName = "approverPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "JobRequisition",
                fkName = "EMPLMNT_APP_JBRQ",
                keyMaps = {
                    @KeyMap(fieldName = "jobRequisitionId")
                }
            )
        }
    )
    public interface EmploymentAppEntity {}

    /**
     * Employment Application Source Type
     */
    @Entity(
        name = "EmploymentAppSourceType",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employment Application Source Type",
        fields = {
            @Field(name = "employmentAppSourceTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "employmentAppSourceTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmploymentAppSourceType",
                title = "Parent",
                fkName = "EMPL_APP_SRC_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "employmentAppSourceTypeId")
                }
            )
        }
    )
    public interface EmploymentAppSourceTypeEntity {}

    /**
     * Employee Leave
     */
    @Entity(
        name = "EmplLeave",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employee Leave",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "leaveTypeId", type = "id-ne"),
            @Field(name = "emplLeaveReasonTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "approverPartyId", type = "id-ne"),
            @Field(name = "leaveStatus", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "leaveTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "EMPL_LEAVE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplLeaveType",
                fkName = "EMPL_LEAVE_ELETP",
                keyMaps = {
                    @KeyMap(fieldName = "leaveTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplLeaveReasonType",
                fkName = "EMP_LEAV_REAS_ELTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplLeaveReasonTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Approver",
                fkName = "EMPL_LEAVE_APPR",
                keyMaps = {
                    @KeyMap(fieldName = "approverPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "EMPL_LEAVE_STS",
                keyMaps = {
                    @KeyMap(fieldName = "leaveStatus", relFieldName = "statusId")
                }
            )
        }
    )
    public interface EmplLeaveEntity {}

    /**
     * Employee Leave Type
     */
    @Entity(
        name = "EmplLeaveType",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employee Leave Type",
        defaultResourceName = "HumanResEntityLabels",
        fields = {
            @Field(name = "leaveTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "leaveTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplLeaveType",
                title = "Parent",
                fkName = "EMPL_LEAVE_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "leaveTypeId")
                }
            )
        }
    )
    public interface EmplLeaveTypeEntity {}

    /**
     * Party Benefit
     */
    @Entity(
        name = "PartyBenefit",
        packageName = "org.ofbiz.humanres.employment",
        title = "Party Benefit",
        fields = {
            @Field(name = "roleTypeIdFrom", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "benefitTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "periodTypeId", type = "id"),
            @Field(name = "cost", type = "currency-amount"),
            @Field(name = "actualEmployerPaidPercent", type = "floating-point"),
            @Field(name = "availableTime", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeIdFrom"),
            @PrimaryKey(field = "roleTypeIdTo"),
            @PrimaryKey(field = "partyIdFrom"),
            @PrimaryKey(field = "partyIdTo"),
            @PrimaryKey(field = "benefitTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "PTY_BNFT_TPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "To",
                fkName = "PTY_BNFT_TPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "PTY_BNFT_FPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "From",
                fkName = "PTY_BNFT_FPTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BenefitType",
                fkName = "PTY_BNFT_BNFTTP",
                keyMaps = {
                    @KeyMap(fieldName = "benefitTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PeriodType",
                fkName = "PTY_BNFT_PRDTYP",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            )
        }
    )
    public interface PartyBenefitEntity {}

    /**
     * Pay Grade
     */
    @Entity(
        name = "PayGrade",
        packageName = "org.ofbiz.humanres.employment",
        title = "Pay Grade",
        fields = {
            @Field(name = "payGradeId", type = "id-ne"),
            @Field(name = "payGradeName", type = "name"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "payGradeId")
        }
    )
    public interface PayGradeEntity {}

    /**
     * Pay History
     */
    @Entity(
        name = "PayHistory",
        packageName = "org.ofbiz.humanres.employment",
        title = "Pay History",
        fields = {
            @Field(name = "roleTypeIdFrom", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "salaryStepSeqId", type = "id"),
            @Field(name = "payGradeId", type = "id"),
            @Field(name = "periodTypeId", type = "id"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeIdFrom"),
            @PrimaryKey(field = "roleTypeIdTo"),
            @PrimaryKey(field = "partyIdFrom"),
            @PrimaryKey(field = "partyIdTo"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Employment",
                fkName = "PAY_HIST_EMPLMNT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom"),
                    @KeyMap(fieldName = "roleTypeIdTo"),
                    @KeyMap(fieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "partyIdTo"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PayGrade",
                fkName = "PAY_HIST_PGRD",
                keyMaps = {
                    @KeyMap(fieldName = "payGradeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SalaryStep",
                fkName = "PAY_HIST_SSTP",
                keyMaps = {
                    @KeyMap(fieldName = "salaryStepSeqId"),
                    @KeyMap(fieldName = "payGradeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PeriodType",
                fkName = "PAY_HIST_PDTP",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            )
        }
    )
    public interface PayHistoryEntity {}

    /**
     * Payroll Preference
     */
    @Entity(
        name = "PayrollPreference",
        packageName = "org.ofbiz.humanres.employment",
        title = "Payroll Preference",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "payrollPreferenceSeqId", type = "id-ne"),
            @Field(name = "deductionTypeId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id"),
            @Field(name = "periodTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "percentage", type = "floating-point"),
            @Field(name = "flatAmount", type = "currency-amount"),
            @Field(name = "routingNumber", type = "short-varchar"),
            @Field(name = "accountNumber", type = "short-varchar"),
            @Field(name = "bankName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "payrollPreferenceSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PRL_PREF_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "PRL_PREF_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DeductionType",
                fkName = "PRL_PREF_DNTP",
                keyMaps = {
                    @KeyMap(fieldName = "deductionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                fkName = "PRL_PREF_PMTP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PeriodType",
                fkName = "PRL_PREF_PDTP",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            )
        }
    )
    public interface PayrollPreferenceEntity {}

    /**
     * Salary Step
     */
    @Entity(
        name = "SalaryStep",
        packageName = "org.ofbiz.humanres.employment",
        title = "Salary Step",
        fields = {
            @Field(name = "salaryStepSeqId", type = "id-ne"),
            @Field(name = "payGradeId", type = "id-ne"),
            @Field(name = "dateModified", type = "date-time"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "salaryStepSeqId"),
            @PrimaryKey(field = "payGradeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PayGrade",
                fkName = "SLRY_STP_PGRD",
                keyMaps = {
                    @KeyMap(fieldName = "payGradeId")
                }
            )
        }
    )
    public interface SalaryStepEntity {}

    /**
     * Termination Reason
     */
    @Entity(
        name = "TerminationReason",
        packageName = "org.ofbiz.humanres.employment",
        title = "Termination Reason",
        fields = {
            @Field(name = "terminationReasonId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "terminationReasonId")
        }
    )
    public interface TerminationReasonEntity {}

    /**
     * Termination Type
     */
    @Entity(
        name = "TerminationType",
        packageName = "org.ofbiz.humanres.employment",
        title = "Termination Type",
        fields = {
            @Field(name = "terminationTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "terminationTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TerminationType",
                title = "Parent",
                fkName = "TERM_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "terminationTypeId")
                }
            )
        }
    )
    public interface TerminationTypeEntity {}

    /**
     * Unemployment Claim
     */
    @Entity(
        name = "UnemploymentClaim",
        packageName = "org.ofbiz.humanres.employment",
        title = "Unemployment Claim",
        fields = {
            @Field(name = "unemploymentClaimId", type = "id-ne"),
            @Field(name = "unemploymentClaimDate", type = "date-time"),
            @Field(name = "description", type = "description"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "partyIdTo", type = "id"),
            @Field(name = "roleTypeIdFrom", type = "id"),
            @Field(name = "roleTypeIdTo", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "unemploymentClaimId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Employment",
                fkName = "UNMPL_CLM_EMPLMNT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom"),
                    @KeyMap(fieldName = "roleTypeIdTo"),
                    @KeyMap(fieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "partyIdTo"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                fkName = "UNMPL_CLM_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface UnemploymentClaimEntity {}

    /**
     * EmplPosition
     */
    @Entity(
        name = "EmplPosition",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition",
        fields = {
            @Field(name = "emplPositionId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "budgetId", type = "id"),
            @Field(name = "budgetItemSeqId", type = "id"),
            @Field(name = "emplPositionTypeId", type = "id"),
            @Field(name = "estimatedFromDate", type = "date-time"),
            @Field(name = "estimatedThruDate", type = "date-time"),
            @Field(name = "salaryFlag", type = "indicator"),
            @Field(name = "exemptFlag", type = "indicator"),
            @Field(name = "fulltimeFlag", type = "indicator"),
            @Field(name = "temporaryFlag", type = "indicator"),
            @Field(name = "actualFromDate", type = "date-time"),
            @Field(name = "actualThruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "EMPL_POS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "EMPL_POS_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BudgetItem",
                fkName = "EMPL_POS_BGTITM",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmplPositionType",
                fkName = "EMPL_POS_EPSTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            )
        }
    )
    public interface EmplPositionEntity {}

    /**
     * EmplPosition Classification Type
     */
    @Entity(
        name = "EmplPositionClassType",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Classification Type",
        fields = {
            @Field(name = "emplPositionClassTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionClassTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionClassType",
                title = "Parent",
                fkName = "EMPL_CLS_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "emplPositionClassTypeId")
                }
            )
        }
    )
    public interface EmplPositionClassTypeEntity {}

    /**
     * EmplPosition Fulfillment
     */
    @Entity(
        name = "EmplPositionFulfillment",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Fulfillment",
        fields = {
            @Field(name = "emplPositionId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPosition",
                fkName = "EMPL_PSFLMT_EMPS",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "EMPL_PSFLMT_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface EmplPositionFulfillmentEntity {}

    /**
     * EmplPosition Reporting Structure
     */
    @Entity(
        name = "EmplPositionReportingStruct",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Reporting Structure",
        fields = {
            @Field(name = "emplPositionIdReportingTo", type = "id-ne"),
            @Field(name = "emplPositionIdManagedBy", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "primaryFlag", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionIdReportingTo"),
            @PrimaryKey(field = "emplPositionIdManagedBy"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPosition",
                title = "ReportingTo",
                fkName = "EMPL_PSRPS_EMPSR",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionIdReportingTo", relFieldName = "emplPositionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPosition",
                title = "ManagedBy",
                fkName = "EMPL_PSRPS_EMPSM",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionIdManagedBy", relFieldName = "emplPositionId")
                }
            )
        }
    )
    public interface EmplPositionReportingStructEntity {}

    /**
     * EmplPosition Responsibility
     */
    @Entity(
        name = "EmplPositionResponsibility",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Responsibility",
        fields = {
            @Field(name = "emplPositionId", type = "id-ne"),
            @Field(name = "responsibilityTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionId"),
            @PrimaryKey(field = "responsibilityTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPosition",
                fkName = "EMPL_PSRTY_EMPS",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ResponsibilityType",
                fkName = "EMPL_PSRTY_RYTP",
                keyMaps = {
                    @KeyMap(fieldName = "responsibilityTypeId")
                }
            )
        }
    )
    public interface EmplPositionResponsibilityEntity {}

    /**
     * EmplPosition Type
     */
    @Entity(
        name = "EmplPositionType",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Type",
        fields = {
            @Field(name = "emplPositionTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                title = "Parent",
                fkName = "EMPL_POSI_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "emplPositionTypeId")
                }
            )
        }
    )
    public interface EmplPositionTypeEntity {}

    /**
     * EmplPosition Type Class
     */
    @Entity(
        name = "EmplPositionTypeClass",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Type Class",
        fields = {
            @Field(name = "emplPositionTypeId", type = "id-ne"),
            @Field(name = "emplPositionClassTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "standardHoursPerWeek", type = "floating-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionTypeId"),
            @PrimaryKey(field = "emplPositionClassTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                fkName = "EMPL_PSTPCS_EPTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionClassType",
                fkName = "EMPL_PSTPCS_EPCTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionClassTypeId")
                }
            )
        }
    )
    public interface EmplPositionTypeClassEntity {}

    /**
     * Valid Responsibility
     */
    @Entity(
        name = "ValidResponsibility",
        packageName = "org.ofbiz.humanres.position",
        title = "Valid Responsibility",
        fields = {
            @Field(name = "emplPositionTypeId", type = "id-ne"),
            @Field(name = "responsibilityTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionTypeId"),
            @PrimaryKey(field = "responsibilityTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                fkName = "VALID_RTY_EPSTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ResponsibilityType",
                fkName = "VALID_RTY_RYTP",
                keyMaps = {
                    @KeyMap(fieldName = "responsibilityTypeId")
                }
            )
        }
    )
    public interface ValidResponsibilityEntity {}

    /**
     * EmplPosition Type Rate
     */
    @Entity(
        name = "EmplPositionTypeRate",
        packageName = "org.ofbiz.humanres.position",
        tableName = "EMPL_POSITION_TYPE_RATE_NEW",
        title = "EmplPosition Type Rate",
        fields = {
            @Field(name = "emplPositionTypeId", type = "id-ne"),
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "payGradeId", type = "id"),
            @Field(name = "salaryStepSeqId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplPositionTypeId"),
            @PrimaryKey(field = "rateTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                fkName = "EMPL_PTPRT_EPTP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SalaryStep",
                fkName = "EMPL_PTPRT_SSTP",
                keyMaps = {
                    @KeyMap(fieldName = "salaryStepSeqId"),
                    @KeyMap(fieldName = "payGradeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RateType",
                fkName = "EMPL_PTPRT_RTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            )
        }
    )
    public interface EmplPositionTypeRateEntity {}

    /**
     * Entity for storing data about recruitment
     */
    @Entity(
        name = "JobRequisition",
        packageName = "org.ofbiz.humanres.recruitment",
        title = "Entity for storing data about recruitment",
        fields = {
            @Field(name = "jobRequisitionId", type = "id-ne"),
            @Field(name = "durationMonths", type = "numeric"),
            @Field(name = "age", type = "numeric"),
            @Field(name = "gender", type = "indicator"),
            @Field(name = "experienceMonths", type = "numeric"),
            @Field(name = "experienceYears", type = "numeric"),
            @Field(name = "qualification", type = "id-long"),
            @Field(name = "jobLocation", type = "id"),
            @Field(name = "skillTypeId", type = "id"),
            @Field(name = "noOfResources", type = "numeric"),
            @Field(name = "jobPostingTypeEnumId", type = "id"),
            @Field(name = "jobRequisitionDate", type = "date"),
            @Field(name = "examTypeEnumId", type = "id"),
            @Field(name = "requiredOnDate", type = "date")
        },
        primaryKeys = {
            @PrimaryKey(field = "jobRequisitionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                fkName = "JOB_REQ_SKTYP",
                keyMaps = {
                    @KeyMap(fieldName = "skillTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ExamType",
                fkName = "JOB_REQ_ENUMEXM",
                keyMaps = {
                    @KeyMap(fieldName = "examTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "JobPostingType",
                fkName = "JOB_REQ_ENUMJBP",
                keyMaps = {
                    @KeyMap(fieldName = "jobPostingTypeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface JobRequisitionEntity {}

    /**
     * Entity for storing data about Interviews conducted
     */
    @Entity(
        name = "JobInterview",
        packageName = "org.ofbiz.humanres.recruitment",
        title = "Entity for storing data about Interviews conducted",
        fields = {
            @Field(name = "jobInterviewId", type = "id-ne"),
            @Field(name = "jobIntervieweePartyId", type = "id"),
            @Field(name = "jobRequisitionId", type = "id"),
            @Field(name = "jobInterviewerPartyId", type = "id"),
            @Field(name = "jobInterviewTypeId", type = "id"),
            @Field(name = "gradeSecuredEnumId", type = "id"),
            @Field(name = "jobInterviewResult", type = "id"),
            @Field(name = "jobInterviewDate", type = "date")
        },
        primaryKeys = {
            @PrimaryKey(field = "jobInterviewId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Interviewee",
                fkName = "JOB_INTW_IEPR",
                keyMaps = {
                    @KeyMap(fieldName = "jobIntervieweePartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Interviewer",
                fkName = "JOB_INTW_IRPR",
                keyMaps = {
                    @KeyMap(fieldName = "jobInterviewerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "JobInterviewType",
                fkName = "JOB_INTW_INTYP",
                keyMaps = {
                    @KeyMap(fieldName = "jobInterviewTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "JobRequisition",
                fkName = "JOB_INTW_JBREQ",
                keyMaps = {
                    @KeyMap(fieldName = "jobRequisitionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "JOB_INTW_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "gradeSecuredEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface JobInterviewEntity {}

    /**
     * Entity for storing data about Interview Types
     */
    @Entity(
        name = "JobInterviewType",
        packageName = "org.ofbiz.humanres.recruitment",
        title = "Entity for storing data about Interview Types",
        fields = {
            @Field(name = "jobInterviewTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "jobInterviewTypeId")
        }
    )
    public interface JobInterviewTypeEntity {}

    /**
     * Training Request
     */
    @Entity(
        name = "TrainingRequest",
        packageName = "org.ofbiz.humanres.trainings",
        title = "Training Request",
        fields = {
            @Field(name = "trainingRequestId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "trainingRequestId")
        }
    )
    public interface TrainingRequestEntity {}

    /**
     * Leave Reason Type
     */
    @Entity(
        name = "EmplLeaveReasonType",
        packageName = "org.ofbiz.humanres.employment",
        title = "Leave Reason Type",
        defaultResourceName = "HumanResEntityLabels",
        fields = {
            @Field(name = "emplLeaveReasonTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "emplLeaveReasonTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplLeaveReasonType",
                title = "Parent",
                fkName = "EMPL_REASON_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "emplLeaveReasonTypeId")
                }
            )
        }
    )
    public interface EmplLeaveReasonTypeEntity {}

    /**
     * Benefit Type
     */
    @ViewEntity(
        name = "BenefitTypeAndParty",
        packageName = "org.ofbiz.humanres.employment",
        title = "Benefit Type",
        members = {
            @MemberEntity(entityAlias = "BT", entityName = "BenefitType"),
            @MemberEntity(entityAlias = "PB", entityName = "PartyBenefit")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "BT"),
            @AliasAll(entityAlias = "PB", excludes = {"benefitTypeId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "BT",
                relEntityAlias = "PB",
                keyMaps = {
                    @KeyMap(fieldName = "benefitTypeId")
                }
            )
        }
    )
    public interface BenefitTypeAndPartyView {}

    /**
     * Employment and Person
     */
    @ViewEntity(
        name = "EmploymentAndPerson",
        packageName = "org.ofbiz.humanres.employment",
        title = "Employment and Person",
        members = {
            @MemberEntity(entityAlias = "EMPLMNT", entityName = "Employment"),
            @MemberEntity(entityAlias = "PERS", entityName = "Person")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "EMPLMNT"),
            @AliasAll(entityAlias = "PERS")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "EMPLMNT",
                relEntityAlias = "PERS",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            )
        }
    )
    public interface EmploymentAndPersonView {}

    /**
     * EmplPosition Fulfillment
     */
    @ViewEntity(
        name = "EmplPositionAndFulfillment",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Fulfillment",
        members = {
            @MemberEntity(entityAlias = "EMPPOS", entityName = "EmplPosition"),
            @MemberEntity(entityAlias = "EPF", entityName = "EmplPositionFulfillment")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "EMPPOS")
        },
        aliases = {
            @Alias(name = "employeePartyId", entityAlias = "EPF", field = "partyId"),
            @Alias(name = "fromDate", entityAlias = "EPF"),
            @Alias(name = "thruDate", entityAlias = "EPF")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "EMPPOS",
                relEntityAlias = "EPF",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            )
        }
    )
    public interface EmplPositionAndFulfillmentView {}

    /**
     * EmplPosition Type Rate Entity and Rate Amount
     */
    @ViewEntity(
        name = "EmplPositionTypeRateAndAmount",
        packageName = "org.ofbiz.humanres.position",
        title = "EmplPosition Type Rate Entity and Rate Amount",
        members = {
            @MemberEntity(entityAlias = "EPTR", entityName = "EmplPositionTypeRate"),
            @MemberEntity(entityAlias = "RA", entityName = "RateAmount")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "EPTR")
        },
        aliases = {
            @Alias(name = "rateAmount", entityAlias = "RA"),
            @Alias(name = "periodTypeId", entityAlias = "RA"),
            @Alias(name = "rateCurrencyUomId", entityAlias = "RA"),
            @Alias(name = "rateAmountFromDate", entityAlias = "RA", field = "fromDate"),
            @Alias(name = "rateAmountThruDate", entityAlias = "RA", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "EPTR",
                relEntityAlias = "RA",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId"),
                    @KeyMap(fieldName = "rateTypeId")
                }
            )
        }
    )
    public interface EmplPositionTypeRateAndAmountView {}

    /**
     * Job Requisition and Position
     */
    @ViewEntity(
        name = "JobRequisitionAndEmplPosition",
        packageName = "org.ofbiz.humanres.recruitment",
        title = "Job Requisition and Position",
        members = {
            @MemberEntity(entityAlias = "JR", entityName = "JobRequisition"),
            @MemberEntity(entityAlias = "EP", entityName = "EmplPosition")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "JR"),
            @AliasAll(entityAlias = "EP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "JR",
                relEntityAlias = "EP",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BudgetItem",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmplPositionType",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                keyMaps = {
                    @KeyMap(fieldName = "skillTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ExamType",
                keyMaps = {
                    @KeyMap(fieldName = "examTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "JobPostingType",
                keyMaps = {
                    @KeyMap(fieldName = "jobPostingTypeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface JobRequisitionAndEmplPositionView {}

    /**
     * To view the employment details of an employee
     */
    @ViewEntity(
        name = "EmplPositionFulfillmentAndReportingStruct",
        packageName = "org.ofbiz.humanres.recruitment",
        title = "To view the employment details of an employee",
        members = {
            @MemberEntity(entityAlias = "EMPPOS", entityName = "EmplPosition"),
            @MemberEntity(entityAlias = "EMPPOSFUL", entityName = "EmplPositionFulfillment"),
            @MemberEntity(entityAlias = "EMPPOSREPST", entityName = "EmplPositionReportingStruct")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "EMPPOSFUL"),
            @Alias(name = "emplPositionId", entityAlias = "EMPPOSFUL"),
            @Alias(name = "emplPositionIdReportingTo", entityAlias = "EMPPOSREPST"),
            @Alias(name = "internalOrganisation", entityAlias = "EMPPOS", field = "partyId"),
            @Alias(name = "reportingDate", entityAlias = "EMPPOSREPST", field = "fromDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "EMPPOS",
                relEntityAlias = "EMPPOSFUL",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            ),
            @ViewLink(
                entityAlias = "EMPPOSFUL",
                relEntityAlias = "EMPPOSREPST",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId", relFieldName = "emplPositionIdManagedBy")
                }
            )
        }
    )
    public interface EmplPositionFulfillmentAndReportingStructView {}

    @ExtendEntity(
        name = "JobRequisition",
        fields = {
            @Field(name = "emplPositionId", type = "id", description = "SCIPIO: Single specific position for this requisition (not strictly required but usually wanted)"),
            @Field(name = "jobDescription", type = "very-long", description = "SCIPIO: Job description, which may be tailored to this requisition")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EmplPosition",
                fkName = "JOB_REQ_APP_POS",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionId")
                }
            )
        }
    )
    public interface JobRequisitionExtension {}

}
