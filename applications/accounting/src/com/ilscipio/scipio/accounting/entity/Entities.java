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
package com.ilscipio.scipio.accounting.entity;

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
     * Budget
     */
    @Entity(
        name = "Budget",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetTypeId", type = "id"),
            @Field(name = "customTimePeriodId", type = "id"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetType",
                fkName = "BUDGET_BGTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "budgetTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomTimePeriod",
                fkName = "BUDGET_CTP",
                keyMaps = {
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "budgetTypeId")
                }
            )
        }
    )
    public interface BudgetEntity {}

    /**
     * Budget Attribute
     */
    @Entity(
        name = "BudgetAttribute",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Attribute",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_ATTR_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface BudgetAttributeEntity {}

    /**
     * Budget Item
     */
    @Entity(
        name = "BudgetItem",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Item",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetItemSeqId", type = "id-ne"),
            @Field(name = "budgetItemTypeId", type = "id"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "purpose", type = "long-varchar"),
            @Field(name = "justification", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BDGTITM_TO_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItemType",
                fkName = "BUDGET_ITEM_BTYP",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            )
        }
    )
    public interface BudgetItemEntity {}

    /**
     * Budget Item Attribute
     */
    @Entity(
        name = "BudgetItemAttribute",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Item Attribute",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItem",
                fkName = "BUDGET_ITEM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface BudgetItemAttributeEntity {}

    /**
     * Budget Item Type
     */
    @Entity(
        name = "BudgetItemType",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Item Type",
        fields = {
            @Field(name = "budgetItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItemType",
                title = "Parent",
                fkName = "BUDGET_ITM_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "budgetItemTypeId")
                }
            )
        }
    )
    public interface BudgetItemTypeEntity {}

    /**
     * Budget Item Type Attribute
     */
    @Entity(
        name = "BudgetItemTypeAttr",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Item Type Attribute",
        fields = {
            @Field(name = "budgetItemTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetItemTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItemType",
                fkName = "BUDGET_ITMTYPATTR",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetItemAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetItem",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            )
        }
    )
    public interface BudgetItemTypeAttrEntity {}

    /**
     * Budget Review
     */
    @Entity(
        name = "BudgetReview",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Review",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetReviewId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "budgetReviewResultTypeId", type = "id-ne"),
            @Field(name = "reviewDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetReviewId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "budgetReviewResultTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_RVW_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "BUDGET_RVW_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetReviewResultType",
                fkName = "BUDGET_RVW_RTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "budgetReviewResultTypeId")
                }
            )
        }
    )
    public interface BudgetReviewEntity {}

    /**
     * Budget Review Result Type
     */
    @Entity(
        name = "BudgetReviewResultType",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Review Result Type",
        fields = {
            @Field(name = "budgetReviewResultTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetReviewResultTypeId")
        }
    )
    public interface BudgetReviewResultTypeEntity {}

    /**
     * Budget Revision
     */
    @Entity(
        name = "BudgetRevision",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Revision",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "revisionSeqId", type = "id-ne"),
            @Field(name = "dateRevised", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "revisionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_RVSN_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            )
        }
    )
    public interface BudgetRevisionEntity {}

    /**
     * Budget Revision Impact
     */
    @Entity(
        name = "BudgetRevisionImpact",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Revision Impact",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetItemSeqId", type = "id-ne"),
            @Field(name = "revisionSeqId", type = "id-ne"),
            @Field(name = "revisedAmount", type = "currency-amount"),
            @Field(name = "addDeleteFlag", type = "indicator"),
            @Field(name = "revisionReason", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetItemSeqId"),
            @PrimaryKey(field = "revisionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_RNIMP_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItem",
                fkName = "BUDGET_RNIMP_BITM",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetRevision",
                fkName = "BUDGET_RNIMP_REV",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "revisionSeqId")
                }
            )
        }
    )
    public interface BudgetRevisionImpactEntity {}

    /**
     * Budget Role
     */
    @Entity(
        name = "BudgetRole",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Role",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_ROLE_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "BUDGET_ROLE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "BUDGET_ROLE_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface BudgetRoleEntity {}

    /**
     * Budget Scenario
     */
    @Entity(
        name = "BudgetScenario",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Scenario",
        fields = {
            @Field(name = "budgetScenarioId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetScenarioId")
        }
    )
    public interface BudgetScenarioEntity {}

    /**
     * Budget Scenario Application
     */
    @Entity(
        name = "BudgetScenarioApplication",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Scenario Application",
        fields = {
            @Field(name = "budgetScenarioApplicId", type = "id-ne"),
            @Field(name = "budgetScenarioId", type = "id-ne"),
            @Field(name = "budgetId", type = "id"),
            @Field(name = "budgetItemSeqId", type = "id"),
            @Field(name = "amountChange", type = "currency-amount"),
            @Field(name = "percentageChange", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetScenarioApplicId"),
            @PrimaryKey(field = "budgetScenarioId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetScenario",
                fkName = "BUDGET_SAPL_BSCN",
                keyMaps = {
                    @KeyMap(fieldName = "budgetScenarioId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_SAPL_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItem",
                fkName = "BUDGET_SAPL_BITM",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            )
        }
    )
    public interface BudgetScenarioApplicationEntity {}

    /**
     * Budget Scenario Rule
     */
    @Entity(
        name = "BudgetScenarioRule",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Scenario Rule",
        fields = {
            @Field(name = "budgetScenarioId", type = "id-ne"),
            @Field(name = "budgetItemTypeId", type = "id-ne"),
            @Field(name = "amountChange", type = "currency-amount"),
            @Field(name = "percentageChange", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetScenarioId"),
            @PrimaryKey(field = "budgetItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetScenario",
                fkName = "BUDGET_SRLE_BSCN",
                keyMaps = {
                    @KeyMap(fieldName = "budgetScenarioId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItemType",
                fkName = "BUDGET_SRLE_BITP",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            )
        }
    )
    public interface BudgetScenarioRuleEntity {}

    /**
     * Budget Status
     */
    @Entity(
        name = "BudgetStatus",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Status",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "statusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "BUDGET_STTS_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "BUDGET_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface BudgetStatusEntity {}

    /**
     * Budget Type
     */
    @Entity(
        name = "BudgetType",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Type",
        fields = {
            @Field(name = "budgetTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetType",
                title = "Parent",
                fkName = "BUDGET_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "budgetTypeId")
                }
            )
        }
    )
    public interface BudgetTypeEntity {}

    /**
     * Budget Type Attribute
     */
    @Entity(
        name = "BudgetTypeAttr",
        packageName = "org.ofbiz.accounting.budget",
        title = "Budget Type Attribute",
        fields = {
            @Field(name = "budgetTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetType",
                fkName = "BUDGET_TPATR_BT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BudgetAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Budget",
                keyMaps = {
                    @KeyMap(fieldName = "budgetTypeId")
                }
            )
        }
    )
    public interface BudgetTypeAttrEntity {}

    /**
     * Financial Account
     */
    @Entity(
        name = "FinAccount",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account",
        fields = {
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "finAccountTypeId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "finAccountName", type = "name"),
            @Field(name = "finAccountCode", type = "long-varchar", encrypt = "true"),
            @Field(name = "finAccountPin", type = "long-varchar", encrypt = "true"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id", description = "The internal organization Party that owns (or rather, is liable for) the account."),
            @Field(name = "ownerPartyId", type = "id", description = "The customer or third party that owns the account."),
            @Field(name = "postToGlAccountId", type = "id"),
            @Field(name = "fromDate", type = "date-time", description = "Describes when account will be valid. If null, valid immediately."),
            @Field(name = "thruDate", type = "date-time", description = "Expiration date of the account. If null, will never expire."),
            @Field(name = "isRefundable", type = "indicator"),
            @Field(name = "replenishPaymentId", type = "id"),
            @Field(name = "replenishLevel", type = "currency-amount"),
            @Field(name = "actualBalance", type = "currency-amount", description = "Calculated as the sum of FinAccountTrans.amount"),
            @Field(name = "availableBalance", type = "currency-amount", description = "Calculated as actualBalance minus sum of outstanding FinAccountAuth.amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountType",
                fkName = "FINACCT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "FINACCT_CURUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "FINACCT_ORGPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Owner",
                fkName = "FINACCT_OWNPTY",
                keyMaps = {
                    @KeyMap(fieldName = "ownerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "PostTo",
                fkName = "FINACCT_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "postToGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                title = "Replenish",
                fkName = "FINACCT_PAYMETH",
                keyMaps = {
                    @KeyMap(fieldName = "replenishPaymentId", relFieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            )
        }
    )
    public interface FinAccountEntity {}

    /**
     * Financial Account Attribute
     */
    @Entity(
        name = "FinAccountAttribute",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Attribute",
        fields = {
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "FINACCT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface FinAccountAttributeEntity {}

    /**
     * Financial Account Authorizations record
     */
    @Entity(
        name = "FinAccountAuth",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Authorizations record",
        fields = {
            @Field(name = "finAccountAuthId", type = "id-ne"),
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "authorizationDate", type = "date-time"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountAuthId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "FINACT_AUTH_FINACT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            )
        }
    )
    public interface FinAccountAuthEntity {}

    /**
     * Financial Account Role
     */
    @Entity(
        name = "FinAccountRole",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Role",
        fields = {
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "FIN_ACT_RL_FNACT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "FIN_ACT_RL_RTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface FinAccountRoleEntity {}

    /**
     * Financial Account Status
     */
    @Entity(
        name = "FinAccountStatus",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Status",
        fields = {
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "statusEndDate", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountId"),
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "statusDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "FINACT_STTS_FNA",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "FINACT_STTS_STI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "FINACT_STTS_USER",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface FinAccountStatusEntity {}

    /**
     * Financial Account Transaction
     */
    @Entity(
        name = "FinAccountTrans",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Transaction",
        fields = {
            @Field(name = "finAccountTransId", type = "id-ne"),
            @Field(name = "finAccountTransTypeId", type = "id-ne"),
            @Field(name = "finAccountId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "glReconciliationId", type = "id"),
            @Field(name = "transactionDate", type = "date-time"),
            @Field(name = "entryDate", type = "date-time"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id", description = "To be used along with orderId to point to an OrderItem that represents the purchase of a product to add money to the account."),
            @Field(name = "performedByPartyId", type = "id"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "statusId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTransId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTransType",
                fkName = "FINACCT_TX_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTransTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "FIN_ACT_TX_FNACT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FIN_ACT_TX_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "FIN_ACT_TX_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "FIN_ACT_TX_ODITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "PerformedBy",
                fkName = "FIN_ACT_TX_PBPTY",
                keyMaps = {
                    @KeyMap(fieldName = "performedByPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Reason",
                fkName = "FIN_ACT_REAS_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "FIN_ACT_TX_STI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlReconciliation",
                fkName = "FIN_ACT_TX_GLREC",
                keyMaps = {
                    @KeyMap(fieldName = "glReconciliationId")
                }
            )
        }
    )
    public interface FinAccountTransEntity {}

    /**
     * Financial Account Transaction Attribute
     */
    @Entity(
        name = "FinAccountTransAttribute",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Transaction Attribute",
        fields = {
            @Field(name = "finAccountTransId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTransId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTrans",
                fkName = "FINACCT_TX_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTransTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface FinAccountTransAttributeEntity {}

    /**
     * Financial Account Transaction Type
     */
    @Entity(
        name = "FinAccountTransType",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Transaction Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "finAccountTransTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTransTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTransType",
                title = "Parent",
                fkName = "FINACCT_TX_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "finAccountTransTypeId")
                }
            )
        }
    )
    public interface FinAccountTransTypeEntity {}

    /**
     * Financial Account Transaction Type Attribute
     */
    @Entity(
        name = "FinAccountTransTypeAttr",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Transaction Type Attribute",
        fields = {
            @Field(name = "finAccountTransTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTransTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTransType",
                fkName = "FINACCT_TX_TYPATR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTransAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountTrans",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransTypeId")
                }
            )
        }
    )
    public interface FinAccountTransTypeAttrEntity {}

    /**
     * Financial Account Type
     */
    @Entity(
        name = "FinAccountType",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "finAccountTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "replenishEnumId", type = "id-ne"),
            @Field(name = "isRefundable", type = "indicator"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountType",
                title = "Parent",
                fkName = "FINACCT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "finAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Replenish",
                fkName = "FINACCT_TYPE_RENUM",
                keyMaps = {
                    @KeyMap(fieldName = "replenishEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface FinAccountTypeEntity {}

    /**
     * Financial Account Type Attribute
     */
    @Entity(
        name = "FinAccountTypeAttr",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Type Attribute",
        fields = {
            @Field(name = "finAccountTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "long-varchar"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountType",
                fkName = "FINACCT_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccountAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FinAccount",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            )
        }
    )
    public interface FinAccountTypeAttrEntity {}

    /**
     * Financial Account Type GL Account
     */
    @Entity(
        name = "FinAccountTypeGlAccount",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Type GL Account",
        fields = {
            @Field(name = "finAccountTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "finAccountTypeId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountType",
                fkName = "FINACCT_TGA_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "FINACCT_TGA_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "FINACCT_TGA_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface FinAccountTypeGlAccountEntity {}

    /**
     * Fixed Asset
     */
    @Entity(
        name = "FixedAsset",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fixedAssetTypeId", type = "id"),
            @Field(name = "parentFixedAssetId", type = "id"),
            @Field(name = "instanceOfProductId", type = "id"),
            @Field(name = "classEnumId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "fixedAssetName", type = "name"),
            @Field(name = "acquireOrderId", type = "id"),
            @Field(name = "acquireOrderItemSeqId", type = "id"),
            @Field(name = "dateAcquired", type = "date-time"),
            @Field(name = "dateLastServiced", type = "date-time"),
            @Field(name = "dateNextService", type = "date-time"),
            @Field(name = "expectedEndOfLife", type = "date"),
            @Field(name = "actualEndOfLife", type = "date"),
            @Field(name = "productionCapacity", type = "fixed-point"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "calendarId", type = "id-ne"),
            @Field(name = "serialNumber", type = "long-varchar"),
            @Field(name = "locatedAtFacilityId", type = "id"),
            @Field(name = "locatedAtLocationSeqId", type = "id"),
            @Field(name = "salvageValue", type = "currency-amount"),
            @Field(name = "depreciation", type = "currency-amount"),
            @Field(name = "purchaseCost", type = "currency-amount"),
            @Field(name = "purchaseCostUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetType",
                fkName = "FIXEDAST_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FixedAssetTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                title = "Parent",
                fkName = "FIXEDAST_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentFixedAssetId", relFieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "InstanceOf",
                fkName = "FIXEDAST_IOPROD",
                keyMaps = {
                    @KeyMap(fieldName = "instanceOfProductId", relFieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Class",
                fkName = "FIXEDAST_CLSENM",
                keyMaps = {
                    @KeyMap(fieldName = "classEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FIXEDAST_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "FIXEDAST_ROLETYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Acquire",
                fkName = "FIXEDAST_ORDHDR",
                keyMaps = {
                    @KeyMap(fieldName = "acquireOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                title = "Acquire",
                fkName = "FIXEDAST_ORDITM",
                keyMaps = {
                    @KeyMap(fieldName = "acquireOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "acquireOrderItemSeqId", relFieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "FIXEDAST_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TechDataCalendar",
                fkName = "FIXEDAST_CALENDAR",
                keyMaps = {
                    @KeyMap(fieldName = "calendarId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "LocatedAt",
                fkName = "FIXEDAST_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "locatedAtFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                title = "LocatedAt",
                keyMaps = {
                    @KeyMap(fieldName = "locatedAtFacilityId", relFieldName = "facilityId"),
                    @KeyMap(fieldName = "locatedAtLocationSeqId", relFieldName = "locationSeqId")
                }
            )
        }
    )
    public interface FixedAssetEntity {}

    /**
     * Fixed Asset Attribute
     */
    @Entity(
        name = "FixedAssetAttribute",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Attribute",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXEDAST_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FixedAssetTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface FixedAssetAttributeEntity {}

    /**
     * Fixed Asset Depreciation Method
     */
    @Entity(
        name = "FixedAssetDepMethod",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Depreciation Method",
        fields = {
            @Field(name = "depreciationCustomMethodId", type = "id-ne"),
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "depreciationCustomMethodId"),
            @PrimaryKey(field = "fixedAssetId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "FIXDAST_DM_CMET",
                keyMaps = {
                    @KeyMap(fieldName = "depreciationCustomMethodId", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXDAST_DM_FXAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            )
        }
    )
    public interface FixedAssetDepMethodEntity {}

    /**
     * Fixed Asset Geo Location with history
     */
    @Entity(
        name = "FixedAssetGeoPoint",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Geo Location with history",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "geoPointId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "geoPointId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FXDASTGEOPT_FXDAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "FXDASTGEOPT_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface FixedAssetGeoPointEntity {}

    /**
     * Fixed Asset Identification
     */
    @Entity(
        name = "FixedAssetIdent",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Identification",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fixedAssetIdentTypeId", type = "id-ne"),
            @Field(name = "idValue", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "fixedAssetIdentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXDASTID_FXAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetIdentType",
                fkName = "FIXDASTID_IDTYP",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetIdentTypeId")
                }
            )
        }
    )
    public interface FixedAssetIdentEntity {}

    /**
     * Fixed Asset Identification Type
     */
    @Entity(
        name = "FixedAssetIdentType",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Identification Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "fixedAssetIdentTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetIdentTypeId")
        }
    )
    public interface FixedAssetIdentTypeEntity {}

    /**
     * Fixed Asset Maintenance
     */
    @Entity(
        name = "FixedAssetMaint",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Maintenance",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "maintHistSeqId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "productMaintTypeId", type = "id-ne", description = "If productMaintSeqId is known can lookup using that and the FixedAsset.productId; for un-scheduled maintenance is filled in directly"),
            @Field(name = "productMaintSeqId", type = "id", description = "Optional, though should be filled in to determine upcoming maintenance for all scheduled maintenance"),
            @Field(name = "scheduleWorkEffortId", type = "id", description = "Has field for estimated/actual start and finish dates, etc"),
            @Field(name = "intervalQuantity", type = "fixed-point"),
            @Field(name = "intervalUomId", type = "id", description = "UOM for intervalQuantity; if used intervalMeterTypeId is generally not used (ie one or the other); if a meter reading is done as well that is not tied to the interval it should be tracked in a FixedAssetMaintMeter record"),
            @Field(name = "intervalMeterTypeId", type = "id", description = "Meter Type for intervalQuantity; if used intervalUomId is generally not used (ie one or the other)"),
            @Field(name = "purchaseOrderId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "maintHistSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FXDASTMNT_FXAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMaintType",
                fkName = "FXDASTMNT_PMNTP",
                keyMaps = {
                    @KeyMap(fieldName = "productMaintTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "Schedule",
                fkName = "FXDASTMNT_SCHWE",
                keyMaps = {
                    @KeyMap(fieldName = "scheduleWorkEffortId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Interval",
                fkName = "FXDASTMNT_INTUOM",
                keyMaps = {
                    @KeyMap(fieldName = "intervalUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMeterType",
                title = "Interval",
                fkName = "FXDASTMNT_PDMTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "intervalMeterTypeId", relFieldName = "productMeterTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Purchase",
                fkName = "FXDASTMNT_PURORD",
                keyMaps = {
                    @KeyMap(fieldName = "purchaseOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "FXDASTMNT_SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface FixedAssetMaintEntity {}

    /**
     * Deprecated - use FixedAssetMeter
     */
    @Entity(
        name = "FixedAssetMaintMeter",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Deprecated - use FixedAssetMeter",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "maintHistSeqId", type = "id-ne"),
            @Field(name = "productMeterTypeId", type = "id-ne"),
            @Field(name = "meterValue", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "maintHistSeqId"),
            @PrimaryKey(field = "productMeterTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetMaint",
                fkName = "FXDASTMNMT_FAMNT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId"),
                    @KeyMap(fieldName = "maintHistSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMeterType",
                fkName = "FXDASTMNMT_PMTYP",
                keyMaps = {
                    @KeyMap(fieldName = "productMeterTypeId")
                }
            )
        }
    )
    public interface FixedAssetMaintMeterEntity {}

    /**
     * Fixed Asset Meter
     */
    @Entity(
        name = "FixedAssetMeter",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Meter",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "productMeterTypeId", type = "id-ne"),
            @Field(name = "readingDate", type = "date-time"),
            @Field(name = "meterValue", type = "fixed-point"),
            @Field(name = "readingReasonEnumId", type = "id"),
            @Field(name = "maintHistSeqId", type = "id"),
            @Field(name = "workEffortId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "productMeterTypeId"),
            @PrimaryKey(field = "readingDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetMaint",
                fkName = "FXDASTMTR_FAMNT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId"),
                    @KeyMap(fieldName = "maintHistSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMeterType",
                fkName = "FXDASTMTR_PMTYP",
                keyMaps = {
                    @KeyMap(fieldName = "productMeterTypeId")
                }
            )
        }
    )
    public interface FixedAssetMeterEntity {}

    /**
     * Fixed Asset Product Representation
     */
    @Entity(
        name = "FixedAssetProduct",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Product Representation",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "fixedAssetProductTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "quantityUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "fixedAssetProductTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "FIXDASTPRD_PRD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXDASTPRD_FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetProductType",
                fkName = "FIXDASTPRD_FAPT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetProductTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "FIXDASTPRD_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface FixedAssetProductEntity {}

    /**
     * Fixed Asset Product Type
     */
    @Entity(
        name = "FixedAssetProductType",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Product Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "fixedAssetProductTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetProductTypeId")
        }
    )
    public interface FixedAssetProductTypeEntity {}

    /**
     * Gl Account Mapping For Fixed Asset Or Fixed Asset Types
     */
    @Entity(
        name = "FixedAssetTypeGlAccount",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Gl Account Mapping For Fixed Asset Or Fixed Asset Types",
        fields = {
            @Field(name = "fixedAssetTypeId", type = "id-ne", description = "The fixed asset type for the mappings. This field can be set to _NA_ in order to define a mapping for all types or for a specific asset (specified by the id in the fixedAssetId field)."),
            @Field(name = "fixedAssetId", type = "id-ne", description = "The fixed asset id for the mappings. This field can be set to _NA_ in order to define a mapping for all assets of a given type (specified by the id in the fixedAssetTypeId field)."),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "assetGlAccountId", type = "id", description = "The (debit) account for the initial asset value (purchase cost)"),
            @Field(name = "accDepGlAccountId", type = "id", description = "The (credit) account for the accumulated depreciation"),
            @Field(name = "depGlAccountId", type = "id", description = "The (debit) account for the depreciation expense (matches the accDepGlAccountId)"),
            @Field(name = "profitGlAccountId", type = "id", description = "The (credit) account for the eventual profit derived from the sale of the asset"),
            @Field(name = "lossGlAccountId", type = "id", description = "The (debit) account for the eventual loss derived from the sale of the asset")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetTypeId"),
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FixedAssetType",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FixedAsset",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FATGL_OP",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Asset",
                fkName = "FATGL_AGL",
                keyMaps = {
                    @KeyMap(fieldName = "assetGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "AccumulatedDepreciation",
                fkName = "FATGL_ACCDGL",
                keyMaps = {
                    @KeyMap(fieldName = "accDepGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Depreciation",
                fkName = "FATGL_DGL",
                keyMaps = {
                    @KeyMap(fieldName = "depGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Profit",
                fkName = "FATGL_PGL",
                keyMaps = {
                    @KeyMap(fieldName = "profitGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Loss",
                fkName = "FATGL_LGL",
                keyMaps = {
                    @KeyMap(fieldName = "lossGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface FixedAssetTypeGlAccountEntity {}

    /**
     * Fixed Asset Registration
     */
    @Entity(
        name = "FixedAssetRegistration",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Registration",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "registrationDate", type = "date-time"),
            @Field(name = "govAgencyPartyId", type = "id-ne"),
            @Field(name = "registrationNumber", type = "long-varchar"),
            @Field(name = "licenseNumber", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXDASTREG_FXAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "GovAgency",
                fkName = "FIXDASTREG_GVAPTY",
                keyMaps = {
                    @KeyMap(fieldName = "govAgencyPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface FixedAssetRegistrationEntity {}

    /**
     * Fixed Asset Standard Cost
     */
    @Entity(
        name = "FixedAssetStdCost",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Standard Cost",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fixedAssetStdCostTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "amountUomId", type = "id"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "fixedAssetStdCostTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FIXASTCO_FIXAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetStdCostType",
                fkName = "FIXASTCO_TYPCOS",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetStdCostTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "FIXASTCO_AMCURR",
                keyMaps = {
                    @KeyMap(fieldName = "amountUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface FixedAssetStdCostEntity {}

    /**
     * Fixed Asset Standard Cost Type
     */
    @Entity(
        name = "FixedAssetStdCostType",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Standard Cost Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "fixedAssetStdCostTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetStdCostTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetStdCostType",
                title = "Parent",
                fkName = "FIXASTCO_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "fixedAssetStdCostTypeId")
                }
            )
        }
    )
    public interface FixedAssetStdCostTypeEntity {}

    /**
     * Fixed Asset Type
     */
    @Entity(
        name = "FixedAssetType",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "fixedAssetTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetType",
                title = "Parent",
                fkName = "FIXEDAST_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "fixedAssetTypeId")
                }
            )
        }
    )
    public interface FixedAssetTypeEntity {}

    /**
     * Fixed Asset Type Attribute
     */
    @Entity(
        name = "FixedAssetTypeAttr",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Type Attribute",
        fields = {
            @Field(name = "fixedAssetTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetType",
                fkName = "FIXEDAST_TYPATTR",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FixedAssetAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FixedAsset",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetTypeId")
                }
            )
        }
    )
    public interface FixedAssetTypeAttrEntity {}

    /**
     * Party Fixed Asset Assignment
     */
    @Entity(
        name = "PartyFixedAssetAssignment",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Party Fixed Asset Assignment",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "allocatedDate", type = "date-time"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
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
                fkName = "PTY_FASTAS_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "PTY_FASTAS_FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PTY_FASTAS_SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface PartyFixedAssetAssignmentEntity {}

    /**
     * Fixed Asset Maintance And Order
     */
    @Entity(
        name = "FixedAssetMaintOrder",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset Maintance And Order",
        fields = {
            @Field(name = "fixedAssetId", type = "id-ne"),
            @Field(name = "maintHistSeqId", type = "id-ne"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "fixedAssetId"),
            @PrimaryKey(field = "maintHistSeqId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "FXDASTMNT_ORD_FXAS",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "FXDASTMNT_ORD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface FixedAssetMaintOrderEntity {}

    /**
     * Accommodation Class
     */
    @Entity(
        name = "AccommodationClass",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Accommodation Class",
        fields = {
            @Field(name = "accommodationClassId", type = "id-ne"),
            @Field(name = "parentClassId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "accommodationClassId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationClass",
                title = "Parent",
                fkName = "ACCOMM_CLASS_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentClassId", relFieldName = "accommodationClassId")
                }
            )
        }
    )
    public interface AccommodationClassEntity {}

    /**
     * Accommodation Spot
     */
    @Entity(
        name = "AccommodationSpot",
        packageName = "org.ofbiz.order.reservations",
        title = "Accommodation Spot",
        fields = {
            @Field(name = "accommodationSpotId", type = "id-ne"),
            @Field(name = "accommodationClassId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "numberOfSpaces", type = "numeric"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "accommodationSpotId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationClass",
                fkName = "ACCOM_CLASS",
                keyMaps = {
                    @KeyMap(fieldName = "accommodationClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "SPOT_FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            )
        }
    )
    public interface AccommodationSpotEntity {}

    /**
     * Accommodation Map
     */
    @Entity(
        name = "AccommodationMap",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Accommodation Map",
        fields = {
            @Field(name = "accommodationMapId", type = "id-ne"),
            @Field(name = "accommodationClassId", type = "id-ne"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "accommodationMapTypeId", type = "id"),
            @Field(name = "numberOfSpaces", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "accommodationMapId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationClass",
                fkName = "ACMD_MAP_CLASS",
                keyMaps = {
                    @KeyMap(fieldName = "accommodationClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "ACMD_MAP_FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AccommodationMapType",
                fkName = "ACMD_MAP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "accommodationMapTypeId")
                }
            )
        }
    )
    public interface AccommodationMapEntity {}

    /**
     * Accommodation Map Type
     */
    @Entity(
        name = "AccommodationMapType",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Accommodation Map Type",
        fields = {
            @Field(name = "accommodationMapTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "accommodationMapTypeId")
        }
    )
    public interface AccommodationMapTypeEntity {}

    /**
     * Invoice
     */
    @Entity(
        name = "Invoice",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceTypeId", type = "id"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "billingAccountId", type = "id"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "invoiceDate", type = "date-time"),
            @Field(name = "dueDate", type = "date-time"),
            @Field(name = "paidDate", type = "date-time"),
            @Field(name = "invoiceMessage", type = "long-varchar"),
            @Field(name = "referenceNumber", type = "short-varchar"),
            @Field(name = "description", type = "description"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "recurrenceInfoId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceType",
                fkName = "INVOICE_INVTYP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "INVOICE_PARTY_FRM",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "INVOICE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "INVOICE_ROLETYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "INVOICE_STTSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "INVOICE_BILLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "INVOICE_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "INVOICE_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceInfo",
                fkName = "INVOICE_RECINFO",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            )
        }
    )
    public interface InvoiceEntity {}

    /**
     * Invoice Attribute
     */
    @Entity(
        name = "InvoiceAttribute",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Attribute",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVOICE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface InvoiceAttributeEntity {}

    /**
     * Invoice Content
     */
    @Entity(
        name = "InvoiceContent",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Content",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceContentTypeId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INV_CNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "INV_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceContentType",
                fkName = "INV_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceContentTypeId")
                }
            )
        }
    )
    public interface InvoiceContentEntity {}

    /**
     * Invoice Content Type
     */
    @Entity(
        name = "InvoiceContentType",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Content Type",
        fields = {
            @Field(name = "invoiceContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceContentType",
                title = "Parent",
                fkName = "INVCT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "invoiceContentTypeId")
                }
            )
        }
    )
    public interface InvoiceContentTypeEntity {}

    /**
     * Invoice Contact Mechanism
     */
    @Entity(
        name = "InvoiceContactMech",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Contact Mechanism",
        neverCache = true,
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "contactMechPurposeTypeId"),
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVOICE_CMECH_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "INVOICE_CMECH_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "INVOICE_CMECH_CMPT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        }
    )
    public interface InvoiceContactMechEntity {}

    /**
     * Invoice Item
     */
    @Entity(
        name = "InvoiceItem",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "invoiceItemTypeId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id", description = "used to specify the override or actual glAccountId used for the invoice, avoids problems if configuration changes after initial posting, etc "),
            @Field(name = "overrideOrgPartyId", type = "id", description = "Used to specify the organization override rather than using the payToPartyId"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "parentInvoiceId", type = "id"),
            @Field(name = "parentInvoiceItemSeqId", type = "id"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "taxableFlag", type = "indicator"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "amount", type = "currency-precise"),
            @Field(name = "description", type = "description"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthorityRateSeqId", type = "id-ne"),
            @Field(name = "salesOpportunityId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                fkName = "INVOICE_ITMITYP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVCE_ITM_INVCE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INVCE_ITM_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "INVCE_ITM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "INVCE_ITM_PRDFT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "INVCE_ITM_PINVIT",
                keyMaps = {
                    @KeyMap(fieldName = "parentInvoiceId", relFieldName = "invoiceId"),
                    @KeyMap(fieldName = "parentInvoiceItemSeqId", relFieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItem",
                title = "Children",
                fkName = "INVCE_ITM_CINVIT",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId", relFieldName = "parentInvoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId", relFieldName = "parentInvoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "INVCE_ITM_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Override",
                fkName = "INVCE_ITM_ORGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "TaxAuthority",
                fkName = "INVCE_ITM_TAXPTY",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Tax",
                fkName = "INVCE_ITM_TAXGEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthorityRateProduct",
                fkName = "INVOICE_ITM_TARP",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthorityRateSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "OverrideOrg",
                fkName = "INVCE_ITM_OVRPTY",
                keyMaps = {
                    @KeyMap(fieldName = "overrideOrgPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "INVCE_ITM_SLSOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            )
        }
    )
    public interface InvoiceItemEntity {}

    /**
     * Invoice Item Association
     */
    @Entity(
        name = "InvoiceItemAssoc",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Association",
        fields = {
            @Field(name = "invoiceIdFrom", type = "id-ne"),
            @Field(name = "invoiceItemSeqIdFrom", type = "id-ne"),
            @Field(name = "invoiceIdTo", type = "id-ne"),
            @Field(name = "invoiceItemSeqIdTo", type = "id-ne"),
            @Field(name = "invoiceItemAssocTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "partyIdTo", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceIdFrom"),
            @PrimaryKey(field = "invoiceItemSeqIdFrom"),
            @PrimaryKey(field = "invoiceIdTo"),
            @PrimaryKey(field = "invoiceItemSeqIdTo"),
            @PrimaryKey(field = "invoiceItemAssocTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemAssocType",
                fkName = "INITMASCTYP_IIASC",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                title = "From",
                fkName = "INITMASC_FIITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceIdFrom", relFieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqIdFrom", relFieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                title = "To",
                fkName = "INITMASC_TIITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceIdTo", relFieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqIdTo", relFieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            )
        }
    )
    public interface InvoiceItemAssocEntity {}

    /**
     * Invoice Item Assoc Type
     */
    @Entity(
        name = "InvoiceItemAssocType",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Assoc Type",
        fields = {
            @Field(name = "invoiceItemAssocTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceItemAssocTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemAssocType",
                title = "Parent",
                fkName = "INITMASCTYP_PRNT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "invoiceItemAssocTypeId")
                }
            )
        }
    )
    public interface InvoiceItemAssocTypeEntity {}

    /**
     * Invoice Item Attribute
     */
    @Entity(
        name = "InvoiceItemAttribute",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Attribute",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "INVOICE_IMAT_ITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface InvoiceItemAttributeEntity {}

    /**
     * Invoice Item Type
     */
    @Entity(
        name = "InvoiceItemType",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "invoiceItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "defaultGlAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                title = "Parent",
                fkName = "INVOICE_ITEM_TPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Default",
                fkName = "INVOICE_ITM_DGLAC",
                keyMaps = {
                    @KeyMap(fieldName = "defaultGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface InvoiceItemTypeEntity {}

    /**
     * Invoice Item Type Attribute
     */
    @Entity(
        name = "InvoiceItemTypeAttr",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Type Attribute",
        fields = {
            @Field(name = "invoiceItemTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceItemTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                fkName = "INVOICE_ITEM_TATR",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItemAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItem",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            )
        }
    )
    public interface InvoiceItemTypeAttrEntity {}

    /**
     * Invoice Item Type GL Account
     */
    @Entity(
        name = "InvoiceItemTypeGlAccount",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Type GL Account",
        fields = {
            @Field(name = "invoiceItemTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceItemTypeId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                fkName = "INVOICE_ITGA_IIT",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "INVOICE_ITGA_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "INVOICE_ITGA_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface InvoiceItemTypeGlAccountEntity {}

    /**
     * Invoice Item Type Map
     */
    @Entity(
        name = "InvoiceItemTypeMap",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Item Type Map",
        fields = {
            @Field(name = "invoiceItemMapKey", type = "id-ne"),
            @Field(name = "invoiceTypeId", type = "id-ne"),
            @Field(name = "invoiceItemTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceItemMapKey"),
            @PrimaryKey(field = "invoiceTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                fkName = "INVOICE_ITEM_MAP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceType",
                fkName = "INVITMMAP_INVTYP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItem",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            )
        }
    )
    public interface InvoiceItemTypeMapEntity {}

    /**
     * Invoice Role
     */
    @Entity(
        name = "InvoiceRole",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Role",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "datetimePerformed", type = "date-time"),
            @Field(name = "percentage", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVCE_RLE_INVCE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "INVCE_RLE_PARTY",
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
                fkName = "INVCE_RLE_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface InvoiceRoleEntity {}

    /**
     * Invoice Status
     */
    @Entity(
        name = "InvoiceStatus",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Status",
        fields = {
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "statusDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "INVCE_STS_STSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVCE_STS_INVCE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            )
        }
    )
    public interface InvoiceStatusEntity {}

    /**
     * Invoice Term
     */
    @Entity(
        name = "InvoiceTerm",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Term",
        fields = {
            @Field(name = "invoiceTermId", type = "id-ne"),
            @Field(name = "termTypeId", type = "id"),
            @Field(name = "invoiceId", type = "id"),
            @Field(name = "invoiceItemSeqId", type = "id"),
            @Field(name = "termValue", type = "currency-amount"),
            @Field(name = "termDays", type = "numeric"),
            @Field(name = "textValue", type = "description"),
            @Field(name = "description", type = "description"),
            @Field(name = "uomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceTermId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                fkName = "INVCE_TRM_TRM",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVCE_TRM_INVCE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InvoiceItem",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface InvoiceTermEntity {}

    /**
     * Invoice Term Attribute
     */
    @Entity(
        name = "InvoiceTermAttribute",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Term Attribute",
        fields = {
            @Field(name = "invoiceTermId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceTermId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceTerm",
                fkName = "INVOICE_TRM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTermId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "TermTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface InvoiceTermAttributeEntity {}

    /**
     * Invoice Type
     */
    @Entity(
        name = "InvoiceType",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "invoiceTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceType",
                title = "Parent",
                fkName = "INVOICE_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "invoiceTypeId")
                }
            )
        }
    )
    public interface InvoiceTypeEntity {}

    /**
     * Invoice Type Attribute
     */
    @Entity(
        name = "InvoiceTypeAttr",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Type Attribute",
        fields = {
            @Field(name = "invoiceTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceType",
                fkName = "INVOICE_TPAT_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Invoice",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            )
        }
    )
    public interface InvoiceTypeAttrEntity {}

    /**
     * Invoice Note
     */
    @Entity(
        name = "InvoiceNote",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice Note",
        fields = {
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "INVOICE_NOTE_INV",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "INVOICE_NOTE_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface InvoiceNoteEntity {}

    /**
     * Accounting Transaction
     */
    @Entity(
        name = "AcctgTrans",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Accounting Transaction",
        fields = {
            @Field(name = "acctgTransId", type = "id-ne"),
            @Field(name = "acctgTransTypeId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "transactionDate", type = "date-time"),
            @Field(name = "isPosted", type = "indicator"),
            @Field(name = "postedDate", type = "date-time"),
            @Field(name = "scheduledPostingDate", type = "date-time"),
            @Field(name = "glJournalId", type = "id"),
            @Field(name = "glFiscalTypeId", type = "id"),
            @Field(name = "voucherRef", type = "short-varchar"),
            @Field(name = "voucherDate", type = "date-time"),
            @Field(name = "groupStatusId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "physicalInventoryId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "invoiceId", type = "id"),
            @Field(name = "paymentId", type = "id"),
            @Field(name = "finAccountTransId", type = "id"),
            @Field(name = "shipmentId", type = "id"),
            @Field(name = "receiptId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "theirAcctgTransId", type = "id-long"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransType",
                fkName = "ACCTTX_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlJournal",
                fkName = "ACCTTX_GLJRNL",
                keyMaps = {
                    @KeyMap(fieldName = "glJournalId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlFiscalType",
                fkName = "ACCTTX_GLFST",
                keyMaps = {
                    @KeyMap(fieldName = "glFiscalTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ACCTTX_GRPSTTS",
                keyMaps = {
                    @KeyMap(fieldName = "groupStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "ACCTTX_FASSET",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PhysicalInventory",
                fkName = "ACCTTX_PHS_INV",
                keyMaps = {
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "ACCTTX_INVITEM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemVariance",
                fkName = "ACCTTX_INVITEMVAR",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId"),
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ACCTTX_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "ACCTTX_ROLETYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "ACCTTX_INVOICE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "ACCTTX_PAYMENT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTrans",
                fkName = "ACCTTX_FNACTTR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "ACCTTX_SHIPMENT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentReceipt",
                fkName = "ACCTTX_SHIPRCPT",
                keyMaps = {
                    @KeyMap(fieldName = "receiptId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "ACCTTX_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AcctgTransTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            )
        }
    )
    public interface AcctgTransEntity {}

    /**
     * Accounting Transaction Attribute
     */
    @Entity(
        name = "AcctgTransAttribute",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Accounting Transaction Attribute",
        fields = {
            @Field(name = "acctgTransId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTrans",
                fkName = "ACCTTX_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AcctgTransTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface AcctgTransAttributeEntity {}

    /**
     * Transaction Entry
     */
    @Entity(
        name = "AcctgTransEntry",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Transaction Entry",
        fields = {
            @Field(name = "acctgTransId", type = "id-ne"),
            @Field(name = "acctgTransEntrySeqId", type = "id-ne"),
            @Field(name = "acctgTransEntryTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "voucherRef", type = "short-varchar"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "theirPartyId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "theirProductId", type = "id"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "glAccountTypeId", type = "id"),
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "origAmount", type = "currency-amount"),
            @Field(name = "origCurrencyUomId", type = "id"),
            @Field(name = "debitCreditFlag", type = "indicator"),
            @Field(name = "dueDate", type = "date"),
            @Field(name = "groupId", type = "id"),
            @Field(name = "taxId", type = "id"),
            @Field(name = "reconcileStatusId", type = "id"),
            @Field(name = "settlementTermId", type = "id"),
            @Field(name = "isSummary", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransId"),
            @PrimaryKey(field = "acctgTransEntrySeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransEntryType",
                fkName = "ACCTTXENT_ATET",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransEntryTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "ACCTTXENT_CURNCY",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "OrigCurrency",
                fkName = "ACCTTXENT_OCURNCY",
                keyMaps = {
                    @KeyMap(fieldName = "origCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTrans",
                fkName = "ACCTTXENT_ACTX",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "ACCTTXENT_INVITEM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ACCTTXENT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "ACCTTXENT_RLTYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "ACCTTXENT_GLACTT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "ACCTTXENT_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountOrganization",
                fkName = "ACCTTXENT_GLACOG",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ACCTTXENT_RCSTS",
                keyMaps = {
                    @KeyMap(fieldName = "reconcileStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SettlementTerm",
                fkName = "ACCTTXENT_STLEN",
                keyMaps = {
                    @KeyMap(fieldName = "settlementTermId")
                }
            )
        }
    )
    public interface AcctgTransEntryEntity {}

    /**
     * Accounting Transaction Entry Type
     */
    @Entity(
        name = "AcctgTransEntryType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Accounting Transaction Entry Type",
        fields = {
            @Field(name = "acctgTransEntryTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransEntryTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransEntryType",
                title = "Parent",
                fkName = "ACCTTXE_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "acctgTransEntryTypeId")
                }
            )
        }
    )
    public interface AcctgTransEntryTypeEntity {}

    /**
     * Accounting Transaction Type
     */
    @Entity(
        name = "AcctgTransType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Accounting Transaction Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "acctgTransTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransType",
                title = "Parent",
                fkName = "ACCTTX_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "acctgTransTypeId")
                }
            )
        }
    )
    public interface AcctgTransTypeEntity {}

    /**
     * Accounting Transaction Type Attribute
     */
    @Entity(
        name = "AcctgTransTypeAttr",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Accounting Transaction Type Attribute",
        fields = {
            @Field(name = "acctgTransTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "acctgTransTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransType",
                fkName = "ACCTTX_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AcctgTransAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AcctgTrans",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            )
        }
    )
    public interface AcctgTransTypeAttrEntity {}

    /**
     * General Ledger Account
     */
    @Entity(
        name = "GlAccount",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "glAccountTypeId", type = "id"),
            @Field(name = "glAccountClassId", type = "id"),
            @Field(name = "glResourceTypeId", type = "id"),
            @Field(name = "glXbrlClassId", type = "id"),
            @Field(name = "parentGlAccountId", type = "id"),
            @Field(name = "accountCode", type = "name"),
            @Field(name = "accountName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "productId", type = "id"),
            @Field(name = "externalId", type = "id", description = "id of the account in an external system where the accounts are imported/exported")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "GLACCT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountClass",
                fkName = "GLACCT_CLSS",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlResourceType",
                fkName = "GLACCT_REC",
                keyMaps = {
                    @KeyMap(fieldName = "glResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlXbrlClass",
                fkName = "GLACCT_XBRLCLS",
                keyMaps = {
                    @KeyMap(fieldName = "glXbrlClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Parent",
                fkName = "GLACCT_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentGlAccountId", relFieldName = "glAccountId")
                }
            )
        },
        indexes = {
            @Index(
                name = "GLACCT_UNQCD",
                unique = true,
                fields = {
                    @IndexField(name = "accountCode")
                }
            )
        }
    )
    public interface GlAccountEntity {}

    /**
     * General Ledger Account Class
     */
    @Entity(
        name = "GlAccountClass",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Class",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "glAccountClassId", type = "id-ne"),
            @Field(name = "parentClassId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "isAssetClass", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountClassId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountClass",
                title = "Parent",
                fkName = "GLACTCLS_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentClassId", relFieldName = "glAccountClassId")
                }
            )
        }
    )
    public interface GlAccountClassEntity {}

    /**
     * General Ledger Account Group
     */
    @Entity(
        name = "GlAccountGroup",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Group",
        fields = {
            @Field(name = "glAccountGroupId", type = "id-ne"),
            @Field(name = "glAccountGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountGroupType",
                fkName = "GLACT_GRP_TP",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountGroupTypeId")
                }
            )
        }
    )
    public interface GlAccountGroupEntity {}

    /**
     * General Ledger Account Group Member
     */
    @Entity(
        name = "GlAccountGroupMember",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Group Member",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "glAccountGroupTypeId", type = "id-ne"),
            @Field(name = "glAccountGroupId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "glAccountGroupTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACT_GPMBR_AC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountGroup",
                fkName = "GLACT_GPMBR_GP",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountGroupType",
                fkName = "GLACT_GPMBR_TP",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountGroupTypeId")
                }
            )
        }
    )
    public interface GlAccountGroupMemberEntity {}

    /**
     * General Ledger Account Group Type
     */
    @Entity(
        name = "GlAccountGroupType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Group Type",
        fields = {
            @Field(name = "glAccountGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountGroupTypeId")
        }
    )
    public interface GlAccountGroupTypeEntity {}

    /**
     * GL Account History
     */
    @Entity(
        name = "GlAccountHistory",
        packageName = "org.ofbiz.accounting.ledger",
        title = "GL Account History",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "customTimePeriodId", type = "id-ne"),
            @Field(name = "openingBalance", type = "currency-amount"),
            @Field(name = "postedDebits", type = "currency-amount"),
            @Field(name = "postedCredits", type = "currency-amount"),
            @Field(name = "endingBalance", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "organizationPartyId"),
            @PrimaryKey(field = "customTimePeriodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACCT_HST_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "GLACCT_HST_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomTimePeriod",
                fkName = "GLACCT_HST_CTP",
                keyMaps = {
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            )
        }
    )
    public interface GlAccountHistoryEntity {}

    /**
     * GL Account Organization
     */
    @Entity(
        name = "GlAccountOrganization",
        packageName = "org.ofbiz.accounting.ledger",
        title = "GL Account Organization",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACCT_ORG_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "GLACCT_ORG_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface GlAccountOrganizationEntity {}

    /**
     * GL Account Role
     */
    @Entity(
        name = "GlAccountRole",
        packageName = "org.ofbiz.accounting.ledger",
        title = "GL Account Role",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACCT_RL_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "GLACCT_RL_PTRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface GlAccountRoleEntity {}

    /**
     * General Ledger Account Type
     */
    @Entity(
        name = "GlAccountType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "glAccountTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                title = "Parent",
                fkName = "GLACTTY_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "glAccountTypeId")
                }
            )
        }
    )
    public interface GlAccountTypeEntity {}

    /**
     * GL Account Organization
     */
    @Entity(
        name = "GlAccountTypeDefault",
        packageName = "org.ofbiz.accounting.ledger",
        title = "GL Account Organization",
        fields = {
            @Field(name = "glAccountTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountTypeId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "GLACCT_TPDF_GLAT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "GLACCT_TPDF_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACCT_TPDF_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface GlAccountTypeDefaultEntity {}

    /**
     * General Ledger Budget Cross Reference
     */
    @Entity(
        name = "GlBudgetXref",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Budget Cross Reference",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "budgetItemTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "allocationPercentage", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "budgetItemTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GL_BDGT_XRF_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItemType",
                fkName = "GL_BDGT_XRF_BIT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetItemTypeId")
                }
            )
        }
    )
    public interface GlBudgetXrefEntity {}

    /**
     * General Ledger Fiscal
     */
    @Entity(
        name = "GlFiscalType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Fiscal",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "glFiscalTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glFiscalTypeId")
        }
    )
    public interface GlFiscalTypeEntity {}

    /**
     * General Ledger Journal
     */
    @Entity(
        name = "GlJournal",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Journal",
        fields = {
            @Field(name = "glJournalId", type = "id-ne"),
            @Field(name = "glJournalName", type = "name"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "isPosted", type = "indicator"),
            @Field(name = "postedDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "glJournalId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "GLJOURN_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface GlJournalEntity {}

    /**
     * General Ledger Reconciliation
     */
    @Entity(
        name = "GlReconciliation",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Reconciliation",
        fields = {
            @Field(name = "glReconciliationId", type = "id-ne"),
            @Field(name = "glReconciliationName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "reconciledBalance", type = "currency-amount"),
            @Field(name = "openingBalance", type = "currency-amount"),
            @Field(name = "reconciledDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "glReconciliationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLREC_GLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "GLREC_GLPARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "GLREC_STI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface GlReconciliationEntity {}

    /**
     * General Ledger Reconciliation Entry
     */
    @Entity(
        name = "GlReconciliationEntry",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Reconciliation Entry",
        fields = {
            @Field(name = "glReconciliationId", type = "id-ne"),
            @Field(name = "acctgTransId", type = "id-ne"),
            @Field(name = "acctgTransEntrySeqId", type = "id-ne"),
            @Field(name = "reconciledAmount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "glReconciliationId"),
            @PrimaryKey(field = "acctgTransId"),
            @PrimaryKey(field = "acctgTransEntrySeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlReconciliation",
                fkName = "GL_RECENT_GLREC",
                keyMaps = {
                    @KeyMap(fieldName = "glReconciliationId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransEntry",
                fkName = "GL_RECENT_ACTTXE",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId"),
                    @KeyMap(fieldName = "acctgTransEntrySeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AcctgTrans",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            )
        }
    )
    public interface GlReconciliationEntryEntity {}

    /**
     * General Ledger Resource
     */
    @Entity(
        name = "GlResourceType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Resource",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "glResourceTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glResourceTypeId")
        }
    )
    public interface GlResourceTypeEntity {}

    /**
     * General Ledger XBRL Class
     */
    @Entity(
        name = "GlXbrlClass",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger XBRL Class",
        fields = {
            @Field(name = "glXbrlClassId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glXbrlClassId")
        }
    )
    public interface GlXbrlClassEntity {}

    /**
     * Party (organization) accounting preferences
     */
    @Entity(
        name = "PartyAcctgPreference",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Party (organization) accounting preferences",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "fiscalYearStartMonth", type = "numeric"),
            @Field(name = "fiscalYearStartDay", type = "numeric"),
            @Field(name = "taxFormId", type = "id-ne"),
            @Field(name = "cogsMethodId", type = "id-ne"),
            @Field(name = "baseCurrencyUomId", type = "id-ne"),
            @Field(name = "invoiceSeqCustMethId", type = "id-ne"),
            @Field(name = "invoiceIdPrefix", type = "very-short"),
            @Field(name = "lastInvoiceNumber", type = "numeric"),
            @Field(name = "lastInvoiceRestartDate", type = "date-time"),
            @Field(name = "useInvoiceIdForReturns", type = "indicator"),
            @Field(name = "quoteSeqCustMethId", type = "id-ne"),
            @Field(name = "quoteIdPrefix", type = "very-short"),
            @Field(name = "lastQuoteNumber", type = "numeric"),
            @Field(name = "orderSeqCustMethId", type = "id-ne"),
            @Field(name = "orderIdPrefix", type = "very-short"),
            @Field(name = "lastOrderNumber", type = "numeric"),
            @Field(name = "refundPaymentMethodId", type = "id"),
            @Field(name = "errorGlJournalId", type = "id", description = "\n                Journal to which all the failed automatic transaction are assigned.\n                If the error journal is set, if the GL posting fails for some reason the triggering operation (finalizing an invoice or payment or whatever) would NOT roll back, instead the partial GL post would be placed into the error journal.\n            "),
            @Field(name = "oldInvoiceSequenceEnumId", type = "id-ne", colName = "INVOICE_SEQUENCE_ENUM_ID"),
            @Field(name = "oldOrderSequenceEnumId", type = "id-ne", colName = "ORDER_SEQUENCE_ENUM_ID"),
            @Field(name = "oldQuoteSequenceEnumId", type = "id-ne", colName = "QUOTE_SEQUENCE_ENUM_ID")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ACTG_PREF_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "TaxForm",
                fkName = "ACTGPREF_TAXFORM",
                keyMaps = {
                    @KeyMap(fieldName = "taxFormId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Cogs",
                fkName = "ACTGPREF_COGS",
                keyMaps = {
                    @KeyMap(fieldName = "cogsMethodId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "ACCTGPREF_CURNCY",
                keyMaps = {
                    @KeyMap(fieldName = "baseCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                title = "Invoice",
                fkName = "ACTGPREF_INVCM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceSeqCustMethId", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                title = "Quote",
                fkName = "ACTGPREF_QTECM",
                keyMaps = {
                    @KeyMap(fieldName = "quoteSeqCustMethId", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                title = "Order",
                fkName = "ACTGPREF_ODRCM",
                keyMaps = {
                    @KeyMap(fieldName = "orderSeqCustMethId", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "ACTGPREF_PAYMTH",
                keyMaps = {
                    @KeyMap(fieldName = "refundPaymentMethodId", relFieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlJournal",
                fkName = "ACTGPREF_GLJRNL",
                keyMaps = {
                    @KeyMap(fieldName = "errorGlJournalId", relFieldName = "glJournalId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "InvoiceSequence",
                fkName = "ACTGPREF_INVSQ",
                keyMaps = {
                    @KeyMap(fieldName = "oldInvoiceSequenceEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "QuoteSequence",
                fkName = "ACTGPREF_QTESQ",
                keyMaps = {
                    @KeyMap(fieldName = "oldQuoteSequenceEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "OrderSequence",
                fkName = "ACTGPREF_ODRSQ",
                keyMaps = {
                    @KeyMap(fieldName = "oldOrderSequenceEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface PartyAcctgPreferenceEntity {}

    /**
     * Running tally of average cost
     * Running tally of a product's average cost in a particular company and facility
     */
    @Entity(
        name = "ProductAverageCost",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Running tally of average cost",
        description = "Running tally of a product's average cost in a particular company and facility",
        fields = {
            @Field(name = "productAverageCostTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "averageCost", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "productAverageCostTypeId"),
            @PrimaryKey(field = "organizationPartyId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductAverageCostType",
                fkName = "AVG_COST_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productAverageCostTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "AVG_COST_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "AVG_COST_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "AVG_COST_FACI",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ProductAverageCostEntity {}

    /**
     * Product average cost type
     */
    @Entity(
        name = "ProductAverageCostType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Product average cost type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "productAverageCostTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productAverageCostTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductAverageCostType",
                title = "Parent",
                fkName = "AVGCOST_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productAverageCostTypeId")
                }
            )
        }
    )
    public interface ProductAverageCostTypeEntity {}

    /**
     * Settlement Term
     */
    @Entity(
        name = "SettlementTerm",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Settlement Term",
        fields = {
            @Field(name = "settlementTermId", type = "id-ne"),
            @Field(name = "termName", type = "name"),
            @Field(name = "termValue", type = "numeric"),
            @Field(name = "uomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "settlementTermId")
        }
    )
    public interface SettlementTermEntity {}

    /**
     * Defines GL Accounts for Inventory Variance Reasons
     */
    @Entity(
        name = "VarianceReasonGlAccount",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Defines GL Accounts for Inventory Variance Reasons",
        fields = {
            @Field(name = "varianceReasonId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "varianceReasonId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "VarianceReason",
                fkName = "VRGL_VREAS",
                keyMaps = {
                    @KeyMap(fieldName = "varianceReasonId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "VRGL_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "VRGL_GLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface VarianceReasonGlAccountEntity {}

    /**
     * Billing Account
     * A BillingAccount Payment Method
     */
    @Entity(
        name = "BillingAccount",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account",
        description = "A BillingAccount Payment Method",
        fields = {
            @Field(name = "billingAccountId", type = "id-ne"),
            @Field(name = "accountLimit", type = "currency-amount"),
            @Field(name = "accountCurrencyUomId", type = "id"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "description", type = "description"),
            @Field(name = "externalAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "billingAccountId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "BILLACCT_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "BILLACCT_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "accountCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "BILLACCT_PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface BillingAccountEntity {}

    /**
     * Billing Account Role
     */
    @Entity(
        name = "BillingAccountRole",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account Role",
        fields = {
            @Field(name = "billingAccountId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "billingAccountId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "BILLACCT_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "BILLACCT_RL_PARTY",
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
                fkName = "BILLACCT_RL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface BillingAccountRoleEntity {}

    /**
     * Billing Account Term
     */
    @Entity(
        name = "BillingAccountTerm",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account Term",
        fields = {
            @Field(name = "billingAccountTermId", type = "id-ne"),
            @Field(name = "billingAccountId", type = "id-ne"),
            @Field(name = "termTypeId", type = "id"),
            @Field(name = "termValue", type = "currency-amount"),
            @Field(name = "termDays", type = "numeric"),
            @Field(name = "uomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "billingAccountTermId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "BILLACCT_TRM_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                fkName = "BILLACCT_TRM_TRM",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "BILLACCT_TRM_BACT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            )
        }
    )
    public interface BillingAccountTermEntity {}

    /**
     * Billing Account Term Attribute
     */
    @Entity(
        name = "BillingAccountTermAttr",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account Term Attribute",
        fields = {
            @Field(name = "billingAccountTermId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "billingAccountTermId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccountTerm",
                fkName = "BILLACCT_TRM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountTermId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "TermTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface BillingAccountTermAttrEntity {}

    /**
     * Credit Card Information
     */
    @Entity(
        name = "CreditCard",
        packageName = "org.ofbiz.accounting.payment",
        title = "Credit Card Information",
        fields = {
            @Field(name = "paymentMethodId", type = "id-ne"),
            @Field(name = "cardType", type = "short-varchar"),
            @Field(name = "cardNumber", type = "credit-card-number", encrypt = "true"),
            @Field(name = "validFromDate", type = "credit-card-date", description = "Not common in some parts of the world."),
            @Field(name = "expireDate", type = "credit-card-date"),
            @Field(name = "issueNumber", type = "credit-card-date", description = "Single digit number on some Switch and Maestro cards"),
            @Field(name = "companyNameOnCard", type = "name"),
            @Field(name = "titleOnCard", type = "name"),
            @Field(name = "firstNameOnCard", type = "name"),
            @Field(name = "middleNameOnCard", type = "name"),
            @Field(name = "lastNameOnCard", type = "name"),
            @Field(name = "suffixOnCard", type = "name"),
            @Field(name = "contactMechId", type = "id-ne", description = "The Billing PostalAddress"),
            @Field(name = "consecutiveFailedAuths", type = "numeric"),
            @Field(name = "lastFailedAuthDate", type = "date-time"),
            @Field(name = "consecutiveFailedNsf", type = "numeric"),
            @Field(name = "lastFailedNsfDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "CREDCARD_PMNTMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "CREDCARD_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "CREDCARD_PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface CreditCardEntity {}

    /**
     * Credit Card Type GL Account
     */
    @Entity(
        name = "CreditCardTypeGlAccount",
        packageName = "org.ofbiz.accounting.payment",
        title = "Credit Card Type GL Account",
        fields = {
            @Field(name = "cardType", type = "short-varchar"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "cardType"),
            @PrimaryKey(field = "organizationPartyId")
        }
    )
    public interface CreditCardTypeGlAccountEntity {}

    /**
     * Deduction
     */
    @Entity(
        name = "Deduction",
        packageName = "org.ofbiz.accounting.payment",
        title = "Deduction",
        fields = {
            @Field(name = "deductionId", type = "id-ne"),
            @Field(name = "deductionTypeId", type = "id"),
            @Field(name = "paymentId", type = "id"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "deductionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DeductionType",
                fkName = "DEDCTN_DEDTYP",
                keyMaps = {
                    @KeyMap(fieldName = "deductionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "DEDCTN_PMNT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            )
        }
    )
    public interface DeductionEntity {}

    /**
     * Deduction Type
     */
    @Entity(
        name = "DeductionType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Deduction Type",
        fields = {
            @Field(name = "deductionTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "deductionTypeId")
        }
    )
    public interface DeductionTypeEntity {}

    /**
     * EFT Account Information
     */
    @Entity(
        name = "EftAccount",
        packageName = "org.ofbiz.accounting.payment",
        title = "EFT Account Information",
        fields = {
            @Field(name = "paymentMethodId", type = "id-ne"),
            @Field(name = "bankName", type = "name"),
            @Field(name = "routingNumber", type = "short-varchar"),
            @Field(name = "accountType", type = "short-varchar"),
            @Field(name = "accountNumber", type = "long-varchar"),
            @Field(name = "nameOnAccount", type = "name"),
            @Field(name = "companyNameOnAccount", type = "name"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "yearsAtBank", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "EFTACCT_PMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "EFTACCT_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "EFTACCT_PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface EftAccountEntity {}

    /**
     * Gift Card Information
     */
    @Entity(
        name = "GiftCard",
        packageName = "org.ofbiz.accounting.payment",
        title = "Gift Card Information",
        fields = {
            @Field(name = "paymentMethodId", type = "id-ne"),
            @Field(name = "cardNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "pinNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "expireDate", type = "credit-card-date"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "GIFTCARD_PMNTMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "GIFTCARD_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "GIFTCARD_PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface GiftCardEntity {}

    /**
     * Gift Card Fulfillment History
     */
    @Entity(
        name = "GiftCardFulfillment",
        packageName = "org.ofbiz.accounting.payment",
        title = "Gift Card Fulfillment History",
        fields = {
            @Field(name = "fulfillmentId", type = "id-ne"),
            @Field(name = "typeEnumId", type = "id-ne"),
            @Field(name = "merchantId", type = "id-vlong-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "surveyResponseId", type = "id-ne"),
            @Field(name = "cardNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "pinNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "responseCode", type = "short-varchar"),
            @Field(name = "referenceNum", type = "short-varchar"),
            @Field(name = "authCode", type = "short-varchar"),
            @Field(name = "fulfillmentDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "fulfillmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "GC_FILL_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "typeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "GC_FILL_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "GC_FILL_ODRH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "GC_FILL_ODRI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyResponse",
                fkName = "GC_FILL_SURVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            )
        }
    )
    public interface GiftCardFulfillmentEntity {}

    /**
     * Payment
     */
    @Entity(
        name = "Payment",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment",
        fields = {
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "paymentTypeId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "paymentMethodId", type = "id"),
            @Field(name = "paymentGatewayResponseId", type = "id"),
            @Field(name = "paymentPreferenceId", type = "id"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "effectiveDate", type = "date-time"),
            @Field(name = "paymentRefNum", type = "short-varchar"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "finAccountTransId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id"),
            @Field(name = "actualCurrencyAmount", type = "currency-amount"),
            @Field(name = "actualCurrencyUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentType",
                fkName = "PAYMENT_PMTYP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PAYMENT_PMETH_TP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "PAYMENT_PMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "PAYMENT_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "ActualCurrency",
                fkName = "PAYMENT_ACUOM",
                keyMaps = {
                    @KeyMap(fieldName = "actualCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CreditCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EftAccount",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "GiftCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderPaymentPreference",
                fkName = "PAYMENT_ORDPMPRF",
                keyMaps = {
                    @KeyMap(fieldName = "paymentPreferenceId", relFieldName = "orderPaymentPreferenceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayResponse",
                fkName = "PAYMENT_PAYGATR",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "PAYMENT_FPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "PAYMENT_TPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "To",
                fkName = "PAYMENT_TRTP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PAYMENT_STTSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTrans",
                fkName = "PAYMENT_FACTX",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PAYMENT_ORGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface PaymentEntity {}

    /**
     * Payment Application
     */
    @Entity(
        name = "PaymentApplication",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Application",
        fields = {
            @Field(name = "paymentApplicationId", type = "id-ne"),
            @Field(name = "paymentId", type = "id"),
            @Field(name = "invoiceId", type = "id"),
            @Field(name = "invoiceItemSeqId", type = "id"),
            @Field(name = "billingAccountId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id", description = "If filled in, payment is applied directly against this GL account"),
            @Field(name = "toPaymentId", type = "id"),
            @Field(name = "taxAuthGeoId", type = "id"),
            @Field(name = "amountApplied", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentApplicationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PAYMENT_APP_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "PAYMENT_APP_INV",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InvoiceItem",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "PAYMENT_APP_BACT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                title = "To",
                fkName = "PAYMENT_APP_TPMT",
                keyMaps = {
                    @KeyMap(fieldName = "toPaymentId", relFieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "PAYMENT_APP_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PAYMENT_APP_ORGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface PaymentApplicationEntity {}

    /**
     * Payment Attribute
     */
    @Entity(
        name = "PaymentAttribute",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Attribute",
        fields = {
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PAYMENT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface PaymentAttributeEntity {}

    /**
     * Payment Budget Allocation
     */
    @Entity(
        name = "PaymentBudgetAllocation",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Budget Allocation",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetItemSeqId", type = "id-ne"),
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetItemSeqId"),
            @PrimaryKey(field = "paymentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Budget",
                fkName = "PAYMENT_BA_BDGT",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
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
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PAYMENT_BA_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            )
        }
    )
    public interface PaymentBudgetAllocationEntity {}

    /**
     * Payment Content
     */
    @Entity(
        name = "PaymentContent",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Content",
        fields = {
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "paymentContentTypeId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "paymentId"),
            @PrimaryKey(field = "paymentContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PAYMENT_CNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PAYMENT_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentContentType",
                fkName = "PAYMENT_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "paymentContentTypeId")
                }
            )
        }
    )
    public interface PaymentContentEntity {}

    /**
     * Payment Content Type
     */
    @Entity(
        name = "PaymentContentType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Content Type",
        fields = {
            @Field(name = "paymentContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentContentType",
                title = "Parent",
                fkName = "PAYCT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "paymentContentTypeId")
                }
            )
        }
    )
    public interface PaymentContentTypeEntity {}

    /**
     * Payment Method
     */
    @Entity(
        name = "PaymentMethod",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Method",
        fields = {
            @Field(name = "paymentMethodId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "finAccountId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PAYMETH_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PAYMETH_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PAYMETH_GLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "PAYMETH_FINACCT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            )
        }
    )
    public interface PaymentMethodEntity {}

    /**
     * PaymentMethodType
     */
    @Entity(
        name = "PaymentMethodType",
        packageName = "org.ofbiz.accounting.payment",
        title = "PaymentMethodType",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "defaultGlAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Default",
                fkName = "PAYMENT_MTP_DGLAC",
                keyMaps = {
                    @KeyMap(fieldName = "defaultGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface PaymentMethodTypeEntity {}

    /**
     * Payment Method Type GL Account
     */
    @Entity(
        name = "PaymentMethodTypeGlAccount",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Method Type GL Account",
        fields = {
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodTypeId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PAYMENT_MTGA_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "PAYMENT_MTGA_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PAYMENT_MTGA_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface PaymentMethodTypeGlAccountEntity {}

    /**
     * Payment Type
     */
    @Entity(
        name = "PaymentType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "paymentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentType",
                title = "Parent",
                fkName = "PAYMENT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "paymentTypeId")
                }
            )
        }
    )
    public interface PaymentTypeEntity {}

    /**
     * Payment Type Attribute
     */
    @Entity(
        name = "PaymentTypeAttr",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Type Attribute",
        fields = {
            @Field(name = "paymentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentType",
                fkName = "PAYMETH_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Payment",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            )
        }
    )
    public interface PaymentTypeAttrEntity {}

    /**
     * Maps PaymentTypes to GlAccountTypes, allowing user to configure payments and gl accounts
     */
    @Entity(
        name = "PaymentGlAccountTypeMap",
        packageName = "org.ofbiz.accounting.payment",
        title = "Maps PaymentTypes to GlAccountTypes, allowing user to configure payments and gl accounts",
        fields = {
            @Field(name = "paymentTypeId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentTypeId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentType",
                fkName = "PMTGLACCT_PMTTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PMTGLACCT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "PMTGLACCT_GLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            )
        }
    )
    public interface PaymentGlAccountTypeMapEntity {}

    /**
     * Payment Gateway Config Type
     */
    @Entity(
        name = "PaymentGatewayConfigType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Gateway Config Type",
        fields = {
            @Field(name = "paymentGatewayConfigTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfigType",
                title = "Parent",
                fkName = "PGCT_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "paymentGatewayConfigTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentGatewayConfigType",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId")
                }
            )
        }
    )
    public interface PaymentGatewayConfigTypeEntity {}

    /**
     * Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayConfig",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "paymentGatewayConfigTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfigType",
                fkName = "PGC_PGCT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigTypeId", relFieldName = "paymentGatewayConfigTypeId")
                }
            )
        }
    )
    public interface PaymentGatewayConfigEntity {}

    /**
     * SagePay Payment Gateway Configuration
     */
    @Entity(
        name = "PaymentGatewaySagePay",
        packageName = "org.ofbiz.accounting.payment",
        title = "SagePay Payment Gateway Configuration",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "vendor", type = "short-varchar", description = "Vendor name"),
            @Field(name = "productionHost", type = "short-varchar", description = "Production Host"),
            @Field(name = "testingHost", type = "short-varchar", description = "Testing Host"),
            @Field(name = "sagePayMode", type = "short-varchar", description = "Mode (PRODUCTION/TEST)"),
            @Field(name = "protocolVersion", type = "very-short", description = "Protocol Version"),
            @Field(name = "authenticationTransType", type = "short-varchar", description = "Authentication type (PAYMENT/AUTHENTICATE/DEFERRED)"),
            @Field(name = "authenticationUrl", type = "long-varchar", description = "Authentication Url"),
            @Field(name = "authoriseTransType", type = "short-varchar", description = "Authorise type (AUTHORISE/RELEASE)"),
            @Field(name = "authoriseUrl", type = "long-varchar", description = "Authorise url"),
            @Field(name = "releaseTransType", type = "short-varchar", description = "Release type (CANCEL/ABORT)"),
            @Field(name = "releaseUrl", type = "long-varchar", description = "Release Url"),
            @Field(name = "voidUrl", type = "long-varchar", description = "Void Url"),
            @Field(name = "refundUrl", type = "long-varchar", description = "Refund Url")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGSP_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewaySagePayEntity {}

    /**
     * Authorize Dot Net Payment Gateway Configuration
     */
    @Entity(
        name = "PaymentGatewayAuthorizeNet",
        packageName = "org.ofbiz.accounting.payment",
        title = "Authorize Dot Net Payment Gateway Configuration",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "transactionUrl", type = "value", description = "Transaction URL"),
            @Field(name = "certificateAlias", type = "value", description = "Certificate Alias"),
            @Field(name = "apiVersion", type = "short-varchar", description = "Target Authorize Dot Net API version"),
            @Field(name = "delimitedData", type = "short-varchar", description = "Delimited data (TRUE|FALSE)"),
            @Field(name = "delimiterChar", type = "short-varchar", description = "Delimited Character - the delimiter to use in the response"),
            @Field(name = "cpVersion", type = "short-varchar", description = "Card Present Version"),
            @Field(name = "cpMarketType", type = "short-varchar", description = "Card Present Market Type"),
            @Field(name = "cpDeviceType", type = "short-varchar", description = "Card Present Device Type"),
            @Field(name = "method", type = "short-varchar", description = "Method - CC for credit card processing"),
            @Field(name = "emailCustomer", type = "short-varchar", description = "Email Customer? - if should send an email to the customer for each transaction (TRUE|FALSE)"),
            @Field(name = "emailMerchant", type = "short-varchar", description = "Email Merchant? - if should send email to the merchant for each transaction (TRUE|FALSE)"),
            @Field(name = "testMode", type = "short-varchar", description = "Test Mode - forces the url property to the test url and adds more logging info to the logs (TRUE|FALSE)"),
            @Field(name = "relayResponse", type = "short-varchar", description = "Relay Response? - if should relay the reposnse to a different server (TRUE|FALSE)"),
            @Field(name = "tranKey", type = "value", description = "Transaction Key", encrypt = "true"),
            @Field(name = "userId", type = "value", description = "Username - your authorize.net userid"),
            @Field(name = "pwd", type = "value", description = "Password - your authorize.net password", encrypt = "true"),
            @Field(name = "transDescription", type = "value", description = "Default Transaction Description"),
            @Field(name = "duplicateWindow", type = "numeric", description = "Check the duplicate transaction in the specified time duration which is specified in seconds. If duplicate transaction occurs in the defined time limit then return error.")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGAN_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayAuthorizeNetEntity {}

    /**
     * eWay Payment Gateway Configuration
     */
    @Entity(
        name = "PaymentGatewayEway",
        packageName = "org.ofbiz.accounting.payment",
        title = "eWay Payment Gateway Configuration",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "customerId", type = "value"),
            @Field(name = "refundPwd", type = "value", encrypt = "true"),
            @Field(name = "testMode", type = "short-varchar"),
            @Field(name = "enableCvn", type = "short-varchar"),
            @Field(name = "enableBeagle", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGEW_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayEwayEntity {}

    /**
     * CyberSource Payment Gateway Configuration
     */
    @Entity(
        name = "PaymentGatewayCyberSource",
        packageName = "org.ofbiz.accounting.payment",
        title = "CyberSource Payment Gateway Configuration",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "merchantId", type = "value", description = "You merchant ID"),
            @Field(name = "apiVersion", type = "short-varchar", description = "Target CyberSource API version"),
            @Field(name = "production", type = "short-varchar", description = "Enable production \"mode\" (true|false)"),
            @Field(name = "keysDir", type = "value", description = "Directory of the keys from CyberSource (Generate using online tools)"),
            @Field(name = "keysFile", type = "value", description = "Name of the keystore (if different then \"merchantID\".p12)"),
            @Field(name = "logEnabled", type = "short-varchar", description = "Log transaction information (true|false)"),
            @Field(name = "logDir", type = "value", description = "Log directory"),
            @Field(name = "logFile", type = "value", description = "Log file name"),
            @Field(name = "logSize", type = "numeric", description = "Max log size (megabytes)"),
            @Field(name = "merchantDescr", type = "value", description = "Merchant Description - Shown on credit card statement"),
            @Field(name = "merchantContact", type = "value", description = "Merchant Description Contact Information - Shown on credit card statement"),
            @Field(name = "autoBill", type = "short-varchar", description = "Auto-Bill In Authorization (true|false)"),
            @Field(name = "enableDav", type = "indicator", description = "Use DAV In Authorization -- May not be supported any longer"),
            @Field(name = "fraudScore", type = "indicator", description = "Use Fraud Scoring In Authorization -- May not be supported any longer"),
            @Field(name = "ignoreAvs", type = "short-varchar", description = "Ignore AVS results (true|false)"),
            @Field(name = "disableBillAvs", type = "indicator", description = "Disable AVS for Capture -- May not be supported any longer"),
            @Field(name = "avsDeclineCodes", type = "value", description = "AVS Decline Codes -- May not be supported any longer")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGCS_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayCyberSourceEntity {}

    /**
     * Payflow Pro Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayPayflowPro",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payflow Pro Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "certsPath", type = "value", description = "Path the the VeriSign Certificate"),
            @Field(name = "hostAddress", type = "value", description = "Address of the payment processor"),
            @Field(name = "hostPort", type = "numeric", description = "Port of the payment processor"),
            @Field(name = "timeout", type = "numeric", description = "Timeout"),
            @Field(name = "proxyAddress", type = "value", description = "Proxy Address"),
            @Field(name = "proxyPort", type = "numeric", description = "Proxy Port"),
            @Field(name = "proxyLogon", type = "value", description = "Proxy Logon"),
            @Field(name = "proxyPassword", type = "value", description = "Proxy Password", encrypt = "true"),
            @Field(name = "vendor", type = "short-varchar", description = "Vendor of account information"),
            @Field(name = "userId", type = "short-varchar", description = "PayFlow UserID of account information"),
            @Field(name = "pwd", type = "value", description = "PayFlow Password of account information", encrypt = "true"),
            @Field(name = "partner", type = "short-varchar", description = "PayFlow Partner of account information"),
            @Field(name = "checkAvs", type = "indicator", description = "Use Address Verification"),
            @Field(name = "checkCvv2", type = "indicator", description = "Require CVV2 Verification"),
            @Field(name = "preAuth", type = "indicator", description = "Pre-Authorize Payments (if set to N will auto-capture)"),
            @Field(name = "enableTransmit", type = "value", description = "Set to false to not transmit anything"),
            @Field(name = "logFileName", type = "value", description = "Log file name"),
            @Field(name = "loggingLevel", type = "numeric", description = "Logging level"),
            @Field(name = "maxLogFileSize", type = "numeric", description = "Max log file size"),
            @Field(name = "stackTraceOn", type = "indicator", description = "Stack trace on/off"),
            @Field(name = "redirectUrl", type = "value", description = "Express Checkout Redirect URL"),
            @Field(name = "returnUrl", type = "value", description = "Express Checkout Return URL"),
            @Field(name = "cancelReturnUrl", type = "value", description = "Express Checkout Return On Cancel URL")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGPF_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayPayflowProEntity {}

    /**
     * PayPal Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayPayPal",
        packageName = "org.ofbiz.accounting.payment",
        title = "PayPal Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "businessEmail", type = "value", description = "Business e-mail"),
            @Field(name = "apiUserName", type = "short-varchar", description = "PayPal API UserName"),
            @Field(name = "apiPassword", type = "short-varchar", description = "PayPal API Password"),
            @Field(name = "apiSignature", type = "short-varchar", description = "PayPal API Signature"),
            @Field(name = "apiEnvironment", type = "short-varchar", description = "PayPal API Environment (valid values are: live, sandbox or beta-sandbox)"),
            @Field(name = "notifyUrl", type = "value", description = "Notify URL"),
            @Field(name = "returnUrl", type = "value", description = "Return URL"),
            @Field(name = "cancelReturnUrl", type = "value", description = "Return On Cancel URL"),
            @Field(name = "imageUrl", type = "value", description = "Image URL to use on PayPal"),
            @Field(name = "confirmTemplate", type = "value", description = "Thank-You / Confirm Order Template (rendered via Freemarker)"),
            @Field(name = "redirectUrl", type = "value", description = "PayPal Redirect URL (Sandbox/Production)"),
            @Field(name = "confirmUrl", type = "value", description = "PayPal Confirm URL Sandbox/Production (JSSE must be configured to use SSL)"),
            @Field(name = "shippingCallbackUrl", type = "url", description = "Specific to Express Checkout which performs callbacks to our server to retrieve shipping estimates"),
            @Field(name = "requireConfirmedShipping", type = "indicator", description = "Indicates that you require that the customer’s shipping address on file with PayPal be a confirmed address.")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGPP_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayPayPalEntity {}

    /**
     * Clear Commerce Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayClearCommerce",
        packageName = "org.ofbiz.accounting.payment",
        title = "Clear Commerce Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "sourceId", type = "short-varchar", description = "Useful for tagging transactions"),
            @Field(name = "groupId", type = "short-varchar", description = "Useful for grouping transactions"),
            @Field(name = "clientId", type = "short-varchar", description = "Client Id of account information"),
            @Field(name = "username", type = "short-varchar", description = "User name of account informatio"),
            @Field(name = "pwd", type = "value", description = "Password of account informatio", encrypt = "true"),
            @Field(name = "userAlias", type = "short-varchar", description = "Alias of account informatio"),
            @Field(name = "effectiveAlias", type = "short-varchar", description = "Effective Alias of account information"),
            @Field(name = "processMode", type = "indicator", description = "Process mode (Y: approve / N: decline / R: random / P: production)"),
            @Field(name = "serverURL", type = "value", description = "Server URL of the payment processor"),
            @Field(name = "enableCVM", type = "indicator", description = "Enable Card Verification Methods (CID, CVC, CVV2)")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGCC_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayClearCommerceEntity {}

    /**
     * RBS WorldPay Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayWorldPay",
        packageName = "org.ofbiz.accounting.payment",
        title = "RBS WorldPay Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "redirectUrl", type = "value", description = "Redirect URL"),
            @Field(name = "instId", type = "value", description = "Worldpay instance Id", encrypt = "true"),
            @Field(name = "authMode", type = "indicator", description = "Authorization Mode (A: Full-Auth / E: Pre-Auth)"),
            @Field(name = "fixContact", type = "indicator", description = "Will displace contact info on WorldPay in non-editable format"),
            @Field(name = "hideContact", type = "indicator", description = "Will hide the contact info completely"),
            @Field(name = "hideCurrency", type = "indicator", description = "This causes the currency drop down to be no hidden, so fixing the currency that the shopper must value purchase in"),
            @Field(name = "langId", type = "short-varchar", description = "The shopper's language choice, as a 2-character ISO 639 code, with optional regionalisation using 2-character country code separated by hyphen"),
            @Field(name = "noLanguageMenu", type = "indicator", description = "This suppresses the display of the language menu if noLanguageMenu no you have a choice of languages enabled for your value installation"),
            @Field(name = "withDelivery", type = "indicator", description = "Displays input fields for delivery address and withDelivery no mandate that they be filled in"),
            @Field(name = "testMode", type = "numeric", description = "Test Mode (100: approve / 101: cancelled / 0: Live Mode (no test)")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGWP_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayWorldPayEntity {}

    /**
     * Orbital Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayOrbital",
        packageName = "org.ofbiz.accounting.payment",
        title = "Orbital Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "username", type = "short-varchar", description = "Orbital Username of account information"),
            @Field(name = "connectionPassword", type = "value", description = "Orbital Password of account information", encrypt = "true"),
            @Field(name = "merchantId", type = "value", description = "You merchant ID"),
            @Field(name = "engineClass", type = "value", description = "Class for the Orbital Gateway - Default should be used - HttpsEngine"),
            @Field(name = "hostName", type = "value", description = "Address of the payment processor"),
            @Field(name = "port", type = "numeric", description = "Port of the payment processor"),
            @Field(name = "hostNameFailover", type = "value", description = "Failover Address of the payment processor"),
            @Field(name = "portFailover", type = "numeric", description = "Failover Port of the payment processor"),
            @Field(name = "connectionTimeoutSeconds", type = "numeric", description = "Timeout"),
            @Field(name = "readTimeoutSeconds", type = "numeric", description = "Read Timeout"),
            @Field(name = "authorizationURI", type = "value", description = "Authorization URI"),
            @Field(name = "sdkVersion", type = "short-varchar", description = "Target Orbital Gateway API version"),
            @Field(name = "sslSocketFactory", type = "short-varchar", description = "SSL Socket Factory (default|strict)"),
            @Field(name = "responseType", type = "short-varchar", description = "Response Type (gateway|host)")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGORB_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayOrbitalEntity {}

    /**
     * SecurePay Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewaySecurePay",
        packageName = "org.ofbiz.accounting.payment",
        title = "SecurePay Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "merchantId", type = "value", description = "You merchant ID"),
            @Field(name = "pwd", type = "value", description = "SecurePay Password of account information", encrypt = "true"),
            @Field(name = "serverURL", type = "value", description = "Server URL of the payment processor"),
            @Field(name = "processTimeout", type = "numeric", description = "Process Timeout"),
            @Field(name = "enableAmountRound", type = "indicator", description = "Enable rounds the currency amount to .00 (Y / N)")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGSCP_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewaySecurePayEntity {}

    /**
     * iDEAL Payment Gateway Config
     */
    @Entity(
        name = "PaymentGatewayiDEAL",
        packageName = "org.ofbiz.accounting.payment",
        title = "iDEAL Payment Gateway Config",
        fields = {
            @Field(name = "paymentGatewayConfigId", type = "id-ne"),
            @Field(name = "merchantId", type = "value", description = "The ID of the webshop, received by the acceptor during the registration process"),
            @Field(name = "merchantSubId", type = "value", description = "SubID of the webshop, default value = 0 (zero); only to be changed in consultation with the acquirer"),
            @Field(name = "merchantReturnURL", type = "value", description = "URL of the page in the webshop to which the consumer is redirected after an iDEAL transaction. This value can be overruled as necessary in the webshop implementation"),
            @Field(name = "acquirerURL", type = "value", description = "URL of the acceptor’s acquirer; the following prescribed values apply to ING"),
            @Field(name = "acquirerTimeout", type = "value", description = "Number of seconds (default = 10) of waiting time for a response from the iDEAL services. If no response is received during that time, an exception is displayed"),
            @Field(name = "privateCert", type = "value", description = "Name of the acceptor’s organization as given during the creation of his or her own certificate. See section 0 for more information about the acceptor’s certificate"),
            @Field(name = "acquirerKeyStoreFilename", type = "value", description = "Keystore file and acquirer’s password"),
            @Field(name = "acquirerKeyStorePassword", type = "value", description = "Password of the Acquirer keystore", encrypt = "true"),
            @Field(name = "merchantKeyStoreFilename", type = "value", description = "Keystore file and merchant’s password"),
            @Field(name = "merchantKeyStorePassword", type = "value", description = "Password of the Merchant keystore", encrypt = "true"),
            @Field(name = "expirationPeriod", type = "value", description = "Expiration period of the transaction")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PGID_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            )
        }
    )
    public interface PaymentGatewayiDEALEntity {}

    /**
     * Payment Gateway Response Message
     */
    @Entity(
        name = "PaymentGatewayRespMsg",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Gateway Response Message",
        fields = {
            @Field(name = "paymentGatewayRespMsgId", type = "id-ne"),
            @Field(name = "paymentGatewayResponseId", type = "id-ne"),
            @Field(name = "pgrMessage", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayRespMsgId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayResponse",
                fkName = "PAYGATRM_PAYGR",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayResponseId")
                }
            )
        }
    )
    public interface PaymentGatewayRespMsgEntity {}

    /**
     * Payment Gateway Response
     */
    @Entity(
        name = "PaymentGatewayResponse",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Gateway Response",
        fields = {
            @Field(name = "paymentGatewayResponseId", type = "id-ne"),
            @Field(name = "paymentServiceTypeEnumId", type = "id-ne"),
            @Field(name = "orderPaymentPreferenceId", type = "id"),
            @Field(name = "paymentMethodTypeId", type = "id"),
            @Field(name = "paymentMethodId", type = "id"),
            @Field(name = "transCodeEnumId", type = "id"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "referenceNum", type = "short-varchar"),
            @Field(name = "altReference", type = "short-varchar"),
            @Field(name = "subReference", type = "short-varchar"),
            @Field(name = "gatewayCode", type = "short-varchar"),
            @Field(name = "gatewayFlag", type = "short-varchar"),
            @Field(name = "gatewayAvsResult", type = "short-varchar"),
            @Field(name = "gatewayCvResult", type = "short-varchar"),
            @Field(name = "gatewayScoreResult", type = "short-varchar"),
            @Field(name = "gatewayMessage", type = "long-varchar"),
            @Field(name = "transactionDate", type = "date-time"),
            @Field(name = "resultDeclined", type = "indicator"),
            @Field(name = "resultNsf", type = "indicator"),
            @Field(name = "resultBadExpire", type = "indicator"),
            @Field(name = "resultBadCardNumber", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGatewayResponseId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ServiceType",
                fkName = "PAYGATR_PSTENUM",
                keyMaps = {
                    @KeyMap(fieldName = "paymentServiceTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "TranCode",
                fkName = "PAYGATR_TXCODE",
                keyMaps = {
                    @KeyMap(fieldName = "transCodeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "PAYGATR_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderPaymentPreference",
                fkName = "PAYGATR_ORDPMPRF",
                keyMaps = {
                    @KeyMap(fieldName = "orderPaymentPreferenceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PAYGATR_PMTP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "PAYGATR_PMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            )
        }
    )
    public interface PaymentGatewayResponseEntity {}

    /**
     * Payment Group
     * Payment Group
     */
    @Entity(
        name = "PaymentGroup",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Group",
        description = "Payment Group",
        fields = {
            @Field(name = "paymentGroupId", type = "id-ne"),
            @Field(name = "paymentGroupTypeId", type = "id-ne"),
            @Field(name = "paymentGroupName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGroupType",
                fkName = "PAYMNTGP_PGTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGroupTypeId")
                }
            )
        }
    )
    public interface PaymentGroupEntity {}

    /**
     * Payment Group Type
     * Payment Group Type
     */
    @Entity(
        name = "PaymentGroupType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Group Type",
        description = "Payment Group Type",
        fields = {
            @Field(name = "paymentGroupTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGroupTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGroupType",
                title = "Parent",
                fkName = "PAYMNTGP_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "paymentGroupTypeId")
                }
            )
        }
    )
    public interface PaymentGroupTypeEntity {}

    /**
     * Payment Group Member
     * Payment Group Member
     */
    @Entity(
        name = "PaymentGroupMember",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment Group Member",
        description = "Payment Group Member",
        fields = {
            @Field(name = "paymentGroupId", type = "id-ne"),
            @Field(name = "paymentId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentGroupId"),
            @PrimaryKey(field = "paymentId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGroup",
                fkName = "PAYGRPMMBR_PG",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "PAYGRPMMBR_PAYMNT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            )
        }
    )
    public interface PaymentGroupMemberEntity {}

    /**
     * PayPal Payment Method Details
     */
    @Entity(
        name = "PayPalPaymentMethod",
        packageName = "org.ofbiz.accounting.payment",
        description = "PayPal Payment Method Details",
        fields = {
            @Field(name = "paymentMethodId", type = "id-ne"),
            @Field(name = "payerId", type = "id"),
            @Field(name = "expressCheckoutToken", type = "short-varchar"),
            @Field(name = "payerStatus", type = "short-varchar"),
            @Field(name = "avsAddr", type = "indicator"),
            @Field(name = "avsZip", type = "indicator"),
            @Field(name = "correlationId", type = "id"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "transactionId", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "paymentMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "PAYPAL_PMNTMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "PAYPAL_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "PAYPAL_PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PayPalPaymentMethodEntity {}

    /**
     * Value Link Key Store
     */
    @Entity(
        name = "ValueLinkKey",
        packageName = "org.ofbiz.accounting.payment",
        title = "Value Link Key Store",
        fields = {
            @Field(name = "merchantId", type = "id-vlong-ne"),
            @Field(name = "publicKey", type = "very-long"),
            @Field(name = "privateKey", type = "very-long"),
            @Field(name = "exchangeKey", type = "very-long"),
            @Field(name = "workingKey", type = "very-long"),
            @Field(name = "workingKeyIndex", type = "numeric"),
            @Field(name = "lastWorkingKey", type = "very-long"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByTerminal", type = "short-varchar"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByTerminal", type = "short-varchar"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "merchantId")
        }
    )
    public interface ValueLinkKeyEntity {}

    /**
     * Party Tax Information
     */
    @Entity(
        name = "PartyTaxAuthInfo",
        packageName = "org.ofbiz.accounting.tax",
        title = "Party Tax Information",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "partyTaxId", type = "id-long-ne"),
            @Field(name = "isExempt", type = "indicator"),
            @Field(name = "isNexus", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "taxAuthGeoId"),
            @PrimaryKey(field = "taxAuthPartyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_TXAI_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "PARTY_TXAI_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            )
        }
    )
    public interface PartyTaxAuthInfoEntity {}

    /**
     * Tax Authority
     */
    @Entity(
        name = "TaxAuthority",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority",
        fields = {
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "requireTaxIdForExemption", type = "indicator"),
            @Field(name = "taxIdFormatPattern", type = "long-varchar"),
            @Field(name = "includeTaxInPrice", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthGeoId"),
            @PrimaryKey(field = "taxAuthPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "TaxAuth",
                fkName = "TAXAUTH_TAGEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "TaxAuth",
                fkName = "TAXAUTH_TAPARTY",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface TaxAuthorityEntity {}

    /**
     * Tax Authority Association
     */
    @Entity(
        name = "TaxAuthorityAssoc",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority Association",
        fields = {
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "toTaxAuthGeoId", type = "id-ne"),
            @Field(name = "toTaxAuthPartyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "taxAuthorityAssocTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthGeoId"),
            @PrimaryKey(field = "taxAuthPartyId"),
            @PrimaryKey(field = "toTaxAuthGeoId"),
            @PrimaryKey(field = "toTaxAuthPartyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "TAXAUTHASC_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                title = "To",
                fkName = "TAXAUTHASC_TOTXA",
                keyMaps = {
                    @KeyMap(fieldName = "toTaxAuthGeoId", relFieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "toTaxAuthPartyId", relFieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthorityAssocType",
                fkName = "TAXAUTHASC_ASTP",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthorityAssocTypeId")
                }
            )
        }
    )
    public interface TaxAuthorityAssocEntity {}

    /**
     * Tax Authority Assoc Type
     */
    @Entity(
        name = "TaxAuthorityAssocType",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority Assoc Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "taxAuthorityAssocTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthorityAssocTypeId")
        }
    )
    public interface TaxAuthorityAssocTypeEntity {}

    /**
     * Tax Authority Product Category
     */
    @Entity(
        name = "TaxAuthorityCategory",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority Product Category",
        fields = {
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "productCategoryId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthGeoId"),
            @PrimaryKey(field = "taxAuthPartyId"),
            @PrimaryKey(field = "productCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "TAXAUTHCAT_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "TAXAUTHCAT_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface TaxAuthorityCategoryEntity {}

    /**
     * Tax Authority GL Account
     */
    @Entity(
        name = "TaxAuthorityGlAccount",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority GL Account",
        fields = {
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthGeoId"),
            @PrimaryKey(field = "taxAuthPartyId"),
            @PrimaryKey(field = "organizationPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "TAXAUTHGLA_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "TAXAUTHGLA_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "TAXAUTHGLA_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface TaxAuthorityGlAccountEntity {}

    /**
     * Tax Authority Rate
     */
    @Entity(
        name = "TaxAuthorityRateProduct",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority Rate",
        fields = {
            @Field(name = "taxAuthorityRateSeqId", type = "id-ne"),
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "taxAuthorityRateTypeId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "titleTransferEnumId", type = "id-ne"),
            @Field(name = "minItemPrice", type = "currency-amount"),
            @Field(name = "minPurchase", type = "currency-amount"),
            @Field(name = "taxShipping", type = "indicator"),
            @Field(name = "taxPercentage", type = "fixed-point"),
            @Field(name = "taxPromotions", type = "indicator"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthorityRateSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "TAXAUTHRTEP_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthorityRateType",
                fkName = "TAXAUTHRTEP_RTTP",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthorityRateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "TAXAUTHRTEP_PSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "TAXAUTHRTEP_PCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface TaxAuthorityRateProductEntity {}

    /**
     * Tax Authority Rate Type
     */
    @Entity(
        name = "TaxAuthorityRateType",
        packageName = "org.ofbiz.accounting.tax",
        title = "Tax Authority Rate Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "taxAuthorityRateTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "taxAuthorityRateTypeId")
        }
    )
    public interface TaxAuthorityRateTypeEntity {}

    /**
     * Zip Sales Tax Lookup
     */
    @Entity(
        name = "ZipSalesRuleLookup",
        packageName = "org.ofbiz.accounting.tax",
        title = "Zip Sales Tax Lookup",
        fields = {
            @Field(name = "stateCode", type = "short-varchar"),
            @Field(name = "city", type = "short-varchar"),
            @Field(name = "county", type = "short-varchar"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "idCode", type = "short-varchar"),
            @Field(name = "taxable", type = "short-varchar"),
            @Field(name = "shipCond", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "stateCode"),
            @PrimaryKey(field = "city"),
            @PrimaryKey(field = "county"),
            @PrimaryKey(field = "fromDate")
        }
    )
    public interface ZipSalesRuleLookupEntity {}

    /**
     * Zip Sales Tax Lookup
     */
    @Entity(
        name = "ZipSalesTaxLookup",
        packageName = "org.ofbiz.accounting.tax",
        title = "Zip Sales Tax Lookup",
        fields = {
            @Field(name = "zipCode", type = "short-varchar"),
            @Field(name = "stateCode", type = "short-varchar"),
            @Field(name = "city", type = "short-varchar"),
            @Field(name = "county", type = "short-varchar"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "countyFips", type = "short-varchar"),
            @Field(name = "countyDefault", type = "indicator"),
            @Field(name = "generalDefault", type = "indicator"),
            @Field(name = "insideCity", type = "indicator"),
            @Field(name = "geoCode", type = "short-varchar"),
            @Field(name = "stateSalesTax", type = "fixed-point"),
            @Field(name = "citySalesTax", type = "fixed-point"),
            @Field(name = "cityLocalSalesTax", type = "fixed-point"),
            @Field(name = "countySalesTax", type = "fixed-point"),
            @Field(name = "countyLocalSalesTax", type = "fixed-point"),
            @Field(name = "comboSalesTax", type = "fixed-point"),
            @Field(name = "stateUseTax", type = "fixed-point"),
            @Field(name = "cityUseTax", type = "fixed-point"),
            @Field(name = "cityLocalUseTax", type = "fixed-point"),
            @Field(name = "countyUseTax", type = "fixed-point"),
            @Field(name = "countyLocalUseTax", type = "fixed-point"),
            @Field(name = "comboUseTax", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "zipCode"),
            @PrimaryKey(field = "stateCode"),
            @PrimaryKey(field = "city"),
            @PrimaryKey(field = "county"),
            @PrimaryKey(field = "fromDate")
        }
    )
    public interface ZipSalesTaxLookupEntity {}

    /**
     * Party Gl Account
     */
    @Entity(
        name = "PartyGlAccount",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Party Gl Account",
        fields = {
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "glAccountTypeId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "organizationPartyId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "glAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "PRTYGLACCT_ORGPRTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PRTYGLACCT_PRTY",
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
                fkName = "PRTYGLACCT_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "PRTYGLACCT_GLAT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PRTYGLACCT_GLACCT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface PartyGlAccountEntity {}

    /**
     * Rate Type
     */
    @Entity(
        name = "RateType",
        packageName = "org.ofbiz.accounting.rate",
        title = "Rate Type",
        defaultResourceName = "AccountingEntityLabels",
        fields = {
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "rateTypeId")
        }
    )
    public interface RateTypeEntity {}

    @Entity(
        name = "RateAmount",
        packageName = "org.ofbiz.accounting.rate",
        fields = {
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "rateCurrencyUomId", type = "id-ne"),
            @Field(name = "periodTypeId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "emplPositionTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time", description = "Describes when a rate amount will be valid. If null, valid immediately."),
            @Field(name = "thruDate", type = "date-time", description = "Describes when a rate amount will be valid untl. If null, valid indefinitly."),
            @Field(name = "rateAmount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "rateTypeId"),
            @PrimaryKey(field = "rateCurrencyUomId"),
            @PrimaryKey(field = "periodTypeId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "emplPositionTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RateType",
                fkName = "RATE_AMOUNT_RT",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "RATE_AMOUNT_RCT",
                keyMaps = {
                    @KeyMap(fieldName = "rateCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "RATE_AMOUNT_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "RATE_AMOUNT_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EmplPositionType",
                fkName = "RATE_AMOUNT_EPT",
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PeriodType",
                fkName = "RATE_AMOUNT_PT",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            )
        }
    )
    public interface RateAmountEntity {}

    /**
     * Party Rate
     */
    @Entity(
        name = "PartyRate",
        packageName = "org.ofbiz.accounting.rate",
        tableName = "PARTY_RATE_NEW",
        title = "Party Rate",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "rateTypeId", type = "id-ne"),
            @Field(name = "defaultRate", type = "indicator"),
            @Field(name = "percentageUsed", type = "floating-point", description = "The percentage of the actual hours registered in timeEntries, used for the task and invoice actuals, if the field is null 100% will be used"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "rateTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PRTY_RATE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RateType",
                fkName = "PRTY_RATE_RTTP",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            )
        }
    )
    public interface PartyRateEntity {}

    /**
     * General Ledger Account Category
     */
    @Entity(
        name = "GlAccountCategory",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Category",
        fields = {
            @Field(name = "glAccountCategoryId", type = "id-ne"),
            @Field(name = "glAccountCategoryTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountCategoryType",
                fkName = "GLACT_CAT_TP",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountCategoryTypeId")
                }
            )
        }
    )
    public interface GlAccountCategoryEntity {}

    /**
     * General Ledger Account Category Member
     */
    @Entity(
        name = "GlAccountCategoryMember",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Category Member",
        fields = {
            @Field(name = "glAccountId", type = "id-ne"),
            @Field(name = "glAccountCategoryId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "amountPercentage", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountId"),
            @PrimaryKey(field = "glAccountCategoryId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLACT_CATMBR_AC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountCategory",
                fkName = "GLACT_CATMBR_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountCategoryId")
                }
            )
        }
    )
    public interface GlAccountCategoryMemberEntity {}

    /**
     * General Ledger Account Category Type
     */
    @Entity(
        name = "GlAccountCategoryType",
        packageName = "org.ofbiz.accounting.ledger",
        title = "General Ledger Account Category Type",
        fields = {
            @Field(name = "glAccountCategoryTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "glAccountCategoryTypeId")
        }
    )
    public interface GlAccountCategoryTypeEntity {}

    /**
     * DATEV - Field definition
     * DATEV - Field definitions for different data categories (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)
     */
    @Entity(
        name = "DatevFieldDefinition",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - Field definition",
        description = "DATEV - Field definitions for different data categories (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)",
        fields = {
            @Field(name = "fieldId", type = "id-ne"),
            @Field(name = "dataCategoryId", type = "id-long"),
            @Field(name = "fieldName", type = "short-varchar"),
            @Field(name = "typeEnumId", type = "id-ne", notNull = true),
            @Field(name = "length", type = "numeric"),
            @Field(name = "maxLength", type = "numeric"),
            @Field(name = "scale", type = "numeric"),
            @Field(name = "format", type = "short-varchar"),
            @Field(name = "required", type = "indicator"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "description", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "fieldId"),
            @PrimaryKey(field = "dataCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                title = "DatevFieldType",
                keyMaps = {
                    @KeyMap(fieldName = "typeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DatevDataCategory",
                keyMaps = {
                    @KeyMap(fieldName = "dataCategoryId")
                }
            )
        }
    )
    public interface DatevFieldDefinitionEntity {}

    /**
     * DATEV - General settings
     * DATEV - General settings of the ASCII data format
     */
    @Entity(
        name = "DatevGeneralSetting",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - General settings",
        description = "DATEV - General settings of the ASCII data format",
        fields = {
            @Field(name = "dataCategoryId", type = "id-long"),
            @Field(name = "charset", type = "short-varchar"),
            @Field(name = "recordLayout", type = "short-varchar"),
            @Field(name = "fieldSeparator", type = "short-varchar"),
            @Field(name = "thousandsSeparator", type = "short-varchar"),
            @Field(name = "decimalSeparator", type = "short-varchar"),
            @Field(name = "endOfRecordSeparator", type = "short-varchar"),
            @Field(name = "dateFormat", type = "short-varchar"),
            @Field(name = "headerRow", type = "indicator"),
            @Field(name = "textDelimiter", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DatevDataCategory",
                fkName = "DATEV_GSTGS_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "dataCategoryId")
                }
            )
        }
    )
    public interface DatevGeneralSettingEntity {}

    /**
     * DATEV - Metadata
     * DATEV - Metadata. If present, first CSV row. Don't be confused with the header row. (Should be the same for all data categories)
     */
    @Entity(
        name = "DatevMetadata",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - Metadata",
        description = "DATEV - Metadata. If present, first CSV row. Don't be confused with the header row. (Should be the same for all data categories)",
        fields = {
            @Field(name = "metadataId", type = "id-vlong"),
            @Field(name = "typeEnumId", type = "id-ne", notNull = true),
            @Field(name = "length", type = "numeric"),
            @Field(name = "maxLength", type = "numeric"),
            @Field(name = "scale", type = "numeric"),
            @Field(name = "format", type = "short-varchar"),
            @Field(name = "required", type = "indicator"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "description", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "metadataId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                title = "DatevFieldType",
                keyMaps = {
                    @KeyMap(fieldName = "typeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface DatevMetadataEntity {}

    /**
     * DATEV - Field mapping to Scipio entity field
     */
    @Entity(
        name = "DatevFieldMapping",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - Field mapping to Scipio entity field",
        fields = {
            @Field(name = "fieldId", type = "id-ne"),
            @Field(name = "dataCategoryId", type = "id-long"),
            @Field(name = "entityName", type = "long-varchar"),
            @Field(name = "entityField", type = "long-varchar"),
            @Field(name = "isAttrEntity", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "fieldId"),
            @PrimaryKey(field = "dataCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DatevFieldDefinition",
                fkName = "DATEV_FIELD_DEF",
                keyMaps = {
                    @KeyMap(fieldName = "fieldId"),
                    @KeyMap(fieldName = "dataCategoryId")
                }
            )
        }
    )
    public interface DatevFieldMappingEntity {}

    /**
     * DATEV - Available Data Categories
     * DATEV - Available Data Categories (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)
     */
    @Entity(
        name = "DatevDataCategory",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - Available Data Categories",
        description = "DATEV - Available Data Categories (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)",
        fields = {
            @Field(name = "dataCategoryId", type = "id-long"),
            @Field(name = "dataCategoryName", type = "short-varchar"),
            @Field(name = "dataCategoryClass", type = "long-varchar"),
            @Field(name = "description", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataCategoryId")
        }
    )
    public interface DatevDataCategoryEntity {}

    /**
     * DATEV - Field definition version
     * DATEV - Field definition version (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)
     */
    @Entity(
        name = "DatevFieldDefinitionVersion",
        packageName = "com.ilscipio.scipio.accounting.external.datev",
        title = "DATEV - Field definition version",
        description = "DATEV - Field definition version (http://www.datev.de/dnlexom/client/app/index.html#/document/1036228/D72057595488525963)",
        fields = {
            @Field(name = "fieldId", type = "id-ne"),
            @Field(name = "dataCategoryId", type = "id-long"),
            @Field(name = "version", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "fieldId"),
            @PrimaryKey(field = "dataCategoryId"),
            @PrimaryKey(field = "version")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DatevFieldDefinition",
                fkName = "DATEV_FLDDV_DEF",
                keyMaps = {
                    @KeyMap(fieldName = "fieldId"),
                    @KeyMap(fieldName = "dataCategoryId")
                }
            )
        }
    )
    public interface DatevFieldDefinitionVersionEntity {}

    /**
     * Financial Account and Role View
     */
    @ViewEntity(
        name = "FinAccountAndRole",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account and Role View",
        members = {
            @MemberEntity(entityAlias = "FA", entityName = "FinAccount"),
            @MemberEntity(entityAlias = "FR", entityName = "FinAccountRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FA")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "FR"),
            @Alias(name = "roleTypeId", entityAlias = "FR"),
            @Alias(name = "roleFromDate", entityAlias = "FR", field = "fromDate"),
            @Alias(name = "roleThruDate", entityAlias = "FR", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FA",
                relEntityAlias = "FR",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            )
        }
    )
    public interface FinAccountAndRoleView {}

    /**
     * Financial Account Transactio Sum
     * View entity to help calculate total of financial account transactions by doing a query for the sum of all amounts             on a range of transactionDates for a given finAccountId, finAccountTransTypeId
     */
    @ViewEntity(
        name = "FinAccountTransSum",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Transactio Sum",
        description = "View entity to help calculate total of financial account transactions by doing a query for the sum of all amounts\n            on a range of transactionDates for a given finAccountId, finAccountTransTypeId",
        members = {
            @MemberEntity(entityAlias = "FAT", entityName = "FinAccountTrans")
        },
        aliases = {
            @Alias(name = "finAccountId", entityAlias = "FAT", groupBy = true),
            @Alias(name = "finAccountTransTypeId", entityAlias = "FAT"),
            @Alias(name = "transactionDate", entityAlias = "FAT"),
            @Alias(name = "amount", entityAlias = "FAT", function = AggregateFunction.SUM)
        }
    )
    public interface FinAccountTransSumView {}

    /**
     * Financial Account Authorization Sum
     * View entity to help calculate total of financial account authorizations by doing a query for the sum of all amounts             on a range of transactionDates for a given finAccountId.  Note there is no auth type to consider here, but authorizations do             have from and thru dates
     */
    @ViewEntity(
        name = "FinAccountAuthSum",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Financial Account Authorization Sum",
        description = "View entity to help calculate total of financial account authorizations by doing a query for the sum of all amounts\n            on a range of transactionDates for a given finAccountId.  Note there is no auth type to consider here, but authorizations do\n            have from and thru dates",
        members = {
            @MemberEntity(entityAlias = "FAA", entityName = "FinAccountAuth")
        },
        aliases = {
            @Alias(name = "finAccountId", entityAlias = "FAA", groupBy = true),
            @Alias(name = "authorizationDate", entityAlias = "FAA"),
            @Alias(name = "fromDate", entityAlias = "FAA"),
            @Alias(name = "thruDate", entityAlias = "FAA"),
            @Alias(name = "amount", entityAlias = "FAA", function = AggregateFunction.SUM)
        }
    )
    public interface FinAccountAuthSumView {}

    /**
     * Fixed Asset and Geo Point View
     */
    @ViewEntity(
        name = "FixedAssetAndGeoPoint",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "Fixed Asset and Geo Point View",
        members = {
            @MemberEntity(entityAlias = "FA", entityName = "FixedAsset"),
            @MemberEntity(entityAlias = "FAGPT", entityName = "FixedAssetGeoPoint"),
            @MemberEntity(entityAlias = "GPT", entityName = "GeoPoint")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GPT")
        },
        aliases = {
            @Alias(name = "fixedAssetId", entityAlias = "FA"),
            @Alias(name = "fromDate", entityAlias = "FAGPT"),
            @Alias(name = "thruDate", entityAlias = "FAGPT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FA",
                relEntityAlias = "FAGPT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @ViewLink(
                entityAlias = "FAGPT",
                relEntityAlias = "GPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FixedAssetGeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId"),
                    @KeyMap(fieldName = "geoPointId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FixedAsset",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "GeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface FixedAssetAndGeoPointView {}

    /**
     * PartyFixedAssetAssignment and RoleType View
     */
    @ViewEntity(
        name = "PartyFixedAssetAssignAndRole",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "PartyFixedAssetAssignment and RoleType View",
        members = {
            @MemberEntity(entityAlias = "PFA", entityName = "PartyFixedAssetAssignment"),
            @MemberEntity(entityAlias = "RT", entityName = "RoleType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PFA"),
            @AliasAll(entityAlias = "RT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PFA",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface PartyFixedAssetAssignAndRoleView {}

    /**
     * FixedAssetMaint and WorkEffort View
     */
    @ViewEntity(
        name = "FixedAssetMaintWorkEffort",
        packageName = "org.ofbiz.accounting.fixedasset",
        title = "FixedAssetMaint and WorkEffort View",
        members = {
            @MemberEntity(entityAlias = "FA", entityName = "FixedAsset"),
            @MemberEntity(entityAlias = "FAM", entityName = "FixedAssetMaint"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FA"),
            @AliasAll(entityAlias = "FAM"),
            @AliasAll(entityAlias = "WE", excludes = {"fixedAssetId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FAM",
                relEntityAlias = "FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @ViewLink(
                entityAlias = "FAM",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "scheduleWorkEffortId", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface FixedAssetMaintWorkEffortView {}

    /**
     * Invoice and related applications and payments
     */
    @ViewEntity(
        name = "InvoiceAndApplAndPayment",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice and related applications and payments",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "PAP", entityName = "PaymentApplication"),
            @MemberEntity(entityAlias = "PAM", entityName = "Payment")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INV"),
            @AliasAll(entityAlias = "PAP", excludes = {"billingAccountId"}),
            @AliasAll(entityAlias = "PAM", prefix = "pm")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "PAP",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @ViewLink(
                entityAlias = "PAP",
                relEntityAlias = "PAM",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            )
        }
    )
    public interface InvoiceAndApplAndPaymentView {}

    /**
     * Invoice and InvoiceType to be able to list invoices by invoiceParentType i.e. sales/purchase invoices
     */
    @ViewEntity(
        name = "InvoiceAndType",
        packageName = "org.ofbiz.accounting.invoice",
        title = "Invoice and InvoiceType to be able to list invoices by invoiceParentType i.e. sales/purchase invoices",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "IVT", entityName = "InvoiceType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INV")
        },
        aliases = {
            @Alias(name = "parentTypeId", entityAlias = "IVT"),
            @Alias(name = "invoiceTypeDesc", entityAlias = "IVT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "IVT",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceItem",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentApplication",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AcctgTrans",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            )
        }
    )
    public interface InvoiceAndTypeView {}

    @ViewEntity(
        name = "InvoiceAndRole",
        packageName = "org.ofbiz.accounting.invoice",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "INR", entityName = "InvoiceRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INV")
        },
        aliases = {
            @Alias(name = "invoiceRolePartyId", entityAlias = "INR", field = "partyId"),
            @Alias(name = "invoiceRoleTypeId", entityAlias = "INR", field = "roleTypeId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "INR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            )
        }
    )
    public interface InvoiceAndRoleView {}

    @ViewEntity(
        name = "InvoiceItemAndAssocProduct",
        packageName = "org.ofbiz.accounting.invoice",
        members = {
            @MemberEntity(entityAlias = "INTM", entityName = "InvoiceItem"),
            @MemberEntity(entityAlias = "IIA", entityName = "InvoiceItemAssoc"),
            @MemberEntity(entityAlias = "PROD", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "IIA", excludes = {"amount"})
        },
        aliases = {
            @Alias(name = "termAmount", entityAlias = "IIA", field = "amount"),
            @Alias(name = "productId", entityAlias = "PROD"),
            @Alias(name = "productName", entityAlias = "PROD"),
            @Alias(name = "amount", entityAlias = "INTM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INTM",
                relEntityAlias = "IIA",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId", relFieldName = "invoiceIdFrom"),
                    @KeyMap(fieldName = "invoiceItemSeqId", relFieldName = "invoiceItemSeqIdFrom")
                }
            ),
            @ViewLink(
                entityAlias = "INTM",
                relEntityAlias = "PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface InvoiceItemAndAssocProductView {}

    @ViewEntity(
        name = "InvItemAndOrdItem",
        packageName = "org.ofbiz.accounting.invoice",
        members = {
            @MemberEntity(entityAlias = "INVITM", entityName = "InvoiceItem"),
            @MemberEntity(entityAlias = "ORDBIL", entityName = "OrderItemBilling")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INVITM")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "ORDBIL"),
            @Alias(name = "orderItemSeqId", entityAlias = "ORDBIL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INVITM",
                relEntityAlias = "ORDBIL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface InvItemAndOrdItemView {}

    @ViewEntity(
        name = "InvoiceItemAndShipmentView",
        packageName = "org.ofbiz.accounting.invoice",
        members = {
            @MemberEntity(entityAlias = "INVITM", entityName = "InvoiceItem"),
            @MemberEntity(entityAlias = "ORDBIL", entityName = "OrderItemBilling"),
            @MemberEntity(entityAlias = "ITMISS", entityName = "ItemIssuance"),
            @MemberEntity(entityAlias = "SHIP", entityName = "Shipment")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INVITM"),
            @AliasAll(entityAlias = "SHIP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INVITM",
                relEntityAlias = "ORDBIL",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "ORDBIL",
                relEntityAlias = "ITMISS",
                keyMaps = {
                    @KeyMap(fieldName = "itemIssuanceId")
                }
            ),
            @ViewLink(
                entityAlias = "ITMISS",
                relEntityAlias = "SHIP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface InvoiceItemAndShipmentViewView {}

    /**
     * InvoiceContent Content and DataResource View
     */
    @ViewEntity(
        name = "InvoiceContentAndInfo",
        packageName = "org.ofbiz.accounting.invoice",
        title = "InvoiceContent Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "INVC", entityName = "InvoiceContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "INVC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INVC",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            )
        }
    )
    public interface InvoiceContentAndInfoView {}

    /**
     * View of AcctgTrans and Entries, for easier lookup and calculation
     */
    @ViewEntity(
        name = "AcctgTransAndEntries",
        packageName = "org.ofbiz.accounting.ledger",
        title = "View of AcctgTrans and Entries, for easier lookup and calculation",
        members = {
            @MemberEntity(entityAlias = "ATR", entityName = "AcctgTrans"),
            @MemberEntity(entityAlias = "ATT", entityName = "AcctgTransType"),
            @MemberEntity(entityAlias = "ATE", entityName = "AcctgTransEntry"),
            @MemberEntity(entityAlias = "GLA", entityName = "GlAccount"),
            @MemberEntity(entityAlias = "GLAC", entityName = "GlAccountClass")
        },
        aliases = {
            @Alias(name = "isPosted", entityAlias = "ATR"),
            @Alias(name = "glFiscalTypeId", entityAlias = "ATR"),
            @Alias(name = "acctgTransTypeId", entityAlias = "ATR"),
            @Alias(name = "transactionDate", entityAlias = "ATR"),
            @Alias(name = "postedDate", entityAlias = "ATR"),
            @Alias(name = "transDescription", entityAlias = "ATR", field = "description"),
            @Alias(name = "glJournalId", entityAlias = "ATR"),
            @Alias(name = "transTypeDescription", entityAlias = "ATT", field = "description"),
            @Alias(name = "invoiceId", entityAlias = "ATR"),
            @Alias(name = "paymentId", entityAlias = "ATR"),
            @Alias(name = "shipmentId", entityAlias = "ATR"),
            @Alias(name = "receiptId", entityAlias = "ATR"),
            @Alias(name = "inventoryItemId", entityAlias = "ATR"),
            @Alias(name = "workEffortId", entityAlias = "ATR"),
            @Alias(name = "fixedAssetId", entityAlias = "ATR"),
            @Alias(name = "physicalInventoryId", entityAlias = "ATR"),
            @Alias(name = "description", entityAlias = "ATR"),
            @Alias(name = "acctgTransId", entityAlias = "ATE"),
            @Alias(name = "acctgTransEntrySeqId", entityAlias = "ATE"),
            @Alias(name = "glAccountId", entityAlias = "ATE"),
            @Alias(name = "productId", entityAlias = "ATE"),
            @Alias(name = "debitCreditFlag", entityAlias = "ATE"),
            @Alias(name = "amount", entityAlias = "ATE"),
            @Alias(name = "currencyUomId", entityAlias = "ATE"),
            @Alias(name = "origAmount", entityAlias = "ATE"),
            @Alias(name = "origCurrencyUomId", entityAlias = "ATE"),
            @Alias(name = "organizationPartyId", entityAlias = "ATE"),
            @Alias(name = "glAccountTypeId", entityAlias = "GLA"),
            @Alias(name = "accountCode", entityAlias = "GLA"),
            @Alias(name = "accountName", entityAlias = "GLA"),
            @Alias(name = "glAccountClassId", entityAlias = "GLAC"),
            @Alias(name = "partyId", entityAlias = "ATE"),
            @Alias(name = "reconcileStatusId", entityAlias = "ATE"),
            @Alias(name = "acctgTransEntryTypeId", entityAlias = "ATE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ATR",
                relEntityAlias = "ATE",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            ),
            @ViewLink(
                entityAlias = "ATR",
                relEntityAlias = "ATT",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "ATE",
                relEntityAlias = "GLA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @ViewLink(
                entityAlias = "GLA",
                relEntityAlias = "GLAC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "glAccountClassId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "ATAE_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountClass",
                fkName = "ATAE_GLACTCLS",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AcctgTransType",
                fkName = "ATAE_ATTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Invoice",
                fkName = "ATAE_INVOICE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "ATAE_PAYMENT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlJournal",
                fkName = "ATAE_GLJOURNAL",
                keyMaps = {
                    @KeyMap(fieldName = "glJournalId")
                }
            )
        }
    )
    public interface AcctgTransAndEntriesView {}

    /**
     * Sum of AcctgTransEntry entity amounts grouped by glAccountId, debitCreditFlag
     */
    @ViewEntity(
        name = "AcctgTransEntrySums",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Sum of AcctgTransEntry entity amounts grouped by glAccountId, debitCreditFlag",
        members = {
            @MemberEntity(entityAlias = "ATE", entityName = "AcctgTransEntry"),
            @MemberEntity(entityAlias = "ACT", entityName = "AcctgTrans"),
            @MemberEntity(entityAlias = "GLA", entityName = "GlAccount")
        },
        aliases = {
            @Alias(name = "glAccountId", entityAlias = "ATE", groupBy = true),
            @Alias(name = "glAccountTypeId", entityAlias = "GLA", groupBy = true),
            @Alias(name = "glAccountClassId", entityAlias = "GLA", groupBy = true),
            @Alias(name = "accountName", entityAlias = "GLA", groupBy = true),
            @Alias(name = "accountCode", entityAlias = "GLA", groupBy = true),
            @Alias(name = "glFiscalTypeId", entityAlias = "ACT", groupBy = true),
            @Alias(name = "acctgTransTypeId", entityAlias = "ACT"),
            @Alias(name = "debitCreditFlag", entityAlias = "ATE", groupBy = true),
            @Alias(name = "amount", entityAlias = "ATE", function = AggregateFunction.SUM),
            @Alias(name = "organizationPartyId", entityAlias = "ATE"),
            @Alias(name = "isPosted", entityAlias = "ACT"),
            @Alias(name = "transactionDate", entityAlias = "ACT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ATE",
                relEntityAlias = "ACT",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            ),
            @ViewLink(
                entityAlias = "ATE",
                relEntityAlias = "GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface AcctgTransEntrySumsView {}

    /**
     * Sum of AcctgTrans entity amounts grouped by acctgTransTypeId
     */
    @ViewEntity(
        name = "AcctgTransSums",
        packageName = "org.ofbiz.accounting.ledger",
        title = "Sum of AcctgTrans entity amounts grouped by acctgTransTypeId",
        members = {
            @MemberEntity(entityAlias = "ATE", entityName = "AcctgTransEntry"),
            @MemberEntity(entityAlias = "ACT", entityName = "AcctgTrans")
        },
        aliases = {
            @Alias(name = "glFiscalTypeId", entityAlias = "ACT", groupBy = true),
            @Alias(name = "acctgTransTypeId", entityAlias = "ACT", groupBy = true),
            @Alias(name = "debitCreditFlag", entityAlias = "ATE", groupBy = true),
            @Alias(name = "amount", entityAlias = "ATE", function = AggregateFunction.SUM),
            @Alias(name = "organizationPartyId", entityAlias = "ATE"),
            @Alias(name = "isPosted", entityAlias = "ACT"),
            @Alias(name = "transactionDate", entityAlias = "ACT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ATE",
                relEntityAlias = "ACT",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            )
        }
    )
    public interface AcctgTransSumsView {}

    /**
     * View of GL Account and its History, for lookup and calculation
     */
    @ViewEntity(
        name = "GlAccountAndHistory",
        packageName = "org.ofbiz.accounting.ledger",
        title = "View of GL Account and its History, for lookup and calculation",
        members = {
            @MemberEntity(entityAlias = "GLA", entityName = "GlAccount"),
            @MemberEntity(entityAlias = "GLAH", entityName = "GlAccountHistory"),
            @MemberEntity(entityAlias = "GLAC", entityName = "GlAccountClass")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GLA"),
            @AliasAll(entityAlias = "GLAH")
        },
        aliases = {
            @Alias(name = "glAccountClassId", entityAlias = "GLAC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GLA",
                relEntityAlias = "GLAH",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @ViewLink(
                entityAlias = "GLA",
                relEntityAlias = "GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountClassId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLAAH_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountClass",
                fkName = "GLAAH_GLACTCLS",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountClassId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountHistory",
                fkName = "GLAAH_GLAH",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId"),
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            )
        }
    )
    public interface GlAccountAndHistoryView {}

    /**
     * View of GL Account and its History totals
     */
    @ViewEntity(
        name = "GlAccountAndHistoryTotals",
        packageName = "org.ofbiz.accounting.ledger",
        title = "View of GL Account and its History totals",
        members = {
            @MemberEntity(entityAlias = "GLA", entityName = "GlAccount"),
            @MemberEntity(entityAlias = "GLAH", entityName = "GlAccountHistory")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GLA", groupBy = true),
            @AliasAll(entityAlias = "GLAH", groupBy = true)
        },
        aliases = {
            @Alias(name = "totalPostedDebits", entityAlias = "GLAH", field = "postedDebits", function = AggregateFunction.SUM),
            @Alias(name = "totalPostedCredits", entityAlias = "GLAH", field = "postedCredits", function = AggregateFunction.SUM),
            @Alias(name = "totalEndingBalance", entityAlias = "GLAH", field = "endingBalance", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GLA",
                relEntityAlias = "GLAH",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLAAHT_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountHistory",
                fkName = "GLAAHT_GLAH",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId"),
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            )
        }
    )
    public interface GlAccountAndHistoryTotalsView {}

    /**
     * View of GL Account Organization and the GlAccount and class, for lookup and calculation
     */
    @ViewEntity(
        name = "GlAccountOrganizationAndClass",
        packageName = "org.ofbiz.accounting.ledger",
        title = "View of GL Account Organization and the GlAccount and class, for lookup and calculation",
        members = {
            @MemberEntity(entityAlias = "GLAO", entityName = "GlAccountOrganization"),
            @MemberEntity(entityAlias = "GLA", entityName = "GlAccount")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GLAO"),
            @AliasAll(entityAlias = "GLA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GLAO",
                relEntityAlias = "GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "GLAOC_GLA",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountOrganization",
                fkName = "GLAOC_GLAO",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId")
                }
            )
        }
    )
    public interface GlAccountOrganizationAndClassView {}

    /**
     * Billing Account and Role
     */
    @ViewEntity(
        name = "BillingAccountAndRole",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account and Role",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "BA", entityName = "BillingAccount"),
            @MemberEntity(entityAlias = "BR", entityName = "BillingAccountRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "BR")
        },
        aliases = {
            @Alias(name = "billingAccountId", entityAlias = "BA"),
            @Alias(name = "accountLimit", entityAlias = "BA"),
            @Alias(name = "accountCurrencyUomId", entityAlias = "BA"),
            @Alias(name = "contactMechId", entityAlias = "BA"),
            @Alias(name = "accountFromDate", entityAlias = "BA", field = "fromDate"),
            @Alias(name = "accountThruDate", entityAlias = "BA", field = "thruDate"),
            @Alias(name = "description", entityAlias = "BA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "BA",
                relEntityAlias = "BR",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "BillingAccountRole",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Invoice",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentApplication",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface BillingAccountAndRoleView {}

    /**
     * Billing Account Role and Address
     * Note that the ContactMech is not linked into this view and the PADDR is not optional, this way we naturally only get postal address entries
     */
    @ViewEntity(
        name = "BillingAccountRoleAndAddress",
        packageName = "org.ofbiz.accounting.payment",
        title = "Billing Account Role and Address",
        description = "Note that the ContactMech is not linked into this view and the PADDR is not optional, this way we naturally only get postal address entries",
        members = {
            @MemberEntity(entityAlias = "BAR", entityName = "BillingAccountRole"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PADDR", entityName = "PostalAddress")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "BAR"),
            @AliasAll(entityAlias = "PADDR")
        },
        aliases = {
            @Alias(name = "pcmFromDate", entityAlias = "PCM", field = "fromDate"),
            @Alias(name = "pcmThruDate", entityAlias = "PCM", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "BAR",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PADDR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface BillingAccountRoleAndAddressView {}

    /**
     * Payment PaymentType PaymentMethodType StatusItem and Party Name View
     */
    @ViewEntity(
        name = "PaymentAndTypePartyNameView",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment PaymentType PaymentMethodType StatusItem and Party Name View",
        members = {
            @MemberEntity(entityAlias = "PY", entityName = "Payment"),
            @MemberEntity(entityAlias = "FPNV", entityName = "PartyNameView"),
            @MemberEntity(entityAlias = "TPNV", entityName = "PartyNameView"),
            @MemberEntity(entityAlias = "TY", entityName = "PaymentType"),
            @MemberEntity(entityAlias = "PMT", entityName = "PaymentMethodType"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PY")
        },
        aliases = {
            @Alias(name = "partyFromFirstName", entityAlias = "FPNV", field = "firstName"),
            @Alias(name = "partyFromLastName", entityAlias = "FPNV", field = "lastName"),
            @Alias(name = "partyFromGroupName", entityAlias = "FPNV", field = "groupName"),
            @Alias(name = "partyToFirstName", entityAlias = "TPNV", field = "firstName"),
            @Alias(name = "partyToLastName", entityAlias = "TPNV", field = "lastName"),
            @Alias(name = "partyToGroupName", entityAlias = "TPNV", field = "groupName"),
            @Alias(name = "paymentTypeDesc", entityAlias = "TY", field = "description"),
            @Alias(name = "parentPaymentTypeId", entityAlias = "TY", field = "parentTypeId"),
            @Alias(name = "statusDesc", entityAlias = "SI", field = "description"),
            @Alias(name = "paymentMethodTypeDesc", entityAlias = "PMT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "FPNV",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "TPNV",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "TY",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            )
        }
    )
    public interface PaymentAndTypePartyNameViewView {}

    /**
     * Payment and Payment type View
     */
    @ViewEntity(
        name = "PaymentAndType",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment and Payment type View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PY", entityName = "Payment"),
            @MemberEntity(entityAlias = "TY", entityName = "PaymentType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PY"),
            @AliasAll(entityAlias = "TY", excludes = {"paymentTypeId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "TY",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentApplication",
                fkName = "PAYTYPE_PAY",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PaymentApplication",
                title = "to",
                fkName = "PAYTYPE_TOPAY",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId", relFieldName = "toPaymentId")
                }
            )
        }
    )
    public interface PaymentAndTypeView {}

    /**
     * Payment, Payment type and CreadiCard View
     */
    @ViewEntity(
        name = "PaymentAndTypeAndCreditCard",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment, Payment type and CreadiCard View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PAT", entityName = "PaymentAndType"),
            @MemberEntity(entityAlias = "CC", entityName = "CreditCard")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PAT"),
            @AliasAll(entityAlias = "CC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PAT",
                relEntityAlias = "CC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId", relFieldName = "paymentMethodId")
                }
            )
        }
    )
    public interface PaymentAndTypeAndCreditCardView {}

    /**
     * Payment and Application View
     */
    @ViewEntity(
        name = "PaymentAndApplication",
        packageName = "org.ofbiz.accounting.payment",
        title = "Payment and Application View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PY", entityName = "Payment"),
            @MemberEntity(entityAlias = "PA", entityName = "PaymentApplication")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PY", excludes = {"overrideGlAccountId"}),
            @AliasAll(entityAlias = "PA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Payment",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentApplication",
                keyMaps = {
                    @KeyMap(fieldName = "paymentApplicationId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentGatewayResponse",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            )
        }
    )
    public interface PaymentAndApplicationView {}

    /**
     * PaymentContent Content and DataResource View
     */
    @ViewEntity(
        name = "PaymentContentAndInfo",
        packageName = "org.ofbiz.accounting.payment",
        title = "PaymentContent Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "PAYC", entityName = "PaymentContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PAYC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PAYC",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            )
        }
    )
    public interface PaymentContentAndInfoView {}

    /**
     * PaymentMethod and CreditCard View
     */
    @ViewEntity(
        name = "PaymentMethodAndCreditCard",
        packageName = "org.ofbiz.accounting.payment",
        title = "PaymentMethod and CreditCard View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PM", entityName = "PaymentMethod"),
            @MemberEntity(entityAlias = "CC", entityName = "CreditCard")
        },
        aliases = {
            @Alias(name = "paymentMethodId", entityAlias = "PM"),
            @Alias(name = "paymentMethodTypeId", entityAlias = "PM"),
            @Alias(name = "partyId", entityAlias = "PM"),
            @Alias(name = "glAccountId", entityAlias = "PM"),
            @Alias(name = "fromDate", entityAlias = "PM"),
            @Alias(name = "thruDate", entityAlias = "PM"),
            @Alias(name = "description", entityAlias = "PM"),
            @Alias(name = "cardType", entityAlias = "CC"),
            @Alias(name = "cardNumber", entityAlias = "CC"),
            @Alias(name = "expireDate", entityAlias = "CC"),
            @Alias(name = "companyNameOnCard", entityAlias = "CC"),
            @Alias(name = "titleOnCard", entityAlias = "CC"),
            @Alias(name = "firstNameOnCard", entityAlias = "CC"),
            @Alias(name = "lastNameOnCard", entityAlias = "CC"),
            @Alias(name = "suffixOnCard", entityAlias = "CC"),
            @Alias(name = "contactMechId", entityAlias = "CC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PM",
                relEntityAlias = "CC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethod",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CreditCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PaymentMethodAndCreditCardView {}

    /**
     * PaymentMethod and EftAccount View
     */
    @ViewEntity(
        name = "PaymentMethodAndEftAccount",
        packageName = "org.ofbiz.accounting.payment",
        title = "PaymentMethod and EftAccount View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PM", entityName = "PaymentMethod"),
            @MemberEntity(entityAlias = "EA", entityName = "EftAccount")
        },
        aliases = {
            @Alias(name = "paymentMethodId", entityAlias = "PM"),
            @Alias(name = "paymentMethodTypeId", entityAlias = "PM"),
            @Alias(name = "partyId", entityAlias = "PM"),
            @Alias(name = "glAccountId", entityAlias = "PM"),
            @Alias(name = "fromDate", entityAlias = "PM"),
            @Alias(name = "thruDate", entityAlias = "PM"),
            @Alias(name = "bankName", entityAlias = "EA"),
            @Alias(name = "routingNumber", entityAlias = "EA"),
            @Alias(name = "accountType", entityAlias = "EA"),
            @Alias(name = "accountNumber", entityAlias = "EA"),
            @Alias(name = "nameOnAccount", entityAlias = "EA"),
            @Alias(name = "companyNameOnAccount", entityAlias = "EA"),
            @Alias(name = "contactMechId", entityAlias = "EA"),
            @Alias(name = "yearsAtBank", entityAlias = "EA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PM",
                relEntityAlias = "EA",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethod",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EftAccount",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PaymentMethodAndEftAccountView {}

    /**
     * PaymentMethod and GiftCard View
     */
    @ViewEntity(
        name = "PaymentMethodAndGiftCard",
        packageName = "org.ofbiz.accounting.payment",
        title = "PaymentMethod and GiftCard View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PM", entityName = "PaymentMethod"),
            @MemberEntity(entityAlias = "GC", entityName = "GiftCard")
        },
        aliases = {
            @Alias(name = "paymentMethodId", entityAlias = "PM"),
            @Alias(name = "paymentMethodTypeId", entityAlias = "PM"),
            @Alias(name = "partyId", entityAlias = "PM"),
            @Alias(name = "glAccountId", entityAlias = "PM"),
            @Alias(name = "fromDate", entityAlias = "PM"),
            @Alias(name = "thruDate", entityAlias = "PM"),
            @Alias(name = "cardNumber", entityAlias = "GC"),
            @Alias(name = "pinNumber", entityAlias = "GC"),
            @Alias(name = "expireDate", entityAlias = "GC"),
            @Alias(name = "contactMechId", entityAlias = "GC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PM",
                relEntityAlias = "GC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethod",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PaymentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "GiftCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PaymentMethodAndGiftCardView {}

    @ViewEntity(
        name = "PartyTaxAuthInfoAndDetail",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "PTAI", entityName = "PartyTaxAuthInfo"),
            @MemberEntity(entityAlias = "PG", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "GEO", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTAI"),
            @AliasAll(entityAlias = "PG"),
            @AliasAll(entityAlias = "GEO")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTAI",
                relEntityAlias = "PG",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTAI",
                relEntityAlias = "GEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            )
        }
    )
    public interface PartyTaxAuthInfoAndDetailView {}

    @ViewEntity(
        name = "TaxAuthorityAndDetail",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "TA", entityName = "TaxAuthority"),
            @MemberEntity(entityAlias = "PG", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "GEO", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TA"),
            @AliasAll(entityAlias = "PG"),
            @AliasAll(entityAlias = "GEO")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TA",
                relEntityAlias = "PG",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "TA",
                relEntityAlias = "GEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            )
        }
    )
    public interface TaxAuthorityAndDetailView {}

    @ViewEntity(
        name = "TaxAuthorityAndGeo",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "TA", entityName = "TaxAuthority"),
            @MemberEntity(entityAlias = "GEO", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TA"),
            @AliasAll(entityAlias = "GEO")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TA",
                relEntityAlias = "GEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            )
        }
    )
    public interface TaxAuthorityAndGeoView {}

    @ViewEntity(
        name = "TaxAuthorityAndPartyGroup",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "TA", entityName = "TaxAuthority"),
            @MemberEntity(entityAlias = "PG", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TA"),
            @AliasAll(entityAlias = "PG")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TA",
                relEntityAlias = "PG",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface TaxAuthorityAndPartyGroupView {}

    @ViewEntity(
        name = "TaxAuthorityAndPartyNameView",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "TA", entityName = "TaxAuthority"),
            @MemberEntity(entityAlias = "PG", entityName = "PartyNameView")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TA"),
            @AliasAll(entityAlias = "PG")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TA",
                relEntityAlias = "PG",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface TaxAuthorityAndPartyNameViewView {}

    @ViewEntity(
        name = "TaxAuthorityCategoryView",
        packageName = "org.ofbiz.accounting.tax",
        members = {
            @MemberEntity(entityAlias = "TAC", entityName = "TaxAuthorityCategory"),
            @MemberEntity(entityAlias = "PC", entityName = "ProductCategory")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TAC"),
            @AliasAll(entityAlias = "PC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TAC",
                relEntityAlias = "PC",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface TaxAuthorityCategoryViewView {}

    /**
     * For viewing balances of tax authority GL accounts
     */
    @ViewEntity(
        name = "TaxAuthorityGlAccountBalance",
        packageName = "org.ofbiz.accounting.tax",
        title = "For viewing balances of tax authority GL accounts",
        members = {
            @MemberEntity(entityAlias = "TAGA", entityName = "TaxAuthorityGlAccount"),
            @MemberEntity(entityAlias = "GLAO", entityName = "GlAccountOrganization"),
            @MemberEntity(entityAlias = "PAP", entityName = "PartyAcctgPreference")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TAGA"),
            @AliasAll(entityAlias = "GLAO")
        },
        aliases = {
            @Alias(name = "baseCurrencyUomId", entityAlias = "PAP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TAGA",
                relEntityAlias = "GLAO",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId")
                }
            ),
            @ViewLink(
                entityAlias = "TAGA",
                relEntityAlias = "PAP",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface TaxAuthorityGlAccountBalanceView {}

    /**
     * GlAccountOrganization, AcctgTransEntry, and AccTrans View Entity Group-By organizationPartyId, glAccountId, debitCreditFlag
     */
    @ViewEntity(
        name = "GlAccOrgAndAcctgTransAndEntry",
        packageName = "org.ofbiz.accounting.ledger",
        title = "GlAccountOrganization, AcctgTransEntry, and AccTrans View Entity Group-By organizationPartyId, glAccountId, debitCreditFlag",
        members = {
            @MemberEntity(entityAlias = "GAO", entityName = "GlAccountOrganization"),
            @MemberEntity(entityAlias = "ATE", entityName = "AcctgTransEntry"),
            @MemberEntity(entityAlias = "ATR", entityName = "AcctgTrans")
        },
        aliases = {
            @Alias(name = "glAccountId", entityAlias = "GAO", groupBy = true),
            @Alias(name = "debitCreditFlag", entityAlias = "ATE", groupBy = true),
            @Alias(name = "isPosted", entityAlias = "ATR", groupBy = true),
            @Alias(name = "transactionDate", entityAlias = "ATR", groupBy = true),
            @Alias(name = "acctgTransId", entityAlias = "ATE", groupBy = true),
            @Alias(name = "organizationPartyId", entityAlias = "ATE", groupBy = true),
            @Alias(name = "totalAmount", entityAlias = "ATE", field = "amount", function = AggregateFunction.SUM),
            @Alias(name = "fromDate", entityAlias = "GAO", groupBy = true),
            @Alias(name = "thruDate", entityAlias = "GAO", groupBy = true)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GAO",
                relEntityAlias = "ATE",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId"),
                    @KeyMap(fieldName = "organizationPartyId")
                }
            ),
            @ViewLink(
                entityAlias = "ATE",
                relEntityAlias = "ATR",
                keyMaps = {
                    @KeyMap(fieldName = "acctgTransId")
                }
            )
        }
    )
    public interface GlAccOrgAndAcctgTransAndEntryView {}

    @ViewEntity(
        name = "RateAmountAndRelations",
        packageName = "org.ofbiz.accounting.rate",
        members = {
            @MemberEntity(entityAlias = "RA", entityName = "RateAmount"),
            @MemberEntity(entityAlias = "RT", entityName = "RateType"),
            @MemberEntity(entityAlias = "PT", entityName = "PeriodType"),
            @MemberEntity(entityAlias = "PN", entityName = "PartyNameView"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "EPT", entityName = "EmplPositionType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "RA")
        },
        aliases = {
            @Alias(name = "rateDescription", entityAlias = "RT", field = "description"),
            @Alias(name = "periodDescription", entityAlias = "PT", field = "description"),
            @Alias(name = "firstName", entityAlias = "PN"),
            @Alias(name = "middleName", entityAlias = "PN"),
            @Alias(name = "lastName", entityAlias = "PN"),
            @Alias(name = "groupName", entityAlias = "PN"),
            @Alias(name = "employeePositionDescription", entityAlias = "EPT", field = "description"),
            @Alias(name = "workEffortName", entityAlias = "WE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RA",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "rateTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "RA",
                relEntityAlias = "PT",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "RA",
                relEntityAlias = "PN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "RA",
                relEntityAlias = "WE",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "RA",
                relEntityAlias = "EPT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "emplPositionTypeId")
                }
            )
        }
    )
    public interface RateAmountAndRelationsView {}

    /**
     * Payment Group Member, Payment and FinAccountTrans view
     */
    @ViewEntity(
        name = "PmtGrpMembrPaymentAndFinAcctTrans",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "Payment Group Member, Payment and FinAccountTrans view",
        members = {
            @MemberEntity(entityAlias = "PGM", entityName = "PaymentGroupMember"),
            @MemberEntity(entityAlias = "PY", entityName = "Payment"),
            @MemberEntity(entityAlias = "FAT", entityName = "FinAccountTrans")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PGM"),
            @AliasAll(entityAlias = "PY")
        },
        aliases = {
            @Alias(name = "finAccountId", entityAlias = "FAT"),
            @Alias(name = "partyId", entityAlias = "FAT"),
            @Alias(name = "finAccountTransStatusId", entityAlias = "FAT", field = "statusId"),
            @Alias(name = "finAccountTransAmount", entityAlias = "FAT", field = "amount"),
            @Alias(name = "glReconciliationId", entityAlias = "FAT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PGM",
                relEntityAlias = "PY",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @ViewLink(
                entityAlias = "PY",
                relEntityAlias = "FAT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransId")
                }
            )
        }
    )
    public interface PmtGrpMembrPaymentAndFinAcctTransView {}

    /**
     * PaymentMethod and FinAccount view
     */
    @ViewEntity(
        name = "PaymentMethodAndFinAccount",
        packageName = "org.ofbiz.accounting.finaccount",
        title = "PaymentMethod and FinAccount view",
        members = {
            @MemberEntity(entityAlias = "PM", entityName = "PaymentMethod"),
            @MemberEntity(entityAlias = "FA", entityName = "FinAccount")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FA"),
            @AliasAll(entityAlias = "PM", excludes = {"finAccountId", "fromDate", "thruDate"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FA",
                relEntityAlias = "PM",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            )
        }
    )
    public interface PaymentMethodAndFinAccountView {}

    @ExtendEntity(
        name = "TaxAuthorityRateProduct",
        fields = {
            @Field(name = "revenueGlAccountId", type = "id"),
            @Field(name = "taxGlAccountId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "GlAccount",
                title = "Revenue",
                fkName = "TAXRATPR_REV_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "revenueGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "GlAccount",
                title = "Tax",
                fkName = "TAXRATPR_TAX_GLAC",
                keyMaps = {
                    @KeyMap(fieldName = "taxGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface TaxAuthorityRateProductExtension {}

    @ExtendEntity(
        name = "InventoryItem",
        fields = {
            @Field(name = "fixedAssetId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                title = "FixedAsset",
                fkName = "IYIM_FAST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            )
        }
    )
    public interface InventoryItemExtension {}

}
