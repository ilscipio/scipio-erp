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
public class ViewEntities {

    /**
     * WorkEffort for use in tree relationships
     */
    @ViewEntity(
        name = "WorkEffortAndChild",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort for use in tree relationships",
        members = {
            @MemberEntity(entityAlias = "WEP", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPH", entityName = "WorkEffort")
        },
        aliases = {
            @Alias(name = "workEffortId", entityAlias = "WEP"),
            @Alias(name = "workEffortName", entityAlias = "WEP"),
            @Alias(name = "workEffortTypeId", entityAlias = "WEP"),
            @Alias(name = "workEffortParentId", entityAlias = "WEP"),
            @Alias(name = "currentStatusId", entityAlias = "WEP"),
            @Alias(name = "childWorkEffortId", entityAlias = "WEPH", field = "workEffortId"),
            @Alias(name = "childWorkEffortName", entityAlias = "WEPH", field = "workEffortName"),
            @Alias(name = "childWorkEffortTypeId", entityAlias = "WEPH", field = "workEffortTypeId"),
            @Alias(name = "childCurrentStatusId", entityAlias = "WEPH", field = "currentStatusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEP",
                relEntityAlias = "WEPH",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId", relFieldName = "workEffortParentId")
                }
            )
        }
    )
    public interface WorkEffortAndChildView {}

    /**
     * WorkEffort Requirement View
     */
    @ViewEntity(
        name = "WorkEffortAndFulfillment",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort Requirement View",
        members = {
            @MemberEntity(entityAlias = "WEF", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WRF", entityName = "WorkRequirementFulfillment"),
            @MemberEntity(entityAlias = "REQ", entityName = "Requirement")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEF", excludes = {"description", "fixedAssetId", "facilityId"}),
            @AliasAll(entityAlias = "WRF"),
            @AliasAll(entityAlias = "REQ", excludes = {"facilityId", "fixedAssetId", "description", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        aliases = {
            @Alias(name = "workEffortDescription", entityAlias = "WEF", field = "description"),
            @Alias(name = "workEffortFixedAssetId", entityAlias = "WEF", field = "fixedAssetId"),
            @Alias(name = "workEffortFacilityId", entityAlias = "WEF", field = "facilityId"),
            @Alias(name = "requirementFacilityId", entityAlias = "REQ", field = "facilityId"),
            @Alias(name = "requirementFixedAssetId", entityAlias = "REQ", field = "fixedAssetId"),
            @Alias(name = "requirementDescription", entityAlias = "REQ", field = "description"),
            @Alias(name = "requirementCreationDate", entityAlias = "REQ", field = "createdDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEF",
                relEntityAlias = "WRF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WRF",
                relEntityAlias = "REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Requirement",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortType",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                title = "Parent",
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
                type = RelationType.MANY,
                relEntityName = "WorkEffortAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkOrderItemFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                title = "Current",
                keyMaps = {
                    @KeyMap(fieldName = "currentStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                title = "Scope",
                keyMaps = {
                    @KeyMap(fieldName = "scopeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Money",
                keyMaps = {
                    @KeyMap(fieldName = "moneyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RecurrenceInfo",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RuntimeData",
                keyMaps = {
                    @KeyMap(fieldName = "runtimeDataId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "NoteData",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId", relFieldName = "workEffortIdFrom")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId", relFieldName = "workEffortIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortStatus",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteItem",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkRequirementFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "TimeEntry",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortDeliverableProd",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortBilling",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "RateAmount",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CommunicationEventWorkEff",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortGoodStandard",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortFixedAssetStd",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortFixedAssetAssign",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortInventoryProduced",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortInventoryAssign",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortSkillStandard",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAndFulfillmentView {}

    /**
     * Work Effort And Fixed Asset Assignment View
     */
    @ViewEntity(
        name = "WorkEffortAndFixedAssetAssign",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Fixed Asset Assignment View",
        members = {
            @MemberEntity(entityAlias = "WEFAA", entityName = "WorkEffortFixedAssetAssign"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "FA", entityName = "FixedAsset")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEFAA"),
            @AliasAll(entityAlias = "WE", excludes = {"fixedAssetId"}),
            @AliasAll(entityAlias = "FA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEFAA",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WEFAA",
                relEntityAlias = "FA",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FixedAsset",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
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
                relEntityName = "StatusItem",
                title = "Availability",
                keyMaps = {
                    @KeyMap(fieldName = "availabilityStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortAndFixedAssetAssignView {}

    /**
     * Work Effort And Party Assignment
     */
    @ViewEntity(
        name = "WorkEffortAndPartyAssign",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Party Assignment",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "WEPA", excludes = {"facilityId"})
        },
        aliases = {
            @Alias(name = "partyAssignFacilityId", entityAlias = "WEPA", field = "facilityId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortType",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
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
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface WorkEffortAndPartyAssignView {}

    /**
     * Work Effort And Party Assignment
     */
    @ViewEntity(
        name = "WorkEffortAndPartyAssignAndType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Party Assignment",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "WETY", entityName = "WorkEffortType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "WEPA", excludes = {"facilityId"})
        },
        aliases = {
            @Alias(name = "partyAssignFacilityId", entityAlias = "WEPA", field = "facilityId"),
            @Alias(name = "parentTypeId", entityAlias = "WETY")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WETY",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortType",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortTypeId")
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
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface WorkEffortAndPartyAssignAndTypeView {}

    /**
     * Work Effort Party Assignment And Roletype
     * To be able to have a dropdown listing with all roles a party has on this workeffort.
     */
    @ViewEntity(
        name = "WorkEffortPartyAssignAndRoleType",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Party Assignment And Roletype",
        description = "To be able to have a dropdown listing with all roles a party has on this workeffort.",
        members = {
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "RT", entityName = "RoleType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEPA"),
            @AliasAll(entityAlias = "RT", excludes = {"roleTypeId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEPA",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface WorkEffortPartyAssignAndRoleTypeView {}

    /**
     * Work Effort Association Entity with Name
     */
    @ViewEntity(
        name = "WorkEffortAssocView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association Entity with Name",
        members = {
            @MemberEntity(entityAlias = "WA", entityName = "WorkEffortAssoc"),
            @MemberEntity(entityAlias = "WETO", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WA")
        },
        aliases = {
            @Alias(name = "workEffortToName", entityAlias = "WETO", field = "workEffortName"),
            @Alias(name = "workEffortToSetup", entityAlias = "WETO", field = "estimatedSetupMillis"),
            @Alias(name = "workEffortToRun", entityAlias = "WETO", field = "estimatedMilliSeconds"),
            @Alias(name = "workEffortToParentId", entityAlias = "WETO", field = "workEffortParentId"),
            @Alias(name = "workEffortToCurrentStatusId", entityAlias = "WETO", field = "currentStatusId"),
            @Alias(name = "workEffortToWorkEffortPurposeTypeId", entityAlias = "WETO", field = "workEffortPurposeTypeId"),
            @Alias(name = "workEffortToEstimatedStartDate", entityAlias = "WETO", field = "estimatedStartDate"),
            @Alias(name = "workEffortToEstimatedCompletionDate", entityAlias = "WETO", field = "estimatedCompletionDate"),
            @Alias(name = "workEffortToActualStartDate", entityAlias = "WETO", field = "actualStartDate"),
            @Alias(name = "workEffortToActualCompletionDate", entityAlias = "WETO", field = "actualCompletionDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WA",
                relEntityAlias = "WETO",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdTo", relFieldName = "workEffortId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                title = "From",
                fkName = "WK_EFFRTASSV_FWE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdFrom", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAssocViewView {}

    /**
     * Work Effort Association From (Parent) View
     */
    @ViewEntity(
        name = "WorkEffortAssocFromView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association From (Parent) View",
        members = {
            @MemberEntity(entityAlias = "WEA", entityName = "WorkEffortAssoc"),
            @MemberEntity(entityAlias = "WEFROM", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEA"),
            @AliasAll(entityAlias = "WEFROM", excludes = {"sequenceNum"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEA",
                relEntityAlias = "WEFROM",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdFrom", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAssocFromViewView {}

    /**
     * Work Effort Association To (Child) View
     */
    @ViewEntity(
        name = "WorkEffortAssocToView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Association To (Child) View",
        members = {
            @MemberEntity(entityAlias = "WEA", entityName = "WorkEffortAssoc"),
            @MemberEntity(entityAlias = "WETO", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEA"),
            @AliasAll(entityAlias = "WETO", excludes = {"sequenceNum"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEA",
                relEntityAlias = "WETO",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortIdTo", relFieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAssocToViewView {}

    /**
     * Find Work Efforts View
     */
    @ViewEntity(
        name = "WorkEffortFindView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Find Work Efforts View",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEA", entityName = "WorkEffortAssoc"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "WEFAA", entityName = "WorkEffortFixedAssetAssign")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE", excludes = {"workEffortParentId", "fixedAssetId"})
        },
        aliases = {
            @Alias(name = "workEffortParentId", entityAlias = "WEA", field = "workEffortIdFrom"),
            @Alias(name = "partyId", entityAlias = "WEPA", field = "partyId"),
            @Alias(name = "fixedAssetId", entityAlias = "WEFAA", field = "fixedAssetId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId", relFieldName = "workEffortIdTo")
                }
            ),
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEFAA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortFindViewView {}

    /**
     * Work Effort Note And Note Data
     */
    @ViewEntity(
        name = "WorkEffortNoteAndData",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Note And Note Data",
        members = {
            @MemberEntity(entityAlias = "WEN", entityName = "WorkEffortNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "workEffortId", entityAlias = "WEN"),
            @Alias(name = "internalNote", entityAlias = "WEN"),
            @Alias(name = "noteId", entityAlias = "WEN"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteParty", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEN",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "NoteData",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "noteParty", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "noteParty", relFieldName = "partyId")
                }
            )
        }
    )
    public interface WorkEffortNoteAndDataView {}

    /**
     * Work Effort And Party Assignment By Group
     * Includes PartyRelationship Link so that a partyId can be specified to find all PartyAssignments for all groups the party is in.
     */
    @ViewEntity(
        name = "WorkEffortPartyAssignByGroup",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Party Assignment By Group",
        description = "Includes PartyRelationship Link so that a partyId can be specified to find all PartyAssignments for all groups the party is in.",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "PREL", entityName = "PartyRelationship")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "WEPA", excludes = {"facilityId"}),
            @AliasAll(entityAlias = "PREL", excludes = {"partyIdTo", "partyIdFrom", "fromDate", "thruDate", "statusId", "comments"})
        },
        aliases = {
            @Alias(name = "partyAssignFacilityId", entityAlias = "WEPA", field = "facilityId"),
            @Alias(name = "partyId", entityAlias = "PREL", field = "partyIdTo"),
            @Alias(name = "groupPartyId", entityAlias = "PREL", field = "partyIdFrom"),
            @Alias(name = "prelFromDate", entityAlias = "PREL", field = "fromDate"),
            @Alias(name = "prelThruDate", entityAlias = "PREL", field = "thruDate"),
            @Alias(name = "prelStatusId", entityAlias = "PREL", field = "statusId"),
            @Alias(name = "prelComments", entityAlias = "PREL", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WEPA",
                relEntityAlias = "PREL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdFrom")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
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
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface WorkEffortPartyAssignByGroupView {}

    /**
     * Work Effort And Party Assignment By Role
     * Also includes PartyRole Link so that if a partyId is specified it will find all PartyAssignments for all roles a party is in; does not link on the partyId of the PartyAssignment
     */
    @ViewEntity(
        name = "WorkEffortPartyAssignByRole",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Party Assignment By Role",
        description = "Also includes PartyRole Link so that if a partyId is specified it will find all PartyAssignments for all roles a party is in; does not link on the partyId of the PartyAssignment",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "WEPA", excludes = {"partyId", "facilityId"}),
            @AliasAll(entityAlias = "PR")
        },
        aliases = {
            @Alias(name = "wepaPartyId", entityAlias = "WEPA", field = "partyId"),
            @Alias(name = "partyAssignFacilityId", entityAlias = "WEPA", field = "facilityId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WEPA",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
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
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface WorkEffortPartyAssignByRoleView {}

    /**
     * WorkEffort and related WorkEffortGoodStandard
     * WorkEffort and its WorkEffortGoodStandard
     */
    @ViewEntity(
        name = "WorkEffortAndGoods",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort and related WorkEffortGoodStandard",
        description = "WorkEffort and its WorkEffortGoodStandard",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEGS", entityName = "WorkEffortGoodStandard")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "WEGS"),
            @Alias(name = "workEffortGoodStdTypeId", entityAlias = "WEGS"),
            @Alias(name = "statusId", entityAlias = "WEGS"),
            @Alias(name = "estimatedQuantity", entityAlias = "WEGS")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEGS",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAndGoodsView {}

    /**
     * WorkEffort and related WorkEffortGoodStandard with Product
     */
    @ViewEntity(
        name = "WorkEffortProductGoods",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort and related WorkEffortGoodStandard with Product",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEGS", entityName = "WorkEffortGoodStandard"),
            @MemberEntity(entityAlias = "PROD", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "WEGS"),
            @AliasAll(entityAlias = "PROD", excludes = {"facilityId", "description", "reserv2ndPPPerc", "reservNthPPPerc", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEGS",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WEGS",
                relEntityAlias = "PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface WorkEffortProductGoodsView {}

    /**
     * WorkEffort and Content and DataResource View
     */
    @ViewEntity(
        name = "WorkEffortAndContentDataResource",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort and Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "WECO", entityName = "WorkEffortContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WECO"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WECO",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                relOptional = true,
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
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
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
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface WorkEffortAndContentDataResourceView {}

    /**
     * Inventory Item and Product assigned for WorkEffort
     * Inventory Item and Product assigned for WorkEffort
     */
    @ViewEntity(
        name = "WorkEffortAndInventoryAssign",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Inventory Item and Product assigned for WorkEffort",
        description = "Inventory Item and Product assigned for WorkEffort",
        members = {
            @MemberEntity(entityAlias = "WEIA", entityName = "WorkEffortInventoryAssign"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEIA")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "II"),
            @Alias(name = "currencyUomId", entityAlias = "II"),
            @Alias(name = "unitCost", entityAlias = "II"),
            @Alias(name = "uomId", entityAlias = "II")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEIA",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface WorkEffortAndInventoryAssignView {}

    /**
     * Inventory Item and Product produced by WorkEffort
     * Inventory Item and Product produced by WorkEffort
     */
    @ViewEntity(
        name = "WorkEffortAndInventoryProduced",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Inventory Item and Product produced by WorkEffort",
        description = "Inventory Item and Product produced by WorkEffort",
        members = {
            @MemberEntity(entityAlias = "WEIP", entityName = "WorkEffortInventoryProduced"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WEIP")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "II"),
            @Alias(name = "currencyUomId", entityAlias = "II"),
            @Alias(name = "unitCost", entityAlias = "II"),
            @Alias(name = "lotId", entityAlias = "II"),
            @Alias(name = "quantityOnHandTotal", entityAlias = "II")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEIP",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface WorkEffortAndInventoryProducedView {}

    /**
     * Work Effort And Party Assignment and PartyNameView
     * Ties WEPA to the party info.
     */
    @ViewEntity(
        name = "WorkEffortPartyAssignView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Party Assignment and PartyNameView",
        description = "Ties WEPA to the party info.",
        members = {
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "PNV", entityName = "PartyNameView")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PNV"),
            @AliasAll(entityAlias = "WEPA", excludes = {"statusId"})
        },
        aliases = {
            @Alias(name = "assignmentStatusId", entityAlias = "WEPA", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEPA",
                relEntityAlias = "PNV",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortPartyAssignment",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyNameView",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
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
                relEntityName = "StatusItem",
                title = "Assignment",
                keyMaps = {
                    @KeyMap(fieldName = "assignmentStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                title = "Expectation",
                keyMaps = {
                    @KeyMap(fieldName = "expectationEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                title = "DelegateReason",
                keyMaps = {
                    @KeyMap(fieldName = "delegateReasonEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface WorkEffortPartyAssignViewView {}

    /**
     * Work Effort And CommunicationEvent
     * Ties WECommEvent to CommunicationEvent.
     */
    @ViewEntity(
        name = "WorkEffortCommunicationEventView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And CommunicationEvent",
        description = "Ties WECommEvent to CommunicationEvent.",
        members = {
            @MemberEntity(entityAlias = "CEWE", entityName = "CommunicationEventWorkEff"),
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CEWE"),
            @AliasAll(entityAlias = "CE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CEWE",
                relEntityAlias = "CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CommunicationEvent",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface WorkEffortCommunicationEventViewView {}

    /**
     * Work Effort And ShoppingList
     * Ties ShoppingListWE to ShoppingList.
     */
    @ViewEntity(
        name = "WorkEffortShoppingListView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And ShoppingList",
        description = "Ties ShoppingListWE to ShoppingList.",
        members = {
            @MemberEntity(entityAlias = "SLWE", entityName = "ShoppingListWorkEffort"),
            @MemberEntity(entityAlias = "SL", entityName = "ShoppingList"),
            @MemberEntity(entityAlias = "SLT", entityName = "ShoppingListType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SLWE"),
            @AliasAll(entityAlias = "SL")
        },
        aliases = {
            @Alias(name = "shoppingListTypeDescription", entityAlias = "SLT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SLWE",
                relEntityAlias = "SL",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId")
                }
            ),
            @ViewLink(
                entityAlias = "SL",
                relEntityAlias = "SLT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ShoppingList",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId")
                }
            )
        }
    )
    public interface WorkEffortShoppingListViewView {}

    /**
     * Work Effort And Quote
     * Ties QuoteWE to Quote.
     */
    @ViewEntity(
        name = "WorkEffortQuoteView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And Quote",
        description = "Ties QuoteWE to Quote.",
        members = {
            @MemberEntity(entityAlias = "QWE", entityName = "QuoteWorkEffort"),
            @MemberEntity(entityAlias = "Q", entityName = "Quote"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "QWE"),
            @AliasAll(entityAlias = "Q")
        },
        aliases = {
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "QWE",
                relEntityAlias = "Q",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @ViewLink(
                entityAlias = "Q",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Quote",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            )
        }
    )
    public interface WorkEffortQuoteViewView {}

    /**
     * Work Effort And OrderHeader
     * Ties OrderHeaderWE to OrderHeader.
     */
    @ViewEntity(
        name = "WorkEffortOrderHeaderView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And OrderHeader",
        description = "Ties OrderHeaderWE to OrderHeader.",
        members = {
            @MemberEntity(entityAlias = "OHWE", entityName = "OrderHeaderWorkEffort"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OT", entityName = "OrderType"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OHWE"),
            @AliasAll(entityAlias = "OH")
        },
        aliases = {
            @Alias(name = "orderTypeDescription", entityAlias = "OT", field = "description"),
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OHWE",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderType",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortOrderHeaderViewView {}

    /**
     * Work Effort And CustRequest
     * Ties CustRequestWE to CustRequest and WorkEffort.
     */
    @ViewEntity(
        name = "WorkEffortCustRequestView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And CustRequest",
        description = "Ties CustRequestWE to CustRequest and WorkEffort.",
        members = {
            @MemberEntity(entityAlias = "CRWE", entityName = "CustRequestWorkEffort"),
            @MemberEntity(entityAlias = "CR", entityName = "CustRequest"),
            @MemberEntity(entityAlias = "CRT", entityName = "CustRequestType"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CRWE"),
            @AliasAll(entityAlias = "CR"),
            @AliasAll(entityAlias = "WE", excludes = {"priority", "description", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        aliases = {
            @Alias(name = "custRequestTypeDescription", entityAlias = "CRT", field = "description"),
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description"),
            @Alias(name = "workEffortPriority", entityAlias = "WE", field = "priority"),
            @Alias(name = "workEffortDescription", entityAlias = "WE", field = "description"),
            @Alias(name = "workEffortCreatedDate", entityAlias = "WE", field = "createdDate"),
            @Alias(name = "workEffortCreatedByUserLogin", entityAlias = "WE", field = "createdByUserLogin"),
            @Alias(name = "workEffortLastModifiedDate", entityAlias = "WE", field = "lastModifiedDate"),
            @Alias(name = "workEffortLastModByUserLogin", entityAlias = "WE", field = "lastModifiedByUserLogin")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CRWE",
                relEntityAlias = "CR",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @ViewLink(
                entityAlias = "CRWE",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CRT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortCustRequestViewView {}

    /**
     * Work Effort And CustRequest
     * Ties CustRequestWE to CustRequest.
     */
    @ViewEntity(
        name = "WorkEffortCustRequestItemView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort And CustRequest",
        description = "Ties CustRequestWE to CustRequest.",
        members = {
            @MemberEntity(entityAlias = "CRIWE", entityName = "CustRequestItemWorkEffort"),
            @MemberEntity(entityAlias = "CRI", entityName = "CustRequestItem"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CRIWE"),
            @AliasAll(entityAlias = "CRI")
        },
        aliases = {
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CRIWE",
                relEntityAlias = "CRI",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "CRI",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequestItem",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface WorkEffortCustRequestItemViewView {}

    /**
     * WorkRequirementFulfillment And Requirement
     * Ties WorkRequirementFulfillment to Requirement.
     */
    @ViewEntity(
        name = "WorkEffortRequirementView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkRequirementFulfillment And Requirement",
        description = "Ties WorkRequirementFulfillment to Requirement.",
        members = {
            @MemberEntity(entityAlias = "WRF", entityName = "WorkRequirementFulfillment"),
            @MemberEntity(entityAlias = "REQ", entityName = "Requirement"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem"),
            @MemberEntity(entityAlias = "WRFT", entityName = "WorkReqFulfType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WRF"),
            @AliasAll(entityAlias = "REQ")
        },
        aliases = {
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description"),
            @Alias(name = "workReqFulfTypeDescription", entityAlias = "WRFT", field = "description"),
            @Alias(name = "requirementDescription", entityAlias = "REQ", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WRF",
                relEntityAlias = "REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @ViewLink(
                entityAlias = "REQ",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @ViewLink(
                entityAlias = "WRF",
                relEntityAlias = "WRFT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "workReqFulfTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Requirement",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkReqFulfType",
                keyMaps = {
                    @KeyMap(fieldName = "workReqFulfTypeId")
                }
            )
        }
    )
    public interface WorkEffortRequirementViewView {}

    /**
     * Sales opportunity and associated work effort
     */
    @ViewEntity(
        name = "WorkEffortAndSalesOpportunity",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Sales opportunity and associated work effort",
        members = {
            @MemberEntity(entityAlias = "SOWE", entityName = "SalesOpportunityWorkEffort"),
            @MemberEntity(entityAlias = "SO", entityName = "SalesOpportunity"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SO"),
            @AliasAll(entityAlias = "WE", excludes = {"description", "createdByUserLogin"})
        },
        aliases = {
            @Alias(name = "workEffortDescription", entityAlias = "WE", field = "description"),
            @Alias(name = "workEffortCreatedByUserLogin", entityAlias = "WE", field = "createdByUserLogin")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SOWE",
                relEntityAlias = "SO",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @ViewLink(
                entityAlias = "SOWE",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SalesOpportunity",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            )
        }
    )
    public interface WorkEffortAndSalesOpportunityView {}

    /**
     * WorkEffortContent, Content and DataResource View
     */
    @ViewEntity(
        name = "WorkEffortContentAndInfo",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffortContent, Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "WC", entityName = "WorkEffortContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WC",
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
    public interface WorkEffortContentAndInfoView {}

    /**
     * Work Effort Contact Mech View
     */
    @ViewEntity(
        name = "WorkEffortContactMechView",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "Work Effort Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "WECM", entityName = "WorkEffortContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WECM"),
            @AliasAll(entityAlias = "CM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WECM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface WorkEffortContactMechViewView {}

    /**
     * WorkEffort and TimeEntry View
     */
    @ViewEntity(
        name = "WorkEffortAndTimeEntry",
        packageName = "org.ofbiz.workeffort.workeffort",
        title = "WorkEffort and TimeEntry View",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "TE", entityName = "TimeEntry")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "TE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "TE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkEffortSkillStandard",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface WorkEffortAndTimeEntryView {}

}
