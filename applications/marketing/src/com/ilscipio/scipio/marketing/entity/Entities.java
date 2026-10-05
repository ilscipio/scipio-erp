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
package com.ilscipio.scipio.marketing.entity;

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
     * Marketing Campaign
     */
    @Entity(
        name = "MarketingCampaign",
        packageName = "org.ofbiz.marketing.campaign",
        title = "Marketing Campaign",
        fields = {
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "parentCampaignId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "campaignName", type = "name"),
            @Field(name = "campaignSummary", type = "very-long"),
            @Field(name = "budgetedCost", type = "currency-amount"),
            @Field(name = "actualCost", type = "currency-amount"),
            @Field(name = "estimatedCost", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "isActive", type = "indicator"),
            @Field(name = "convertedLeads", type = "id-ne"),
            @Field(name = "expectedResponsePercent", type = "floating-point"),
            @Field(name = "expectedRevenue", type = "currency-amount"),
            @Field(name = "numSent", type = "numeric"),
            @Field(name = "startDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketingCampaignId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                title = "Parent",
                fkName = "MKTGCPN_PRNT",
                keyMaps = {
                    @KeyMap(fieldName = "parentCampaignId", relFieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "MKTGCPN_STS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "MKTGCPN_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface MarketingCampaignEntity {}

    /**
     * Marketing Campaign Note
     */
    @Entity(
        name = "MarketingCampaignNote",
        packageName = "org.ofbiz.marketing.campaign",
        title = "Marketing Campaign Note",
        fields = {
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketingCampaignId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "MKTGCPN_NOTE_CMPN",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "MKTGCPN_NOTE_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface MarketingCampaignNoteEntity {}

    /**
     * Marketing Campaign Price
     */
    @Entity(
        name = "MarketingCampaignPrice",
        packageName = "org.ofbiz.marketing.campaign",
        title = "Marketing Campaign Price",
        fields = {
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "productPriceRuleId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketingCampaignId"),
            @PrimaryKey(field = "productPriceRuleId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "MKTGCPN_PRICE_MC",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceRule",
                fkName = "MKTGCPN_PRICE_PP",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceRuleId")
                }
            )
        }
    )
    public interface MarketingCampaignPriceEntity {}

    /**
     * Marketing Campaign Promo
     */
    @Entity(
        name = "MarketingCampaignPromo",
        packageName = "org.ofbiz.marketing.campaign",
        title = "Marketing Campaign Promo",
        fields = {
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "productPromoId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketingCampaignId"),
            @PrimaryKey(field = "productPromoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "MKTGCPN_PROMO_MC",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "MKTGCPN_PROMO_PP",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            )
        }
    )
    public interface MarketingCampaignPromoEntity {}

    /**
     * Marketing Campaign Role
     */
    @Entity(
        name = "MarketingCampaignRole",
        packageName = "org.ofbiz.marketing.campaign",
        title = "Marketing Campaign Role",
        fields = {
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketingCampaignId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "MKTGCPN_ROLE_MC",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "MKTGCPN_ROLE_PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
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
            )
        }
    )
    public interface MarketingCampaignRoleEntity {}

    /**
     * Contact List
     */
    @Entity(
        name = "ContactList",
        packageName = "org.ofbiz.marketing.contact",
        title = "Contact List",
        fields = {
            @Field(name = "contactListId", type = "id-ne"),
            @Field(name = "contactListTypeId", type = "id-ne"),
            @Field(name = "contactMechTypeId", type = "id-ne"),
            @Field(name = "marketingCampaignId", type = "id"),
            @Field(name = "contactListName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "isPublic", type = "indicator"),
            @Field(name = "singleUse", type = "indicator", description = "Whether members of the list should be contacted only once."),
            @Field(name = "ownerPartyId", type = "id"),
            @Field(name = "verifyEmailFrom", type = "long-varchar"),
            @Field(name = "verifyEmailScreen", type = "long-varchar"),
            @Field(name = "verifyEmailSubject", type = "long-varchar"),
            @Field(name = "verifyEmailWebSiteId", type = "id"),
            @Field(name = "optOutScreen", type = "long-varchar"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactListId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "CNCT_LST_MKCMPN",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactListType",
                fkName = "CNCT_LST_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "contactListTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "CNCT_LST_CMCHTP",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "CNCT_LST_CBUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "CNCT_LST_LMUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Owner",
                fkName = "CNCT_LST_OPTY",
                keyMaps = {
                    @KeyMap(fieldName = "ownerPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface ContactListEntity {}

    /**
     * Web Site Contact List
     */
    @Entity(
        name = "WebSiteContactList",
        packageName = "org.ofbiz.marketing.contact",
        title = "Web Site Contact List",
        fields = {
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "contactListId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "contactListId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "WEB_SITE_CNTCT_LST",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactList",
                fkName = "CNTCT_LST_WEB_SITE",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId")
                }
            )
        }
    )
    public interface WebSiteContactListEntity {}

    /**
     * Contact List
     */
    @Entity(
        name = "ContactListCommStatus",
        packageName = "org.ofbiz.marketing.contact",
        title = "Contact List",
        fields = {
            @Field(name = "contactListId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "messageId", type = "value"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactListId"),
            @PrimaryKey(field = "communicationEventId"),
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactList",
                fkName = "CNCT_LST_CST_CL",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "CNCT_LST_CST_CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "CNCT_LST_CST_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CNCT_LST_CST_PT",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CNCT_LST_CST_ST",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "CNCT_LST_CST_ST_UL",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CNTLSTCST_MSG_ID",
                unique = true,
                fields = {
                    @IndexField(name = "messageId")
                }
            )
        }
    )
    public interface ContactListCommStatusEntity {}

    /**
     * Contact List Party
     */
    @Entity(
        name = "ContactListParty",
        packageName = "org.ofbiz.marketing.contact",
        title = "Contact List Party",
        fields = {
            @Field(name = "contactListId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "preferredContactMechId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactListId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactList",
                fkName = "CNCT_LSTPTY_CLST",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CNCT_LSTPTY_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CNCT_LSTPTY_STS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "Preferred",
                fkName = "CNCT_LSTPTY_PCM",
                keyMaps = {
                    @KeyMap(fieldName = "preferredContactMechId", relFieldName = "contactMechId")
                }
            )
        }
    )
    public interface ContactListPartyEntity {}

    /**
     * Contact List Party Status
     */
    @Entity(
        name = "ContactListPartyStatus",
        packageName = "org.ofbiz.marketing.contact",
        title = "Contact List Party Status",
        fields = {
            @Field(name = "contactListId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "setByUserLoginId", type = "id-vlong"),
            @Field(name = "optInVerifyCode", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactListId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "fromDate"),
            @PrimaryKey(field = "statusDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactListParty",
                fkName = "CTLSTPTST_CLP",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "fromDate")
                }
            )
        }
    )
    public interface ContactListPartyStatusEntity {}

    /**
     * Contact List Type
     */
    @Entity(
        name = "ContactListType",
        packageName = "org.ofbiz.marketing.contact",
        title = "Contact List Type",
        defaultResourceName = "MarketingEntityLabels",
        fields = {
            @Field(name = "contactListTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactListTypeId")
        }
    )
    public interface ContactListTypeEntity {}

    /**
     * Segment Group
     */
    @Entity(
        name = "SegmentGroup",
        packageName = "org.ofbiz.marketing.segment",
        title = "Segment Group",
        fields = {
            @Field(name = "segmentGroupId", type = "id-ne"),
            @Field(name = "segmentGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "productStoreId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "segmentGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SegmentGroupType",
                fkName = "SGMTGRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "SGMTGRP_PRST",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface SegmentGroupEntity {}

    /**
     * Segment Group Classification
     */
    @Entity(
        name = "SegmentGroupClassification",
        packageName = "org.ofbiz.marketing.segment",
        title = "Segment Group Classification",
        fields = {
            @Field(name = "segmentGroupId", type = "id-ne"),
            @Field(name = "partyClassificationGroupId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "segmentGroupId"),
            @PrimaryKey(field = "partyClassificationGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SegmentGroup",
                fkName = "SGMTGRPCLS_SGGP",
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationGroup",
                fkName = "SGMTGRPCLS_PCGP",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationGroupId")
                }
            )
        }
    )
    public interface SegmentGroupClassificationEntity {}

    /**
     * Segment Group Geo
     */
    @Entity(
        name = "SegmentGroupGeo",
        packageName = "org.ofbiz.marketing.segment",
        title = "Segment Group Geo",
        fields = {
            @Field(name = "segmentGroupId", type = "id-ne"),
            @Field(name = "geoId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "segmentGroupId"),
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SegmentGroup",
                fkName = "SGMTGRPGEO_SGGP",
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "SGMTGRPGEO_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface SegmentGroupGeoEntity {}

    /**
     * Segment Group Role
     */
    @Entity(
        name = "SegmentGroupRole",
        packageName = "org.ofbiz.marketing.segment",
        title = "Segment Group Role",
        fields = {
            @Field(name = "segmentGroupId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "segmentGroupId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SegmentGroup",
                fkName = "SGMTGRPRL_SGGP",
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "SGMTGRPRL_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
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
            )
        }
    )
    public interface SegmentGroupRoleEntity {}

    /**
     * Segment Group Type
     */
    @Entity(
        name = "SegmentGroupType",
        packageName = "org.ofbiz.marketing.segment",
        title = "Segment Group Type",
        defaultResourceName = "MarketingEntityLabels",
        fields = {
            @Field(name = "segmentGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "segmentGroupTypeId")
        }
    )
    public interface SegmentGroupTypeEntity {}

    /**
     * Tracking Code
     */
    @Entity(
        name = "TrackingCode",
        packageName = "org.ofbiz.marketing.tracking",
        title = "Tracking Code",
        fields = {
            @Field(name = "trackingCodeId", type = "id-ne"),
            @Field(name = "trackingCodeTypeId", type = "id-ne"),
            @Field(name = "marketingCampaignId", type = "id"),
            @Field(name = "redirectUrl", type = "url"),
            @Field(name = "overrideLogo", type = "url"),
            @Field(name = "overrideCss", type = "url"),
            @Field(name = "prodCatalogId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "description", type = "description"),
            @Field(name = "trackableLifetime", type = "numeric"),
            @Field(name = "billableLifetime", type = "numeric"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "groupId", type = "id"),
            @Field(name = "subgroupId", type = "id"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "trackingCodeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "TKNG_COD_MKCMPN",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCodeType",
                fkName = "TKNG_COD_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeTypeId")
                }
            )
        }
    )
    public interface TrackingCodeEntity {}

    /**
     * Tracking Code Visit
     */
    @Entity(
        name = "TrackingCodeOrder",
        packageName = "org.ofbiz.marketing.tracking",
        title = "Tracking Code Visit",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "trackingCodeTypeId", type = "id-ne"),
            @Field(name = "trackingCodeId", type = "id-ne"),
            @Field(name = "isBillable", type = "indicator"),
            @Field(name = "siteId", type = "long-varchar"),
            @Field(name = "hasExported", type = "indicator"),
            @Field(name = "affiliateReferredTimeStamp", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "trackingCodeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "TKNG_CODODR_ODR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCode",
                fkName = "TKNG_CODODR_TKCD",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCodeType",
                fkName = "TKNG_CODODR_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeTypeId")
                }
            )
        }
    )
    public interface TrackingCodeOrderEntity {}

    /**
     * Tracking Code And Order Return
     */
    @Entity(
        name = "TrackingCodeOrderReturn",
        packageName = "org.ofbiz.marketing.tracking",
        title = "Tracking Code And Order Return",
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "trackingCodeTypeId", type = "id-ne"),
            @Field(name = "trackingCodeId", type = "id-ne"),
            @Field(name = "isBillable", type = "indicator"),
            @Field(name = "siteId", type = "long-varchar"),
            @Field(name = "hasExported", type = "indicator"),
            @Field(name = "affiliateReferredTimeStamp", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "trackingCodeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "TKNG_CODODR_RTN",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "TKNG_CODODR_ODRTN",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCode",
                fkName = "TKNG_CODODR_RTNTCD",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCodeType",
                fkName = "TKNG_CODODR_RTNTYP",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeTypeId")
                }
            )
        }
    )
    public interface TrackingCodeOrderReturnEntity {}

    /**
     * Tracking Code Type
     */
    @Entity(
        name = "TrackingCodeType",
        packageName = "org.ofbiz.marketing.tracking",
        title = "Tracking Code Type",
        defaultResourceName = "MarketingEntityLabels",
        fields = {
            @Field(name = "trackingCodeTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "trackingCodeTypeId")
        }
    )
    public interface TrackingCodeTypeEntity {}

    /**
     * Tracking Code Visit
     */
    @Entity(
        name = "TrackingCodeVisit",
        packageName = "org.ofbiz.marketing.tracking",
        title = "Tracking Code Visit",
        fields = {
            @Field(name = "trackingCodeId", type = "id-ne"),
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "sourceEnumId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "trackingCodeId"),
            @PrimaryKey(field = "visitId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TrackingCode",
                fkName = "TKNG_CODVST_TKCD",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "TKNG_CODVST_SRCEM",
                keyMaps = {
                    @KeyMap(fieldName = "sourceEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface TrackingCodeVisitEntity {}

    /**
     * Main entity of information about sales opportunities
     */
    @Entity(
        name = "SalesOpportunity",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Main entity of information about sales opportunities",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "opportunityName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "nextStep", type = "very-long"),
            @Field(name = "nextStepDate", type = "date-time"),
            @Field(name = "estimatedAmount", type = "currency-amount"),
            @Field(name = "estimatedProbability", type = "fixed-point"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "marketingCampaignId", type = "id-ne"),
            @Field(name = "dataSourceId", type = "id-ne"),
            @Field(name = "estimatedCloseDate", type = "date-time"),
            @Field(name = "opportunityStageId", type = "id-ne"),
            @Field(name = "typeEnumId", type = "id-ne"),
            @Field(name = "createdByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "SLSOPP_CRNCY_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunityStage",
                fkName = "SLSOPP_STAGE",
                keyMaps = {
                    @KeyMap(fieldName = "opportunityStageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Type",
                fkName = "SLSOPP_TYP_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "typeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MarketingCampaign",
                fkName = "SLSOPP_MKTGCMPG",
                keyMaps = {
                    @KeyMap(fieldName = "marketingCampaignId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "SLSOPP_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface SalesOpportunityEntity {}

    /**
     * Tracks a history of sales opportunity information
     */
    @Entity(
        name = "SalesOpportunityHistory",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Tracks a history of sales opportunity information",
        fields = {
            @Field(name = "salesOpportunityHistoryId", type = "id-ne"),
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "nextStep", type = "very-long"),
            @Field(name = "estimatedAmount", type = "currency-amount"),
            @Field(name = "estimatedProbability", type = "fixed-point"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "estimatedCloseDate", type = "date-time"),
            @Field(name = "opportunityStageId", type = "id-ne"),
            @Field(name = "changeNote", type = "very-long", description = "Used to track a reason for this change"),
            @Field(name = "modifiedByUserLogin", type = "id-vlong"),
            @Field(name = "modifiedTimestamp", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityHistoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "SLOPHI_CRNCY_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunityStage",
                fkName = "SLOPHI_STAGE",
                keyMaps = {
                    @KeyMap(fieldName = "opportunityStageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SLOPHI_SLSOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "SLOPHI_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "modifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface SalesOpportunityHistoryEntity {}

    /**
     * Describes roles of different parties involved in a sales opportunity
     */
    @Entity(
        name = "SalesOpportunityRole",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Describes roles of different parties involved in a sales opportunity",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SLSOPPRL_SLSOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SLSOPPRL_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "SLSOPPRL_ROLETYPE",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "SLSOPPRL_PTYROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface SalesOpportunityRoleEntity {}

    /**
     * Describes stages of a sales opportunity with associated probability factors.
     */
    @Entity(
        name = "SalesOpportunityStage",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Describes stages of a sales opportunity with associated probability factors.",
        fields = {
            @Field(name = "opportunityStageId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "defaultProbability", type = "fixed-point"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "opportunityStageId")
        }
    )
    public interface SalesOpportunityStageEntity {}

    /**
     * Relates sales opportunities to their work efforts.
     */
    @Entity(
        name = "SalesOpportunityWorkEffort",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Relates sales opportunities to their work efforts.",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SOPPWEFF_SOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "SOPPWEFF_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface SalesOpportunityWorkEffortEntity {}

    /**
     * Relates sales opportunities to their quotes.
     */
    @Entity(
        name = "SalesOpportunityQuote",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Relates sales opportunities to their quotes.",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "quoteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId"),
            @PrimaryKey(field = "quoteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SOPPQTE_SOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "SOPPQTE_QTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            )
        }
    )
    public interface SalesOpportunityQuoteEntity {}

    /**
     * Stores sales forecast data for sales opportunities.
     */
    @Entity(
        name = "SalesForecast",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Stores sales forecast data for sales opportunities.",
        fields = {
            @Field(name = "salesForecastId", type = "id-ne"),
            @Field(name = "parentSalesForecastId", type = "id"),
            @Field(name = "organizationPartyId", type = "id"),
            @Field(name = "internalPartyId", type = "id"),
            @Field(name = "customTimePeriodId", type = "id"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "quotaAmount", type = "currency-amount"),
            @Field(name = "forecastAmount", type = "currency-amount"),
            @Field(name = "bestCaseAmount", type = "currency-amount"),
            @Field(name = "closedAmount", type = "currency-amount"),
            @Field(name = "percentOfQuotaForecast", type = "fixed-point"),
            @Field(name = "percentOfQuotaClosed", type = "fixed-point"),
            @Field(name = "pipelineAmount", type = "currency-amount"),
            @Field(name = "createdByUserLoginId", type = "id-vlong"),
            @Field(name = "modifiedByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesForecastId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesForecast",
                title = "Parent",
                fkName = "SALES4C_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentSalesForecastId", relFieldName = "salesForecastId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "SALES4C_ORG_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Internal",
                fkName = "SALES4C_INT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "internalPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomTimePeriod",
                fkName = "SALES4C_TIME_PER",
                keyMaps = {
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "SALES4C_CUR_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "SALES4C_CRT_USER",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ModifiedBy",
                fkName = "SALES4C_MOD_USER",
                keyMaps = {
                    @KeyMap(fieldName = "modifiedByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface SalesForecastEntity {}

    /**
     * Stores Details of Resourses of Sales Forecast for simulation of MRP
     */
    @Entity(
        name = "SalesForecastDetail",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Stores Details of Resourses of Sales Forecast for simulation of MRP",
        fields = {
            @Field(name = "salesForecastId", type = "id-ne"),
            @Field(name = "salesForecastDetailId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "quantityUomId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productCategoryId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesForecastId"),
            @PrimaryKey(field = "salesForecastDetailId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesForecast",
                fkName = "SALES4CDTL_SALES4C",
                keyMaps = {
                    @KeyMap(fieldName = "salesForecastId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Quantity",
                fkName = "SALES4CDTL_QTY_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SALES4CDTL_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "SALES4CDTL_PCTGRY",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface SalesForecastDetailEntity {}

    /**
     * Keeps a record of changes to a sales forecast.
     */
    @Entity(
        name = "SalesForecastHistory",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Keeps a record of changes to a sales forecast.",
        fields = {
            @Field(name = "salesForecastHistoryId", type = "id-ne"),
            @Field(name = "salesForecastId", type = "id-ne"),
            @Field(name = "parentSalesForecastId", type = "id"),
            @Field(name = "organizationPartyId", type = "id"),
            @Field(name = "internalPartyId", type = "id"),
            @Field(name = "customTimePeriodId", type = "id"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "quotaAmount", type = "currency-amount"),
            @Field(name = "forecastAmount", type = "currency-amount"),
            @Field(name = "bestCaseAmount", type = "currency-amount"),
            @Field(name = "closedAmount", type = "currency-amount"),
            @Field(name = "percentOfQuotaForecast", type = "fixed-point"),
            @Field(name = "percentOfQuotaClosed", type = "fixed-point"),
            @Field(name = "changeNote", type = "very-long", description = "Used to track a reason for this change"),
            @Field(name = "modifiedByUserLoginId", type = "id-vlong"),
            @Field(name = "modifiedTimestamp", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesForecastHistoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesForecast",
                fkName = "SALES4CH_SALES4C",
                keyMaps = {
                    @KeyMap(fieldName = "salesForecastId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "SALES4CH_ORG_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Internal",
                fkName = "SALES4CH_INT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "internalPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomTimePeriod",
                fkName = "SALES4CH_TIME_PER",
                keyMaps = {
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "SALES4CH_CUR_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ModifiedBy",
                fkName = "SALES4CH_MOD_USER",
                keyMaps = {
                    @KeyMap(fieldName = "modifiedByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface SalesForecastHistoryEntity {}

    /**
     * Sales opportunity competitors record
     */
    @Entity(
        name = "SalesOpportunityCompetitor",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Sales opportunity competitors record",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "competitorPartyId", type = "id-ne"),
            @Field(name = "positionEnumId", type = "id-ne"),
            @Field(name = "strengths", type = "very-long"),
            @Field(name = "weaknesses", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId"),
            @PrimaryKey(field = "competitorPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SOPPCOMP_SOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            )
        }
    )
    public interface SalesOpportunityCompetitorEntity {}

    /**
     * Sales opportunity traking code
     */
    @Entity(
        name = "SalesOpportunityTrckCode",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "Sales opportunity traking code",
        fields = {
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "trackingCodeId", type = "id-ne"),
            @Field(name = "receivedDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOpportunityId"),
            @PrimaryKey(field = "trackingCodeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "SOPPTRKCD_SOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            )
        }
    )
    public interface SalesOpportunityTrckCodeEntity {}

    @ViewEntity(
        name = "ContactListPartyAndStatus",
        packageName = "org.ofbiz.marketing.contact",
        members = {
            @MemberEntity(entityAlias = "CLPS", entityName = "ContactListPartyStatus"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CLPS"),
            @AliasAll(entityAlias = "SI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CLPS",
                relEntityAlias = "SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface ContactListPartyAndStatusView {}

    /**
     *              This view entity models the following options:             - SegmentGroupRole(SGR) -> SegmentGroupRole(SGRTO)             - SegmentGroupRole(SGR) -> SegmentGroupRole(SGRTO) -> PartyRelationship(PRSGR)             - SegmentGroupRole(SGR) -> SegmentGroupClassification(SGC) -> PartyClassification(PC)             - SegmentGroupRole(SGR) -> SegmentGroupClassification(SGC) -> PartyClassification(PC) -> PartyRelationship(PRPC)             Typical fields to constrain:             - partyId of User -> sgrPartyId (SGR)             - partyId of Customer -> sgrToPartyId (SGRTO) AND prSgrPartyIdTo (PRSGR) AND pcPartyId (PC) AND prPcPartyIdTo (PRPC)               NOTE: these 4 partyIds represent the 4 options for entity relationship paths listed above               NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null             - roleTypeId of User -> sgrRoleTypeId (SGR) - ex: SALES_REP, MANAGER, etc             - roleTypeId of Customer -> sgrToRoleTypeId (SGRTO)               NOTE: because not all of these will exist, each needs to be the given roleTypeId(s) OR null             - roleTypeId of _Employer_ in Employee/Employer relationship -> prSgrRoleTypeIdFrom (PRSGR) AND prPcRoleTypeIdFrom (PRPC) - INTERNAL_ORGANIZATIO               NOTE: constraining these fields is optional as the EMPLOYEE roleTypeIdTo is often sufficient               NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null             - roleTypeId of _Employee_ in Employee/Employer relationship -> prSgrRoleTypeIdTo (PRSGR) AND prPcRoleTypeIdTo (PRPC) - EMPLOYEE               NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null             - partyRelationshipTypeId in Employee/Employer relationship -> prSgrPartyRelationshipTypeId (PRSGR) AND prPcPartyRelationshipTypeId (PRPC)               NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null         
     */
    @ViewEntity(
        name = "SegmentGroupViewRelatedParties",
        packageName = "org.ofbiz.marketing.segment",
        description = "\n            This view entity models the following options:\n            - SegmentGroupRole(SGR) -> SegmentGroupRole(SGRTO)\n            - SegmentGroupRole(SGR) -> SegmentGroupRole(SGRTO) -> PartyRelationship(PRSGR)\n            - SegmentGroupRole(SGR) -> SegmentGroupClassification(SGC) -> PartyClassification(PC)\n            - SegmentGroupRole(SGR) -> SegmentGroupClassification(SGC) -> PartyClassification(PC) -> PartyRelationship(PRPC)\n            Typical fields to constrain:\n            - partyId of User -> sgrPartyId (SGR)\n            - partyId of Customer -> sgrToPartyId (SGRTO) AND prSgrPartyIdTo (PRSGR) AND pcPartyId (PC) AND prPcPartyIdTo (PRPC)\n              NOTE: these 4 partyIds represent the 4 options for entity relationship paths listed above\n              NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null\n            - roleTypeId of User -> sgrRoleTypeId (SGR) - ex: SALES_REP, MANAGER, etc\n            - roleTypeId of Customer -> sgrToRoleTypeId (SGRTO)\n              NOTE: because not all of these will exist, each needs to be the given roleTypeId(s) OR null\n            - roleTypeId of _Employer_ in Employee/Employer relationship -> prSgrRoleTypeIdFrom (PRSGR) AND prPcRoleTypeIdFrom (PRPC) - INTERNAL_ORGANIZATIO\n              NOTE: constraining these fields is optional as the EMPLOYEE roleTypeIdTo is often sufficient\n              NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null\n            - roleTypeId of _Employee_ in Employee/Employer relationship -> prSgrRoleTypeIdTo (PRSGR) AND prPcRoleTypeIdTo (PRPC) - EMPLOYEE\n              NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null\n            - partyRelationshipTypeId in Employee/Employer relationship -> prSgrPartyRelationshipTypeId (PRSGR) AND prPcPartyRelationshipTypeId (PRPC)\n              NOTE: because not all of these will exist, each needs to be the given partyId(s) OR null\n        ",
        members = {
            @MemberEntity(entityAlias = "SGR", entityName = "SegmentGroupRole"),
            @MemberEntity(entityAlias = "SGRTO", entityName = "SegmentGroupRole"),
            @MemberEntity(entityAlias = "PRSGR", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "SGC", entityName = "SegmentGroupClassification"),
            @MemberEntity(entityAlias = "PC", entityName = "PartyClassification"),
            @MemberEntity(entityAlias = "PRPC", entityName = "PartyRelationship")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SGR", prefix = "sgr"),
            @AliasAll(entityAlias = "SGRTO", prefix = "sgrTo"),
            @AliasAll(entityAlias = "PRSGR", prefix = "prSgr"),
            @AliasAll(entityAlias = "SGC", prefix = "sgc"),
            @AliasAll(entityAlias = "PC", prefix = "pc"),
            @AliasAll(entityAlias = "PRPC", prefix = "prPc")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SGR",
                relEntityAlias = "SGRTO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupId")
                }
            ),
            @ViewLink(
                entityAlias = "SGRTO",
                relEntityAlias = "PRSGR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdFrom")
                }
            ),
            @ViewLink(
                entityAlias = "SGR",
                relEntityAlias = "SGC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "segmentGroupId")
                }
            ),
            @ViewLink(
                entityAlias = "SGC",
                relEntityAlias = "PC",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationGroupId")
                }
            ),
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "PRPC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdFrom")
                }
            )
        }
    )
    public interface SegmentGroupViewRelatedPartiesView {}

    /**
     * View entity for reporting number of visits for a tracking code
     */
    @ViewEntity(
        name = "TrackingCodeAndVisit",
        packageName = "org.ofbiz.marketing.reports",
        title = "View entity for reporting number of visits for a tracking code",
        members = {
            @MemberEntity(entityAlias = "TC", entityName = "TrackingCode"),
            @MemberEntity(entityAlias = "TCV", entityName = "TrackingCodeVisit")
        },
        aliases = {
            @Alias(name = "trackingCodeId", entityAlias = "TC", groupBy = true),
            @Alias(name = "visitId", entityAlias = "TCV", function = AggregateFunction.COUNT),
            @Alias(name = "fromDate", entityAlias = "TCV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TC",
                relEntityAlias = "TCV",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            )
        }
    )
    public interface TrackingCodeAndVisitView {}

    /**
     * View entity for reporting number of orders and total order amounts
     */
    @ViewEntity(
        name = "TrackingCodeAndOrderHeader",
        packageName = "org.ofbiz.marketing.reports",
        title = "View entity for reporting number of orders and total order amounts",
        members = {
            @MemberEntity(entityAlias = "TCO", entityName = "TrackingCodeOrder"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "grandTotal", entityAlias = "OH", function = AggregateFunction.SUM),
            @Alias(name = "orderId", entityAlias = "TCO", function = AggregateFunction.COUNT),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "trackingCodeId", entityAlias = "TCO", groupBy = true)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TCO",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface TrackingCodeAndOrderHeaderView {}

    /**
     * Order Header And Tracking Code Order View
     */
    @ViewEntity(
        name = "TrackingCodeOrderAndOrderHeader",
        packageName = "org.ofbiz.marketing.reports",
        title = "Order Header And Tracking Code Order View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "TCO", entityName = "TrackingCodeOrder"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "TCO"),
            @Alias(name = "trackingCodeId", entityAlias = "TCO"),
            @Alias(name = "siteId", entityAlias = "TCO"),
            @Alias(name = "hasExported", entityAlias = "TCO"),
            @Alias(name = "affiliateReferredTimeStamp", entityAlias = "TCO"),
            @Alias(name = "statusId", entityAlias = "OH")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TCO",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface TrackingCodeOrderAndOrderHeaderView {}

    /**
     * Return Header And Tracking Code Order Return View
     */
    @ViewEntity(
        name = "TrackingCodeOrderReturnAndReturnHeader",
        packageName = "org.ofbiz.marketing.reports",
        title = "Return Header And Tracking Code Order Return View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "TCO", entityName = "TrackingCodeOrderReturn"),
            @MemberEntity(entityAlias = "RH", entityName = "ReturnHeader")
        },
        aliases = {
            @Alias(name = "returnId", entityAlias = "TCO"),
            @Alias(name = "orderId", entityAlias = "TCO"),
            @Alias(name = "orderItemSeqId", entityAlias = "TCO"),
            @Alias(name = "trackingCodeId", entityAlias = "TCO"),
            @Alias(name = "siteId", entityAlias = "TCO"),
            @Alias(name = "hasExported", entityAlias = "TCO"),
            @Alias(name = "affiliateReferredTimeStamp", entityAlias = "TCO"),
            @Alias(name = "statusId", entityAlias = "RH")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TCO",
                relEntityAlias = "RH",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            )
        }
    )
    public interface TrackingCodeOrderReturnAndReturnHeaderView {}

    /**
     * View entity for reporting number of visits for a marketing campaign.  Note that because          marketingCampaignId is a field of TrackingCode, this is really very similar to TrackingCodeAndVisit,          except the group-by is with marketingCampaignId instead of trackingCodeId
     */
    @ViewEntity(
        name = "MarketingCampaignAndVisit",
        packageName = "org.ofbiz.marketing.reports",
        title = "View entity for reporting number of visits for a marketing campaign.  Note that because          marketingCampaignId is a field of TrackingCode, this is really very similar to TrackingCodeAndVisit,          except the group-by is with marketingCampaignId instead of trackingCodeId",
        members = {
            @MemberEntity(entityAlias = "TC", entityName = "TrackingCode"),
            @MemberEntity(entityAlias = "TCV", entityName = "TrackingCodeVisit")
        },
        aliases = {
            @Alias(name = "marketingCampaignId", entityAlias = "TC", groupBy = true),
            @Alias(name = "visitId", entityAlias = "TCV", function = AggregateFunction.COUNT),
            @Alias(name = "fromDate", entityAlias = "TCV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TC",
                relEntityAlias = "TCV",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            )
        }
    )
    public interface MarketingCampaignAndVisitView {}

    /**
     * View entity for reporting number of orders and total order amounts
     */
    @ViewEntity(
        name = "MarketingCampaignAndOrderHeader",
        packageName = "org.ofbiz.marketing.reports",
        title = "View entity for reporting number of orders and total order amounts",
        members = {
            @MemberEntity(entityAlias = "TC", entityName = "TrackingCode"),
            @MemberEntity(entityAlias = "TCO", entityName = "TrackingCodeOrder"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "grandTotal", entityAlias = "OH", function = AggregateFunction.SUM),
            @Alias(name = "orderId", entityAlias = "TCO", function = AggregateFunction.COUNT),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "marketingCampaignId", entityAlias = "TC", groupBy = true)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TC",
                relEntityAlias = "TCO",
                keyMaps = {
                    @KeyMap(fieldName = "trackingCodeId")
                }
            ),
            @ViewLink(
                entityAlias = "TCO",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface MarketingCampaignAndOrderHeaderView {}

    /**
     * SalesOpportunity And Role View
     */
    @ViewEntity(
        name = "SalesOpportunityAndRole",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "SalesOpportunity And Role View",
        members = {
            @MemberEntity(entityAlias = "SO", entityName = "SalesOpportunity"),
            @MemberEntity(entityAlias = "SR", entityName = "SalesOpportunityRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SO")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "SR"),
            @Alias(name = "roleTypeId", entityAlias = "SR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SO",
                relEntityAlias = "SR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            )
        }
    )
    public interface SalesOpportunityAndRoleView {}

    /**
     * View for selecting the forecast with its time period.
     */
    @ViewEntity(
        name = "SalesForecastAndCustomTimePeriod",
        packageName = "org.ofbiz.marketing.opportunity",
        title = "View for selecting the forecast with its time period.",
        members = {
            @MemberEntity(entityAlias = "SF", entityName = "SalesForecast"),
            @MemberEntity(entityAlias = "CTP", entityName = "CustomTimePeriod")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SF"),
            @AliasAll(entityAlias = "CTP", excludes = {"organizationPartyId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SF",
                relEntityAlias = "CTP",
                keyMaps = {
                    @KeyMap(fieldName = "customTimePeriodId")
                }
            )
        }
    )
    public interface SalesForecastAndCustomTimePeriodView {}

}
