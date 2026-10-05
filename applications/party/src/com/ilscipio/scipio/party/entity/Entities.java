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
package com.ilscipio.scipio.party.entity;

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
     * Addendum
     */
    @Entity(
        name = "Addendum",
        packageName = "org.ofbiz.party.agreement",
        title = "Addendum",
        fields = {
            @Field(name = "addendumId", type = "id-ne"),
            @Field(name = "agreementId", type = "id"),
            @Field(name = "agreementItemSeqId", type = "id"),
            @Field(name = "addendumCreationDate", type = "date-time"),
            @Field(name = "addendumEffectiveDate", type = "date-time"),
            @Field(name = "addendumText", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "addendumId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "ADDNDM_AGRMNT",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "ADDNDM_AGRMNT_ITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            )
        }
    )
    public interface AddendumEntity {}

    /**
     * Agreement
     */
    @Entity(
        name = "Agreement",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "partyIdTo", type = "id"),
            @Field(name = "roleTypeIdFrom", type = "id"),
            @Field(name = "roleTypeIdTo", type = "id"),
            @Field(name = "agreementTypeId", type = "id"),
            @Field(name = "agreementDate", type = "date-time"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "description", type = "description"),
            @Field(name = "textData", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "AGRMNT_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
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
                relEntityName = "RoleType",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "From",
                fkName = "AGRMNT_FPRTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "To",
                fkName = "AGRMNT_TPRTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyRelationship",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom"),
                    @KeyMap(fieldName = "roleTypeIdTo"),
                    @KeyMap(fieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "partyIdTo")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementType",
                fkName = "AGRMNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "agreementTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "agreementTypeId")
                }
            )
        }
    )
    public interface AgreementEntity {}

    /**
     * Agreement Attribute
     */
    @Entity(
        name = "AgreementAttribute",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Attribute",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface AgreementAttributeEntity {}

    /**
     * Agreement Geographical Applicability
     */
    @Entity(
        name = "AgreementGeographicalApplic",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Geographical Applicability",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "geoId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_GEOAP_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_GEOAP_AGRI",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "AGRMNT_GEOAP_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface AgreementGeographicalApplicEntity {}

    /**
     * Agreement Item
     */
    @Entity(
        name = "AgreementItem",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "agreementItemTypeId", type = "id"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "agreementText", type = "very-long"),
            @Field(name = "agreementImage", type = "object")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_ITEM_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItemType",
                fkName = "AGRMNT_ITEM_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "agreementItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "agreementItemTypeId")
                }
            )
        }
    )
    public interface AgreementItemEntity {}

    /**
     * Agreement Item Attribute
     */
    @Entity(
        name = "AgreementItemAttribute",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item Attribute",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_ITEM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface AgreementItemAttributeEntity {}

    /**
     * Agreement Item Type
     */
    @Entity(
        name = "AgreementItemType",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "agreementItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItemType",
                title = "Parent",
                fkName = "AGRMNT_TYPEPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "agreementItemTypeId")
                }
            )
        }
    )
    public interface AgreementItemTypeEntity {}

    /**
     * Agreement Item Type Attribute
     */
    @Entity(
        name = "AgreementItemTypeAttr",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item Type Attribute",
        fields = {
            @Field(name = "agreementItemTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementItemTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItemType",
                fkName = "AGRMNT_ITEM_TYPATR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementItemAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementItem",
                keyMaps = {
                    @KeyMap(fieldName = "agreementItemTypeId")
                }
            )
        }
    )
    public interface AgreementItemTypeAttrEntity {}

    /**
     * Agreement Content
     */
    @Entity(
        name = "AgreementContent",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Content",
        fields = {
            @Field(name = "agreementId", type = "id"),
            @Field(name = "agreementItemSeqId", type = "id"),
            @Field(name = "agreementContentTypeId", type = "id"),
            @Field(name = "contentId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "agreementContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AG_CNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "AG_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementContentType",
                fkName = "AG_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "agreementContentTypeId")
                }
            )
        }
    )
    public interface AgreementContentEntity {}

    /**
     * Agreement Content Type
     */
    @Entity(
        name = "AgreementContentType",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Content Type",
        fields = {
            @Field(name = "agreementContentTypeId", type = "id"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementContentType",
                title = "Parent",
                fkName = "AGCT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "agreementContentTypeId")
                }
            )
        }
    )
    public interface AgreementContentTypeEntity {}

    /**
     * Agreement Party Application
     */
    @Entity(
        name = "AgreementPartyApplic",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Party Application",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_PTYA_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AgreementItem",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "AGRMNT_PTYA_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface AgreementPartyApplicEntity {}

    /**
     * Agreement Product Application
     */
    @Entity(
        name = "AgreementProductAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Product Application",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "price", type = "currency-precise")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_PRDA_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "AGRMNT_PRDA_PRD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface AgreementProductApplEntity {}

    /**
     * Agreement Promo Applicability
     */
    @Entity(
        name = "AgreementPromoAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Promo Applicability",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "AGRMNT_PROM_PRO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_PROM_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            )
        }
    )
    public interface AgreementPromoApplEntity {}

    /**
     * Agreement Facility Application
     */
    @Entity(
        name = "AgreementFacilityAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Facility Application",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "facilityId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_FACLT_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "AGRMNT_FACLT_PRD",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface AgreementFacilityApplEntity {}

    /**
     * Agreement Role
     */
    @Entity(
        name = "AgreementRole",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Role",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_ROLE_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "AGRMNT_ROLE_PTY",
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
                fkName = "AGRMNT_ROLE_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface AgreementRoleEntity {}

    /**
     * Agreement Term
     */
    @Entity(
        name = "AgreementTerm",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Term",
        fields = {
            @Field(name = "agreementTermId", type = "id-ne"),
            @Field(name = "termTypeId", type = "id"),
            @Field(name = "agreementId", type = "id"),
            @Field(name = "agreementItemSeqId", type = "id"),
            @Field(name = "invoiceItemTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "termValue", type = "currency-precise"),
            @Field(name = "termDays", type = "numeric"),
            @Field(name = "textValue", type = "description"),
            @Field(name = "minQuantity", type = "floating-point"),
            @Field(name = "maxQuantity", type = "floating-point"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementTermId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                fkName = "AGRMNT_TERM_TTYP",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_TERM_AGR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_TERM_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItemType",
                fkName = "AGRMNT_TERM_IIT",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceItemTypeId")
                }
            )
        }
    )
    public interface AgreementTermEntity {}

    /**
     * Agreement Term Attribute
     */
    @Entity(
        name = "AgreementTermAttribute",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Term Attribute",
        fields = {
            @Field(name = "agreementTermId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementTermId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementTerm",
                fkName = "AGRMNT_TERM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementTermId")
                }
            )
        }
    )
    public interface AgreementTermAttributeEntity {}

    /**
     * Agreement Type
     */
    @Entity(
        name = "AgreementType",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "agreementTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementType",
                title = "Parent",
                fkName = "AGRMNT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "agreementTypeId")
                }
            )
        }
    )
    public interface AgreementTypeEntity {}

    /**
     * Agreement Type Attribute
     */
    @Entity(
        name = "AgreementTypeAttr",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Type Attribute",
        fields = {
            @Field(name = "agreementTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementType",
                fkName = "AGRMNT_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementTypeId")
                }
            )
        }
    )
    public interface AgreementTypeAttrEntity {}

    /**
     * Agreement WorkEffort Application
     */
    @Entity(
        name = "AgreementWorkEffortApplic",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement WorkEffort Application",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Agreement",
                fkName = "AGRMNT_WEA_AGRMNT",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AgreementItem",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "AGRMNT_WEA_WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface AgreementWorkEffortApplicEntity {}

    /**
     * Term Type
     */
    @Entity(
        name = "TermType",
        packageName = "org.ofbiz.party.agreement",
        title = "Term Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                title = "Parent",
                fkName = "TERM_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "termTypeId")
                }
            )
        }
    )
    public interface TermTypeEntity {}

    /**
     * Term Type Attribute
     */
    @Entity(
        name = "TermTypeAttr",
        packageName = "org.ofbiz.party.agreement",
        title = "Term Type Attribute",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                fkName = "TERM_TYPATR_TTYP",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementTermAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementTerm",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTermAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTerm",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteTermAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteTerm",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceTermAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InvoiceTerm",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            )
        }
    )
    public interface TermTypeAttrEntity {}

    /**
     * Agreement Employment Application
     */
    @Entity(
        name = "AgreementEmploymentAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Employment Application",
        fields = {
            @Field(name = "agreementId", type = "id-ne"),
            @Field(name = "agreementItemSeqId", type = "id-ne"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "roleTypeIdFrom", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "agreementDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "agreementId"),
            @PrimaryKey(field = "agreementItemSeqId"),
            @PrimaryKey(field = "partyIdTo"),
            @PrimaryKey(field = "partyIdFrom"),
            @PrimaryKey(field = "roleTypeIdTo"),
            @PrimaryKey(field = "roleTypeIdFrom"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Employment",
                fkName = "AGRMNT_EMPL_APPL",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom"),
                    @KeyMap(fieldName = "roleTypeIdTo"),
                    @KeyMap(fieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "partyIdTo"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId", relFieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "agreementId", relFieldName = "partyIdTo")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "AGRMNT_EMPL_AITM",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            )
        }
    )
    public interface AgreementEmploymentApplEntity {}

    /**
     * CommunicationEvent Content Association Type
     */
    @Entity(
        name = "CommContentAssocType",
        packageName = "org.ofbiz.party.communication",
        title = "CommunicationEvent Content Association Type",
        fields = {
            @Field(name = "commContentAssocTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "commContentAssocTypeId")
        }
    )
    public interface CommContentAssocTypeEntity {}

    /**
     * CommunicationEvent Content Association
     */
    @Entity(
        name = "CommEventContentAssoc",
        packageName = "org.ofbiz.party.communication",
        title = "CommunicationEvent Content Association",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "commContentAssocTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "communicationEventId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "From",
                fkName = "COMMEV_CA_FROM",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COMMEV_CA_COMMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommContentAssocType",
                fkName = "COMMEV_CA_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "commContentAssocTypeId")
                }
            )
        }
    )
    public interface CommEventContentAssocEntity {}

    /**
     * Communication Event
     */
    @Entity(
        name = "CommunicationEvent",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event",
        fields = {
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "communicationEventTypeId", type = "id"),
            @Field(name = "origCommEventId", type = "id"),
            @Field(name = "parentCommEventId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "contactMechTypeId", type = "id"),
            @Field(name = "contactMechIdFrom", type = "id"),
            @Field(name = "contactMechIdTo", type = "id"),
            @Field(name = "roleTypeIdFrom", type = "id"),
            @Field(name = "roleTypeIdTo", type = "id"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "partyIdTo", type = "id"),
            @Field(name = "entryDate", type = "date-time"),
            @Field(name = "datetimeStarted", type = "date-time"),
            @Field(name = "datetimeEnded", type = "date-time"),
            @Field(name = "subject", type = "long-varchar"),
            @Field(name = "contentMimeTypeId", type = "id-vlong"),
            @Field(name = "content", type = "very-long"),
            @Field(name = "note", type = "comment"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "contactListId", type = "id"),
            @Field(name = "headerString", type = "very-long"),
            @Field(name = "fromString", type = "very-long"),
            @Field(name = "toString", type = "very-long"),
            @Field(name = "ccString", type = "very-long"),
            @Field(name = "bccString", type = "very-long"),
            @Field(name = "messageId", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEventType",
                fkName = "COM_EVNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "COM_EVNT_TPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "To",
                fkName = "COM_EVNT_TRTYP",
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
                relEntityName = "Party",
                title = "From",
                fkName = "COM_EVNT_FPTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "From",
                fkName = "COM_EVNT_FRTYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "COM_EVNT_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "COM_EVNT_CMTP",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "From",
                fkName = "COM_EVNT_FCM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechIdFrom", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "To",
                fkName = "COM_EVNT_TCM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechIdTo", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactList",
                fkName = "COM_EVNT_CLST",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MimeType",
                fkName = "COM_EVNT_MIMETYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contentMimeTypeId", relFieldName = "mimeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "COM_EVNT_RESENUM",
                keyMaps = {
                    @KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")
                }
            )
        },
        indexes = {
            @Index(
                name = "COMMEVT_MSG_ID",
                unique = true,
                fields = {
                    @IndexField(name = "messageId")
                }
            )
        }
    )
    public interface CommunicationEventEntity {}

    /**
     * Communication Event Product
     */
    @Entity(
        name = "CommunicationEventProduct",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event Product",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "COMEV_PROD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COMEV_PROD_CMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventProductEntity {}

    /**
     * Communication Event Purpose Type
     */
    @Entity(
        name = "CommunicationEventPrpTyp",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event Purpose Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "communicationEventPrpTypId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "communicationEventPrpTypId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEventPrpTyp",
                title = "Parent",
                fkName = "COM_EVNT_PRP_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "communicationEventPrpTypId")
                }
            )
        }
    )
    public interface CommunicationEventPrpTypEntity {}

    /**
     * Communication Event Purpose
     */
    @Entity(
        name = "CommunicationEventPurpose",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event Purpose",
        fields = {
            @Field(name = "communicationEventPrpTypId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "communicationEventPrpTypId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COM_EVNT_PRP_EVNT",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEventPrpTyp",
                fkName = "COM_EVNT_PRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventPrpTypId")
                }
            )
        }
    )
    public interface CommunicationEventPurposeEntity {}

    /**
     * Communication Event Role Entity showing all participants of the communication event.
     */
    @Entity(
        name = "CommunicationEventRole",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event Role Entity showing all participants of the communication event.",
        fields = {
            @Field(name = "communicationEventId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id", description = "For communication event participants this represents the contactMechId of the ContactMech used."),
            @Field(name = "statusId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "communicationEventId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COM_EVRL_CMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "COM_EVRL_PTY",
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
                fkName = "COM_EVRL_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "COM_EVRL_CMCH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "COM_EVRL_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface CommunicationEventRoleEntity {}

    /**
     * Communication Event Type
     */
    @Entity(
        name = "CommunicationEventType",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "communicationEventTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "contactMechTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "communicationEventTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEventType",
                title = "Parent",
                fkName = "COM_EVNT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "communicationEventTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                title = "ContacMechType",
                fkName = "COM_EVNT_TYPE_CMT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            )
        }
    )
    public interface CommunicationEventTypeEntity {}

    /**
     * Contact Mechanism
     */
    @Entity(
        name = "ContactMech",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism",
        fields = {
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "contactMechTypeId", type = "id"),
            @Field(name = "infoString", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "CONT_MECH_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContactMechTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            )
        },
        indexes = {
            @Index(
                name = "INFO_STRING_IDX",
                fields = {
                    @IndexField(name = "infoString")
                }
            )
        }
    )
    public interface ContactMechEntity {}

    /**
     * Contact Mechanism Attribute
     */
    @Entity(
        name = "ContactMechAttribute",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Attribute",
        fields = {
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "CONT_MECH_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContactMechTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface ContactMechAttributeEntity {}

    /**
     * Contact Mechanism Link
     */
    @Entity(
        name = "ContactMechLink",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Link",
        fields = {
            @Field(name = "contactMechIdFrom", type = "id-ne"),
            @Field(name = "contactMechIdTo", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechIdFrom"),
            @PrimaryKey(field = "contactMechIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "From",
                fkName = "CONT_MECH_FCMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechIdFrom", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "To",
                fkName = "CONT_MECH_TCMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechIdTo", relFieldName = "contactMechId")
                }
            )
        }
    )
    public interface ContactMechLinkEntity {}

    /**
     * Contact Mechanism Purpose Type
     */
    @Entity(
        name = "ContactMechPurposeType",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Purpose Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechPurposeTypeId")
        }
    )
    public interface ContactMechPurposeTypeEntity {}

    /**
     * Contact Mechanism Type
     */
    @Entity(
        name = "ContactMechType",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "contactMechTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                title = "Parent",
                fkName = "CONT_MECH_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "contactMechTypeId")
                }
            )
        }
    )
    public interface ContactMechTypeEntity {}

    /**
     * Contact Mechanism Type Attribute
     */
    @Entity(
        name = "ContactMechTypeAttr",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Type Attribute",
        fields = {
            @Field(name = "contactMechTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "CONT_MECH_TYP_ATR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContactMechAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            )
        }
    )
    public interface ContactMechTypeAttrEntity {}

    /**
     * Contact Mechanism Type Purpose
     * Defines which ContactMechPurposeType entites apply to which ContactMechType
     */
    @Entity(
        name = "ContactMechTypePurpose",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mechanism Type Purpose",
        description = "Defines which ContactMechPurposeType entites apply to which ContactMechType",
        fields = {
            @Field(name = "contactMechTypeId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechTypeId"),
            @PrimaryKey(field = "contactMechPurposeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "CONT_MECH_TP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "CONT_MECH_TP_PRPTP",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        }
    )
    public interface ContactMechTypePurposeEntity {}

    /**
     * Email Address Verification
     * Holds hashes for email address verification
     */
    @Entity(
        name = "EmailAddressVerification",
        packageName = "org.ofbiz.party.contact",
        title = "Email Address Verification",
        description = "Holds hashes for email address verification",
        fields = {
            @Field(name = "emailAddress", type = "id-vlong-ne"),
            @Field(name = "verifyHash", type = "value"),
            @Field(name = "expireDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "emailAddress")
        },
        indexes = {
            @Index(
                name = "EMAIL_VERIFY_HASH",
                unique = true,
                fields = {
                    @IndexField(name = "verifyHash")
                }
            )
        }
    )
    public interface EmailAddressVerificationEntity {}

    /**
     * Party Contact Mechanism
     */
    @Entity(
        name = "PartyContactMech",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mechanism",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "allowSolicitation", type = "indicator"),
            @Field(name = "extension", type = "long-varchar"),
            @Field(name = "verified", type = "indicator"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "yearsWithContactMech", type = "numeric"),
            @Field(name = "monthsWithContactMech", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_CMECH_PARTY",
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
                relEntityName = "PartyGroup",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "PARTY_CMECH_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "PARTY_CMECH_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "PARTY_CMECH_CMECH",
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
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMechPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechEntity {}

    /**
     * Party Contact Mechanism Purpose
     */
    @Entity(
        name = "PartyContactMechPurpose",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mechanism Purpose",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "contactMechPurposeTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "PARTY_CMPRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_CMPRP_PARTY",
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
                relEntityName = "PartyGroup",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "PARTY_CMPRP_CMECH",
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechPurposeEntity {}

    /**
     * Postal Address
     */
    @Entity(
        name = "PostalAddress",
        packageName = "org.ofbiz.party.contact",
        title = "Postal Address",
        fields = {
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "toName", type = "name"),
            @Field(name = "attnName", type = "name"),
            @Field(name = "address1", type = "long-varchar"),
            @Field(name = "address2", type = "long-varchar"),
            @Field(name = "directions", type = "long-varchar"),
            @Field(name = "city", type = "name"),
            @Field(name = "postalCode", type = "short-varchar"),
            @Field(name = "postalCodeExt", type = "short-varchar"),
            @Field(name = "countryGeoId", type = "id"),
            @Field(name = "stateProvinceGeoId", type = "id"),
            @Field(name = "countyGeoId", type = "id"),
            @Field(name = "postalCodeGeoId", type = "id"),
            @Field(name = "geoPointId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "POST_ADDR_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Country",
                fkName = "POST_ADDR_CGEO",
                keyMaps = {
                    @KeyMap(fieldName = "countryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "StateProvince",
                fkName = "POST_ADDR_SPGEO",
                keyMaps = {
                    @KeyMap(fieldName = "stateProvinceGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "County",
                fkName = "POST_ADDR_CNTG",
                keyMaps = {
                    @KeyMap(fieldName = "countyGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "PostalCode",
                fkName = "POST_ADDR_PCGEO",
                keyMaps = {
                    @KeyMap(fieldName = "postalCodeGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "POST_ADDR_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        },
        indexes = {
            @Index(
                name = "ADDRESS1_IDX",
                fields = {
                    @IndexField(name = "address1")
                }
            ),
            @Index(
                name = "ADDRESS2_IDX",
                fields = {
                    @IndexField(name = "address2")
                }
            ),
            @Index(
                name = "CITY_IDX",
                fields = {
                    @IndexField(name = "city")
                }
            ),
            @Index(
                name = "POSTAL_CODE_IDX",
                fields = {
                    @IndexField(name = "postalCode")
                }
            )
        }
    )
    public interface PostalAddressEntity {}

    /**
     * Postal Address Boundary
     */
    @Entity(
        name = "PostalAddressBoundary",
        packageName = "org.ofbiz.party.contact",
        title = "Postal Address Boundary",
        fields = {
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "geoId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "POST_ADDR_BNDRY",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "POST_ADDR_BNDRYGEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface PostalAddressBoundaryEntity {}

    /**
     * Telecommunications Number
     */
    @Entity(
        name = "TelecomNumber",
        packageName = "org.ofbiz.party.contact",
        title = "Telecommunications Number",
        fields = {
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "countryCode", type = "very-short"),
            @Field(name = "areaCode", type = "very-short"),
            @Field(name = "contactNumber", type = "short-varchar"),
            @Field(name = "askForName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "TEL_NUM_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        indexes = {
            @Index(
                name = "COUNTRY_CODE_IDX",
                fields = {
                    @IndexField(name = "countryCode")
                }
            ),
            @Index(
                name = "AREA_CODE_IDX",
                fields = {
                    @IndexField(name = "areaCode")
                }
            ),
            @Index(
                name = "CONTACT_NUMBER_IDX",
                fields = {
                    @IndexField(name = "contactNumber")
                }
            )
        }
    )
    public interface TelecomNumberEntity {}

    /**
     * Ftp server
     */
    @Entity(
        name = "FtpAddress",
        packageName = "org.ofbiz.party.contact",
        title = "Ftp server",
        fields = {
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "hostname", type = "long-varchar"),
            @Field(name = "port", type = "numeric"),
            @Field(name = "username", type = "long-varchar"),
            @Field(name = "ftpPassword", type = "long-varchar", encrypt = "true"),
            @Field(name = "binaryTransfer", type = "indicator"),
            @Field(name = "filePath", type = "long-varchar"),
            @Field(name = "zipFile", type = "indicator"),
            @Field(name = "passiveMode", type = "indicator"),
            @Field(name = "defaultTimeout", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "FTP_SRV_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface FtpAddressEntity {}

    /**
     * Valid Contact Mechanism Role
     */
    @Entity(
        name = "ValidContactMechRole",
        packageName = "org.ofbiz.party.contact",
        title = "Valid Contact Mechanism Role",
        fields = {
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "contactMechTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "contactMechTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "VAL_CMRLE_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechType",
                fkName = "VAL_CMRLE_CMTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            )
        }
    )
    public interface ValidContactMechRoleEntity {}

    /**
     * Need Type
     */
    @Entity(
        name = "NeedType",
        packageName = "org.ofbiz.party.need",
        title = "Need Type",
        fields = {
            @Field(name = "needTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "needTypeId")
        }
    )
    public interface NeedTypeEntity {}

    /**
     * Party Need
     */
    @Entity(
        name = "PartyNeed",
        packageName = "org.ofbiz.party.need",
        title = "Party Need",
        fields = {
            @Field(name = "partyNeedId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "partyTypeId", type = "id"),
            @Field(name = "needTypeId", type = "id"),
            @Field(name = "communicationEventId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productCategoryId", type = "id"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "datetimeRecorded", type = "date-time"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyNeedId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NeedType",
                fkName = "PARTY_NEED_NDTP",
                keyMaps = {
                    @KeyMap(fieldName = "needTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_NEED_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "PARTY_NEED_RTYP",
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
                relEntityName = "PartyType",
                fkName = "PARTY_NEED_PTTP",
                keyMaps = {
                    @KeyMap(fieldName = "partyTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "PARTY_NEED_CMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PARTY_NEED_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PARTY_NEED_PCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface PartyNeedEntity {}

    /**
     * Address Matching Map
     */
    @Entity(
        name = "AddressMatchMap",
        packageName = "org.ofbiz.party.party",
        title = "Address Matching Map",
        fields = {
            @Field(name = "mapKey", type = "id-vlong"),
            @Field(name = "mapValue", type = "id-vlong"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "mapKey"),
            @PrimaryKey(field = "mapValue")
        }
    )
    public interface AddressMatchMapEntity {}

    /**
     * Affiliate Party
     */
    @Entity(
        name = "Affiliate",
        packageName = "org.ofbiz.party.party",
        title = "Affiliate Party",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "affiliateName", type = "name"),
            @Field(name = "affiliateDescription", type = "description"),
            @Field(name = "yearEstablished", type = "very-short"),
            @Field(name = "siteType", type = "comment"),
            @Field(name = "sitePageViews", type = "comment"),
            @Field(name = "siteVisitors", type = "comment"),
            @Field(name = "dateTimeCreated", type = "date-time"),
            @Field(name = "dateTimeApproved", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "AFFILIATE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyGroup",
                fkName = "AFFILIATE_PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface AffiliateEntity {}

    /**
     * Party
     */
    @Entity(
        name = "Party",
        packageName = "org.ofbiz.party.party",
        title = "Party",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "partyTypeId", type = "id-ne"),
            @Field(name = "externalId", type = "id"),
            @Field(name = "preferredCurrencyUomId", type = "id-ne"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "isUnread", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyType",
                fkName = "PARTY_PTY_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "partyTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PARTY_CUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PARTY_LMCUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "PARTY_PREF_CRNCY",
                keyMaps = {
                    @KeyMap(fieldName = "preferredCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PARTY_STATUSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "partyTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "PARTY_DATSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PARTYEXT_ID_IDX",
                fields = {
                    @IndexField(name = "externalId")
                }
            )
        }
    )
    public interface PartyEntity {}

    /**
     * Party Identification
     */
    @Entity(
        name = "PartyIdentification",
        packageName = "org.ofbiz.party.party",
        title = "Party Identification",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "partyIdentificationTypeId", type = "id-ne"),
            @Field(name = "idValue", type = "id-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "partyIdentificationTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyIdentificationType",
                fkName = "PARTY_ID_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdentificationTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_ID_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PARTY_ID_VALIDX",
                fields = {
                    @IndexField(name = "idValue")
                }
            )
        }
    )
    public interface PartyIdentificationEntity {}

    /**
     * Party Identification Type
     */
    @Entity(
        name = "PartyIdentificationType",
        packageName = "org.ofbiz.party.party",
        title = "Party Identification Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "partyIdentificationTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyIdentificationTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyIdentificationType",
                title = "Parent",
                fkName = "PARTY_ID_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyIdentificationTypeId")
                }
            )
        }
    )
    public interface PartyIdentificationTypeEntity {}

    /**
     * Party Geo Location with history
     */
    @Entity(
        name = "PartyGeoPoint",
        packageName = "org.ofbiz.party.party",
        title = "Party Geo Location with history",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "geoPointId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "geoPointId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTYGEOPT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "PARTYGEOPT_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface PartyGeoPointEntity {}

    /**
     * Party Attribute
     */
    @Entity(
        name = "PartyAttribute",
        packageName = "org.ofbiz.party.party",
        title = "Party Attribute",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface PartyAttributeEntity {}

    /**
     * Party Carrier Account
     */
    @Entity(
        name = "PartyCarrierAccount",
        packageName = "org.ofbiz.party.party",
        title = "Party Carrier Account",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "carrierPartyId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "accountNumber", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "carrierPartyId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_CRRACT_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Carrier",
                fkName = "PARTY_CRRACT_CPT",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface PartyCarrierAccountEntity {}

    /**
     * Party Classification
     */
    @Entity(
        name = "PartyClassification",
        packageName = "org.ofbiz.party.party",
        title = "Party Classification",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "partyClassificationGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "partyClassificationGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_CLASS_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationGroup",
                fkName = "PARTY_CLASS_GRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationGroupId")
                }
            )
        }
    )
    public interface PartyClassificationEntity {}

    /**
     * Party Classification Group
     */
    @Entity(
        name = "PartyClassificationGroup",
        packageName = "org.ofbiz.party.party",
        title = "Party Classification Group",
        fields = {
            @Field(name = "partyClassificationGroupId", type = "id-ne"),
            @Field(name = "partyClassificationTypeId", type = "id"),
            @Field(name = "parentGroupId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyClassificationGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationGroup",
                title = "Parent",
                fkName = "PARTY_CLASS_GRPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentGroupId", relFieldName = "partyClassificationGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationType",
                fkName = "PARTY_CLSGRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationTypeId")
                }
            )
        }
    )
    public interface PartyClassificationGroupEntity {}

    /**
     * Party Classification Type
     */
    @Entity(
        name = "PartyClassificationType",
        packageName = "org.ofbiz.party.party",
        title = "Party Classification Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "partyClassificationTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyClassificationTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationType",
                title = "Parent",
                fkName = "PARTY_CLASS_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyClassificationTypeId")
                }
            )
        }
    )
    public interface PartyClassificationTypeEntity {}

    /**
     * Party Data Object
     */
    @Entity(
        name = "PartyContent",
        packageName = "org.ofbiz.party.party",
        title = "Party Data Object",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "partyContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "partyContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_CNT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PARTY_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyContentType",
                fkName = "PARTY_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "partyContentTypeId")
                }
            )
        }
    )
    public interface PartyContentEntity {}

    /**
     * Party Content Type
     */
    @Entity(
        name = "PartyContentType",
        packageName = "org.ofbiz.party.party",
        title = "Party Content Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "partyContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyContentType",
                title = "Parent",
                fkName = "PARTYCNT_TP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyContentTypeId")
                }
            )
        }
    )
    public interface PartyContentTypeEntity {}

    /**
     * Party Data Source
     */
    @Entity(
        name = "PartyDataSource",
        packageName = "org.ofbiz.party.party",
        title = "Party Data Source",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "isCreate", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "dataSourceId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_DATSRC_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "PARTY_DATSRC_DSC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            )
        }
    )
    public interface PartyDataSourceEntity {}

    /**
     * Party Group
     */
    @Entity(
        name = "PartyGroup",
        packageName = "org.ofbiz.party.party",
        title = "Party Group",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "groupName", type = "name"),
            @Field(name = "groupNameLocal", type = "name"),
            @Field(name = "officeSiteName", type = "name"),
            @Field(name = "annualRevenue", type = "currency-amount"),
            @Field(name = "numEmployees", type = "numeric"),
            @Field(name = "tickerSymbol", type = "very-short"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "logoImageUrl", type = "url")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_GRP_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        indexes = {
            @Index(
                name = "GROUP_NAME_IDX",
                fields = {
                    @IndexField(name = "groupName")
                }
            )
        }
    )
    public interface PartyGroupEntity {}

    /**
     * Party ICS AVS Override
     */
    @Entity(
        name = "PartyIcsAvsOverride",
        packageName = "org.ofbiz.party.party",
        title = "Party ICS AVS Override",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "avsDeclineString", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_ICSAVS_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyIcsAvsOverrideEntity {}

    /**
     * Party Invitation
     */
    @Entity(
        name = "PartyInvitation",
        packageName = "org.ofbiz.party.party",
        title = "Party Invitation",
        fields = {
            @Field(name = "partyInvitationId", type = "id-ne"),
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "toName", type = "name"),
            @Field(name = "emailAddress", type = "long-varchar"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "lastInviteDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyInvitationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PTYINV_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PTYINV_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface PartyInvitationEntity {}

    /**
     * Party Invitation Group Assoc
     */
    @Entity(
        name = "PartyInvitationGroupAssoc",
        packageName = "org.ofbiz.party.party",
        title = "Party Invitation Group Assoc",
        fields = {
            @Field(name = "partyInvitationId", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyInvitationId"),
            @PrimaryKey(field = "partyIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyGroup",
                title = "To",
                fkName = "PTYINVGA_PTYGRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "PTYINVGA_PTYTO",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyInvitation",
                fkName = "PTYINVGA_PTYINV",
                keyMaps = {
                    @KeyMap(fieldName = "partyInvitationId")
                }
            )
        }
    )
    public interface PartyInvitationGroupAssocEntity {}

    /**
     * Party Invitation Role Assoc
     */
    @Entity(
        name = "PartyInvitationRoleAssoc",
        packageName = "org.ofbiz.party.party",
        title = "Party Invitation Role Assoc",
        fields = {
            @Field(name = "partyInvitationId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyInvitationId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "PTYINVROLE_ROLET",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyInvitation",
                fkName = "PTYINVROLE_PTYINV",
                keyMaps = {
                    @KeyMap(fieldName = "partyInvitationId")
                }
            )
        }
    )
    public interface PartyInvitationRoleAssocEntity {}

    /**
     * Party Name History
     */
    @Entity(
        name = "PartyNameHistory",
        packageName = "org.ofbiz.party.party",
        title = "Party Name History",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "changeDate", type = "date-time"),
            @Field(name = "groupName", type = "name", description = "For Party Groups only"),
            @Field(name = "firstName", type = "name"),
            @Field(name = "middleName", type = "name"),
            @Field(name = "lastName", type = "name"),
            @Field(name = "personalTitle", type = "name"),
            @Field(name = "suffix", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "changeDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PTY_NMHIS_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyNameHistoryEntity {}

    /**
     * Party Note
     */
    @Entity(
        name = "PartyNote",
        packageName = "org.ofbiz.party.party",
        title = "Party Note",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_NOTE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "PARTY_NOTE_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface PartyNoteEntity {}

    /**
     * Party Profile Defaults
     */
    @Entity(
        name = "PartyProfileDefault",
        packageName = "org.ofbiz.party.party",
        title = "Party Profile Defaults",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "defaultShipAddr", type = "id"),
            @Field(name = "defaultBillAddr", type = "id"),
            @Field(name = "defaultPayMeth", type = "id"),
            @Field(name = "defaultShipMeth", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "productStoreId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_PROF_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PARTY_PROF_PSTORE",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface PartyProfileDefaultEntity {}

    /**
     * Party Relationship
     */
    @Entity(
        name = "PartyRelationship",
        packageName = "org.ofbiz.party.party",
        title = "Party Relationship",
        fields = {
            @Field(name = "partyIdFrom", type = "id-ne"),
            @Field(name = "partyIdTo", type = "id-ne"),
            @Field(name = "roleTypeIdFrom", type = "id-ne"),
            @Field(name = "roleTypeIdTo", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "relationshipName", type = "name", description = "Official name of relationship, such as title in a company"),
            @Field(name = "securityGroupId", type = "id-ne"),
            @Field(name = "priorityTypeId", type = "id"),
            @Field(name = "partyRelationshipTypeId", type = "id"),
            @Field(name = "permissionsEnumId", type = "id-ne"),
            @Field(name = "positionTitle", type = "name", description = "The exact word used within the company"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyIdFrom"),
            @PrimaryKey(field = "partyIdTo"),
            @PrimaryKey(field = "roleTypeIdFrom"),
            @PrimaryKey(field = "roleTypeIdTo"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
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
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "From",
                fkName = "PARTY_REL_FPROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "To",
                fkName = "PARTY_REL_TPROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdTo", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PARTY_REL_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PriorityType",
                fkName = "PARTY_REL_PRTYP",
                keyMaps = {
                    @KeyMap(fieldName = "priorityTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRelationshipType",
                fkName = "PARTY_REL_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "partyRelationshipTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "PARTY_REL_SECGRP",
                keyMaps = {
                    @KeyMap(fieldName = "securityGroupId", relFieldName = "groupId")
                }
            )
        }
    )
    public interface PartyRelationshipEntity {}

    /**
     * Party Relationship Type
     */
    @Entity(
        name = "PartyRelationshipType",
        packageName = "org.ofbiz.party.party",
        title = "Party Relationship Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "partyRelationshipTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "partyRelationshipName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "roleTypeIdValidFrom", type = "id"),
            @Field(name = "roleTypeIdValidTo", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyRelationshipTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRelationshipType",
                title = "Parent",
                fkName = "PARTY_RELTYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyRelationshipTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "ValidFrom",
                fkName = "PARTY_RELTYP_VFRT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdValidFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "ValidTo",
                fkName = "PARTY_RELTYP_VTRT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdValidTo", relFieldName = "roleTypeId")
                }
            )
        }
    )
    public interface PartyRelationshipTypeEntity {}

    /**
     * Party Role
     */
    @Entity(
        name = "PartyRole",
        packageName = "org.ofbiz.party.party",
        title = "Party Role",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_RLE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "PARTY_RLE_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "RoleTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyRoleEntity {}

    /**
     * Tracks a history of the status of a Party
     */
    @Entity(
        name = "PartyStatus",
        packageName = "org.ofbiz.party.party",
        title = "Tracks a history of the status of a Party",
        fields = {
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "statusDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PARTY_STS_STSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_STS_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "PARTY_STTS_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface PartyStatusEntity {}

    /**
     * Party Tax Information
     * NOTE: this entity is deprecated by PartyTaxAuthInfo
     */
    @Entity(
        name = "OldPartyTaxInfo",
        packageName = "org.ofbiz.party.party",
        tableName = "PARTY_TAX_INFO",
        title = "Party Tax Information",
        description = "NOTE: this entity is deprecated by PartyTaxAuthInfo",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "geoId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "partyTaxId", type = "id-long-ne"),
            @Field(name = "isExempt", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "geoId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PARTY_TXI_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "PARTY_TXI_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface OldPartyTaxInfoEntity {}

    /**
     * Party Type
     */
    @Entity(
        name = "PartyType",
        packageName = "org.ofbiz.party.party",
        title = "Party Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "partyTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyType",
                title = "Parent",
                fkName = "PARTY_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "partyTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyType",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId")
                }
            )
        }
    )
    public interface PartyTypeEntity {}

    /**
     * Party Type Attribute
     */
    @Entity(
        name = "PartyTypeAttr",
        packageName = "org.ofbiz.party.party",
        title = "Party Type Attribute",
        fields = {
            @Field(name = "partyTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyType",
                fkName = "PARTY_TYP_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "partyTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyTypeId")
                }
            )
        }
    )
    public interface PartyTypeAttrEntity {}

    /**
     * Person
     */
    @Entity(
        name = "Person",
        packageName = "org.ofbiz.party.party",
        title = "Person",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "salutation", type = "name"),
            @Field(name = "firstName", type = "name"),
            @Field(name = "middleName", type = "name"),
            @Field(name = "lastName", type = "name"),
            @Field(name = "personalTitle", type = "name"),
            @Field(name = "suffix", type = "name"),
            @Field(name = "nickname", type = "name"),
            @Field(name = "firstNameLocal", type = "name"),
            @Field(name = "middleNameLocal", type = "name"),
            @Field(name = "lastNameLocal", type = "name"),
            @Field(name = "otherLocal", type = "name"),
            @Field(name = "memberId", type = "id"),
            @Field(name = "gender", type = "indicator"),
            @Field(name = "birthDate", type = "date"),
            @Field(name = "deceasedDate", type = "date"),
            @Field(name = "height", type = "floating-point"),
            @Field(name = "weight", type = "floating-point"),
            @Field(name = "mothersMaidenName", type = "long-varchar", encrypt = "true"),
            @Field(name = "maritalStatus", type = "indicator"),
            @Field(name = "socialSecurityNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "passportNumber", type = "long-varchar", encrypt = "true"),
            @Field(name = "passportExpireDate", type = "date"),
            @Field(name = "totalYearsWorkExperience", type = "floating-point"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "employmentStatusEnumId", type = "id"),
            @Field(name = "residenceStatusEnumId", type = "id"),
            @Field(name = "occupation", type = "name"),
            @Field(name = "yearsWithEmployer", type = "numeric"),
            @Field(name = "monthsWithEmployer", type = "numeric"),
            @Field(name = "existingCustomer", type = "indicator"),
            @Field(name = "cardId", type = "id-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PERSON_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "EmploymentStatus",
                fkName = "PERSON_EMPS_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "employmentStatusEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ResidenceStatus",
                fkName = "PERSON_RESS_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "residenceStatusEnumId", relFieldName = "enumId")
                }
            )
        },
        indexes = {
            @Index(
                name = "FIRST_NAME_IDX",
                fields = {
                    @IndexField(name = "firstName")
                }
            ),
            @Index(
                name = "LAST_NAME_IDX",
                fields = {
                    @IndexField(name = "lastName")
                }
            ),
            @Index(
                name = "CARD_ID_IDX",
                unique = true,
                fields = {
                    @IndexField(name = "cardId")
                }
            )
        }
    )
    public interface PersonEntity {}

    /**
     * Priority Type
     */
    @Entity(
        name = "PriorityType",
        packageName = "org.ofbiz.party.party",
        title = "Priority Type",
        fields = {
            @Field(name = "priorityTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "priorityTypeId")
        }
    )
    public interface PriorityTypeEntity {}

    /**
     * Role Type
     */
    @Entity(
        name = "RoleType",
        packageName = "org.ofbiz.party.party",
        title = "Role Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "Parent",
                fkName = "ROLE_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "roleTypeId")
                }
            )
        }
    )
    public interface RoleTypeEntity {}

    /**
     * Role Type Attribute
     */
    @Entity(
        name = "RoleTypeAttr",
        packageName = "org.ofbiz.party.party",
        title = "Role Type Attribute",
        fields = {
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "ROLE_TYPATR_RTYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyRelationshipType",
                title = "ValidFrom",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId", relFieldName = "roleTypeIdValidFrom")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyRelationshipType",
                title = "ValidTo",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId", relFieldName = "roleTypeIdValidTo")
                }
            )
        }
    )
    public interface RoleTypeAttrEntity {}

    /**
     * Vendor
     */
    @Entity(
        name = "Vendor",
        packageName = "org.ofbiz.party.party",
        title = "Vendor",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "manifestCompanyName", type = "name"),
            @Field(name = "manifestCompanyTitle", type = "name"),
            @Field(name = "manifestLogoUrl", type = "url"),
            @Field(name = "manifestPolicies", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "VENDOR_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface VendorEntity {}

    /**
     * Role Type
     */
    @Entity(
        name = "ServerHitBucketStats",
        packageName = "org.ofbiz.party.party",
        title = "Role Type",
        defaultResourceName = "PartyEntityLabels",
        fields = {
            @Field(name = "statsId", type = "id-ne"),
            @Field(name = "serverHostName", type = "id-ne"),
            @Field(name = "date", type = "date-time", description = "NOTE: This date is truncated to minutes based on bucketMinutes"),
            @Field(name = "bucketMinutes", type = "numeric"),
            @Field(name = "count", type = "numeric"),
            @Field(name = "contentIds", type = "very-long", description = "Comma-separated list"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "entityJson", type = "json-object", description = "JSON fields for extra data")
        },
        primaryKeys = {
            @PrimaryKey(field = "statsId")
        },
        indexes = {
            @Index(
                name = "HITBCKT_DATE",
                fields = {
                    @IndexField(name = "serverHostName"),
                    @IndexField(name = "date")
                }
            ),
            @Index(
                name = "HITBCKT_FROMDATE",
                fields = {
                    @IndexField(name = "serverHostName"),
                    @IndexField(name = "fromDate")
                }
            ),
            @Index(
                name = "HITBCKT_THRUDATE",
                fields = {
                    @IndexField(name = "serverHostName"),
                    @IndexField(name = "thruDate")
                }
            )
        }
    )
    public interface ServerHitBucketStatsEntity {}

    /**
     * Agreement Item and Agreement Product Applicability View
     */
    @ViewEntity(
        name = "AgreementItemAndProductAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item and Agreement Product Applicability View",
        members = {
            @MemberEntity(entityAlias = "AGI", entityName = "AgreementItem"),
            @MemberEntity(entityAlias = "AGPA", entityName = "AgreementProductAppl"),
            @MemberEntity(entityAlias = "AG", entityName = "Agreement")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "AGI"),
            @AliasAll(entityAlias = "AGPA"),
            @AliasAll(entityAlias = "AG", excludes = {"productId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AGPA",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AG",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        }
    )
    public interface AgreementItemAndProductApplView {}

    /**
     * Agreement Item and Agreement Facility Applicability View
     */
    @ViewEntity(
        name = "AgreementItemAndFacilityAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item and Agreement Facility Applicability View",
        members = {
            @MemberEntity(entityAlias = "AGI", entityName = "AgreementItem"),
            @MemberEntity(entityAlias = "AGFA", entityName = "AgreementFacilityAppl"),
            @MemberEntity(entityAlias = "AG", entityName = "Agreement")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "AGI"),
            @AliasAll(entityAlias = "AGFA"),
            @AliasAll(entityAlias = "AG")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AGFA",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AG",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Agreement",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        }
    )
    public interface AgreementItemAndFacilityApplView {}

    /**
     * Agreement Item and Agreement Party Applicability View
     */
    @ViewEntity(
        name = "AgreementItemAndPartyAppl",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement Item and Agreement Party Applicability View",
        members = {
            @MemberEntity(entityAlias = "AGI", entityName = "AgreementItem"),
            @MemberEntity(entityAlias = "AGPA", entityName = "AgreementPartyApplic"),
            @MemberEntity(entityAlias = "AG", entityName = "Agreement")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "AGI"),
            @AliasAll(entityAlias = "AGPA"),
            @AliasAll(entityAlias = "AG", excludes = {"productId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AGPA",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "AGI",
                relEntityAlias = "AG",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        }
    )
    public interface AgreementItemAndPartyApplView {}

    /**
     * AgreementContent Content and DataResource View
     */
    @ViewEntity(
        name = "AgreementContentAndInfo",
        packageName = "org.ofbiz.accounting.invoice",
        title = "AgreementContent Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "AGC", entityName = "AgreementContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "AGC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "AGC",
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
    public interface AgreementContentAndInfoView {}

    /**
     * Agreement and AgreementRole View
     */
    @ViewEntity(
        name = "AgreementAndRole",
        packageName = "org.ofbiz.party.agreement",
        title = "Agreement and AgreementRole View",
        members = {
            @MemberEntity(entityAlias = "AGR", entityName = "Agreement"),
            @MemberEntity(entityAlias = "AR", entityName = "AgreementRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "AGR"),
            @AliasAll(entityAlias = "AR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "AGR",
                relEntityAlias = "AR",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
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
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "To",
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
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "AgreementRole",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId")
                }
            )
        }
    )
    public interface AgreementAndRoleView {}

    /**
     * CommEvent and Content and DataResource View
     */
    @ViewEntity(
        name = "CommEventContentDataResource",
        packageName = "org.ofbiz.party.communication",
        title = "CommEvent and Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "CECA", entityName = "CommEventContentAssoc"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CECA"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CECA",
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
                relEntityName = "CommunicationEvent",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
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
    public interface CommEventContentDataResourceView {}

    /**
     * Communication Event And Role View
     */
    @ViewEntity(
        name = "CommunicationEventAndSubscr",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event And Role View",
        members = {
            @MemberEntity(entityAlias = "SC", entityName = "SubscriptionCommEvent"),
            @MemberEntity(entityAlias = "SU", entityName = "Subscription")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SC", excludes = {"communicationEventId"}),
            @AliasAll(entityAlias = "SU")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SC",
                relEntityAlias = "SU",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionId")
                }
            )
        }
    )
    public interface CommunicationEventAndSubscrView {}

    /**
     * Communication Event And Product View
     */
    @ViewEntity(
        name = "CommunicationEventAndProduct",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event And Product View",
        members = {
            @MemberEntity(entityAlias = "CP", entityName = "CommunicationEventProduct"),
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CP"),
            @AliasAll(entityAlias = "CE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CP",
                relEntityAlias = "CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventAndProductView {}

    /**
     * Communication Event And Role View
     */
    @ViewEntity(
        name = "CommunicationEventAndRole",
        packageName = "org.ofbiz.party.communication",
        title = "Communication Event And Role View",
        members = {
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent"),
            @MemberEntity(entityAlias = "CR", entityName = "CommunicationEventRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CE")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "CR"),
            @Alias(name = "roleTypeId", entityAlias = "CR"),
            @Alias(name = "roleStatusId", entityAlias = "CR", field = "statusId"),
            @Alias(name = "contactMechId", entityAlias = "CR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CE",
                relEntityAlias = "CR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
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
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CommunicationEventType",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CommunicationEventPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "To",
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
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeIdFrom", relFieldName = "roleTypeId")
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
                relEntityName = "ContactMechType",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CommunicationEventRole",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyNeed",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CommunicationEventWorkEff",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventAndRoleView {}

    /**
     * Sum of communication events over status
     */
    @ViewEntity(
        name = "CommunicationEventSum",
        packageName = "org.ofbiz.party.communication",
        title = "Sum of communication events over status",
        members = {
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliases = {
            @Alias(name = "communicationEventId", entityAlias = "CE", function = AggregateFunction.COUNT),
            @Alias(name = "statusId", entityAlias = "CE"),
            @Alias(name = "partyIdTo", entityAlias = "CE", groupBy = true)
        }
    )
    public interface CommunicationEventSumView {}

    /**
     * Party Contact Purpose View
     */
    @ViewEntity(
        name = "PartyContactDetailByPurpose",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Purpose View",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PCMP", entityName = "PartyContactMechPurpose"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber"),
            @MemberEntity(entityAlias = "STTG", entityName = "Geo"),
            @MemberEntity(entityAlias = "CTYG", entityName = "Geo"),
            @MemberEntity(entityAlias = "CTRYG", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "PA"),
            @AliasAll(entityAlias = "TN"),
            @AliasAll(entityAlias = "STTG", prefix = "state"),
            @AliasAll(entityAlias = "CTYG", prefix = "county"),
            @AliasAll(entityAlias = "CTRYG", prefix = "country")
        },
        aliases = {
            @Alias(name = "contactMechPurposeTypeId", entityAlias = "PCMP"),
            @Alias(name = "purposeFromDate", entityAlias = "PCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "PCMP", field = "thruDate"),
            @Alias(name = "contactMechTypeId", entityAlias = "CM"),
            @Alias(name = "infoString", entityAlias = "CM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCMP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PA",
                relEntityAlias = "STTG",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "stateProvinceGeoId", relFieldName = "geoId")
                }
            ),
            @ViewLink(
                entityAlias = "PA",
                relEntityAlias = "CTYG",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "countyGeoId", relFieldName = "geoId")
                }
            ),
            @ViewLink(
                entityAlias = "PA",
                relEntityAlias = "CTRYG",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "countryGeoId", relFieldName = "geoId")
                }
            )
        }
    )
    public interface PartyContactDetailByPurposeView {}

    /**
     * Contact Mech Detail View
     */
    @ViewEntity(
        name = "ContactMechDetail",
        packageName = "org.ofbiz.party.contact",
        title = "Contact Mech Detail View",
        members = {
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa", excludes = {"contactMechId"}),
            @AliasAll(entityAlias = "TN", prefix = "tn", excludes = {"contactMechId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface ContactMechDetailView {}

    /**
     * Party Contact Mech and Contact Mech View
     */
    @ViewEntity(
        name = "PartyContactMechAndContactMech",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech and Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechAndContactMechView {}

    /**
     * Party Contact Mech And Postal Address
     */
    @ViewEntity(
        name = "PartyContactMechAndPostalAddress",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech And Postal Address",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechAndPostalAddressView {}

    /**
     * Party Contact Mech And Postal Address And Purpose
     */
    @ViewEntity(
        name = "PartyContactMechAndPostalAddressAndPurpose",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech And Postal Address And Purpose",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "PCMP", entityName = "PartyContactMechPurpose")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA"),
            @AliasAll(entityAlias = "PCMP", excludes = {"fromDate", "thruDate"})
        },
        aliases = {
            @Alias(name = "purposeFromDate", entityAlias = "PCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "PCMP", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCMP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechAndPostalAddressAndPurposeView {}

    /**
     * Party Contact Mech And TelecomNumber
     */
    @ViewEntity(
        name = "PartyContactMechAndTelecomNumber",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech And TelecomNumber",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "TN")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechAndTelecomNumberView {}

    /**
     * Party Contact Mech And TelecomNumber And Purpose
     */
    @ViewEntity(
        name = "PartyContactMechAndTelecomNumberAndPurpose",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech And TelecomNumber And Purpose",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber"),
            @MemberEntity(entityAlias = "PCMP", entityName = "PartyContactMechPurpose")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "TN"),
            @AliasAll(entityAlias = "PCMP", excludes = {"fromDate", "thruDate"})
        },
        aliases = {
            @Alias(name = "purposeFromDate", entityAlias = "PCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "PCMP", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCMP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechAndTelecomNumberAndPurposeView {}

    /**
     * Party Contact Mech Detail
     */
    @ViewEntity(
        name = "PartyContactMechDetail",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech Detail",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa"),
            @AliasAll(entityAlias = "TN", prefix = "tn")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechDetailView {}

    /**
     * Party Contact Mech Detail And Purpose
     */
    @ViewEntity(
        name = "PartyContactMechDetail",
        packageName = "org.ofbiz.party.contact",
        title = "Party Contact Mech Detail And Purpose",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa"),
            @AliasAll(entityAlias = "TN", prefix = "tn")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyContactMechDetail {}

    /**
     * Party and Contact Mech View
     */
    @ViewEntity(
        name = "PartyAndContactMech",
        packageName = "org.ofbiz.party.contact",
        title = "Party and Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa"),
            @AliasAll(entityAlias = "TN", prefix = "tn")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyAndContactMechView {}

    /**
     * Party and Contact Mech/Postal Address View
     */
    @ViewEntity(
        name = "PartyAndPostalAddress",
        packageName = "org.ofbiz.party.contact",
        title = "Party and Contact Mech/Postal Address View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PA")
        },
        aliases = {
            @Alias(name = "contactMechId", entityAlias = "CM"),
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "statusId", entityAlias = "PTY"),
            @Alias(name = "fromDate", entityAlias = "PCM"),
            @Alias(name = "thruDate", entityAlias = "PCM"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "contactMechTypeId", entityAlias = "CM"),
            @Alias(name = "infoString", entityAlias = "CM"),
            @Alias(name = "comments", entityAlias = "PCM"),
            @Alias(name = "extension", entityAlias = "PCM"),
            @Alias(name = "allowSolicitation", entityAlias = "PCM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
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
            )
        }
    )
    public interface PartyAndPostalAddressView {}

    /**
     * Party and TelecomNumber View
     */
    @ViewEntity(
        name = "PartyAndTelecomNumber",
        packageName = "org.ofbiz.party.contact",
        title = "Party and TelecomNumber View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "TN")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyAndTelecomNumberView {}

    @ViewEntity(
        name = "ContactListPartyAndContactMech",
        packageName = "org.ofbiz.party.contact",
        members = {
            @MemberEntity(entityAlias = "CLP", entityName = "ContactListParty"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CLP"),
            @AliasAll(entityAlias = "CM")
        },
        aliases = {
            @Alias(name = "contactFromDate", entityAlias = "PCM", field = "fromDate"),
            @Alias(name = "contactThruDate", entityAlias = "PCM", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CLP",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "preferredContactMechId", relFieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CLP",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "preferredContactMechId", relFieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactListParty",
                keyMaps = {
                    @KeyMap(fieldName = "contactListId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "fromDate")
                }
            )
        }
    )
    public interface ContactListPartyAndContactMechView {}

    /**
     * PartyAcctgPreference and PartyGroup
     */
    @ViewEntity(
        name = "PartyAcctgPrefAndGroup",
        packageName = "org.ofbiz.party.party",
        title = "PartyAcctgPreference and PartyGroup",
        members = {
            @MemberEntity(entityAlias = "PTYACCPREF", entityName = "PartyAcctgPreference"),
            @MemberEntity(entityAlias = "PTYGROUP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PTYROLE", entityName = "PartyRole")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTYACCPREF"),
            @Alias(name = "baseCurrencyUomId", entityAlias = "PTYACCPREF"),
            @Alias(name = "groupName", entityAlias = "PTYGROUP"),
            @Alias(name = "roleTypeId", entityAlias = "PTYROLE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTYACCPREF",
                relEntityAlias = "PTYGROUP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTYACCPREF",
                relEntityAlias = "PTYROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyAcctgPrefAndGroupView {}

    /**
     * Party and Party Group View
     */
    @ViewEntity(
        name = "PartyAndGroup",
        packageName = "org.ofbiz.party.party",
        title = "Party and Party Group View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PGRP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyAndGroupView {}

    /**
     * Party and Party Group And Party Role View
     */
    @ViewEntity(
        name = "PartyAndGroupAndRole",
        packageName = "org.ofbiz.party.party",
        title = "Party and Party Group And Party Role View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PRL", entityName = "PartyRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PGRP"),
            @AliasAll(entityAlias = "PRL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyAndGroupAndRoleView {}

    /**
     * Party and Person View
     */
    @ViewEntity(
        name = "PartyAndPerson",
        packageName = "org.ofbiz.party.party",
        title = "Party and Person View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PERS", entityName = "Person")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PERS")
        },
        aliases = {
            @Alias(name = "createdStamp", entityAlias = "PTY", field = "createdStamp")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PERS",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyAndPersonView {}

    /**
     * Party and Party Role View
     */
    @ViewEntity(
        name = "PartyAndRole",
        packageName = "org.ofbiz.party.party",
        title = "Party and Party Role View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PRL", entityName = "PartyRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PRL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyAndRoleView {}

    /**
     * Party and Contact Mech View
     */
    @ViewEntity(
        name = "PartyAndUserLogin",
        packageName = "org.ofbiz.party.party",
        title = "Party and Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "ULN", entityName = "UserLogin")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "userLoginId", entityAlias = "ULN"),
            @Alias(name = "currentPassword", entityAlias = "ULN"),
            @Alias(name = "passwordHint", entityAlias = "ULN"),
            @Alias(name = "enabled", entityAlias = "ULN"),
            @Alias(name = "disabledDateTime", entityAlias = "ULN"),
            @Alias(name = "successiveFailedLogins", entityAlias = "ULN")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "ULN",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
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
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface PartyAndUserLoginView {}

    /**
     * Parts of Party and UserLogin and Person
     */
    @ViewEntity(
        name = "PartyAndUserLoginAndPerson",
        packageName = "org.ofbiz.party.party",
        title = "Parts of Party and UserLogin and Person",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "ULN", entityName = "UserLogin"),
            @MemberEntity(entityAlias = "PER", entityName = "Person")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "createdDate", entityAlias = "PTY"),
            @Alias(name = "statusId", entityAlias = "PTY"),
            @Alias(name = "userLoginId", entityAlias = "ULN"),
            @Alias(name = "currentPassword", entityAlias = "ULN"),
            @Alias(name = "passwordHint", entityAlias = "ULN"),
            @Alias(name = "enabled", entityAlias = "ULN"),
            @Alias(name = "disabledDateTime", entityAlias = "ULN"),
            @Alias(name = "successiveFailedLogins", entityAlias = "ULN"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "firstName", entityAlias = "PER")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "ULN",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
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
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface PartyAndUserLoginAndPersonView {}

    /**
     * Parts of Party and UserLogin and Person Full
     */
    @ViewEntity(
        name = "PartyAndUserLoginAndPersonFull",
        packageName = "org.ofbiz.party.party",
        title = "Parts of Party and UserLogin and Person Full",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "ULN", entityName = "UserLogin"),
            @MemberEntity(entityAlias = "PER", entityName = "Person")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "ULN", excludes = {"partyId"}),
            @AliasAll(entityAlias = "PER", excludes = {"partyId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "ULN",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
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
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface PartyAndUserLoginAndPersonFullView {}

    /**
     * Parts of Party and UserLogin and Person And PartyRole Full
     */
    @ViewEntity(
        name = "PartyAndUserLoginAndPersonAndRoleFull",
        packageName = "org.ofbiz.party.party",
        title = "Parts of Party and UserLogin and Person And PartyRole Full",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "ULN", entityName = "UserLogin"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "ULN", excludes = {"partyId"}),
            @AliasAll(entityAlias = "PER", excludes = {"partyId"}),
            @AliasAll(entityAlias = "PR", excludes = {"partyId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "ULN",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
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
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface PartyAndUserLoginAndPersonAndRoleFullView {}

    /**
     * View to show the party assignment names for a workeffort with the partyStatus and AssignStatus
     */
    @ViewEntity(
        name = "PartyDetailAndWorkEffortAssign",
        packageName = "org.ofbiz.party.party",
        title = "View to show the party assignment names for a workeffort with the partyStatus and AssignStatus",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "WEF", entityName = "WorkEffortPartyAssignment"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PGR", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "partyStatusId", entityAlias = "PTY", field = "statusId"),
            @Alias(name = "workEffortTypeId", entityAlias = "WE"),
            @Alias(name = "workEffortId", entityAlias = "WEF"),
            @Alias(name = "fromDate", entityAlias = "WEF"),
            @Alias(name = "thruDate", entityAlias = "WEF"),
            @Alias(name = "roleTypeId", entityAlias = "WEF"),
            @Alias(name = "statusId", entityAlias = "WEF"),
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "groupName", entityAlias = "PGR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WEF",
                relEntityAlias = "PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "WEF",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PGR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyDetailAndWorkEffortAssignView {}

    /**
     * PartyIdentification and Party View
     */
    @ViewEntity(
        name = "PartyIdentificationAndParty",
        packageName = "org.ofbiz.party.party",
        title = "PartyIdentification and Party View",
        members = {
            @MemberEntity(entityAlias = "PIN", entityName = "PartyIdentification"),
            @MemberEntity(entityAlias = "PIT", entityName = "PartyIdentificationType"),
            @MemberEntity(entityAlias = "PA", entityName = "Party")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PIN"),
            @AliasAll(entityAlias = "PA")
        },
        aliases = {
            @Alias(name = "partyIdentTypeDesc", entityAlias = "PIT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PIN",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PIN",
                relEntityAlias = "PIT",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdentificationTypeId")
                }
            )
        }
    )
    public interface PartyIdentificationAndPartyView {}

    /**
     * Party and Geo Point View
     */
    @ViewEntity(
        name = "PartyAndGeoPoint",
        packageName = "org.ofbiz.party.party",
        title = "Party and Geo Point View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PGPT", entityName = "PartyGeoPoint"),
            @MemberEntity(entityAlias = "GPT", entityName = "GeoPoint")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GPT")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "fromDate", entityAlias = "PGPT"),
            @Alias(name = "thruDate", entityAlias = "PGPT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PGPT",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PGPT",
                relEntityAlias = "GPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyGeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "geoPointId")
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
                relEntityName = "GeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface PartyAndGeoPointView {}

    /**
     * UserLogin, Party, Person and PartyGroup
     */
    @ViewEntity(
        name = "UserLoginAndPartyDetails",
        packageName = "org.ofbiz.party.party",
        title = "UserLogin, Party, Person and PartyGroup",
        members = {
            @MemberEntity(entityAlias = "ULN", entityName = "UserLogin"),
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "createdDate", entityAlias = "PTY"),
            @Alias(name = "statusId", entityAlias = "PTY"),
            @Alias(name = "groupName", entityAlias = "PTYGRP"),
            @Alias(name = "userLoginId", entityAlias = "ULN"),
            @Alias(name = "currentPassword", entityAlias = "ULN"),
            @Alias(name = "passwordHint", entityAlias = "ULN"),
            @Alias(name = "enabled", entityAlias = "ULN"),
            @Alias(name = "disabledDateTime", entityAlias = "ULN"),
            @Alias(name = "successiveFailedLogins", entityAlias = "ULN"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "firstName", entityAlias = "PER")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ULN",
                relEntityAlias = "PTY",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "ULN",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "ULN",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
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
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface UserLoginAndPartyDetailsView {}

    /**
     * Party Contact Purpose View
     */
    @ViewEntity(
        name = "PartyContactWithPurpose",
        packageName = "org.ofbiz.party.party",
        title = "Party Contact Purpose View",
        members = {
            @MemberEntity(entityAlias = "PCMP", entityName = "PartyContactMechPurpose"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PT", entityName = "ContactMechPurposeType")
        },
        aliases = {
            @Alias(name = "contactMechId", entityAlias = "CM"),
            @Alias(name = "partyId", entityAlias = "PCM"),
            @Alias(name = "contactMechPurposeTypeId", entityAlias = "PCMP"),
            @Alias(name = "contactFromDate", entityAlias = "PCM", field = "fromDate"),
            @Alias(name = "contactThruDate", entityAlias = "PCM", field = "thruDate"),
            @Alias(name = "purposeFromDate", entityAlias = "PCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "PCMP", field = "thruDate"),
            @Alias(name = "contactMechTypeId", entityAlias = "CM"),
            @Alias(name = "infoString", entityAlias = "CM"),
            @Alias(name = "comments", entityAlias = "PCM"),
            @Alias(name = "extension", entityAlias = "PCM"),
            @Alias(name = "allowSolicitation", entityAlias = "PCM"),
            @Alias(name = "purposeDescription", entityAlias = "PT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCMP",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCMP",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCMP",
                relEntityAlias = "PT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMechPurposeType",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
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
                relEntityName = "ContactMech",
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
    public interface PartyContactWithPurposeView {}

    /**
     * Party Contact Mech And Purpose View
     */
    @ViewEntity(
        name = "PartyContactMechAndPurpose",
        packageName = "org.ofbiz.party.party",
        title = "Party Contact Mech And Purpose View",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PCMP", entityName = "PartyContactMechPurpose")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM", excludes = {"fromDate", "thruDate"}),
            @AliasAll(entityAlias = "PCMP", excludes = {"fromDate", "thruDate"})
        },
        aliases = {
            @Alias(name = "contactFromDate", entityAlias = "PCM", field = "fromDate"),
            @Alias(name = "contactThruDate", entityAlias = "PCM", field = "thruDate"),
            @Alias(name = "purposeFromDate", entityAlias = "PCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "PCMP", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCMP",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMechPurposeType",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
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
                relEntityName = "ContactMech",
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
    public interface PartyContactMechAndPurposeView {}

    /**
     * Party Content Detail View
     */
    @ViewEntity(
        name = "PartyContentDetail",
        packageName = "org.ofbiz.content.content",
        title = "Party Content Detail View",
        members = {
            @MemberEntity(entityAlias = "PCT", entityName = "PartyContent"),
            @MemberEntity(entityAlias = "CNT", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCT"),
            @AliasAll(entityAlias = "CNT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCT",
                relEntityAlias = "CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContentType",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            )
        }
    )
    public interface PartyContentDetailView {}

    /**
     * Party Name Contact Mech View
     */
    @ViewEntity(
        name = "PartyNameContactMechView",
        packageName = "org.ofbiz.party.party",
        title = "Party Name Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PTYPCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PTYCM", entityName = "ContactMech")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "statusId", entityAlias = "PTY"),
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "firstNameLocal", entityAlias = "PER"),
            @Alias(name = "lastNameLocal", entityAlias = "PER"),
            @Alias(name = "personalTitle", entityAlias = "PER"),
            @Alias(name = "suffix", entityAlias = "PER"),
            @Alias(name = "groupName", entityAlias = "PTYGRP"),
            @Alias(name = "groupNameLocal", entityAlias = "PTYGRP"),
            @Alias(name = "contactMechId", entityAlias = "PTYPCM"),
            @Alias(name = "fromDate", entityAlias = "PTYPCM"),
            @Alias(name = "thruDate", entityAlias = "PTYPCM"),
            @Alias(name = "contactMechTypeId", entityAlias = "PTYCM"),
            @Alias(name = "infoString", entityAlias = "PTYCM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYPCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTYPCM",
                relEntityAlias = "PTYCM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyNameContactMechViewView {}

    /**
     * Party Name View
     */
    @ViewEntity(
        name = "PartyNameView",
        packageName = "org.ofbiz.party.party",
        title = "Party Name View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "description", entityAlias = "PTY"),
            @Alias(name = "statusId", entityAlias = "PTY"),
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "firstNameLocal", entityAlias = "PER"),
            @Alias(name = "lastNameLocal", entityAlias = "PER"),
            @Alias(name = "personalTitle", entityAlias = "PER"),
            @Alias(name = "suffix", entityAlias = "PER"),
            @Alias(name = "groupName", entityAlias = "PTYGRP"),
            @Alias(name = "groupNameLocal", entityAlias = "PTYGRP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyNameViewView {}

    /**
     * Party Note View
     */
    @ViewEntity(
        name = "PartyNoteView",
        packageName = "org.ofbiz.party.party",
        title = "Party Note View",
        members = {
            @MemberEntity(entityAlias = "PN", entityName = "PartyNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "targetPartyId", entityAlias = "PN", field = "partyId"),
            @Alias(name = "noteId", entityAlias = "ND"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND"),
            @Alias(name = "noteParty", entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PN",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface PartyNoteViewView {}

    /**
     *              Links two PartyRelationships together to be used to get a member of a group that is a member of a group, so links the to party of the first to the from party of the second.             To query the group ID would go on onePartyIdFrom and the member ID would go in twoPartyIdTo.         
     */
    @ViewEntity(
        name = "PartyRelationshipToFrom",
        packageName = "org.ofbiz.party.party",
        description = "\n            Links two PartyRelationships together to be used to get a member of a group that is a member of a group, so links the to party of the first to the from party of the second.\n            To query the group ID would go on onePartyIdFrom and the member ID would go in twoPartyIdTo.\n        ",
        members = {
            @MemberEntity(entityAlias = "PR1", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "PR2", entityName = "PartyRelationship")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PR1", prefix = "one"),
            @AliasAll(entityAlias = "PR2", prefix = "two")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PR1",
                relEntityAlias = "PR2",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyIdFrom")
                }
            )
        }
    )
    public interface PartyRelationshipToFromView {}

    /**
     * Party Relationship And Details
     */
    @ViewEntity(
        name = "PartyRelationshipAndDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Relationship And Details",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PTYREL", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTYREL")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PTY"),
            @Alias(name = "partyTypeId", entityAlias = "PTY"),
            @Alias(name = "description", entityAlias = "PTY"),
            @Alias(name = "partyStatusId", entityAlias = "PTY", field = "statusId"),
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "firstNameLocal", entityAlias = "PER"),
            @Alias(name = "lastNameLocal", entityAlias = "PER"),
            @Alias(name = "personalTitle", entityAlias = "PER"),
            @Alias(name = "suffix", entityAlias = "PER"),
            @Alias(name = "groupName", entityAlias = "PTYGRP"),
            @Alias(name = "groupNameLocal", entityAlias = "PTYGRP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYREL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdTo")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyRelationshipAndDetailView {}

    /**
     * Party Relationship And Contact Mech Details
     */
    @ViewEntity(
        name = "PartyRelationshipAndContactMechDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Relationship And Contact Mech Details",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PTYREL", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY", excludes = {"statusId"}),
            @AliasAll(entityAlias = "PTYREL"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa"),
            @AliasAll(entityAlias = "TN", prefix = "tn")
        },
        aliases = {
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "firstNameLocal", entityAlias = "PER"),
            @Alias(name = "lastNameLocal", entityAlias = "PER"),
            @Alias(name = "personalTitle", entityAlias = "PER"),
            @Alias(name = "suffix", entityAlias = "PER"),
            @Alias(name = "groupName", entityAlias = "PTYGRP"),
            @Alias(name = "groupNameLocal", entityAlias = "PTYGRP"),
            @Alias(name = "contactMechId", entityAlias = "PCM"),
            @Alias(name = "partyStatusId", entityAlias = "PTY", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYREL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdTo")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRelationship",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom"),
                    @KeyMap(fieldName = "partyIdTo"),
                    @KeyMap(fieldName = "roleTypeIdFrom"),
                    @KeyMap(fieldName = "roleTypeIdTo"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRelationshipType",
                keyMaps = {
                    @KeyMap(fieldName = "partyRelationshipTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId"),
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyRelationshipAndContactMechDetailView {}

    /**
     * Party Relationship And Party Details
     */
    @ViewEntity(
        name = "PartyRelationshipAndPartyDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Relationship And Party Details",
        members = {
            @MemberEntity(entityAlias = "TO_PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PTYREL", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "PTYRELTP", entityName = "PartyRelationshipType"),
            @MemberEntity(entityAlias = "TO_PER", entityName = "Person"),
            @MemberEntity(entityAlias = "TO_PTYGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "FROM_PER", entityName = "Person"),
            @MemberEntity(entityAlias = "FROM_PTYGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTYREL")
        },
        aliases = {
            @Alias(name = "relParentTypeId", entityAlias = "PTYRELTP", field = "parentTypeId"),
            @Alias(name = "partyId", entityAlias = "TO_PTY"),
            @Alias(name = "partyTypeId", entityAlias = "TO_PTY"),
            @Alias(name = "description", entityAlias = "TO_PTY"),
            @Alias(name = "partyStatusId", entityAlias = "TO_PTY", field = "statusId"),
            @Alias(name = "toFirstName", entityAlias = "TO_PER", field = "firstName"),
            @Alias(name = "toMiddleName", entityAlias = "TO_PER", field = "middleName"),
            @Alias(name = "toLastName", entityAlias = "TO_PER", field = "lastName"),
            @Alias(name = "tofirstNameLocal", entityAlias = "TO_PER", field = "firstNameLocal"),
            @Alias(name = "toLastNameLocal", entityAlias = "TO_PER", field = "lastNameLocal"),
            @Alias(name = "toPersonalTitle", entityAlias = "TO_PER", field = "personalTitle"),
            @Alias(name = "toSuffix", entityAlias = "TO_PER", field = "suffix"),
            @Alias(name = "toGroupName", entityAlias = "TO_PTYGRP", field = "groupName"),
            @Alias(name = "toGroupNameLocal", entityAlias = "TO_PTYGRP", field = "groupNameLocal"),
            @Alias(name = "fromFirstName", entityAlias = "FROM_PER", field = "firstName"),
            @Alias(name = "fromMiddleName", entityAlias = "FROM_PER", field = "middleName"),
            @Alias(name = "fromLastName", entityAlias = "FROM_PER", field = "lastName"),
            @Alias(name = "fromfirstNameLocal", entityAlias = "FROM_PER", field = "firstNameLocal"),
            @Alias(name = "fromLastNameLocal", entityAlias = "FROM_PER", field = "lastNameLocal"),
            @Alias(name = "fromPersonalTitle", entityAlias = "FROM_PER", field = "personalTitle"),
            @Alias(name = "fromSuffix", entityAlias = "FROM_PER", field = "suffix"),
            @Alias(name = "fromGroupName", entityAlias = "FROM_PTYGRP", field = "groupName"),
            @Alias(name = "fromGroupNameLocal", entityAlias = "FROM_PTYGRP", field = "groupNameLocal")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TO_PTY",
                relEntityAlias = "PTYREL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdTo")
                }
            ),
            @ViewLink(
                entityAlias = "TO_PTY",
                relEntityAlias = "TO_PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "TO_PTY",
                relEntityAlias = "TO_PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTYREL",
                relEntityAlias = "FROM_PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTYREL",
                relEntityAlias = "PTYRELTP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyRelationshipTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "PTYREL",
                relEntityAlias = "FROM_PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyRelationshipAndPartyDetailView {}

    /**
     * Party Role and Party Detail (Person, PartyGroup, etc.) View
     */
    @ViewEntity(
        name = "PartyRoleAndPartyDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Role and Party Detail (Person, PartyGroup, etc.) View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "PERSON", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PR"),
            @AliasAll(entityAlias = "PERSON", excludes = {"comments"}),
            @AliasAll(entityAlias = "PTYGRP", excludes = {"comments"})
        },
        aliases = {
            @Alias(name = "personComments", entityAlias = "PERSON", field = "comments"),
            @Alias(name = "partyGroupComments", entityAlias = "PTYGRP", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PERSON",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyRoleAndPartyDetailView {}

    /**
     * Party Role and Party Detail (Person, PartyGroup, etc.) View
     */
    @ViewEntity(
        name = "PartyRoleDetailAndPartyDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Role and Party Detail (Person, PartyGroup, etc.) View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "RT", entityName = "RoleType"),
            @MemberEntity(entityAlias = "PERSON", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY", excludes = {"description"}),
            @AliasAll(entityAlias = "PR"),
            @AliasAll(entityAlias = "RT"),
            @AliasAll(entityAlias = "PERSON", excludes = {"comments"}),
            @AliasAll(entityAlias = "PTYGRP", excludes = {"comments"})
        },
        aliases = {
            @Alias(name = "personComments", entityAlias = "PERSON", field = "comments"),
            @Alias(name = "partyGroupComments", entityAlias = "PTYGRP", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PERSON",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PR",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface PartyRoleDetailAndPartyDetailView {}

    /**
     * Party Role and Party Detail (Person, PartyGroup, etc.) View
     */
    @ViewEntity(
        name = "PartyRoleNameDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Role and Party Detail (Person, PartyGroup, etc.) View",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "PERSON", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY"),
            @AliasAll(entityAlias = "PR"),
            @AliasAll(entityAlias = "PERSON", excludes = {"comments"}),
            @AliasAll(entityAlias = "PTYGRP", excludes = {"comments"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PERSON",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyRoleNameDetailView {}

    /**
     * Party Role And Contact Mech Details
     */
    @ViewEntity(
        name = "PartyRoleAndContactMechDetail",
        packageName = "org.ofbiz.party.party",
        title = "Party Role And Contact Mech Details",
        members = {
            @MemberEntity(entityAlias = "PTY", entityName = "Party"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "RT", entityName = "RoleType"),
            @MemberEntity(entityAlias = "PERSON", entityName = "Person"),
            @MemberEntity(entityAlias = "PTYGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PTY", excludes = {"description"}),
            @AliasAll(entityAlias = "PR"),
            @AliasAll(entityAlias = "RT"),
            @AliasAll(entityAlias = "PERSON", excludes = {"comments"}),
            @AliasAll(entityAlias = "PTYGRP", excludes = {"comments"}),
            @AliasAll(entityAlias = "PCM", excludes = {"roleTypeId"}),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "PA", prefix = "pa"),
            @AliasAll(entityAlias = "TN", prefix = "tn")
        },
        aliases = {
            @Alias(name = "personComments", entityAlias = "PERSON", field = "comments"),
            @Alias(name = "partyGroupComments", entityAlias = "PTYGRP", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PERSON",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PTYGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PR",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "PTY",
                relEntityAlias = "PCM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
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
                relEntityName = "PartyContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "contactMechId"),
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
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
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
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface PartyRoleAndContactMechDetailView {}

    /**
     * Party Role View in 4 levels
     */
    @ViewEntity(
        name = "RoleTypeIn3Levels",
        packageName = "org.ofbiz.party.party",
        title = "Party Role View in 4 levels",
        members = {
            @MemberEntity(entityAlias = "RT1", entityName = "RoleType"),
            @MemberEntity(entityAlias = "RT2", entityName = "RoleType"),
            @MemberEntity(entityAlias = "RT3", entityName = "RoleType")
        },
        aliases = {
            @Alias(name = "topRoleTypeId", entityAlias = "RT1", field = "roleTypeId"),
            @Alias(name = "topDescription", entityAlias = "RT1", field = "description"),
            @Alias(name = "midRoleTypeId", entityAlias = "RT2", field = "roleTypeId"),
            @Alias(name = "midDescription", entityAlias = "RT2", field = "description"),
            @Alias(name = "lowRoleTypeId", entityAlias = "RT3", field = "roleTypeId"),
            @Alias(name = "lowDescription", entityAlias = "RT3", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RT1",
                relEntityAlias = "RT2",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId", relFieldName = "parentTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "RT2",
                relEntityAlias = "RT3",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId", relFieldName = "parentTypeId")
                }
            )
        }
    )
    public interface RoleTypeIn3LevelsView {}

    /**
     * Party Role View
     */
    @ViewEntity(
        name = "RoleTypeAndParty",
        packageName = "org.ofbiz.party.party",
        title = "Party Role View",
        members = {
            @MemberEntity(entityAlias = "PR", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "RT", entityName = "RoleType")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PR"),
            @Alias(name = "roleTypeId", entityAlias = "RT"),
            @Alias(name = "parentTypeId", entityAlias = "RT"),
            @Alias(name = "description", entityAlias = "RT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RT",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface RoleTypeAndPartyView {}

    @ViewEntity(
        name = "PartyExport",
        packageName = "org.ofbiz.accounting.reports",
        members = {
            @MemberEntity(entityAlias = "PRT", entityName = "Party"),
            @MemberEntity(entityAlias = "GRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PER", entityName = "Person"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRelationship"),
            @MemberEntity(entityAlias = "CGRP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "PRL", entityName = "PartyRole"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech"),
            @MemberEntity(entityAlias = "PCP", entityName = "PartyContactMechPurpose"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "TN", entityName = "TelecomNumber"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "PRT"),
            @Alias(name = "statusId", entityAlias = "PRT"),
            @Alias(name = "preferredCurrencyUomId", entityAlias = "PRT"),
            @Alias(name = "groupName", entityAlias = "GRP"),
            @Alias(name = "firstName", entityAlias = "PER"),
            @Alias(name = "middleName", entityAlias = "PER"),
            @Alias(name = "lastName", entityAlias = "PER"),
            @Alias(name = "companyPartyId", entityAlias = "PR", field = "partyIdFrom"),
            @Alias(name = "companyName", entityAlias = "CGRP", field = "groupName"),
            @Alias(name = "roleTypeId", entityAlias = "PRL"),
            @Alias(name = "contactMechTypeId", entityAlias = "CM"),
            @Alias(name = "contactMechPurposeTypeId", entityAlias = "PCP"),
            @Alias(name = "emailAddress", entityAlias = "CM", field = "infoString"),
            @Alias(name = "telCountryCode", entityAlias = "TN", field = "countryCode"),
            @Alias(name = "telAreaCode", entityAlias = "TN", field = "areaCode"),
            @Alias(name = "telContactNumber", entityAlias = "TN", field = "contactNumber"),
            @Alias(name = "address1", entityAlias = "PA"),
            @Alias(name = "address2", entityAlias = "PA"),
            @Alias(name = "city", entityAlias = "PA"),
            @Alias(name = "stateProvinceGeoId", entityAlias = "PA"),
            @Alias(name = "postalCode", entityAlias = "PA"),
            @Alias(name = "countryGeoId", entityAlias = "PA"),
            @Alias(name = "fromDate", entityAlias = "PCM"),
            @Alias(name = "thruDate", entityAlias = "PCM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PRT",
                relEntityAlias = "GRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PRT",
                relEntityAlias = "PER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PRT",
                relEntityAlias = "PR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdTo")
                }
            ),
            @ViewLink(
                entityAlias = "PR",
                relEntityAlias = "CGRP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PRT",
                relEntityAlias = "PRL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PRT",
                relEntityAlias = "PCM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "CM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "TN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId"),
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PartyExportView {}

    /**
     * PartyClassificationGroup and PartyClassificationTypeView
     */
    @ViewEntity(
        name = "PartyClassificationGroupAndType",
        packageName = "org.ofbiz.party.party",
        title = "PartyClassificationGroup and PartyClassificationTypeView",
        members = {
            @MemberEntity(entityAlias = "PCG", entityName = "PartyClassificationGroup"),
            @MemberEntity(entityAlias = "PCT", entityName = "PartyClassificationType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCG")
        },
        aliases = {
            @Alias(name = "typeDescription", entityAlias = "PCT", field = "description"),
            @Alias(name = "parentTypeId", entityAlias = "PCT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCG",
                relEntityAlias = "PCT",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationTypeId")
                }
            )
        }
    )
    public interface PartyClassificationGroupAndTypeView {}

    @ExtendEntity(
        name = "CustomTimePeriod",
        fields = {
            @Field(name = "organizationPartyId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Organization",
                fkName = "ORG_PRD_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface CustomTimePeriodExtension {}

    @ExtendEntity(
        name = "NoteData",
        fields = {
            @Field(name = "noteParty", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Note",
                fkName = "NOTE_DATA_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "noteParty", relFieldName = "partyId")
                }
            )
        }
    )
    public interface NoteDataExtension {}

    @ExtendEntity(
        name = "ServerHit",
        fields = {
            @Field(name = "internalContentId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "idByIpContactMechId", type = "id"),
            @Field(name = "refByWebContactMechId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SERVER_HIT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "IdByIp",
                fkName = "SERVER_HIT_IDBYIP",
                keyMaps = {
                    @KeyMap(fieldName = "idByIpContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "RefByWeb",
                fkName = "SERVER_HIT_REFWEB",
                keyMaps = {
                    @KeyMap(fieldName = "refByWebContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "SERVER_HIT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "internalContentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ServerHitExtension {}

    @ExtendEntity(
        name = "ServerHitBin",
        fields = {
            @Field(name = "internalContentId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "SERVER_HBIN_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "internalContentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ServerHitBinExtension {}

    @ExtendEntity(
        name = "Visit",
        fields = {
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id")
        }
    )
    public interface VisitExtension {}

    @ExtendEntity(
        name = "Visitor",
        fields = {
            @Field(name = "partyId", type = "id")
        }
    )
    public interface VisitorExtension {}

    @ExtendEntity(
        name = "UserLogin",
        fields = {
            @Field(name = "partyId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "USER_PARTY",
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
                relEntityName = "PartyGroup",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface UserLoginExtension {}

    @ExtendEntity(
        name = "UserLoginHistory",
        fields = {
            @Field(name = "partyId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "USER_LH_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface UserLoginHistoryExtension {}

}
