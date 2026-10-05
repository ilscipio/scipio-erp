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
package com.ilscipio.scipio.product.entity;

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
     * Catalog
     */
    @Entity(
        name = "ProdCatalog",
        packageName = "org.ofbiz.product.catalog",
        title = "Catalog",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "catalogName", type = "name"),
            @Field(name = "useQuickAdd", type = "indicator"),
            @Field(name = "styleSheet", type = "url"),
            @Field(name = "headerLogo", type = "url"),
            @Field(name = "contentPathPrefix", type = "long-varchar"),
            @Field(name = "templatePathPrefix", type = "long-varchar"),
            @Field(name = "viewAllowPermReqd", type = "indicator"),
            @Field(name = "purchaseAllowPermReqd", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "prodCatalogId")
        }
    )
    public interface ProdCatalogEntity {}

    /**
     * Catalog Category Association
     */
    @Entity(
        name = "ProdCatalogCategory",
        packageName = "org.ofbiz.product.catalog",
        title = "Catalog Category Association",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "prodCatalogCategoryTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "prodCatalogId"),
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "prodCatalogCategoryTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalog",
                fkName = "PROD_CC_CATALOG",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_CC_CATEGORY",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalogCategoryType",
                fkName = "PROD_CC_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogCategoryTypeId")
                }
            )
        }
    )
    public interface ProdCatalogCategoryEntity {}

    /**
     * Catalog Category Association Type
     */
    @Entity(
        name = "ProdCatalogCategoryType",
        packageName = "org.ofbiz.product.catalog",
        title = "Catalog Category Association Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "prodCatalogCategoryTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "prodCatalogCategoryTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalogCategoryType",
                title = "Parent",
                fkName = "PROD_PCCT_TYPEPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "prodCatalogCategoryTypeId")
                }
            )
        }
    )
    public interface ProdCatalogCategoryTypeEntity {}

    /**
     * Product Catalog Inventory Facility Applicability
     */
    @Entity(
        name = "ProdCatalogInvFacility",
        packageName = "org.ofbiz.product.catalog",
        title = "Product Catalog Inventory Facility Applicability",
        fields = {
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "prodCatalogId"),
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalog",
                fkName = "PROD_CIF_CATALOG",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PROD_CIF_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ProdCatalogInvFacilityEntity {}

    /**
     * ProdCatalog Role Association
     */
    @Entity(
        name = "ProdCatalogRole",
        packageName = "org.ofbiz.product.catalog",
        title = "ProdCatalog Role Association",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "prodCatalogId"),
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
                fkName = "PCATRLE_PTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalog",
                fkName = "PCATRLE_CATALOG",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogId")
                }
            )
        }
    )
    public interface ProdCatalogRoleEntity {}

    /**
     * Product Category
     */
    @Entity(
        name = "ProductCategory",
        packageName = "org.ofbiz.product.category",
        title = "Product Category",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "productCategoryTypeId", type = "id"),
            @Field(name = "primaryParentCategoryId", type = "id"),
            @Field(name = "categoryName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "longDescription", type = "very-long"),
            @Field(name = "categoryImageUrl", type = "url"),
            @Field(name = "linkOneImageUrl", type = "url"),
            @Field(name = "linkTwoImageUrl", type = "url"),
            @Field(name = "smallImageUrl", type = "url"),
            @Field(name = "mediumImageUrl", type = "url"),
            @Field(name = "largeImageUrl", type = "url"),
            @Field(name = "detailImageUrl", type = "url"),
            @Field(name = "originalImageUrl", type = "url"),
            @Field(name = "detailScreen", type = "long-varchar"),
            @Field(name = "showInSelect", type = "indicator"),
            @Field(name = "mediaImageId", type = "id", description = "Content.contentId value of a CMS media image, for manual use (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategoryType",
                fkName = "PROD_CTGRY_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                title = "PrimaryParent",
                fkName = "PROD_CTGRY_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "primaryParentCategoryId", relFieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategory",
                title = "PrimaryChild",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId", relFieldName = "primaryParentCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryEntity {}

    /**
     * Inline Category Media Details
     */
    @Entity(
        name = "CategoryMediaDetails",
        packageName = "org.ofbiz.product.product",
        title = "Inline Category Media Details",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "smallImageWidth", type = "numeric"),
            @Field(name = "smallImageHeight", type = "numeric"),
            @Field(name = "smallImageMimeTypeId", type = "id-vlong"),
            @Field(name = "smallImagePresetJson", type = "very-long"),
            @Field(name = "mediumImageWidth", type = "numeric"),
            @Field(name = "mediumImageHeight", type = "numeric"),
            @Field(name = "mediumImageMimeTypeId", type = "id-vlong"),
            @Field(name = "mediumImagePresetJson", type = "very-long"),
            @Field(name = "largeImageWidth", type = "numeric"),
            @Field(name = "largeImageHeight", type = "numeric"),
            @Field(name = "largeImageMimeTypeId", type = "id-vlong"),
            @Field(name = "largeImagePresetJson", type = "very-long"),
            @Field(name = "detailImageWidth", type = "numeric"),
            @Field(name = "detailImageHeight", type = "numeric"),
            @Field(name = "detailImageMimeTypeId", type = "id-vlong"),
            @Field(name = "detailImagePresetJson", type = "very-long"),
            @Field(name = "originalImageWidth", type = "numeric"),
            @Field(name = "originalImageHeight", type = "numeric"),
            @Field(name = "originalImageMimeTypeId", type = "id-vlong"),
            @Field(name = "originalImagePresetJson", type = "very-long"),
            @Field(name = "originalImageFileName", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "CATEGMEDDET_CATID",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface CategoryMediaDetailsEntity {}

    /**
     * Product Category Attribute
     */
    @Entity(
        name = "ProductCategoryAttribute",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Attribute",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_CTGRY_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface ProductCategoryAttributeEntity {}

    /**
     * Product Category Data Object
     */
    @Entity(
        name = "ProductCategoryContent",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Data Object",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "prodCatContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "purchaseFromDate", type = "date-time"),
            @Field(name = "purchaseThruDate", type = "date-time"),
            @Field(name = "useCountLimit", type = "numeric"),
            @Field(name = "useDaysLimit", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "prodCatContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PRDCAT_CNT_PRDCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PRDCAT_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategoryContentType",
                fkName = "PRDCAT_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatContentTypeId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRDCAT_CNT_CTTP",
                fields = {
                    @IndexField(name = "productCategoryId"),
                    @IndexField(name = "prodCatContentTypeId")
                }
            )
        }
    )
    public interface ProductCategoryContentEntity {}

    /**
     * Product Category Content Type
     */
    @Entity(
        name = "ProductCategoryContentType",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Content Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "prodCatContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "viewType", type = "name", description = "For images: main, additional or a custom type (SCIPIO)"),
            @Field(name = "viewNumber", type = "value", description = "For images: 0 for main, 1-4 or greater for additional images (SCIPIO)"),
            @Field(name = "viewSize", type = "name", description = "For images: original, detail, large, 320x240, etc., also known as sizeType (SCIPIO)"),
            @Field(name = "viewVariantId", type = "name", description = "For images: flexible expression pattern for generating variant productCategoryContentTypeId of an original image URL (SCIPIO)"),
            @Field(name = "viewVariantDesc", type = "name", description = "For images: flexible expression pattern for generating variant description of an original image URL (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "prodCatContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategoryContentType",
                title = "Parent",
                fkName = "PRDCATCNT_TYP_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "prodCatContentTypeId")
                }
            )
        }
    )
    public interface ProductCategoryContentTypeEntity {}

    /**
     * Product Category GlAccount
     */
    @Entity(
        name = "ProductCategoryGlAccount",
        packageName = "org.ofbiz.product.category",
        title = "Product Category GlAccount",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountTypeId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "organizationPartyId"),
            @PrimaryKey(field = "glAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PRD_CT_GLACT_PCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PRD_CT_GLACT_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "PRD_CT_GLACT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PRD_CT_GLACT_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface ProductCategoryGlAccountEntity {}

    /**
     * Product Category Link
     */
    @Entity(
        name = "ProductCategoryLink",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Link",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "linkSeqId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment", description = "Internal comments, not for public display."),
            @Field(name = "sequenceNum", type = "numeric", description = "This field is used to sort the links. The linkSeqId field is not used because it is part of the primary key and cannot be changed."),
            @Field(name = "titleText", type = "description"),
            @Field(name = "detailText", type = "very-long"),
            @Field(name = "imageUrl", type = "url"),
            @Field(name = "imageTwoUrl", type = "url"),
            @Field(name = "linkTypeEnumId", type = "id"),
            @Field(name = "linkInfo", type = "long-varchar"),
            @Field(name = "detailSubScreen", type = "long-varchar", description = "This is optional. If not specified a default should be used by the category detail template.")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "linkSeqId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_CLNK_CATEGORY",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "LinkType",
                fkName = "PROD_CLNK_LKTPENM",
                keyMaps = {
                    @KeyMap(fieldName = "linkTypeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductCategoryLinkEntity {}

    /**
     * Product Category Member
     */
    @Entity(
        name = "ProductCategoryMember",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Member",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "sortPriority", type = "fixed-point", description = "Search priority; between 0-1.0 is less than default, higher prioritizes (opposite sequenceNum)")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_CMBR_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_CMBR_CATEGORY",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRD_CMBR_PCT",
                fields = {
                    @IndexField(name = "productCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryMemberEntity {}

    /**
     * Product Category Role
     */
    @Entity(
        name = "ProductCategoryRole",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Role",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
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
                fkName = "PROD_CRLE_PTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_CRLE_CATEGORY",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryRoleEntity {}

    /**
     * Product Category Rollup
     */
    @Entity(
        name = "ProductCategoryRollup",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Rollup",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "parentProductCategoryId", type = "id-ne", description = "The parent category; it should be one of productCategoryId already setup in ProductCategory or ProductCategoryRollup"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "parentProductCategoryId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                title = "Current",
                fkName = "PROD_CRLP_CURRENT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                title = "Parent",
                fkName = "PROD_CRLP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId", relFieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryRollup",
                title = "Child",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId", relFieldName = "parentProductCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryRollup",
                title = "Parent",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId", relFieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryRollup",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRDCR_PARPC",
                fields = {
                    @IndexField(name = "parentProductCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryRollupEntity {}

    /**
     * Product Category Type
     */
    @Entity(
        name = "ProductCategoryType",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productCategoryTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategoryType",
                title = "Parent",
                fkName = "PROD_CTGRY_TYPEPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productCategoryTypeId")
                }
            )
        }
    )
    public interface ProductCategoryTypeEntity {}

    /**
     * Product Category Type Attribute
     */
    @Entity(
        name = "ProductCategoryTypeAttr",
        packageName = "org.ofbiz.product.category",
        title = "Product Category Type Attribute",
        fields = {
            @Field(name = "productCategoryTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategoryType",
                fkName = "PROD_CTGRY_TATTR",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategoryAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryTypeId")
                }
            )
        }
    )
    public interface ProductCategoryTypeAttrEntity {}

    /**
     * Product Configuration Templates
     */
    @Entity(
        name = "ProductConfig",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Templates",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "description", type = "description"),
            @Field(name = "longDescription", type = "very-long"),
            @Field(name = "configTypeId", type = "id"),
            @Field(name = "defaultConfigOptionId", type = "id"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "isMandatory", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "sequenceNum"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Product",
                fkName = "PROD_CONF_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItem",
                fkName = "PROD_CONF_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            )
        }
    )
    public interface ProductConfigEntity {}

    /**
     * Product Configuration Question
     */
    @Entity(
        name = "ProductConfigItem",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Question",
        fields = {
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "configItemTypeId", type = "id"),
            @Field(name = "configItemName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "longDescription", type = "very-long"),
            @Field(name = "imageUrl", type = "url")
        },
        primaryKeys = {
            @PrimaryKey(field = "configItemId")
        }
    )
    public interface ProductConfigItemEntity {}

    /**
     * Product Configuration Question Data Object
     */
    @Entity(
        name = "ProdConfItemContent",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Question Data Object",
        fields = {
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "confItemContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "confItemContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                fkName = "CIMT_CNT_PCIT",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CIMT_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdConfItemContentType",
                fkName = "CIMT_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "confItemContentTypeId")
                }
            )
        }
    )
    public interface ProdConfItemContentEntity {}

    /**
     * Product Content Type
     */
    @Entity(
        name = "ProdConfItemContentType",
        packageName = "org.ofbiz.product.config",
        title = "Product Content Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "confItemContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "confItemContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdConfItemContentType",
                title = "Parent",
                fkName = "PCICT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "confItemContentTypeId")
                }
            )
        }
    )
    public interface ProdConfItemContentTypeEntity {}

    /**
     * Product Configuration Options
     */
    @Entity(
        name = "ProductConfigOption",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Options",
        fields = {
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "configOptionId", type = "id-ne"),
            @Field(name = "configOptionName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "configOptionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItem",
                fkName = "PROD_OPTN_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            )
        }
    )
    public interface ProductConfigOptionEntity {}

    /**
     * Product Configuration Options
     */
    @Entity(
        name = "ProductConfigOptionIactn",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Options",
        fields = {
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "configOptionId", type = "id-ne"),
            @Field(name = "configItemIdTo", type = "id-ne"),
            @Field(name = "configOptionIdTo", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "configIactnTypeId", type = "id", description = "INCOMPATIBLE, etc..."),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "configOptionId"),
            @PrimaryKey(field = "configItemIdTo"),
            @PrimaryKey(field = "configOptionIdTo"),
            @PrimaryKey(field = "sequenceNum")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItem",
                fkName = "PROD_OPTIA_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigOption",
                title = "ConfigOption",
                fkName = "PROD_OPTIA_OPTN",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItemTo",
                fkName = "PROD_OPTIA_ITMT",
                keyMaps = {
                    @KeyMap(fieldName = "configItemIdTo", relFieldName = "configItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigOption",
                title = "ConfigOptionTo",
                fkName = "PROD_OPTIA_OPTT",
                keyMaps = {
                    @KeyMap(fieldName = "configItemIdTo", relFieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionIdTo", relFieldName = "configOptionId")
                }
            )
        }
    )
    public interface ProductConfigOptionIactnEntity {}

    /**
     * Product Configuration Option to Products
     */
    @Entity(
        name = "ProductConfigProduct",
        packageName = "org.ofbiz.product.config",
        title = "Product Configuration Option to Products",
        fields = {
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "configOptionId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "configOptionId"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItem",
                fkName = "PROD_CONFP_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigOption",
                title = "ConfigOption",
                fkName = "PROD_CONFP_OPTN",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Product",
                fkName = "PROD_CONFP_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductConfigProductEntity {}

    /**
     * Existing Product Configurations
     */
    @Entity(
        name = "ProductConfigConfig",
        packageName = "org.ofbiz.product.config",
        title = "Existing Product Configurations",
        fields = {
            @Field(name = "configId", type = "id-ne"),
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "configOptionId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "configId"),
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "configOptionId"),
            @PrimaryKey(field = "sequenceNum")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigItem",
                title = "ConfigItem",
                fkName = "PROD_CONFC_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigOption",
                title = "ConfigOption",
                fkName = "PROD_CONFC_OPTN",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionId")
                }
            )
        }
    )
    public interface ProductConfigConfigEntity {}

    /**
     * Product Configurations Stats
     */
    @Entity(
        name = "ProductConfigStats",
        packageName = "org.ofbiz.product.config",
        title = "Product Configurations Stats",
        fields = {
            @Field(name = "configId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "numOfConfs", type = "numeric"),
            @Field(name = "configTypeId", type = "id", description = "HIDDEN, TEMPLATE, etc...")
        },
        primaryKeys = {
            @PrimaryKey(field = "configId"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Product",
                fkName = "PROD_CONFS_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductConfigStatsEntity {}

    /**
     * Config Option Product Options
     */
    @Entity(
        name = "ConfigOptionProductOption",
        packageName = "org.ofbiz.product.config",
        title = "Config Option Product Options",
        fields = {
            @Field(name = "configId", type = "id-ne"),
            @Field(name = "configItemId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "configOptionId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productOptionId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "configId"),
            @PrimaryKey(field = "configItemId"),
            @PrimaryKey(field = "configOptionId"),
            @PrimaryKey(field = "sequenceNum"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigConfig",
                title = "Config",
                fkName = "PROD_OPTN_CONF",
                keyMaps = {
                    @KeyMap(fieldName = "configId"),
                    @KeyMap(fieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionId"),
                    @KeyMap(fieldName = "sequenceNum")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductConfigProduct",
                title = "Product",
                fkName = "PROD_OPTN_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId"),
                    @KeyMap(fieldName = "configOptionId"),
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ConfigOptionProductOptionEntity {}

    /**
     * Cost Component
     */
    @Entity(
        name = "CostComponent",
        packageName = "org.ofbiz.product.cost",
        title = "Cost Component",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "costComponentId", type = "id-ne"),
            @Field(name = "costComponentTypeId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "geoId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "costComponentCalcId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "cost", type = "fixed-point", description = "Higher precision in case it is a calculated number"),
            @Field(name = "costUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "costComponentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentType",
                fkName = "COST_COMP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CostComponentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "COST_COMP_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "COST_COMP_PRODFEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "COST_COMP_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "COST_COMP_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "COST_COMP_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "COST_COMP_FXADSST",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentCalc",
                fkName = "COST_COMP_CALC",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentCalcId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "COST_COMP_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "costUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface CostComponentEntity {}

    /**
     * Cost Component Attribute
     */
    @Entity(
        name = "CostComponentAttribute",
        packageName = "org.ofbiz.product.cost",
        title = "Cost Component Attribute",
        fields = {
            @Field(name = "costComponentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "costComponentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponent",
                fkName = "COST_COMP_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CostComponentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface CostComponentAttributeEntity {}

    /**
     * Cost Component Type
     */
    @Entity(
        name = "CostComponentType",
        packageName = "org.ofbiz.product.cost",
        title = "Cost Component Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "costComponentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "costComponentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentType",
                title = "Parent",
                fkName = "COST_COMP_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "costComponentTypeId")
                }
            )
        }
    )
    public interface CostComponentTypeEntity {}

    /**
     * Cost Component Type Attribute
     */
    @Entity(
        name = "CostComponentTypeAttr",
        packageName = "org.ofbiz.product.cost",
        title = "Cost Component Type Attribute",
        fields = {
            @Field(name = "costComponentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "costComponentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentType",
                fkName = "COST_COMP_TATTR",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CostComponentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CostComponent",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            )
        }
    )
    public interface CostComponentTypeAttrEntity {}

    /**
     * Cost Component Calculation
     */
    @Entity(
        name = "CostComponentCalc",
        packageName = "org.ofbiz.product.cost",
        title = "Cost Component Calculation",
        fields = {
            @Field(name = "costComponentCalcId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "costGlAccountTypeId", type = "id"),
            @Field(name = "offsettingGlAccountTypeId", type = "id"),
            @Field(name = "fixedCost", type = "currency-amount"),
            @Field(name = "variableCost", type = "currency-amount"),
            @Field(name = "perMilliSecond", type = "numeric"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "costCustomMethodId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "costComponentCalcId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                title = "Cost",
                fkName = "COST_COM_CGLAT",
                keyMaps = {
                    @KeyMap(fieldName = "costGlAccountTypeId", relFieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                title = "Offsetting",
                fkName = "COST_COM_OGLAT",
                keyMaps = {
                    @KeyMap(fieldName = "offsettingGlAccountTypeId", relFieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "COST_COM_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "COST_COM_CMET",
                keyMaps = {
                    @KeyMap(fieldName = "costCustomMethodId", relFieldName = "customMethodId")
                }
            )
        }
    )
    public interface CostComponentCalcEntity {}

    /**
     * Product Cost Calculation
     */
    @Entity(
        name = "ProductCostComponentCalc",
        packageName = "org.ofbiz.product.cost",
        title = "Product Cost Calculation",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "costComponentTypeId", type = "id-ne"),
            @Field(name = "costComponentCalcId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "costComponentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PR_COS_COMPCALC",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentType",
                fkName = "PR_COS_CCT",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CostComponentCalc",
                fkName = "PR_COS_CCC",
                keyMaps = {
                    @KeyMap(fieldName = "costComponentCalcId")
                }
            )
        }
    )
    public interface ProductCostComponentCalcEntity {}

    /**
     * Container
     */
    @Entity(
        name = "Container",
        packageName = "org.ofbiz.product.facility",
        title = "Container",
        fields = {
            @Field(name = "containerId", type = "id-ne"),
            @Field(name = "containerTypeId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "containerId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContainerType",
                fkName = "CONTAINER_CTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "containerTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "CONTAINER_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ContainerEntity {}

    /**
     * Container Type
     */
    @Entity(
        name = "ContainerType",
        packageName = "org.ofbiz.product.facility",
        title = "Container Type",
        fields = {
            @Field(name = "containerTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "containerTypeId")
        }
    )
    public interface ContainerTypeEntity {}

    /**
     * Container Geo Location with history
     */
    @Entity(
        name = "ContainerGeoPoint",
        packageName = "org.ofbiz.product.facility",
        title = "Container Geo Location with history",
        fields = {
            @Field(name = "containerId", type = "id-ne"),
            @Field(name = "geoPointId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "containerId"),
            @PrimaryKey(field = "geoPointId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Container",
                fkName = "CONTNRGEOPT_CONTNR",
                keyMaps = {
                    @KeyMap(fieldName = "containerId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "CONTNRGEOPT_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface ContainerGeoPointEntity {}

    /**
     * Facility
     */
    @Entity(
        name = "Facility",
        packageName = "org.ofbiz.product.facility",
        title = "Facility",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "facilityTypeId", type = "id"),
            @Field(name = "parentFacilityId", type = "id"),
            @Field(name = "ownerPartyId", type = "id-ne"),
            @Field(name = "defaultInventoryItemTypeId", type = "id"),
            @Field(name = "facilityName", type = "name"),
            @Field(name = "primaryFacilityGroupId", type = "id"),
            @Field(name = "oldSquareFootage", type = "numeric", colName = "SQUARE_FOOTAGE"),
            @Field(name = "facilitySize", type = "fixed-point"),
            @Field(name = "facilitySizeUomId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "defaultDaysToShip", type = "numeric", description = "In the absence of a product specific days to ship in ProductFacility, this will be used"),
            @Field(name = "openedDate", type = "date-time"),
            @Field(name = "closedDate", type = "date-time"),
            @Field(name = "description", type = "description"),
            @Field(name = "defaultDimensionUomId", type = "id", description = "This field store the unit of measurement of dimension (length, width and height)"),
            @Field(name = "defaultWeightUomId", type = "id"),
            @Field(name = "geoPointId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityType",
                fkName = "FACILITY_FCTYP",
                keyMaps = {
                    @KeyMap(fieldName = "facilityTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Parent",
                fkName = "FACILITY_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                fkName = "FACILITY_PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "primaryFacilityGroupId", relFieldName = "facilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Owner",
                fkName = "FACILITY_OWNER",
                keyMaps = {
                    @KeyMap(fieldName = "ownerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemType",
                title = "Default",
                fkName = "FAC_INVITM_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "defaultInventoryItemTypeId", relFieldName = "inventoryItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Dimension",
                fkName = "FAC_DEF_DUOM",
                keyMaps = {
                    @KeyMap(fieldName = "defaultDimensionUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Weight",
                fkName = "FAC_DEF_WUOM",
                keyMaps = {
                    @KeyMap(fieldName = "defaultWeightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductStore",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "facilityTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "FACILITY_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "FacilitySize",
                fkName = "FACILITY_SUOM",
                keyMaps = {
                    @KeyMap(fieldName = "facilitySizeUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface FacilityEntity {}

    /**
     * Facility Attribute
     */
    @Entity(
        name = "FacilityAttribute",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Attribute",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface FacilityAttributeEntity {}

    /**
     * Facility Calendar
     */
    @Entity(
        name = "FacilityCalendar",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Calendar",
        fields = {
            @Field(name = "facilityId", type = "id"),
            @Field(name = "calendarId", type = "id"),
            @Field(name = "facilityCalendarTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "calendarId"),
            @PrimaryKey(field = "facilityCalendarTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_CAL_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "TechDataCalendar",
                fkName = "FACILITY_TEC_CAL",
                keyMaps = {
                    @KeyMap(fieldName = "calendarId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityCalendarType",
                fkName = "FACILITY_CAL_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "facilityCalendarTypeId")
                }
            )
        }
    )
    public interface FacilityCalendarEntity {}

    /**
     * Facility Calendar Type
     */
    @Entity(
        name = "FacilityCalendarType",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Calendar Type",
        fields = {
            @Field(name = "facilityCalendarTypeId", type = "id"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityCalendarTypeId")
        }
    )
    public interface FacilityCalendarTypeEntity {}

    /**
     * Facility Role Type
     */
    @Entity(
        name = "FacilityCarrierShipment",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Role Type",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "shipmentMethodTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FACILITY_CSH_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_CSH_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "FACILITY_CSH_STP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CarrierShipmentMethod",
                fkName = "FACILITY_CSH_CSM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface FacilityCarrierShipmentEntity {}

    /**
     * Facility Contact Mechanism
     */
    @Entity(
        name = "FacilityContactMech",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Contact Mechanism",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "extension", type = "very-short"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACIL_CMECH_FACIL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "FACIL_CMECH_CMECH",
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
                type = RelationType.MANY,
                relEntityName = "FacilityContactMechPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface FacilityContactMechEntity {}

    /**
     * Facility Contact Mechanism Purpose
     */
    @Entity(
        name = "FacilityContactMechPurpose",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Contact Mechanism Purpose",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "contactMechId"),
            @PrimaryKey(field = "contactMechPurposeTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "FACIL_CMPRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACIL_CMPRP_FACIL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "FACIL_CMPRP_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface FacilityContactMechPurposeEntity {}

    /**
     * Facility Group
     */
    @Entity(
        name = "FacilityGroup",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Group",
        fields = {
            @Field(name = "facilityGroupId", type = "id-ne"),
            @Field(name = "facilityGroupTypeId", type = "id"),
            @Field(name = "primaryParentGroupId", type = "id"),
            @Field(name = "facilityGroupName", type = "name"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroupType",
                fkName = "FACILITY_GP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "facilityGroupTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                title = "PrimaryParent",
                fkName = "FACILITY_GP_PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "primaryParentGroupId", relFieldName = "facilityGroupId")
                }
            )
        }
    )
    public interface FacilityGroupEntity {}

    /**
     * Facility Group
     */
    @Entity(
        name = "FacilityGroupMember",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Group",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "facilityGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "facilityGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_MEM_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                fkName = "FACILITY_MEM_FGRP",
                keyMaps = {
                    @KeyMap(fieldName = "facilityGroupId")
                }
            )
        }
    )
    public interface FacilityGroupMemberEntity {}

    /**
     * Facility Group Role
     */
    @Entity(
        name = "FacilityGroupRole",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Group Role",
        fields = {
            @Field(name = "facilityGroupId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityGroupId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                fkName = "FGROUP_RLE_FGRP",
                keyMaps = {
                    @KeyMap(fieldName = "facilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "FGROUP_RLE_PTRLE",
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
    public interface FacilityGroupRoleEntity {}

    /**
     * Facility Group Rollup
     */
    @Entity(
        name = "FacilityGroupRollup",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Group Rollup",
        fields = {
            @Field(name = "facilityGroupId", type = "id-ne"),
            @Field(name = "parentFacilityGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityGroupId"),
            @PrimaryKey(field = "parentFacilityGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                title = "Current",
                fkName = "FGRP_FRLP_CURRENT",
                keyMaps = {
                    @KeyMap(fieldName = "facilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityGroup",
                title = "Parent",
                fkName = "FGRP_FRLP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentFacilityGroupId", relFieldName = "facilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityGroupRollup",
                title = "Child",
                keyMaps = {
                    @KeyMap(fieldName = "facilityGroupId", relFieldName = "parentFacilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityGroupRollup",
                title = "Parent",
                keyMaps = {
                    @KeyMap(fieldName = "parentFacilityGroupId", relFieldName = "facilityGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityGroupRollup",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentFacilityGroupId")
                }
            )
        }
    )
    public interface FacilityGroupRollupEntity {}

    /**
     * Facility Group Type
     */
    @Entity(
        name = "FacilityGroupType",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Group Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "facilityGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityGroupTypeId")
        }
    )
    public interface FacilityGroupTypeEntity {}

    /**
     * Facility Location
     */
    @Entity(
        name = "FacilityLocation",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Location",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "locationSeqId", type = "id-ne"),
            @Field(name = "locationTypeEnumId", type = "id-ne"),
            @Field(name = "areaId", type = "id"),
            @Field(name = "aisleId", type = "id"),
            @Field(name = "sectionId", type = "id"),
            @Field(name = "levelId", type = "id"),
            @Field(name = "positionId", type = "id"),
            @Field(name = "geoPointId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "locationSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_LOC_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Type",
                fkName = "FACILITY_LOC_TENM",
                keyMaps = {
                    @KeyMap(fieldName = "locationTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "FACILITY_LOC_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface FacilityLocationEntity {}

    /**
     * Facility Location Geo Location with history
     */
    @Entity(
        name = "FacilityLocationGeoPoint",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Location Geo Location with history",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "locationSeqId", type = "id-ne"),
            @Field(name = "geoPointId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "locationSeqId"),
            @PrimaryKey(field = "geoPointId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityLocation",
                fkName = "FACLOCGEOPT_FACLOC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoPoint",
                fkName = "FACLOCGEOPT_GEOPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        }
    )
    public interface FacilityLocationGeoPointEntity {}

    /**
     * Facility Party
     */
    @Entity(
        name = "FacilityParty",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Party",
        fields = {
            @Field(name = "facilityId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_RLE_FACI",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FACILITY_RLE_PRT",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "FACILITY_RLE_ROL",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "FACILITY_PRTY_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface FacilityPartyEntity {}

    /**
     * Facility Content
     */
    @Entity(
        name = "FacilityContent",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Content",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FAC_CNT_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "FAC_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface FacilityContentEntity {}

    /**
     * Facility Type
     */
    @Entity(
        name = "FacilityType",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "facilityTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityType",
                title = "Parent",
                fkName = "FACILITY_TYPEPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "facilityTypeId")
                }
            )
        }
    )
    public interface FacilityTypeEntity {}

    /**
     * Facility Type Attribute
     */
    @Entity(
        name = "FacilityTypeAttr",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Type Attribute",
        fields = {
            @Field(name = "facilityTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityType",
                fkName = "FACILITY_TPAT_FT",
                keyMaps = {
                    @KeyMap(fieldName = "facilityTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Facility",
                keyMaps = {
                    @KeyMap(fieldName = "facilityTypeId")
                }
            )
        }
    )
    public interface FacilityTypeAttrEntity {}

    /**
     * Product Facility
     */
    @Entity(
        name = "ProductFacility",
        packageName = "org.ofbiz.product.facility",
        title = "Product Facility",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "minimumStock", type = "fixed-point"),
            @Field(name = "reorderQuantity", type = "fixed-point"),
            @Field(name = "daysToShip", type = "numeric"),
            @Field(name = "lastInventoryCount", type = "fixed-point", description = "This field represents availableToPromiseTotal of a product at a certain point of time and is being updated regularly by a schedule service every hour (or ECA)"),
            @Field(name = "lastInventoryCountQoh", type = "fixed-point", description = "This field represents quantityOnHandTotal of a product at a certain point of time and is being updated regularly by a schedule service every hour (or ECA) (SCIPIO)"),
            @Field(name = "updatesSinceLastCount", type = "numeric", description = "Number of setLastInventoryCount calls below recount threshold left to trigger another recalc - see inventory.properties#inventory.cache.recountThreshold (SCIPIO)"),
            @Field(name = "lastInvStamp", type = "date-time", description = "Last time lastInventoryCount is updated (SCIPIO)"),
            @Field(name = "lastInvMode", type = "id", description = "Last way that lastInventoryCount updated: AUTO (DEFAULT), SCHEDULED, MANUAL (SCIPIO)"),
            @Field(name = "lastInvModeCount", type = "numeric", description = "Number of times lastInventoryCount has been updated using the value in lastInvMode, resets anytime mode changes (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "facilityId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_FAC_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PROD_FAC_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ProductFacilityEntity {}

    /**
     * Product Facility
     */
    @Entity(
        name = "ProductFacilityLocation",
        packageName = "org.ofbiz.product.facility",
        title = "Product Facility",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "locationSeqId", type = "id-ne"),
            @Field(name = "minimumStock", type = "fixed-point"),
            @Field(name = "moveQuantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "locationSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_FCL_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FacilityLocation",
                fkName = "PROD_FCL_FCL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            )
        }
    )
    public interface ProductFacilityLocationEntity {}

    /**
     * Product Feature
     */
    @Entity(
        name = "ProductFeature",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "productFeatureTypeId", type = "id-ne"),
            @Field(name = "productFeatureCategoryId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "numberSpecified", type = "fixed-point"),
            @Field(name = "defaultAmount", type = "currency-amount"),
            @Field(name = "defaultSequenceNum", type = "numeric"),
            @Field(name = "abbrev", type = "id"),
            @Field(name = "idCode", type = "id-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureCategory",
                fkName = "PROD_FEAT_CATEGORY",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureType",
                fkName = "PROD_FEAT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "PROD_FEAT_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            )
        }
    )
    public interface ProductFeatureEntity {}

    /**
     * Product Feature Applicability
     */
    @Entity(
        name = "ProductFeatureAppl",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Applicability",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "productFeatureApplTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "recurringAmount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productFeatureId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureApplType",
                fkName = "PROD_FAPPL_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureApplTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_FAPPL_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "PROD_FAPPL_FEATURE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureApplEntity {}

    /**
     * Product Feature Applicability Type
     */
    @Entity(
        name = "ProductFeatureApplType",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Applicability Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productFeatureApplTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureApplTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureApplType",
                title = "Parent",
                fkName = "PROD_FAPPL_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productFeatureApplTypeId")
                }
            )
        }
    )
    public interface ProductFeatureApplTypeEntity {}

    /**
     * Product Feature Applicability Attribute
     */
    @Entity(
        name = "ProductFeatureApplAttr",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Applicability Attribute",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productFeatureId"),
            @PrimaryKey(field = "fromDate"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_FAPPA_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "PROD_FAPPA_FEATURE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureAppl",
                fkName = "PROD_FAPPA_FEATAPP",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "productFeatureId"),
                    @KeyMap(fieldName = "fromDate")
                }
            )
        }
    )
    public interface ProductFeatureApplAttrEntity {}

    /**
     * Product Feature Category
     */
    @Entity(
        name = "ProductFeatureCategory",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Category",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productFeatureCategoryId", type = "id-ne"),
            @Field(name = "parentCategoryId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureCategory",
                title = "Parent",
                fkName = "PROD_FEAT_CAT_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentCategoryId", relFieldName = "productFeatureCategoryId")
                }
            )
        }
    )
    public interface ProductFeatureCategoryEntity {}

    /**
     * Product Feature Category Application
     */
    @Entity(
        name = "ProductFeatureCategoryAppl",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Category Application",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "productFeatureCategoryId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "productFeatureCategoryId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_FCAPPL_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureCategory",
                fkName = "PROD_FCAPPL_FCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureCategoryId")
                }
            )
        }
    )
    public interface ProductFeatureCategoryApplEntity {}

    /**
     * Product Category Feature Group Application
     */
    @Entity(
        name = "ProductFeatureCatGrpAppl",
        packageName = "org.ofbiz.product.feature",
        title = "Product Category Feature Group Application",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "productFeatureGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "productFeatureGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_FCGAPL_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureGroup",
                fkName = "PROD_FCGAPL_FGRP",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureGroupId")
                }
            )
        }
    )
    public interface ProductFeatureCatGrpApplEntity {}

    /**
     * Product Feature Data Resource
     */
    @Entity(
        name = "ProductFeatureDataResource",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Data Resource",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId"),
            @PrimaryKey(field = "productFeatureId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "PFEAT_DR_DATRES",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "PFEAT_DR_FEATURE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureDataResourceEntity {}

    /**
     * Product Feature Group
     */
    @Entity(
        name = "ProductFeatureGroup",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Group",
        fields = {
            @Field(name = "productFeatureGroupId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureGroupId")
        }
    )
    public interface ProductFeatureGroupEntity {}

    /**
     * Product Feature Group Applicability
     */
    @Entity(
        name = "ProductFeatureGroupAppl",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Group Applicability",
        fields = {
            @Field(name = "productFeatureGroupId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureGroupId"),
            @PrimaryKey(field = "productFeatureId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureGroup",
                fkName = "PROD_FGAPP_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "PROD_FGAPP_FEATURE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureGroupApplEntity {}

    /**
     * Product Feature Interaction
     */
    @Entity(
        name = "ProductFeatureIactn",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Interaction",
        fields = {
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "productFeatureIdTo", type = "id-ne"),
            @Field(name = "productFeatureIactnTypeId", type = "id"),
            @Field(name = "productId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureId"),
            @PrimaryKey(field = "productFeatureIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureIactnType",
                fkName = "PROD_FICTN_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureIactnTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                title = "Main",
                fkName = "PROD_FICTN_MFEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                title = "Assoc",
                fkName = "PROD_FICTN_AFEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureIdTo", relFieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureIactnEntity {}

    /**
     * Product Feature Interaction Type
     */
    @Entity(
        name = "ProductFeatureIactnType",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Interaction Type",
        fields = {
            @Field(name = "productFeatureIactnTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureIactnTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureIactnType",
                title = "Parent",
                fkName = "PROD_FICTN_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productFeatureIactnTypeId")
                }
            )
        }
    )
    public interface ProductFeatureIactnTypeEntity {}

    /**
     * Product Feature Type
     */
    @Entity(
        name = "ProductFeatureType",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productFeatureTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeatureType",
                title = "Parent",
                fkName = "PROD_FEAT_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productFeatureTypeId")
                }
            )
        }
    )
    public interface ProductFeatureTypeEntity {}

    /**
     * Product Feature Price
     */
    @Entity(
        name = "ProductFeaturePrice",
        packageName = "org.ofbiz.product.price",
        title = "Product Feature Price",
        fields = {
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "productPriceTypeId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "price", type = "currency-precise"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "productFeatureId"),
            @PrimaryKey(field = "productPriceTypeId"),
            @PrimaryKey(field = "currencyUomId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceType",
                fkName = "PROD_F_PRICE_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "PROD_F_PRICE_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PROD_F_PRICE_CBUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PROD_F_PRICE_LMBUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRD_FT_PRC_GENLKP",
                fields = {
                    @IndexField(name = "productFeatureId"),
                    @IndexField(name = "currencyUomId")
                }
            )
        }
    )
    public interface ProductFeaturePriceEntity {}

    /**
     * Inventory Item
     */
    @Entity(
        name = "InventoryItem",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "inventoryItemTypeId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "ownerPartyId", type = "id", description = "The owner of the inventory item."),
            @Field(name = "statusId", type = "id"),
            @Field(name = "datetimeReceived", type = "date-time"),
            @Field(name = "datetimeManufactured", type = "date-time"),
            @Field(name = "expireDate", type = "date-time"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "containerId", type = "id"),
            @Field(name = "lotId", type = "id"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "binNumber", type = "id"),
            @Field(name = "locationSeqId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "quantityOnHandTotal", type = "fixed-point"),
            @Field(name = "availableToPromiseTotal", type = "fixed-point"),
            @Field(name = "accountingQuantityTotal", type = "fixed-point"),
            @Field(name = "oldQuantityOnHand", type = "fixed-point", colName = "QUANTITY_ON_HAND"),
            @Field(name = "oldAvailableToPromise", type = "fixed-point", colName = "AVAILABLE_TO_PROMISE"),
            @Field(name = "serialNumber", type = "value"),
            @Field(name = "softIdentifier", type = "value"),
            @Field(name = "activationNumber", type = "value"),
            @Field(name = "activationValidThru", type = "date-time"),
            @Field(name = "unitCost", type = "fixed-point", description = "Higher precision in case it is a calculated number"),
            @Field(name = "currencyUomId", type = "id", description = "The currency Uom of the unit cost.")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemType",
                fkName = "INV_ITEM_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InventoryItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "INV_ITEM_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "INV_ITEM_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Owner",
                fkName = "INV_ITEM_OWNPARTY",
                keyMaps = {
                    @KeyMap(fieldName = "ownerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "INV_ITEM_STTSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "INV_ITEM_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Container",
                fkName = "INV_ITEM_CONTAINER",
                keyMaps = {
                    @KeyMap(fieldName = "containerId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Lot",
                fkName = "INV_ITEM_LOT",
                keyMaps = {
                    @KeyMap(fieldName = "lotId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFacility",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "INV_ITEM_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "INV_ITEM_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        },
        indexes = {
            @Index(
                name = "INVITEM_SOFID",
                unique = true,
                fields = {
                    @IndexField(name = "softIdentifier")
                }
            ),
            @Index(
                name = "INVITEM_ACTNM",
                unique = true,
                fields = {
                    @IndexField(name = "activationNumber")
                }
            ),
            @Index(
                name = "INV_ITEM_SN",
                fields = {
                    @IndexField(name = "serialNumber")
                }
            )
        }
    )
    public interface InventoryItemEntity {}

    /**
     * Inventory Item Attribute
     */
    @Entity(
        name = "InventoryItemAttribute",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Attribute",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_ITEM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InventoryItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface InventoryItemAttributeEntity {}

    /**
     * Inventory Item Detail
     */
    @Entity(
        name = "InventoryItemDetail",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Detail",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "inventoryItemDetailSeqId", type = "id-ne"),
            @Field(name = "effectiveDate", type = "date-time"),
            @Field(name = "quantityOnHandDiff", type = "fixed-point"),
            @Field(name = "availableToPromiseDiff", type = "fixed-point"),
            @Field(name = "accountingQuantityDiff", type = "fixed-point"),
            @Field(name = "unitCost", type = "fixed-point"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "shipGroupSeqId", type = "id"),
            @Field(name = "shipmentId", type = "id"),
            @Field(name = "shipmentItemSeqId", type = "id"),
            @Field(name = "returnId", type = "id"),
            @Field(name = "returnItemSeqId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "maintHistSeqId", type = "id"),
            @Field(name = "itemIssuanceId", type = "id"),
            @Field(name = "receiptId", type = "id"),
            @Field(name = "physicalInventoryId", type = "id"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId"),
            @PrimaryKey(field = "inventoryItemDetailSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_ITDTL_INVIT",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "INV_ITDTL_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGrpInvRes",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetMaint",
                fkName = "INV_ITDTL_FAMNT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId"),
                    @KeyMap(fieldName = "maintHistSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ItemIssuance",
                fkName = "INV_ITDTL_ITMIS",
                keyMaps = {
                    @KeyMap(fieldName = "itemIssuanceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortInventoryAssign",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffortInventoryProduced",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentReceipt",
                fkName = "INV_ITDTL_SHRCT",
                keyMaps = {
                    @KeyMap(fieldName = "receiptId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PhysicalInventory",
                fkName = "INV_ITDTL_PHINV",
                keyMaps = {
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Reason",
                fkName = "INV_ITDTL_REAS",
                keyMaps = {
                    @KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InventoryItemVariance",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId"),
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            )
        },
        indexes = {
            @Index(
                name = "INVITEM_DETAIL_DATE",
                fields = {
                    @IndexField(name = "inventoryItemId"),
                    @IndexField(name = "createdStamp")
                }
            )
        }
    )
    public interface InventoryItemDetailEntity {}

    /**
     * Inventory Item Status History
     */
    @Entity(
        name = "InventoryItemStatus",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Status History",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusDatetime", type = "date-time"),
            @Field(name = "statusEndDatetime", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong"),
            @Field(name = "ownerPartyId", type = "id", description = "Used to track a changed (new) ownerPartyId as a status changes."),
            @Field(name = "productId", type = "id", description = "Used to track a changed (new) productId as a status changes. In other words over time the item may be represented by a different Product (like new versus refurbished).")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId"),
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "statusDatetime")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_ITEM_STTS_II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "INV_ITEM_STTS_SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "INV_ITEM_STTS_USER",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface InventoryItemStatusEntity {}

    /**
     * Inventory Item Temporary Reservation
     */
    @Entity(
        name = "InventoryItemTempRes",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Temporary Reservation",
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "reservedDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productStoreId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "INV_ITEM_TR_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "INV_ITEM_TR_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface InventoryItemTempResEntity {}

    /**
     * Inventory Item Type
     */
    @Entity(
        name = "InventoryItemType",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "inventoryItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemType",
                title = "Parent",
                fkName = "INV_ITEM_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "inventoryItemTypeId")
                }
            )
        }
    )
    public interface InventoryItemTypeEntity {}

    /**
     * Inventory Item Type Attribute
     */
    @Entity(
        name = "InventoryItemTypeAttr",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Type Attribute",
        fields = {
            @Field(name = "inventoryItemTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemType",
                fkName = "INV_ITEM_TYP_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InventoryItemAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InventoryItem",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemTypeId")
                }
            )
        }
    )
    public interface InventoryItemTypeAttrEntity {}

    /**
     * Inventory Item Variance
     */
    @Entity(
        name = "InventoryItemVariance",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Variance",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "physicalInventoryId", type = "id-ne"),
            @Field(name = "varianceReasonId", type = "id"),
            @Field(name = "availableToPromiseVar", type = "fixed-point"),
            @Field(name = "quantityOnHandVar", type = "fixed-point"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId"),
            @PrimaryKey(field = "physicalInventoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PhysicalInventory",
                fkName = "INV_ITEM_VAR_PINV",
                keyMaps = {
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "VarianceReason",
                fkName = "INV_ITEM_VAR_RSN",
                keyMaps = {
                    @KeyMap(fieldName = "varianceReasonId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_ITEM_VAR_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface InventoryItemVarianceEntity {}

    /**
     * Inventory Item Label Type
     */
    @Entity(
        name = "InventoryItemLabelType",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Label Type",
        fields = {
            @Field(name = "inventoryItemLabelTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemLabelTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemLabelType",
                title = "Parent",
                fkName = "INV_ITLT_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "inventoryItemLabelTypeId")
                }
            )
        }
    )
    public interface InventoryItemLabelTypeEntity {}

    /**
     * Inventory Item Label
     */
    @Entity(
        name = "InventoryItemLabel",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Label",
        fields = {
            @Field(name = "inventoryItemLabelId", type = "id-ne"),
            @Field(name = "inventoryItemLabelTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemLabelId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemLabelType",
                fkName = "INV_ITLA_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemLabelTypeId")
                }
            )
        }
    )
    public interface InventoryItemLabelEntity {}

    /**
     * Inventory Item Label Applicability
     */
    @Entity(
        name = "InventoryItemLabelAppl",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Label Applicability",
        fields = {
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "inventoryItemLabelTypeId", type = "id-ne"),
            @Field(name = "inventoryItemLabelId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryItemId"),
            @PrimaryKey(field = "inventoryItemLabelTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_ITLAP_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemLabelType",
                fkName = "INV_ITLAP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemLabelTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemLabel",
                fkName = "INV_ITLAP_LAB",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemLabelId")
                }
            )
        }
    )
    public interface InventoryItemLabelApplEntity {}

    /**
     * Inventory Transfer
     */
    @Entity(
        name = "InventoryTransfer",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Transfer",
        fields = {
            @Field(name = "inventoryTransferId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "locationSeqId", type = "id"),
            @Field(name = "containerId", type = "id"),
            @Field(name = "facilityIdTo", type = "id"),
            @Field(name = "locationSeqIdTo", type = "id"),
            @Field(name = "containerIdTo", type = "id"),
            @Field(name = "itemIssuanceId", type = "id"),
            @Field(name = "sendDate", type = "date-time"),
            @Field(name = "receiveDate", type = "date-time"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "inventoryTransferId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "INV_XFER_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "INV_XFER_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "INV_XFER_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Container",
                fkName = "INV_XFER_CONT",
                keyMaps = {
                    @KeyMap(fieldName = "containerId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "To",
                fkName = "INV_XFER_TFAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityIdTo", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "facilityIdTo", relFieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqIdTo", relFieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Container",
                title = "To",
                fkName = "INV_XFER_TCNT",
                keyMaps = {
                    @KeyMap(fieldName = "containerIdTo", relFieldName = "containerId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ItemIssuance",
                fkName = "INV_XFER_ISSU",
                keyMaps = {
                    @KeyMap(fieldName = "itemIssuanceId")
                }
            )
        }
    )
    public interface InventoryTransferEntity {}

    /**
     * Lot
     */
    @Entity(
        name = "Lot",
        packageName = "org.ofbiz.product.inventory",
        title = "Lot",
        fields = {
            @Field(name = "lotId", type = "id-ne"),
            @Field(name = "creationDate", type = "date-time"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "expirationDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "lotId")
        }
    )
    public interface LotEntity {}

    /**
     * Physical Inventory
     */
    @Entity(
        name = "PhysicalInventory",
        packageName = "org.ofbiz.product.inventory",
        title = "Physical Inventory",
        fields = {
            @Field(name = "physicalInventoryId", type = "id-ne"),
            @Field(name = "physicalInventoryDate", type = "date-time"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "generalComments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "physicalInventoryId")
        }
    )
    public interface PhysicalInventoryEntity {}

    /**
     * Variance Reason
     */
    @Entity(
        name = "VarianceReason",
        packageName = "org.ofbiz.product.inventory",
        title = "Variance Reason",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "varianceReasonId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "varianceReasonId")
        }
    )
    public interface VarianceReasonEntity {}

    /**
     * Product PaymentMethodType
     */
    @Entity(
        name = "ProductPaymentMethodType",
        packageName = "org.ofbiz.product.price",
        title = "Product PaymentMethodType",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "productPricePurposeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "paymentMethodTypeId"),
            @PrimaryKey(field = "productPricePurposeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_PMT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PROD_PMT_PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPricePurpose",
                fkName = "PROD_PMT_PPRP",
                keyMaps = {
                    @KeyMap(fieldName = "productPricePurposeId")
                }
            )
        }
    )
    public interface ProductPaymentMethodTypeEntity {}

    /**
     * Product Price
     */
    @Entity(
        name = "ProductPrice",
        packageName = "org.ofbiz.product.price",
        title = "Product Price",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productPriceTypeId", type = "id-ne"),
            @Field(name = "productPricePurposeId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "productStoreGroupId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "price", type = "currency-precise"),
            @Field(name = "termUomId", type = "id", description = "Mainly used for recurring and usage prices to specify a time/freq measure, or a usage unit measure (bits, minutes, etc)"),
            @Field(name = "customPriceCalcService", type = "id", description = "Points to a CustomMethod used to specify a service for the calculation of the unit price of the product (NOTE: a better name for this field might be priceCalcCustomMethodId)"),
            @Field(name = "priceWithoutTax", type = "currency-precise", description = "Always without tax if populated, regardless of if price does or does not include tax."),
            @Field(name = "priceWithTax", type = "currency-precise", description = "Always with tax if populated, regardless of if price does or does not include tax."),
            @Field(name = "taxAmount", type = "currency-precise"),
            @Field(name = "taxPercentage", type = "fixed-point"),
            @Field(name = "taxAuthPartyId", type = "id-ne"),
            @Field(name = "taxAuthGeoId", type = "id-ne"),
            @Field(name = "taxInPrice", type = "indicator", description = "If Y the price field has tax included for the given taxAuthPartyId/taxAuthGeoId at the taxPercentage."),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productPriceTypeId"),
            @PrimaryKey(field = "productPricePurposeId"),
            @PrimaryKey(field = "currencyUomId"),
            @PrimaryKey(field = "productStoreGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_PRICE_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceType",
                fkName = "PROD_PRICE_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPricePurpose",
                fkName = "PROD_PRICE_PURP",
                keyMaps = {
                    @KeyMap(fieldName = "productPricePurposeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "PROD_PRICE_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Term",
                fkName = "PROD_PRICE_TUOM",
                keyMaps = {
                    @KeyMap(fieldName = "termUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                fkName = "PROD_PRICE_PSTG",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "PROD_PRICE_CMET",
                keyMaps = {
                    @KeyMap(fieldName = "customPriceCalcService", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "TaxAuthority",
                fkName = "PROD_PRC_TAXPTY",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "TaxAuthority",
                fkName = "PROD_PRC_TAXGEO",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PROD_PRICE_CBUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PROD_PRICE_LMBUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRD_PRC_GENLKP",
                fields = {
                    @IndexField(name = "productId"),
                    @IndexField(name = "productPricePurposeId"),
                    @IndexField(name = "currencyUomId"),
                    @IndexField(name = "productStoreGroupId")
                }
            )
        }
    )
    public interface ProductPriceEntity {}

    /**
     * Product Price Action
     */
    @Entity(
        name = "ProductPriceAction",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Action",
        fields = {
            @Field(name = "productPriceRuleId", type = "id-ne"),
            @Field(name = "productPriceActionSeqId", type = "id-ne"),
            @Field(name = "productPriceActionTypeId", type = "id-ne"),
            @Field(name = "amount", type = "fixed-point"),
            @Field(name = "rateCode", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceRuleId"),
            @PrimaryKey(field = "productPriceActionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceActionType",
                fkName = "PROD_PCACT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceActionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceRule",
                fkName = "PROD_PCACT_RL",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceRuleId")
                }
            )
        }
    )
    public interface ProductPriceActionEntity {}

    /**
     * Product Price Type
     */
    @Entity(
        name = "ProductPriceActionType",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productPriceActionTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceActionTypeId")
        }
    )
    public interface ProductPriceActionTypeEntity {}

    /**
     * Product Price Automatic Notice History
     */
    @Entity(
        name = "ProductPriceAutoNotice",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Automatic Notice History",
        fields = {
            @Field(name = "productPriceNoticeId", type = "id-ne"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "runDate", type = "date-time"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceNoticeId")
        }
    )
    public interface ProductPriceAutoNoticeEntity {}

    /**
     * Product Price Change History
     */
    @Entity(
        name = "ProductPriceChange",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Change History",
        fields = {
            @Field(name = "productPriceChangeId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productPriceTypeId", type = "id-ne"),
            @Field(name = "productPricePurposeId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "productStoreGroupId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "price", type = "currency-amount"),
            @Field(name = "oldPrice", type = "currency-amount"),
            @Field(name = "changedDate", type = "date-time"),
            @Field(name = "changedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceChangeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPrice",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "productPriceTypeId"),
                    @KeyMap(fieldName = "productPricePurposeId"),
                    @KeyMap(fieldName = "currencyUomId"),
                    @KeyMap(fieldName = "productStoreGroupId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangedBy",
                fkName = "PROD_PRCHNG_CHUL",
                keyMaps = {
                    @KeyMap(fieldName = "changedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ProductPriceChangeEntity {}

    /**
     * Product Price Condition
     */
    @Entity(
        name = "ProductPriceCond",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Condition",
        fields = {
            @Field(name = "productPriceRuleId", type = "id-ne"),
            @Field(name = "productPriceCondSeqId", type = "id-ne"),
            @Field(name = "inputParamEnumId", type = "id"),
            @Field(name = "operatorEnumId", type = "id"),
            @Field(name = "condValue", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceRuleId"),
            @PrimaryKey(field = "productPriceCondSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceRule",
                fkName = "PROD_PCCOND_RULE",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "InputParam",
                fkName = "PROD_PCCOND_INENUM",
                keyMaps = {
                    @KeyMap(fieldName = "inputParamEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Operator",
                fkName = "PROD_PCCOND_OPENUM",
                keyMaps = {
                    @KeyMap(fieldName = "operatorEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductPriceCondEntity {}

    /**
     * Product Price Purpose
     */
    @Entity(
        name = "ProductPricePurpose",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Purpose",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productPricePurposeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPricePurposeId")
        }
    )
    public interface ProductPricePurposeEntity {}

    /**
     * Product Pice Rule
     */
    @Entity(
        name = "ProductPriceRule",
        packageName = "org.ofbiz.product.price",
        title = "Product Pice Rule",
        fields = {
            @Field(name = "productPriceRuleId", type = "id-ne"),
            @Field(name = "ruleName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "isSale", type = "indicator"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceRuleId")
        }
    )
    public interface ProductPriceRuleEntity {}

    /**
     * Product Price Type
     */
    @Entity(
        name = "ProductPriceType",
        packageName = "org.ofbiz.product.price",
        title = "Product Price Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productPriceTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPriceTypeId")
        }
    )
    public interface ProductPriceTypeEntity {}

    /**
     * Quantity Break
     */
    @Entity(
        name = "QuantityBreak",
        packageName = "org.ofbiz.product.price",
        title = "Quantity Break",
        fields = {
            @Field(name = "quantityBreakId", type = "id-ne"),
            @Field(name = "quantityBreakTypeId", type = "id"),
            @Field(name = "fromQuantity", type = "fixed-point"),
            @Field(name = "thruQuantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "quantityBreakId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuantityBreakType",
                fkName = "QUANT_BRK_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "quantityBreakTypeId")
                }
            )
        }
    )
    public interface QuantityBreakEntity {}

    /**
     * Quantity Break Type
     */
    @Entity(
        name = "QuantityBreakType",
        packageName = "org.ofbiz.product.price",
        title = "Quantity Break Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "quantityBreakTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "quantityBreakTypeId")
        }
    )
    public interface QuantityBreakTypeEntity {}

    /**
     * Sale Type
     */
    @Entity(
        name = "SaleType",
        packageName = "org.ofbiz.product.price",
        title = "Sale Type",
        fields = {
            @Field(name = "saleTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "saleTypeId")
        }
    )
    public interface SaleTypeEntity {}

    /**
     * Good Identification
     */
    @Entity(
        name = "GoodIdentification",
        packageName = "org.ofbiz.product.product",
        title = "Good Identification",
        fields = {
            @Field(name = "goodIdentificationTypeId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "idValue", type = "id-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "goodIdentificationTypeId"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GoodIdentificationType",
                fkName = "GOOD_ID_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "goodIdentificationTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "GOOD_ID_PRODICT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        indexes = {
            @Index(
                name = "GOOD_ID_VALIDX",
                fields = {
                    @IndexField(name = "idValue")
                }
            )
        }
    )
    public interface GoodIdentificationEntity {}

    /**
     * Good Identification Type
     */
    @Entity(
        name = "GoodIdentificationType",
        packageName = "org.ofbiz.product.product",
        title = "Good Identification Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "goodIdentificationTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "goodIdentificationTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GoodIdentificationType",
                title = "Parent",
                fkName = "GOOD_ID_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "goodIdentificationTypeId")
                }
            )
        }
    )
    public interface GoodIdentificationTypeEntity {}

    /**
     * Product
     */
    @Entity(
        name = "Product",
        packageName = "org.ofbiz.product.product",
        title = "Product",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productTypeId", type = "id"),
            @Field(name = "primaryProductCategoryId", type = "id", description = "The primary category ; it should be one of the productCategoryId already setup in ProductCategoryMember"),
            @Field(name = "manufacturerPartyId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "introductionDate", type = "date-time"),
            @Field(name = "releaseDate", type = "date-time"),
            @Field(name = "supportDiscontinuationDate", type = "date-time"),
            @Field(name = "salesDiscontinuationDate", type = "date-time"),
            @Field(name = "salesDiscWhenNotAvail", type = "indicator"),
            @Field(name = "internalName", type = "description"),
            @Field(name = "brandName", type = "name"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "productName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "longDescription", type = "very-long"),
            @Field(name = "priceDetailText", type = "description"),
            @Field(name = "smallImageUrl", type = "url"),
            @Field(name = "mediumImageUrl", type = "url"),
            @Field(name = "largeImageUrl", type = "url"),
            @Field(name = "detailImageUrl", type = "url"),
            @Field(name = "originalImageUrl", type = "url"),
            @Field(name = "detailScreen", type = "long-varchar"),
            @Field(name = "inventoryMessage", type = "description"),
            @Field(name = "inventoryItemTypeId", type = "id"),
            @Field(name = "requireInventory", type = "indicator"),
            @Field(name = "quantityUomId", type = "id"),
            @Field(name = "quantityIncluded", type = "fixed-point", description = "If you have a six-pack of 12oz soda cans you would have quantityIncluded=12, quantityUomId=oz, piecesIncluded=6."),
            @Field(name = "piecesIncluded", type = "numeric"),
            @Field(name = "requireAmount", type = "indicator"),
            @Field(name = "fixedAmount", type = "currency-amount", description = "Use this for products which are sold in fixed denominations, such as gift certificates or calling cards."),
            @Field(name = "amountUomTypeId", type = "id"),
            @Field(name = "weightUomId", type = "id"),
            @Field(name = "weight", type = "fixed-point", description = "The shipping weight of the product."),
            @Field(name = "productWeight", type = "fixed-point"),
            @Field(name = "heightUomId", type = "id"),
            @Field(name = "productHeight", type = "fixed-point"),
            @Field(name = "shippingHeight", type = "fixed-point"),
            @Field(name = "widthUomId", type = "id"),
            @Field(name = "productWidth", type = "fixed-point"),
            @Field(name = "shippingWidth", type = "fixed-point"),
            @Field(name = "depthUomId", type = "id"),
            @Field(name = "productDepth", type = "fixed-point"),
            @Field(name = "shippingDepth", type = "fixed-point"),
            @Field(name = "diameterUomId", type = "id"),
            @Field(name = "productDiameter", type = "fixed-point"),
            @Field(name = "productRating", type = "fixed-point"),
            @Field(name = "ratingTypeEnum", type = "id"),
            @Field(name = "returnable", type = "indicator"),
            @Field(name = "taxable", type = "indicator"),
            @Field(name = "chargeShipping", type = "indicator"),
            @Field(name = "autoCreateKeywords", type = "indicator"),
            @Field(name = "includeInPromotions", type = "indicator"),
            @Field(name = "isVirtual", type = "indicator"),
            @Field(name = "isVariant", type = "indicator"),
            @Field(name = "virtualVariantMethodEnum", type = "id", description = "This field defines the method of selecting a variant from the selectable features on the virtual product. Either as a variant explosion which will work to about 200 variants or as feature explosion which almost has no limits"),
            @Field(name = "originGeoId", type = "id"),
            @Field(name = "requirementMethodEnumId", type = "id"),
            @Field(name = "billOfMaterialLevel", type = "numeric"),
            @Field(name = "reservMaxPersons", type = "fixed-point", description = "maximum number of persons who can rent this asset at the same time"),
            @Field(name = "reserv2ndPPPerc", type = "fixed-point", description = "percentage of the end price for the 2nd person renting this asset connected to this product"),
            @Field(name = "reservNthPPPerc", type = "fixed-point", description = "percentage of the end price for the Nth person renting this asset connected to this product"),
            @Field(name = "configId", type = "id", description = "Used to safe the persisted configuration Id for AGGREGATED products."),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "inShippingBox", type = "indicator"),
            @Field(name = "defaultShipmentBoxTypeId", type = "id"),
            @Field(name = "lotIdFilledIn", type = "long-varchar", description = "Indicate if the lotId must be informed"),
            @Field(name = "orderDecimalQuantity", type = "indicator", description = "use to indicate if decimal quantity can be ordered for this product. Default value is Y"),
            @Field(name = "listed", type = "indicator", description = "Whether to include product in public category listings, otherwise considered URL-only, default Y (SCIPIO)"),
            @Field(name = "searchable", type = "indicator", description = "Whether to include product in public searches, otherwise considered URL-only, default Y (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductType",
                fkName = "PROD_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "productTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                title = "Primary",
                fkName = "PROD_PRIMARY_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "primaryProductCategoryId", relFieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PROD_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Manufacturer",
                fkName = "PROD_MFG_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "manufacturerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Quantity",
                fkName = "PROD_QUANT_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UomType",
                title = "Amount",
                fkName = "PROD_AMOUNT_UOMT",
                keyMaps = {
                    @KeyMap(fieldName = "amountUomTypeId", relFieldName = "uomTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Weight",
                fkName = "PROD_WEIGHT_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Height",
                fkName = "PROD_HEIGHT_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "heightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Width",
                fkName = "PROD_WIDTH_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "widthUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Depth",
                fkName = "PROD_DEPTH_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "depthUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Diameter",
                fkName = "PROD_DIAMTR_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "diameterUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "VirtualVariantMethod",
                fkName = "PROD_VVMETHOD_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "virtualVariantMethodEnum", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Rating",
                fkName = "PROD_RATE_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "ratingTypeEnum", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "RequirementMethod",
                fkName = "PROD_RQMT_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "requirementMethodEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Origin",
                fkName = "PROD_ORG_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "originGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PROD_CB_USERLOGIN",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PROD_LMB_USERLOGIN",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductFeatureAndAppl",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentBoxType",
                title = "Default",
                fkName = "PROD_SHBX_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "defaultShipmentBoxTypeId", relFieldName = "shipmentBoxTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItemType",
                fkName = "PROD_INV_ITEM_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemTypeId", relFieldName = "inventoryItemTypeId")
                }
            )
        }
    )
    public interface ProductEntity {}

    /**
     * Inline Product Media Details
     */
    @Entity(
        name = "ProductMediaDetails",
        packageName = "org.ofbiz.product.product",
        title = "Inline Product Media Details",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "smallImageWidth", type = "numeric"),
            @Field(name = "smallImageHeight", type = "numeric"),
            @Field(name = "smallImageMimeTypeId", type = "id-vlong"),
            @Field(name = "smallImagePresetJson", type = "very-long"),
            @Field(name = "mediumImageWidth", type = "numeric"),
            @Field(name = "mediumImageHeight", type = "numeric"),
            @Field(name = "mediumImageMimeTypeId", type = "id-vlong"),
            @Field(name = "mediumImagePresetJson", type = "very-long"),
            @Field(name = "largeImageWidth", type = "numeric"),
            @Field(name = "largeImageHeight", type = "numeric"),
            @Field(name = "largeImageMimeTypeId", type = "id-vlong"),
            @Field(name = "largeImagePresetJson", type = "very-long"),
            @Field(name = "detailImageWidth", type = "numeric"),
            @Field(name = "detailImageHeight", type = "numeric"),
            @Field(name = "detailImageMimeTypeId", type = "id-vlong"),
            @Field(name = "detailImagePresetJson", type = "very-long"),
            @Field(name = "originalImageWidth", type = "numeric"),
            @Field(name = "originalImageHeight", type = "numeric"),
            @Field(name = "originalImageMimeTypeId", type = "id-vlong"),
            @Field(name = "originalImagePresetJson", type = "very-long"),
            @Field(name = "originalImageFileName", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODMEDDET_PRODID",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductMediaDetailsEntity {}

    /**
     * Inline Product Media Details
     */
    @Entity(
        name = "ProductImageOpRequest",
        packageName = "org.ofbiz.product.product",
        title = "Inline Product Media Details",
        fields = {
            @Field(name = "piorId", type = "id-ne"),
            @Field(name = "serviceId", type = "id-ne", description = "Typically service name (e.g.: productImageAutoRescale)"),
            @Field(name = "mode", type = "id-ne", description = "Values: sync, async/async-memory, async-persist"),
            @Field(name = "productId", type = "id"),
            @Field(name = "serviceArgsJson", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "piorId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODIMOPREQ_PRODID",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductImageOpRequestEntity {}

    /**
     * Product Association
     */
    @Entity(
        name = "ProductAssoc",
        packageName = "org.ofbiz.product.product",
        title = "Product Association",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productIdTo", type = "id-ne"),
            @Field(name = "productAssocTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "reason", type = "long-varchar"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "scrapFactor", type = "fixed-point"),
            @Field(name = "instruction", type = "long-varchar"),
            @Field(name = "routingWorkEffortId", type = "id"),
            @Field(name = "estimateCalcMethod", type = "id"),
            @Field(name = "recurrenceInfoId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productIdTo"),
            @PrimaryKey(field = "productAssocTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductAssocType",
                fkName = "PROD_ASSOC_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Main",
                fkName = "PROD_ASSOC_MPROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Assoc",
                fkName = "PROD_ASSOC_APROD",
                keyMaps = {
                    @KeyMap(fieldName = "productIdTo", relFieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "Routing",
                fkName = "PROD_ASSOC_RTWE",
                keyMaps = {
                    @KeyMap(fieldName = "routingWorkEffortId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "PROD_ASSOC_CUSM",
                keyMaps = {
                    @KeyMap(fieldName = "estimateCalcMethod", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceInfo",
                fkName = "PROD_ASSOC_RECINFO",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            )
        }
    )
    public interface ProductAssocEntity {}

    /**
     * Product Association Type
     */
    @Entity(
        name = "ProductAssocType",
        packageName = "org.ofbiz.product.product",
        title = "Product Association Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productAssocTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productAssocTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductAssocType",
                title = "Parent",
                fkName = "PROD_ASSOC_TYPEPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productAssocTypeId")
                }
            )
        }
    )
    public interface ProductAssocTypeEntity {}

    /**
     * Product Role
     */
    @Entity(
        name = "ProductRole",
        packageName = "org.ofbiz.product.product",
        title = "Product Role",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric", description = "a product may have several parties associated to it with the same role; this field can be used to define the order of parties associated to the product in that role"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
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
                fkName = "PROD_RLE_PTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_RLE_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductRoleEntity {}

    /**
     * Product Attribute
     */
    @Entity(
        name = "ProductAttribute",
        packageName = "org.ofbiz.product.product",
        title = "Product Attribute",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrType", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface ProductAttributeEntity {}

    /**
     * Product Calculated Info
     */
    @Entity(
        name = "ProductCalculatedInfo",
        packageName = "org.ofbiz.product.product",
        title = "Product Calculated Info",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "totalQuantityOrdered", type = "fixed-point"),
            @Field(name = "totalTimesViewed", type = "numeric"),
            @Field(name = "averageCustomerRating", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODCI_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductCalculatedInfoEntity {}

    /**
     * Product Data Object
     */
    @Entity(
        name = "ProductContent",
        packageName = "org.ofbiz.product.product",
        title = "Product Data Object",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "productContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "purchaseFromDate", type = "date-time"),
            @Field(name = "purchaseThruDate", type = "date-time"),
            @Field(name = "useCountLimit", type = "numeric"),
            @Field(name = "useTime", type = "numeric"),
            @Field(name = "useTimeUomId", type = "id"),
            @Field(name = "useRoleTypeId", type = "id"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "productContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_CNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PROD_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductContentType",
                fkName = "PROD_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productContentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "Use",
                fkName = "PROD_CNT_URT",
                keyMaps = {
                    @KeyMap(fieldName = "useRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "UseTime",
                fkName = "PROD_CNT_UTU",
                keyMaps = {
                    @KeyMap(fieldName = "useTimeUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ProductContentEntity {}

    /**
     * Product Content Type
     */
    @Entity(
        name = "ProductContentType",
        packageName = "org.ofbiz.product.product",
        title = "Product Content Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "viewType", type = "name", description = "For images: main, additional or a custom type (SCIPIO)"),
            @Field(name = "viewNumber", type = "value", description = "For images: 0 for main, 1-4 or greater for additional images (SCIPIO)"),
            @Field(name = "viewSize", type = "name", description = "For images: original, detail, large, 320x240, etc., also known as sizeType (SCIPIO)"),
            @Field(name = "viewVariantId", type = "name", description = "For images: flexible expression pattern for generating variant productContentTypeId of an original image URL (SCIPIO)"),
            @Field(name = "viewVariantDesc", type = "name", description = "For images: flexible expression pattern for generating variant description of an original image URL (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "productContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductContentType",
                title = "Parent",
                fkName = "PRDCT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productContentTypeId")
                }
            )
        }
    )
    public interface ProductContentTypeEntity {}

    /**
     * Product Geo
     */
    @Entity(
        name = "ProductGeo",
        packageName = "org.ofbiz.product.product",
        title = "Product Geo",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "geoId", type = "id"),
            @Field(name = "productGeoEnumId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRDGEO_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "PRDGEO_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "PRDGEO_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "productGeoEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductGeoEntity {}

    /**
     * Product GlAccount
     */
    @Entity(
        name = "ProductGlAccount",
        packageName = "org.ofbiz.product.product",
        title = "Product GlAccount",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "organizationPartyId", type = "id-ne"),
            @Field(name = "glAccountTypeId", type = "id-ne"),
            @Field(name = "glAccountId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "organizationPartyId"),
            @PrimaryKey(field = "glAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_GLACT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PROD_GLACT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "organizationPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccountType",
                fkName = "PROD_GLACT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                fkName = "PROD_GLACT_GLACT",
                keyMaps = {
                    @KeyMap(fieldName = "glAccountId")
                }
            )
        }
    )
    public interface ProductGlAccountEntity {}

    /**
     * Product Keyword
     */
    @Entity(
        name = "ProductKeyword",
        packageName = "org.ofbiz.product.product",
        tableName = "PRODUCT_KEYWORD_NEW",
        title = "Product Keyword",
        neverCache = true,
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "keyword", type = "short-varchar"),
            @Field(name = "keywordTypeId", type = "id-ne"),
            @Field(name = "relevancyWeight", type = "numeric"),
            @Field(name = "statusId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "keyword"),
            @PrimaryKey(field = "keywordTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_KWD_PROD_NEW",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "PROD_KWD_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "keywordTypeId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PROD_KWD_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PROD_KWD_KWD_NEW",
                fields = {
                    @IndexField(name = "keyword")
                }
            )
        }
    )
    public interface ProductKeywordEntity {}

    /**
     * Product Meter
     */
    @Entity(
        name = "ProductMeter",
        packageName = "org.ofbiz.product.product",
        title = "Product Meter",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productMeterTypeId", type = "id-ne", description = "Part of the primary key as different meters on a machine should have distinct types"),
            @Field(name = "meterUomId", type = "id", description = "Is on this entity instead of the ProductMeterType entity for more flexibility; for example being able to find all speedometers regardless of their primary unit"),
            @Field(name = "meterName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productMeterTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODMTR_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMeterType",
                fkName = "PRODMTR_MTRTYP",
                keyMaps = {
                    @KeyMap(fieldName = "productMeterTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Meter",
                fkName = "PRODMTR_MTRUOM",
                keyMaps = {
                    @KeyMap(fieldName = "meterUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ProductMeterEntity {}

    /**
     * Product Meter Type
     */
    @Entity(
        name = "ProductMeterType",
        packageName = "org.ofbiz.product.product",
        title = "Product Meter Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productMeterTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "defaultUomId", type = "id", description = "This is optional and if applicable can describe the meter better")
        },
        primaryKeys = {
            @PrimaryKey(field = "productMeterTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Default",
                fkName = "PRODMTRTP_DUOM",
                keyMaps = {
                    @KeyMap(fieldName = "defaultUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ProductMeterTypeEntity {}

    /**
     * Product Maintenance
     * This is used to specify the details for scheduled maintenance.
     */
    @Entity(
        name = "ProductMaint",
        packageName = "org.ofbiz.product.product",
        title = "Product Maintenance",
        description = "This is used to specify the details for scheduled maintenance.",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productMaintSeqId", type = "id-ne"),
            @Field(name = "productMaintTypeId", type = "id"),
            @Field(name = "maintName", type = "name"),
            @Field(name = "maintTemplateWorkEffortId", type = "id", description = "Template of Maintenance Plan. WorkEffort may have WorkEffortAssocs for tasks/breakdown detailes"),
            @Field(name = "intervalQuantity", type = "fixed-point"),
            @Field(name = "intervalUomId", type = "id", description = "UOM for intervalQuantity; if used intervalMeterTypeId is generally not used (ie one or the other)"),
            @Field(name = "intervalMeterTypeId", type = "id", description = "Meter Type for intervalQuantity; if used intervalUomId is generally not used (ie one or the other)"),
            @Field(name = "repeatCount", type = "numeric", description = "If 0 or null means no limit to repeat count; can be used with multiple ProductMaint records for a single ProductMaintType in cases where maintenance intervals are not evenly distributed, or only need to be done once like a break-in period")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "productMaintSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PRODMNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMaintType",
                fkName = "PRODMNT_MNTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "productMaintTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "MaintTemplate",
                fkName = "PRODMNT_TPLHWE",
                keyMaps = {
                    @KeyMap(fieldName = "maintTemplateWorkEffortId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Interval",
                fkName = "PRODMNT_INTUOM",
                keyMaps = {
                    @KeyMap(fieldName = "intervalUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMeterType",
                title = "Interval",
                fkName = "PRODMNT_PDMTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "intervalMeterTypeId", relFieldName = "productMeterTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductMeter",
                title = "Interval",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "intervalMeterTypeId", relFieldName = "productMeterTypeId")
                }
            )
        }
    )
    public interface ProductMaintEntity {}

    /**
     * Product Maintenance Type
     * This is for both scheduled and unscheduled maintenance; use ProductMaint to track details for scheduled maintenance
     */
    @Entity(
        name = "ProductMaintType",
        packageName = "org.ofbiz.product.product",
        title = "Product Maintenance Type",
        description = "This is for both scheduled and unscheduled maintenance; use ProductMaint to track details for scheduled maintenance",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productMaintTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "parentTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "productMaintTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductMaintType",
                title = "Parent",
                fkName = "PRODMNT_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productMaintTypeId")
                }
            )
        }
    )
    public interface ProductMaintTypeEntity {}

    /**
     * Product Review
     */
    @Entity(
        name = "ProductReview",
        packageName = "org.ofbiz.product.product",
        title = "Product Review",
        fields = {
            @Field(name = "productReviewId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "postedAnonymous", type = "indicator"),
            @Field(name = "postedDateTime", type = "date-time"),
            @Field(name = "productRating", type = "fixed-point"),
            @Field(name = "productReview", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "productReviewId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PROD_REVIEW_PRDSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_REVIEW_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "PROD_REVIEW_ULH",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PROD_REVIEW_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface ProductReviewEntity {}

    /**
     * Product Search Result Constraint
     */
    @Entity(
        name = "ProductSearchConstraint",
        packageName = "org.ofbiz.product.product",
        title = "Product Search Result Constraint",
        neverCache = true,
        fields = {
            @Field(name = "productSearchResultId", type = "id-ne"),
            @Field(name = "constraintSeqId", type = "id-ne"),
            @Field(name = "constraintName", type = "long-varchar"),
            @Field(name = "infoString", type = "long-varchar"),
            @Field(name = "includeSubCategories", type = "indicator"),
            @Field(name = "isAnd", type = "indicator"),
            @Field(name = "anyPrefix", type = "indicator"),
            @Field(name = "anySuffix", type = "indicator"),
            @Field(name = "removeStems", type = "indicator"),
            @Field(name = "lowValue", type = "short-varchar"),
            @Field(name = "highValue", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "productSearchResultId"),
            @PrimaryKey(field = "constraintSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductSearchResult",
                fkName = "PROD_SCHRSI_RES",
                keyMaps = {
                    @KeyMap(fieldName = "productSearchResultId")
                }
            )
        }
    )
    public interface ProductSearchConstraintEntity {}

    /**
     * Product Search Result
     */
    @Entity(
        name = "ProductSearchResult",
        packageName = "org.ofbiz.product.product",
        title = "Product Search Result",
        neverCache = true,
        fields = {
            @Field(name = "productSearchResultId", type = "id-ne"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "orderByName", type = "long-varchar"),
            @Field(name = "isAscending", type = "indicator"),
            @Field(name = "numResults", type = "numeric"),
            @Field(name = "secondsTotal", type = "floating-point"),
            @Field(name = "searchDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productSearchResultId")
        }
    )
    public interface ProductSearchResultEntity {}

    /**
     * Product Type
     */
    @Entity(
        name = "ProductType",
        packageName = "org.ofbiz.product.product",
        title = "Product Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "isPhysical", type = "indicator"),
            @Field(name = "isDigital", type = "indicator"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductType",
                title = "Parent",
                fkName = "PROD_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "productTypeId")
                }
            )
        }
    )
    public interface ProductTypeEntity {}

    /**
     * Product Type Attribute
     */
    @Entity(
        name = "ProductTypeAttr",
        packageName = "org.ofbiz.product.product",
        title = "Product Type Attribute",
        fields = {
            @Field(name = "productTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductType",
                fkName = "PROD_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "productTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productTypeId")
                }
            )
        }
    )
    public interface ProductTypeAttrEntity {}

    /**
     * For information related to a specific vendor and product, especially for multi-vendor stores. The ProductStoreGroup is to be used much like in ProductPrice.
     */
    @Entity(
        name = "VendorProduct",
        packageName = "org.ofbiz.product.product",
        description = "For information related to a specific vendor and product, especially for multi-vendor stores. The ProductStoreGroup is to be used much like in ProductPrice.",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "vendorPartyId", type = "id-ne"),
            @Field(name = "productStoreGroupId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "vendorPartyId"),
            @PrimaryKey(field = "productStoreGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "VENDPROD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Vendor",
                fkName = "VENDPROD_VPTY",
                keyMaps = {
                    @KeyMap(fieldName = "vendorPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                fkName = "VENDPROD_PSGRP",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            )
        }
    )
    public interface VendorProductEntity {}

    /**
     * Product Promotion
     */
    @Entity(
        name = "ProductPromo",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "promoName", type = "name"),
            @Field(name = "promoText", type = "description"),
            @Field(name = "userEntered", type = "indicator"),
            @Field(name = "showToCustomer", type = "indicator"),
            @Field(name = "requireCode", type = "indicator"),
            @Field(name = "useLimitPerOrder", type = "numeric"),
            @Field(name = "useLimitPerCustomer", type = "numeric"),
            @Field(name = "useLimitPerPromotion", type = "numeric"),
            @Field(name = "billbackFactor", type = "fixed-point"),
            @Field(name = "overrideOrgPartyId", type = "id"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PROD_PRMO_OPA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideOrgPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PROD_PRMO_CUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PROD_PRMO_LMCUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ProductPromoEntity {}

    /**
     * Product Promotion Action
     */
    @Entity(
        name = "ProductPromoAction",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Action",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoRuleId", type = "id-ne"),
            @Field(name = "productPromoActionSeqId", type = "id-ne"),
            @Field(name = "productPromoActionEnumId", type = "id-ne"),
            @Field(name = "orderAdjustmentTypeId", type = "id"),
            @Field(name = "serviceName", type = "long-varchar"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "amount", type = "fixed-point"),
            @Field(name = "productId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "useCartQuantity", type = "indicator"),
            @Field(name = "distributeAmount", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "productPromoRuleId"),
            @PrimaryKey(field = "productPromoActionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Action",
                fkName = "PROD_PRACT_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoActionEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRACT_PR",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoRule",
                fkName = "PROD_PRACT_RL",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustmentType",
                fkName = "PROD_PRACT_OATYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductPromoCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoActionSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductPromoProduct",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoActionSeqId")
                }
            )
        }
    )
    public interface ProductPromoActionEntity {}

    /**
     * Product Promotion Category
     */
    @Entity(
        name = "ProductPromoCategory",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Category",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoRuleId", type = "id-ne"),
            @Field(name = "productPromoActionSeqId", type = "id-ne"),
            @Field(name = "productPromoCondSeqId", type = "id-ne"),
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "andGroupId", type = "id"),
            @Field(name = "productPromoApplEnumId", type = "id-ne"),
            @Field(name = "includeSubCategories", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "productPromoRuleId"),
            @PrimaryKey(field = "productPromoActionSeqId"),
            @PrimaryKey(field = "productPromoCondSeqId"),
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "andGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRCAT_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "PROD_PRCAT_PRCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Appl",
                fkName = "PROD_PRCAT_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoApplEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductPromoCategoryEntity {}

    /**
     * Product Promotion
     */
    @Entity(
        name = "ProductPromoCode",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion",
        fields = {
            @Field(name = "productPromoCodeId", type = "id-ne"),
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "userEntered", type = "indicator"),
            @Field(name = "requireEmailOrParty", type = "indicator"),
            @Field(name = "useLimitPerCode", type = "numeric"),
            @Field(name = "useLimitPerCustomer", type = "numeric"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoCodeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRCOD_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PROD_PRCOD_CUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PROD_PRCOD_LMCUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ProductPromoCodeEntity {}

    /**
     * Product Promotion Email
     */
    @Entity(
        name = "ProductPromoCodeEmail",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Email",
        fields = {
            @Field(name = "productPromoCodeId", type = "id-ne"),
            @Field(name = "emailAddress", type = "email")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoCodeId"),
            @PrimaryKey(field = "emailAddress")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoCode",
                fkName = "PROD_PRCDE_PCD",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoCodeId")
                }
            )
        }
    )
    public interface ProductPromoCodeEmailEntity {}

    /**
     * Product Promotion Party
     */
    @Entity(
        name = "ProductPromoCodeParty",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Party",
        fields = {
            @Field(name = "productPromoCodeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoCodeId"),
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoCode",
                fkName = "PROD_PRCDP_PCD",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoCodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PROD_PRCDP_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface ProductPromoCodePartyEntity {}

    /**
     * Product Promotion Condition
     */
    @Entity(
        name = "ProductPromoCond",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Condition",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoRuleId", type = "id-ne"),
            @Field(name = "productPromoCondSeqId", type = "id-ne"),
            @Field(name = "inputParamEnumId", type = "id"),
            @Field(name = "operatorEnumId", type = "id"),
            @Field(name = "condValue", type = "long-varchar"),
            @Field(name = "otherValue", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "productPromoRuleId"),
            @PrimaryKey(field = "productPromoCondSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRCOND_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoRule",
                fkName = "PROD_PRCOND_RULE",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "InputParam",
                fkName = "PROD_PRCOND_INENUM",
                keyMaps = {
                    @KeyMap(fieldName = "inputParamEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Operator",
                fkName = "PROD_PRCOND_OPENUM",
                keyMaps = {
                    @KeyMap(fieldName = "operatorEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductPromoCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoCondSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductPromoProduct",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoCondSeqId")
                }
            )
        }
    )
    public interface ProductPromoCondEntity {}

    /**
     * Product Promotion Category
     */
    @Entity(
        name = "ProductPromoProduct",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Category",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoRuleId", type = "id-ne"),
            @Field(name = "productPromoActionSeqId", type = "id-ne"),
            @Field(name = "productPromoCondSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "productPromoApplEnumId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "productPromoRuleId"),
            @PrimaryKey(field = "productPromoActionSeqId"),
            @PrimaryKey(field = "productPromoCondSeqId"),
            @PrimaryKey(field = "productId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRPRD_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_PRPRD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Appl",
                fkName = "PROD_PRPRD_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoApplEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductPromoProductEntity {}

    /**
     * Product Promotion Rule
     */
    @Entity(
        name = "ProductPromoRule",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Rule",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoRuleId", type = "id-ne"),
            @Field(name = "ruleName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "productPromoRuleId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRRLE_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            )
        }
    )
    public interface ProductPromoRuleEntity {}

    /**
     * Product Promotion Use
     */
    @Entity(
        name = "ProductPromoUse",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Use",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "promoSequenceId", type = "id-ne"),
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "productPromoCodeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "totalDiscountAmount", type = "currency-amount"),
            @Field(name = "quantityLeftInActions", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "promoSequenceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PROD_PRUSE_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoCode",
                fkName = "PROD_PRUSE_CODE",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoCodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "PROD_PRUSE_ORDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PROD_PRUSE_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PRODPRUSE_PRMPTY",
                fields = {
                    @IndexField(name = "productPromoId"),
                    @IndexField(name = "partyId")
                }
            ),
            @Index(
                name = "PRODPRUSE_PCDPTY",
                fields = {
                    @IndexField(name = "productPromoCodeId"),
                    @IndexField(name = "partyId")
                }
            )
        }
    )
    public interface ProductPromoUseEntity {}

    /**
     * Product Store
     */
    @Entity(
        name = "ProductStore",
        packageName = "org.ofbiz.product.store",
        title = "Product Store",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "primaryStoreGroupId", type = "id"),
            @Field(name = "storeName", type = "name"),
            @Field(name = "companyName", type = "name"),
            @Field(name = "title", type = "name"),
            @Field(name = "subtitle", type = "description"),
            @Field(name = "payToPartyId", type = "id", description = "Note that this corresponds with the organizationPartyId that GL transactions will be posted to."),
            @Field(name = "daysToCancelNonPay", type = "numeric"),
            @Field(name = "manualAuthIsCapture", type = "indicator"),
            @Field(name = "prorateShipping", type = "indicator"),
            @Field(name = "prorateTaxes", type = "indicator"),
            @Field(name = "viewCartOnAdd", type = "indicator"),
            @Field(name = "autoSaveCart", type = "indicator"),
            @Field(name = "autoApproveReviews", type = "indicator"),
            @Field(name = "isDemoStore", type = "indicator"),
            @Field(name = "isImmediatelyFulfilled", type = "indicator", description = "If immediately fulfilled (for physical stores, etc): don't send email notices, don't reserve inventory, and IFF inventory info isn't found on the server then don't issue inventory right away"),
            @Field(name = "inventoryFacilityId", type = "id"),
            @Field(name = "oneInventoryFacility", type = "indicator"),
            @Field(name = "checkInventory", type = "indicator"),
            @Field(name = "reserveInventory", type = "indicator"),
            @Field(name = "reserveOrderEnumId", type = "id"),
            @Field(name = "requireInventory", type = "indicator"),
            @Field(name = "balanceResOnOrderCreation", type = "indicator", description = "If set to Y, when a new sales order is created with backordered items, then reservations on the facility/product are reassigned according to the priority given by the shipBeforeDate field."),
            @Field(name = "requirementMethodEnumId", type = "id"),
            @Field(name = "orderNumberPrefix", type = "id-long"),
            @Field(name = "defaultLocaleString", type = "very-short"),
            @Field(name = "localeStrings", type = "very-long", description = "JSON list of locales supported by this store, used to filter input and UserLogin.lastLocale (SCIPIO)"),
            @Field(name = "defaultCurrencyUomId", type = "id"),
            @Field(name = "currencyUomIds", type = "very-long", description = "JSON list of currency UOMs supported by this store, used to filter input and UserLogin.lastCurrencyUom (SCIPIO)"),
            @Field(name = "defaultTimeZoneString", type = "id-long"),
            @Field(name = "defaultSalesChannelEnumId", type = "id"),
            @Field(name = "allowPassword", type = "indicator"),
            @Field(name = "defaultPassword", type = "long-varchar"),
            @Field(name = "explodeOrderItems", type = "indicator"),
            @Field(name = "checkGcBalance", type = "indicator"),
            @Field(name = "retryFailedAuths", type = "indicator"),
            @Field(name = "headerApprovedStatus", type = "id"),
            @Field(name = "itemApprovedStatus", type = "id"),
            @Field(name = "digitalItemApprovedStatus", type = "id"),
            @Field(name = "headerDeclinedStatus", type = "id"),
            @Field(name = "itemDeclinedStatus", type = "id"),
            @Field(name = "headerCancelStatus", type = "id"),
            @Field(name = "itemCancelStatus", type = "id"),
            @Field(name = "authDeclinedMessage", type = "long-varchar"),
            @Field(name = "authFraudMessage", type = "long-varchar"),
            @Field(name = "authErrorMessage", type = "long-varchar"),
            @Field(name = "visualThemeId", type = "id"),
            @Field(name = "storeCreditAccountEnumId", type = "id", description = "Specify the type (Billing Account or Financial Account) of Store Credit Account used for refund return. Default to Financial Account. \n              This field is override by ReturnHeader.billingAccountId or ReturnHeader.finAccountId, whichever is specified but if only finAccountId is specified explicitly then system will first\n              try to locate any billing account with -ve amount. If found, then amount is credit to this billing account else the amount will be credit to the financial account of the user.\n          "),
            @Field(name = "usePrimaryEmailUsername", type = "indicator"),
            @Field(name = "requireCustomerRole", type = "indicator"),
            @Field(name = "autoInvoiceDigitalItems", type = "indicator", description = "Default Y. Invoice digital items when order is placed rather than waiting for completing order items (though shipment/fulfillment)."),
            @Field(name = "reqShipAddrForDigItems", type = "indicator", description = "Default Y. Require Shipping Address for Digital Items? Note this only has an effect if there are ONLY digital goods in the cart."),
            @Field(name = "showCheckoutGiftOptions", type = "indicator"),
            @Field(name = "selectPaymentTypePerItem", type = "indicator"),
            @Field(name = "showPricesWithVatTax", type = "indicator"),
            @Field(name = "showTaxIsExempt", type = "indicator", description = "default Y; if set to N do not show isExempt checkbox for PartyTaxAuthInfo, always force to N"),
            @Field(name = "vatTaxAuthGeoId", type = "id"),
            @Field(name = "vatTaxAuthPartyId", type = "id"),
            @Field(name = "enableAutoSuggestionList", type = "indicator", description = "The auto-suggestion list is a special ShoppingList that the addSuggestionsToShoppingList service will maintain for cross-sells of ordered items."),
            @Field(name = "enableDigProdUpload", type = "indicator"),
            @Field(name = "prodSearchExcludeVariants", type = "indicator", description = "default Y; if set to Y an additional constraint will of isVariant!=Y will be added to all product searches for the store"),
            @Field(name = "digProdUploadCategoryId", type = "id"),
            @Field(name = "autoOrderCcTryExp", type = "indicator", description = "For auto-orders try other Credit Card expiration dates (if date is wrong or general failure where type not known)?"),
            @Field(name = "autoOrderCcTryOtherCards", type = "indicator", description = "For auto-orders try other Credit Cards for the customer?"),
            @Field(name = "autoOrderCcTryLaterNsf", type = "indicator", description = "For auto-orders if Credit Cards fails for NSF (Not Sufficient Funds) try again later?"),
            @Field(name = "autoOrderCcTryLaterMax", type = "numeric", description = "For auto-orders if Credit Cards fails for NSF try again how many times?"),
            @Field(name = "storeCreditValidDays", type = "numeric", description = "How many days that store credit is valid for. Null value implies no expiration."),
            @Field(name = "autoApproveInvoice", type = "indicator", description = "If Y or empty, sales invoices created from orders will be marked ready."),
            @Field(name = "autoApproveOrder", type = "indicator", description = "If N, orders will not be automatically approved when payment is authorized."),
            @Field(name = "shipIfCaptureFails", type = "indicator", description = "If N, the captureOrderPayments will cause a service error if credit card capture fails."),
            @Field(name = "setOwnerUponIssuance", type = "indicator", description = "If Y or empty, set the inventory item owner upon issuance."),
            @Field(name = "reqReturnInventoryReceive", type = "indicator", description = "Default N. This is the default value for the ReturnHeader.needsInventoryReceive field. If set to Y return will automatically go to the Received status when Accepted instead of waiting for actual receipt of the return."),
            @Field(name = "addToCartRemoveIncompat", type = "indicator", description = "Default N. If Y then on add to cart remove all products in cart with a ProductAssoc record related to or from the product and with the PRODUCT_INCOMPATABLE type."),
            @Field(name = "addToCartReplaceUpsell", type = "indicator", description = "Default N. If Y then on add to cart remove all products in cart with a ProductAssoc record related from the product and with the PRODUCT_UPGRADE type."),
            @Field(name = "splitPayPrefPerShpGrp", type = "indicator", description = "Default N. If Y then before the order is stored the OrderPaymentPreference record will be split, one for each OrderItemShipGroup."),
            @Field(name = "managedByLot", type = "indicator", description = "If Y, the preparator can choose the InventoryItem by this lotId when he makes the picklist."),
            @Field(name = "showOutOfStockProducts", type = "indicator", description = "Default Y. If N then out of stock products will not be displayed on site"),
            @Field(name = "orderDecimalQuantity", type = "indicator", description = "use to indicate if decimal quantity can be ordered for this productStore. Default value is Y"),
            @Field(name = "oldStyleSheet", type = "url", colName = "STYLE_SHEET"),
            @Field(name = "oldHeaderLogo", type = "url", colName = "HEADER_LOGO"),
            @Field(name = "oldHeaderMiddleBackground", type = "url", colName = "HEADER_MIDDLE_BACKGROUND"),
            @Field(name = "oldHeaderRightBackground", type = "url", colName = "HEADER_RIGHT_BACKGROUND")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                title = "Primary",
                fkName = "PROD_STR_PRSTRGP",
                keyMaps = {
                    @KeyMap(fieldName = "primaryStoreGroupId", relFieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PROD_STR_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ReserveOrder",
                fkName = "PROD_STR_RORDENUM",
                keyMaps = {
                    @KeyMap(fieldName = "reserveOrderEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "RequirementMethod",
                fkName = "PROD_STR_RQMTENUM",
                keyMaps = {
                    @KeyMap(fieldName = "requirementMethodEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "PROD_STR_PAYTOPTY",
                keyMaps = {
                    @KeyMap(fieldName = "payToPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "PROD_STR_CURUOM",
                keyMaps = {
                    @KeyMap(fieldName = "defaultCurrencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "DefaultSalesChannel",
                fkName = "PROD_STR_SALECHN",
                keyMaps = {
                    @KeyMap(fieldName = "defaultSalesChannelEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "HeaderApproved",
                fkName = "PROD_STR_HAPSTS",
                keyMaps = {
                    @KeyMap(fieldName = "headerApprovedStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "ItemApproved",
                fkName = "PROD_STR_IAPSTS",
                keyMaps = {
                    @KeyMap(fieldName = "itemApprovedStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "DigitalItemApproved",
                fkName = "PROD_STR_DIAPSTS",
                keyMaps = {
                    @KeyMap(fieldName = "digitalItemApprovedStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "HeaderDeclined",
                fkName = "PROD_STR_HDCSTS",
                keyMaps = {
                    @KeyMap(fieldName = "headerDeclinedStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "ItemDeclined",
                fkName = "PROD_STR_IDCSTS",
                keyMaps = {
                    @KeyMap(fieldName = "itemDeclinedStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "HeaderCancel",
                fkName = "PROD_STR_HCNSTS",
                keyMaps = {
                    @KeyMap(fieldName = "headerCancelStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "ItemCancel",
                fkName = "PROD_STR_ICNSTS",
                keyMaps = {
                    @KeyMap(fieldName = "itemCancelStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                title = "Vat",
                fkName = "PROD_STR_VATTXA",
                keyMaps = {
                    @KeyMap(fieldName = "vatTaxAuthGeoId", relFieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "vatTaxAuthPartyId", relFieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "StoreCreditAccount",
                fkName = "PROD_STR_STRCRDACT",
                keyMaps = {
                    @KeyMap(fieldName = "storeCreditAccountEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreEntity {}

    /**
     * Product Store Attribute
     */
    @Entity(
        name = "ProductStoreAttribute",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Attribute",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrType", type = "id-long-ne"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "attrName"),
            @PrimaryKey(field = "attrType")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PROD_STR_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrType")
                }
            )
        }
    )
    public interface ProductStoreAttributeEntity {}

    /**
     * Product Store Type Attribute
     */
    @Entity(
        name = "ProductStoreTypeAttr",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Type Attribute",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "attrType", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "attrType")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrType")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStore",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface ProductStoreTypeAttrEntity {}

    /**
     * Product Store Catalog Association
     */
    @Entity(
        name = "ProductStoreCatalog",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Catalog Association",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "prodCatalogId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PS_CAT_PRDSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalog",
                fkName = "PS_CAT_CATALOG",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogId")
                }
            )
        }
    )
    public interface ProductStoreCatalogEntity {}

    /**
     * Product Store Email Settings
     */
    @Entity(
        name = "ProductStoreEmailSetting",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Email Settings",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "emailType", type = "id-ne"),
            @Field(name = "bodyScreenLocation", type = "long-varchar", description = "if empty defaults to a screen based on the emailType"),
            @Field(name = "xslfoAttachScreenLocation", type = "long-varchar", description = "if specified is used to generate XSL:FO that is transformed to a PDF via Apache FOP and attached to the email"),
            @Field(name = "fromAddress", type = "email"),
            @Field(name = "sendAs", type = "short-varchar"),
            @Field(name = "ccAddress", type = "email"),
            @Field(name = "bccAddress", type = "email"),
            @Field(name = "subject", type = "comment"),
            @Field(name = "contentType", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "emailType")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTREM_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "PRDSTREM_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "emailType", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreEmailSettingEntity {}

    /**
     * Allows financial account, such as gift certificate or calling cards, to be configured at store level
     */
    @Entity(
        name = "ProductStoreFinActSetting",
        packageName = "org.ofbiz.product.store",
        title = "Allows financial account, such as gift certificate or calling cards, to be configured at store level",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "finAccountTypeId", type = "id-ne"),
            @Field(name = "requirePinCode", type = "indicator"),
            @Field(name = "validateGCFinAcct", type = "indicator", description = "determines whether the store should validate gift card numbers against the gift certificate codes stored in FinAccount.\n              Set to N if using external gift card provider."),
            @Field(name = "accountCodeLength", type = "numeric", description = "length of auto-generated account code"),
            @Field(name = "pinCodeLength", type = "numeric", description = "length of auto-generated pin code, if it is required"),
            @Field(name = "accountValidDays", type = "numeric", description = "number of days an account of this type would be valid for"),
            @Field(name = "authValidDays", type = "numeric", description = "number of days an authorization of this type would be valid for"),
            @Field(name = "purchaseSurveyId", type = "id", description = "This survey is typically used to collect information such as name of buyer, recipient, email, message, etc. and is quite flexible"),
            @Field(name = "purchSurveySendTo", type = "id", description = "Field name on the purchase survey with the send to email address"),
            @Field(name = "purchSurveyCopyMe", type = "id", description = "Whether the BCC on ProductStoreEmailSetting should be copied for email notifications"),
            @Field(name = "allowAuthToNegative", type = "indicator"),
            @Field(name = "minBalance", type = "currency-amount"),
            @Field(name = "replenishThreshold", type = "currency-amount"),
            @Field(name = "replenishMethodEnumId", type = "id", description = "Replenish Method for Replenish Account. Can be FARP_TOP_OFF or FARP_REPLENISH_LEVEL. Default FARP_TOP_OFF.")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "finAccountTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRSTFNAC_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountType",
                fkName = "PRSTFNAC_FNACTP",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "PRSTFNAC_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "purchaseSurveyId", relFieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ReplenishMethod",
                fkName = "PRSTFNAC_FARPMTD",
                keyMaps = {
                    @KeyMap(fieldName = "replenishMethodEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreFinActSettingEntity {}

    /**
     * Product Store Inventory Facility Applicability
     */
    @Entity(
        name = "ProductStoreFacility",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Inventory Facility Applicability",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRFAC_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PRDSTRFAC_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ProductStoreFacilityEntity {}

    /**
     * ProductStore Group
     */
    @Entity(
        name = "ProductStoreGroup",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore Group",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "productStoreGroupId", type = "id-ne"),
            @Field(name = "productStoreGroupTypeId", type = "id"),
            @Field(name = "primaryParentGroupId", type = "id"),
            @Field(name = "productStoreGroupName", type = "name"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreGroupId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroupType",
                fkName = "PRDSTR_GP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                title = "PrimaryParent",
                fkName = "PRDSTR_GP_PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "primaryParentGroupId", relFieldName = "productStoreGroupId")
                }
            )
        }
    )
    public interface ProductStoreGroupEntity {}

    /**
     * ProductStore Group Member
     */
    @Entity(
        name = "ProductStoreGroupMember",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore Group Member",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "productStoreGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "productStoreGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTR_MEM_PRDSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                fkName = "PRDSTR_MEM_PSGRP",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            )
        }
    )
    public interface ProductStoreGroupMemberEntity {}

    /**
     * ProductStore Group Role
     */
    @Entity(
        name = "ProductStoreGroupRole",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore Group Role",
        fields = {
            @Field(name = "productStoreGroupId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreGroupId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                fkName = "PSGRP_RLE_PSGP",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "PSGRP_RLE_PTRLE",
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
    public interface ProductStoreGroupRoleEntity {}

    /**
     * ProductStore Group Rollup
     */
    @Entity(
        name = "ProductStoreGroupRollup",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore Group Rollup",
        fields = {
            @Field(name = "productStoreGroupId", type = "id-ne"),
            @Field(name = "parentGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreGroupId"),
            @PrimaryKey(field = "parentGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                title = "Current",
                fkName = "PSGRP_RLP_CURRENT",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreGroup",
                title = "Parent",
                fkName = "PSGRP_RLP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentGroupId", relFieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreGroupRollup",
                title = "Child",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId", relFieldName = "parentGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreGroupRollup",
                title = "Parent",
                keyMaps = {
                    @KeyMap(fieldName = "parentGroupId", relFieldName = "productStoreGroupId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreGroupRollup",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentGroupId")
                }
            )
        }
    )
    public interface ProductStoreGroupRollupEntity {}

    /**
     * ProductStore Group Type
     */
    @Entity(
        name = "ProductStoreGroupType",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore Group Type",
        fields = {
            @Field(name = "productStoreGroupTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreGroupTypeId")
        }
    )
    public interface ProductStoreGroupTypeEntity {}

    /**
     * Product Store Inventory Facility Applicability
     */
    @Entity(
        name = "ProductStoreKeywordOvrd",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Inventory Facility Applicability",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "keyword", type = "short-varchar"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "target", type = "long-varchar"),
            @Field(name = "targetTypeEnumId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "keyword"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRKWO_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "PRDSTRKWO_ENM",
                keyMaps = {
                    @KeyMap(fieldName = "targetTypeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreKeywordOvrdEntity {}

    /**
     * Product Store Payment Settings
     */
    @Entity(
        name = "ProductStorePaymentSetting",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Payment Settings",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "paymentServiceTypeEnumId", type = "id-ne"),
            @Field(name = "paymentService", type = "value"),
            @Field(name = "paymentCustomMethodId", type = "id"),
            @Field(name = "paymentGatewayConfigId", type = "id"),
            @Field(name = "paymentPropertiesPath", type = "value"),
            @Field(name = "applyToAllProducts", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "paymentMethodTypeId"),
            @PrimaryKey(field = "paymentServiceTypeEnumId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDS_PS_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PRDS_PS_PMNTTP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "PRDS_PS_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "paymentServiceTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentGatewayConfig",
                fkName = "PRDS_PS_PGC",
                keyMaps = {
                    @KeyMap(fieldName = "paymentGatewayConfigId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "PRDS_PS_CUS_MET",
                keyMaps = {
                    @KeyMap(fieldName = "paymentCustomMethodId", relFieldName = "customMethodId")
                }
            )
        }
    )
    public interface ProductStorePaymentSettingEntity {}

    /**
     * Product Store Promotion Applicability
     */
    @Entity(
        name = "ProductStorePromoAppl",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Promotion Applicability",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "manualOnly", type = "indicator", description = "\n              If set to Y then the promotion is not automatically evaluated, but only if it\n              is manually added to the cart.\n          ")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRPRMO_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PRDSTRPRMO_PRMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            )
        }
    )
    public interface ProductStorePromoApplEntity {}

    /**
     * Product Store Role Association
     */
    @Entity(
        name = "ProductStoreRole",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Role Association",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "productStoreId"),
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
                fkName = "PRDSTRRLE_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRRLE_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface ProductStoreRoleEntity {}

    /**
     * Product Store Carrier Shipment Method
     */
    @Entity(
        name = "ProductStoreShipmentMeth",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Carrier Shipment Method",
        fields = {
            @Field(name = "productStoreShipMethId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "companyPartyId", type = "id"),
            @Field(name = "minWeight", type = "fixed-point"),
            @Field(name = "maxWeight", type = "fixed-point"),
            @Field(name = "minSize", type = "fixed-point"),
            @Field(name = "maxSize", type = "fixed-point"),
            @Field(name = "minTotal", type = "currency-amount"),
            @Field(name = "maxTotal", type = "currency-amount"),
            @Field(name = "allowUspsAddr", type = "indicator"),
            @Field(name = "requireUspsAddr", type = "indicator"),
            @Field(name = "allowCompanyAddr", type = "indicator"),
            @Field(name = "requireCompanyAddr", type = "indicator"),
            @Field(name = "includeNoChargeItems", type = "indicator"),
            @Field(name = "includeFeatureGroup", type = "id"),
            @Field(name = "excludeFeatureGroup", type = "id"),
            @Field(name = "includeGeoId", type = "id"),
            @Field(name = "excludeGeoId", type = "id"),
            @Field(name = "serviceName", type = "long-varchar"),
            @Field(name = "configProps", type = "long-varchar"),
            @Field(name = "shipmentCustomMethodId", type = "id"),
            @Field(name = "shipmentGatewayConfigId", type = "id"),
            @Field(name = "sequenceNumber", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreShipMethId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "companyPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                title = "Include",
                keyMaps = {
                    @KeyMap(fieldName = "includeGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                title = "Exclude",
                keyMaps = {
                    @KeyMap(fieldName = "excludeGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfig",
                fkName = "PRDS_SM_SGC",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfigId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "PRDS_SM_CUS_MET",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentCustomMethodId", relFieldName = "customMethodId")
                }
            )
        }
    )
    public interface ProductStoreShipmentMethEntity {}

    /**
     * Product Store Survey Application
     */
    @Entity(
        name = "ProductStoreSurveyAppl",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Survey Application",
        fields = {
            @Field(name = "productStoreSurveyId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "surveyApplTypeId", type = "id-ne"),
            @Field(name = "groupName", type = "name"),
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productCategoryId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "surveyTemplate", type = "long-varchar"),
            @Field(name = "resultTemplate", type = "long-varchar"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreSurveyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRSVY_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "PRDSTRSVY_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyApplType",
                fkName = "PRDSTRSVY_SATP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyApplTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface ProductStoreSurveyApplEntity {}

    /**
     * Product Store Vendor Payment
     * Used to define payments that a vendor related to the store will accept (for multi-vendor stores)
     */
    @Entity(
        name = "ProductStoreVendorPayment",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Vendor Payment",
        description = "Used to define payments that a vendor related to the store will accept (for multi-vendor stores)",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "vendorPartyId", type = "id-ne"),
            @Field(name = "paymentMethodTypeId", type = "id-ne"),
            @Field(name = "creditCardEnumId", type = "id-ne", description = "If not applicable for the paymentMethodTypeId, use \"_NA_\"")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "vendorPartyId"),
            @PrimaryKey(field = "paymentMethodTypeId"),
            @PrimaryKey(field = "creditCardEnumId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRVPM_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Vendor",
                fkName = "PRDSTRVPM_VPTY",
                keyMaps = {
                    @KeyMap(fieldName = "vendorPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "PRDSTRVPM_PMMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "CreditCard",
                fkName = "PRDSTRVPM_CCEN",
                keyMaps = {
                    @KeyMap(fieldName = "creditCardEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreVendorPaymentEntity {}

    /**
     * Product Store Vendor Shipment
     * Used to define Carrier-ShipmentMethod combinations that a vendor related to the store will accept (for multi-vendor stores)
     */
    @Entity(
        name = "ProductStoreVendorShipment",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Vendor Shipment",
        description = "Used to define Carrier-ShipmentMethod combinations that a vendor related to the store will accept (for multi-vendor stores)",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "vendorPartyId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id-ne"),
            @Field(name = "carrierPartyId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "vendorPartyId"),
            @PrimaryKey(field = "shipmentMethodTypeId"),
            @PrimaryKey(field = "carrierPartyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRVSH_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Vendor",
                fkName = "PRDSTRVSH_VPTY",
                keyMaps = {
                    @KeyMap(fieldName = "vendorPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "PRDSTRVSH_SHMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Carrier",
                fkName = "PRDSTRVSH_CPTY",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface ProductStoreVendorShipmentEntity {}

    /**
     * Product Store Marketplace
     * Defines (external) marketplace linked to a ProductStore
     */
    @Entity(
        name = "ProductStoreMarketplace",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Marketplace",
        description = "Defines (external) marketplace linked to a ProductStore",
        fields = {
            @Field(name = "marketplaceId", type = "id-ne"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "orderFulfillmentChannelEnumId", type = "id-ne", description = "Represents in which way logistics are taken care of (marketplace or store)"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "marketplaceId"),
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "orderFulfillmentChannelEnumId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "PRDSTRMKTPLC_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "OrderFulfillmentChannel",
                fkName = "PRDSTRMKTPLC_OFC_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "orderFulfillmentChannelEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ProductStoreMarketplaceEntity {}

    /**
     * Product Subscription Resource
     */
    @Entity(
        name = "ProductSubscriptionResource",
        packageName = "org.ofbiz.product.subscription",
        title = "Product Subscription Resource",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "subscriptionResourceId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "purchaseFromDate", type = "date-time"),
            @Field(name = "purchaseThruDate", type = "date-time"),
            @Field(name = "maxLifeTime", type = "numeric", description = "The length in time of the subscription"),
            @Field(name = "maxLifeTimeUomId", type = "id"),
            @Field(name = "availableTime", type = "numeric"),
            @Field(name = "availableTimeUomId", type = "id"),
            @Field(name = "useCountLimit", type = "numeric"),
            @Field(name = "useTime", type = "numeric", description = "The length of time this subscription can be used"),
            @Field(name = "useTimeUomId", type = "id"),
            @Field(name = "useRoleTypeId", type = "id"),
            @Field(name = "automaticExtend", type = "indicator", description = "If this subscription is automatically extended with the same period as the initial period."),
            @Field(name = "canclAutmExtTime", type = "numeric", description = "The time period (before the end of the thruDate) after which the automatic extension of the subscription will be executed."),
            @Field(name = "canclAutmExtTimeUomId", type = "id", description = "Unit Of Measure used for the automatic extension of the subscription."),
            @Field(name = "gracePeriodOnExpiry", type = "numeric", description = "The time period (after the end of the thruDate) after which the subscription will be expired."),
            @Field(name = "gracePeriodOnExpiryUomId", type = "id", description = "Unit Of Measure used for the grace period of the subscription.")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "subscriptionResourceId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_SBRS_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionResource",
                fkName = "PROD_SBRS_SBRS",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "Use",
                fkName = "PROD_SBRS_URT",
                keyMaps = {
                    @KeyMap(fieldName = "useRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "UseTime",
                fkName = "PROD_SBRS_UTU",
                keyMaps = {
                    @KeyMap(fieldName = "useTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "CancelTime",
                fkName = "PROD_SBRS_CTU",
                keyMaps = {
                    @KeyMap(fieldName = "canclAutmExtTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "AvailableTime",
                fkName = "PROD_SBRS_ATU",
                keyMaps = {
                    @KeyMap(fieldName = "availableTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "MaxLifeTime",
                fkName = "PROD_SBRS_MTU",
                keyMaps = {
                    @KeyMap(fieldName = "maxLifeTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "GracePeriod",
                fkName = "PROD_SBRS_GTU",
                keyMaps = {
                    @KeyMap(fieldName = "gracePeriodOnExpiryUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ProductSubscriptionResourceEntity {}

    /**
     * Subscription
     */
    @Entity(
        name = "Subscription",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription",
        fields = {
            @Field(name = "subscriptionId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "subscriptionResourceId", type = "id"),
            @Field(name = "communicationEventId", type = "id", description = "now replaced by entity: SubscriptionCommEvent"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "originatedFromPartyId", type = "id"),
            @Field(name = "originatedFromRoleTypeId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "partyNeedId", type = "id"),
            @Field(name = "needTypeId", type = "id"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productCategoryId", type = "id"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "subscriptionTypeId", type = "id"),
            @Field(name = "externalSubscriptionId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "purchaseFromDate", type = "date-time"),
            @Field(name = "purchaseThruDate", type = "date-time"),
            @Field(name = "maxLifeTime", type = "numeric", description = "The length in time of the (extended) subscription"),
            @Field(name = "maxLifeTimeUomId", type = "id"),
            @Field(name = "availableTime", type = "numeric"),
            @Field(name = "availableTimeUomId", type = "id"),
            @Field(name = "useCountLimit", type = "numeric"),
            @Field(name = "useTime", type = "numeric"),
            @Field(name = "useTimeUomId", type = "id"),
            @Field(name = "automaticExtend", type = "indicator", description = "If this subscription is automatically extended with the same period as the initial period."),
            @Field(name = "canclAutmExtTime", type = "numeric", description = "The time period (before the end of the thruDate) after which the automatic extension of the subscription will be executed."),
            @Field(name = "canclAutmExtTimeUomId", type = "id", description = "Unit Of Measure used for the automatic extension of the subscription."),
            @Field(name = "gracePeriodOnExpiry", type = "numeric", description = "The time period (before the end of the thruDate) after which the automatic extension of the subscription will be executed."),
            @Field(name = "gracePeriodOnExpiryUomId", type = "id", description = "Unit Of Measure used for the automatic extension of the subscription."),
            @Field(name = "expirationCompletedDate", type = "date-time", description = "The date when expiration completed.")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionResource",
                fkName = "SUBSC_SRESRC",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "SUBSC_CONT_MECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SUBSC_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "UseTime",
                fkName = "SUBSC_UTU",
                keyMaps = {
                    @KeyMap(fieldName = "useTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "CancelTime",
                fkName = "SUBSC_CTU",
                keyMaps = {
                    @KeyMap(fieldName = "canclAutmExtTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "AvailableTime",
                fkName = "SUBSC_ATU",
                keyMaps = {
                    @KeyMap(fieldName = "availableTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "MaxLifeTime",
                fkName = "SUBSC_MTU",
                keyMaps = {
                    @KeyMap(fieldName = "maxLifeTimeUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "SUBSC_ROLE_TYPE",
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
                relEntityName = "Party",
                title = "OriginatedFrom",
                fkName = "SUBSC_OPARTY",
                keyMaps = {
                    @KeyMap(fieldName = "originatedFromPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                title = "OriginatedFrom",
                fkName = "SUBSC_OROLE_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "originatedFromRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                title = "OriginatedFrom",
                keyMaps = {
                    @KeyMap(fieldName = "originatedFromPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "originatedFromRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyNeed",
                keyMaps = {
                    @KeyMap(fieldName = "partyNeedId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NeedType",
                fkName = "SUBSC_NEED_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "needTypeId")
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
                relEntityName = "OrderItem",
                fkName = "SUBSC_ORDERITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SUBSC_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "SUBSC_PROD_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "SUBSC_INV_ITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionType",
                fkName = "SUBSC_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SubscriptionTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "GracePeriod",
                fkName = "SUBSC_GTU",
                keyMaps = {
                    @KeyMap(fieldName = "gracePeriodOnExpiryUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface SubscriptionEntity {}

    /**
     * Subscription Activity
     */
    @Entity(
        name = "SubscriptionActivity",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Activity",
        fields = {
            @Field(name = "subscriptionActivityId", type = "id-ne"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "dateSent", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionActivityId")
        }
    )
    public interface SubscriptionActivityEntity {}

    /**
     * Subscription Attribute
     */
    @Entity(
        name = "SubscriptionAttribute",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Attribute",
        fields = {
            @Field(name = "subscriptionId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Subscription",
                fkName = "SUBSC_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SubscriptionTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface SubscriptionAttributeEntity {}

    /**
     * Subscription Fulfillment Piece
     */
    @Entity(
        name = "SubscriptionFulfillmentPiece",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Fulfillment Piece",
        fields = {
            @Field(name = "subscriptionActivityId", type = "id-ne"),
            @Field(name = "subscriptionId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionActivityId"),
            @PrimaryKey(field = "subscriptionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Subscription",
                fkName = "SUBSC_FP",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionActivity",
                fkName = "SUBSC_FP_ACT",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionActivityId")
                }
            )
        }
    )
    public interface SubscriptionFulfillmentPieceEntity {}

    /**
     * Subscription Resource
     */
    @Entity(
        name = "SubscriptionResource",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Resource",
        fields = {
            @Field(name = "subscriptionResourceId", type = "id-ne"),
            @Field(name = "parentResourceId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "contentId", type = "id", description = "Optional (use if applicable) ID of a Content record that this would represent a subscription to."),
            @Field(name = "webSiteId", type = "id", description = "Optional (use if applicable) ID of a WebSite record that this would represent a subscription to."),
            @Field(name = "serviceNameOnExpiry", type = "long-varchar", description = "Name of service which will run on subscription expiration.")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionResource",
                title = "Parent",
                fkName = "SUBSC_RES_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentResourceId", relFieldName = "subscriptionResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "SUBSC_RES_CNTNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "SUBSC_RES_WBSITE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            )
        }
    )
    public interface SubscriptionResourceEntity {}

    /**
     * Subscription Type
     */
    @Entity(
        name = "SubscriptionType",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "subscriptionTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionType",
                title = "Parent",
                fkName = "SUBSC_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "subscriptionTypeId")
                }
            )
        }
    )
    public interface SubscriptionTypeEntity {}

    /**
     * Subscription Type Attribute
     */
    @Entity(
        name = "SubscriptionTypeAttr",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Type Attribute",
        fields = {
            @Field(name = "subscriptionTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SubscriptionType",
                fkName = "SUBSC_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SubscriptionAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Subscription",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionTypeId")
                }
            )
        }
    )
    public interface SubscriptionTypeAttrEntity {}

    /**
     * Subscription Communication Event 
     */
    @Entity(
        name = "SubscriptionCommEvent",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription Communication Event ",
        fields = {
            @Field(name = "subscriptionId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "subscriptionId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "SUBSC_COM_EVENT",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Subscription",
                fkName = "SUBSC_SUBSC",
                keyMaps = {
                    @KeyMap(fieldName = "subscriptionId")
                }
            )
        }
    )
    public interface SubscriptionCommEventEntity {}

    /**
     * Market Interest
     */
    @Entity(
        name = "MarketInterest",
        packageName = "org.ofbiz.product.supplier",
        title = "Market Interest",
        fields = {
            @Field(name = "productCategoryId", type = "id-ne"),
            @Field(name = "partyClassificationGroupId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productCategoryId"),
            @PrimaryKey(field = "partyClassificationGroupId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductCategory",
                fkName = "MARKET_INT_PCAT",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyClassificationGroup",
                fkName = "MARKET_INT_PCGRP",
                keyMaps = {
                    @KeyMap(fieldName = "partyClassificationGroupId")
                }
            )
        }
    )
    public interface MarketInterestEntity {}

    /**
     * Reorder Guideline
     */
    @Entity(
        name = "ReorderGuideline",
        packageName = "org.ofbiz.product.supplier",
        title = "Reorder Guideline",
        fields = {
            @Field(name = "reorderGuidelineId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "geoId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "reorderQuantity", type = "fixed-point"),
            @Field(name = "reorderLevel", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "reorderGuidelineId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "REORDER_GD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "REORDER_GD_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "REORDER_GD_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "REORDER_GD_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface ReorderGuidelineEntity {}

    /**
     * Preference Type
     */
    @Entity(
        name = "SupplierPrefOrder",
        packageName = "org.ofbiz.product.supplier",
        title = "Preference Type",
        fields = {
            @Field(name = "supplierPrefOrderId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "supplierPrefOrderId")
        }
    )
    public interface SupplierPrefOrderEntity {}

    /**
     * Supplier Product
     */
    @Entity(
        name = "SupplierProduct",
        packageName = "org.ofbiz.product.supplier",
        title = "Supplier Product",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "availableFromDate", type = "date-time"),
            @Field(name = "availableThruDate", type = "date-time"),
            @Field(name = "supplierPrefOrderId", type = "id"),
            @Field(name = "supplierRatingTypeId", type = "id"),
            @Field(name = "standardLeadTimeDays", type = "fixed-point"),
            @Field(name = "minimumOrderQuantity", type = "fixed-point"),
            @Field(name = "orderQtyIncrements", type = "fixed-point"),
            @Field(name = "unitsIncluded", type = "fixed-point"),
            @Field(name = "quantityUomId", type = "id"),
            @Field(name = "agreementId", type = "id"),
            @Field(name = "agreementItemSeqId", type = "id"),
            @Field(name = "lastPrice", type = "currency-precise"),
            @Field(name = "shippingPrice", type = "currency-precise"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "supplierProductName", type = "name"),
            @Field(name = "supplierProductId", type = "id"),
            @Field(name = "canDropShip", type = "indicator"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "currencyUomId"),
            @PrimaryKey(field = "minimumOrderQuantity"),
            @PrimaryKey(field = "availableFromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SUPPL_PROD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SUPPL_PROD_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SupplierPrefOrder",
                fkName = "SUPPL_PROD_SPORD",
                keyMaps = {
                    @KeyMap(fieldName = "supplierPrefOrderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SupplierRatingType",
                fkName = "SUPPL_PROD_SRTPE",
                keyMaps = {
                    @KeyMap(fieldName = "supplierRatingTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "SUPPL_PROD_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Quantity",
                fkName = "SUPPL_PROD_QUOM",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "AgreementItem",
                fkName = "SUPPL_PROD_AGRIT",
                keyMaps = {
                    @KeyMap(fieldName = "agreementId"),
                    @KeyMap(fieldName = "agreementItemSeqId")
                }
            )
        }
    )
    public interface SupplierProductEntity {}

    /**
     * Supplier-specific product feature information
     */
    @Entity(
        name = "SupplierProductFeature",
        packageName = "org.ofbiz.product.supplier",
        title = "Supplier-specific product feature information",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne"),
            @Field(name = "description", type = "name"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "idCode", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "productFeatureId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SUPPL_FEAT_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "SUPPL_FEAT_FEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "SUPPL_FEAT_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            )
        }
    )
    public interface SupplierProductFeatureEntity {}

    /**
     * Supplier Rating Type
     */
    @Entity(
        name = "SupplierRatingType",
        packageName = "org.ofbiz.product.supplier",
        title = "Supplier Rating Type",
        fields = {
            @Field(name = "supplierRatingTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "supplierRatingTypeId")
        }
    )
    public interface SupplierRatingTypeEntity {}

    /**
     * Product Promo Content
     */
    @Entity(
        name = "ProductPromoContent",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promo Content",
        fields = {
            @Field(name = "productPromoId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "productPromoContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productPromoId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "productPromoContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "PRODPR_CNT_PROD_PR",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PRODPR_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductContentType",
                fkName = "PRODPR_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoContentTypeId", relFieldName = "productContentTypeId")
                }
            )
        }
    )
    public interface ProductPromoContentEntity {}

    /**
     * Product Group Order
     */
    @Entity(
        name = "ProductGroupOrder",
        packageName = "org.ofbiz.product.product",
        title = "Product Group Order",
        fields = {
            @Field(name = "groupOrderId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "reqOrderQty", type = "fixed-point"),
            @Field(name = "soldOrderQty", type = "fixed-point"),
            @Field(name = "jobId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "groupOrderId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_GROUP_ORDER",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "GROUP_ORDER_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "JobSandbox",
                fkName = "GROUP_ORDER_JOB",
                keyMaps = {
                    @KeyMap(fieldName = "jobId")
                }
            )
        }
    )
    public interface ProductGroupOrderEntity {}

    /**
     * Product And ProductCategoryMember View
     */
    @ViewEntity(
        name = "ProductAndCategoryMember",
        packageName = "org.ofbiz.product.category",
        title = "Product And ProductCategoryMember View",
        members = {
            @MemberEntity(entityAlias = "PROD", entityName = "Product"),
            @MemberEntity(entityAlias = "PCM", entityName = "ProductCategoryMember")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PROD"),
            @AliasAll(entityAlias = "PCM", excludes = {"comments"})
        },
        aliases = {
            @Alias(name = "memberComments", entityAlias = "PCM", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PROD",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryMember",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId"),
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductAndCategoryMemberView {}

    /**
     * ProductCategory And ProductCategoryMember View
     */
    @ViewEntity(
        name = "ProductCategoryAndMember",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategory And ProductCategoryMember View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductCategory"),
            @MemberEntity(entityAlias = "PCM", entityName = "ProductCategoryMember")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "PCM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryMember",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId"),
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductCategoryAndMemberView {}

    /**
     * ProductCategoryContent, Content and DataResource View
     */
    @ViewEntity(
        name = "ProductCategoryContentAndInfo",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryContent, Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "PCC", entityName = "ProductCategoryContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCC",
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
    public interface ProductCategoryContentAndInfoView {}

    /**
     * ProductCategoryContent and ElectronicText Required View
     */
    @ViewEntity(
        name = "ProductCategoryContentAndElectronicText",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryContent and ElectronicText Required View",
        members = {
            @MemberEntity(entityAlias = "PCC", entityName = "ProductCategoryContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr"),
            @AliasAll(entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCC",
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
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductCategoryContentAndElectronicTextView {}

    /**
     * ProductCategoryContent and ElectronicText Required Short View
     */
    @ViewEntity(
        name = "ProductCategoryContentAndElecTextShort",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryContent and ElectronicText Required Short View",
        members = {
            @MemberEntity(entityAlias = "PCC", entityName = "ProductCategoryContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCC",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductCategoryContentAndElecTextShortView {}

    /**
     * ProductCategoryContent and ContentAssoc and ElectronicText Required Short View
     */
    @ViewEntity(
        name = "ProductCategoryContentAssocAndElecTextShort",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryContent and ContentAssoc and ElectronicText Required Short View",
        members = {
            @MemberEntity(entityAlias = "PCC", entityName = "ProductCategoryContent"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "TOCO", entityName = "Content"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliases = {
            @Alias(name = "productCategoryId", entityAlias = "PCC"),
            @Alias(name = "prodCatContentTypeId", entityAlias = "PCC"),
            @Alias(name = "fromDate", entityAlias = "PCC", field = "fromDate"),
            @Alias(name = "thruDate", entityAlias = "PCC", field = "thruDate"),
            @Alias(name = "contentAssocTypeId", entityAlias = "CA"),
            @Alias(name = "contentId", entityAlias = "CA"),
            @Alias(name = "contentIdTo", entityAlias = "CA"),
            @Alias(name = "caFromDate", entityAlias = "CA", field = "fromDate"),
            @Alias(name = "caThruDate", entityAlias = "CA", field = "thruDate"),
            @Alias(name = "localeString", entityAlias = "TOCO"),
            @Alias(name = "dataResourceId", entityAlias = "TOCO"),
            @Alias(name = "textData", entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCC",
                relEntityAlias = "CA",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "TOCO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "TOCO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductCategoryContentAssocAndElecTextShortView {}

    /**
     * ProductCategoryMember And ProductCategoryRole View
     */
    @ViewEntity(
        name = "ProductCategoryMemberAndRole",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryMember And ProductCategoryRole View",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "ProductCategoryMember"),
            @MemberEntity(entityAlias = "PCR", entityName = "ProductCategoryRole")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PCM"),
            @Alias(name = "productCategoryId", entityAlias = "PCM"),
            @Alias(name = "fromDate", entityAlias = "PCM"),
            @Alias(name = "thruDate", entityAlias = "PCM"),
            @Alias(name = "comments", entityAlias = "PCM"),
            @Alias(name = "sequenceNum", entityAlias = "PCM"),
            @Alias(name = "quantity", entityAlias = "PCM"),
            @Alias(name = "partyId", entityAlias = "PCR"),
            @Alias(name = "roleTypeId", entityAlias = "PCR"),
            @Alias(name = "roleFromDate", entityAlias = "PCR", field = "fromDate"),
            @Alias(name = "roleThruDate", entityAlias = "PCR", field = "thruDate"),
            @Alias(name = "roleComments", entityAlias = "PCR", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PCR",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryMember",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId"),
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryRole",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "roleFromDate", relFieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductCategoryMemberAndRoleView {}

    @ViewEntity(
        name = "ProductCategoryRollupAndChild",
        packageName = "org.ofbiz.product.category",
        members = {
            @MemberEntity(entityAlias = "PCR", entityName = "ProductCategoryRollup"),
            @MemberEntity(entityAlias = "CPC", entityName = "ProductCategory")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CPC")
        },
        aliases = {
            @Alias(name = "parentProductCategoryId", entityAlias = "PCR"),
            @Alias(name = "fromDate", entityAlias = "PCR"),
            @Alias(name = "thruDate", entityAlias = "PCR"),
            @Alias(name = "sequenceNum", entityAlias = "PCR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCR",
                relEntityAlias = "CPC",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryRollupAndChildView {}

    /**
     * ProductCategoryRollup And ProductCategoryRole View
     * Allow the lookup of a category that is in another category that a party/role is related to. So, the party/role is related to the parent category.
     */
    @ViewEntity(
        name = "ProductCategoryRollupAndRole",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryRollup And ProductCategoryRole View",
        description = "Allow the lookup of a category that is in another category that a party/role is related to. So, the party/role is related to the parent category.",
        members = {
            @MemberEntity(entityAlias = "PCRLP", entityName = "ProductCategoryRollup"),
            @MemberEntity(entityAlias = "PCR", entityName = "ProductCategoryRole")
        },
        aliases = {
            @Alias(name = "productCategoryId", entityAlias = "PCRLP"),
            @Alias(name = "parentProductCategoryId", entityAlias = "PCRLP"),
            @Alias(name = "fromDate", entityAlias = "PCRLP"),
            @Alias(name = "thruDate", entityAlias = "PCRLP"),
            @Alias(name = "sequenceNum", entityAlias = "PCRLP"),
            @Alias(name = "partyId", entityAlias = "PCR"),
            @Alias(name = "roleTypeId", entityAlias = "PCR"),
            @Alias(name = "roleFromDate", entityAlias = "PCR", field = "fromDate"),
            @Alias(name = "roleThruDate", entityAlias = "PCR", field = "thruDate"),
            @Alias(name = "roleComments", entityAlias = "PCR", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCRLP",
                relEntityAlias = "PCR",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId", relFieldName = "productCategoryId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryRollup",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId"),
                    @KeyMap(fieldName = "parentProductCategoryId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategoryRole",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId", relFieldName = "productCategoryId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "roleFromDate", relFieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                title = "Parent",
                keyMaps = {
                    @KeyMap(fieldName = "parentProductCategoryId", relFieldName = "productCategoryId")
                }
            )
        }
    )
    public interface ProductCategoryRollupAndRoleView {}

    /**
     * Product Config And Product  View Entity, to be able to see which products use a certain configuration item
     */
    @ViewEntity(
        name = "ProductConfigAndProduct",
        packageName = "org.ofbiz.product.config",
        title = "Product Config And Product  View Entity, to be able to see which products use a certain configuration item",
        members = {
            @MemberEntity(entityAlias = "PDC", entityName = "ProductConfig"),
            @MemberEntity(entityAlias = "PD", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PDC", excludes = {"description", "longDescription"}),
            @AliasAll(entityAlias = "PD")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PDC",
                relEntityAlias = "PD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductConfigAndProductView {}

    /**
     * Product Config and Product Config Product
     */
    @ViewEntity(
        name = "ProductConfigAndConfigProduct",
        packageName = "org.ofbiz.product.config",
        title = "Product Config and Product Config Product",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductConfig"),
            @MemberEntity(entityAlias = "PCP", entityName = "ProductConfigProduct")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "PCP", prefix = "config")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "PCP",
                keyMaps = {
                    @KeyMap(fieldName = "configItemId")
                }
            )
        }
    )
    public interface ProductConfigAndConfigProductView {}

    /**
     * Container and Geo Point View
     */
    @ViewEntity(
        name = "ContainerAndGeoPoint",
        packageName = "org.ofbiz.product.facility",
        title = "Container and Geo Point View",
        members = {
            @MemberEntity(entityAlias = "CT", entityName = "Container"),
            @MemberEntity(entityAlias = "CTGPT", entityName = "ContainerGeoPoint"),
            @MemberEntity(entityAlias = "GPT", entityName = "GeoPoint")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GPT")
        },
        aliases = {
            @Alias(name = "containerId", entityAlias = "CT"),
            @Alias(name = "fromDate", entityAlias = "CTGPT"),
            @Alias(name = "thruDate", entityAlias = "CTGPT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CT",
                relEntityAlias = "CTGPT",
                keyMaps = {
                    @KeyMap(fieldName = "containerId")
                }
            ),
            @ViewLink(
                entityAlias = "CTGPT",
                relEntityAlias = "GPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContainerGeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "containerId"),
                    @KeyMap(fieldName = "geoPointId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Container",
                keyMaps = {
                    @KeyMap(fieldName = "containerId")
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
    public interface ContainerAndGeoPointView {}

    /**
     * Facility Contact Mech and Contact Mech View
     */
    @ViewEntity(
        name = "FacilityContactMechAndContactMech",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Contact Mech and Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "CM", entityName = "FacilityContactMech"),
            @MemberEntity(entityAlias = "MC", entityName = "ContactMech")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "MC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "MC",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
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
    public interface FacilityContactMechAndContactMechView {}

    /**
     * Facility and Contact Mech View
     */
    @ViewEntity(
        name = "FacilityAndContactMech",
        packageName = "org.ofbiz.product.facility",
        title = "Facility and Contact Mech View",
        members = {
            @MemberEntity(entityAlias = "FA", entityName = "Facility"),
            @MemberEntity(entityAlias = "CM", entityName = "FacilityContactMech"),
            @MemberEntity(entityAlias = "MC", entityName = "ContactMech")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FA"),
            @AliasAll(entityAlias = "CM"),
            @AliasAll(entityAlias = "MC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FA",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "MC",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface FacilityAndContactMechView {}

    /**
     * Facility Contact Mech and Purpose View
     */
    @ViewEntity(
        name = "FacilityContactMechAndPurpose",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Contact Mech and Purpose View",
        members = {
            @MemberEntity(entityAlias = "FCM", entityName = "FacilityContactMech"),
            @MemberEntity(entityAlias = "FCMP", entityName = "FacilityContactMechPurpose")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FCM", excludes = {"fromDate", "thruDate"}),
            @AliasAll(entityAlias = "FCMP", excludes = {"fromDate", "thruDate"})
        },
        aliases = {
            @Alias(name = "contactFromDate", entityAlias = "FCM", field = "fromDate"),
            @Alias(name = "contactThruDate", entityAlias = "FCM", field = "thruDate"),
            @Alias(name = "purposeFromDate", entityAlias = "FCMP", field = "fromDate"),
            @Alias(name = "purposeThruDate", entityAlias = "FCMP", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FCM",
                relEntityAlias = "FCMP",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
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
    public interface FacilityContactMechAndPurposeView {}

    /**
     * FacilityContactMech And ContactMech And PostalAddress And FacilityContactMechPurpose
     */
    @ViewEntity(
        name = "FacilityPostalAddressAndPurpose",
        packageName = "org.ofbiz.product.facility",
        title = "FacilityContactMech And ContactMech And PostalAddress And FacilityContactMechPurpose",
        members = {
            @MemberEntity(entityAlias = "FTCT", entityName = "FacilityContactMech"),
            @MemberEntity(entityAlias = "FTCTP", entityName = "FacilityContactMechPurpose"),
            @MemberEntity(entityAlias = "CT", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PTA", entityName = "PostalAddress")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FTCT"),
            @AliasAll(entityAlias = "CT"),
            @AliasAll(entityAlias = "PTA"),
            @AliasAll(entityAlias = "FTCTP", excludes = {"fromDate", "thruDate"})
        },
        aliases = {
            @Alias(name = "puFromDate", entityAlias = "FTCTP", field = "fromDate"),
            @Alias(name = "puThruDate", entityAlias = "FTCTP", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FTCT",
                relEntityAlias = "FTCTP",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "FTCT",
                relEntityAlias = "CT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CT",
                relEntityAlias = "PTA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface FacilityPostalAddressAndPurposeView {}

    /**
     * Facility Location and Geo Point View
     */
    @ViewEntity(
        name = "FacilityLocationAndGeoPoint",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Location and Geo Point View",
        members = {
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation"),
            @MemberEntity(entityAlias = "FLGPT", entityName = "FacilityLocationGeoPoint"),
            @MemberEntity(entityAlias = "GPT", entityName = "GeoPoint")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GPT")
        },
        aliases = {
            @Alias(name = "facilityId", entityAlias = "FL"),
            @Alias(name = "locationSeqId", entityAlias = "FL"),
            @Alias(name = "fromDate", entityAlias = "FLGPT"),
            @Alias(name = "thruDate", entityAlias = "FLGPT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FL",
                relEntityAlias = "FLGPT",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "FLGPT",
                relEntityAlias = "GPT",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "FacilityLocationGeoPoint",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId"),
                    @KeyMap(fieldName = "geoPointId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
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
    public interface FacilityLocationAndGeoPointView {}

    /**
     * Facility Content Detail View
     */
    @ViewEntity(
        name = "FacilityContentDetail",
        packageName = "org.ofbiz.product.facility",
        title = "Facility Content Detail View",
        members = {
            @MemberEntity(entityAlias = "FCT", entityName = "FacilityContent"),
            @MemberEntity(entityAlias = "CNT", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "FCT"),
            @AliasAll(entityAlias = "CNT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "FCT",
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
    public interface FacilityContentDetailView {}

    /**
     * Product Facility And Contactmech And Postal Address View Entity, to be able to list products by geographic location
     */
    @ViewEntity(
        name = "ProductFacilityAndPostalAddress",
        packageName = "org.ofbiz.product.facility",
        title = "Product Facility And Contactmech And Postal Address View Entity, to be able to list products by geographic location",
        members = {
            @MemberEntity(entityAlias = "PDFT", entityName = "ProductFacility"),
            @MemberEntity(entityAlias = "FTCT", entityName = "FacilityContactMech"),
            @MemberEntity(entityAlias = "CT", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PTA", entityName = "PostalAddress")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PDFT"),
            @AliasAll(entityAlias = "FTCT"),
            @AliasAll(entityAlias = "CT"),
            @AliasAll(entityAlias = "PTA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PDFT",
                relEntityAlias = "FTCT",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @ViewLink(
                entityAlias = "FTCT",
                relEntityAlias = "CT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "CT",
                relEntityAlias = "PTA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface ProductFacilityAndPostalAddressView {}

    /**
     * ProductFacilityLocation Quantity Test View
     */
    @ViewEntity(
        name = "ProductFacilityLocationQuantityTest",
        packageName = "org.ofbiz.product.facility",
        title = "ProductFacilityLocation Quantity Test View",
        members = {
            @MemberEntity(entityAlias = "PFL", entityName = "ProductFacilityLocation"),
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PFL", groupBy = true),
            @Alias(name = "facilityId", entityAlias = "PFL", groupBy = true),
            @Alias(name = "locationSeqId", entityAlias = "PFL", groupBy = true),
            @Alias(name = "minimumStock", entityAlias = "PFL", groupBy = true),
            @Alias(name = "moveQuantity", entityAlias = "PFL", groupBy = true),
            @Alias(name = "locationTypeEnumId", entityAlias = "FL", groupBy = true),
            @Alias(name = "availableToPromiseTotal", entityAlias = "II", function = AggregateFunction.SUM),
            @Alias(name = "quantityOnHandTotal", entityAlias = "II", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PFL",
                relEntityAlias = "FL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "PFL",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
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
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            )
        }
    )
    public interface ProductFacilityLocationQuantityTestView {}

    /**
     * ProductFacilityLocation And FacilityLocation View
     */
    @ViewEntity(
        name = "ProductFacilityLocationView",
        packageName = "org.ofbiz.product.facility",
        title = "ProductFacilityLocation And FacilityLocation View",
        members = {
            @MemberEntity(entityAlias = "PFL", entityName = "ProductFacilityLocation"),
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PFL"),
            @AliasAll(entityAlias = "FL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PFL",
                relEntityAlias = "FL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "InventoryItem",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            )
        }
    )
    public interface ProductFacilityLocationViewView {}

    /**
     * ProductFeature And ProductFeatureAppl View
     */
    @ViewEntity(
        name = "ProductFeatureAndAppl",
        packageName = "org.ofbiz.product.feature",
        title = "ProductFeature And ProductFeatureAppl View",
        members = {
            @MemberEntity(entityAlias = "PF", entityName = "ProductFeature"),
            @MemberEntity(entityAlias = "PFA", entityName = "ProductFeatureAppl")
        },
        aliases = {
            @Alias(name = "productFeatureId", entityAlias = "PF"),
            @Alias(name = "productFeatureTypeId", entityAlias = "PF"),
            @Alias(name = "productFeatureCategoryId", entityAlias = "PF"),
            @Alias(name = "description", entityAlias = "PF"),
            @Alias(name = "uomId", entityAlias = "PF"),
            @Alias(name = "numberSpecified", entityAlias = "PF"),
            @Alias(name = "defaultAmount", entityAlias = "PF"),
            @Alias(name = "defaultSequenceNum", entityAlias = "PF"),
            @Alias(name = "abbrev", entityAlias = "PF"),
            @Alias(name = "idCode", entityAlias = "PF"),
            @Alias(name = "productId", entityAlias = "PFA"),
            @Alias(name = "productFeatureApplTypeId", entityAlias = "PFA"),
            @Alias(name = "fromDate", entityAlias = "PFA"),
            @Alias(name = "thruDate", entityAlias = "PFA"),
            @Alias(name = "sequenceNum", entityAlias = "PFA"),
            @Alias(name = "amount", entityAlias = "PFA"),
            @Alias(name = "recurringAmount", entityAlias = "PFA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PF",
                relEntityAlias = "PFA",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFeature",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFeatureAppl",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "productFeatureId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFeatureType",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFeatureApplType",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureApplTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFeatureCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SupplierProductFeature",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureAndApplView {}

    /**
     * Product Feature Group And Applicability View
     */
    @ViewEntity(
        name = "ProductFeatureGroupAndAppl",
        packageName = "org.ofbiz.product.feature",
        title = "Product Feature Group And Applicability View",
        members = {
            @MemberEntity(entityAlias = "PFGA", entityName = "ProductFeatureGroupAppl"),
            @MemberEntity(entityAlias = "PF", entityName = "ProductFeature")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PFGA"),
            @AliasAll(entityAlias = "PF")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PFGA",
                relEntityAlias = "PF",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProductFeatureGroupAndApplView {}

    /**
     * ProductFeatureGroupAppl And ProductFeatureAppl View
     */
    @ViewEntity(
        name = "ProdFeaGrpAppAndProdFeaApp",
        packageName = "org.ofbiz.product.feature",
        title = "ProductFeatureGroupAppl And ProductFeatureAppl View",
        members = {
            @MemberEntity(entityAlias = "PFGA", entityName = "ProductFeatureGroupAppl"),
            @MemberEntity(entityAlias = "PFA", entityName = "ProductFeatureAppl")
        },
        aliases = {
            @Alias(name = "productFeatureGroupId", entityAlias = "PFGA"),
            @Alias(name = "productFeatureId", entityAlias = "PFGA"),
            @Alias(name = "groupFromDate", entityAlias = "PFA", field = "fromDate"),
            @Alias(name = "groupThruDate", entityAlias = "PFA", field = "thruDate"),
            @Alias(name = "productId", entityAlias = "PFA"),
            @Alias(name = "productFeatureApplTypeId", entityAlias = "PFA"),
            @Alias(name = "fromDate", entityAlias = "PFA"),
            @Alias(name = "thruDate", entityAlias = "PFA"),
            @Alias(name = "sequenceNum", entityAlias = "PFA"),
            @Alias(name = "amount", entityAlias = "PFA"),
            @Alias(name = "recurringAmount", entityAlias = "PFA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PFGA",
                relEntityAlias = "PFA",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ProdFeaGrpAppAndProdFeaAppView {}

    /**
     * Balance Inventory Items View
     */
    @ViewEntity(
        name = "BalanceInventoryItemsView",
        packageName = "org.ofbiz.product.inventory",
        title = "Balance Inventory Items View",
        members = {
            @MemberEntity(entityAlias = "INV", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "RES", entityName = "OrderItemShipGrpInvRes")
        },
        aliases = {
            @Alias(name = "inventoryItemId", entityAlias = "INV"),
            @Alias(name = "productId", entityAlias = "INV"),
            @Alias(name = "facilityId", entityAlias = "INV"),
            @Alias(name = "inventoryItemTypeId", entityAlias = "INV"),
            @Alias(name = "availableToPromiseTotal", entityAlias = "INV"),
            @Alias(name = "quantityOnHandTotal", entityAlias = "INV"),
            @Alias(name = "orderId", entityAlias = "RES"),
            @Alias(name = "shipGroupSeqId", entityAlias = "RES"),
            @Alias(name = "orderItemSeqId", entityAlias = "RES"),
            @Alias(name = "quantity", entityAlias = "RES"),
            @Alias(name = "quantityNotAvailable", entityAlias = "RES"),
            @Alias(name = "reserveOrderEnumId", entityAlias = "RES"),
            @Alias(name = "reservedDatetime", entityAlias = "RES"),
            @Alias(name = "sequenceId", entityAlias = "RES")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "INV",
                relEntityAlias = "RES",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface BalanceInventoryItemsViewView {}

    /**
     * InventoryItem And FacilityLocation View
     */
    @ViewEntity(
        name = "InventoryItemAndLocation",
        packageName = "org.ofbiz.product.inventory",
        title = "InventoryItem And FacilityLocation View",
        members = {
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "PR", entityName = "Product"),
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "II", excludes = {"comments"}),
            @AliasAll(entityAlias = "PR", excludes = {"facilityId", "inventoryItemTypeId"}),
            @AliasAll(entityAlias = "FL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "FL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "PR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductFacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "productId"),
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InventoryItem",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface InventoryItemAndLocationView {}

    @ViewEntity(
        name = "InventoryItemAndDetail",
        packageName = "org.ofbiz.product.inventory",
        members = {
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "IID", entityName = "InventoryItemDetail")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "IID")
        },
        aliases = {
            @Alias(name = "inventoryItemId", entityAlias = "II"),
            @Alias(name = "inventoryItemTypeId", entityAlias = "II"),
            @Alias(name = "productId", entityAlias = "II"),
            @Alias(name = "partyId", entityAlias = "II"),
            @Alias(name = "ownerPartyId", entityAlias = "II"),
            @Alias(name = "statusId", entityAlias = "II"),
            @Alias(name = "datetimeReceived", entityAlias = "II"),
            @Alias(name = "datetimeManufactured", entityAlias = "II"),
            @Alias(name = "expireDate", entityAlias = "II"),
            @Alias(name = "facilityId", entityAlias = "II"),
            @Alias(name = "containerId", entityAlias = "II"),
            @Alias(name = "lotId", entityAlias = "II"),
            @Alias(name = "uomId", entityAlias = "II"),
            @Alias(name = "binNumber", entityAlias = "II"),
            @Alias(name = "locationSeqId", entityAlias = "II"),
            @Alias(name = "comments", entityAlias = "II"),
            @Alias(name = "quantityOnHandTotal", entityAlias = "II"),
            @Alias(name = "availableToPromiseTotal", entityAlias = "II"),
            @Alias(name = "accountingQuantityTotal", entityAlias = "II"),
            @Alias(name = "oldQuantityOnHand", entityAlias = "II"),
            @Alias(name = "oldAvailableToPromise", entityAlias = "II"),
            @Alias(name = "serialNumber", entityAlias = "II"),
            @Alias(name = "softIdentifier", entityAlias = "II"),
            @Alias(name = "activationNumber", entityAlias = "II"),
            @Alias(name = "activationValidThru", entityAlias = "II"),
            @Alias(name = "currencyUomId", entityAlias = "II"),
            @Alias(name = "inventoryItemFixedAssetId", entityAlias = "II", field = "fixedAssetId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "IID",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface InventoryItemAndDetailView {}

    /**
     * Inventory Item Detail Summary View
     */
    @ViewEntity(
        name = "InventoryItemDetailSummary",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item Detail Summary View",
        members = {
            @MemberEntity(entityAlias = "IID", entityName = "InventoryItemDetail")
        },
        aliases = {
            @Alias(name = "inventoryItemId", entityAlias = "IID", groupBy = true),
            @Alias(name = "availableToPromiseTotal", entityAlias = "IID", field = "availableToPromiseDiff", function = AggregateFunction.SUM),
            @Alias(name = "quantityOnHandTotal", entityAlias = "IID", field = "quantityOnHandDiff", function = AggregateFunction.SUM),
            @Alias(name = "accountingQuantityTotal", entityAlias = "IID", field = "accountingQuantityDiff", function = AggregateFunction.SUM)
        }
    )
    public interface InventoryItemDetailSummaryView {}

    /**
     * Inventory Item And Inventory Item Detail for Summation View
     */
    @ViewEntity(
        name = "InventoryItemDetailForSum",
        packageName = "org.ofbiz.product.inventory",
        title = "Inventory Item And Inventory Item Detail for Summation View",
        members = {
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "IID", entityName = "InventoryItemDetail")
        },
        aliases = {
            @Alias(name = "quantityOnHandSum", entityAlias = "IID", field = "quantityOnHandDiff", function = AggregateFunction.SUM),
            @Alias(name = "accountingQuantitySum", entityAlias = "IID", field = "accountingQuantityDiff", function = AggregateFunction.SUM),
            @Alias(name = "inventoryItemTypeId", entityAlias = "II", groupBy = true),
            @Alias(name = "facilityId", entityAlias = "II", groupBy = true),
            @Alias(name = "productId", entityAlias = "II", groupBy = true),
            @Alias(name = "unitCost", entityAlias = "II", groupBy = true),
            @Alias(name = "currencyUomId", entityAlias = "II", groupBy = true),
            @Alias(name = "effectiveDate", entityAlias = "IID"),
            @Alias(name = "orderId", entityAlias = "IID"),
            @Alias(name = "ownerPartyId", entityAlias = "II"),
            @Alias(name = "quantityOnHandDiff", entityAlias = "IID"),
            @Alias(name = "accountingQuantityDiff", entityAlias = "IID")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "IID",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface InventoryItemDetailForSumView {}

    /**
     * This view-entity is for querying a count (findCountByCondition) of InventoryItems that were in a certain status at a certain point in time.
     */
    @ViewEntity(
        name = "InventoryItemStatusForCount",
        packageName = "org.ofbiz.product.inventory",
        description = "This view-entity is for querying a count (findCountByCondition) of InventoryItems that were in a certain status at a certain point in time.",
        members = {
            @MemberEntity(entityAlias = "IIS", entityName = "InventoryItemStatus"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliases = {
            @Alias(name = "facilityId", entityAlias = "II"),
            @Alias(name = "inventoryItemTypeId", entityAlias = "II"),
            @Alias(name = "inventoryItemId", entityAlias = "IIS"),
            @Alias(name = "productId", entityAlias = "IIS"),
            @Alias(name = "statusId", entityAlias = "IIS"),
            @Alias(name = "statusDatetime", entityAlias = "IIS"),
            @Alias(name = "statusEndDatetime", entityAlias = "IIS")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "IIS",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface InventoryItemStatusForCountView {}

    /**
     * PhysicalInventory and InventoryItemVariance View
     */
    @ViewEntity(
        name = "PhysicalInventoryAndVariance",
        packageName = "org.ofbiz.product.inventory",
        title = "PhysicalInventory and InventoryItemVariance View",
        members = {
            @MemberEntity(entityAlias = "PHINV", entityName = "PhysicalInventory"),
            @MemberEntity(entityAlias = "IIV", entityName = "InventoryItemVariance")
        },
        aliases = {
            @Alias(name = "physicalInventoryId", entityAlias = "PHINV"),
            @Alias(name = "physicalInventoryDate", entityAlias = "PHINV"),
            @Alias(name = "partyId", entityAlias = "PHINV"),
            @Alias(name = "generalComments", entityAlias = "PHINV"),
            @Alias(name = "inventoryItemId", entityAlias = "IIV"),
            @Alias(name = "varianceReasonId", entityAlias = "IIV"),
            @Alias(name = "availableToPromiseVar", entityAlias = "IIV"),
            @Alias(name = "quantityOnHandVar", entityAlias = "IIV"),
            @Alias(name = "comments", entityAlias = "IIV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PHINV",
                relEntityAlias = "IIV",
                keyMaps = {
                    @KeyMap(fieldName = "physicalInventoryId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "VarianceReason",
                keyMaps = {
                    @KeyMap(fieldName = "varianceReasonId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InventoryItem",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
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
                relEntityName = "PartyGroup",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface PhysicalInventoryAndVarianceView {}

    /**
     * GoodIdentification and Product View
     */
    @ViewEntity(
        name = "GoodIdentificationAndProduct",
        packageName = "org.ofbiz.product.product",
        title = "GoodIdentification and Product View",
        members = {
            @MemberEntity(entityAlias = "GI", entityName = "GoodIdentification"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GI"),
            @AliasAll(entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductType",
                keyMaps = {
                    @KeyMap(fieldName = "productTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                title = "Primary",
                keyMaps = {
                    @KeyMap(fieldName = "primaryProductCategoryId", relFieldName = "productCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                title = "Manufacturer",
                keyMaps = {
                    @KeyMap(fieldName = "manufacturerPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Quantity",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UomType",
                title = "Amount",
                keyMaps = {
                    @KeyMap(fieldName = "amountUomTypeId", relFieldName = "uomTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Weight",
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Height",
                keyMaps = {
                    @KeyMap(fieldName = "heightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Width",
                keyMaps = {
                    @KeyMap(fieldName = "widthUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Uom",
                title = "Depth",
                keyMaps = {
                    @KeyMap(fieldName = "depthUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Enumeration",
                keyMaps = {
                    @KeyMap(fieldName = "ratingTypeEnum", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface GoodIdentificationAndProductView {}

    /**
     * Product and ProductAssoc View
     */
    @ViewEntity(
        name = "ProductAndAssoc",
        packageName = "org.ofbiz.product.product",
        title = "Product and ProductAssoc View",
        members = {
            @MemberEntity(entityAlias = "PD", entityName = "Product"),
            @MemberEntity(entityAlias = "PDA", entityName = "ProductAssoc")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PD", field = "productId"),
            @Alias(name = "internalName", entityAlias = "PD", field = "internalName"),
            @Alias(name = "productIdTo", entityAlias = "PDA", field = "productIdTo"),
            @Alias(name = "productAssocTypeId", entityAlias = "PDA", field = "productAssocTypeId"),
            @Alias(name = "quantity", entityAlias = "PDA", field = "quantity"),
            @Alias(name = "fromDate", entityAlias = "PDA", field = "fromDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PD",
                relEntityAlias = "PDA",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductAndAssocView {}

    /**
     * Product and ProductAssoc To Full View
     */
    @ViewEntity(
        name = "ProductAndAssocAll",
        packageName = "org.ofbiz.product.product",
        title = "Product and ProductAssoc To Full View",
        members = {
            @MemberEntity(entityAlias = "PD", entityName = "Product"),
            @MemberEntity(entityAlias = "PDA", entityName = "ProductAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PD"),
            @AliasAll(entityAlias = "PDA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PD",
                relEntityAlias = "PDA",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductAndAssocAllView {}

    /**
     * Product and ProductAssoc To Full View
     */
    @ViewEntity(
        name = "ProductAndAssocToAll",
        packageName = "org.ofbiz.product.product",
        title = "Product and ProductAssoc To Full View",
        members = {
            @MemberEntity(entityAlias = "PD", entityName = "Product"),
            @MemberEntity(entityAlias = "PDA", entityName = "ProductAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PD"),
            @AliasAll(entityAlias = "PDA", excludes = {"productId"})
        },
        aliases = {
            @Alias(name = "productIdFrom", entityAlias = "PDA", field = "productId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PD",
                relEntityAlias = "PDA",
                keyMaps = {
                    @KeyMap(fieldName = "productId", relFieldName = "productIdTo")
                }
            )
        }
    )
    public interface ProductAndAssocToAllView {}

    /**
     * ProductContent, Content and DataResource View
     */
    @ViewEntity(
        name = "ProductContentAndInfo",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent, Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
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
    public interface ProductContentAndInfoView {}

    /**
     * ProductContent And Content Required View
     */
    @ViewEntity(
        name = "ProductContentAndContent",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent And Content Required View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CO")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ProductContentAndContentView {}

    /**
     * ProductContent And DataResource Required View
     */
    @ViewEntity(
        name = "ProductContentAndDataResource",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent And DataResource Required View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
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
        }
    )
    public interface ProductContentAndDataResourceView {}

    /**
     * ProductContent And ElectronicText Required View
     */
    @ViewEntity(
        name = "ProductContentAndElectronicText",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent And ElectronicText Required View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr"),
            @AliasAll(entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
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
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductContentAndElectronicTextView {}

    /**
     * ProductContent And ElectronicText Required Short View
     */
    @ViewEntity(
        name = "ProductContentAndElecTextShort",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent And ElectronicText Required Short View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductContentAndElecTextShortView {}

    /**
     * ProductContent And ContentAssoc And ElectronicText Required Short View
     */
    @ViewEntity(
        name = "ProductContentAssocAndElecTextShort",
        packageName = "org.ofbiz.product.product",
        title = "ProductContent And ContentAssoc And ElectronicText Required Short View",
        members = {
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "TOCO", entityName = "Content"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText"),
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "TOCO", entityName = "Content"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PC"),
            @Alias(name = "contentId", entityAlias = "PC"),
            @Alias(name = "productContentTypeId", entityAlias = "PC"),
            @Alias(name = "fromDate", entityAlias = "PC"),
            @Alias(name = "thruDate", entityAlias = "PC"),
            @Alias(name = "contentAssocTypeId", entityAlias = "CA"),
            @Alias(name = "contentIdTo", entityAlias = "CA"),
            @Alias(name = "caFromDate", entityAlias = "CA", field = "fromDate"),
            @Alias(name = "caThruDate", entityAlias = "CA", field = "thruDate"),
            @Alias(name = "localeString", entityAlias = "TOCO"),
            @Alias(name = "dataResourceId", entityAlias = "TOCO"),
            @Alias(name = "textData", entityAlias = "EL"),
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "CA",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "TOCO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "TOCO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ProductContentAssocAndElecTextShortView {}

    /**
     * View entity joining Product and InventoryItem to allow queries for InventoryItem based on product attributes
     */
    @ViewEntity(
        name = "ProductInventoryItem",
        packageName = "org.ofbiz.product.product",
        title = "View entity joining Product and InventoryItem to allow queries for InventoryItem based on product attributes",
        members = {
            @MemberEntity(entityAlias = "PR", entityName = "Product"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PR", excludes = {"facilityId", "inventoryItemTypeId"}),
            @AliasAll(entityAlias = "II", excludes = {"comments"})
        },
        aliases = {
            @Alias(name = "productFacilityId", entityAlias = "PR", field = "facilityId"),
            @Alias(name = "inventoryComments", entityAlias = "II", field = "comments")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductInventoryItemView {}

    /**
     * Virtual and Associated Product Prices View
     * When using this to get an associated product price summary the only columns you should request are: productId, productName, assocProductCount, assocMinPrice, assocMaxPrice. The rest of the field aliases should only be used for specifying constraints since they will break the grouping.
     */
    @ViewEntity(
        name = "ProductVirtualAndAssocPrices",
        packageName = "org.ofbiz.product.product",
        title = "Virtual and Associated Product Prices View",
        description = "When using this to get an associated product price summary the only columns you should request are: productId, productName, assocProductCount, assocMinPrice, assocMaxPrice. The rest of the field aliases should only be used for specifying constraints since they will break the grouping.",
        members = {
            @MemberEntity(entityAlias = "PVIRT", entityName = "Product"),
            @MemberEntity(entityAlias = "PA", entityName = "ProductAssoc"),
            @MemberEntity(entityAlias = "PASC", entityName = "Product"),
            @MemberEntity(entityAlias = "PASCPRC", entityName = "ProductPrice")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PVIRT"),
            @Alias(name = "internalName", entityAlias = "PVIRT"),
            @Alias(name = "productName", entityAlias = "PVIRT"),
            @Alias(name = "productAssocTypeId", entityAlias = "PA"),
            @Alias(name = "fromDate", entityAlias = "PA"),
            @Alias(name = "thruDate", entityAlias = "PA"),
            @Alias(name = "assocProductId", entityAlias = "PASC", field = "productId"),
            @Alias(name = "assocProductCount", entityAlias = "PASC", field = "productId", function = AggregateFunction.COUNT_DISTINCT),
            @Alias(name = "assocPriceTypeId", entityAlias = "PASCPRC", field = "productPriceTypeId"),
            @Alias(name = "assocCurrencyUomId", entityAlias = "PASCPRC", field = "currencyUomId"),
            @Alias(name = "assocProductStoreGroupId", entityAlias = "PASCPRC", field = "productStoreGroupId"),
            @Alias(name = "assocPriceFromDate", entityAlias = "PASCPRC", field = "fromDate"),
            @Alias(name = "assocPriceThruDate", entityAlias = "PASCPRC", field = "thruDate"),
            @Alias(name = "assocMinPrice", entityAlias = "PASCPRC", field = "price", function = AggregateFunction.MIN),
            @Alias(name = "assocMaxPrice", entityAlias = "PASCPRC", field = "price", function = AggregateFunction.MAX)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PVIRT",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PA",
                relEntityAlias = "PASC",
                keyMaps = {
                    @KeyMap(fieldName = "productIdTo", relFieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PASC",
                relEntityAlias = "PASCPRC",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductVirtualAndAssocPricesView {}

    /**
     * Virtual and Variant Product View
     */
    @ViewEntity(
        name = "ProductVirtualAndVariantInfo",
        packageName = "org.ofbiz.product.product",
        title = "Virtual and Variant Product View",
        members = {
            @MemberEntity(entityAlias = "PVIRT", entityName = "Product"),
            @MemberEntity(entityAlias = "PVA", entityName = "ProductAssoc"),
            @MemberEntity(entityAlias = "PVAR", entityName = "Product"),
            @MemberEntity(entityAlias = "PVARFA", entityName = "ProductFeatureAppl"),
            @MemberEntity(entityAlias = "PVARF", entityName = "ProductFeature"),
            @MemberEntity(entityAlias = "PVARPRC", entityName = "ProductPrice")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PVIRT"),
            @Alias(name = "productName", entityAlias = "PVIRT"),
            @Alias(name = "internalName", entityAlias = "PVIRT"),
            @Alias(name = "productAssocTypeId", entityAlias = "PVA"),
            @Alias(name = "fromDate", entityAlias = "PVA"),
            @Alias(name = "thruDate", entityAlias = "PVA"),
            @Alias(name = "variantProductId", entityAlias = "PVAR", field = "productId"),
            @Alias(name = "productFeatureApplTypeId", entityAlias = "PVARFA"),
            @Alias(name = "variantFeatureApplFromDate", entityAlias = "PVARFA", field = "fromDate"),
            @Alias(name = "variantFeatureApplThruDate", entityAlias = "PVARFA", field = "thruDate"),
            @Alias(name = "productFeatureId", entityAlias = "PVARF"),
            @Alias(name = "productFeatureTypeId", entityAlias = "PVARF"),
            @Alias(name = "productFeatureCategoryId", entityAlias = "PVARF"),
            @Alias(name = "description", entityAlias = "PVARF"),
            @Alias(name = "variantPriceTypeId", entityAlias = "PVARPRC", field = "productPriceTypeId"),
            @Alias(name = "variantCurrencyUomId", entityAlias = "PVARPRC", field = "currencyUomId"),
            @Alias(name = "variantProductStoreGroupId", entityAlias = "PVARPRC", field = "productStoreGroupId"),
            @Alias(name = "variantPriceFromDate", entityAlias = "PVARPRC", field = "fromDate"),
            @Alias(name = "variantPriceThruDate", entityAlias = "PVARPRC", field = "thruDate"),
            @Alias(name = "variantPrice", entityAlias = "PVARPRC", field = "price")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PVIRT",
                relEntityAlias = "PVA",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PVA",
                relEntityAlias = "PVAR",
                keyMaps = {
                    @KeyMap(fieldName = "productIdTo", relFieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PVAR",
                relEntityAlias = "PVARFA",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PVARFA",
                relEntityAlias = "PVARF",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @ViewLink(
                entityAlias = "PVAR",
                relEntityAlias = "PVARPRC",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductVirtualAndVariantInfoView {}

    /**
     * Product And Price View
     */
    @ViewEntity(
        name = "ProductAndPriceView",
        packageName = "org.ofbiz.product.product",
        title = "Product And Price View",
        members = {
            @MemberEntity(entityAlias = "PR", entityName = "Product"),
            @MemberEntity(entityAlias = "PP", entityName = "ProductPrice")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PR"),
            @Alias(name = "productTypeId", entityAlias = "PR"),
            @Alias(name = "productName", entityAlias = "PR"),
            @Alias(name = "internalName", entityAlias = "PR"),
            @Alias(name = "description", entityAlias = "PR"),
            @Alias(name = "primaryProductCategoryId", entityAlias = "PR"),
            @Alias(name = "isVirtual", entityAlias = "PR"),
            @Alias(name = "productPriceTypeId", entityAlias = "PP"),
            @Alias(name = "productPricePurposeId", entityAlias = "PP"),
            @Alias(name = "currencyUomId", entityAlias = "PP"),
            @Alias(name = "fromDate", entityAlias = "PP"),
            @Alias(name = "thruDate", entityAlias = "PP"),
            @Alias(name = "price", entityAlias = "PP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PR",
                relEntityAlias = "PP",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductAndPriceViewView {}

    /**
     * Product Promotion Email and Party View
     */
    @ViewEntity(
        name = "ProductPromoCodeEmailParty",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Email and Party View",
        members = {
            @MemberEntity(entityAlias = "PPCE", entityName = "ProductPromoCodeEmail"),
            @MemberEntity(entityAlias = "CM", entityName = "ContactMech"),
            @MemberEntity(entityAlias = "PCM", entityName = "PartyContactMech")
        },
        aliases = {
            @Alias(name = "productPromoCodeId", entityAlias = "PPCE"),
            @Alias(name = "infoString", entityAlias = "CM"),
            @Alias(name = "partyId", entityAlias = "PCM"),
            @Alias(name = "fromDate", entityAlias = "PCM"),
            @Alias(name = "thruDate", entityAlias = "PCM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PPCE",
                relEntityAlias = "CM",
                keyMaps = {
                    @KeyMap(fieldName = "emailAddress", relFieldName = "infoString")
                }
            ),
            @ViewLink(
                entityAlias = "CM",
                relEntityAlias = "PCM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface ProductPromoCodeEmailPartyView {}

    /**
     * Product Promotion Use Check View
     */
    @ViewEntity(
        name = "ProductPromoUseCheck",
        packageName = "org.ofbiz.product.promo",
        title = "Product Promotion Use Check View",
        members = {
            @MemberEntity(entityAlias = "PPU", entityName = "ProductPromoUse"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PPU")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "OH")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PPU",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface ProductPromoUseCheckView {}

    /**
     * Product Store Facility By Order
     */
    @ViewEntity(
        name = "ProductStoreFacilityByOrder",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Facility By Order",
        members = {
            @MemberEntity(entityAlias = "ORH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "PSF", entityName = "ProductStoreFacility"),
            @MemberEntity(entityAlias = "PDS", entityName = "ProductStore"),
            @MemberEntity(entityAlias = "FAC", entityName = "Facility")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "ORH"),
            @Alias(name = "productStoreId", entityAlias = "PSF"),
            @Alias(name = "facilityId", entityAlias = "PSF"),
            @Alias(name = "fromDate", entityAlias = "PSF"),
            @Alias(name = "thruDate", entityAlias = "PSF"),
            @Alias(name = "sequenceNum", entityAlias = "PSF"),
            @Alias(name = "storeName", entityAlias = "PDS"),
            @Alias(name = "facilityName", entityAlias = "FAC"),
            @Alias(name = "facilityTypeId", entityAlias = "FAC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ORH",
                relEntityAlias = "PSF",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @ViewLink(
                entityAlias = "PSF",
                relEntityAlias = "PDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @ViewLink(
                entityAlias = "PSF",
                relEntityAlias = "FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface ProductStoreFacilityByOrderView {}

    /**
     * Product Store Promotion and Applicability View
     */
    @ViewEntity(
        name = "ProductStorePromoAndAppl",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Promotion and Applicability View",
        members = {
            @MemberEntity(entityAlias = "PSPA", entityName = "ProductStorePromoAppl"),
            @MemberEntity(entityAlias = "PP", entityName = "ProductPromo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PSPA")
        },
        aliases = {
            @Alias(name = "promoName", entityAlias = "PP"),
            @Alias(name = "userEntered", entityAlias = "PP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PSPA",
                relEntityAlias = "PP",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            )
        }
    )
    public interface ProductStorePromoAndApplView {}

    /**
     * Product Store Carrier And Shipment Method Type View
     */
    @ViewEntity(
        name = "ProductStoreShipmentMethView",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Carrier And Shipment Method Type View",
        members = {
            @MemberEntity(entityAlias = "PSSM", entityName = "ProductStoreShipmentMeth"),
            @MemberEntity(entityAlias = "SM", entityName = "ShipmentMethodType")
        },
        aliases = {
            @Alias(name = "productStoreShipMethId", entityAlias = "PSSM"),
            @Alias(name = "productStoreId", entityAlias = "PSSM"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "PSSM"),
            @Alias(name = "partyId", entityAlias = "PSSM"),
            @Alias(name = "roleTypeId", entityAlias = "PSSM"),
            @Alias(name = "companyPartyId", entityAlias = "PSSM"),
            @Alias(name = "minSize", entityAlias = "PSSM"),
            @Alias(name = "maxSize", entityAlias = "PSSM"),
            @Alias(name = "minTotal", entityAlias = "PSSM"),
            @Alias(name = "maxTotal", entityAlias = "PSSM"),
            @Alias(name = "minWeight", entityAlias = "PSSM"),
            @Alias(name = "maxWeight", entityAlias = "PSSM"),
            @Alias(name = "allowUspsAddr", entityAlias = "PSSM"),
            @Alias(name = "requireUspsAddr", entityAlias = "PSSM"),
            @Alias(name = "allowCompanyAddr", entityAlias = "PSSM"),
            @Alias(name = "requireCompanyAddr", entityAlias = "PSSM"),
            @Alias(name = "includeNoChargeItems", entityAlias = "PSSM"),
            @Alias(name = "includeGeoId", entityAlias = "PSSM"),
            @Alias(name = "excludeGeoId", entityAlias = "PSSM"),
            @Alias(name = "includeFeatureGroup", entityAlias = "PSSM"),
            @Alias(name = "excludeFeatureGroup", entityAlias = "PSSM"),
            @Alias(name = "serviceName", entityAlias = "PSSM"),
            @Alias(name = "configProps", entityAlias = "PSSM"),
            @Alias(name = "shipmentCustomMethodId", entityAlias = "PSSM"),
            @Alias(name = "shipmentGatewayConfigId", entityAlias = "PSSM"),
            @Alias(name = "sequenceNumber", entityAlias = "PSSM"),
            @Alias(name = "description", entityAlias = "SM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PSSM",
                relEntityAlias = "SM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "companyPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                title = "Include",
                keyMaps = {
                    @KeyMap(fieldName = "includeGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                title = "Exclude",
                keyMaps = {
                    @KeyMap(fieldName = "excludeGeoId", relFieldName = "geoId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "ShipmentMethodType",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            )
        }
    )
    public interface ProductStoreShipmentMethViewView {}

    /**
     * Subscription And Communication Event View
     */
    @ViewEntity(
        name = "SubscriptionAndCommEvent",
        packageName = "org.ofbiz.product.subscription",
        title = "Subscription And Communication Event View",
        members = {
            @MemberEntity(entityAlias = "SC", entityName = "SubscriptionCommEvent"),
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SC"),
            @AliasAll(entityAlias = "CE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SC",
                relEntityAlias = "CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface SubscriptionAndCommEventView {}

    /**
     * Supplier-product and product antityview for purchase order entry
     */
    @ViewEntity(
        name = "SupplierProductAndProduct",
        packageName = "org.ofbiz.product.supplier",
        title = "Supplier-product and product antityview for purchase order entry",
        members = {
            @MemberEntity(entityAlias = "SP", entityName = "SupplierProduct"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SP"),
            @AliasAll(entityAlias = "PR", excludes = {"productId", "comments", "quantityUomId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SP",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface SupplierProductAndProductView {}

    @ExtendEntity(
        name = "ProductCategory",
        fields = {
            @Field(name = "imageProfile", type = "id", description = "Name of a media profile for the main product category image, either ImageSizePreset.presetId or media profile name from mediaprofiles.properties, defaults to IMAGE_PRODUCT-ORIGINAL_IMAGE_URL (SCIPIO)")
        }
    )
    public interface ProductCategoryExtension {}

    @ExtendEntity(
        name = "Product",
        fields = {
            @Field(name = "imageProfile", type = "id", description = "Name of a media profile for the main product image, either ImageSizePreset.presetId or media profile name from mediaprofiles.properties, defaults to IMAGE_PRODUCT-ORIGINAL_IMAGE_URL (SCIPIO)")
        }
    )
    public interface ProductExtension {}

    @ExtendEntity(
        name = "ProductStore",
        fields = {
            @Field(name = "reqPayMethForFreeOrders", type = "indicator", description = "SCIPIO: Default Y. If set to N, orders with total zero may go through without payment method selected."),
            @Field(name = "isContentReference", type = "indicator", description = "SCIPIO: Default N. If set to Y, this store's defaultLocaleString\n            (and potentially other settings) is considered the default for its member content records (product/category content, etc.).\n            This is used in multi-store setting by Solr to determine which defaultLocaleString should be used for fields and content that have no localeString and associated to multiple stores.\n            NOTE: 2020-02-03: If multiple stores being considered for a product lookup have isContentReference, they are prioritized by lowest defaultPriority value;\n            if none of the stores looked up have this flag set to Y, the one with lowest defaultPriority value is used as the content reference store."),
            @Field(name = "showDiscontinuedProducts", type = "indicator", description = "SCIPIO: Default Y. If N, products that have reached their salesDiscontinuationDate will not be shown."),
            @Field(name = "useVariantStockCalc", type = "indicator", description = "SCIPIO: Default N. If Y, the calculation for showOutOfStockProducts in store for virtual products will include their variants."),
            @Field(name = "useAnonShoppingList", type = "indicator", description = "SCIPIO: Default N. If Y, the store will support anonymous ShoppingList persisted on device; this is distinct from the auto-save ShoppingList (autoSaveCart) sometimes referred to as the \"guest\" list."),
            @Field(name = "defaultPriority", type = "numeric", description = "SCIPIO: Default store lookup priority when looking up a store for a product; lower value means higher priority. Used in conjunction with isContentReference and elsewhere."),
            @Field(name = "reviewsPurchased", type = "indicator", description = "SCIPIO: Default N. If Y, the store will support product reviews for purchased products."),
            @Field(name = "multipleReviews", type = "indicator", description = "SCIPIO: Default N. If Y, the store will support multiple product reviews for unregistered per user."),
            @Field(name = "saveAbandonedCart", type = "indicator", description = "SCIPIO: Default Y. If Y, ShoppingCart will be save as an abandoned cart when session expires."),
            @Field(name = "sendAbandonedCartReminder", type = "indicator", description = "SCIPIO: Default N. If Y, abandoned ShoppingCart reminder email will be sent to customers."),
            @Field(name = "maxAbandonedCartReminderRetry", type = "numeric", description = "SCIPIO: Default 1. Number of retry times email reminder will be sent"),
            @Field(name = "abandonedCartReminderDayOffset", type = "numeric", description = "SCIPIO: Default 1. Number of days after abandoned cart is created email reminder will start"),
            @Field(name = "notificationEmailShipmentDelivered", type = "indicator", description = "SCIPIO: Default N. If Y, an email will be sent when a shipment status changes to SHIPMENT_DELIVERED.")
        }
    )
    public interface ProductStoreExtension {}

    @ExtendEntity(
        name = "WebSite",
        fields = {
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "allowProductStoreChange", type = "indicator", description = "Allow change of ProductStore for this WebSite (webapp). Defaults to N (no)."),
            @Field(name = "isDefault", type = "indicator", description = "If Y then it is default WebSite"),
            @Field(name = "isStoreDefault", type = "indicator", description = "SCIPIO: If Y then it is default WebSite for its ProductStore (added 2018-09-26)")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "WEB_SITE_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface WebSiteExtension {}

}
