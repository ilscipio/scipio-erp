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
public class ViewEntities {

    /**
     * ContentApproval, ProductContent, Content and DataResource View
     */
    @ViewEntity(
        name = "ContentApprovalProductContentAndInfo",
        packageName = "org.ofbiz.content.content",
        title = "ContentApproval, ProductContent, Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "CA", entityName = "ContentApproval"),
            @MemberEntity(entityAlias = "PRC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CA"),
            @AliasAll(entityAlias = "PRC", excludes = {"contentId", "sequenceNum"}),
            @AliasAll(entityAlias = "CO", excludes = {"contentId"}),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "PRC",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "PRC",
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
    public interface ContentApprovalProductContentAndInfoView {}

    /**
     * ProductCategoryMember And ProductPrice View Entiry
     */
    @ViewEntity(
        name = "ProductCategoryMemberAndPrice",
        packageName = "org.ofbiz.product.category",
        title = "ProductCategoryMember And ProductPrice View Entiry",
        members = {
            @MemberEntity(entityAlias = "PCM", entityName = "ProductCategoryMember"),
            @MemberEntity(entityAlias = "PD", entityName = "Product"),
            @MemberEntity(entityAlias = "PP", entityName = "ProductPrice")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PCM"),
            @AliasAll(entityAlias = "PD", prefix = "product", excludes = {"productId"}),
            @AliasAll(entityAlias = "PP", prefix = "price", excludes = {"productId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PCM",
                relEntityAlias = "PD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PD",
                relEntityAlias = "PP",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ProductCategoryMemberAndPriceView {}

    /**
     * ProductStoreGroup And ProductStore View Entiry
     */
    @ViewEntity(
        name = "ProductStoreGroupAndMember",
        packageName = "org.ofbiz.product.store",
        title = "ProductStoreGroup And ProductStore View Entiry",
        members = {
            @MemberEntity(entityAlias = "PSG", entityName = "ProductStoreGroup"),
            @MemberEntity(entityAlias = "PSGM", entityName = "ProductStoreGroupMember"),
            @MemberEntity(entityAlias = "PS", entityName = "ProductStore")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PSG"),
            @AliasAll(entityAlias = "PSGM"),
            @AliasAll(entityAlias = "PS")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PSG",
                relEntityAlias = "PSGM",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            ),
            @ViewLink(
                entityAlias = "PSGM",
                relEntityAlias = "PS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface ProductStoreGroupAndMemberView {}

    /**
     * ProductStoreGroupRollup And ProductStoreGroup View Entity
     */
    @ViewEntity(
        name = "ProductStoreGroupRollupAndChild",
        packageName = "org.ofbiz.product.store",
        title = "ProductStoreGroupRollup And ProductStoreGroup View Entity",
        members = {
            @MemberEntity(entityAlias = "PSGR", entityName = "ProductStoreGroupRollup"),
            @MemberEntity(entityAlias = "PSG", entityName = "ProductStoreGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PSG"),
            @AliasAll(entityAlias = "PSGR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PSG",
                relEntityAlias = "PSGR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreGroupId")
                }
            )
        }
    )
    public interface ProductStoreGroupRollupAndChildView {}

    /**
     * ProductStore and ProductStoreCatalog View Entity
     */
    @ViewEntity(
        name = "ProductStoreAndCatalogAssoc",
        packageName = "org.ofbiz.product.store",
        title = "ProductStore and ProductStoreCatalog View Entity",
        members = {
            @MemberEntity(entityAlias = "PS", entityName = "ProductStore"),
            @MemberEntity(entityAlias = "PSC", entityName = "ProductStoreCatalog")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PS"),
            @AliasAll(entityAlias = "PSC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PS",
                relEntityAlias = "PSC",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface ProductStoreAndCatalogAssocView {}

}
