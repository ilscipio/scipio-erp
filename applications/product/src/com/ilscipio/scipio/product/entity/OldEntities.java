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
public class OldEntities {

    /**
     * Facility Role
     */
    @Entity(
        name = "OldFacilityRole",
        packageName = "org.ofbiz.product.facility",
        tableName = "FACILITY_ROLE",
        title = "Facility Role",
        fields = {
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "facilityId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "FACILITY_RLE_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "FACILITY_RLE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                fkName = "FACILITY_RLE_ROLE",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface OldFacilityRoleEntity {}

    /**
     * Old Product Keyword
     */
    @Entity(
        name = "OldProductKeyword",
        packageName = "org.ofbiz.product.facility",
        tableName = "PRODUCT_KEYWORD",
        title = "Old Product Keyword",
        fields = {
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "keyword", type = "short-varchar"),
            @Field(name = "relevancyWeight", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "keyword")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_KWD_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        indexes = {
            @Index(
                name = "PROD_KWD_KWD",
                fields = {
                    @IndexField(name = "keyword")
                }
            )
        }
    )
    public interface OldProductKeywordEntity {}

    /**
     * Product Keyword Result
     */
    @Entity(
        name = "OldProductKeywordResult",
        packageName = "org.ofbiz.product.product",
        tableName = "PRODUCT_KEYWORD_RESULT",
        title = "Product Keyword Result",
        neverCache = true,
        fields = {
            @Field(name = "productKeywordResultId", type = "id-ne"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "productCategoryId", type = "id"),
            @Field(name = "searchString", type = "short-varchar"),
            @Field(name = "intraKeywordOperator", type = "very-short"),
            @Field(name = "anyPrefix", type = "indicator"),
            @Field(name = "anySuffix", type = "indicator"),
            @Field(name = "removeStems", type = "indicator"),
            @Field(name = "numResults", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "productKeywordResultId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductCategory",
                keyMaps = {
                    @KeyMap(fieldName = "productCategoryId")
                }
            )
        }
    )
    public interface OldProductKeywordResultEntity {}

}
