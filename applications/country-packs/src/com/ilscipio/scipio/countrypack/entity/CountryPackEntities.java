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
package com.ilscipio.scipio.countrypack.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Store entities of the country-pack framework (blueprint section 6). The packs themselves are files (folders next to the
 * component file); the store keeps which pack it uses and the state of each task of the pack.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
public class CountryPackEntities {

    @Entity(
        name = "CountryPackAssignment",
        packageName = "com.ilscipio.scipio.countrypack",
        title = "Country Pack Assignment",
        description = "A country pack that a product store uses: the home country (one) or a market (any number). "
                + "packVersion is the pack version that the store has applied.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "packId", type = "id-ne", description = "Pack id, for example de"),
            @Field(name = "roleId", type = "id-ne", description = "HOME or MARKET"),
            @Field(name = "packVersion", type = "numeric"),
            @Field(name = "appliedDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "packId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")})
        }
    )
    public interface CountryPackAssignmentEntity {}

    @Entity(
        name = "CountryPackTask",
        packageName = "com.ilscipio.scipio.countrypack",
        title = "Country Pack Task",
        description = "A setup task of a country pack in one store (LUCID number, tax decision ...). "
                + "statusId: NEEDS_YOU, DONE, DONE_BY_SCIPIO or NOT_NEEDED. The text fields are copies from the pack at the time of the apply.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "packId", type = "id-ne"),
            @Field(name = "taskId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "required", type = "indicator", description = "Y: the market stays closed until the task is closed"),
            @Field(name = "title", type = "description"),
            @Field(name = "kind", type = "id", description = "registration, decision or info"),
            @Field(name = "linkUrl", type = "url"),
            @Field(name = "numberLabel", type = "description", description = "Set: the task asks for a number, for example a registration number"),
            @Field(name = "detail", type = "very-long"),
            @Field(name = "valueText", type = "id-long", description = "The number that the seller entered"),
            @Field(name = "note", type = "description"),
            @Field(name = "sinceVersion", type = "numeric"),
            @Field(name = "doneDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId"),
            @PrimaryKey(field = "packId"),
            @PrimaryKey(field = "taskId")
        },
        relations = {
            @Relation(type = RelationType.ONE_NOFK, relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")})
        }
    )
    public interface CountryPackTaskEntity {}
}
