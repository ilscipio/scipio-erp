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
 * The pause of a store (R4-15, J5-01). A row with a pausedDate: the store is paused. No row, or a row without pausedDate: the
 * store is live (the resume keeps the row for the counters of the channel reconcile). This row is the only state of the pause:
 * the storefront, the checkout and the quantity of each sales channel read it (see
 * {@link com.ilscipio.scipio.product.store.StorePauses}).
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public class StorePauseEntities {

    @Entity(
        name = "ProductStorePause",
        packageName = "org.ofbiz.product.store",
        title = "Product Store Pause",
        description = "The seller paused the store: no checkout, a note for the buyers, quantity 0 on each sales channel. "
                + "The resume empties pausedDate. An empty pauseUntilDate: no back-on day.",
        fields = {
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "pausedDate", type = "date-time", description = "When the pause started; empty: the store is live"),
            @Field(name = "pauseUntilDate", type = "date-time", description = "The back-on day: the pause ends at this time"),
            @Field(name = "pauseNote", type = "very-long", description = "The note for the buyers (at most 300 characters, StorePauses.NOTE_MAX)"),
            @Field(name = "pausedByUserLoginId", type = "id-vlong", description = "The login that paused the store"),
            @Field(name = "stateSeq", type = "numeric", description = "Counts each pause and resume (review R4-15 C2)"),
            @Field(name = "reconciledSeq", type = "numeric", description = "The stateSeq of the last finished channel reconcile; lower than stateSeq: a reconcile is pending")
        },
        primaryKeys = {
            @PrimaryKey(field = "productStoreId")
        },
        relations = {
            @Relation(type = RelationType.ONE, fkName = "PRDS_PAUSE_STORE", relEntityName = "ProductStore", keyMaps = {@KeyMap(fieldName = "productStoreId")})
        }
    )
    public interface ProductStorePauseEntity {}
}
