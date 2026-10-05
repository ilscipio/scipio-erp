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
/**
 * SCIPIO: Adds the preferred supplier to each MRP proposal so the proposals screen can hand a
 * buy proposal over to the order app (ApprovedProductRequirements?partyId=) in one click.
 */
import org.ofbiz.entity.util.EntityQuery;

enriched = [];
supplierCache = [:];
(context.proposals ?: []).each { req ->
    row = [:];
    row.putAll(req);
    productId = req.productId;
    if (productId && "PRODUCT_REQUIREMENT".equals(req.requirementTypeId)) {
        supplierPartyId = supplierCache[productId];
        if (supplierPartyId == null) {
            supplierProduct = EntityQuery.use(delegator).from("SupplierProduct").where("productId", productId)
                    .filterByDate("availableFromDate", "availableThruDate").orderBy("supplierPrefOrderId").queryFirst();
            supplierPartyId = supplierProduct ? supplierProduct.partyId : "";
            supplierCache[productId] = supplierPartyId;
        }
        if (supplierPartyId) {
            row.supplierPartyId = supplierPartyId;
            party = from("PartyNameView").where("partyId", supplierPartyId).cache(true).queryOne();
            row.supplierName = party ? (party.groupName ?: ((party.firstName ?: "") + " " + (party.lastName ?: "")).trim()) : supplierPartyId;
        }
    }
    enriched.add(row);
}
context.proposals = enriched;
