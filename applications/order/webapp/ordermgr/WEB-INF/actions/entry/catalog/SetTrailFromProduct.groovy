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
 * SCIPIO: Special orderentry-only script to set the current trail to the current product (best-effort).
 * If it cannot determine one, it clears the trail (so the trail doesn't show for an unrelated product).
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import org.ofbiz.product.product.*;

final module = "SetTrailFromProduct.groovy";

product = context.product;
productId = product?.productId;
if (productId) {
    trails = ProductWorker.getProductRollupTrails(delegator, productId, [CatalogWorker.getCatalogTopCategoryId(request)], true);
    if (trails) {
        CategoryWorker.setTrail(request, trails[0]);
    } else {
        CategoryWorker.resetTrail(request);
        // NOTE: May happen if someone tries multiple tabs or other cases
        Debug.logWarning("OrderEntry: Could not determine a category trail for product '" + productId + "'; setting TOP", module)
    }
}