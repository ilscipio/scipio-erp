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
 * SCIPIO: Loads the standard cost of the product when a productId is given; the screen stays usable without one.
 * Also resolves the product's routing (for the Sources panel links) by reusing the existing getProductRouting service.
 */
import org.ofbiz.service.ServiceUtil

if (productId) {
    def result = dispatcher.runSync("getProductStandardCost", [productId: productId, recalculate: (context.recalculate == true),
            currencyUomId: parameters.currencyUomId ?: "USD", userLogin: userLogin])
    if (ServiceUtil.isError(result)) {
        context.errorMessage = ServiceUtil.getErrorMessage(result)
    } else {
        result.each { k, v -> if (k != "responseMessage") context[k] = v }
    }

    def routingResult = dispatcher.runSync("getProductRouting", [productId: productId, ignoreDefaultRouting: "Y", userLogin: userLogin])
    if (!ServiceUtil.isError(routingResult)) {
        context.productRouting = routingResult.routing
    }
}
