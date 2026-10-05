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

/*
 * This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.product.store.*;

//either optProduct, optProductId or productId must be specified
product = request.getAttribute("optProduct");
optProductId = request.getAttribute("optProductId");
productId = product?.productId ?: optProductId ?: request.getAttribute("productId");
solrProduct = request.getAttribute("solrProduct");

context.remove("averageRating");
context.remove("numRatings");

// get the product entity
if (!product && productId) {
    product = delegator.findOne("Product", [productId : productId], true);
}

categoryId = null;
reviews = null;
if (product) {
    categoryId = parameters.category_id ?: request.getAttribute("productCategoryId");
    // get the product review(s)
    reviews = product.getRelated("ProductReview", null, ["-postedDateTime"], false);
}

// get the average rating
if (reviews) {
    totalProductRating = 0;
    numRatings = 0;
    reviews.each { productReview ->
        productRating = productReview.productRating;
        if (productRating) {
            totalProductRating += productRating;
            numRatings++;
        }
    }
    if (numRatings) {
        context.averageRating = totalProductRating/numRatings;
        context.numRatings = numRatings;
    }
}

context.productId = productId;
context.categoryId = categoryId;
context.productReviews = reviews;
