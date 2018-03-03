/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
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
