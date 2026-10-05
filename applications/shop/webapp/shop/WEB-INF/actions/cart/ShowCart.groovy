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

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.order.shoppingcart.product.ProductDisplayWorker;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.EntityUtil;

// Get the Cart and Prepare Size
shoppingCart = ShoppingCartEvents.getCartObject(request);
context.shoppingCartSize = shoppingCart?.size() ?: 0;
context.shoppingCart = shoppingCart;

context.productStore = ProductStoreWorker.getProductStore(request);

if (parameters.add_product_id) { // check if a parameter is passed
    add_product_id = parameters.add_product_id;
    product = from("Product").where("productId", add_product_id).cache(true).queryOne();
    context.product = product;
}

// get all the possible gift wrap options
allgiftWraps = from("ProductFeature").where("productFeatureTypeId", "GIFT_WRAP").orderBy("defaultSequenceNum").queryList();
context.allgiftWraps = allgiftWraps;

// get the shopping lists for the logged in user
if (userLogin) {
    allShoppingLists = from("ShoppingList").where(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, userLogin.partyId),
                EntityCondition.makeCondition("listName", EntityOperator.NOT_EQUAL, "auto-save")).orderBy("listName").queryList();
    context.shoppingLists = allShoppingLists;
}

// Get Cart Associated Products Data
associatedProducts = ProductDisplayWorker.getRandomCartProductAssoc(request, true);
context.associatedProducts = associatedProducts;

context.contentPathPrefix = CatalogWorker.getContentPathPrefix(request);

//Get Cart Items
shoppingCartItems = shoppingCart.items();

if(shoppingCartItems) {
    shoppingCartItems.each { shoppingCartItem ->
        if (shoppingCartItem.getProductId()) {
            if (shoppingCartItem.getParentProductId()) {
                parentProductId = shoppingCartItem.getParentProductId();
            } else {
                parentProductId = shoppingCartItem.getProductId();
            }
            context.parentProductId = parentProductId;
        }
        productCategoryMembers = from("ProductCategoryMember").where("productId", parentProductId).queryList();
        if (productCategoryMembers) {
            productCategoryMember = EntityUtil.getFirst(productCategoryMembers);
            productCategory = productCategoryMember.getRelatedOne("ProductCategory", false);
            context.productCategory = productCategory;
        }
    }
}
