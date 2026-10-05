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
import org.ofbiz.order.shoppingcart.product.ProductPromoWorker;

promoShowLimit = 3;

//Get Promo Text Data
productPromosAll = ProductPromoWorker.getStoreProductPromos(delegator, dispatcher, request);
//Make sure that at least one promo has non-empty promoText
showPromoText = false;
promoToShow = 0;
productPromosAllShowable = new ArrayList(productPromosAll.size());
productPromosAll.each { productPromo ->
    promoText = productPromo.promoText;

    if (promoText && !"N".equals(productPromo.showToCustomer)) {
        showPromoText = true;
        promoToShow++;
        productPromosAllShowable.add(productPromo);
    }
}

// now slim it down to promoShowLimit
productPromosRandomTemp = new ArrayList(productPromosAllShowable);
productPromos = null;
if (productPromosRandomTemp.size() > promoShowLimit) {
    productPromos = new ArrayList(promoShowLimit);
    for (i = 0; i < promoShowLimit; i++) {
        randomIndex = Math.round(java.lang.Math.random() * (productPromosRandomTemp.size() - 1)) as int;
        productPromos.add(productPromosRandomTemp.remove(randomIndex));
    }
} else {
    productPromos = productPromosRandomTemp;
}

context.promoShowLimit = promoShowLimit;
context.productPromosAllShowable = productPromosAllShowable;
context.productPromos = productPromos;
context.showPromoText = showPromoText;
context.promoToShow = promoToShow;
