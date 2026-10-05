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

import org.ofbiz.product.catalog.*;

context.catalogId = CatalogWorker.getCurrentCatalogId(request);
// SCIPIO: This is not necessary anymore and interferes with SideDeepCategory.groovy and Breadcrumbs.groovy
//promoCat = CatalogWorker.getCatalogPromotionsCategoryId(request, catalogId);
//request.setAttribute("productCategoryId", promoCat);

/* NOTE DEJ20070220 woah, this is doing weird stuff like always showing the last viewed category when going to the main page;
 * It appears this was done for to make it go back to the desired category after logging in, but this is NOT the place to do that,
 * and IMO this is an unacceptable side-effect.
 *
 * The whole thing should be re-thought, and should preferably NOT use a custom session variable or try to go through the main page.
 *
 * NOTE: see section commented out in Category.groovy for the other part of this.
 *
 * NOTE JLR 20070221 this should be done using the same method than in add to cart. I will do it like that and remove all this after.
 *
productCategoryId = session.getAttribute("productCategoryId");
if (!productCategoryId) {
    request.setAttribute("productCategoryId", promoCat);
} else {
    request.setAttribute("productCategoryId", productCategoryId);
}
*/
