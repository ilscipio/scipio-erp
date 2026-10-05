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

import java.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.common.CommonWorkers;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.webapp.control.*;

productStore = ProductStoreWorker.getProductStore(request);

prodCatalog = CatalogWorker.getProdCatalog(request);
if (prodCatalog) {
    catalogStyleSheet = prodCatalog.styleSheet;
    if (catalogStyleSheet) globalContext.catalogStyleSheet = catalogStyleSheet;
    catalogHeaderLogo = prodCatalog.headerLogo;
    if (catalogHeaderLogo) globalContext.catalogHeaderLogo = catalogHeaderLogo;
}

globalContext.productStore = productStore;
globalContext.checkLoginUrl = LoginWorker.makeLoginUrl(request, "checkLogin");
globalContext.catalogQuickaddUse = CatalogWorker.getCatalogQuickaddUse(request);
