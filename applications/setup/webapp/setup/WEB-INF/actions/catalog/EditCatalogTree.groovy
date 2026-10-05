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
 * SCIPIO: SETUP interactive catalog tree data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditCatalogTree.groovy";

// FIXME?: setupEctMaxProductsPerCat is a session-based control for the time being, breaking convention with rest of setup
ectMaxProductsPerCat = context.ectMaxProductsPerCat;
if (ectMaxProductsPerCat == null) {
    try {
        ectMaxProductsPerCat = (request.getAttribute("setupEctMaxProductsPerCat") ?: request.getParameter("setupEctMaxProductsPerCat")) as Integer;
    } catch(Exception e) {
    }
    if (ectMaxProductsPerCat != null) {
        session.setAttribute("setupEctMaxProductsPerCat", ectMaxProductsPerCat);
    }
}
// TODO: REVIEW: I can't leave this at zero during debug...
//if (ectMaxProductsPerCat == null) ectMaxProductsPerCat = 0; // DEFAULT ZERO: fastest and least confusing
//if (ectMaxProductsPerCat == null) ectMaxProductsPerCat = ;
context.ectMaxProductsPerCat = ectMaxProductsPerCat;

context.ectEventStates = context.eventStates;
context.ectIsEventError = context.isSetupEventError;

// CORE DATA PREP
GroovyUtil.runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/tree/EditCatalogTreeCore.groovy", null, context);

// SPECIAL: all categories list needed for dropdowns and such - primaryProductCategoryId (and/or other) fields,
// here must sort it
context.allStoreCategories = EntityUtil.orderBy(context.allStoreCategoriesMap?.values(), ["categoryName", "productCategoryId"]);







