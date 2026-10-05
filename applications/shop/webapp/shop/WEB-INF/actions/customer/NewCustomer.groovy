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

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.product.store.ProductStoreWorker;

productStore = ProductStoreWorker.getProductStore(request);
context.productStoreId = productStore.productStoreId;
context.productStore = productStore;

context.createAllowPassword = "Y".equals(productStore.allowPassword);
context.getUsername = !"Y".equals(productStore.usePrimaryEmailUsername);

previousParams = parameters._PREVIOUS_PARAMS_;
if (previousParams) {
    previousParams = "?" + previousParams;
} else {
    previousParams = "";
}
context.previousParams = previousParams;

//the parameters from janrain
userInfoMap = request.getAttribute("userInfoMap");
if (!userInfoMap) {
    userInfoMap = request.getSession().getAttribute("userInfoMap");
}
if (userInfoMap) {
    if (userInfoMap.givenName && userInfoMap.familyName) {
        requestParameters.USER_FIRST_NAME = userInfoMap.givenName;
        requestParameters.USER_LAST_NAME = userInfoMap.familyName;
    } else if (userInfoMap.formatted) {
        requestParameters.USER_FIRST_NAME = userInfoMap.formatted;
    }
    requestParameters.CUSTOMER_EMAIL = userInfoMap.email;
    requestParameters.preferredUsername = userInfoMap.preferredUsername;
    requestParameters.USERNAME = userInfoMap.preferredUsername;
    request.getSession().setAttribute("userInfoMap", userInfoMap);
}

// SCIPIO: should be automatic now...
//donePage = "main;" + parameters.visit.sessionId
donePage = "main";
context.donePage = donePage;
