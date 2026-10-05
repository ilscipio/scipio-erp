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

import java.util.HashMap;
import org.ofbiz.base.util.UtilHttp;

tryEntity = true;
errorMessage = parameters._ERROR_MESSAGE_;
if (errorMessage) {
    tryEntity = false;
}

donePage = parameters.DONE_PAGE ?: "viewprofile";

userLoginData = userLogin;
if (!tryEntity) userLoginData = UtilHttp.getParameterMap(request);
if (!userLoginData) userLoginData = [:];

// SCIPIO: 20-12-04: Introduced a new flag in order to allow changepassword screen visualization
if (!userLogin) {
    context.hasVerifyHash = parameters.containsKey("h");
}

context.donePage = donePage;
context.userLoginData = userLoginData;

