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
package org.ofbiz.catalina.container;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;

import org.apache.catalina.realm.GenericPrincipal;
import org.apache.catalina.realm.RealmBase;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

public class ScipioRealm extends RealmBase
{
    public static final String module = ScipioRealm.class.getName();

    @Override
    protected String getPassword(String username) {
        Delegator delegator = DelegatorFactory.getDelegator(null);
        try {
            GenericValue userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", username).queryOne();
            if (userLogin!=null){
                return userLogin.getString("currentPassword");
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return null;
    }

    @Override
    protected Principal getPrincipal(String username) {
        List<String> roles = new ArrayList<>();
        return new GenericPrincipal(username,
                getPassword(username),
                roles);
    }

}