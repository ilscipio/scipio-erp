/*******************************************************************************
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
 *******************************************************************************/
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