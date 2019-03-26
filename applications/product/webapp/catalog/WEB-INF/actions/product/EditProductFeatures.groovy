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

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap

context.productFeatureAndAppls = from("ProductFeatureAndAppl").where("productId", productId).orderBy("sequenceNum", "productFeatureApplTypeId", "productFeatureTypeId", "description").queryList();

context.productFeatureCategories = from("ProductFeatureCategory").orderBy("description").queryList();

context.productFeatureApplTypes = from("ProductFeatureApplType").orderBy("description").cache(true).queryList();

context.productFeatureGroups = from("ProductFeatureGroup").orderBy("description").queryList();

// SCIPIO (2019-08-02): Filtering by idCode not null in order to expose only the ones with idCodes and avoid confusion
DynamicViewEntity dve = new DynamicViewEntity();
dve.addMemberEntity("PFT", "ProductFeatureType");
dve.addMemberEntity("PF", "ProductFeature");
//dve.addAliasAll("PFT", null, null);
dve.addAlias("PF", "idCode", "idCode", null, null, false, "count");
dve.addAlias("PFT", "productFeatureTypeId", null, null, true, true, null);
dve.addViewLink("PF", "PFT", Boolean.FALSE, UtilMisc.toList(new ModelKeyMap("productFeatureTypeId", "productFeatureTypeId")));

// FIXME: I don't like filtering like this (PF.ID_CODE) but that's the only way I could make it working.
// Looks like we have a bug that includes the function inside the where clause when it should just use the col name.
eli = from(dve).where(EntityCondition.makeCondition("PF.ID_CODE", EntityOperator.NOT_EQUAL, null)).cursorScrollInsensitive().queryIterator();
productFeatureTypes = [];
if (UtilValidate.isNotEmpty(eli)) {
    while (productFeatureAndCode = eli.next()) {
        productFeatureTypes.add(delegator.findOne("ProductFeatureType", UtilMisc.toMap("productFeatureTypeId", productFeatureAndCode.productFeatureTypeId), true));
//        Debug.log("productFeatureAndCode[" + productFeatureAndCode.productFeatureTypeId + "]: " + productFeatureAndCode.idCode);
    }
    eli.close();
}
context.productFeatureTypes = productFeatureTypes;
