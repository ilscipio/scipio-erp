package com.ilscipio.scipio.product.product;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

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
public abstract class ProductServices {

    public static final String module = ProductServices.class.getName();

    public static Set<String> usedProductFields = new HashSet<String>(Arrays.asList("productId", "productTypeId", "introductionDate", "releaseDate",
            "salesDiscontinuationDate", "productRating", "lastModifiedDate", "lastUpdatedStamp"));

    /**
     * SCIPIO: Function to mark a product as discontinued in solr. Sets attribute "salesDiscontinuationDate" to current date.
     * NOTE: This is redundant if solr ECAs are enabled and should not be used in that case.
     */
    public static Map<String, Object> setProductToSalesDiscontinued(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String productId = (String) context.get("productId");
        if (UtilValidate.isEmpty(productId)) {
            return ServiceUtil.returnError("Empty productId given");
        }
        try {
            GenericValue product = delegator.findOne("Product", false, "productId", productId);
            if (UtilValidate.isEmpty(product)) {
                ServiceUtil.returnError("Could not find product with given id:" + productId);
            }
            product.set("salesDiscontinuationDate", new Timestamp(System.currentTimeMillis()));
            product.set("comments", "Product discontinued (manually disabled)");
            product.store();
            LocalDispatcher dispatcher = dctx.getDispatcher();
            Map<String, Object> params = new HashMap<String, Object>();
            params.put("productId", productId);
            try {
                dispatcher.runSync("updateSolrWithProduct", params);
            }
            catch (GenericServiceException e) {
                Debug.logError(e, module);
                ServiceUtil.returnError("Error reindexing product to Solr:" + e.getMessage());
            }
        }
        catch (GenericEntityException e) {
            Debug.logError(e, module);
            ServiceUtil.returnError("Error while disabling product:" + e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

}
