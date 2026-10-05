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
package com.ilscipio.scipio.product.service;

import java.sql.Timestamp;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * Service markProductReviewed: the seller confirms (or withdraws the confirmation of) the AI values of a product.
 * The flag is the product attribute scipio.reviewed (Y or N); attrDescription holds the user and the date.
 * The MCP action is catalog/product review_mark (W1-08c).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08c).</p>
 */
public class ProductReviewServices {
    public static final String ATTR_NAME = "scipio.reviewed";

    @Service(
        name = "markProductReviewed",
        engine = "java",
        location = "com.ilscipio.scipio.product.service.ProductReviewServices",
        invoke = "markProductReviewed",
        description = "Stores the product attribute scipio.reviewed (Y or N): the seller confirmed the AI values. "
                + "Needs the permission CATALOG_UPDATE. The user and the date go into attrDescription.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "reviewed", type = "Boolean", mode = "IN", optional = "false"),
            @Attribute(name = "attrValue", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface MarkProductReviewed {}

    public static String flag(boolean reviewed) {
        return reviewed ? "Y" : "N";
    }

    public static String note(boolean reviewed, String userLoginId, Timestamp when) {
        return (reviewed ? "reviewed by " : "review withdrawn by ") + userLoginId + " at " + when;
    }

    public static Map<String, Object> markProductReviewed(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError("Permission CATALOG_UPDATE is required.");
        }
        String productId = (String) context.get("productId");
        Boolean reviewed = (Boolean) context.get("reviewed");
        if (reviewed == null) {
            return ServiceUtil.returnError("reviewed (true or false) is required.");
        }
        try {
            if (EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne() == null) {
                return ServiceUtil.returnError("Product not found: " + productId);
            }
            GenericValue attr = delegator.makeValue("ProductAttribute", "productId", productId, "attrName", ATTR_NAME);
            attr.set("attrValue", flag(reviewed));
            attr.set("attrDescription", note(reviewed, userLogin != null ? userLogin.getString("userLoginId") : null,
                    new Timestamp(System.currentTimeMillis())));
            delegator.createOrStore(attr);
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("attrValue", flag(reviewed));
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Review mark failed: " + e.getMessage());
        }
    }
}
