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
package com.ilscipio.scipio.product.event;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/product/ProductUomFormulas.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductUomFormulas {

    private static final String MODULE = ProductUomFormulas.class.getName();


    /**
     * UoM conversion formula based on product values
     */
    public static Map<String, Object> convertUomProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        BigDecimal toVal = null;
        BigDecimal productFactor = null;
        BigDecimal fromVal = null;
        BigDecimal ratio = null;
        Object args = context.get("arguments");
        if (UtilValidate.isEmpty(((Map<String, Object>) ((Map<String, Object>) args).get("conversionParameters")).get("productId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNoSpecifiedForUomConversion", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Debug.logVerbose("Conversion factor from uomConversion: " + ((Map<String, Object>) ((Map<String, Object>) args).get("uomConversion")).get("conversionFactor"), MODULE);
        Object productId = ((Map<String, Object>) ((Map<String, Object>) args).get("conversionParameters")).get("productId");
        Debug.logVerbose("convertUomProduct: productId=" + productId, MODULE);
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(product)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductNoSpecifiedForUomConversion", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Object uomId = ((Map<String, Object>) args).get("uomId");
        GenericValue fromUom = null;
        try {
            fromUom = EntityQuery.use(delegator)
                    .from("Uom")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        uomId = ((Map<String, Object>) args).get("uomIdTo");
        GenericValue toUom = null;
        try {
            toUom = EntityQuery.use(delegator)
                    .from("Uom")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (java.util.Objects.equals(fromUom.get("uomTypeId"), toUom.get("uomTypeId"))) {
            productFactor = (BigDecimal) ((Map<String, Object>) ((Map<String, Object>) args).get("uomConversion")).get("conversionFactor");
        } else {
            fromVal = BigDecimal.ONE;
            if ("LENGTH_MEASURE".equals(fromUom.get("uomTypeId"))) {
                fromVal = (BigDecimal) product.get("productDepth");
            }
            if ("AREA_MEASURE".equals(fromUom.get("uomTypeId"))) {
                fromVal = ((new BigDecimal(product.get("productDepth").toString())).multiply(new BigDecimal(product.get("productWidth").toString()))).setScale(15, RoundingMode.HALF_UP);
            }
            if ("VOLUME_DRY_MEASURE".equals(fromUom.get("uomTypeId"))) {
                fromVal = ((new BigDecimal(product.get("productDepth").toString())).multiply((new BigDecimal(product.get("productWidth").toString())).multiply(new BigDecimal(product.get("productHeight").toString())))).setScale(15, RoundingMode.HALF_UP);
            }
            if ("WEIGHT_MEASURE".equals(fromUom.get("uomTypeId"))) {
                fromVal = (BigDecimal) product.get("weight");
            }
            Debug.logVerbose("From product-based conversion factor: " + fromVal, MODULE);
            toVal = BigDecimal.ONE;
            if ("LENGTH_MEASURE".equals(toUom.get("uomTypeId"))) {
                toVal = (BigDecimal) product.get("productDepth");
            }
            if ("AREA_MEASURE".equals(toUom.get("uomTypeId"))) {
                toVal = ((new BigDecimal(product.get("productDepth").toString())).multiply(new BigDecimal(product.get("productWidth").toString()))).setScale(15, RoundingMode.HALF_UP);
            }
            if ("VOLUME_DRY_MEASURE".equals(toUom.get("uomTypeId"))) {
                toVal = ((new BigDecimal(product.get("productDepth").toString())).multiply((new BigDecimal(product.get("productWidth").toString())).multiply(new BigDecimal(product.get("productHeight").toString())))).setScale(15, RoundingMode.HALF_UP);
            }
            if ("WEIGHT_MEASURE".equals(toUom.get("uomTypeId"))) {
                toVal = (BigDecimal) product.get("weight");
            }
            Debug.logVerbose("To product-based conversion factor: " + toVal, MODULE);
            ratio = ((new BigDecimal(toVal.toString())).divide(new BigDecimal(fromVal.toString()), java.math.RoundingMode.HALF_UP)).setScale(15, RoundingMode.HALF_UP);
            Debug.logVerbose("To/From ratio is " + ratio, MODULE);
            productFactor = ((new BigDecimal(((Map<String, Object>) ((Map<String, Object>) args).get("uomConversion")).get("conversionFactor").toString())).multiply(new BigDecimal(ratio.toString()))).setScale(15, RoundingMode.HALF_UP);
            Debug.logVerbose("Resulting product-based conversion factor: " + productFactor, MODULE);
        }
        BigDecimal totQuantity = ((new BigDecimal(((Map<String, Object>) args).get("originalValue").toString())).multiply(new BigDecimal(productFactor.toString()))).setScale(15, RoundingMode.HALF_UP);
        result.put("convertedValue", totQuantity);

        return result;
    }

}
