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
package com.ilscipio.scipio.compliance;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Packaging placed on the market (PPWR / national EPR declarations such as LUCID data reports): kilograms per
 * destination country and material, from the shipped boxes (ShipmentBoxType packaging) and the shipped items
 * (product packaging) of a store's sales orders in a period.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class PackagingReportWorker {

    private PackagingReportWorker() {}

    /** country geoId -> material enumId -> kilograms (3 decimals). */
    public static Map<String, Map<String, BigDecimal>> placedOnMarket(Delegator delegator, String productStoreId, Timestamp from, Timestamp thru)
            throws GenericEntityException {
        Map<String, Map<String, BigDecimal>> out = new TreeMap<>();
        List<GenericValue> shipments = EntityQuery.use(delegator).from("Shipment").where(EntityCondition.makeCondition(
                EntityCondition.makeCondition("shipmentTypeId", "SALES_SHIPMENT"),
                EntityCondition.makeCondition("statusId", EntityOperator.IN, List.of("SHIPMENT_SHIPPED", "SHIPMENT_DELIVERED")),
                EntityCondition.makeCondition("createdDate", EntityOperator.GREATER_THAN_EQUAL_TO, from),
                EntityCondition.makeCondition("createdDate", EntityOperator.LESS_THAN, thru))).queryList();
        for (GenericValue sh : shipments) {
            GenericValue order = sh.getString("primaryOrderId") != null
                    ? EntityQuery.use(delegator).from("OrderHeader").where("orderId", sh.getString("primaryOrderId")).cache().queryOne() : null;
            if (order == null || !productStoreId.equals(order.getString("productStoreId"))) {
                continue;
            }
            String country = "UNKNOWN";
            if (sh.getString("destinationContactMechId") != null) {
                GenericValue pa = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", sh.getString("destinationContactMechId")).cache().queryOne();
                if (pa != null && pa.getString("countryGeoId") != null) {
                    country = pa.getString("countryGeoId");
                }
            }
            Map<String, BigDecimal> byMaterial = out.computeIfAbsent(country, k -> new TreeMap<>());
            for (GenericValue pkg : EntityQuery.use(delegator).from("ShipmentPackage").where("shipmentId", sh.getString("shipmentId")).queryList()) {
                if (pkg.getString("shipmentBoxTypeId") != null) {
                    addComponents(delegator, byMaterial, "shipmentBoxTypeId", pkg.getString("shipmentBoxTypeId"), BigDecimal.ONE);
                }
            }
            for (GenericValue item : EntityQuery.use(delegator).from("ShipmentItem").where("shipmentId", sh.getString("shipmentId")).queryList()) {
                if (item.getString("productId") != null && item.getBigDecimal("quantity") != null) {
                    addComponents(delegator, byMaterial, "productId", item.getString("productId"), item.getBigDecimal("quantity"));
                }
            }
        }
        return out;
    }

    private static void addComponents(Delegator delegator, Map<String, BigDecimal> byMaterial, String field, String value, BigDecimal quantity)
            throws GenericEntityException {
        for (GenericValue pc : EntityQuery.use(delegator).from("PackagingComponent").where(field, value).cache().queryList()) {
            BigDecimal w = pc.getBigDecimal("weight");
            if (w == null) {
                continue;
            }
            BigDecimal kg = toKg(w, pc.getString("weightUomId")).multiply(quantity);
            byMaterial.merge(pc.getString("materialId"), kg.setScale(3, RoundingMode.HALF_UP), BigDecimal::add);
        }
    }

    static BigDecimal toKg(BigDecimal w, String uom) {
        if (uom == null) {
            return w;
        }
        switch (uom) {
        case "WT_g": return w.divide(BigDecimal.valueOf(1000), 6, RoundingMode.HALF_UP);
        case "WT_mg": return w.divide(BigDecimal.valueOf(1000000), 9, RoundingMode.HALF_UP);
        case "WT_lb": return w.multiply(new BigDecimal("0.45359237"));
        case "WT_oz": return w.multiply(new BigDecimal("0.028349523"));
        default: return w;
        }
    }

    /** CSV lines: country;material;kg. */
    public static String toCsv(Map<String, Map<String, BigDecimal>> report) {
        StringBuilder sb = new StringBuilder("country;material;kg\n");
        report.forEach((country, mats) -> mats.forEach((mat, kg) -> sb.append(country).append(';').append(mat).append(';').append(kg.toPlainString()).append('\n')));
        return sb.toString();
    }

}
