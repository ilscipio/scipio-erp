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
package com.ilscipio.scipio.manufacturing.event;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * BOM component consumption formulas, implementing the "interfaceBomFormula" service interface.
 *
 * <p>Hand-written replacement for: component://manufacturing/script/org/ofbiz/manufacturing/bom/BomFormulas.xml</p>
 *
 * <p>These are services (engine=java), not request events: the original auto-conversion wrongly
 * generated HttpServletRequest-based event stubs for them.</p>
 *
 * <p>SCIPIO: 4.0.0: Hand-written from minilang.</p>
 */
public class BomFormulas {

    private BomFormulas() {}

    /** Example formula: quantity = neededQuantity * 10. */
    public static Map<String, Object> exampleComponentFormula(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> arguments = UtilGenerics.cast(context.get("arguments"));
        BigDecimal neededQuantity = (BigDecimal) arguments.get("neededQuantity");

        BigDecimal totQuantity = neededQuantity.multiply(BigDecimal.TEN).setScale(2, RoundingMode.HALF_EVEN);

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("quantity", totQuantity);
        return result;
    }

    /**
     * Formula that computes the quantity of linear component needed in the BOM: how many
     * fixed-width pieces (of the given "width") are needed to cover neededQuantity * amount,
     * rounding up to the next whole piece whenever there is a remainder.
     */
    public static Map<String, Object> linearComponentConsumptionFormula(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> arguments = UtilGenerics.cast(context.get("arguments"));
        BigDecimal neededQuantity = (BigDecimal) arguments.get("neededQuantity");
        BigDecimal amount = (BigDecimal) arguments.get("amount");
        BigDecimal width = (BigDecimal) arguments.get("width");

        BigDecimal totQuantity = neededQuantity.multiply(amount).setScale(2, RoundingMode.HALF_EVEN);
        BigDecimal quantityDou = totQuantity.divide(width, 2, RoundingMode.HALF_EVEN);
        int quantityInt = quantityDou.setScale(0, RoundingMode.HALF_EVEN).intValue();

        BigDecimal quantity;
        if (BigDecimal.valueOf(quantityInt).compareTo(quantityDou) < 0) {
            quantity = BigDecimal.valueOf(quantityInt + 1L).setScale(2, RoundingMode.HALF_EVEN);
        } else {
            quantity = BigDecimal.valueOf(quantityInt).setScale(2, RoundingMode.HALF_EVEN);
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("quantity", quantity);
        return result;
    }

}
