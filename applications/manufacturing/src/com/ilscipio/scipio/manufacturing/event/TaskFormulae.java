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
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * Routing task time estimate formulas, implementing the "interfaceTaskFormula" service interface.
 *
 * <p>Hand-written replacement for: component://manufacturing/script/org/ofbiz/manufacturing/techdata/TaskFormulae.xml</p>
 *
 * <p>This is a service (engine=java), not a request event: the original auto-conversion wrongly
 * generated an HttpServletRequest-based event stub for it.</p>
 *
 * <p>SCIPIO: 4.0.0: Hand-written from minilang.</p>
 */
public class TaskFormulae {

    private TaskFormulae() {}

    /** Example task formula: totalTime = task.estimatedMilliSeconds * quantity * 10. */
    public static Map<String, Object> exampleTaskFormula(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> arguments = UtilGenerics.cast(context.get("arguments"));
        GenericValue task = (GenericValue) arguments.get("workEffort");
        BigDecimal quantity = (BigDecimal) arguments.get("quantity");
        BigDecimal estimatedMilliSeconds = task.getBigDecimal("estimatedMilliSeconds");

        BigDecimal taskTime = estimatedMilliSeconds.multiply(quantity).setScale(2, RoundingMode.HALF_EVEN);
        BigDecimal totalTime = taskTime.multiply(BigDecimal.TEN).setScale(2, RoundingMode.HALF_EVEN);

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("totalTime", totalTime);
        return result;
    }

}
