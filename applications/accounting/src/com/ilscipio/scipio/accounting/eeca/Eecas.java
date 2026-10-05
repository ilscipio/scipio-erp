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
package com.ilscipio.scipio.accounting.eeca;

import com.ilscipio.scipio.service.def.eeca.*;

/**
 * Auto-generated annotation-based entity ECA definitions.
 *
 * <p>Generated from eecas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Eecas {

    /**
     * EECA for entity FinAccountTrans on create-store/return.
     */
    @Eeca(
        entity = "FinAccountTrans",
        operation = "create-store",
        event = "return",
        condition = "statusId == 'FINACT_TRNS_APPROVED'",
        actions = {
            @EecaAction(
                service = "updateFinAccountBalancesFromTrans",
                mode = "sync"
            )
        }
    )
    public interface FinAccountTransCreateStoreReturnEeca1 {}

    /**
     * EECA for entity FinAccountAuth on create-store/return.
     */
    @Eeca(
        entity = "FinAccountAuth",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "updateFinAccountBalancesFromAuth",
                mode = "sync"
            )
        }
    )
    public interface FinAccountAuthCreateStoreReturnEeca2 {}

}
