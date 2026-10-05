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
package com.ilscipio.scipio.order.eeca;

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
     * EECA for entity OrderHeader on create-store/return.
     */
    @Eeca(
        entity = "OrderHeader",
        operation = "create-store",
        event = "return",
        condition = "statusId == 'ORDER_COMPLETED' && needsInventoryIssuance == 'Y'",
        actions = {
            @EecaAction(
                service = "issueImmediatelyFulfilledOrder",
                mode = "sync"
            )
        }
    )
    public interface OrderHeaderCreateStoreReturnEeca1 {}

    /**
     * EECA for entity OrderItem on create-store/return.
     */
    @Eeca(
        entity = "OrderItem",
        operation = "create-store",
        event = "return",
        condition = "!empty(quoteId)",
        actions = {
            @EecaAction(
                service = "checkUpdateQuoteStatus",
                mode = "sync"
            )
        }
    )
    public interface OrderItemCreateStoreReturnEeca2 {}

    /**
     * EECA for entity OrderPaymentPreference on create-store/return.
     */
    @Eeca(
        entity = "OrderPaymentPreference",
        operation = "create-store",
        event = "return",
        condition = "!empty(orderPaymentPreferenceId) && !empty(statusId)",
        actions = {
            @EecaAction(
                service = "changeOrderPaymentStatus",
                mode = "sync"
            )
        }
    )
    public interface OrderPaymentPreferenceCreateStoreReturnEeca3 {}

    /**
     * EECA for entity OrderHeader on create-store/return.
     */
    @Eeca(
        entity = "OrderHeader",
        operation = "create-store",
        event = "return",
        condition = "statusId == 'ORDER_CREATED'",
        assignments = {
            @EecaSet(fieldName = "noteParty", value = "admin"),
            @EecaSet(fieldName = "noteInfo", value = "An order has been created"),
            @EecaSet(fieldName = "moreInfoItemName", value = "orderId"),
            @EecaSet(fieldName = "moreInfoItemId", envName = "orderId"),
            @EecaSet(fieldName = "moreInfoUrl", value = "/ordermgr/control/orderview")
        },
        actions = {
            @EecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface OrderHeaderCreateStoreReturnEeca4 {}

}
