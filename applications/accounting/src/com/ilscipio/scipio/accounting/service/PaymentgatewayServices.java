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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentgatewayServices {

    /**
     * Update Payment Gateway Config
     */
    @Service(
        name = "updatePaymentGatewayConfig",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfig",
        description = "Update Payment Gateway Config",
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayConfig", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayConfig", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfig {}

    /**
     * Update Payment Gateway Config SagePay
     */
    @Service(
        name = "updatePaymentGatewayConfigSagePay",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigSagePay",
        description = "Update Payment Gateway Config SagePay",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewaySagePay", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewaySagePay", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigSagePay {}

    /**
     * Delete Payment Gateway Config
     */
    @Service(
        name = "deletePaymentGatewayConfig",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Payment Gateway Config",
        defaultEntityName = "PaymentGatewayConfig",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGatewayConfig {}

    /**
     * Create Payment Gateway Config Authorize Dot Net
     */
    @Service(
        name = "createPaymentGatewayConfigAuthorizeNet",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Payment Gateway Config Authorize Dot Net",
        defaultEntityName = "PaymentGatewayAuthorizeNet",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentGatewayConfigAuthorizeNet {}

    /**
     * Update Payment Gateway Config Authorize Dot Net
     */
    @Service(
        name = "updatePaymentGatewayConfigAuthorizeNet",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigAuthorizeNet",
        description = "Update Payment Gateway Config Authorize Dot Net",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayAuthorizeNet", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayAuthorizeNet", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigAuthorizeNet {}

    /**
     * Delete Payment Gateway Config Authorize Dot Net
     */
    @Service(
        name = "deletePaymentGatewayConfigAuthorizeNet",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Payment Gateway Config Authorize Dot Net",
        defaultEntityName = "PaymentGatewayAuthorizeNet",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGatewayConfigAuthorizeNet {}

    /**
     * Create Payment Gateway Config Clear Commerce
     */
    @Service(
        name = "createPaymentGatewayConfigClearCommerce",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Payment Gateway Config Clear Commerce",
        defaultEntityName = "PaymentGatewayClearCommerce",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentGatewayConfigClearCommerce {}

    /**
     * Update Payment Gateway Config Clear Commerce
     */
    @Service(
        name = "updatePaymentGatewayConfigClearCommerce",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigClearCommerce",
        description = "Update Payment Gateway Config Clear Commerce",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayClearCommerce", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayClearCommerce", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigClearCommerce {}

    /**
     * Delete Payment Gateway Config Clear Commerce
     */
    @Service(
        name = "deletePaymentGatewayConfigClearCommerce",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Payment Gateway Config Clear Commerce",
        defaultEntityName = "PaymentGatewayClearCommerce",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGatewayConfigClearCommerce {}

    /**
     * Update Payment Gateway Config CyberSource
     */
    @Service(
        name = "updatePaymentGatewayConfigCyberSource",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigCyberSource",
        description = "Update Payment Gateway Config CyberSource",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayCyberSource", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayCyberSource", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigCyberSource {}

    /**
     * Update Payment Gateway Config Payflow Pro
     */
    @Service(
        name = "updatePaymentGatewayConfigPayflowPro",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigPayflowPro",
        description = "Update Payment Gateway Config Payflow Pro",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayPayflowPro", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayPayflowPro", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigPayflowPro {}

    /**
     * Update Payment Gateway Config PayPal
     */
    @Service(
        name = "updatePaymentGatewayConfigPayPal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigPayPal",
        description = "Update Payment Gateway Config PayPal",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayPayPal", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayPayPal", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigPayPal {}

    /**
     * Update Payment Gateway Config WorldPay
     */
    @Service(
        name = "updatePaymentGatewayConfigWorldPay",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigWorldPay",
        description = "Update Payment Gateway Config WorldPay",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayWorldPay", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayWorldPay", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigWorldPay {}

    /**
     * Update Payment Gateway Config Type
     */
    @Service(
        name = "updatePaymentGatewayConfigType",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigType",
        description = "Update Payment Gateway Config Type",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayConfigType", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayConfigType", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigType {}

    /**
     * Update Payment Gateway Config SecurePay
     */
    @Service(
        name = "updatePaymentGatewayConfigSecurePay",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigSecurePay",
        description = "Update Payment Gateway Config SecurePay",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewaySecurePay", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewaySecurePay", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigSecurePay {}

    @Service(
        name = "updatePaymentGatewayConfigEway",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "PaymentGatewayEway",
        auth = "true",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigEway {}

    /**
     * Update Payment Gateway Config iDEAL
     */
    @Service(
        name = "updatePaymentGatewayConfigiDEAL",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigiDEAL",
        description = "Update Payment Gateway Config iDEAL",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayiDEAL", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayiDEAL", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigiDEAL {}

    /**
     * Update Payment Gateway Config iDEAL
     */
    @Service(
        name = "updatePaymentGatewayConfigOrbital",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml",
        invoke = "updatePaymentGatewayConfigOrbital",
        description = "Update Payment Gateway Config iDEAL",
        implemented = {@Implements(service = "updatePaymentGatewayConfig")},
        entityAttributes = {
            @EntityAttributes(entityName = "PaymentGatewayOrbital", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PaymentGatewayOrbital", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGatewayConfigOrbital {}

}
