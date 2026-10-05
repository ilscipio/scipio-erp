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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CheckoutServices {

    @Service(
        name = "createUpdateCustomerAndShippingAddress",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/CheckoutServices.xml",
        invoke = "createUpdateCustomerAndShippingAddress",
        implemented = {@Implements(service = "createUpdateShippingAddress")},
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "firstName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN"),
            @Attribute(name = "shipToCountryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToAreaCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToContactNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToExtension", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailContactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "shipToPhoneContactMechId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdateCustomerAndShippingAddress {}

    @Service(
        name = "createUpdateBillingAddressAndPaymentMethod",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/CheckoutServices.xml",
        invoke = "createUpdateBillingAddressAndPaymentMethod",
        implemented = {@Implements(service = "createUpdateBillingAddress"), @Implements(service = "createUpdateCreditCard")},
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "billToCountryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToAreaCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToContactNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToExtension", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToCardSecurityCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "billToPhoneContactMechId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateUpdateBillingAddressAndPaymentMethod {}

    @Service(
        name = "setAnonUserLogin",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/CheckoutServices.xml",
        invoke = "setAnonUserLogin",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetAnonUserLogin {}

}
