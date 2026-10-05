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
import org.ofbiz.base.util.Debug

// SCIPIO: Use this script to check first if custom PaymentGatewayConfig exist (mostly from addons)

// Check if PaymentGatewayStripeRest entity exists
paymentGatewayStripeRestModelEntity = delegator.getModelReader().getModelEntityNoCheck("PaymentGatewayStripeRest");
if (paymentGatewayStripeRestModelEntity) {    
    paymentGatewayStripeRest = delegator.findOne("PaymentGatewayStripeRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayStripeRest = paymentGatewayStripeRest;
    context.paymentGatewayStripeRestModelEntity = paymentGatewayStripeRestModelEntity;    
}

// Check if PaymentGatewayPayPalRest entity exists
paymentGatewayPayPalRestModelEntity = delegator.getModelReader().getModelEntityNoCheck("PaymentGatewayPayPalRest");
if (paymentGatewayPayPalRestModelEntity) {
    paymentGatewayPayPalRest = delegator.findOne("PaymentGatewayPayPalRest", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayPayPalRest = paymentGatewayPayPalRest;
    context.paymentGatewayPayPalRestModelEntity = paymentGatewayPayPalRestModelEntity;
}

// Check if PaymentGatewayRedsys entity exists
paymentGatewayRedsysModelEntity = delegator.getModelReader().getModelEntityNoCheck("PaymentGatewayRedsys");
if (paymentGatewayRedsysModelEntity) {
    paymentGatewayRedsys = delegator.findOne("PaymentGatewayRedsys", ["paymentGatewayConfigId" : parameters.paymentGatewayConfigId], false);
    context.paymentGatewayRedsys = paymentGatewayRedsys;
    context.paymentGatewayRedsysModelEntity = paymentGatewayRedsysModelEntity;
}