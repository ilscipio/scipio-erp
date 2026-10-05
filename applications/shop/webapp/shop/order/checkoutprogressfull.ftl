<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<@section>
<#if checkoutMode == "primary">
    <@nav type="steps" activeElem=(activeStep!"cart")>
        <@step name="cart" icon="fa fa-cart-arrow-down" href=makePageUrl("showcart")>${uiLabelMap.PageTitleShoppingCart}</@step>
        <@step name="shippingAddress" icon="fa fa-building" href=makePageUrl("checkoutshippingaddress")>${uiLabelMap.OrderAddress}</@step>
        <@step name="shippingOptions" icon="fa fa-truck" href=makePageUrl("checkoutshippingoptions")>${uiLabelMap.EcommerceShippingOptions}</@step>
        <@step name="billing" icon="fa fa-credit-card" href=makePageUrl("checkoutpayment")>${uiLabelMap.EcommercePaymentOptions}</@step>
        <@step name="orderReview" icon="fa fa-info" href=makePageUrl("checkoutreview")>${uiLabelMap.EcommerceOrderConfirmation}</@step>
    </@nav>
<#else>
    <#-- SCIPIO: Migrated from anonymousCheckoutLinks.ftl -->
    <@nav type="steps" activeElem=(activeStep!"cart")>
        <@step name="cart" icon="fa fa-cart-arrow-down" href=makePageUrl("showcart")>${uiLabelMap.PageTitleShoppingCart}</@step>
        <@step name="customer" icon="fa fa-user" href=makePageUrl("setCustomer")>Personal Info</@step>
        <@step name="shippingAddress" icon="fa fa-building" href=makePageUrl("setShipping")>${uiLabelMap.OrderAddress}</@step>
        <@step name="shippingOptions" icon="fa fa-truck" href=makePageUrl("setShipOptions")>${uiLabelMap.EcommerceShippingOptions}</@step>
        <@step name="billing" icon="fa fa-credit-card" href=makePageUrl("setPaymentOption")>${uiLabelMap.EcommercePaymentOptions}</@step>
        <#-- SCIPIO: TODO? Merge with billing? -->
        <@step name="billingInfo" icon="fa fa-credit-card" href=makePageUrl("setPaymentInformation?paymentMethodTypeId=${requestParameters.paymentMethodTypeId!}")>Billing Info</@step>
        <@step name="orderReview" icon="fa fa-info">${uiLabelMap.EcommerceOrderConfirmation}</@step>
    </@nav>
</#if>
</@section>
