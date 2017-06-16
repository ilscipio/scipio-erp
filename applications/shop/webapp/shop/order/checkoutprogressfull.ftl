<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<@section>
<#if checkoutMode == "primary">
    <@nav type="steps" activeElem=(activeStep!"cart")>
        <@step name="cart" icon="fa fa-cart-arrow-down" href=makeOfbizUrl("showcart")>${uiLabelMap.PageTitleShoppingCart}</@step>
        <@step name="shippingAddress" icon="fa fa-building" href=makeOfbizUrl("checkoutshippingaddress")>${uiLabelMap.OrderShippingAddress}</@step>
        <@step name="shippingOptions" icon="fa fa-truck" href=makeOfbizUrl("checkoutshippingoptions")>${uiLabelMap.EcommerceShippingOptions}</@step>
        <@step name="billing" icon="fa fa-credit-card" href=makeOfbizUrl("checkoutpayment")>${uiLabelMap.EcommercePaymentOptions}</@step>
        <@step name="orderReview" icon="fa fa-info" href=makeOfbizUrl("checkoutreview")>${uiLabelMap.EcommerceOrderConfirmation}</@step>
    </@nav>
<#else>
    <#-- SCIPIO: Migrated from anonymousCheckoutLinks.ftl -->
    <@nav type="steps" activeElem=(activeStep!"cart")>
        <@step name="cart" icon="fa fa-cart-arrow-down" href=makeOfbizUrl("showcart")>${uiLabelMap.PageTitleShoppingCart}</@step>
        <@step name="customer" icon="fa fa-user" href=makeOfbizUrl("setCustomer")>Personal Info</@step>
        <@step name="shippingAddress" icon="fa fa-building" href=makeOfbizUrl("setShipping")>${uiLabelMap.OrderShippingAddress}</@step>
        <@step name="shippingOptions" icon="fa fa-truck" href=makeOfbizUrl("setShipOptions")>${uiLabelMap.EcommerceShippingOptions}</@step>
        <@step name="billing" icon="fa fa-credit-card" href=makeOfbizUrl("setPaymentOption")>${uiLabelMap.EcommercePaymentOptions}</@step>
        <#-- SCIPIO: TODO? Merge with billing? -->
        <@step name="billingInfo" icon="fa fa-credit-card" href=makeOfbizUrl("setPaymentInformation?paymentMethodTypeId=${requestParameters.paymentMethodTypeId!}")>Billing Info</@step>
        <@step name="orderReview" icon="fa fa-info">${uiLabelMap.EcommerceOrderConfirmation}</@step>
    </@nav>
</#if>
</@section>
