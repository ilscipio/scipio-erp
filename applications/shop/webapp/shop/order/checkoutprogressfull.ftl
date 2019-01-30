<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
