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

<#if orderHeader?has_content>

<#-- SCIPIO: Moved to page title: <@heading>${uiLabelMap.EcommerceOrderConfirmation}</@heading>-->
<p>${uiLabelMap.ShopThankYouForOrder}</p>
<#assign printable = printable!false>
<#if !isDemoStore?? || isDemoStore>
  <#if printable>
    <p>${uiLabelMap.OrderDemoFrontNote}.</p>
  <#else>
    <@alert type="info">${uiLabelMap.OrderDemoFrontNote}.</@alert>
  </#if>
</#if>
<#if paymentMethodType?has_content && paymentMethodType.paymentMethodTypeId == "EXT_LIGHTNING">
    <#assign bitcoinAmount = Static["org.ofbiz.common.uom.UomWorker"].convertDatedUom(orderDate, orderGrandTotal!grandTotal!0, currencyUomId!,"XBT",dispatcher,true)>
    <@alert type="warning">
    ${uiLabelMap.OrderPaymentDescLightningNote}
    </@alert>
</#if>
<#if !printable>
  <#include "component://shop/webapp/shop/order/hubpay.ftl">
</#if>

  <@render resource="component://shop/widget/OrderScreens.xml#orderheader" />
  <#if subscriptions && validPaymentMethodTypeForSubscriptions> 
    <form name="addCommonToCartForm" action="<@pageUrl>addordertocart/orderstatus</@pageUrl>" method="post">
        <input type="hidden" name="add_all" value="false" />
        <input type="hidden" name="orderId" value="${orderHeader.orderId}" />
  </#if>       
  <@render resource="component://shop/widget/OrderScreens.xml#orderitems" />
  <#if subscriptions && validPaymentMethodTypeForSubscriptions> 
    </form>
  </#if>
  
  <#if !printable>
    <@menu type="button">
      <@menuitem type="link" href=makePageUrl("main") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.EcommerceContinueShopping />
    </@menu>
  </#if>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderSpecifiedNotFound}.</@commonMsg>
</#if>
