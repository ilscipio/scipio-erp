<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
</#if>

  <@render resource="component://shop/widget/OrderScreens.xml#orderheader" />
  <#if subscriptions && validPaymentMethodTypeForSubscriptions> 
    <form name="addCommonToCartForm" action="<@ofbizUrl>addordertocart/orderstatus</@ofbizUrl>" method="post">
        <input type="hidden" name="add_all" value="false" />
        <input type="hidden" name="orderId" value="${orderHeader.orderId}" />
  </#if>       
  <@render resource="component://shop/widget/OrderScreens.xml#orderitems" />
  <#if subscriptions && validPaymentMethodTypeForSubscriptions> 
    </form>
  </#if>
  
  <#if !printable>
    <@menu type="button">
      <@menuitem type="link" href=makeOfbizUrl("main") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.EcommerceContinueShopping />
    </@menu>
  </#if>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderSpecifiedNotFound}.</@commonMsg>
</#if>
