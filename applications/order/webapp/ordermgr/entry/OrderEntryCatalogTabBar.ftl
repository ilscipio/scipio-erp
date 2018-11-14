<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#if orderHeader?has_content>
    <@section title=uiLabelMap.PageTitleLookupBulkAddProduct/>
  <#else>
    <#assign sectionTitle>
        ${rawLabel('CommonCreate')}
        <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
            ${rawLabel('OrderPurchaseOrder')}
        <#else>
            ${rawLabel('OrderSalesOrder')}
        </#if>
    </#assign>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("orderentry") text=uiLabelMap.OrderOrderItems class="+${styles.action_nav!}" />
      </@menu>
    </#macro>
    <@section title=sectionTitle menuContent=menuContent />
  </#if>