<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#if orderHeader?has_content>
    <@section title=uiLabelMap.PageTitleLookupBulkAddProduct/>
  <#else>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("orderentry") text=uiLabelMap.OrderOrderItems class="+${styles.action_nav!}" />
        <#-- SCIPIO: Copied (moved) product edit link from editProduct.ftl -->
        <#if (showEditProductLink!false) && security.hasEntityPermission("CATALOG", "_UPDATE", request) && product?has_content && productId?has_content><#-- SCIPIO: changed to _UPDATE from _CREATE -->
          <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/ViewProduct?productId=${rawString(productId)}${rawString(externalKeyParam!)}")
            target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
        </#if>
      </@menu>
    </#macro>
    <@section menuContent=menuContent 
        title=(rawLabel('CommonCreate')+" "+rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder')))/>
  </#if>