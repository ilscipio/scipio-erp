<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#include "component://order/webapp/ordermgr/common/common.ftl">
<#import "component://product/webapp/catalog/common/cataloglib.ftl" as cataloglib>

  <#if orderHeader?has_content>
    <@section title=uiLabelMap.PageTitleLookupBulkAddProduct/>
  <#else>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        <@menuitem type="link" href=makePageUrl("orderentry") text=uiLabelMap.OrderOrderItems class="+${styles.action_nav!}" />
       
        <#if (showProductLinks!false) && product?has_content>
          <#-- SCIPIO: Copied (moved) product edit link from editProduct.ftl -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeServerUrl("/catalog/control/ViewProduct?productId=${rawString(product.productId)}${rawString(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <@cataloglib.productShopPageUrlMenuItem productId=product.productId/><#-- SCIPIO: Live product link -->
        </#if>

        <#if (showCategoryLinks!false) && productCategory?has_content><#-- SCIPIO: category links -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeServerUrl("/catalog/control/EditCategory?productCategoryId=${rawString(productCategory.productCategoryId)}${rawString(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditCategory class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <@cataloglib.categoryShopPageUrlMenuItem categoryId=productCategory.productCategoryId/><#-- SCIPIO: Live product link -->
        </#if>
      </@menu>
    </#macro>
    <@section menuContent=menuContent 
        title=(rawLabel('CommonCreate')+" "+rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder')))/>
  </#if>