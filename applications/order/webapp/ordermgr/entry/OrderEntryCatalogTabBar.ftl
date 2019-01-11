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
       
        <#if (showProductLinks!false) && product?has_content>
          <#-- SCIPIO: Copied (moved) product edit link from editProduct.ftl -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/ViewProduct?productId=${rawString(product.productId)}${rawString(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <#-- SCIPIO: Live product link -->
          <#assign productPageUrl = getPropertyValue("catalog", "shop.default.link.product.prefix")!>
          <#if productPageUrl?has_content>
            <@menuitem type="link" href=makeOfbizInterWebappUrl(productPageUrl+(product.productId)) text=uiLabelMap.CommonShopPage
                class="+${styles.action_nav!} ${styles.action_view!}" target="_blank"/>
          </#if>
        </#if>

        <#if (showCategoryLinks!false) && productCategory?has_content><#-- SCIPIO: category links -->
          <#if security.hasEntityPermission("CATALOG", "_UPDATE", request)><#-- SCIPIO: changed to _UPDATE from _CREATE -->
            <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/EditCategory?productCategoryId=${rawString(productCategory.productCategoryId)}${rawString(externalKeyParam!)}")
              target="catalog" text=uiLabelMap.ProductEditCategory class="+${styles.action_nav!} ${styles.action_update!}" />
          </#if>
          <#-- SCIPIO: Live product link -->
          <#assign categoryPageUrl = getPropertyValue("catalog", "shop.default.link.category.prefix")!>
          <#if categoryPageUrl?has_content>
            <@menuitem type="link" href=makeOfbizInterWebappUrl(categoryPageUrl+(productCategory.productCategoryId)) text=uiLabelMap.CommonShopPage
                class="+${styles.action_nav!} ${styles.action_view!}" target="_blank"/>
          </#if>
        </#if>
      </@menu>
    </#macro>
    <@section menuContent=menuContent 
        title=(rawLabel('CommonCreate')+" "+rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder')))/>
  </#if>