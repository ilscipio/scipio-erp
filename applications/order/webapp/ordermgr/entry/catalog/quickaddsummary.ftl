<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if product??>
<@fields type="default-nolabelarea">
  <@row>
    <@cell class="+pid">
      <strong>${product.productId}</strong>
    </@cell>
  </@row>
  <@row>
    <@cell class="+name">
      <a href="<@pageUrl>product?product_id=${product.productId}</@pageUrl>" class="${styles.link_nav_info_name!}">${productContentWrapper.get("PRODUCT_NAME")!}</a>
    </@cell>
  </@row>
  <@row>
    <@cell class="+listPrice">
      <#if price.listPrice?? && price.price?? && price.price?double < price.listPrice?double>
        ${uiLabelMap.ProductListPrice}: <@ofbizCurrency amount=price.listPrice isoCode=price.currencyUsed/>
      <#else>
        &nbsp;
      </#if>
    </@cell>
  </@row>
  <@row>
    <@cell class="+totalPrice">
    <#if totalPrice??>
        <div>${uiLabelMap.ProductAggregatedPrice}: <span class="basePrice"><@ofbizCurrency amount=totalPrice isoCode=totalPrice.currencyUsed/></span></div>
    <#else>
      <div class="<#if price.isSale?? && price.isSale>salePrice<#else>normalPrice</#if>">
        <strong><@ofbizCurrency amount=price.price isoCode=price.currencyUsed/></strong>
      </div>
    </#if>
    </@cell>
  </@row>
  <@row>
    <@cell class="+qty">
    <#-- check to see if introductionDate hasn't passed yet -->
    <#if product.introductionDate?? && nowTimestamp.before(product.introductionDate)>
      <span class="${styles.text_color_alert!}">${uiLabelMap.ProductNotYetAvailable}</a>
    <#-- check to see if salesDiscontinuationDate has passed -->
    <#elseif product.salesDiscontinuationDate?? && nowTimestamp.before(product.salesDiscontinuationDate)>
      <span class="${styles.text_color_alert!}">${uiLabelMap.ProductNoLongerAvailable}</a>
    <#-- check to see if the product is a virtual product -->
    <#elseif product.isVirtual?default("N") == "Y">
      <a href="<@pageUrl>product?<#if categoryId??>category_id=${categoryId}&amp;</#if>product_id=${product.productId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseVariations}...</a>
    <#else>
      <@field type="input" size="5" name="quantity_${product.productId}" value="" />
    </#if>
    </@cell>
  </@row>
</@fields>
<#else>
  <@commonMsg type="error">${uiLabelMap.ProductErrorProductNotFound}.</@commonMsg>
</#if>


