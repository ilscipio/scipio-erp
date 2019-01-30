<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makePageUrl("digitalproductedit") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.EcommerceDigitalNewProduct />
    </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleDigitalProductList menuContent=menuContent>
    <@table type="data-complex">
      <@tr>
        <@td width="30%"><b>${uiLabelMap.ProductProductName}</b>
        </@td>
        <@td width="45%"><b>${uiLabelMap.CommonDescription}</b>
        </@td>
        <@td width="20%">&nbsp;</@td>
      </@tr>
      <#list supplierProductList as supplierProduct>
        <#assign product = supplierProduct.getRelatedOne("Product", true)/>
        <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
        <@tr>
          <@td>${(product.productName)!}
          </@td>
          <@td>${(product.description)!}
          </@td>
          <@td align="right">
            <a href="<@pageUrl>digitalproductedit?productId=${supplierProduct.productId}&amp;currencyUomId=${supplierProduct.currencyUomId}&amp;minimumOrderQuantity=${supplierProduct.minimumOrderQuantity}&amp;availableFromDate=${supplierProduct.availableFromDate}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">Edit</a>
          </@td>
        </@tr>
      </#list>
      <#if !supplierProductList?has_content>
        <@tr><@td colspan="3"><@commonMsg type="result-norecord">${uiLabelMap.EcommerceNoDigitalProductsFound}</@commonMsg></@td></@tr>
      </#if>
    </@table>
</@section>

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makePageUrl("digitalproductedit") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.EcommerceDigitalNewProduct />
    </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceDigitalProductPurchaseHistoryCommission menuContent=menuContent>
    <#-- SCIPIO: what goes here? -->
</@section>
