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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("digitalproductedit") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.EcommerceDigitalNewProduct />
    </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleDigitalProductList menuContent=menuContent>
    <@table type="data-complex"> <#-- orig: width="100%" cellpadding="1" cellspacing="0" border="0" -->
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
            <a href="<@ofbizUrl>digitalproductedit?productId=${supplierProduct.productId}&amp;currencyUomId=${supplierProduct.currencyUomId}&amp;minimumOrderQuantity=${supplierProduct.minimumOrderQuantity}&amp;availableFromDate=${supplierProduct.availableFromDate}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">Edit</a>
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
      <@menuitem type="link" href=makeOfbizUrl("digitalproductedit") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.EcommerceDigitalNewProduct />
    </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceDigitalProductPurchaseHistoryCommission menuContent=menuContent>
    <#-- SCIPIO: what goes here? -->
</@section>
