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

<#if product??>
<@fields type="default-nolabelarea">
  <@row>
    <@cell class="+pid">
      <strong>${product.productId}</strong>
    </@cell>
  </@row>
  <@row>
    <@cell class="+name">
      <a href="<@ofbizUrl>product?product_id=${product.productId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${productContentWrapper.get("PRODUCT_NAME")!}</a>
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
      <a href="<@ofbizUrl>product?<#if categoryId??>category_id=${categoryId}&amp;</#if>product_id=${product.productId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.OrderChooseVariations}...</a>
    <#else>
      <@field type="input" size="5" name="quantity_${product.productId}" value="" />
    </#if>
    </@cell>
  </@row>
</@fields>
<#else>
  <@commonMsg type="error">${uiLabelMap.ProductErrorProductNotFound}.</@commonMsg>
</#if>


