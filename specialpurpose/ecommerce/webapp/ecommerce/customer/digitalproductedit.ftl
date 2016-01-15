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

<#assign productPrice = productPriceList[0]!/>

<@section title="${uiLabelMap.PageTitleDigitalProductEdit}" class="+screenlet">
<#if !supplierProduct?has_content && parameters.productId?has_content>
  <div><h3>${uiLabelMap.EcommerceMessage1} [${parameters.productId}] ${uiLabelMap.EcommerceMessage2}</h3></div>
<#else>

    <#if !supplierProduct??>
      <h1>${uiLabelMap.EcommerceAddNewDigitalProduct}</h1>
      <form method="post" action="<@ofbizUrl>createCustomerDigitalDownloadProduct</@ofbizUrl>" name="editdigitaluploadform">
        <input type="hidden" name="productStoreId" value="${productStore.productStoreId}" />
    <#else>
      <h1>${uiLabelMap.EcommerceUpdateDigitalProduct}</h1>
      <form method="post" action="<@ofbizUrl>updateCustomerDigitalDownloadProduct</@ofbizUrl>" name="editdigitaluploadform">
        <input type="hidden" name="productId" value="${parameters.productId}" />
        <input type="hidden" name="currencyUomId" value="${parameters.currencyUomId}" />
        <input type="hidden" name="minimumOrderQuantity" value="${parameters.minimumOrderQuantity}" />
        <input type="hidden" name="availableFromDate" value="${parameters.availableFromDate}" />
    </#if>
    &nbsp;<a href="<@ofbizUrl>digitalproductlist</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBackToList}</a>

    <@table width="90%" border="0" cellpadding="2" cellspacing="0">
    <@tr>
      <@td width="26%" align="right" valign="top">${uiLabelMap.ProductProductName}</@td>
      <@td width="74%"><input type="text" class="inputBox" size="30" maxlength="60" name="productName" value="${(product.productName)!}"/>*</@td>
    </@tr>
    <@tr>
      <@td width="26%" align="right" valign="top">${uiLabelMap.ProductProductDescription}</@td>
      <@td width="74%"><input type="text" class="inputBox" size="30" maxlength="60" name="description" value="${(product.description)!}"/></@td>
    </@tr>
    <@tr>
      <@td width="26%" align="right" valign="top">${uiLabelMap.ProductPrice}</@td>
      <@td width="74%"><input type="text" class="inputBox" size="30" maxlength="60" name="price" value="${(productPrice.price)!}"/>*</@td>
    </@tr>
    <@tr>
      <@td width="26%" align="right" valign="top">&nbsp;</@td>
      <@td width="74%"><a href="javascript:document.editdigitaluploadform.submit()" class="${styles.link_action_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a></@td>
    </@tr>
  </@table>
  </form>
</#if>
</@section>

<#if supplierProduct?has_content>
<@section title="${uiLabelMap.OrderDigitalProductFiles}" class="+screenlet">
        <#list productContentAndInfoList as productContentAndInfo>
            <div>
              ${productContentAndInfo.contentName} (${uiLabelMap.CommonSince}: ${productContentAndInfo.fromDate})
              <a href="<@ofbizUrl>removeCustomerDigitalDownloadProductFile?contentId=${productContentAndInfo.contentId}&amp;productContentTypeId=${productContentAndInfo.productContentTypeId}&amp;fromDate=${productContentAndInfo.fromDate}&amp;productId=${parameters.productId}&amp;currencyUomId=${parameters.currencyUomId}&amp;minimumOrderQuantity=${parameters.minimumOrderQuantity}&amp;availableFromDate=${parameters.availableFromDate}</@ofbizUrl>" class="${styles.link_action_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
            </div>
        </#list>

        <div><hr /></div>
        <div class="tableheadtext">${uiLabelMap.EcommerceDigitalAddFromMyFiles}</div>
        <div>
        <form method="post" action="<@ofbizUrl>addCustomerDigitalDownloadProductFile</@ofbizUrl>" name="adddigitaluploadfile">
          <input type="hidden" name="productId" value="${parameters.productId}" />
          <input type="hidden" name="currencyUomId" value="${parameters.currencyUomId}" />
          <input type="hidden" name="minimumOrderQuantity" value="${parameters.minimumOrderQuantity}" />
          <input type="hidden" name="availableFromDate" value="${parameters.availableFromDate}" />
          <select name="contentId" class="selectBox">
            <#list ownerContentAndRoleList as ownerContentAndRole>
              <option value="${ownerContentAndRole.contentId}">${ownerContentAndRole.contentName}</option>
            </#list>
          </select>
          <a href="javascript:document.adddigitaluploadfile.submit()" class="${styles.link_action_sys!} ${styles.action_update!}">${uiLabelMap.CommonAdd}</a>
        </form>
        </div>
    &nbsp;<a href="<@ofbizUrl>digitalproductlist</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBackToList}</a>
</@section>
</#if>
