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

<#assign productPrice = productPriceList[0]!/>

<@section title=uiLabelMap.PageTitleDigitalProductEdit>
<#if !supplierProduct?has_content && parameters.productId?has_content>
  <@commonMsg type="info">${uiLabelMap.EcommerceMessage1} [${parameters.productId}] ${uiLabelMap.EcommerceMessage2}</@commonMsg>
<#else>

    <#if !supplierProduct??>
      <@heading>${uiLabelMap.EcommerceAddNewDigitalProduct}</@heading>
      <form method="post" action="<@ofbizUrl>createCustomerDigitalDownloadProduct</@ofbizUrl>" name="editdigitaluploadform">
        <input type="hidden" name="productStoreId" value="${productStore.productStoreId}" />
    <#else>
      <@heading>${uiLabelMap.EcommerceUpdateDigitalProduct}</@heading>
      <form method="post" action="<@ofbizUrl>updateCustomerDigitalDownloadProduct</@ofbizUrl>" name="editdigitaluploadform">
        <input type="hidden" name="productId" value="${parameters.productId}" />
        <input type="hidden" name="currencyUomId" value="${parameters.currencyUomId}" />
        <input type="hidden" name="minimumOrderQuantity" value="${parameters.minimumOrderQuantity}" />
        <input type="hidden" name="availableFromDate" value="${parameters.availableFromDate}" />
    </#if>
    &nbsp;<a href="<@ofbizUrl>digitalproductlist</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBackToList}</a>

    <@table type="fields"> <#-- orig: width="90%" border="0" cellpadding="2" cellspacing="0" -->
    <@tr>
      <@td width="26%">${uiLabelMap.ProductProductName}</@td>
      <@td width="74%"><input type="text" size="30" maxlength="60" name="productName" value="${(product.productName)!}"/>*</@td>
    </@tr>
    <@tr>
      <@td width="26%">${uiLabelMap.ProductProductDescription}</@td>
      <@td width="74%"><input type="text" size="30" maxlength="60" name="description" value="${(product.description)!}"/></@td>
    </@tr>
    <@tr>
      <@td width="26%">${uiLabelMap.ProductPrice}</@td>
      <@td width="74%"><input type="text" size="30" maxlength="60" name="price" value="${(productPrice.price)!}"/>*</@td>
    </@tr>
    <@tr>
      <@td width="26%">&nbsp;</@td>
      <@td width="74%"><a href="javascript:document.editdigitaluploadform.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a></@td>
    </@tr>
  </@table>
  </form>
</#if>
</@section>

<#if supplierProduct?has_content>
<@section title=uiLabelMap.OrderDigitalProductFiles>
    <#list productContentAndInfoList as productContentAndInfo>
        <div>
          ${productContentAndInfo.contentName} (${uiLabelMap.CommonSince}: ${productContentAndInfo.fromDate})
          <a href="<@ofbizUrl>removeCustomerDigitalDownloadProductFile?contentId=${productContentAndInfo.contentId}&amp;productContentTypeId=${productContentAndInfo.productContentTypeId}&amp;fromDate=${productContentAndInfo.fromDate}&amp;productId=${parameters.productId}&amp;currencyUomId=${parameters.currencyUomId}&amp;minimumOrderQuantity=${parameters.minimumOrderQuantity}&amp;availableFromDate=${parameters.availableFromDate}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
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
      <select name="contentId">
        <#list ownerContentAndRoleList as ownerContentAndRole>
          <option value="${ownerContentAndRole.contentId}">${ownerContentAndRole.contentName}</option>
        </#list>
      </select>
      <a href="javascript:document.adddigitaluploadfile.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonAdd}</a>
    </form>
    </div>
    &nbsp;<a href="<@ofbizUrl>digitalproductlist</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBackToList}</a>
</@section>
</#if>
