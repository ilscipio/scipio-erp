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
<@section title="${rawString(uiLabelMap.ProductSearchProducts)}, ${rawString(uiLabelMap.ProductSearchFor)}">
    <#list searchConstraintStrings as searchConstraintString>
      <div>&nbsp;<a href="<@ofbizUrl>keywordsearch?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N&amp;SEARCH_CATEGORY_ID=${parameters.SEARCH_CATEGORY_ID!}</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <span>${uiLabelMap.CommonSortedBy}:</span>${searchSortOrderString}
    <div><a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a></div>

  <#if !productIds?has_content>
    <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
  <#else>
    <@script>
        function checkProductToBagTextArea(field, idValue) {
            fullValue = idValue + "\n";
            tempStr = document.forms["quickCreateVirtualWithVariants"].elements["variantProductIdsBag"].value;
            if (field.checked) {
                if (tempStr.length > 0 && tempStr.substring(tempStr.length-1, tempStr.length) != "\n") {
                    tempStr = tempStr + "\n";
                }
                document.forms["quickCreateVirtualWithVariants"].elements["variantProductIdsBag"].value = tempStr + fullValue;
            } else {
                start = document.forms["quickCreateVirtualWithVariants"].elements["variantProductIdsBag"].value.indexOf(fullValue);
                if (start >= 0) {
                    end = start + fullValue.length;
                    document.forms["quickCreateVirtualWithVariants"].elements["variantProductIdsBag"].value = tempStr.substring(0, start) + tempStr.substring(end, tempStr.length);
                    //document.forms["quickCreateVirtualWithVariants"].elements["variantProductIdsBag"].value += start + ", " + end + "\n";
                }
            }
        }

        function toggleAll(e) {
            var cform = document.products;
            var len = cform.elements.length;
            for (var i = 0; i < len; i++) {
                var element = cform.elements[i];
                if (element.name == "selectResult" && element.checked != e.checked) {
                    toggle(element);
                }
            }
        }

        function toggle(e) {
            e.checked = !e.checked;
        }
    </@script>
  
  <#-- Scipio: NOTE: actual keyword search params are stored in session -->
  <#if parameters.ACTIVE_PRODUCT?has_content && parameters.GOOGLE_SYNCED?has_content && parameters.DISCONTINUED_PRODUCT?has_content>
    <#assign paramStr = "~clearSearch=N/~noConditionFind=${noConditionFind}/~ACTIVE_PRODUCT=${parameters.ACTIVE_PRODUCT}/~GOOGLE_SYNCED=${parameters.GOOGLE_SYNCED}/~DISCONTINUED_PRODUCT=${parameters.DISCONTINUED_PRODUCT}/~productStoreId=${parameters.productStoreId}">
  <#else>
    <#assign paramStr = "~clearSearch=N/~noConditionFind=${noConditionFind}">
  </#if>
  <@paginate mode="content" url=makeOfbizUrl("keywordsearch") paramStr=paramStr paramDelim="/" paramPrefix="~" viewSize=viewSize!1 previousViewSize=previousViewSize!1 viewIndex=viewIndex!0 listSize=listSize!0 paginateToggle=true paginateOn=((paging!"N")=="Y")>
    <form method="post" name="products">
      <input type="hidden" name="productStoreId" value="${parameters.productStoreId!}" />
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
          <@tr>
            <@th><input type="checkbox" name="selectAll" value="0" onclick="javascript:toggleAll(this);"/> ${uiLabelMap.ProductProduct}</@th>
          </@tr>
        </@thead>
        <@tbody>
        <#assign listIndex = lowIndex>
        <#list productIds as productId><#-- note that there is no boundary range because that is being done before the list is put in the content -->
          <#assign product = delegator.findOne("Product", {"productId":productId}, false)>
          <@tr valign="middle">
            <@td>
              <input type="checkbox" name="selectResult" value="${productId}" onchange="checkProductToBagTextArea(this, '${productId}');"/>
              <a href="<@ofbizUrl>ViewProduct?productId=${productId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">[${productId}] ${(product.internalName)!}</a>
            </@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    </form>
  </@paginate>

  </#if>
</@section>
