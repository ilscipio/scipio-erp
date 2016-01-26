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
<@section title="${uiLabelMap.ProductSearchProducts}, ${uiLabelMap.ProductSearchFor}:">
    <#list searchConstraintStrings as searchConstraintString>
      <div><a href="<@ofbizUrl>keywordsearch?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>${searchConstraintString}</div>
    </#list>
    <span class="label">${uiLabelMap.CommonSortedBy}:</span>${searchSortOrderString}
    <div><a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !productIds?has_content>
      <div><@resultMsg>${uiLabelMap.ProductNoResultsFound}.</@resultMsg></div>
    </#if>

    <#if productIds?has_content>
      <@script>
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

      <#-- Cato: NOTE: actual keyword search params are stored in session -->
      <@paginate mode="content" url=makeOfbizUrl("keywordsearch") paramStr="~clearSearch=N/~noConditionFind=${noConditionFind}" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 previousViewSize=previousViewSize!1 viewIndex=viewIndex!0 listSize=listSize!0 paginateToggle=true paginateOn=((paging!"Y") == "Y")>
      <form method="post" name="products" action="">
        <fieldset>
          <input type="hidden" name="productStoreId" value="${parameters.productStoreId!}" />
          <input type="hidden" name="SEARCH_CATEGORY_ID" value="${(requestParameters.SEARCH_CATEGORY_ID)!}" />
          <@table type="data-list" autoAltRows=true firstRowAlt=true cellspacing="" class="+border-top border-bottom"> <#-- orig: class="basic-table border-top border-bottom" -->
            <#assign listIndex = lowIndex />
            <#list productIds as productId>
              <#assign product = delegator.findOne("Product", {"productId" : productId}, true) />
              <@tr>
                <@td>
                  <input type="checkbox" name="selectResult" value="${productId}"/>
                  <a href="<@ofbizUrl>EditProduct?productId=${productId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">[${productId}] ${(product.internalName)!}</a>
                </@td>
              </@tr>
            </#list>
          </@table>
        </fieldset>
      </form>
      </@paginate>
    </#if>
</@section>