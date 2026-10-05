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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<@section title=uiLabelMap.ProductCompareProducts>
  <#assign productCompareList = Static["org.ofbiz.product.product.ProductEvents"].getProductCompareList(request)/>
  <#if productCompareList?has_content>
    <@table type="data-list" autoAltRows=false>
    <#list productCompareList as product>
      <@tr>
        <@td>
          ${Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "PRODUCT_NAME", request, "raw")}
        </@td>
        <@td>
          <form method="post" action="<@pageUrl>removeFromCompare</@pageUrl>" name="removeFromCompare${product_index}form">
            <input type="hidden" name="productId" value="${product.productId}"/>
          </form>
          <a href="javascript:document.removeFromCompare${product_index}form.submit()" class="${styles.link_run_session!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
        </@td>
      </@tr>
    </#list>
  </@table>
  <@menu type="button">
    <@menuitem type="link" href=makePageUrl("clearCompareList") text=uiLabelMap.CommonClearAll class="+${styles.action_run_session!} ${styles.action_clear!}" />
    <@menuitem type="link" href="javascript:popUp('${escapeVal(makePageUrl('compareProducts'), 'js')}', 'compareProducts', '650', '750')" text=uiLabelMap.ProductCompareProducts class="+${styles.action_nav!} ${styles.action_view!}" />
  </@menu>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsToCompare}</@commonMsg>
</#if>
</@section>
