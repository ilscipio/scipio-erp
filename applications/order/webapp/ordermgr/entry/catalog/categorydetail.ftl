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

<#-- SCIPIO: WARNING: 2019: This entire page and its macros are duplicated in the shop webapp (FIXME?) -->

<#include "component://order/webapp/ordermgr/entry/catalog/catalogcommon.ftl">

<#-- SCIPIO: TODO: This select was coded for shop's solr filter, but we can't use solr here so may have to be recoded -->
<#macro catDetailSortSelect args={}>
  <#local idNum = getRequestVar("scpCatDetailId")!0>
  <#local idNum = idNum + 1>
  <#local dummy = setRequestVar("scpCatDetailId", idNum)>
  <form method="post" action="<@catalogUrl currentCategoryId=productCategoryId/>" style="display:none;" id="pcdsort-form-${idNum}">
      <@field type="hidden" name="sortOrder" value=sortOrderEff/>
      <@field type="hidden" name="sortAscending" value=sortAscendingEff/>
      <@field type="hidden" name="sortChg" value="Y"/><#-- Indicates that user intentionally changed the sort order -->
      <@field type="hidden" name="VIEW_SIZE" value=(viewSize!1)/>
      <@field type="hidden" name="VIEW_INDEX" value=(viewIndex!0)/>
  </form>
  <div class="pcdsort-sortOrder-select-wrapper ${styles.text_right!}" style="white-space: nowrap;">
    <label for="pcdsort-sortOrder-select" style="display:inline;">${uiLabelMap.ProductSortedBy}:</label>
    <@field type="select" inline=true id="pcdsort-sortOrder-select-${idNum}" style="display:inline;" label=uiLabelMap.ProductSortedBy>
        <@productSortOrderSelectOptions type="cat" sortOrder=(sortOrder!sortOrderDef!) sortAscending=(sortAscending!sortAscendingDef!true)/>
    </@field>
    <@productSortOrderSelectScript id="pcdsort-sortOrder-select-${idNum}" formId="pcdsort-form-${idNum}" submitForm=true/>
  </div>
</#macro>

<@section>
    <#if productCategoryMembers?has_content>
        <@paginate mode="content" layout="both" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <#-- SCIPIO: TODO: sorting support: pass these to @paginate once implemented:
            infoWidget={"size":3, "always":true}
            ctrlWidget={"content":catDetailSortSelect, "size":3, "always":true, "layout":"top"}>
            paramStr="sortOrder=${escapeVal(sortOrderEff, 'url')}&sortAscending=${sortAscendingEff}" -->
            <@grid columns=4>
                <#list productCategoryMembers as productCategoryMember>
                    <li><@render resource=productsummaryScreen reqAttribs={"productId":productCategoryMember.productId,
                        <#-- SCIPIO: NOTE: unsure if optProductId, productCategoryMember, listIndex are really still needed;
                            passing for compatibility for now -->
                        "optProductId":productCategoryMember.productId, "productCategoryMember":productCategoryMember, "listIndex":productCategoryMember_index}/></li>
                </#list>
            </@grid>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}</@commonMsg>
    </#if>
 </@section>
 
