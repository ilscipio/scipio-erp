<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

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
    <#if productCategory?? && solrProducts?has_content>
        <@paginate mode="content" layout="both" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)
            infoWidget={"size":3, "always":true}
            ctrlWidget={"content":catDetailSortSelect, "size":3, "always":true, "layout":"top"}
            paramStr="sortOrder=${escapeVal(sortOrderEff, 'url')}&sortAscending=${sortAscendingEff}">
            <@grid columns=4>
                <#list solrProducts as solrProduct>
                    <li><@render resource=productsummaryScreen reqAttribs={"productId":solrProduct.productId}  ctxVars={"solrProducts": solrProducts!{}}/>
                    </li>
                </#list>
            </@grid>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}</@commonMsg>
    </#if>
 </@section>   