<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#macro categorySortSelect args={}>
  <#local idNum = getRequestVar("scpCatDetailId")!0>
  <#local idNum = idNum + 1>
  <#local dummy = setRequestVar("scpCatDetailId", idNum)>
  <#-- NOTE: @productSortOrderSelectXxx macros defined in catalogcommon.ftl -->
  <form method="post" action="" style="display:none;" id="pcdsort-form-${idNum}">
      <@field type="hidden" name="sortOrder" value=sortOrderEff/>
      <@field type="hidden" name="sortAscending" value=sortAscendingEff/>
      <@field type="hidden" name="sortChg" value="Y"/><#-- Indicates that user intentionally changed the order -->
      <@field type="hidden" name="VIEW_SIZE" value=(viewSize!1)/>
      <@field type="hidden" name="VIEW_INDEX" value=(viewIndex!0)/>
  </form>
  <div class="pcdsort-sortOrder-select-wrapper ${styles.text_right!}" style="white-space: nowrap;">
    <label for="pcdsort-sortOrder-select" style="display:inline;">${uiLabelMap.ProductSortedBy}:</label>
    <@field type="select" inline=true id="pcdsort-sortOrder-select-${idNum}" style="display:inline;" label=uiLabelMap.ProductSortedBy>
        <@productSortOrderSelectOptions sortOrder=(sortOrder!sortOrderDef!) sortAscending=(sortAscending!sortAscendingDef!true)/>
    </@field>
    <@productSortOrderSelectScript id="pcdsort-sortOrder-select-${idNum}" formId="pcdsort-form-${idNum}" submitForm=true/>
  </div>
</#macro>

<@section>
    <#if productCategory?? && solrProducts?has_content>
        <@paginate mode="content" layout="both" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)
            secondDlg={"content":categorySortSelect, "size":3, "always":true} firstDlg={"size":3, "always":true}
            paramStr="sortOrder=${escapeVal(sortOrderEff, 'url')}&sortAscending=${sortAscendingEff}">
            <@grid columns=4>
                <#list solrProducts as solrProduct>
                    <li><@render resource=productsummaryScreen reqAttribs={"productId":solrProduct.productId}/>
                    </li>
                </#list>
            </@grid>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}</@commonMsg>
    </#if>
 </@section>   