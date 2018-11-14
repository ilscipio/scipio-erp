<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section>
    <@table type="fields">
        <#if product.requireAmount?has_content && product.requireAmount=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRequireAmount}
              </@td>
              <@td colspan="3"><#if product.requireAmount=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.amountUomTypeId?has_content> 
            <#assign productAmount = product.getRelatedOne("AmountUomType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductAmountUomTypeId}
              </@td>
              <@td colspan="3">${(productAmount.get("description",locale))?default(product.amountUomTypeId)!}</@td>
            </@tr>
        </#if>
    </@table>


</@section>