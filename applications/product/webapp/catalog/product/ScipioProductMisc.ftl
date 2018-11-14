<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>

    <@table type="fields">
        <#if product.returnable?has_content && product.returnable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductReturnable}
              </@td>
              <@td colspan="3"><#if product.returnable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#-- marketing -->
        <#if product.includeInPromotions?has_content && product.includeInPromotions=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductIncludePromotions}
              </@td>
              <@td colspan="3"><#if product.includeInPromotions=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#--
        <#if product.contentInfoText?has_content && product.contentInfoText=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductContentInfoText}
              </@td>
              <@td colspan="3"><#if product.contentInfoText=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>-->

        <#-- tax -->
        <#if product.taxable?has_content && product.taxable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductTaxable}
              </@td>
              <@td colspan="3"><#if product.taxable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>