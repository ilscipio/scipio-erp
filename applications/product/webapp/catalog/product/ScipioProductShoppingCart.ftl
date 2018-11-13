<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section>

    <@table type="fields">
        <#if product.orderDecimalQuantity?has_content && product.chargeShipping=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.FormFieldTitle_orderDecimalQuantity}
              </@td>
              <@td colspan="3"><#if product.orderDecimalQuantity=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>