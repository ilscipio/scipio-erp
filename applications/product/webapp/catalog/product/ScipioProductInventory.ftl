<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section>
    <@table type="fields">
        <#-- inventory -->
        <#if product.salesDiscWhenNotAvail?has_content && product.salesDiscWhenNotAvail=="Y">
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesDiscontinuationNotAvailable}
              </@td>
              <@td colspan="3"><#if product.salesDiscWhenNotAvail=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.requirementMethodEnumId?has_content>
            <#assign productRequirement = product.getRelatedOne("RequirementMethodEnumeration", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRequirementMethodEnumId}
              </@td>
              <@td colspan="3">${(productRequirement.get("description",locale))?default(product.requirementMethodEnumId)!}</@td>
            </@tr>
        </#if>

        <#if product.lotIdFilledIn?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLotId}
              </@td>
              <@td colspan="3">${product.lotIdFilledIn!""}</@td>
            </@tr>
        </#if>

        <#if product.inventoryMessage?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductInventoryMessage}
              </@td>
              <@td colspan="3">${product.inventoryMessage!""}</@td>
            </@tr>
        </#if>
    </@table>


</@section>