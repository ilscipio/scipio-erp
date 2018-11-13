<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->


<@section>
    <@table type="fields">

        <#-- quantity and shipping -->
        <#if product.quantityIncluded?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductQuantityIncluded}
              </@td>
                <@td colspan="3">${product.quantityIncluded!""}
                                 <#if product.quantityUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("QuantityUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.piecesIncluded?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductPiecesIncluded}
              </@td>
                <@td colspan="3">${product.piecesIncluded!""}
                </@td>
            </@tr>
        </#if>

        <#if product.inShippingBox?has_content && product.inShippingBox=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingBox}
              </@td>
              <@td colspan="3"><#if product.inShippingBox=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.defaultShipmentBoxTypeId?has_content>
            <#assign productAttrDescription = product.getRelatedOne("DefaultShipmentBoxType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductDefaultShipmentBoxTypeId}
              </@td>
              <@td colspan="3">${(productAttrDescription.get("description",locale))?default(product.defaultShipmentBoxTypeId)!}</@td>
            </@tr>
        </#if>

        <#if product.chargeShipping?has_content && product.chargeShipping=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductChargeShipping}
              </@td>
              <@td colspan="3"><#if product.chargeShipping=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>