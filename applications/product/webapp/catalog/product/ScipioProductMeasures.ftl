<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section>
    <@table type="fields">
        <#-- measurements -->
        <#if product.productHeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductHeight}
              </@td>
                <@td colspan="3">${product.productHeight!""}
                                 <#if product.heightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("HeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productWidth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductWidth}
              </@td>
                <@td colspan="3">${product.productWidth!""}
                                 <#if product.widthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WidthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productDepth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductDepth}
              </@td>
                <@td colspan="3">${product.productDepth!""}
                                 <#if product.depthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DepthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productDiameter?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductDiameter}
              </@td>
                <@td colspan="3">${product.productDiameter!""}
                                 <#if product.diameterUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DiameterUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productWeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductWeight}
              </@td>
                <@td colspan="3">${product.productWeight!""}
                                 <#if product.weightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#-- Shipping info
        <#if product.shippingHeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingHeight}
              </@td>
                <@td colspan="3">${product.shippingHeight!""}
                                 <#if product.heightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("HeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingWidth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingWidth}
              </@td>
                <@td colspan="3">${product.shippingWidth!""}
                                 <#if product.widthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WidthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingDepth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingDepth}
              </@td>
                <@td colspan="3">${product.shippingDepth!""}
                                 <#if product.depthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DepthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>


        <#if product.shippingDiameter?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingDiameter}
              </@td>
                <@td colspan="3">${product.shippingDiameter!""}
                                 <#if product.diameterUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DiameterUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingWeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingWeight}
              </@td>
                <@td colspan="3">${product.shippingWeight!""}
                                 <#if product.weightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>
        -->

    </@table>


</@section>