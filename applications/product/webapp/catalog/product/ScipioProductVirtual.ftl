<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.ProductVirtualFieldGroup>

    <@table type="fields">
        <#if product.isVirtual?has_content && product.isVirtual=="Y">
            <@tr>
                <@td class="${styles.grid_large!}2">${uiLabelMap.ProductVirtualProduct}
                </@td>
                <@td colspan="3">
                    <#if product.isVirtual=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if>
                    <#if product.virtualVariantMethodEnum?has_content>
                        <#assign virtualVariantEnum = product.getRelatedOne("VirtualVariantMethodEnumeration", true)/>
                        (${(virtualVariantEnum.get("description",locale))!})
                    </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.isVariant?has_content && product.isVariant=="Y">
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductVariantProduct}
              </@td>
              <@td colspan="3"><#if product.isVariant=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>