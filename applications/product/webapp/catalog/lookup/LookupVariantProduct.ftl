<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if (requestAttributes.uiLabelMap)??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

<p>[${product.productId}] ${product.internalName}</p>

<#if searchFeatures?has_content>
  <@row>
    <@cell>
  <form method="post" action="<@ofbizUrl>LookupVariantProduct</@ofbizUrl>" name="selectAllForm">
        <input type="hidden" name="productId" value="${product.productId}" />
        <#list searchFeatures as searchFeature>
            <@field type="select" name=searchFeature.featureType label=(searchFeature.featureType)>
                <#assign features = searchFeature.features>
                <option value=""></option>
                <#list features as feature>
                  <#if searchFeature.selectedFeatureId?has_content && searchFeature.selectedFeatureId == feature.productFeatureId>
                    <option value="${feature.productFeatureId}" selected>${feature.get("description",locale)}</option>
                  <#else>
                    <option value="${feature.productFeatureId}">${feature.get("description",locale)}</option>
                  </#if>
                </#list>
            </@field>
        </#list>
        <@field type="submit" text=uiLabelMap.CommonSearch class="+${styles.link_run_sys!} ${styles.action_find!}" />
  </form>
    </@cell>    
  </@row>
</#if>

<#if variantProducts??>
  <@row>
    <@cell>
    <@table type="data-list">
       <@thead>
        <@tr class="header-row">
            <@td>${uiLabelMap.ProductProductId}</@td>
            <@td>${uiLabelMap.ProductBrandName}</@td>
            <@td>${uiLabelMap.ProductInternalName}</@td>
        </@tr>
        </@thead>
        <#list variantProducts as variant>
            <@tr>
                <@td><a class="${styles.link_nav_info_id!}" href="javascript:set_value('${variant.productId}')">${variant.productId}</a></@td>
                <@td>${variant.brandName!}</@td>
                <@td>${variant.internalName!}</@td>
            </@tr>
        </#list>
    </@table>
    </@cell>
  </@row>
</#if>
<#if productFeatureIds??>
  <@row>
    <@cell>
      <form method="post" action="<@ofbizUrl>LookupVariantProduct</@ofbizUrl>" name="createNewVariant">
        <@fields type="default-nolabelarea">
        <input type="hidden" name="productId" value="${product.productId}" />
        <input type="hidden" name="productFeatureIds" value="${productFeatureIds}" />
        <@field type="input" name="productVariantId" value=productVariantId />
        <@field type="submit" text=uiLabelMap.ProductQuickAddVariants class="+${styles.link_run_session!} ${styles.action_add!}" />
        </@fields>
      </form>
    </@cell>
  </@row>
</#if>

