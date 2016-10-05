<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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
    <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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

