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

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if productCategoryId?has_content>
    <@menuitem type="link" href=makeOfbizUrl("EditCategory?productCategoryId=${productCategoryId}") text="[${rawLabel('ProductBackToEditCategory')}]" class="+${styles.action_nav!} ${styles.action_cancel!}" />
  </#if>
  </@menu>
</#macro>
<#assign sectionTitle>
    <b>${uiLabelMap.ProductCheckingForExistingProductInCategory} <#if (productCategory.description)?has_content>"${productCategory.description}"</#if> [${uiLabelMap.CommonId}:${productCategoryId!}]</b>
    <#if productFeatureAndTypeDatas?has_content>
    ${uiLabelMap.CommonWhere}
        <#list productFeatureAndTypeDatas as productFeatureAndTypeData>
            <#assign productFeatureType = productFeatureAndTypeData.productFeatureType>
            <#assign productFeature = productFeatureAndTypeData.productFeature>
            ${productFeatureType.description} = ${productFeature.description}
            <#if productFeatureAndTypeData_has_next>, ${uiLabelMap.CommonAnd} </#if>
        </#list>
    </#if>
</#assign>
<@section title=wrapAsRaw(sectionTitle, 'htmlmarkup') menuContent=menuContent>
      <#if products?has_content>
        <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr>
                <@td>${uiLabelMap.ProductInternalName}</@td>
                <@td>${uiLabelMap.ProductProductName}</@td>
                <@td width="10%">&nbsp;</@td>
            </@tr>
          </@thead>
          <@tbody>
          <#list products as product>
            <@tr>
                <@td>${product.internalName?default("-no internal name-")} [${product.productId}]</@td>
                <@td>${product.productName?default("-no name-")} [${product.productId}]</@td>
                <@td width="10%"><a href="<@ofbizUrl>ViewProduct?productId=${product.productId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">[${uiLabelMap.ProductThisIsIt}]</a></@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoExistingProductsFound}.</@commonMsg>
      </#if>

      <@row>
        <@cell>
        <form name="createProductInCategoryForm" method="post" action="<@ofbizUrl>createProductInCategory</@ofbizUrl>">
            <input type="hidden" name="productCategoryId" value="${productCategoryId}" />
            <input type="hidden" name="pft_${productFeatureType.productFeatureTypeId}" value="${productFeature.productFeatureId}"/>
                <#list productFeatureAndTypeDatas! as productFeatureAndTypeData>
                <#assign productFeatureType = productFeatureAndTypeData.productFeatureType>
                <#assign productFeature = productFeatureAndTypeData.productFeature>
                <#assign productFeatureTypeId = productFeatureType.productFeatureTypeId>
                <@field type="display" label=(productFeatureType.description)>
                    ${productFeature.description}
                    <#if requestParameters["pftsel_" + productFeatureTypeId]??>
                        <input type="hidden" name="pftsel_${productFeatureTypeId}" value="Y"/>
                        [${uiLabelMap.ProductSelectable}]
                    <#else>
                        <input type="hidden" name="pftsel_${productFeatureTypeId}" value="N"/>
                        [${uiLabelMap.ProductStandard}]
                    </#if>
                </@field>
                </#list>
                <@field type="display" label=uiLabelMap.ProductInternalName>
                    <input type="hidden" name="internalName" value="${requestParameters.internalName!}"/>
                        <div>&nbsp;${requestParameters.internalName?default("&nbsp;")}</div>
                </@field>
                <@field type="display" label=uiLabelMap.ProductProductName>
                    <input type="hidden" name="productName" value="${requestParameters.productName!}"/>
                        <div>&nbsp;${requestParameters.productName?default("&nbsp;")}</div>
                </@field>
                <@field type="display" label=uiLabelMap.ProductShortDescription>
                    <input type="hidden" name="description" value="${requestParameters.description!}"/>
                        <div>&nbsp;${requestParameters.description?default("&nbsp;")}</div>
                </@field>
                <@field type="display" label=uiLabelMap.ProductDefaultPrice>
                    <input type="hidden" name="defaultPrice" value="${requestParameters.defaultPrice!}"/>
                        <input type="hidden" name="currencyUomId" value="${requestParameters.currencyUomId!}"/>
                        <div>&nbsp;${requestParameters.defaultPrice?default("&nbsp;")}&nbsp;${requestParameters.currencyUomId?default("&nbsp;")}</div>
                </@field>
                <@field type="display" label=uiLabelMap.ProductAverageCost>
                    <input type="hidden" name="averageCost" value="${requestParameters.averageCost!}"/>
                        <div>&nbsp;${requestParameters.averageCost?default("&nbsp;")}</div>
                </@field>
                <@field type="input" label=uiLabelMap.ProductNewProductId name="productId" value="" />
                <@field type="submit" text=uiLabelMap.ProductCreateNewProduct class="+${styles.link_run_sys!} ${styles.action_add!}"/>
        </form>
        </@cell>
      </@row>
</@section>