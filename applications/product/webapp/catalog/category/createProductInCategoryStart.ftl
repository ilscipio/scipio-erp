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

<#assign productFeaturesByTypeMap = Static["org.ofbiz.product.feature.ParametricSearch"].makeCategoryFeatureLists(productCategoryId, delegator)>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if productCategoryId?has_content>
    <@menuitem type="link" href=makeOfbizUrl("EditCategory?productCategoryId=${productCategoryId}") text="[${rawLabel('ProductBackToEditCategory')}]" class="+${styles.action_nav!} ${styles.action_cancel!}" />
  </#if>
  </@menu>
</#macro>
<@section menuContent=menuContent>
        <form name="createProductInCategoryCheckExistingForm" method="post" action="<@ofbizUrl>CreateProductInCategoryCheckExisting</@ofbizUrl>">
            <input type="hidden" name="productCategoryId" value="${productCategoryId}" />
            <#list productFeaturesByTypeMap.keySet() as productFeatureTypeId>
                <#assign findPftMap = {"productFeatureTypeId":productFeatureTypeId}>
                <#assign productFeatureType = delegator.findOne("ProductFeatureType", findPftMap, true)>
                <#assign productFeatures = productFeaturesByTypeMap[productFeatureTypeId]>
                <@field type="generic" label=(productFeatureType.description)>
                    <@field type="select" name="pft_${productFeatureTypeId}">
                        <option value="">- ${uiLabelMap.CommonNone} -</option>
                        <#list productFeatures as productFeature>
                            <option value="${productFeature.productFeatureId}">${productFeature.description}</option>
                        </#list>
                    </@field>
                    <@field type="checkbox" name="pftsel_${productFeatureTypeId}" label=(uiLabelMap.ProductSelectable!)/>
                </@field>
            </#list>
                <@field type="input" label=uiLabelMap.ProductInternalName name="internalName" size="30" maxlength="60"/>
                <@field type="input" label=uiLabelMap.ProductProductName name="productName" size="30" maxlength="60"/>
                <@field type="input" label=uiLabelMap.ProductShortDescription name="description" size="60" maxlength="250"/>
                <@field type="generic" label=uiLabelMap.ProductDefaultPrice>
                    <@field type="input" name="defaultPrice" size="8"/>
                    <#assign findCurrenciesMap = {"uomTypeId":"CURRENCY_MEASURE"}>
                    <#assign currencies = delegator.findByAnd('Uom', findCurrenciesMap, null, true) />
                    <#if currencies?has_content && (currencies?size > 0)>
                        <@field type="select" name="currencyUomId">
                            <option value=""></option>
                            <#list currencies as currency>
                                <option value="${currency.uomId}">${currency.get("description",locale)} [${currency.uomId}]</option>
                            </#list>
                        </@field>
                    </#if>
                </@field>
                <@field type="input" label=uiLabelMap.ProductAverageCost name="averageCost" size="8"/>
                <@field type="submit" submitType="link" href="javascript:document.createProductInCategoryCheckExistingForm.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.ProductCheckExisting />
        </form>
</@section>
