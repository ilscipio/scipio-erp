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
<@section>
    <#-- General info -->
    <@table type="fields">
        <#-- 
        <#if product.productId?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductId}
              </@td>
              <@td colspan="3">${product.productId}</@td>
            </@tr>
        </#if>-->

        <#if product.largeImageUrl?has_content>
                <@tr>
                  <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLargeImage}</@td>
                  <@td colspan="3"><@img src=(product.largeImageUrl!) height="150px" width="100px" type="contain"/></@td>
                </@tr>
        </#if>

        <#if product.productName?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductName}
              </@td>
              <@td colspan="3">${product.productName}</@td>
            </@tr>
        </#if>

        <#if product.internalName?has_content && (product.productName?has_content && product.productName != product.internalName)>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductInternalName}
              </@td>
              <@td colspan="3">${product.internalName}</@td>
            </@tr>
        </#if>

        <#if product.brandName?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductBrandName}
              </@td>
              <@td colspan="3">${product.brandName}</@td>
            </@tr>
        </#if>

        <#if product.productTypeId?has_content>
            <#assign productType = product.getRelatedOne("ProductType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductType}
              </@td>
              <@td colspan="3">${(productType.get("description",locale))!}</@td>
            </@tr>
        </#if>

        <#if product.manufacturerPartyId?has_content>
            <#assign manufacturerParty = product.getRelatedOne("ManufacturerParty", false)!/>
            <#assign manufacturer = manufacturerParty.getRelatedOne("PartyGroup", false)!/>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductOemPartyId}
              </@td>
              <@td colspan="3"><a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${product.manufacturerPartyId!}</@ofbizInterWebappUrl>">${(manufacturer.get("description",locale))!product.manufacturerPartyId}</a></@td>
            </@tr>
        </#if>

        <#if product.comments?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.Productomments}
              </@td>
              <@td colspan="3">${product.comments!}</@td>
            </@tr>    
        </#if>
    </@table>
</@section>