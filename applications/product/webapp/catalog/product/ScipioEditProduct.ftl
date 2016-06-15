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
<#if !product?has_content>
    <#assign product = {}/>
</#if>


<@section>
<form name="EditProduct" target="<@ofbizUrl>updateProduct</@ofbizUrl>" method="post">
    <@row>
        <@cell>
            <#-- General info -->
            <@heading>${uiLabelMap.CommonOverview}</@heading>
            <@field type="text" name="productId" label=uiLabelMap.ProductProductId value="${product.productId!}"/>
            <@field type="text" name="productName" label=uiLabelMap.ProductProductName value="${product.productName!}" maxlength="255"/>
            <@field type="text" name="internalName" label=uiLabelMap.ProductInternalName value="${product.internalName!}" maxlength="255"/>
            <@field type="text" name="brandName" label=uiLabelMap.ProductBrandName value="${product.brandName!}" maxlength="60"/>
            <@field type="select" label=uiLabelMap.ProductProductType name="productTypeId">
              <#assign options =  delegator.findByAnd("ProductType",{},["description ASC"], true)>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.productTypeId?has_content && product.productTypeId == option.productTypeId>
                            <#assign selected = true/>
                        <#elseif option.productTypeId=="FINISHED_GOOD">
                            <#assign selected = true/>
                        <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                        <#if option.productTypeId=="FINISHED_GOOD">
                            <#assign selected = true/>
                        <#else>
                            <#assign selected = false/>
                        </#if>
                    </#if>
                    <@field type="option" value="${option.productTypeId!}" selected=selected>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="text" name="manufacturerPartyId" label=uiLabelMap.ProductOemPartyId value="${product.manufacturerPartyId!}" maxlength="20"/>
            <@field type="textarea" label=uiLabelMap.CommonComments id="comments" name="comments">${product.comments!""}</@field>
            <@field type="checkbox" name="isVirtual" label=uiLabelMap.ProductVirtualProduct value="${product.isVirtual?default('N')}"/>
            <@field type="checkbox" name="isVariant" label=uiLabelMap.ProductVariantProduct value="${product.isVariant?default('N')}"/>        
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Category -->
            <@heading>${uiLabelMap.ProductPrimaryCategory}</@heading>
            <@field type="select" label=uiLabelMap.ProductPrimaryCategory name="primaryProductCategoryId">
                  <@field type="option" value=""></@field>
                  <#assign options =  delegator.findByAnd("ProductCategory",{},["categoryName ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.primaryProductCategoryId?has_content && product.primaryProductCategoryId == option.productCategoryId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.productCategoryId}" selected=selected>${option.categoryName!} [${option.productCategoryId!}]</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Misc -->
            <@heading>${uiLabelMap.CommonMiscellaneous}</@heading>
            <@field type="checkbox" name="returnable" label=uiLabelMap.ProductReturnable value="${product.returnable?default('N')}" readonly=(product?has_content)/>
            <@field type="checkbox" name="includeInPromotions" label=uiLabelMap.ProductIncludePromotions value="${product.includeInPromotions?default('N')}" readonly=(product?has_content)/>
            <@field type="checkbox" name="taxable" label=uiLabelMap.ProductTaxable value="${product.taxable?default('N')}" readonly=(product?has_content)/>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Dates -->
            <@heading>${uiLabelMap.CommonDates}</@heading>
            <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonIntroductionDate name="introductionDate" value=(product.introductionDate!) size="25" maxlength="30" id="introductionDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonReleaseDate name="releaseDate" value=(product.releaseDate!) size="25" maxlength="30" id="releaseDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.ProductSalesThruDate name="salesDiscontinuationDate" value=(product.salesDiscontinuationDate!) size="25" maxlength="30" id="salesDiscontinuationDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.ProductSupportThruDate name="supportDiscontinuationDate" value=(product.supportDiscontinuationDate!) size="25" maxlength="30" id="supportDiscontinuationDate" />
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Rates -->
            <@heading>${uiLabelMap.CommonRate}</@heading>
            <@field type="text" name="productRating" label=uiLabelMap.ProductRating value="${product.productRating!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductRatingTypeEnum name="ratingTypeEnum">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Enumeration",{"enumTypeId":"PROD_RATING_TYPE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.ratingTypeEnum?has_content && product.ratingTypeEnum == option.enumTypeId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.enumTypeId!}" selected=selected>${option.get("description", locale)} (${option.get("enumCode", locale)})</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- ShoppingCart -->
            <@heading>${uiLabelMap.CommonShoppingCart}</@heading>
            <@field type="checkbox" name="orderDecimalQuantity" label=uiLabelMap.ProductShippingBox value="${product.orderDecimalQuantity?default('N')}" readonly=(product?has_content)/>
        </@cell>
    </@row>
    <@row>
        <@cell>
                     
            <#-- Inventory -->
            <@heading>${uiLabelMap.CommonInventory}</@heading>
            <@field type="checkbox" name="salesDiscWhenNotAvail" label=uiLabelMap.ProductSalesDiscontinuationNotAvailable value="${product.salesDiscWhenNotAvail?default('N')}"/>
            <@field type="checkbox" name="requireInventory" label=uiLabelMap.ProductRequireInventory value="${product.requireInventory?default('N')}" tooltip="${uiLabelMap.ProductInventoryRequiredProduct}"/>
            <@field type="select" label=uiLabelMap.ProductRequirementMethodEnumId name="requirementMethodEnumId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Enumeration",{"enumTypeId":"PROD_REQ_METHOD"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.requirementMethodEnumId?has_content && product.requirementMethodEnumId == option.enumId>
                            <#assign selected = true/>
                        <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.enumId!}" selected=selected>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="select" label=uiLabelMap.ProductLotId name="lotIdFilledIn">
                <@field type="option" value="Allowed" selected=(product.lotIdFilledIn?has_content && product.lotIdFilledIn =="Allowed")>${uiLabelMap.lotIdFilledInAllowed}</@field>
                <@field type="option" value="Mandatory" selected=(product.lotIdFilledIn?has_content && product.lotIdFilledIn =="Mandatory")>${uiLabelMap.lotIdFilledInMandatory}</@field>
                <@field type="option" value="Forbidden" selected=(product.lotIdFilledIn?has_content && product.lotIdFilledIn =="Forbidden")>${uiLabelMap.lotIdFilledInForbidden}</@field>
            </@field>
            <@field type="text" name="inventoryMessage" label=uiLabelMap.ProductInventoryMessage value="${product.inventoryMessage!}" maxlength="255"/>
          <#-- SCIPIO: new from upstream since 2016-06-13 -->
          <#if product?has_content>
            <@field type="display" name="inventoryItemTypeId" label=uiLabelMap.ProductInventoryItemTypeId>
                ${(product.getRelatedOne("InventoryItemType").get("description", locale))!}
            </@field>
          <#else>
            <@field type="select" name="inventoryItemTypeId" label=uiLabelMap.ProductInventoryItemTypeId>
                <#assign invItemTypes = delegator.findByAnd("InventoryItemType", {}, ["description"], true)![]>
                <#list invItemTypes as invItemType>
                    <@field type="option" value=invItemType.inventoryItemTypeId>${invItemType.get("description", locale)!invItemType.inventoryItemTypeId}</@field>
                </#list>
            </@field>
          </#if> 
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Amount -->
            <@heading>${uiLabelMap.CommonAmount}</@heading>
            <@field type="checkbox" name="requireAmount" label=uiLabelMap.ProductRequireAmount value="${product.requireAmount?default('N')}"/>
            <@field type="select" label=uiLabelMap.ProductAmountUomTypeId name="amountUomTypeId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("UomType",{},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.amountUomTypeId?has_content && product.amountUomTypeId == option.amountUomTypeId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomTypeId!}" selected=selected>${option.get("description", locale)}</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
           <#-- Measures -->
            <@heading>${uiLabelMap.CommonMeasures}</@heading>
            <@field type="text" name="productHeight" label=uiLabelMap.ProductProductHeight value="${product.productHeight!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductHeightUomId name="heightUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.heightUomId?has_content && product.heightUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productWidth" label=uiLabelMap.ProductProductWidth value="${product.productWidth!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductWidthUomId name="widthUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.widthUomId?has_content && product.widthUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
            <@field type="text" name="productDepth" label=uiLabelMap.ProductProductDepth value="${product.productDepth!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductDepthUomId name="depthUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.depthUomId?has_content && product.depthUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productDiameter" label=uiLabelMap.ProductProductDiameter value="${product.productDiameter!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductDiameterUomId name="diameterUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.diameterUomId?has_content && product.diameterUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productWeight" label=uiLabelMap.ProductProductWeight value="${product.productWeight!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductWeightUomId name="weightUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"WEIGHT_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.weightUomId?has_content && product.weightUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
        
            <#--
                <#if product.largeImageUrl?has_content>
                        <@tr>
                          <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLargeImage}</@td>
                          SCIPIO: The inline styles should probably be replaced by the th and img-thumgnail classes for foundation/bootstrap 
                          <@td colspan="3"><img src="${product.largeImageUrl!}" style="max-width: 100%; height: auto" width="100"/></@td>
                        </@tr>
                </#if>
        
         
        
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
             <#if product.contentInfoText?has_content && product.contentInfoText=="Y">   
                    <@tr>
                      <@td class="${styles.grid_large!}2">${uiLabelMap.ProductContentInfoText}
                      </@td>
                      <@td colspan="3"><#if product.contentInfoText=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
                    </@tr>
                </#if>
        
                -->
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Shipping -->
            <@heading>${uiLabelMap.CommonShipping}</@heading>
            <@field type="text" name="quantityIncluded" label=uiLabelMap.ProductQuantityIncluded value="${product.quantityIncluded!}" maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductQuantityUomId name="quantityUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.quantityUomId?has_content && product.quantityUomId == option.uomId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.uomId!}" selected=selected>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
            <@field type="text" name="piecesIncluded" label=uiLabelMap.ProductPiecesIncluded value="${product.piecesIncluded!}" maxlength="20"/>
            <@field type="checkbox" name="inShippingBox" label=uiLabelMap.ProductShippingBox value="${product.inShippingBox?default('N')}"/>
            <@field type="select" label=uiLabelMap.ProductDefaultShipmentBoxTypeId name="defaultShipmentBoxTypeId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("ShipmentBoxType",{},["description ASC"], true)/>
                <#list options as option>
                    <#if product?has_content>
                        <#if product.defaultShipmentBoxTypeId?has_content && product.defaultShipmentBoxTypeId == option.shipmentBoxTypeId>
                            <#assign selected = true/>
                         <#else>
                            <#assign selected = false/>
                        </#if>
                    <#else>
                         <#assign selected = false/>
                    </#if>
                    <@field type="option" value="${option.shipmentBoxTypeId!}" selected=selected>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="checkbox" name="chargeShipping" label=uiLabelMap.ProductChargeShipping value="${product.chargeShipping?default('N')}"/>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#if product?has_content>
                <@field type="submit" text="${uiLabelMap.ProductUpdateProduct}" class="+${styles.link_run_sys!} ${productActionClass!}"/>
            <#else>
                <@field type="submit" text="${uiLabelMap.ProductCreateProduct}" class="+${styles.link_run_sys!} ${productActionClass!}"/>
            </#if>
        </@cell>
    </@row>
</form>

</@section>