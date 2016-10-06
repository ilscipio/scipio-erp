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

<#assign isError = isError!false>

<#-- 2016-06-15: if a productId was specified but we got no product for it, then that
    is an error and shouldn't allow anything -->
<#if productId?has_content && !product?has_content && (parameters.isCreate!) != "true">

    <@commonMsg type="error">${getLabel("OrderProductNotFound", "OrderErrorUiLabels")!} (${productId})</@commonMsg>

<#else>

<@section>
<form name="EditProduct" action="<@ofbizUrl><#if product?has_content>updateProduct<#else>createProduct</#if></@ofbizUrl>" method="post">

  <#-- productParams is for fields that can safely accept user params values
    productParamsRestr is for fields that must not change after creation (so should explicitly disallow user params)
   -->
  <#if product?has_content>
    <#if (parameters.isCreate!) == "true">
      <#-- product that was just created; ignore submitted params so we reload the newly created ones -->
      <#assign productParams = {}>
      <#assign productParamsRestr = {}>
    <#else>
      <#-- existing product -->
      <#-- NOTE: it's possible we could want empty map in all cases instead of parameters (that is stock form widget behavior). 
        But for now, we can allow param overriding for more possibilities for pre-filling,
        and to preserve user input upon errors.
        TODO: analyze on larger scale
      <#assign productParams = {}>-->
      <#if (parameters.isProductChange!) == "true">
        <#--<#assign productParams = {}>-->
        <#if isError>
          <#assign productParams = parameters>
        <#else>
          <#assign productParams = {}>
        </#if>
      <#else>
        <#-- in this case, we could want to empty map instead... pros and cons... leave overridable for now -->
        <#--<#assign productParams = {}>-->
        <#assign productParams = parameters>
      </#if>
      <#assign productParamsRestr = {}>
    </#if>
  <#else>
    <#-- new product, not yet created -->
    <#assign productParams = parameters>
    <#assign productParamsRestr = parameters>
  </#if>

<#-- SCIPIO: 2016-06-15: according to upstream, some fields must be considered read-only after initial creation
    pass this map to args= for affected fields 
    NOTE: all those fields must also be changed to use productParamsRestr isntead of productParams! -->
<#assign readOnlyAfterCreateArgs = {
    "readonly":(product?has_content),
    "tooltip":uiLabelMap.ProductNotModificationRecreatingProduct
}>

    <#if !product?has_content>
      <input type="hidden" name="isCreate" value="true" />
    </#if>

    <input type="hidden" name="isProductChange" value="true" />

<#-- NOTE: only supports productParams atm -->
<#function checkSelected name optionVal defaultVal=false>
    <#if productParams[name]?has_content>
        <#return (productParams[name] == optionVal)/>
    <#elseif product[name]?has_content>
        <#return (product[name] == optionVal)/>
    <#elseif !defaultVal?is_boolean>
        <#return (defaultVal == optionVal)/>
    <#else>
        <#return false/>
    </#if>
</#function>

    <@row>
        <@cell>
            <#-- General info -->
            <@heading>${uiLabelMap.CommonOverview}</@heading>
          <#if product.productId?has_content>
            <@field type="display" name="productId" label=uiLabelMap.ProductProductId value=product.productId />
            <input type="hidden" name="productId" value="${product.productId}"/>
          <#else>
            <@field type="text" name="productId" label=uiLabelMap.ProductProductId value=(productParams.productId!) tooltip=uiLabelMap.CommonLeaveEmptyAutoValue />
          </#if>
            <@field type="text" name="productName" label=uiLabelMap.ProductProductName value=(productParams.productName!product.productName!) maxlength="255" required=true/>
            <@field type="text" name="internalName" label=uiLabelMap.ProductInternalName value=(productParams.internalName!product.internalName!) maxlength="255" required=true />
            <@field type="text" name="brandName" label=uiLabelMap.ProductBrandName value=(productParams.brandName!product.brandName!) maxlength="60"/>
            <@field type="select" label=uiLabelMap.ProductProductType name="productTypeId" required=true>
              <#assign options = delegator.findByAnd("ProductType",{},["description ASC"], true)>
                <#list options as option>
                    <@field type="option" value=(option.productTypeId!) selected=checkSelected("productTypeId", option.productTypeId, "FINISHED_GOOD")>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="text" name="manufacturerPartyId" label=uiLabelMap.ProductOemPartyId value=(productParams.manufacturerPartyId!product.manufacturerPartyId!) maxlength="20"/>
            <@field type="textarea" label=uiLabelMap.CommonComments id="comments" name="comments">${productParams.comments!product.comments!""}</@field>
            <@field type="checkbox" name="isVirtual" label=uiLabelMap.ProductVirtualProduct currentValue=(productParams.isVirtual!product.isVirtual!'N') value="Y" altValue="N"/>
            <@field type="checkbox" name="isVariant" label=uiLabelMap.ProductVariantProduct currentValue=(productParams.isVariant!product.isVariant!'N') value="Y" altValue="N"/>        
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
                    <@field type="option" value=(option.productCategoryId) selected=checkSelected("primaryProductCategoryId", option.productCategoryId)>${option.categoryName!} [${option.productCategoryId!}]</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Misc -->
            <@heading>${uiLabelMap.CommonMiscellaneous}</@heading>
            <@field type="checkbox" name="returnable" args=readOnlyAfterCreateArgs label=uiLabelMap.ProductReturnable currentValue=(productParamsRestr.returnable!product.returnable!'N') value="Y" altValue="N" />
            <@field type="checkbox" name="includeInPromotions" args=readOnlyAfterCreateArgs label=uiLabelMap.ProductIncludePromotions currentValue=(productParamsRestr.includeInPromotions!product.includeInPromotions!'N') value="Y" altValue="N" />
            <@field type="checkbox" name="taxable" args=readOnlyAfterCreateArgs label=uiLabelMap.ProductTaxable currentValue=(productParamsRestr.taxable!product.taxable!'N') value="Y" altValue="N" />
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Dates -->
            <@heading>${uiLabelMap.CommonDates}</@heading>
            <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonIntroductionDate name="introductionDate" value=(productParams.introductionDate!product.introductionDate!) size="25" maxlength="30" id="introductionDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.CommonReleaseDate name="releaseDate" value=(productParams.releaseDate!product.releaseDate!) size="25" maxlength="30" id="releaseDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.ProductSalesThruDate name="salesDiscontinuationDate" value=(productParams.salesDiscontinuationDate!product.salesDiscontinuationDate!) size="25" maxlength="30" id="salesDiscontinuationDate" />
            <@field type="datetime" dateType="datetime" label=uiLabelMap.ProductSupportThruDate name="supportDiscontinuationDate" value=(productParams.supportDiscontinuationDate!product.supportDiscontinuationDate!) size="25" maxlength="30" id="supportDiscontinuationDate" />
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- Rates -->
            <@heading>${uiLabelMap.CommonRate}</@heading>
            <@field type="text" name="productRating" label=uiLabelMap.ProductRating value=(productParams.productRating!product.productRating!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductRatingTypeEnum name="ratingTypeEnum">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Enumeration",{"enumTypeId":"PROD_RATING_TYPE"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.enumId!) selected=checkSelected("ratingTypeEnum", option.enumId)>${option.get("description", locale)} (${option.get("enumCode", locale)})</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#-- ShoppingCart -->
            <@heading>${uiLabelMap.CommonShoppingCart}</@heading>
            <@field type="checkbox" name="orderDecimalQuantity" args=readOnlyAfterCreateArgs label=uiLabelMap.ProductShippingBox currentValue=(productParamsRestr.orderDecimalQuantity!product.orderDecimalQuantity!'N') value="Y" altValue="N"/>
        </@cell>
    </@row>
    <@row>
        <@cell>
                     
            <#-- Inventory -->
            <@heading>${uiLabelMap.CommonInventory}</@heading>
            <@field type="checkbox" name="salesDiscWhenNotAvail" label=uiLabelMap.ProductSalesDiscontinuationNotAvailable currentValue=(productParams.salesDiscWhenNotAvail!product.salesDiscWhenNotAvail!'N') value="Y" altValue="N"/>
            <@field type="checkbox" name="requireInventory" label=uiLabelMap.ProductRequireInventory currentValue=(productParams.requireInventory!product.requireInventory!'N') value="Y" altValue="N" tooltip=uiLabelMap.ProductInventoryRequiredProduct/>
            <@field type="select" label=uiLabelMap.ProductRequirementMethodEnumId name="requirementMethodEnumId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Enumeration",{"enumTypeId":"PROD_REQ_METHOD"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.enumId!) selected=checkSelected("requirementMethodEnumId", option.enumId)>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="select" label=uiLabelMap.ProductLotId name="lotIdFilledIn">
                <#assign selectedLot = productParams.lotIdFilledIn!product.lotIdFilledIn!"">

                <@field type="option" value="Allowed" selected=(selectedLot =="Allowed")>${uiLabelMap.lotIdFilledInAllowed}</@field>
                <@field type="option" value="Mandatory" selected=(selectedLot =="Mandatory")>${uiLabelMap.lotIdFilledInMandatory}</@field>
                <@field type="option" value="Forbidden" selected=(selectedLot =="Forbidden")>${uiLabelMap.lotIdFilledInForbidden}</@field>
            </@field>
            <@field type="text" name="inventoryMessage" label=uiLabelMap.ProductInventoryMessage value=(productParams.inventoryMessage!product.inventoryMessage!) maxlength="255"/>
          <#-- SCIPIO: new from upstream since 2016-06-13 -->
          <#if product?has_content>
            <@field type="display" name="inventoryItemTypeId" label=uiLabelMap.ProductInventoryItemTypeId>
                ${(product.getRelatedOne("InventoryItemType").get("description", locale))!}
            </@field>
          <#else>
            <@field type="select" name="inventoryItemTypeId" label=uiLabelMap.ProductInventoryItemTypeId>
                <#assign invItemTypes = delegator.findByAnd("InventoryItemType", {}, ["description"], true)![]>
                <#list invItemTypes as invItemType>
                    <#if productParams.inventoryItemTypeId?has_content>
                        <#assign selected = (productParams.inventoryItemTypeId == invItemType.inventoryItemTypeId)/>
                    <#else>
                        <#-- SCIPIO: NOTE: unlike stock we will explicitly set NON_SERIAL_INV_ITEM as the default because it's the most common/sensible -->
                        <#assign selected = ("NON_SERIAL_INV_ITEM" == invItemType.inventoryItemTypeId)>
                    </#if>
                    <@field type="option" value=invItemType.inventoryItemTypeId selected=selected>${invItemType.get("description", locale)!invItemType.inventoryItemTypeId}</@field>
                </#list>
            </@field>
          </#if> 
        </@cell>
    </@row>

    <@row>
        <@cell>
            <#-- Amount -->
            <@heading>${uiLabelMap.CommonAmount}</@heading>
            <@field type="checkbox" name="requireAmount" label=uiLabelMap.ProductRequireAmount currentValue=(productParams.requireAmount!product.requireAmount!'N') value="Y" altValue="N"/>
            <@field type="select" label=uiLabelMap.ProductAmountUomTypeId name="amountUomTypeId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("UomType",{},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomTypeId!) selected=checkSelected("amountUomTypeId", option.uomTypeId!)>${option.get("description", locale)}</@field>
                </#list>
            </@field>
        </@cell>
    </@row>
    <@row>
        <@cell>
           <#-- Measures -->
            <@heading>${uiLabelMap.CommonMeasures}</@heading>
            <@field type="text" name="productHeight" label=uiLabelMap.ProductProductHeight value=(productParams.productHeight!product.productHeight!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductHeightUomId name="heightUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("heightUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productWidth" label=uiLabelMap.ProductProductWidth value=(productParams.productWidth!product.productWidth!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductWidthUomId name="widthUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("widthUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
            <@field type="text" name="productDepth" label=uiLabelMap.ProductProductDepth value=(productParams.productDepth!product.productDepth!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductDepthUomId name="depthUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("depthUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productDiameter" label=uiLabelMap.ProductProductDiameter value=(productParams.productDiameter!product.productDiameter!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductDiameterUomId name="diameterUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"LENGTH_MEASURE"},["description ASC"], true)/>
                <#list options as option>>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("diameterUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
        
            <@field type="text" name="productWeight" label=uiLabelMap.ProductProductWeight value=(productParams.productWeight!product.productWeight!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductWeightUomId name="weightUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{"uomTypeId":"WEIGHT_MEASURE"},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("weightUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
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
            <@field type="text" name="quantityIncluded" label=uiLabelMap.ProductQuantityIncluded value=(productParams.quantityIncluded!product.quantityIncluded!) maxlength="255"/>
            <@field type="select" label=uiLabelMap.ProductQuantityUomId name="quantityUomId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("Uom",{},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.uomId!) selected=checkSelected("quantityUomId", option.uomId!)>${option.get("description", locale)} (${option.get("abbreviation", locale)})</@field>
                </#list>
            </@field>
            <@field type="text" name="piecesIncluded" label=uiLabelMap.ProductPiecesIncluded value=(productParams.piecesIncluded!product.piecesIncluded!) maxlength="20"/>
            <@field type="checkbox" name="inShippingBox" label=uiLabelMap.ProductShippingBox currentValue=(productParams.inShippingBox!product.inShippingBox!'N') value="Y" altValue="N"/>
            <@field type="select" label=uiLabelMap.ProductDefaultShipmentBoxTypeId name="defaultShipmentBoxTypeId">
                <@field type="option" value=""></@field>
                <#assign options =  delegator.findByAnd("ShipmentBoxType",{},["description ASC"], true)/>
                <#list options as option>
                    <@field type="option" value=(option.shipmentBoxTypeId!) selected=checkSelected("defaultShipmentBoxTypeId", option.shipmentBoxTypeId!)>${option.get("description", locale)}</@field>
                </#list>
            </@field>
            <@field type="checkbox" name="chargeShipping" label=uiLabelMap.ProductChargeShipping currentValue=(productParams.chargeShipping!product.chargeShipping!'N') value="Y" altValue="N"/>
        </@cell>
    </@row>
    <@row>
        <@cell>
            <#if product?has_content>
                <@field type="submit" text=uiLabelMap.ProductUpdateProduct class="+${styles.link_run_sys!} ${productActionClass!}"/>
            <#else>
                <@field type="submit" text=uiLabelMap.ProductCreateProduct class="+${styles.link_run_sys!} ${productActionClass!}"/>
            </#if>
        </@cell>
    </@row>
</form>

</@section>

</#if>