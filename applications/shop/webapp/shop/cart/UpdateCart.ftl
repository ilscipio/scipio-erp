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

<#if shoppingCart?has_content && (shoppingCart.size() > 0)>
  <@section title="${rawString(uiLabelMap.EcommerceStep)} 1: ${rawString(uiLabelMap.PageTitleShoppingCart)}">
  <div id="cartSummaryPanel" style="display: none;">
    <a href="javascript:void(0);" id="openCartPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
    <#-- Scipio: Always disable responsive on this one or it won't play nice with JS... -->
    <@table type="data-list" responsive=false id="cartSummaryPanel_cartItems" summary="This table displays the list of item added into Shopping Cart.">
      <@thead>
        <@tr>
          <@th width="25%" id="orderItem">${uiLabelMap.ProductProduct}</@th>
          <@th width="15%" id="description" class="${styles.text_right!}"></@th>
          <@th width="15%" id="quantity" class="${styles.text_right!}">${uiLabelMap.CommonQuantity}</@th>
          <@th width="10%" id="unitPrice" class="${styles.text_right!}">${uiLabelMap.EcommerceUnitPrice}</@th>
          <@th width="15%" id="adjustment" class="${styles.text_right!}">${uiLabelMap.EcommerceAdjustments}</@th>
          <@th width="15%" id="itemTotal" class="${styles.text_right!}">${uiLabelMap.EcommerceItemTotal}</@th>
          <@td>&nbsp;</@td>
        </@tr>
      </@thead>
      <@tbody>
        <#list shoppingCart.items() as cartLine>
          <@tr id="cartItemDisplayRow_${cartLine_index}">
            <@td headers="orderItem">
              <#if cartLine.getProductId()??>
                <#-- product item -->
                <#if cartLine.getParentProductId()??>
                  <#assign parentProductId = cartLine.getParentProductId() />
                <#else>
                  <#assign parentProductId = cartLine.getProductId() />
                </#if>
                <#-- SCIPIO: Uncomment if you want to use the image placeholders
                  <#assign smallImageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(cartLine.getProduct(), "SMALL_IMAGE_URL", locale, dispatcher, "url")! />
                  <#if !smallImageUrl?string?has_content><#assign smallImageUrl = "" /></#if>
                  <img src="<@ofbizContentUrl>${requestAttributes.contentPathPrefix!}${smallImageUrl}</@ofbizContentUrl>" alt="Product Image" />
                -->
                <a href="<@ofbizCatalogAltUrl productId=parentProductId/>" class="${styles.link_nav_info_idname!}" target="_blank">${cartLine.getProductId()!} - ${cartLine.getName()!}</a>
                <#-- For configurable products, the selected options are shown -->
                <#if cartLine.getConfigWrapper()??>
                  <#assign selectedOptions = cartLine.getConfigWrapper().getSelectedOptions()! />
                  <#if selectedOptions??>
                    <ul class="order-item-attrib-list">
                    <#list selectedOptions as option>
                        <li>${option.getDescription()}</li>
                    </#list>
                    </ul>
                  </#if>
                </#if>
                <#assign attrs = cartLine.getOrderItemAttributes()/>
                <#if attrs?has_content>
                    <#assign attrEntries = attrs.entrySet()/>
                    <ul class="order-item-attrib-list">
                    <#list attrEntries as attrEntry>
                        <li>
                            ${attrEntry.getKey()}: ${attrEntry.getValue()}
                        </li>
                    </#list>
                    </ul>
                </#if>
              <#else>
                <#-- non-product item -->
                ${cartLine.getItemTypeDescription()!}: ${cartLine.getName()!}  
                <#assign attrs = cartLine.getOrderItemAttributes()/>
                <#if attrs?has_content>
                    <#assign attrEntries = attrs.entrySet()/>
                    <ul class="order-item-attrib-list">
                    <#list attrEntries as attrEntry>
                        <li>
                            ${attrEntry.getKey()}: ${attrEntry.getValue()}
                        </li>
                    </#list>
                    </ul>
                </#if>
              </#if>
            </@td>
            <@td headers="description"></@td>
            <@td headers="quantity" class="${styles.text_right!}"><span id="completedCartItemQty_${cartLine_index}">${cartLine.getQuantity()?string.number}</span></@td>
            <@td headers="unitPrice" class="${styles.text_right!}">${cartLine.getDisplayPrice()}</@td>
            <@td headers="adjustment" class="${styles.text_right!}"><span id="completedCartItemAdjustment_${cartLine_index}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency() /></span></@td>
            <@td headers="itemTotal" align="right" class="${styles.text_right!}"><span id="completedCartItemSubTotal_${cartLine_index}"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency() /></span></@td>
            <@td>&nbsp;</@td>
            </@tr>
        </#list>
        <#--Scipio: styling issues: 
        </@tbody>
        <@tfoot>-->
        <@tr>
            <@td colspan="5"></@td>
            <@td colspan="1"><hr /></@td>
            <@td>&nbsp;</@td>
        </@tr>
        <@tr id="completedCartSubtotalRow">
          <@td id="subTotal" scope="row" colspan="5" class="${styles.text_right!}">${uiLabelMap.CommonSubtotal}</@td>
          <@td headers="subTotal" id="completedCartSubTotal" nowrap="nowrap" class="${styles.text_right!}"><@ofbizCurrency amount=shoppingCart.getSubTotal() isoCode=shoppingCart.getCurrency() /></@td>
          <@td>&nbsp;</@td>
        </@tr>
        <#assign orderAdjustmentsTotal = 0 />
        <#list shoppingCart.getAdjustments() as cartAdjustment>
          <#assign orderAdjustmentsTotal = orderAdjustmentsTotal + Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(cartAdjustment, shoppingCart.getSubTotal()) />
        </#list>
        <@tr id="completedCartDiscountRow">
          <@td id="productDiscount" scope="row" colspan="5" class="${styles.text_right!}">${uiLabelMap.ProductDiscount}</@td>
          <@td headers="productDiscount" id="completedCartDiscount" nowrap="nowrap" class="${styles.text_right!}"><input type="hidden" value="${orderAdjustmentsTotal}" id="initializedCompletedCartDiscount" /><@ofbizCurrency amount=orderAdjustmentsTotal isoCode=shoppingCart.getCurrency() /></@td>
          <@td>&nbsp;</@td>
        </@tr>
        <@tr>
          <@td id="shippingAndHandling" scope="row" colspan="5" class="${styles.text_right!}">${uiLabelMap.OrderShippingAndHandling}</@td>
          <@td headers="shippingAndHandling" id="completedCartTotalShipping" nowrap="nowrap" class="${styles.text_right!}"><@ofbizCurrency amount=shoppingCart.getTotalShipping() isoCode=shoppingCart.getCurrency() /></@td>
          <@td>&nbsp;</@td>
        </@tr>
        <#-- tax adjustments -->
          <@tr>
            <@td id="salesTax" scope="row" colspan="5" class="${styles.text_right!}">${uiLabelMap.OrderTotalSalesTax}</@td>
            <@td nowrap="nowrap" class="${styles.text_right!}" headers="salesTax" id="completedCartTotalSalesTax"><@ofbizCurrency amount=shoppingCart.getDisplayTaxIncluded() isoCode=shoppingCart.getCurrency()/></@td>
            <@td>&nbsp;</@td>
          </@tr>
        
        <@tr>
            <@td colspan="5"></@td>
            <@td colspan="1"><hr /></@td>
            <@td>&nbsp;</@td>
        </@tr>
        <@tr>
          <@td id="grandTotal" scope="row" colspan="5" class="${styles.text_right!}"><strong>${uiLabelMap.CommonTotal}</strong></@td>
          <@td headers="grandTotal" id="completedCartDisplayGrandTotal" nowrap="nowrap" class="${styles.text_right!}">
                <strong><@ofbizCurrency amount=shoppingCart.getDisplayGrandTotal() isoCode=shoppingCart.getCurrency() /></strong>
           </@td>
           <@td>&nbsp;</@td>     
        </@tr>
      <#--Scipio: styling issues: 
      </@tfoot>-->
        </@tbody>
    </@table>
  </div>
  <div id="editCartPanel">
    <form id="cartForm" method="post" action="<@ofbizUrl></@ofbizUrl>">
      <fieldset>
        <input type="hidden" name="removeSelected" value="false" />
        
        <@alert type="error" containerId="cartFormServerError_container" containerStyle="display:none;">
          <div id="cartFormServerError" class="errorMessage"></div>
        </@alert>

        <#-- Scipio: Always disable responsive on this one or it won't play nice with JS... -->
        <@table type="data-list" responsive=false id="editCartPanel_cartItems">
          <@thead>
            <@tr>
              <@th width="25%" id="editOrderItem">${uiLabelMap.ProductProduct}</@th>
              <@th width="15%" id="editDescription" class="${styles.text_right!}"></@th>
              <@th width="15%" id="editQuantity" class="${styles.text_right!}">${uiLabelMap.CommonQuantity}</@th>
              <@th width="10%" id="editUnitPrice" class="${styles.text_right!}">${uiLabelMap.EcommerceUnitPrice}</@th>
              <@th width="15%" id="editAdjustment" class="${styles.text_right!}">${uiLabelMap.EcommerceAdjustments}</@th>
              <@th width="15%" id="editItemTotal" class="${styles.text_right!}">${uiLabelMap.EcommerceItemTotal}</@th>
              <@th id="removeItem" class="${styles.text_right!}"></@th>
            </@tr>
          </@thead>
          <@tbody id="updateBody">
            <#list shoppingCart.items() as cartLine>
              <@tr id="cartItemRow_${cartLine_index}">
                <@td headers="editOrderItem">
                  <#if cartLine.getProductId()??>
                    <#-- product item -->
                    <#if cartLine.getParentProductId()??>
                      <#assign parentProductId = cartLine.getParentProductId() />
                    <#else>
                      <#assign parentProductId = cartLine.getProductId() />
                    </#if>
                      <#-- SCIPIO: Uncomment if you want to use the image placeholders
                      <#if cartLine.getProductId()??>
                        <#if cartLine.getParentProductId()??>
                          <#assign parentProductId = cartLine.getParentProductId() />
                        <#else>
                          <#assign parentProductId = cartLine.getProductId() />
                        </#if>
                        <#assign smallImageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(cartLine.getProduct(), "SMALL_IMAGE_URL", locale, dispatcher, "url")! />
                        <#if !smallImageUrl?string?has_content><#assign smallImageUrl=""></#if>
                        <#if smallImageUrl?string?has_content>
                          <#assign imgUrl><@ofbizContentUrl>${requestAttributes.contentPathPrefix!}${smallImageUrl}</@ofbizContentUrl></#assign>
                          <@img src=imgUrl width="150px;" height="75px"/>
                        </#if>
                      -->
                    <a href="<@ofbizCatalogAltUrl productId=parentProductId/>" class="${styles.link_nav_info_idname!}" target="_blank">${cartLine.getProductId()!} - ${cartLine.getName()!}</a>
                    <#-- For configurable products, the selected options are shown -->
                    <#if cartLine.getConfigWrapper()??>
                      <#assign selectedOptions = cartLine.getConfigWrapper().getSelectedOptions()! />
                      <#if selectedOptions??>
                        <ul class="order-item-attrib-list">
                        <#list selectedOptions as option>
                            <li>${option.getDescription()}</li>
                        </#list>
                        </ul>
                      </#if>
                    </#if>
                    <#assign attrs = cartLine.getOrderItemAttributes()/>
                    <#if attrs?has_content>
                        <#assign attrEntries = attrs.entrySet()/>
                        <ul class="order-item-attrib-list">
                        <#list attrEntries as attrEntry>
                            <li>
                                ${attrEntry.getKey()}: ${attrEntry.getValue()}
                            </li>
                        </#list>
                        </ul>
                    </#if>
                  <#else>
                    <#-- non-product item -->
                    ${cartLine.getItemTypeDescription()!}: ${cartLine.getName()!} 
                    <#assign attrs = cartLine.getOrderItemAttributes()/>
                    <#if attrs?has_content>
                        <#assign attrEntries = attrs.entrySet()/>
                        <ul class="order-item-attrib-list">
                        <#list attrEntries as attrEntry>
                            <li>
                                ${attrEntry.getKey()}: ${attrEntry.getValue()}
                            </li>
                        </#list>
                        </ul>
                    </#if>
                  </#if>
                </@td>
                <@td headers="editDescription"></@td>
                <@td headers="editQuantity" class="${styles.text_right!}">
                  <#compress>
                        <#if cartLine.getIsPromo() || cartLine.getShoppingListId()??>
                            ${cartLine.getQuantity()?string.number}
                        <#else><#-- Is Promo or Shoppinglist -->
                            <input type="hidden" name="cartLineProductId" id="cartLineProductId_${cartLine_index}" value="${cartLine.getProductId()}" /> 
                            <@field type="select" inline=true name="update${cartLine_index}" id="qty_${cartLine_index}" class="+validate-number">
                                <#list 1..99 as x>
                                    <#if cartLine.getQuantity()==x>
                                        <#assign selected = true/>
                                    <#else>
                                        <#assign selected = false/>
                                    </#if>
                                    <@field type="option" value=(x) selected=selected>${x}</@field>
                                </#list>
                            </@field>
                            <span id="advice-required-qty_${cartLine_index}" style="display:none;" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                            <span id="advice-validate-number-qty_${cartLine_index}" style="display:none;" class="errorMessage"> (${uiLabelMap.CommonPleaseEnterValidNumberInThisField}) </span>
                        </#if>
                    </#compress>
                </@td>
                <@td headers="editUnitPrice" id="itemUnitPrice_${cartLine_index}" class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayPrice() isoCode=shoppingCart.getCurrency() /></@td>
                <#if !cartLine.getIsPromo()>
                  <@td headers="editAdjustment" id="addPromoCode_${cartLine_index}" class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency() /></@td>
                <#else>
                  <@td headers="editAdjustment" class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency() /></@td>
                </#if>
                <@td headers="editItemTotal" id="displayItem_${cartLine_index}" class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency() /></@td>
                <#if !cartLine.getIsPromo()>
                  <@td class="${styles.text_center!}"><a id="removeItemLink_${cartLine_index}" href="javascript:void(0);" style="font-size: 20px;" title="${uiLabelMap.FormFieldTitle_removeButton}"><i class="fa fa-trash-o"></i></a></@td>
                </#if>
              </@tr>
            </#list>
            <#--Scipio: styling issues: 
            </@tbody>
            <@tfoot>-->
            <@tr>
                <@td colspan="5"></@td>
                <@td colspan="1"><hr /></@td>
                <@td>&nbsp;</@td>
            </@tr>

            <@tr>
                <@td colspan="5" class="${styles.text_right!}">
                    ${uiLabelMap.CommonSubTotal}
                </@td>
                <@td nowrap="nowrap" class="${styles.text_right!}" id="cartSubTotal">
                    <@ofbizCurrency amount=shoppingCart.getDisplaySubTotal() isoCode=shoppingCart.getCurrency()/>
                </@td>
                <@td>&nbsp;</@td>
            </@tr>

           <#-- other adjustments -->
            <#list shoppingCart.getAdjustments() as cartAdjustment>
                <#assign adjustmentType = cartAdjustment.getRelatedOne("OrderAdjustmentType", true) />
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                        <#--${uiLabelMap.OrderPromotion}: ${cartAdjustment.description!""}-->
                        ${adjustmentType.get("description", locale)!}: ${cartAdjustment.get("description", locale)!}
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}" id="cartDiscountValue"><@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(cartAdjustment, shoppingCart.getSubTotal()) isoCode=shoppingCart.getCurrency()/></@td>
                    <@td>&nbsp;</@td>
                </@tr>
            </#list>

            <#-- Shipping and handling -->
            <@tr>
              <@td scope="row" colspan="5" class="${styles.text_right!}">${uiLabelMap.OrderShippingAndHandling}</@td>
              <@td nowrap="nowrap" class="${styles.text_right!}" id="cartTotalShipping"><@ofbizCurrency amount=shoppingCart.getTotalShipping() isoCode=shoppingCart.getCurrency() /></@td>
              <@td>&nbsp;</@td>
            </@tr>

            <#-- tax adjustments -->
              <@tr>
                <@td colspan="5" class="${styles.text_right!}">${uiLabelMap.OrderTotalSalesTax}</@td>
                <@td nowrap="nowrap" class="${styles.text_right!}" id="cartTotalSalesTax"><@ofbizCurrency amount=shoppingCart.getTotalSalesTax() isoCode=shoppingCart.getCurrency()/></@td>
                <@td>&nbsp;</@td>
              </@tr>
            
            <#-- grand total -->
            <@tr>
                <@td colspan="5"></@td>
                <@td colspan="1"><hr /></@td>
                <@td>&nbsp;</@td>
            </@tr>
            <@tr>
                <@td colspan="5" class="${styles.text_right!}">
                    <strong>${uiLabelMap.CommonTotal}</strong>
                </@td>
                <@td nowrap="nowrap" class="${styles.text_right!}" id="cartDisplayGrandTotal">
                    <strong><@ofbizCurrency amount=shoppingCart.getDisplayGrandTotal() isoCode=shoppingCart.getCurrency()/></strong>
                </@td>
                <@td>&nbsp;</@td>
            </@tr>

            <#--
            Scipio: styling issues: 
            </@tfoot>-->
            </@tbody>
        </@table>
      </fieldset>
      <#--<fieldset id="productPromoCodeFields">-->
        <@field type="input" id="productPromoCode" name="productPromoCode" value="" label=uiLabelMap.EcommerceEnterPromoCode />
      <#--</fieldset>-->
      <#--<fieldset>-->
        <@field type="submitarea">
          <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="updateShoppingCart" text="${rawString(uiLabelMap.EcommerceContinueToStep)} 2"/>
          <@field type="submit" submitType="link" style="display: none;" class="${styles.link_run_session!}" href="javascript:void(0);" id="processingShipping" text="${rawString(uiLabelMap.EcommercePleaseWait)}..."/>
        </@field>
      <#--</fieldset>-->
    </form>
  </div>
  </@section>
</#if>