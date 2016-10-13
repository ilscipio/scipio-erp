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
<#-- SCIPIO: ToDo: Rewrite the following javascript -->
<@script>
function addToList() {
    var cform = document.cartform;
    cform.action = "<@ofbizUrl>addBulkToShoppingList</@ofbizUrl>";
    cform.submit();
}
function gwAll(e) {
    var cform = document.cartform;
    var len = cform.elements.length;
    var selectedValue = e.value;
    if (selectedValue == "") {
        return;
    }

    var cartSize = ${shoppingCartSize};
    var passed = 0;
    for (var i = 0; i < len; i++) {
        var element = cform.elements[i];
        var ename = element.name;
        var sname = ename.substring(0,16);
        if (sname == "option^GIFT_WRAP") {
            var options = element.options;
            var olen = options.length;
            var matching = -1;
            for (var x = 0; x < olen; x++) {
                var thisValue = element.options[x].value;
                if (thisValue == selectedValue) {
                    element.selectedIndex = x;
                    passed++;
                }
            }
        }
    }
    if (cartSize > passed && selectedValue != "NO^") {
        showErrorAlert("${uiLabelMap.CommonErrorMessage2}","${uiLabelMap.EcommerceSelectedGiftWrap}");
    }
    cform.submit();
}

function setAlternateGwp(field) {
  window.location=field.value;
};
</@script>

<#assign fixedAssetExist = shoppingCart.containAnyWorkEffortCartItems() /> <#-- change display format when rental items exist in the shoppingcart -->


<#assign cartHasItems = (shoppingCartSize > 0)>
<#assign cartEmpty = (!cartHasItems)>
<#if ((sessionAttributes.lastViewedProducts)?has_content && (sessionAttributes.lastViewedProducts?size > 0))>
  <#assign continueLink = "product?product_id=" + sessionAttributes.lastViewedProducts.get(0)>
<#else>
  <#assign continueLink = "main">
</#if>


<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if shoppingCart.items()?has_content>
            <@menuitem type="link" href="javascript:document.cartform.submit();" class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.EcommerceRecalculateCart disabled=cartEmpty />
            <@menuitem type="link" href=makeOfbizUrl("emptycart") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.EcommerceEmptyCart disabled=cartEmpty />
            <@menuitem type="link" href="javascript:removeSelected('cartform');" class="+${styles.action_run_session!} ${styles.action_remove!}" text=uiLabelMap.EcommerceRemoveSelected disabled=cartEmpty />
        </#if>
    </@menu>
</#macro>

<@section menuContent=menuContent>

  <#if (shoppingCartSize > 0)>
    <#assign itemsFromList = false />
    <#assign promoItems = false />
    <form method="post" action="<@ofbizUrl>modifycart</@ofbizUrl>" name="cartform">
    <@fields fieldArgs={"checkboxType":"simple-standard"}><#-- TODO: type="..." -->
      <input type="hidden" name="removeSelected" value="false" />
        <@table type="data-complex" role="grid"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th width="25%">${uiLabelMap.ProductProduct}</@th>
                    <@th width="15%" class="${styles.text_right!}"></@th>
                    <@th width="10%">${uiLabelMap.CommonQuantity}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.EcommerceUnitPrice}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.EcommerceAdjustments}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.EcommerceItemTotal}</@th>
                    <@th width="5%"><@field type="checkbox" widgetOnly=true name="selectAll" value=(uiLabelMap.CommonY) onClick="javascript:toggleAll(this, 'cartform', 'selectedItem');" /></@th>
                </@tr>
            </@thead>
            <@tbody>
                <#assign itemClass = "2">
                <#list shoppingCart.items() as cartLine>
                    <#assign cartLineIndex = shoppingCart.getItemIndex(cartLine) />
                    <#assign lineOptionalFeatures = cartLine.getOptionalProductFeatures() />
               
                    <#if itemClass == "1"><#assign rowColor=styles.row_alt!><#else><#assign rowColor=styles.row_reg!></#if> 
                    <#assign itemProduct = cartLine.getProduct() />
                    <#-- if inventory is not required check to see if it is out of stock and needs to have a message shown about that... -->
                    <#assign isStoreInventoryNotRequiredAndNotAvailable = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequiredAndAvailable(request, itemProduct, cartLine.getQuantity(), false, false) />
                    <#if isStoreInventoryNotRequiredAndNotAvailable && itemProduct.inventoryMessage?has_content>
                        <@tr type="meta"><@td colspan="6"><@commonMsg type="warning">${itemProduct.inventoryMessage}</@commonMsg></@td></@tr>
                    </#if>
                    <@tr class="${rowColor!}">
                        <@td> 
                        <#if cartLine.getProductId()??>
                            <#-- product item -->
                            <#if cartLine.getParentProductId()??>
                                <#assign parentProductId = cartLine.getParentProductId() />
                            <#else>
                                <#assign parentProductId = cartLine.getProductId() />
                            </#if>
                            <a href="<@ofbizCatalogAltUrl productId=parentProductId/>" class="${styles.link_nav_info_idname!}" target="_blank">${cartLine.getProductId()} - ${cartLine.getName()!}</a>
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
                            <#-- 
                            <#if (cartLine.getIsPromo() && cartLine.getAlternativeOptionProductIds()?has_content)>
                              Show alternate gifts if there are any...
                              <div class="tableheadtext">${uiLabelMap.OrderChooseFollowingForGift}:</div>
                              <select name="dummyAlternateGwpSelect${cartLineIndex}" onchange="setAlternateGwp(this);">
                              <option value="">- ${uiLabelMap.OrderChooseAnotherGift} -</option>
                              <#list cartLine.getAlternativeOptionProductIds() as alternativeOptionProductId>
                                <#assign alternativeOptionName = Static["org.ofbiz.product.product.ProductWorker"].getGwpAlternativeOptionName(dispatcher, delegator, alternativeOptionProductId, requestAttributes.locale) />
                                <option value="<@ofbizUrl>setDesiredAlternateGwpProductId?alternateGwpProductId=${alternativeOptionProductId}&alternateGwpLine=${cartLineIndex}</@ofbizUrl>">${alternativeOptionName!alternativeOptionProductId}</option>
                              </#list>
                              </select>
                            </#if>
                            -->
                        </#if>
                        </@td>
                        <#-- giftWrap & promotion info -->
                        <@td>
                             <#if cartLine.getShoppingListId()??>
                              <#assign itemsFromList = true />
                              <a href="<@ofbizUrl>editShoppingList?shoppingListId=${cartLine.getShoppingListId()}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">L</a>
                            </#if>
                            <#if cartLine.getIsPromo()>
                              <#assign promoItems = true />
                              Free! <#--${uiLabelMap.EcommercePromotionalItems}-->
                            </#if>
                            <#assign showNoGiftWrapOptions = false />
                            <#assign giftWrapOption = lineOptionalFeatures.GIFT_WRAP! />
                            <#assign selectedOption = cartLine.getAdditionalProductFeatureAndAppl("GIFT_WRAP")! />
                            <#if giftWrapOption?has_content>
                              <select name="option^GIFT_WRAP_${cartLineIndex}" onchange="javascript:this.form.submit();">
                                <option value="NO^">${uiLabelMap.EcommerceNoGiftWrap}</option>
                                <#list giftWrapOption as option>
                                  <option value="${option.productFeatureId}" <#if ((selectedOption.productFeatureId)?? && selectedOption.productFeatureId == option.productFeatureId)>selected="selected"</#if>>${option.description} : ${option.amount!0}</option>
                                </#list>
                              </select>
                            <#elseif showNoGiftWrapOptions>
                              <select name="option^GIFT_WRAP_${cartLineIndex}" onchange="javascript:this.form.submit();">
                                <option value="">${uiLabelMap.EcommerceNoGiftWrap}</option>
                              </select>
                            </#if>
                        </@td>
                        <#-- end gift wrap option -->

                        <#-- QUANTITY -->
                        <@td>
                            <#if cartLine.getIsPromo() || cartLine.getShoppingListId()??>
                                <#if fixedAssetExist == true && cartLine.getReservStart()??>
                                  <#-- Scipio: NOTE: stock bugfixes applied here -->
                                  <@modal id="${rawString(cartLine.productId)}_q" label=cartLine.getQuantity()?string.number>   
                                    <@fields type="default-compact"> 
                                      <@field type="display" label=uiLabelMap.EcommerceStartdate value=(cartLine.getReservStart()?string("yyyy-MM-dd")) />
                                      <@field type="display" label=uiLabelMap.CommonDays value=(cartLine.getReservLength()?string.number) />
                                      <@field type="display" label=uiLabelMap.CommonPersons value=(cartLine.getReservPersons()?string.number) />
                                      <@field type="display" label=uiLabelMap.CommonQuantity value=(cartLine.getQuantity()?string.number) />
                                    </@fields>
                                  </@modal>                                                 
                                <#else><#-- fixedAssetExist -->
                                    ${cartLine.getQuantity()?string.number}
                                </#if>
                            <#else><#-- Is Promo or Shoppinglist -->
                                <#if fixedAssetExist == true>
                                    <#-- Scipio:FIXME?: can't put in modal easily because inputs end up outside form -->
                                    <#if cartLine.getReservStart()??>
                                      <@fields type="default-compact"> 
                                        <@field type="datetime" dateType="date" name="reservStart_${cartLineIndex}" maxlength=10 label=uiLabelMap.EcommerceStartdate value=(cartLine.getReservStart()?string("yyyy-MM-dd")) 
                                          postfix=false datePostfix=false/><#-- FIXME: not enough space for postfix right now -->
                                        <@field type="input" name="reservLength_${cartLineIndex}"  label=uiLabelMap.CommonDays value=(cartLine.getReservLength()?string.number) />
                                        <@field type="input" name="reservPersons_${cartLineIndex}" label=uiLabelMap.CommonPersons value=(cartLine.getReservPersons()?string.number) />
                                        <@field type="input" name="update_${cartLineIndex}" label=uiLabelMap.CommonQuantity value=(cartLine.getQuantity()?string.number) onChange="javascript:this.form.submit();"/> 
                                      </@fields>
                                    <#else>
                                      <@field type="input" widgetOnly=true name="update_${cartLineIndex}" value=(cartLine.getQuantity()?string.number) onChange="javascript:this.form.submit();"/> 
                                    </#if>
                                <#else><#-- fixedAssetExist -->
                                    <@field type="select" widgetOnly=true name="update_${cartLineIndex}" onChange="javascript:this.form.submit();">
                                        <#list 1..99 as x>
                                            <#if cartLine.getQuantity() == x>
                                                <#assign selected = true/>
                                            <#else>
                                                <#assign selected = false/>
                                            </#if>
                                            <@field type="option" value=(x) selected=selected>${x}</@field>
                                        </#list>
                                    </@field>
                                </#if>
                            </#if>
                        </@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayPrice() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td><#if !cartLine.getIsPromo()><@field type="checkbox" widgetOnly=true name="selectedItem" value=(cartLineIndex) onClick="javascript:checkToggle(this,'cartform','selectedItem');" /><#else>&nbsp;</#if></@td>
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
                        <@td nowrap="nowrap" class="${styles.text_right!}">
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
                        <@td nowrap="nowrap" class="${styles.text_right!}"><@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(cartAdjustment, shoppingCart.getSubTotal()) isoCode=shoppingCart.getCurrency()/></@td>
                        <@td>&nbsp;</@td>
                    </@tr>
                </#list>

                <#-- tax adjustments -->
                <#if (shoppingCart.getDisplayTaxIncluded() > 0.0)>
                  <@tr>
                    <@td colspan="5" class="${styles.text_right!}">${uiLabelMap.OrderTotalSalesTax}</@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}"><@ofbizCurrency amount=shoppingCart.getDisplayTaxIncluded() isoCode=shoppingCart.getCurrency()/></@td>
                    <@td>&nbsp;</@td>
                  </@tr>
                </#if>

                
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
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <strong><@ofbizCurrency amount=shoppingCart.getDisplayGrandTotal() isoCode=shoppingCart.getCurrency()/></strong>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>

            <#--Scipio: styling issues: 
            </@tfoot>-->
            </@tbody>
        </@table>
    </@fields>
    </form>
    <@row>
        <@cell columns=3>
            <@menu type="button">
                <@menuitem type="link" href=makeOfbizUrl(continueLink) text=uiLabelMap.EcommerceContinueShopping class="+${styles.action_nav!} ${styles.action_cancel!}"/>
            </@menu>
        </@cell>
        <@cell columns=9 class="${styles.text_right!}">
            <@menu type="button">
                <@menuitem type="link" href=makeOfbizUrl("checkoutoptionslogin") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckout disabled=cartEmpty/>
                <#--<@menuitem type="link" href=makeOfbizUrl("quickcheckout") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckoutQuick disabled=cartEmpty/>-->
                <@menuitem type="link" href=makeOfbizUrl("onePageCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.EcommerceOnePageCheckout disabled=cartEmpty/>
            </@menu>
        </@cell>
    </@row>    
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.EcommerceYourShoppingCartEmpty}.</@commonMsg>
    <@row>
        <@cell>
            <@menu type="button">
                <@menuitem type="link" href=makeOfbizUrl(continueLink) text=uiLabelMap.EcommerceContinueShopping class="+${styles.action_nav!} ${styles.action_cancel!}"/>
            </@menu>
        </@cell>
    </@row>
  </#if>
</@section>


<@section> <#-- Scipio: look strange: title=uiLabelMap.ProductPromotions -->
    <@row>
        <@cell columns=6>
            <@section title=uiLabelMap.ProductPromoCodes>
                <form method="post" action="<@ofbizUrl>addpromocode<#if requestAttributes._CURRENT_VIEW_?has_content>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>" name="addpromocodeform">
                    <input type="text" size="15" name="productPromoCodeId" value="" />
                    <input type="submit" class="${styles.link_run_session!} ${styles.action_add!}" value="${uiLabelMap.OrderAddCode}" />
                    <#assign productPromoCodeIds = (shoppingCart.getProductPromoCodesEntered())! />
                    <#if productPromoCodeIds?has_content>
                        ${uiLabelMap.ProductPromoCodesEntered}
                        <ul>
                          <#list productPromoCodeIds as productPromoCodeId>
                            <li>${productPromoCodeId}</li>
                          </#list>
                        </ul>
                    </#if>
                </form>
            </@section>
        </@cell>
        <@cell columns=6>
            <#if showPromoText?? && showPromoText>
                <@panel type="callout">
                  <@section title=uiLabelMap.OrderSpecialOffers>
                    <ol>
                      <#list productPromos as productPromo>
                        <li>${productPromo.promoName!}
                           <#--${productPromo.promoText!}<br/>--><#-- Enable for further promotion information -->
                           <a href="<@ofbizUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@ofbizUrl>" class="${styles.action_view!}">${uiLabelMap.CommonDetails}</a>
                        </li>
                      </#list>
                    </ol>
                    <a href="<@ofbizUrl>showAllPromotions</@ofbizUrl>" class="${styles.link_nav!}">${uiLabelMap.OrderViewAllPromotions}</a>
                  </@section>
                </@panel>
            </#if>
        </@cell>
    </@row>
</@section>

<#if associatedProducts?has_content>
  <@section title="${rawLabel('EcommerceYouMightAlsoIntrested')}:">
    <@grid columns=5>
        <#list associatedProducts as assocProduct>
            <li>
                <#-- Product summary 
                    <@render resource="component://shop/widget/CatalogScreens.xml#productsummary" reqAttribs={"optProduct":assocProduct, "listIndex":assocProduct_index}/>
                -->
                <#-- mini product summary -->
                <@render resource="component://shop/widget/CatalogScreens.xml#miniproductsummary" reqAttribs={"optProductId": assocProduct.productId, "productId": assocProduct.productId, "listIndex": assocProduct_index} />
            </li>
        </#list>
    </@grid>
  </@section>
</#if>

<#-- SCIPIO: Uncomment for a quick-add form; allows users to add products to the cart on the fly -->
<#--
<@section title=uiLabelMap.CommonQuickAdd>
    <form method="post" action="<@ofbizUrl>additem<#if requestAttributes._CURRENT_VIEW_?has_content>/${requestAttributes._CURRENT_VIEW_}</#if></@ofbizUrl>" name="quickaddform">
        <fieldset>
        ${uiLabelMap.EcommerceProductNumber}<input type="text" name="add_product_id" value="${requestParameters.add_product_id!}" />
         // check if rental data present  insert extra fields in Quick Add
        <#if (product?? && product.getString("productTypeId") == "ASSET_USAGE") || (product?? && product.getString("productTypeId") == "ASSET_USAGE_OUT_IN")>
            ${uiLabelMap.EcommerceStartDate}: <input type="text" size="10" name="reservStart" value="${requestParameters.reservStart!""}" />
            ${uiLabelMap.EcommerceLength}: <input type="text" size="2" name="reservLength" value="${requestParameters.reservLength!""}" />
            </div>
            <div>
            &nbsp;&nbsp;${uiLabelMap.OrderNbrPersons}: <input type="text" size="3" name="reservPersons" value="${requestParameters.reservPersons!"1"}" />
        </#if>
        ${uiLabelMap.CommonQuantity}: <input type="text" size="5" name="quantity" value="${requestParameters.quantity!"1"}" />
        <input type="submit" class="${styles.link_run_session!} ${styles.action_add!}" value="${uiLabelMap.OrderAddToCart}" />
        </fieldset>
    </form>
</@section>-->