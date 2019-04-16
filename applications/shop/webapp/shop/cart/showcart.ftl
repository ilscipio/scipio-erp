<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/cart/cartcommon.ftl">

<#-- SCIPIO: TODO: Rewrite the following javascript -->
<@script>
function addToList() {
    var cform = document.cartform;
    cform.action = "<@pageUrl>addBulkToShoppingList</@pageUrl>";
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
        showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}","${escapeVal(uiLabelMap.EcommerceSelectedGiftWrap, 'js')}");
    }
    cform.submit();
}

function setAlternateGwp(field) {
  window.location=field.value;
};
</@script>

<#assign fixedAssetExist = shoppingCart.containAnyWorkEffortCartItems() /> <#-- change display format when rental items exist in the shoppingcart -->

<#assign showDetailed = showDetailed!true>
<#assign showDetailedAdjustments = showDetailedAdjustments!true>
<#-- SCIPIO: sales tax details to not show because they are especially extremely confusing due to way they combining with promotions -->
<#assign showDetailedTax = showDetailedTax!false>

<#assign cartHasItems = (shoppingCartSize > 0)>
<#assign cartEmpty = (!cartHasItems)>
<#assign lastViewedProducts = lastViewedProducts!sessionAttributes.lastViewedProducts!><#-- SCIPIO: Access session only once -->
<#if ((lastViewedProducts)?has_content && (lastViewedProducts?size > 0))>
  <#assign continueLink><@catalogAltUrl productCategoryId=requestParameters.category_id!"" productId=(lastViewedProducts.get(0)) rawParams=true/></#assign>
<#else>
  <#assign continueLink = makePageUrl("main")>
</#if>


<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if shoppingCart.items()?has_content>
            <@menuitem type="link" href="javascript:document.cartform.submit();" class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.EcommerceRecalculateCart disabled=cartEmpty />
            <@menuitem type="link" href=makePageUrl("emptycart") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.EcommerceEmptyCart disabled=cartEmpty />
            <@menuitem type="link" href="javascript:removeSelected('cartform');" class="+${styles.action_run_session!} ${styles.action_remove!}" text=uiLabelMap.EcommerceRemoveSelected disabled=cartEmpty />
        </#if>
    </@menu>
</#macro>

<@section menuContent=menuContent>

  <#if (shoppingCartSize > 0)>
    <#assign itemsFromList = false />
    <#assign promoItems = false />
    <form method="post" action="<@pageUrl>modifycart</@pageUrl>" name="cartform">
    <@fields fieldArgs={"checkboxType":"simple-standard"}><#-- TODO: type="..." -->
      <input type="hidden" name="removeSelected" value="false" />
        <@table type="data-complex" role="grid">
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
            
            <#-- SCIPIO: OrderItemAttributes and ProductConfigWrappers -->
            <#macro orderItemAttrInfo cartLine>
                <#-- SCIPIO: OrderItemAttributes and ProductConfigWrappers -->
                <#if cartLine.getConfigWrapper()??>
                  <#local selectedOptions = cartLine.getConfigWrapper().getSelectedOptions()! />
                  <#if selectedOptions?has_content>
                    <ul class="order-item-attrib-list">
                      <#list selectedOptions as option>
                        <li>${option.getDescription()}</li>
                      </#list>
                    </ul>
                  </#if>
                </#if>
                <#local attrs = cartLine.getOrderItemAttributes()/>
                <#if attrs?has_content>
                    <#assign attrEntries = attrs.entrySet()/>
                    <ul class="order-item-attrib-list">
                      <#list attrEntries as attrEntry>
                        <li>${attrEntry.getKey()}: ${attrEntry.getValue()}</li>
                      </#list>
                    </ul>
                </#if>
            </#macro>
            
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
                            <a href="<@catalogAltUrl productId=parentProductId/>" class="${styles.link_nav_info_idname!}" target="_blank">${cartLine.getProductId()} - ${cartLine.getName()!}</a>
                            <@orderItemAttrInfo cartLine=cartLine/>
                        <#else>
                            <#-- non-product item -->
                            ${cartLine.getItemTypeDescription()!}: ${cartLine.getName()!}
                            <@orderItemAttrInfo cartLine=cartLine/>                        
                            <#-- 
                            <#if (cartLine.getIsPromo() && cartLine.getAlternativeOptionProductIds()?has_content)>
                              Show alternate gifts if there are any...
                              <div class="tableheadtext">${uiLabelMap.OrderChooseFollowingForGift}:</div>
                              <select name="dummyAlternateGwpSelect${cartLineIndex}" onchange="setAlternateGwp(this);">
                              <option value="">- ${uiLabelMap.OrderChooseAnotherGift} -</option>
                              <#list cartLine.getAlternativeOptionProductIds() as alternativeOptionProductId>
                                <#assign alternativeOptionName = Static["org.ofbiz.product.product.ProductWorker"].getGwpAlternativeOptionName(dispatcher, delegator, alternativeOptionProductId, requestAttributes.locale) />
                                <option value="<@pageUrl>setDesiredAlternateGwpProductId?alternateGwpProductId=${alternativeOptionProductId}&alternateGwpLine=${cartLineIndex}</@pageUrl>">${alternativeOptionName!alternativeOptionProductId}</option>
                              </#list>
                              </select>
                            </#if>
                            -->
                        </#if>
                        <#-- SCIPIO: show application survey response QA list for this item -->
                        <#assign surveyResponses = cartLine.getSurveyResponses()!>
                        <#if surveyResponses?has_content>
                          <@orderItemSurvResList survResList=surveyResponses/>
                        </#if>
                   
                        </@td>
                        <#-- giftWrap & promotion info -->
                        <@td>
                             <#if cartLine.getShoppingListId()??>
                              <#assign itemsFromList = true />
                              <a href="<@pageUrl>editShoppingList?shoppingListId=${cartLine.getShoppingListId()}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">L</a>
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
                                  <option value="${option.productFeatureId}"<#if ((selectedOption.productFeatureId)?? && selectedOption.productFeatureId == option.productFeatureId)> selected="selected"</#if>>${option.description} : ${option.amount!0}</option>
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
                                  <#-- SCIPIO: NOTE: stock bugfixes applied here -->
                                  <@modal id="${raw(cartLine.productId)}_q" label=cartLine.getQuantity()?string.number>   
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
                                    <#-- SCIPIO:FIXME?: can't put in modal easily because inputs end up outside form -->
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
                                        <#if (cartLine.getQuantity() < 1)>
                                            <@field type="option" value=cartLine.getQuantity() selected=true>${cartLine.getQuantity()}</@field>
                                        </#if>
                                        <#list 1..99 as x>
                                            <#if cartLine.getQuantity() == x>
                                                <#assign selected = true/>
                                            <#else>
                                                <#assign selected = false/>
                                            </#if>
                                            <@field type="option" value=(x) selected=selected>${x}</@field>
                                        </#list>
                                        <#if (cartLine.getQuantity() > 99)>
                                            <@field type="option" value=cartLine.getQuantity() selected=true>${cartLine.getQuantity()}</@field>
                                        </#if>
                                    </@field>
                                </#if>
                            </#if>
                        </@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayPrice() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td><#if !cartLine.getIsPromo()><@field type="checkbox" widgetOnly=true name="selectedItem" value=(cartLineIndex) onClick="javascript:checkToggle(this,'cartform','selectedItem');" /><#else>&nbsp;</#if></@td>
                    </@tr>

                    <#-- now show adjustment details per line item -->
                    <#assign itemAdjustments = cartLine.getAdjustments()>
                    <#if showDetailed && showDetailedAdjustments>
                      <#list itemAdjustments as orderItemAdjustment>
                        <#-- SCIPIO: tax adjustments are especially confusing, so have their own option to hide -->
                        <#if showDetailedTax || !["SALES_TAX"]?seq_contains(orderItemAdjustment.orderAdjustmentTypeId)>
                        <@tr>
                          <@td colspan="4">
                            <#assign adjustmentType = orderItemAdjustment.getRelatedOne("OrderAdjustmentType", true)! />
                            ${uiLabelMap.EcommerceAdjustment}: ${adjustmentType.get("description",locale)!}
                            <#if orderItemAdjustment.description?has_content>: ${escapeVal(orderItemAdjustment.get("description",locale), 'htmlmarkup', {"allow":"internal"})}</#if>
                            <#if orderItemAdjustment.orderAdjustmentTypeId == "SALES_TAX">
                              <#if orderItemAdjustment.primaryGeoId?has_content>
                                <#assign primaryGeo = orderItemAdjustment.getRelatedOne("PrimaryGeo", true)/>
                                <#if primaryGeo.geoName?has_content>
                                  ${uiLabelMap.OrderJurisdiction}: ${primaryGeo.geoName!primaryGeo.abbreviation!}<#-- [${primaryGeo.abbreviation!}]-->
                                </#if>
                                <#if orderItemAdjustment.secondaryGeoId?has_content>
                                  <#assign secondaryGeo = orderItemAdjustment.getRelatedOne("SecondaryGeo", true)/>
                                  (${uiLabelMap.CommonIn}: ${secondaryGeo.geoName!secondaryGeo.abbreviation!}<#--  [${secondaryGeo.abbreviation!}])-->
                                </#if>
                              </#if>
                              <#if orderItemAdjustment.sourcePercentage??>${uiLabelMap.EcommerceRate}: ${orderItemAdjustment.sourcePercentage}</#if>
                              <#if orderItemAdjustment.customerReferenceId?has_content>${uiLabelMap.OrderCustomerTaxId}: ${orderItemAdjustment.customerReferenceId}</#if>
                              <#if orderItemAdjustment.exemptAmount??>${uiLabelMap.EcommerceExemptAmount}: ${orderItemAdjustment.exemptAmount}</#if>
                            </#if>
                            <#if orderItemAdjustment.orderAdjustmentTypeId == "VAT_TAX"> <#-- European VAT support (VAT included) -->
                                <#if orderItemAdjustment.amountAlreadyIncluded?has_content && !orderItemAdjustment.exemptAmount?has_content><#-- TODO: Check for missing label. -->
                                  : <@ofbizCurrency amount=orderItemAdjustment.amountAlreadyIncluded.setScale(taxFinalScale, taxRounding) isoCode=currencyUomId/>
                                </#if>
                            </#if>
                          </@td>
                          <@td class="text-right">
                              <@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency()/>
                          </@td>
                          <@td></@td>
                        </@tr>
                        </#if>
                      </#list>
                   </#if>
                </#list>
            <#-- SCIPIO: styling issues: 
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

            <#-- SCIPIO: styling issues: 
            </@tfoot>-->
            </@tbody>
        </@table>
    </@fields>
    </form>
    <@row>
        <@cell columns=3>
            <@menu type="button">
                <@menuitem type="link" href=continueLink text=uiLabelMap.EcommerceContinueShopping class="+${styles.action_nav!} ${styles.action_cancel!}"/>
            </@menu>
        </@cell>
        <@cell columns=9 class="${styles.text_right!}">
            <@menu type="button">
                <@menuitem type="link" href=makePageUrl("checkoutoptionslogin") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckout disabled=cartEmpty/>
                <#--<@menuitem type="link" href=makePageUrl("quickcheckout") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckoutQuick disabled=cartEmpty/>-->
                <@menuitem type="link" href=makePageUrl("onePageCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.EcommerceOnePageCheckout disabled=cartEmpty/>
            </@menu>
        </@cell>
    </@row>    
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.EcommerceYourShoppingCartEmpty}.</@commonMsg>
    <@row>
        <@cell>
            <@menu type="button">
                <@menuitem type="link" href=continueLink text=uiLabelMap.EcommerceContinueShopping class="+${styles.action_nav!} ${styles.action_cancel!}"/>
            </@menu>
        </@cell>
    </@row>
  </#if>
</@section>


<@section> <#-- SCIPIO: look strange: title=uiLabelMap.ProductPromotions -->
    <@row>
        <@cell columns=6>
            <@section title=uiLabelMap.ProductPromoCodes>
                <form method="post" action="<@pageUrl>addpromocode<#if requestAttributes._CURRENT_VIEW_?has_content>/${requestAttributes._CURRENT_VIEW_}</#if></@pageUrl>" name="addpromocodeform">
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
                <@panel type="callout" title=uiLabelMap.OrderSpecialOffers>
                  <@section>
                    <ol>
                      <#list productPromos as productPromo>
                        <li>${productPromo.promoName!}
                           <#--${productPromo.promoText!}<br/>--><#-- Enable for further promotion information -->
                           <a href="<@pageUrl>showPromotionDetails?productPromoId=${productPromo.productPromoId}</@pageUrl>" class="${styles.action_view!}">${uiLabelMap.CommonDetails}</a>
                        </li>
                      </#list>
                    </ol>
                    <a href="<@pageUrl>showAllPromotions</@pageUrl>" class="${styles.link_nav!}">${uiLabelMap.OrderViewAllPromotions}</a>
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
    <form method="post" action="<@pageUrl>additem<#if requestAttributes._CURRENT_VIEW_?has_content>/${requestAttributes._CURRENT_VIEW_}</#if></@pageUrl>" name="quickaddform">
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