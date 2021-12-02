<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/cart/cartcommon.ftl">


<#assign fixedAssetExist = shoppingCart.containAnyWorkEffortCartItems() /> <#-- change display format when rental items exist in the shoppingcart -->

<#assign showDetailed = showDetailed!true>
<#assign showDetailedAdjustments = showDetailedAdjustments!true>
<#-- SCIPIO: sales tax details to not show because they are especially extremely confusing due to way they combining with promotions -->
<#assign showDetailedTax = showDetailedTax!false>

<#assign cartHasItems = (shoppingCartSize > 0)>
<#assign cartEmpty = (!cartHasItems)>

<@section>

  <#if (shoppingCartSize > 0)>
    <#assign itemsFromList = false />
    <#assign promoItems = false />

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
<#--                    <#assign isStoreInventoryNotRequiredAndNotAvailable = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequiredAndAvailable(request, itemProduct, cartLine.getQuantity(), false, false) />-->
<#--                    <#if isStoreInventoryNotRequiredAndNotAvailable && itemProduct.inventoryMessage?has_content>-->
<#--                        <@tr type="meta"><@td colspan="6"><@commonMsg type="warning">${itemProduct.inventoryMessage}</@commonMsg></@td></@tr>-->
<#--                    </#if>-->
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
                                <#list giftWrapOption as option>
                                    <#if ((selectedOption.productFeatureId)?? && selectedOption.productFeatureId == option.productFeatureId)>
                                        ${option.description} : ${option.amount!0}
                                    </#if>
                                </#list>
                              </select>
                            <#elseif showNoGiftWrapOptions>
                                ${uiLabelMap.EcommerceNoGiftWrap}
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
                                        <@modal id="${raw(cartLine.productId)}_q" label=cartLine.getQuantity()?string.number>
                                            <@fields type="default-compact">
                                                <@field type="display" label=uiLabelMap.EcommerceStartdate value=(cartLine.getReservStart()?string("yyyy-MM-dd")) />
                                                <@field type="display" label=uiLabelMap.CommonDays value=(cartLine.getReservLength()?string.number) />
                                                <@field type="display" label=uiLabelMap.CommonPersons value=(cartLine.getReservPersons()?string.number) />
                                                <@field type="display" label=uiLabelMap.CommonQuantity value=(cartLine.getQuantity()?string.number) />
                                            </@fields>
                                        </@modal>
                                    <#else>
                                        ${(cartLine.getQuantity()?string.number)}
                                    </#if>
                                <#else><#-- fixedAssetExist -->
                                    ${cartLine.getQuantity()}
                                </#if>
                            </#if>
                        </@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayPrice() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=shoppingCart.getCurrency()/></@td>
                        <@td class="${styles.text_right!}"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=shoppingCart.getCurrency()/></@td>
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
    <@row>
        <@cell columns=9 class="${styles.text_right!}">
            <@menu type="button">
                <@menuitem type="link" href=makePageUrl("loadCartFromAbandonedCart?visitId=" + abandonedCart.visitId) class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.AbandonedCartContinueToShow disabled=cartEmpty/>
            </@menu>
        </@cell>
    </@row>

  </#if>
</@section>