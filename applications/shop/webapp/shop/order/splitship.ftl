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
<#include "ordercommon.ftl">

<#-- Scipio: TODO: Convert and review -->

<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; payment info
        form.action="<@ofbizUrl>updateShippingOptions/checkoutpayment</@ofbizUrl>";
        form.submit();
    } else if (mode == "CS") {
        // continue shopping
        form.action="<@ofbizUrl>updateShippingOptions/showcart</@ofbizUrl>";
        form.submit();
    } else if (mode == "NA") {
        // new address
        form.action="<@ofbizUrl>updateShippingOptions/editcontactmech?DONE_PAGE=splitship&preContactMechTypeId=POSTAL_ADDRESS&contactMechPurposeTypeId=SHIPPING_LOCATION</@ofbizUrl>";
        form.submit();
    } else if (mode == "SV") {
        // save option; return to current screen
        form.action="<@ofbizUrl>updateShippingOptions/splitship</@ofbizUrl>";
        form.submit();
    } else if (mode == "SA") {
        // selected shipping address
        form.action="<@ofbizUrl>updateShippingAddress/splitship</@ofbizUrl>";
        form.submit();
    }
}
</@script>

<@section title=uiLabelMap.OrderItemGroups>
    <@table type="data-complex"> <#-- orig: width="100%" cellspacing="0" cellpadding="1" border="0" -->
      <#assign shipGroups = cart.getShipGroups()>
      <#if (shipGroups.size() > 0)>
        <#assign groupIdx = 0>
        <#list shipGroups as group>
          <#assign shipEstimateWrapper = Static["org.ofbiz.order.shoppingcart.shipping.ShippingEstimateWrapper"].getWrapper(dispatcher, cart, groupIdx)>
          <#assign carrierShipmentMethods = shipEstimateWrapper.getShippingMethods()>
          <#assign groupNumber = groupIdx + 1>
          <form method="post" action="#" name="editgroupform${groupIdx}">
            <input type="hidden" name="groupIndex" value="${groupIdx}"/>
            <@tr>
              <@td>
                <div><b>${uiLabelMap.CommonGroup} ${groupNumber}:</b></div>
                <#list group.getShipItems() as item>
                  <#assign groupItem = group.getShipItemInfo(item)>
                  <div>&nbsp;&nbsp;&nbsp;${item.getName()} - (${groupItem.getItemQuantity()})</div>
                </#list>
              </@td>
              <@td>
                <div>
                  <#--<span class="tabletext">${uiLabelMap.CommonAdd}:</span>-->
                  <a href="javascript:submitForm(document.editgroupform${groupIdx}, 'NA', '');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.PartyAddNewAddress}</a>
                </div>
                <div>
                  <#assign selectedContactMechId = cart.getShippingContactMechId(groupIdx)?default("")>
                  <select name="shippingContactMechId" onchange="javascript:submitForm(document.editgroupform${groupIdx}, 'SA', null);">
                    <option value="">${uiLabelMap.OrderSelectShippingAddress}</option>
                    <#list shippingContactMechList as shippingContactMech>
                      <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                      <option value="${shippingAddress.contactMechId}" <#if (shippingAddress.contactMechId == selectedContactMechId)>selected="selected"</#if>>${shippingAddress.address1}</option>
                    </#list>
                  </select>
                </div>
                <#if cart.getShipmentMethodTypeId(groupIdx)??>
                  <#assign selectedShippingMethod = cart.getShipmentMethodTypeId(groupIdx) + "@" + cart.getCarrierPartyId(groupIdx)>
                <#else>
                  <#assign selectedShippingMethod = "">
                </#if>
                <select name="shipmentMethodString">
                  <option value="">${uiLabelMap.OrderSelectShippingMethod}</option>
                  <#list carrierShipmentMethods as carrierShipmentMethod>
                    <#assign shippingEst = shipEstimateWrapper.getShippingEstimate(carrierShipmentMethod)?default(-1)>
                    <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
                    <option value="${shippingMethod}" <#if (shippingMethod == selectedShippingMethod)>selected="selected"</#if>>
                      <#if carrierShipmentMethod.partyId != "_NA_">
                        ${carrierShipmentMethod.partyId!}&nbsp;
                      </#if>
                      ${carrierShipmentMethod.description!}
                      <#if shippingEst?has_content>
                        &nbsp;-&nbsp;
                        <#if (shippingEst > -1)>
                          <@ofbizCurrency amount=shippingEst isoCode=cart.getCurrency()/>
                        <#else>
                          ${uiLabelMap.OrderCalculatedOffline}
                        </#if>
                      </#if>
                    </option>
                  </#list>
                </select>

                <@heading>${uiLabelMap.OrderSpecialInstructions}</@heading>
                <textarea cols="35" rows="3" wrap="hard" name="shippingInstructions">${cart.getShippingInstructions(groupIdx)!}</textarea>
              </@td>
              <@td>
                <div>
                  <select name="maySplit">
                    <#assign maySplitStr = cart.getMaySplit(groupIdx)?default("")>
                    <option value="">${uiLabelMap.OrderSplittingPreference}</option>
                    <option value="false" <#if maySplitStr == "N">selected="selected"</#if>>${uiLabelMap.OrderShipAllItemsTogether}</option>
                    <option value="true" <#if maySplitStr == "Y">selected="selected"</#if>>${uiLabelMap.OrderShipItemsWhenAvailable}</option>
                  </select>
                </div>
                <div>
                  <select name="isGift">
                    <#assign isGiftStr = cart.getIsGift(groupIdx)?default("")>
                    <option value="">${uiLabelMap.OrderIsGift} ?</option>
                    <option value="false" <#if isGiftStr == "N">selected="selected"</#if>>${uiLabelMap.OrderNotAGift}</option>
                    <option value="true" <#if isGiftStr == "Y">selected="selected"</#if>>${uiLabelMap.OrderYesIsAGift}</option>
                  </select>
                </div>

                <@heading>${uiLabelMap.OrderGiftMessage}</@heading>
                <textarea cols="30" rows="3" wrap="hard" name="giftMessage">${cart.getGiftMessage(groupIdx)!}</textarea>
              </@td>
              <@td><input type="button" class="${styles.link_run_session!} ${styles.action_update!}" value="${uiLabelMap.CommonSave}" onclick="javascript:submitForm(document.editgroupform${groupIdx}, 'SV', null);"/></@td>
            </@tr>
            <#assign groupIdx = groupIdx + 1>
            <#if group_has_next>
              <@tr type="util">
                <@td colspan="6"><hr /></@td>
              </@tr>
            </#if>
          </form>
        </#list>
      <#else>
        <div>${uiLabelMap.OrderNoShipGroupsDefined}.</div>
      </#if>
    </@table>
</@section>

<@section title=uiLabelMap.EcommerceAssignItems>
  <@table type="data-complex"> <#-- orig: width="100%" cellspacing="0" cellpadding="1" border="0" -->
    <@thead>
      <@tr>
        <@td>${uiLabelMap.OrderProduct}</@td>
        <@td align="center">${uiLabelMap.OrderTotalQty}</@td>
        <@td></@td>
        <@td align="center">${uiLabelMap.OrderMoveQty}</@td>
        <@td></@td>
        <@td></@td>
        <@td></@td>
        <@td></@td>
      </@tr>
    </@thead>
    <@tbody>
      <#list cart.items() as cartLine>
        <#assign cartLineIndex = cart.getItemIndex(cartLine)>
        <@tr>
          <form method="post" action="<@ofbizUrl>updatesplit</@ofbizUrl>" name="editgroupform">
            <input type="hidden" name="itemIndex" value="${cartLineIndex}"/>
            <@td>
              <div>
                <#if cartLine.getProductId()??>
                  <#-- product item -->
                  <#-- start code to display a small image of the product -->
                  <#-- Scipio: Uncomment to display image
                  <#assign smallImageUrl = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(cartLine.getProduct(), "SMALL_IMAGE_URL", locale, dispatcher, "url")!>
                  <#if !smallImageUrl?string?has_content><#assign smallImageUrl = "/images/defaultImage.jpg"></#if>
                  <#if smallImageUrl?string?has_content>
                    <a href="<@ofbizUrl>product?product_id=${cartLine.getProductId()}</@ofbizUrl>">
                      <img src="<@ofbizContentUrl ctxPrefix=true>${smallImageUrl}</@ofbizContentUrl>" class="cssImgSmall" alt="" />
                    </a>
                  </#if>
                  -->
                  <#-- end code to display a small image of the product -->
                  <a href="<@ofbizUrl>product?product_id=${cartLine.getProductId()}</@ofbizUrl>" class="${styles.link_nav_info_desc!}">${cartLine.getProductId()} -
                  ${cartLine.getName()!}</a> : ${cartLine.getDescription()!}

                  <#-- display the registered ship groups and quantity -->
                  <#assign itemShipGroups = cart.getShipGroups(cartLine)>
                  <#list itemShipGroups.entrySet() as group>
                    <div>
                      <#assign groupNumber = group.getKey() + 1>
                      <b>Group - </b>${groupNumber} / <b>${uiLabelMap.CommonQuantity} - </b>${group.getValue()}
                    </div>
                  </#list>

                  <#-- if inventory is not required check to see if it is out of stock and needs to have a message shown about that... -->
                  <#assign itemProduct = cartLine.getProduct()>
                  <#assign isStoreInventoryNotRequiredAndNotAvailable = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequiredAndAvailable(request, itemProduct, cartLine.getQuantity(), false, false)>
                  <#if isStoreInventoryNotRequiredAndNotAvailable && itemProduct.inventoryMessage?has_content>
                    <b>(${itemProduct.inventoryMessage})</b>
                  </#if>

                <#else>
                  <#-- this is a non-product item -->
                  <b>${cartLine.getItemTypeDescription()!}</b> : ${cartLine.getName()!}
                </#if>
              </div>

            </@td>
            <@td align="right">${cartLine.getQuantity()?string.number}&nbsp;&nbsp;&nbsp;
            </@td>
            <@td>&nbsp;
            </@td>
            <@td align="center">
              <input size="6" type="text" name="quantity" value="${cartLine.getQuantity()?string.number}"/>
            </@td>
            <@td>&nbsp;
            </@td>
            <@td>${uiLabelMap.CommonFrom}:
                <select name="fromGroupIndex">
                  <#list itemShipGroups.entrySet() as group>
                    <#assign groupNumber = group.getKey() + 1>
                    <option value="${group.getKey()}">${uiLabelMap.CommonGroup} ${groupNumber}</option>
                  </#list>
                </select>
            </@td>
            <@td>${uiLabelMap.CommonTo}:
                <select name="toGroupIndex">
                  <#list 0..(cart.getShipGroupSize() - 1) as groupIdx>
                    <#assign groupNumber = groupIdx + 1>
                    <option value="${groupIdx}">${uiLabelMap.CommonGroup} ${groupNumber}</option>
                  </#list>
                  <option value="-1">${uiLabelMap.CommonNew} ${uiLabelMap.CommonGroup}</option>
                </select>
            </@td>
            <@td><input type="submit" class="${styles.link_run_session!} ${styles.action_update!}" value="${uiLabelMap.CommonSubmit}"/></@td>
          </form>
        </@tr>
      </#list>
    </@tbody>
  </@table>
</@section>

<@checkoutActionsMenu directLinks=true >
  <@menuitem type="link" href=makeOfbizUrl("checkoutpayment") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue />
</@checkoutActionsMenu>
