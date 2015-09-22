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

<#-- Continuation of showcart.ftl:  List of order items and forms to modify them. -->
<#macro showAssoc productAssoc>
  <#assign productAssocType = (delegator.findOne("ProductAssocType", {"productAssocTypeId" : productAssoc.productAssocTypeId}, false))/>
  <#assign assocProduct = (delegator.findOne("Product", {"productId" : productAssoc.productIdTo}, false))/>
  <#if assocProduct?has_content>
    <@td><a href="<@ofbizUrl>/product?product_id=${productAssoc.productIdTo}</@ofbizUrl>"class="${styles.button_default!}">${productAssoc.productIdTo}</a></@td>
    <@td>- ${(assocProduct.productName)!}<i>(${(productAssocType.description)?default("Unknown")})</i></@td>
  </#if>
</#macro>

<@section title="${uiLabelMap.OrderOrderItems}">
  <#if (shoppingCartSize > 0)>
    <form method="post" action="<@ofbizUrl>modifycart</@ofbizUrl>" name="cartform">
      <input type="hidden" name="removeSelected" value="false"/>
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <input type="hidden" name="finalizeReqShipInfo" value="false"/>
        <input type="hidden" name="finalizeReqOptions" value="false"/>
        <input type="hidden" name="finalizeReqPayInfo" value="false"/>
        <input type="hidden" name="finalizeReqAdditionalParty" value="false"/>
      </#if>
      <@table type="data-complex" autoAltRows=true class="basic-table" cellspacing="0" cellpadding="1" border="0">
        <@thead>
        <@tr>
          <@th>&nbsp;</@th>
          <@th colspan="2">
              <b>${uiLabelMap.ProductProduct}</b>
              <#if (shoppingCart.getOrderType() == 'SALES_ORDER') && (productStore.showCheckoutGiftOptions)?default('Y') != 'N'>
                  <select name="GWALL" onchange="javascript:gwAll(this);">
                    <option value="">${uiLabelMap.OrderGiftWrapAllItems}</option>
                    <option value="NO^">${uiLabelMap.OrderNoGiftWrap}</option>
                    <#if allgiftWraps?has_content>
                      <#list allgiftWraps as option>
                        <option value="${option.productFeatureId?default("")}">${option.description?default("")} : <@ofbizCurrency amount=option.defaultAmount?default(0) isoCode=currencyUomId/></option>
                      </#list>
                    </#if>
                  </select>
              </#if>
          </@th>
          <@th align="center"><b>${uiLabelMap.OrderQuantity}</b></@th>
          <@th align="right"><b>${uiLabelMap.CommonUnitPrice}</b></@th>
          <@th align="right"><b>${uiLabelMap.OrderAdjustments}</b></@th>
          <@th align="right"><b>${uiLabelMap.OrderItemTotal}</b></@th>
          <@th align="center"><input type="checkbox" name="selectAll" value="0" onclick="javascript:toggleAll(this);" /></@th>
        </@tr>
        </@thead>

        <#assign itemsFromList = false>
        <#list shoppingCart.items() as cartLine>
          <#assign cartLineIndex = shoppingCart.getItemIndex(cartLine)>
          <#assign lineOptionalFeatures = cartLine.getOptionalProductFeatures()>
          <#--<@tr type="util"><@td colspan="8"><hr/></@td></@tr>-->
          <@tr valign="top">
            <@td>&nbsp;</@td>
            <@td>
          <@table type="fields" inheritAltRows=true class="basic-table" border="0">
          <@tr><@td colspan="2">
                  <#if cartLine.getProductId()??>
                    <#-- product item -->
                    <a href="<@ofbizUrl>product?product_id=${cartLine.getProductId()}</@ofbizUrl>" class="${styles.button_default!}">${cartLine.getProductId()}</a> -
                    <input size="60" type="text" name="description_${cartLineIndex}" value="${cartLine.getName()?default("")}"/><br />
                    <i>${cartLine.getDescription()!}</i>
                    <#if shoppingCart.getOrderType() != "PURCHASE_ORDER">
                      <#-- only applies to sales orders, not purchase orders -->
                      <#-- if inventory is not required check to see if it is out of stock and needs to have a message shown about that... -->
                      <#assign itemProduct = cartLine.getProduct()>
                      <#assign isStoreInventoryNotRequiredAndNotAvailable = Static["org.ofbiz.product.store.ProductStoreWorker"].isStoreInventoryRequiredAndAvailable(request, itemProduct, cartLine.getQuantity(), false, false)>
                      <#if isStoreInventoryNotRequiredAndNotAvailable && itemProduct.inventoryMessage?has_content>
                          <b>(${itemProduct.inventoryMessage})</b>
                      </#if>
                    </#if>
                  <#else>
                    <#-- this is a non-product item -->
                    <b>${cartLine.getItemTypeDescription()!}</b> : ${cartLine.getName()!}
                  </#if>
                    <#-- display the item's features -->
                   <#assign features = "">
                   <#if cartLine.getFeaturesForSupplier(dispatcher,shoppingCart.getPartyId())?has_content>
                       <#assign features = cartLine.getFeaturesForSupplier(dispatcher, shoppingCart.getPartyId())>
                   <#elseif cartLine.getStandardFeatureList()?has_content>
                       <#assign features = cartLine.getStandardFeatureList()>
                   </#if>
                   <#if features?has_content>
                     <br /><i>${uiLabelMap.ProductFeatures}: <#list features as feature>${feature.description?default("")} </#list></i>
                   </#if>
                    <#-- show links to survey response for this item -->
                    <#if cartLine.getAttribute("surveyResponses")?has_content>
                        <br />Surveys:
                       <#list cartLine.getAttribute("surveyResponses") as surveyResponseId>
                        <a href="/content/control/ViewSurveyResponses?surveyResponseId=${surveyResponseId}${StringUtil.wrapString(externalKeyParam)}" class="${styles.button_default!}" style="font-size: xx-small;">${surveyResponseId}</a>
                       </#list>
                    </#if>
            </@td></@tr>
            <#if cartLine.getRequirementId()?has_content>
                <@tr>
                    <@td colspan="2">
                      <b>${uiLabelMap.OrderRequirementId}</b>: ${cartLine.getRequirementId()!}
                    </@td>
                </@tr>
            </#if>
            <#if cartLine.getQuoteId()?has_content>
                <#if cartLine.getQuoteItemSeqId()?has_content>
                  <@tr>
                    <@td colspan="2">
                      <b>${uiLabelMap.OrderOrderQuoteId}</b>: ${cartLine.getQuoteId()!} - ${cartLine.getQuoteItemSeqId()!}
                    </@td>
                  </@tr>
                </#if>
            </#if>
            <#if cartLine.getItemComment()?has_content>
              <@tr><@td>${uiLabelMap.CommonComment} : </@td>
                  <@td>${cartLine.getItemComment()!}
              </@td></@tr>
            </#if>
            <#if cartLine.getDesiredDeliveryDate()?has_content>
              <@tr><@td>${uiLabelMap.OrderDesiredDeliveryDate}: </@td>
                  <@td>${cartLine.getDesiredDeliveryDate()!}
              </@td></@tr>
            </#if>
            <#-- inventory summary -->
            <#if cartLine.getProductId()??>
              <#assign productId = cartLine.getProductId()>
              <#assign product = cartLine.getProduct()>
              <@tr>
                <@td colspan="2">
                    <a href="/catalog/control/EditProductInventoryItems?productId=${productId}" class="${styles.button_default!}"><b>${uiLabelMap.ProductInventory}</b></a> : 
                    ${uiLabelMap.ProductAtp} = ${availableToPromiseMap.get(productId)}, ${uiLabelMap.ProductQoh} = ${quantityOnHandMap.get(productId)}
                    <#if Static["org.ofbiz.entity.util.EntityTypeUtil"].hasParentType(delegator, "ProductType", "productTypeId", product.productTypeId, "parentTypeId", "MARKETING_PKG")>
                    ${uiLabelMap.ProductMarketingPackageATP} = ${mktgPkgATPMap.get(productId)}, ${uiLabelMap.ProductMarketingPackageQOH} = ${mktgPkgQOHMap.get(productId)}
                    <#if (mktgPkgATPMap.get(cartLine.getProductId()) < cartLine.getQuantity()) && (shoppingCart.getOrderType() == 'SALES_ORDER')>
                      <#assign backOrdered = cartLine.getQuantity() - mktgPkgATPMap.get(cartLine.getProductId())/>
                      <span style="color: red; font-size: 15px;">[${backOrdered!}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
                    </#if>
                    </#if>
                    <#if (availableToPromiseMap.get(cartLine.getProductId()) <= 0) && (shoppingCart.getOrderType() == 'SALES_ORDER') && product.productTypeId! != "DIGITAL_GOOD" && product.productTypeId! != "MARKETING_PKG_AUTO" && product.productTypeId! != "MARKETING_PKG_PICK">
                      <span style="color: red;">[${cartLine.getQuantity()}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
                    <#else>
                      <#if (availableToPromiseMap.get(cartLine.getProductId()) < cartLine.getQuantity()) && (shoppingCart.getOrderType() == 'SALES_ORDER') && product.productTypeId != "DIGITAL_GOOD" && product.productTypeId != "MARKETING_PKG_AUTO" && product.productTypeId != "MARKETING_PKG_PICK">
                        <#assign backOrdered = cartLine.getQuantity() - availableToPromiseMap.get(cartLine.getProductId())/>
                        <span style="color: red;">[${backOrdered!}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
                      </#if>
                    </#if>
                </@td>
              </@tr>
            </#if>
            <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
              <#assign currentOrderItemType = cartLine.getItemTypeGenericValue()!/>
                <@tr>
                  <@td>
                      ${uiLabelMap.OrderOrderItemType}:
                      <select name="itemType_${cartLineIndex}">
                        <#if currentOrderItemType?has_content>
                        <option value="${currentOrderItemType.orderItemTypeId}">${currentOrderItemType.get("description",locale)}</option>
                        <option value="${currentOrderItemType.orderItemTypeId}">---</option>
                        </#if>
                        <option value="">&nbsp;</option>
                        <#list purchaseOrderItemTypeList as orderItemType>
                        <option value="${orderItemType.orderItemTypeId}">${orderItemType.get("description",locale)}</option>
                        </#list>
                      </select>
                  </@td>
                </@tr>
            </#if>

            <#-- ship before/after date -->
            <@tr>
              <@td colspan="2">
               <@table type="fields" class="basic-table" inheritAltRows=true border="0" cellpadding="0" cellspacing="0" width="100%">
               <@tr>
                <@td>
                  ${uiLabelMap.OrderShipAfterDate}
                    <@htmlTemplate.renderDateTimeField name="shipAfterDate_${cartLineIndex}" value="${cartLine.getShipAfterDate()?default('')}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="shipAfterDate_${cartLineIndex}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                </@td>
               </@tr>
               <@tr>
                <@td>
                  ${uiLabelMap.OrderShipBeforeDate}
                    <@htmlTemplate.renderDateTimeField name="shipBeforeDate_${cartLineIndex}" value="${cartLine.getShipBeforeDate()?default('')}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="shipBeforeDate_${cartLineIndex}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                </@td>
               </@tr>
               </@table>
              </@td>
            </@tr>

            <#-- Show Associated Products (not for Variants) -->
            <#if cartLine.getProductId()??>
              <#assign itemProductAssocList = cartLine.getProduct().getRelated("MainProductAssoc", null, Static["org.ofbiz.base.util.UtilMisc"].toList("productAssocTypeId", "sequenceNum"), false)!/>
            </#if>
            <#if itemProductAssocList?? && itemProductAssocList?has_content>
              <#--<@tr type="util"><@td colspan="8"><hr /></@td></@tr>-->
              <@tr>
                <@td>${uiLabelMap.OrderAssociatedProducts}</@td>
                <@td><a href="<@ofbizUrl>LookupAssociatedProducts?productId=${cartLine.getProductId()!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderQuickLookup}</a></@td>
              </@tr>
              <#assign relatedProdCount = 0/>
              <#list itemProductAssocList! as itemProductAssoc>
                <@tr>
                  <#if "PRODUCT_VARIANT" != itemProductAssoc.productAssocTypeId>
                    <#assign relatedProdCount = relatedProdCount + 1/>
                    <#if (relatedProdCount > 3)><#break></#if>
                    <@showAssoc productAssoc=itemProductAssoc />
                  </#if>
                </@tr>
              </#list>
            </#if>
          </@table>

                <#if (cartLine.getIsPromo() && cartLine.getAlternativeOptionProductIds()?has_content)>
                  <#-- Show alternate gifts if there are any... -->
                  <div>${uiLabelMap.OrderChooseFollowingForGift}:</div>
                  <#list cartLine.getAlternativeOptionProductIds() as alternativeOptionProductId>
                    <#assign alternativeOptionProduct = delegator.findOne("Product", Static["org.ofbiz.base.util.UtilMisc"].toMap("productId", alternativeOptionProductId), true)>
                    <#assign alternativeOptionName = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(alternativeOptionProduct, "PRODUCT_NAME", locale, dispatcher)!>
                    <div><a href="<@ofbizUrl>setDesiredAlternateGwpProductId?alternateGwpProductId=${alternativeOptionProductId}&amp;alternateGwpLine=${cartLineIndex}</@ofbizUrl>" class="${styles.button_default!}">Select: ${alternativeOptionName?default(alternativeOptionProductId)}</a></div>
                  </#list>
                </#if>
            </@td>

            <#-- gift wrap option -->
            <#assign showNoGiftWrapOptions = false>
            <@td nowrap="nowrap" align="right">
              <#assign giftWrapOption = lineOptionalFeatures.GIFT_WRAP!>
              <#assign selectedOption = cartLine.getAdditionalProductFeatureAndAppl("GIFT_WRAP")!>
              <#if giftWrapOption?has_content>
                <select name="option^GIFT_WRAP_${cartLineIndex}" onchange="javascript:document.cartform.submit()">
                  <option value="NO^">${uiLabelMap.OrderNoGiftWrap}</option>
                  <#list giftWrapOption as option>
                    <option value="${option.productFeatureId}" <#if ((selectedOption.productFeatureId)?? && selectedOption.productFeatureId == option.productFeatureId)>selected="selected"</#if>>${option.description} : <@ofbizCurrency amount=option.amount?default(0) isoCode=currencyUomId/></option>
                  </#list>
                </select>
              <#elseif showNoGiftWrapOptions>
                <select name="option^GIFT_WRAP_${cartLineIndex}" onchange="javascript:document.cartform.submit()">
                  <option value="">${uiLabelMap.OrderNoGiftWrap}</option>
                </select>
              <#else>
                &nbsp;
              </#if>
            </@td>
            <#-- end gift wrap option -->
            <@td nowrap="nowrap" align="center">
                <#if cartLine.getIsPromo() || cartLine.getShoppingListId()??>
                    ${cartLine.getQuantity()?string.number}
                <#else>
                    <input size="6" type="text" name="update_${cartLineIndex}" value="${cartLine.getQuantity()?string.number}"/>
                </#if>
                <#if (cartLine.getSelectedAmount() > 0) >
                  <br /><b>${uiLabelMap.OrderAmount}:</b><br /><input size="6" type="text" name="amount_${cartLineIndex}" value="${cartLine.getSelectedAmount()?string.number}"/>
                </#if>
            </@td>
            <@td nowrap="nowrap" align="right">
                <#if cartLine.getIsPromo() || (shoppingCart.getOrderType() == "SALES_ORDER" && !security.hasEntityPermission("ORDERMGR", "_SALES_PRICEMOD", session))>
                  <@ofbizCurrency amount=cartLine.getDisplayPrice() isoCode=currencyUomId/>
                <#else>
                    <#if (cartLine.getSelectedAmount() > 0) >
                        <#assign price = cartLine.getBasePrice() / cartLine.getSelectedAmount()>
                    <#else>
                        <#assign price = cartLine.getBasePrice()>
                    </#if>
                    <input size="8" type="text" name="price_${cartLineIndex}" value="<@ofbizAmount amount=price/>"/>
                </#if>
            </@td>
            <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=cartLine.getOtherAdjustments() isoCode=currencyUomId/></@td>
            <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=cartLine.getDisplayItemSubTotal() isoCode=currencyUomId/></@td>
            <@td nowrap="nowrap" align="center"><#if !cartLine.getIsPromo()><input type="checkbox" name="selectedItem" value="${cartLineIndex}" onclick="javascript:checkToggle(this);"/><#else>&nbsp;</#if></@td>
          </@tr>
        </#list>

      <@tfoot>
        <@tr type="util"><@td colspan="8"><hr /></@td></@tr>
        <#if shoppingCart.getAdjustments()?has_content>
              <@tr>
                <@td colspan="6" nowrap="nowrap" align="right">${uiLabelMap.OrderSubTotal}:</@td>
                <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=shoppingCart.getSubTotal() isoCode=currencyUomId/></@td>
                <@td>&nbsp;</@td>
              </@tr>
            <#list shoppingCart.getAdjustments() as cartAdjustment>
              <#assign adjustmentType = cartAdjustment.getRelatedOne("OrderAdjustmentType", true)>
              <#if adjustmentType.get("orderAdjustmentTypeId",locale) != 'SHIPPING_CHARGES'>
                <@tr>
                  <@td colspan="6" nowrap="nowrap" align="right">
                      <i>${uiLabelMap.OrderAdjustment}</i> - ${adjustmentType.get("description",locale)!}
                    <#if cartAdjustment.productPromoId?has_content><a href="<@ofbizUrl>showPromotionDetails?productPromoId=${cartAdjustment.productPromoId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDetails}</a></#if>:
                  </@td>
                  <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(cartAdjustment, shoppingCart.getSubTotal()) isoCode=currencyUomId/></@td>
                  <@td>&nbsp;</@td>
                </@tr>
              </#if>
            </#list>
            <#if (orderShippingTotal != 0)>
              <@tr>
                <@td align="right" colspan="4">${uiLabelMap.FacilityShippingAndHandling}</@td>
                <@td align="right" nowrap="nowrap"><@ofbizCurrency amount=orderShippingTotal isoCode=currencyUomId/></@td>
              </@tr>
            </#if>
            <#if (orderTaxTotal != 0)>
              <@tr>
                <@td align="right" colspan="4">${uiLabelMap.OrderSalesTax}</@td>
                <@td align="right" nowrap="nowrap"><@ofbizCurrency amount=orderTaxTotal isoCode=currencyUomId/></@td>
              </@tr>
            </#if>
        </#if>

        <@tr>
          <@td colspan="6" align="right" valign="bottom">
            <b>${uiLabelMap.OrderCartTotal}:</b>
          </@td>
          <@td align="right" valign="bottom">
            <hr />
            <div><b><@ofbizCurrency amount=shoppingCart.getGrandTotal() isoCode=currencyUomId/></b></div>
          </@td>
          <@td>&nbsp;</@td>
        </@tr>
        <#--<@tr>
          <@td colspan="8">&nbsp;</@td>
        </@tr>-->
      </@tfoot>
      </@table>
    </form>
  <#else>
    <@resultMsg>${uiLabelMap.OrderNoOrderItemsToDisplay}</@resultMsg>
  </#if>
</@section>

