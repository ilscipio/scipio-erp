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
    <@td><a href="<@ofbizUrl>product?product_id=${productAssoc.productIdTo}</@ofbizUrl>"class="${styles.link_nav_info_id!}">${productAssoc.productIdTo}</a></@td>
    <@td>- ${(assocProduct.productName)!} <i>(${(productAssocType.description)!"Unknown"})</i></@td>
  </#if>
</#macro>

<@section title=uiLabelMap.OrderOrderItems>
  <#if (shoppingCartSize > 0)>
    <form method="post" action="<@ofbizUrl>modifycart</@ofbizUrl>" name="cartform">
      <input type="hidden" name="removeSelected" value="false"/>
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <input type="hidden" name="finalizeReqShipInfo" value="false"/>
        <input type="hidden" name="finalizeReqOptions" value="false"/>
        <input type="hidden" name="finalizeReqPayInfo" value="false"/>
        <input type="hidden" name="finalizeReqAdditionalParty" value="false"/>
      </#if>
      <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="1" --> <#-- orig: border="0" -->
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
                        <option value="${option.productFeatureId!""}">${option.description!""} : <@ofbizCurrency amount=option.defaultAmount?default(0) isoCode=currencyUomId/></option>
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

          <@fields type="default-manual-widgetonly">
          <@table type="fields" inheritAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: border="0" -->
          <@tr><@td colspan="2">
                  <#if cartLine.getProductId()??>
                    <#-- product item -->
                    <a href="<@ofbizUrl>product?product_id=${cartLine.getProductId()}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${cartLine.getProductId()}</a> -
                    <@field size="30" type="input" inline=true name="description_${cartLineIndex}" value=(cartLine.getName()!"")/><br />
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
                     <br /><i>${uiLabelMap.ProductFeatures}: <#list features as feature>${feature.description!""} </#list></i>
                   </#if>
                    <#-- show links to survey response for this item -->
                    <#if cartLine.getAttribute("surveyResponses")?has_content>
                        <br />Surveys:
                       <#list cartLine.getAttribute("surveyResponses") as surveyResponseId>
                        <a href="<@ofbizInterWebappUrl>/content/control/ViewSurveyResponses?surveyResponseId=${surveyResponseId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" style="font-size: xx-small;">${surveyResponseId}</a>
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
                    <a href="<@ofbizInterWebappUrl>/catalog/control/EditProductInventoryItems?productId=${productId}</@ofbizInterWebappUrl>" class="${styles.link_nav!} ${styles.action_update!}"><b>${uiLabelMap.ProductInventory}</b></a> : 
                    ${uiLabelMap.ProductAtp} = ${availableToPromiseMap.get(productId)}, ${uiLabelMap.ProductQoh} = ${quantityOnHandMap.get(productId)}
                    <#if Static["org.ofbiz.entity.util.EntityTypeUtil"].hasParentType(delegator, "ProductType", "productTypeId", product.productTypeId, "parentTypeId", "MARKETING_PKG")>
                    ${uiLabelMap.ProductMarketingPackageATP} = ${mktgPkgATPMap.get(productId)}, ${uiLabelMap.ProductMarketingPackageQOH} = ${mktgPkgQOHMap.get(productId)}
                    <#if (mktgPkgATPMap.get(cartLine.getProductId()) < cartLine.getQuantity()) && (shoppingCart.getOrderType() == 'SALES_ORDER')>
                      <#assign backOrdered = cartLine.getQuantity() - mktgPkgATPMap.get(cartLine.getProductId())/>
                      <span class="${styles.text_color_alert!}" style="font-size: 15px;">[${backOrdered!}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
                    </#if>
                    </#if>
                    <#assign isPhysical = Static["org.ofbiz.product.product.ProductWorker"].isPhysical(product)/>
                    <#if (availableToPromiseMap.get(cartLine.getProductId()) <= 0) && (shoppingCart.getOrderType() == 'SALES_ORDER') && (product.productTypeId!) != "MARKETING_PKG_AUTO" && (product.productTypeId!) != "MARKETING_PKG_PICK" && isPhysical>
                      <span class="${styles.text_color_alert!}">[${cartLine.getQuantity()}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
                    <#else>
                      <#if (availableToPromiseMap.get(cartLine.getProductId()) < cartLine.getQuantity()) && (shoppingCart.getOrderType() == 'SALES_ORDER') && product.productTypeId != "MARKETING_PKG_AUTO" && product.productTypeId != "MARKETING_PKG_PICK" && isPhysical>
                        <#assign backOrdered = cartLine.getQuantity() - availableToPromiseMap.get(cartLine.getProductId())/>
                        <span class="${styles.text_color_alert!}">[${backOrdered!}&nbsp;${uiLabelMap.OrderBackOrdered}]</span>
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
                      <@field type="select" name="itemType_${cartLineIndex}">
                        <#if currentOrderItemType?has_content>
                        <option value="${currentOrderItemType.orderItemTypeId}">${currentOrderItemType.get("description",locale)}</option>
                        <option value="${currentOrderItemType.orderItemTypeId}">---</option>
                        </#if>
                        <option value="">&nbsp;</option>
                        <#list purchaseOrderItemTypeList as orderItemType>
                        <option value="${orderItemType.orderItemTypeId}">${orderItemType.get("description",locale)}</option>
                        </#list>
                      </@field>
                  </@td>
                </@tr>
            </#if>

            <#-- ship before/after date -->
            <@tr>
              <@td colspan="2">
                <@fields type="default-compact">
                  <@field type="datetime" label=uiLabelMap.OrderShipAfterDate name="shipAfterDate_${cartLineIndex}" value=cartLine.getShipAfterDate()!'' size="25" maxlength="30" id="shipAfterDate_${cartLineIndex}" dateType="date-time" />
                  <@field type="datetime" label=uiLabelMap.OrderShipBeforeDate name="shipBeforeDate_${cartLineIndex}" value=cartLine.getShipBeforeDate()!'' size="25" maxlength="30" id="shipBeforeDate_${cartLineIndex}" dateType="date-time" />
                </@fields>
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
                <@td><a href="<@ofbizUrl>LookupAssociatedProducts?productId=${cartLine.getProductId()!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.OrderQuickLookup}</a></@td>
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
          </@fields>

                <#if (cartLine.getIsPromo() && cartLine.getAlternativeOptionProductIds()?has_content)>
                  <#-- Show alternate gifts if there are any... -->
                  <div>${uiLabelMap.OrderChooseFollowingForGift}:</div>
                  <#list cartLine.getAlternativeOptionProductIds() as alternativeOptionProductId>
                    <#assign alternativeOptionProduct = delegator.findOne("Product", {"productId":alternativeOptionProductId}, true)>
                    <#assign alternativeOptionName = Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(alternativeOptionProduct, "PRODUCT_NAME", locale, dispatcher, "html")!>
                    <div><a href="<@ofbizUrl>setDesiredAlternateGwpProductId?alternateGwpProductId=${alternativeOptionProductId}&amp;alternateGwpLine=${cartLineIndex}</@ofbizUrl>" class="${styles.link_run_session_long!} ${styles.action_update!}">Select: ${alternativeOptionName!(alternativeOptionProductId)}</a></div>
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
                    <#if cartAdjustment.productPromoId?has_content><a href="<@ofbizUrl>showPromotionDetails?productPromoId=${cartAdjustment.productPromoId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonDetails}</a></#if>:
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
            <#if orderVATTaxTotal?has_content && (orderVATTaxTotal > 0)>
              <@tr type="util"><@td colspan="8"><hr /></@td></@tr>
              <@tr>
                <@td align="right" colspan="4">${uiLabelMap.OrderSalesTaxIncluded}</@td>
                <@td align="right" nowrap="nowrap"><@ofbizCurrency amount=orderVATTaxTotal isoCode=currencyUomId/></@td>
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
    <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderItemsToDisplay}</@commonMsg>
  </#if>
</@section>

