<#include "component://shop/webapp/shop/order/subscriptioncommon.ftl">

<@section title=uiLabelMap.ShopOrderSubscriptionItems menuContent=menuContent>
  <@table type="data-complex" class="+order-detail-items">
    <@thead>
    <@tr>
      <#if maySelect>
        <#-- SCIPIO: TODO? could want to omit some these when showDetailed==false -->
        <@th width="25%">${uiLabelMap.OrderProduct}</@th>
        <@th width="10%">${uiLabelMap.OrderQtyOrdered}</@th>
        <@th width="10%">${uiLabelMap.OrderQtyPicked}</@th>
        <@th width="10%">${uiLabelMap.OrderQtyShipped}</@th>
        <@th width="10%">${uiLabelMap.OrderQtyCanceled}</@th>
        <@th width="10%">${uiLabelMap.EcommerceUnitPrice}</@th>
      <#else>
        <@th width="45%">${uiLabelMap.OrderProduct}</@th>
        <@th width="25%" colspan="3">${uiLabelMap.ShopSubscriptionDetails}</@th>        
        <@th width="15%">${uiLabelMap.CommonQuantity}</@th><#--${uiLabelMap.OrderQtyOrdered}-->
        <@th width="10%">${uiLabelMap.EcommerceUnitPrice}</@th>
      </#if>
      <@th width="15%">${uiLabelMap.OrderAdjustments}</@th>
      <@th width="15%">${uiLabelMap.EcommerceItemTotal}</@th><#--${uiLabelMap.CommonSubtotal}-->
      <#if orderHeader?has_content && orderHeader.statusId == "ORDER_CREATED">
        <@th width="15%">${uiLabelMap.EcommerceAuthSubscription}</@th>
      </#if>
      <#if (maySelect) && ((roleTypeId!) == "PLACING_CUSTOMER")>
        <@th colspan="4"></@th>
      <#else>
        <#-- SCIPIO: even if maySelect false, add extra column to standardize look -->
        <@th colspan="2">&nbsp;</@th>
      </#if>
      
    </@tr>
    </@thead>
    <@tbody>
         <#list subscriptionItems.keySet() as orderItem>      
            <#assign producSubscriptionResources = subscriptionItems.get(orderItem)>
      
      
      <#-- get info from workeffort and calculate rental quantity, if it was a rental item -->
      <#assign rentalQuantity = 1> <#-- no change if no rental item -->
      <#if orderItem.orderItemTypeId == "RENTAL_ORDER_ITEM" && workEfforts??>
        <#list workEfforts as workEffort>
          <#if workEffort.workEffortId == orderItem.orderItemSeqId>
            <#assign rentalQuantity = localOrderReadHelper.getWorkEffortRentalQuantity(workEffort)>
            <#assign workEffortSave = workEffort>
            <#break>
          </#if>
        </#list>
      <#else>
        <#assign WorkOrderItemFulfillments = orderItem.getRelated("WorkOrderItemFulfillment", null, null, false)!>
        <#if WorkOrderItemFulfillments?has_content>
          <#list WorkOrderItemFulfillments as WorkOrderItemFulfillment>
            <#assign workEffortSave = WorkOrderItemFulfillment.getRelatedOne("WorkEffort", true)!>
            <#break>
          </#list>
        </#if>
      </#if>

      <#--<@tr><@td colspan="${numColumns}"></@td></@tr>-->

      <@tr>
        <#-- SCIPIO: Workaround for access from macros -->
        <#assign orderItem = orderItem>

        <#-- SCIPIO: Use a cancel link form toggle to prevent cluttering up things by default -->
        <#assign cancelItemLabel = getLabel("StatusValidChange.transitionName.ITEM_APPROVED.ITEM_CANCELLED", "CommonEntityLabels")?replace(" ", "&nbsp;")>
        <#macro cancelItemForm>
            <#-- SCIPIO: FIXME: -->
            <@alert type="warning">${uiLabelMap.CommonWarning}: Cancel may fail for some payment methods</@alert>
            
            <@field type="select" name="irm_${orderItem.orderItemSeqId}" label=uiLabelMap.OrderReturnReason>
              <#-- SCIPIO: Usually stores want a reason...<option value=""></option>-->
              <#list orderItemChangeReasons as reason>
                <option value="${reason.enumId}"<#if (parameters["irm_${orderItem.orderItemSeqId}"]!) == reason.enumId> selected="selected"</#if>>${reason.get("description",locale)!(reason.enumId)}</option>
              </#list>
            </@field>
            <@field type="text" name="icm_${orderItem.orderItemSeqId}" value=(parameters["icm_${orderItem.orderItemSeqId}"]!) size="30" maxlength="60" label=uiLabelMap.CommonComments/>
            <br/><@field type="submit" submitType="link" href="javascript:document.addCommonToCartForm.action='${makeOfbizUrl('cancelOrderItem')?js_string}';document.addCommonToCartForm.submit()" 
                class="${styles.link_run_sys!} ${styles.action_terminate!}" text=cancelItemLabel />
            <input type="hidden" name="orderItemSeqId" value="${orderItem.orderItemSeqId}"/>
            <#-- SCIPIO: Extra hidden input to help with hide/show logic -->
            <input type="hidden" name="cancelitem_${orderItem.orderItemSeqId}" value="Y"/>
        </#macro>
        <#macro cancelLinkContent>
          <#-- SCIPIO: NOTE: Originally this was going to be a modal, but it does not work easily as the fields no longer fall within the <form> when they are in a modal and call fails -->
          <a href="javascript:jQuery('#row_orderitem_cancel_${orderItem.orderItemSeqId}').toggle(); void(0);" class="${styles.link_nav_inline!}">[${cancelItemLabel}]</a>
          <#--<@modal id="row_orderitem_cancel_${orderItem.orderItemSeqId}" label="[${cancelItemLabel}]">
            <@section title="${rawString(cancelItemLabel)}: ${rawString(orderItem.itemDescription!)}">
              <@cancelItemForm />
            </@section>
          </@modal>-->
        </#macro>
        <#assign mayCancelItem = false>
        <#if maySelect>
          <#assign pickedQty = localOrderReadHelper.getItemPickedQuantityBd(orderItem)>
          <#assign mayCancelItem = (orderHeader.statusId != "ORDER_SENT" && orderItem.statusId != "ITEM_COMPLETED" && orderItem.statusId != "ITEM_CANCELLED" && pickedQty == 0)>
        </#if>
        <#if !orderItem.productId?? || orderItem.productId == "_?_">
          <#-- non-product item -->
          <@td>
            ${escapeVal(orderItem.itemDescription!"", 'htmlmarkup', {"allow":"internal"})} <#if !printable && maySelect && mayCancelItem> <@cancelLinkContent /></#if>
            <#assign orderItemAttributes = orderItem.getRelated("OrderItemAttribute", null, null, false)!/>
            <#if orderItemAttributes?has_content>
                <ul>
                <#list orderItemAttributes as orderItemAttribute>
                    <li>
                        ${orderItemAttribute.attrName} : ${orderItemAttribute.attrValue}
                    </li>
                </#list>
                </ul>
            </#if>
          </@td>
        <#else>
          <#-- product item -->
          <#assign product = orderItem.getRelatedOne("Product", true)!/> <#-- should always exist because of FK constraint, but just in case -->
          <@td>
            <#if !printable><a href="<@ofbizCatalogAltUrl fullPath="true" secure="false" productId=orderItem.productId/>" class="${styles.link_nav_info_desc!}" target="_blank"></#if>${orderItem.productId} - ${orderItem.itemDescription!""}<#if !printable></a></#if>
            <#-- SCIPIO: Link to downloads to consume -->
            <#-- TODO: delegate status tests -->
            <#if !printable && orderHeader?has_content && !["ORDER_REJECTED", "ORDER_CANCELLED"]?seq_contains(orderHeader.statusId!)>
              <#if (productDownloads[orderItem.productId!])?has_content><#-- implied?: (product.productType!) == "DIGITAL_GOOD" && -->
                <#assign dlAvail = ((orderHeader.statusId!) == "ORDER_COMPLETED")>
                <a href="<#if dlAvail><@ofbizUrl uri="orderdownloads" /><#else>javascript:void(0);</#if>" class="${styles.link_nav_inline!} ${styles.action_export!}<#if !dlAvail> ${styles.disabled!} ${styles.tooltip!}</#if>"<#rt/>
                    <#if !dlAvail> title="${uiLabelMap.ShopDownloadsAvailableOnceOrderCompleted}"</#if>>[${uiLabelMap.ContentDownload}]</a><#lt/>
              </#if>
            </#if>

            <#-- SCIPIO: TODO: LIST CONFIG OPTIONS HERE -->

            <#assign orderItemAttributes = orderItem.getRelated("OrderItemAttribute", null, null, false)!/>
            <#if orderItemAttributes?has_content>
                <ul>
                <#list orderItemAttributes as orderItemAttribute>
                    <li>
                        ${orderItemAttribute.attrName} : ${orderItemAttribute.attrValue}
                    </li>
                </#list>
                </ul>
            </#if>
            <#if showDetailed && product?has_content>
              <#if product.piecesIncluded?? && product.piecesIncluded?long != 0>
                  [${uiLabelMap.OrderPieces}: ${product.piecesIncluded}]
              </#if>
              <#if (product.quantityIncluded?? && product.quantityIncluded != 0) || product.quantityUomId?has_content>
                <#assign quantityUom = product.getRelatedOne("QuantityUom", true)!/>
                  [${uiLabelMap.CommonQuantity}: ${product.quantityIncluded!} ${((quantityUom.abbreviation)!(product.quantityUomId))!}]
              </#if>
              <#if (product.weight?? && product.weight != 0) || product.weightUomId?has_content>
                <#assign weightUom = product.getRelatedOne("WeightUom", true)!/>
                  [${uiLabelMap.CommonWeight}: ${product.weight!} ${((weightUom.abbreviation)!(product.weightUomId))!}]
              </#if>
              <#if (product.productHeight?? && product.productHeight != 0) || product.heightUomId?has_content>
                <#assign heightUom = product.getRelatedOne("HeightUom", true)!/>
                  [${uiLabelMap.CommonHeight}: ${product.productHeight!} ${((heightUom.abbreviation)!(product.heightUomId))!}]
              </#if>
              <#if (product.productWidth?? && product.productWidth != 0) || product.widthUomId?has_content>
                <#assign widthUom = product.getRelatedOne("WidthUom", true)!/>
                  [${uiLabelMap.CommonWidth}: ${product.productWidth!} ${((widthUom.abbreviation)!(product.widthUomId))!}]
              </#if>
              <#if (product.productDepth?? && product.productDepth != 0) || product.depthUomId?has_content>
                <#assign depthUom = product.getRelatedOne("DepthUom", true)!/>
                  [${uiLabelMap.CommonDepth}: ${product.productDepth!} ${((depthUom.abbreviation)!(product.depthUomId))!}]
              </#if>
            </#if>
            <#if maySelect>
              <#assign returns = orderItem.getRelated("ReturnItem", null, null, false)!>
              <#if returns?has_content>
                <#list returns as return>
                  <#assign returnHeader = return.getRelatedOne("ReturnHeader", false)>
                  <#if returnHeader.statusId != "RETURN_CANCELLED">
                    <#if returnHeader.statusId == "RETURN_REQUESTED" || returnHeader.statusId == "RETURN_APPROVED">
                      <#assign displayState = "${uiLabelMap.OrderOrderReturn} ${uiLabelMap.PartyPending}">
                    <#else>
                      <#assign displayState = uiLabelMap.OrderReturned>
                    </#if>
                    ${displayState} (#${return.returnId})
                  </#if>
                </#list>
              </#if>
            </#if>
            <#if !printable && maySelect && mayCancelItem> <@cancelLinkContent /></#if>
          </@td>
          <#if !(maySelect)>
            <@td>
                <ul>
                <#list producSubscriptionResources as productSubscriptionResource>                    
                    <li>
                        <@subscriptionLinkContent productSubscriptionResource {} orderItem productSubscriptionResource_index/>
                    </li>
                </#list>
                </ul>
            </@td>
            <@td></@td>
            <@td></@td>
          </#if>
          <@td>
            ${orderItem.quantity?string.number}
          </@td>
          <#if maySelect>
          <@td>
            <#assign pickedQty = localOrderReadHelper.getItemPickedQuantityBd(orderItem)>
            <#if (pickedQty > 0) && orderHeader.statusId == "ORDER_APPROVED">${(pickedQty!0)?string.number}<#else>${(pickedQty!0)?string.number}</#if>
          </@td>
          <@td>
            <#assign shippedQty = localOrderReadHelper.getItemShippedQuantity(orderItem)>
            ${(shippedQty!0)?string.number}
          </@td>
          <@td>
            <#assign canceledQty = localOrderReadHelper.getItemCanceledQuantity(orderItem)>
            ${(canceledQty!0)?string.number}
          </@td>
          </#if>
          <@td>
            <@ofbizCurrency amount=orderItem.unitPrice isoCode=currencyUomId/>
          </@td>
          <@td>
            <@ofbizCurrency amount=localOrderReadHelper.getOrderItemAdjustmentsTotal(orderItem) isoCode=currencyUomId/>
          </@td>
          <@td>
            <#-- SCIPIO: THIS IS WRONG - MUST USE getOrderItemSubTotal INSTEAD!
            <#if workEfforts??>
              <@ofbizCurrency amount=localOrderReadHelper.getOrderItemTotal(orderItem)*rentalQuantity isoCode=currencyUomId/>
            <#else>
              <@ofbizCurrency amount=localOrderReadHelper.getOrderItemTotal(orderItem) isoCode=currencyUomId/>
            </#if>-->
            <#if workEfforts??>
              <@ofbizCurrency amount=localOrderReadHelper.getOrderItemSubTotal(orderItem)*rentalQuantity isoCode=currencyUomId/>
            <#else>
              <@ofbizCurrency amount=localOrderReadHelper.getOrderItemSubTotal(orderItem) isoCode=currencyUomId/>
            </#if>
          </@td>
          
          <#if orderItem.statusId != "ITEM_CREATED">
            <@td>
                <input type="hidden" name="subscriptionId_${orderItem.orderItemSeqId}" value="${orderItem.subscriptionId!}"/>
                <input type="hidden" name="authorizeItem_${orderItem.orderItemSeqId}" value="N"/>
                <#if orderItem.statusId == "ITEM_APPROVED">
                    <@field type="submit" submitType="link" href="javascript:document.addCommonToCartForm.action='${makeOfbizUrl('authorizeBillingAgreement')?js_string}';document.addCommonToCartForm.authorizeItem_${orderItem.orderItemSeqId}.value='Y';document.addCommonToCartForm.submit();" 
                    class="${styles.link_run_sys!} ${styles.action_terminate!}" text="${uiLabelMap.ShopAuthorizeSubscription}" />
                <#elseif orderItem.statusId == "ITEM_COMPLETED">
                    <@field type="submit" submitType="link" href="javascript:document.addCommonToCartForm.action='${makeOfbizUrl('cancelBillingAgreementPayPalRest')?js_string}';document.addCommonToCartForm.cancelItem_${orderItem.orderItemSeqId}.value='Y';document.addCommonToCartForm.submit();" 
                    class="${styles.link_run_sys!} ${styles.action_terminate!}" text="${uiLabelMap.ShopCancelSubscription}" />
                </#if>
            </@td>
          </#if>
       
          <#if maySelect && (roleTypeId!) == "PLACING_CUSTOMER">
            <@td></@td>
            <@td>
              <input name="item_id" value="${orderItem.orderItemSeqId}" type="checkbox"/>
            </@td>
            <@td></@td>
          <#else>
            <@td></@td>
          </#if>
         
        </#if>
      </@tr>
      <#-- now cancel reason and comment field -->
      <#-- SCIPIO: Inlined cancel item form -->
      <#if !printable && maySelect && mayCancelItem>
        <#assign style = "">
        <#-- only display initially if there was an attempt to cancel (which presumably failed, otherwise mayCancelItem will go false) -->
        <#if (parameters["cancelitem_${orderItem.orderItemSeqId}"]!) != "Y">
          <#assign style = "display:none;">
        </#if>
        <@tr id="row_orderitem_cancel_${orderItem.orderItemSeqId}" style=style>
          <@td colspan="7">
            <@row>
              <@cell small=2>
                <strong>${cancelItemLabel}:</strong>
              </@cell>
              <@cell small=10>
                <@fields type="default-compact">
                  <@cancelItemForm />
                </@fields>
              </@cell>
            </@row>
          </@td>
          <@td colspan="4">
          </@td>
        </@tr>
      </#if>
      <#-- show info from workeffort if it was a rental item -->
      <#if orderItem.orderItemTypeId == "RENTAL_ORDER_ITEM">
        <#if workEffortSave??>
          <@tr><@td colspan="${numColumns}">${uiLabelMap.CommonFrom}: ${workEffortSave.estimatedStartDate?string("yyyy-MM-dd")}<#rt/>
            <#lt/> ${uiLabelMap.CommonUntil} ${workEffortSave.estimatedCompletionDate?string("yyyy-MM-dd")} ${uiLabelMap.CommonFor} ${workEffortSave.reservPersons} ${uiLabelMap.CommonPerson}(s)</@td></@tr>
        </#if>
      </#if>
      <#-- now show adjustment details per line item -->
      <#assign itemAdjustments = localOrderReadHelper.getOrderItemAdjustments(orderItem)>
      <#if showDetailed && showDetailedAdjustments>
      <#list itemAdjustments as orderItemAdjustment>
        <#-- SCIPIO: tax adjustments are especially confusing, so have their own option to hide -->
        <#if showDetailedTax || !["SALES_TAX"]?seq_contains(orderItemAdjustment.orderAdjustmentTypeId)>
        <@tr>
          <@td>
            <#--${uiLabelMap.EcommerceAdjustment}: ${localOrderReadHelper.getAdjustmentType(orderItemAdjustment)}-->
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
                  : <@ofbizCurrency amount=orderItemAdjustment.amountAlreadyIncluded isoCode=currencyUomId/>
                </#if>
            </#if>
          </@td>
          <@td colspan="5"></@td>
          <@td>
            <@ofbizCurrency amount=localOrderReadHelper.getOrderItemAdjustmentTotal(orderItem, orderItemAdjustment) isoCode=currencyUomId/>
          </@td>
          <@td></@td>
          <#if maySelect><@td colspan="3"></@td><#else><@td></@td></#if>
        </@tr>
        </#if>
      </#list>
      </#if>
      <#-- show the order item ship group info -->
      <#assign orderItemShipGroupAssocs = orderItem.getRelated("OrderItemShipGroupAssoc", null, null, false)!>
      <#if showDetailed && orderItemShipGroupAssocs?has_content>
        <#list orderItemShipGroupAssocs as shipGroupAssoc>
          <#assign shipGroup = shipGroupAssoc.getRelatedOne("OrderItemShipGroup", false)!>
          <#assign shipGroupAddress = (shipGroup.getRelatedOne("PostalAddress", false))!>
          <@tr>
            <@td>
              ${uiLabelMap.OrderShipGroup}: [${shipGroup.shipGroupSeqId}] ${shipGroupAddress.address1!(uiLabelMap.CommonNA)}
            </@td>
            <@td>
              <#-- SCIPIO: Don't show this if maySelect false because in that case there's no header and the quantity comes out of thin air. -->
            <#if maySelect>
              ${shipGroupAssoc.quantity?string.number}
            </#if>
            </@td>
            <@td colspan="${numColumns - 2}"></@td>
          </@tr>
        </#list>
      </#if>
        
    </#list>

    </@tbody>
  </@table>
</@section>