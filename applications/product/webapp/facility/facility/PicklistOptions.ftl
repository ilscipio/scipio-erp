<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.FacilitySelectOptionsToGroupBy>
  <form method="post" name="selectFactors" action="<@ofbizUrl>PicklistOptions</@ofbizUrl>">
    <input type="hidden" name="facilityId" value="${facilityId}"/>
  <@row>
    <@cell columns=4>
      <@field type="checkbox" label=uiLabelMap.FacilityGroupByShippingMethod name="groupByShippingMethod" value="Y" checked=("${requestParameters.groupByShippingMethod!}" == "Y") />
    </@cell>
    <@cell columns=4>
      <@field type="checkbox" label=uiLabelMap.FacilityGroupByWarehouseArea name="groupByWarehouseArea" value="Y" checked=("${requestParameters.groupByWarehouseArea!}" == "Y") />
    </@cell>
    <@cell columns=4>
      <@field type="checkbox" label=uiLabelMap.FacilityGroupByNoOfOrderItems name="groupByNoOfOrderItems" value="Y" checked=("${requestParameters.groupByNoOfOrderItems!}" == "Y") />
    </@cell>
  </@row>
 
    <#-- TODO: these two were align=right... -->
    <@field type="input" label=uiLabelMap.FacilityGroupFirst size="4" name="maxNumberOfOrders" value="50"/>      
    <@field type="submit" text=uiLabelMap.CommonSubmit class="+${styles.link_run_sys!} ${styles.action_select!}" align="right"/>
  </form>
</@section>  
  
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("ReviewOrdersNotPickedOrPacked?facilityId=${facilityId}") text=uiLabelMap.FormFieldTitle_reviewOrdersNotPickedOrPacked class="+${styles.action_nav!} ${styles.action_view!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.ProductFindOrdersToPick menuContent=menuContent>
  <#if pickMoveInfoList?has_content>
    <@table type="data-complex" autoAltRows=false> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <#if pickMoveInfoList?has_content || rushOrderInfo?has_content>
       <@thead>
        <@tr class="header-row">
          <#if !((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
            <@th>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
          <#else>
            <@th>${uiLabelMap.ProductShipmentMethod}</@th>
            <@th>${uiLabelMap.ProductWarehouseArea}</@th>
            <@th>${uiLabelMap.ProductNumberOfOrderItems}</@th>
          </#if>
          <@th>${uiLabelMap.ProductReadyToPick}</@th>
          <@th>${uiLabelMap.ProductNeedStockMove}</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
      </#if>
      <#if rushOrderInfo?has_content>
        <#assign orderReadyToPickInfoList = rushOrderInfo.orderReadyToPickInfoList!>
        <#assign orderNeedsStockMoveInfoList = rushOrderInfo.orderNeedsStockMoveInfoList!>
        <#assign orderReadyToPickInfoListSize = (orderReadyToPickInfoList.size())?default(0)>
        <#assign orderNeedsStockMoveInfoListSize = (orderNeedsStockMoveInfoList.size())?default(0)>
        <@tr>
          <@td>[Rush Orders, all Methods]</@td>
          <@td>${orderReadyToPickInfoListSize}</@td>
          <@td>${orderNeedsStockMoveInfoListSize}</@td>
          <@td>
            <#if orderReadyToPickInfoList?has_content>
              <form method="post" action="<@ofbizUrl>createPicklistFromOrders</@ofbizUrl>">
                <input type="hidden" name="facilityId" value="${facilityId}"/>
                <input type="hidden" name="isRushOrder" value="Y"/>
                ${uiLabelMap.ProductPickFirst}:
                <input type="text" size="4" name="maxNumberOfOrders" value="20"/>
                <input type="submit" value="${uiLabelMap.ProductCreatePicklist}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
              </form>
            <#else>
              &nbsp;
            </#if>
          </@td>
        </@tr>
      </#if>
      
        <#assign orderReadyToPickInfoListSizeTotal = 0>
        <#assign orderNeedsStockMoveInfoListSizeTotal = 0>
        <#assign alt_row = false>
        <#list pickMoveInfoList as pickMoveInfo>
          <#assign groupName = pickMoveInfo.groupName!>
          <#assign groupName1 = pickMoveInfo.groupName1!>
          <#assign groupName2 = pickMoveInfo.groupName2!>
          <#assign groupName3 = pickMoveInfo.groupName3!>
          <#assign orderReadyToPickInfoList = pickMoveInfo.orderReadyToPickInfoList!>
          <#assign orderNeedsStockMoveInfoList = pickMoveInfo.orderNeedsStockMoveInfoList!>
          <#assign orderReadyToPickInfoListSize = (orderReadyToPickInfoList.size())?default(0)>
          <#assign orderNeedsStockMoveInfoListSize = (orderNeedsStockMoveInfoList.size())?default(0)>
          <#assign orderReadyToPickInfoListSizeTotal = orderReadyToPickInfoListSizeTotal + orderReadyToPickInfoListSize>
          <#assign orderNeedsStockMoveInfoListSizeTotal = orderNeedsStockMoveInfoListSizeTotal + orderNeedsStockMoveInfoListSize>
          <@tr valign="middle" alt=alt_row>
                
              <#assign viewGroupDetailForm>
                    <form name="viewGroupDetail_${pickMoveInfo_index}" action="<@ofbizUrl>PicklistOptions</@ofbizUrl>" method="post">
                      <input type="hidden" name="viewDetail" value="${groupName!}"/>
                      <input type="hidden" name="groupByShippingMethod" value="${requestParameters.groupByShippingMethod!}"/>
                      <input type="hidden" name="groupByWarehouseArea" value="${requestParameters.groupByWarehouseArea!}"/>
                      <input type="hidden" name="groupByNoOfOrderItems" value="${requestParameters.groupByNoOfOrderItems!}"/>
                      <input type="hidden" name="facilityId" value="${facilityId!}"/>
                    </form>  
              </#assign>  
              <#if ((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
                <@td>
                  ${viewGroupDetailForm}
                  <#if groupName1?has_content>
                    <a href="javascript:document.viewGroupDetail_${pickMoveInfo_index}.submit()" class="${styles.link_nav_info_name!}">${groupName1}</a>
                  </#if>
                </@td>
                <@td>
                  <#if groupName2?has_content>
                    <a href="javascript:document.viewGroupDetail_${pickMoveInfo_index}.submit()" class="${styles.link_nav_info_name!}">${groupName2}</a>
                  </#if>
                </@td>
                <@td>
                  <#if groupName3?has_content>
                    <a href="javascript:document.viewGroupDetail_${pickMoveInfo_index}.submit()" class="${styles.link_nav_info_name!}">${groupName3}</a>
                  </#if>
                </@td>
              <#else>
                <@td>
                  ${viewGroupDetailForm}
                  <a href="javascript:document.viewGroupDetail_${pickMoveInfo_index}.submit()" class="${styles.link_nav_info_name!}">${groupName!}</a>
                </@td>
              </#if>
            <@td>
              <#if !((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
                <#if orderReadyToPickInfoListSize == 0 >${uiLabelMap.CommonN}<#else>${uiLabelMap.CommonY}</#if>
              <#else>
                ${orderReadyToPickInfoListSize}
              </#if>
            </@td>
            <@td>
              <#if !((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
                <#if orderNeedsStockMoveInfoListSize == 0>${uiLabelMap.CommonN}<#else>${uiLabelMap.CommonY}</#if>
              <#else>
                ${orderNeedsStockMoveInfoListSize}
              </#if>
            </@td>
            <@td>
              <#if orderReadyToPickInfoList?has_content>
                <form method="post" action="<@ofbizUrl>createPicklistFromOrders</@ofbizUrl>">
                  <input type="hidden" name="facilityId" value="${facilityId!}"/>
                  <input type="hidden" name="groupByShippingMethod" value="${requestParameters.groupByShippingMethod!}"/>
                  <input type="hidden" name="groupByWarehouseArea" value="${requestParameters.groupByWarehouseArea!}"/>
                  <input type="hidden" name="groupByNoOfOrderItems" value="${requestParameters.groupByNoOfOrderItems!}"/>
                  <input type="hidden" name="orderIdList" value=""/>
                  <#assign orderIdsForPickList = orderReadyToPickInfoList!>
                  <#list orderIdsForPickList as orderIdForPickList>
                    <input type="hidden" name="orderIdList" value="${orderIdForPickList.orderHeader.orderId}"/>
                  </#list>
                  <#if ((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
                    <span>${uiLabelMap.ProductPickFirst}</span>
                    <input type="text" size="4" name="maxNumberOfOrders" value="20"/>
                  </#if>
                  <input type="submit" value="${uiLabelMap.ProductCreatePicklist}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
                </form>
              <#else>
                &nbsp;
              </#if>
            </@td>
            <@td>
              <#if orderReadyToPickInfoList?has_content>
                <form method="post" action="<@ofbizUrl>printPickSheets</@ofbizUrl>" target="_blank">
                  <input type="hidden" name="printGroupName" value="${groupName!}"/>
                  <input type="hidden" name="facilityId" value="${facilityId!}"/>
                  <input type="hidden" name="groupByShippingMethod" value="${requestParameters.groupByShippingMethod!}"/>
                  <input type="hidden" name="groupByWarehouseArea" value="${requestParameters.groupByWarehouseArea!}"/>
                  <input type="hidden" name="groupByNoOfOrderItems" value="${requestParameters.groupByNoOfOrderItems!}"/>
                  <#if !((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
                    <input type="hidden" name="maxNumberOfOrdersToPrint" value="1"/>
                    <input type="hidden" name="orderId" value="${groupName!}"/>
                  <#else>
                    <span>${uiLabelMap.FormFieldTitle_printPickSheetFirst}</span>
                    <input type="text" size="4" name="maxNumberOfOrdersToPrint" value="20"/>
                  </#if>
                  <input type="submit" value="${uiLabelMap.FormFieldTitle_printPickSheet}" class="${styles.link_run_sys!} ${styles.action_export!}"/>
                </form>
              <#else>
                &nbsp;
              </#if>
            </@td>
          </@tr>
          <#-- toggle the row color -->
          <#assign alt_row = !alt_row>
        </#list>
        <#if ((requestParameters.groupByShippingMethod?? && requestParameters.groupByShippingMethod == "Y") || (requestParameters.groupByWarehouseArea?? && requestParameters.groupByWarehouseArea == "Y") || (requestParameters.groupByNoOfOrderItems?? && requestParameters.groupByNoOfOrderItems == "Y"))>
          <@tr alt=alt_row>
            <@th>${uiLabelMap.CommonAllMethods}</@th>
            <@td>&nbsp;</@td>
            <@td>&nbsp;</@td>
            <@th>${orderReadyToPickInfoListSizeTotal}</@th>
            <@th>${orderNeedsStockMoveInfoListSizeTotal}</@th>
            <@td>
              <#if (orderReadyToPickInfoListSizeTotal > 0)>
                <form method="post" action="<@ofbizUrl>createPicklistFromOrders</@ofbizUrl>">
                  <input type="hidden" name="facilityId" value="${facilityId!}"/>
                  <span>${uiLabelMap.ProductPickFirst}</span>
                  <input type="text" size="4" name="maxNumberOfOrders" value="20"/>
                  <input type="submit" value="${uiLabelMap.ProductCreatePicklist}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
                </form>
              <#else>
                &nbsp;
              </#if>
            </@td>
            <@td>
              <#if (orderReadyToPickInfoListSizeTotal > 0)>
                <form method="post" action="<@ofbizUrl>printPickSheets</@ofbizUrl>" target="_blank">
                  <input type="hidden" name="facilityId" value="${facilityId!}"/>
                  <span>${uiLabelMap.FormFieldTitle_printPickSheetFirst}</span>
                  <input type="text" size="4" name="maxNumberOfOrdersToPrint" value="20"/>
                  <input type="submit" value="${uiLabelMap.FormFieldTitle_printPickSheet}" class="${styles.link_run_sys!} ${styles.action_export!}"/>
                </form>
              <#else>
                &nbsp;
              </#if>
            </@td>
          </@tr>
        </#if>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.ProductNoOrdersFoundReadyToPickOrNeedStockMoves}.</@commonMsg>
  </#if>
</@section>

<#assign viewDetail = requestParameters.viewDetail!>
<#if viewDetail?has_content>
  <#list pickMoveInfoList as pickMoveInfo>
    <#assign groupName = pickMoveInfo.groupName!>
    <#if (groupName!) == viewDetail>
      <#assign toPickList = pickMoveInfo.orderReadyToPickInfoList!>
    </#if>
  </#list>
</#if>

<#if toPickList?has_content>
  <@section title=uiLabelMap.ProductPickingDetail>
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.ProductOrderId}</@th>
          <@th>${uiLabelMap.FormFieldTitle_orderDate}</@th>
          <@th>${uiLabelMap.ProductChannel}</@th>
          <@th>${uiLabelMap.ProductOrderItem}</@th>
          <@th>${uiLabelMap.ProductProductDescription}</@th>
          <@th>${uiLabelMap.ProductOrderShipGroupId}</@th>
          <@th>${uiLabelMap.ProductQuantity}</@th>
          <@th>${uiLabelMap.ProductQuantityNotAvailable}</@th>
        </@tr>
       </@thead>
        <#list toPickList as toPick>
          <#assign oiasgal = toPick.orderItemShipGrpInvResList>
          <#assign header = toPick.orderHeader>
          <#assign channel = header.getRelatedOne("SalesChannelEnumeration", false)!>
          <#list oiasgal as oiasga>
            <#assign orderProduct = oiasga.getRelatedOne("OrderItem", false).getRelatedOne("Product", false)!>
            <#assign product = oiasga.getRelatedOne("InventoryItem", false).getRelatedOne("Product", false)!>
            <@tr valign="middle">
              <@td><a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${oiasga.orderId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" target="_blank">${oiasga.orderId}</a></@td>
              <@td>${header.orderDate?string}</@td>
              <@td>${(channel.description)!}</@td>
              <@td>${oiasga.orderItemSeqId}</@td>
              <@td>
                <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${orderProduct.productId!}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_name!}" target="_blank">${(orderProduct.internalName)!}</a>
                <#if orderProduct.productId != product.productId>
                  &nbsp;[<a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${product.productId!}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_name!}" target="_blank">${(product.internalName)!}</a>]
                </#if>
              </@td>
              <@td>${oiasga.shipGroupSeqId}</@td>
              <@td>${oiasga.quantity}</@td>
              <@td>${oiasga.quantityNotAvailable!}</@td>
            </@tr>
          </#list>
        </#list>
      </@table>
  </@section>
</#if>
