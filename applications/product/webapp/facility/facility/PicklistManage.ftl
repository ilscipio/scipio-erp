<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.ProductPicklistManage>
  
  <#if picklistInfoList?has_content>
    <@paginate mode="content" url=makeOfbizUrl("PicklistManage") paramStr="facilityId=${facilityId!}" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=picklistCount!0 altParam=true forcePost=true>

      <#list picklistInfoList as picklistInfo>
        <#assign picklist = picklistInfo.picklist>

        <#-- Picklist -->
        <div>
          <span>${uiLabelMap.ProductPickList}</span> ${picklist.picklistId}
          <span>${uiLabelMap.CommonDate}</span> ${picklist.picklistDate}
          <form method="post" action="<@ofbizUrl>updatePicklist</@ofbizUrl>" style="display: inline;">
            <input type="hidden" name="facilityId" value="${facilityId}"/>
            <input type="hidden" name="picklistId" value="${picklist.picklistId}"/>
            <select name="statusId">
              <option value="${picklistInfo.statusItem.statusId}" selected>${picklistInfo.statusItem.get("description",locale)}</option>
              <option value="${picklistInfo.statusItem.statusId}">---</option>
              <#list picklistInfo.statusValidChangeToDetailList as statusValidChangeToDetail>
                <option value="${statusValidChangeToDetail.get("statusIdTo", locale)}">${statusValidChangeToDetail.get("description", locale)} (${statusValidChangeToDetail.get("transitionName", locale)})</option>
              </#list>
            </select>
            <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
          </form>
          <span>${uiLabelMap.ProductCreatedModifiedBy}</span> ${picklist.createdByUserLogin}/${picklist.lastModifiedByUserLogin}
          <a href="<@ofbizUrl>PicklistReport.pdf?picklistId=${picklist.picklistId}</@ofbizUrl>" target="_blank" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.ProductPick}/${uiLabelMap.ProductPacking} ${uiLabelMap.CommonReports}</a>
          <hr />
        </div>
        <#if picklistInfo.shipmentMethodType?has_content>
          <div style="margin-left: 15px;">
            <span>${uiLabelMap.CommonFor} ${uiLabelMap.ProductShipmentMethodType}</span> ${picklistInfo.shipmentMethodType.description!picklistInfo.shipmentMethodType.shipmentMethodTypeId}
          </div>
        </#if>

        <#-- PicklistRole -->
        <#list picklistInfo.picklistRoleInfoList! as picklistRoleInfo>
          <div style="margin-left: 15px;">
            <span>${uiLabelMap.PartyParty}</span> ${picklistRoleInfo.partyNameView.firstName!} ${picklistRoleInfo.partyNameView.middleName!} ${picklistRoleInfo.partyNameView.lastName!} ${picklistRoleInfo.partyNameView.groupName!}
            <span>${uiLabelMap.PartyRole}</span> ${picklistRoleInfo.roleType.description}
            <span>${uiLabelMap.CommonFrom}</span> ${picklistRoleInfo.picklistRole.fromDate}
            <#if picklistRoleInfo.picklistRole.thruDate??><span>${uiLabelMap.CommonThru}</span> ${picklistRoleInfo.picklistRole.thruDate}</#if>
          </div>
        </#list>
        <div style="margin-left: 15px;">
          <span>${uiLabelMap.ProductAssignPicker}</span>
          <form method="post" action="<@ofbizUrl>createPicklistRole</@ofbizUrl>" style="display: inline;">
            <input type="hidden" name="facilityId" value="${facilityId}"/>
            <input type="hidden" name="picklistId" value="${picklist.picklistId}"/>
            <input type="hidden" name="roleTypeId" value="PICKER"/>
            <select name="partyId">
              <#list partyRoleAndPartyDetailList as partyRoleAndPartyDetail>
                <option value="${partyRoleAndPartyDetail.partyId}">${partyRoleAndPartyDetail.firstName!} ${partyRoleAndPartyDetail.middleName!} ${partyRoleAndPartyDetail.lastName!} ${partyRoleAndPartyDetail.groupName!} [${partyRoleAndPartyDetail.partyId}]</option>
              </#list>
            </select>
            <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
          </form>
        </div>

        <#-- PicklistStatusHistory -->
        <#list picklistInfo.picklistStatusHistoryInfoList! as picklistStatusHistoryInfo>
          <div style="margin-left: 15px;">
            <span>${uiLabelMap.CommonStatus}</span> ${uiLabelMap.CommonChange} ${uiLabelMap.CommonFrom} ${picklistStatusHistoryInfo.statusItem.get("description",locale)}
            ${uiLabelMap.CommonTo} ${picklistStatusHistoryInfo.statusItemTo.description}
            ${uiLabelMap.CommonOn} ${picklistStatusHistoryInfo.picklistStatusHistory.changeDate}
            ${uiLabelMap.CommonBy} ${picklistStatusHistoryInfo.picklistStatusHistory.changeUserLoginId}
          </div>
        </#list>
        <hr />
        <#-- PicklistBin -->
        <#list picklistInfo.picklistBinInfoList! as picklistBinInfo>
          <#assign isBinComplete = Static["org.ofbiz.shipment.picklist.PickListServices"].isBinComplete(delegator, picklistBinInfo.picklistBin.picklistBinId)/>
          <#if (!isBinComplete)>
            <div style="margin-left: 15px;">
              <span>${uiLabelMap.ProductBinNum}</span> ${picklistBinInfo.picklistBin.binLocationNumber}&nbsp;(${picklistBinInfo.picklistBin.picklistBinId})
              <#if picklistBinInfo.primaryOrderHeader??><span>${uiLabelMap.ProductPrimaryOrderId}</span> ${picklistBinInfo.primaryOrderHeader.orderId}</#if>
              <#if picklistBinInfo.primaryOrderItemShipGroup??><span>${uiLabelMap.ProductPrimaryShipGroupSeqId}</span> ${picklistBinInfo.primaryOrderItemShipGroup.shipGroupSeqId}</#if>
              <#if !picklistBinInfo.picklistItemInfoList?has_content><a href="javascript:document.DeletePicklistBin_${picklistInfo_index}_${picklistBinInfo_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></#if>
              <form name="DeletePicklistBin_${picklistInfo_index}_${picklistBinInfo_index}" method="post" action="<@ofbizUrl>deletePicklistBin</@ofbizUrl>">
                <input type="hidden" name="picklistBinId" value="${picklistBinInfo.picklistBin.picklistBinId}"/>
                <input type="hidden" name="facilityId" value="${facilityId!}"/>
              </form>
            </div>
            <div style="margin-left: 30px;">
              <span>${uiLabelMap.CommonUpdate} ${uiLabelMap.ProductBinNum}</span>
              <form method="post" action="<@ofbizUrl>updatePicklistBin</@ofbizUrl>" style="display: inline;">
                <input type="hidden" name="facilityId" value="${facilityId}"/>
                <input type="hidden" name="picklistBinId" value="${picklistBinInfo.picklistBin.picklistBinId}"/>
                <span>${uiLabelMap.ProductLocation} ${uiLabelMap.CommonNbr}</span>
                <input type"text" size="2" name="binLocationNumber" value="${picklistBinInfo.picklistBin.binLocationNumber}"/>
                <span>${uiLabelMap.PageTitlePickList}</span>
                <select name="picklistId">
                  <#list picklistActiveList as picklistActive>
                    <#assign picklistActiveStatusItem = picklistActive.getRelatedOne("StatusItem", true)>
                    <option value="${picklistActive.picklistId}"<#if picklistActive.picklistId == picklist.picklistId> selected="selected"</#if>>${picklistActive.picklistId} [${uiLabelMap.CommonDate}:${picklistActive.picklistDate},${uiLabelMap.CommonStatus}:${picklistActiveStatusItem.get("description",locale)}]</option>
                  </#list>
                </select>
                <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
              </form>
            </div>
            <#if picklistBinInfo.picklistItemInfoList?has_content>
              <div style="margin-left: 30px;">
                <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                 <@thead>
                  <@tr class="header-row">
                    <@th>${uiLabelMap.ProductOrderId}</@th>
                    <@th>${uiLabelMap.ProductOrderShipGroupId}</@th>
                    <@th>${uiLabelMap.ProductOrderItem}</@th>
                    <@th>${uiLabelMap.ProductProduct}</@th>
                    <@th>${uiLabelMap.ProductInventoryItem}</@th>
                    <@th>${uiLabelMap.ProductLocation}</@th>
                    <@th>${uiLabelMap.ProductQuantity}</@th>
                    <@th>&nbsp;</@th>
                  </@tr>
                  </@thead>
                  <#assign alt_row = false>
                  <#list picklistBinInfo.picklistItemInfoList! as picklistItemInfo>
                    <#assign picklistItem = picklistItemInfo.picklistItem>
                    <#assign inventoryItemAndLocation = picklistItemInfo.inventoryItemAndLocation>
                    <@tr valign="middle">
                      <@td>${picklistItem.orderId}</@td>
                      <@td>${picklistItem.shipGroupSeqId}</@td>
                      <@td>${picklistItem.orderItemSeqId}</@td>
                      <@td>${picklistItemInfo.orderItem.productId}<#if picklistItemInfo.orderItem.productId != inventoryItemAndLocation.productId>&nbsp;[${inventoryItemAndLocation.productId}]</#if></@td>
                      <@td>${inventoryItemAndLocation.inventoryItemId}</@td>
                      <@td>${inventoryItemAndLocation.areaId!}-${inventoryItemAndLocation.aisleId!}-${inventoryItemAndLocation.sectionId!}-${inventoryItemAndLocation.levelId!}-${inventoryItemAndLocation.positionId!}</@td>
                      <@td>${picklistItem.quantity}</@td>
                      <#if !picklistItemInfo.itemIssuanceList?has_content>
                        <@td>
                          <form name="deletePicklistItem_${picklist.picklistId}_${picklistItem.orderId}_${picklistItemInfo_index}" method="post" action="<@ofbizUrl>deletePicklistItem</@ofbizUrl>">
                            <input type="hidden" name="picklistBinId" value="${picklistItemInfo.picklistItem.picklistBinId}"/>
                            <input type="hidden" name="orderId" value="${picklistItemInfo.picklistItem.orderId}"/>
                            <input type="hidden" name="orderItemSeqId" value="${picklistItemInfo.picklistItem.orderItemSeqId}"/>
                            <input type="hidden" name="shipGroupSeqId" value="${picklistItemInfo.picklistItem.shipGroupSeqId}"/>
                            <input type="hidden" name="inventoryItemId" value="${picklistItemInfo.picklistItem.inventoryItemId}"/>
                            <input type="hidden" name="facilityId" value="${facilityId!}"/>
                            <a href="javascript:document.deletePicklistItem_${picklist.picklistId}_${picklistItem.orderId}_${picklistItemInfo_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                          </form>
                        </@td>
                      </#if>
                      <@td>
                        <#-- picklistItem.orderItemShipGrpInvRes (do we want to display any of this info?) -->
                        <#-- picklistItemInfo.itemIssuanceList -->
                        <#list picklistItemInfo.itemIssuanceList! as itemIssuance>
                          <b>${uiLabelMap.ProductIssue} ${uiLabelMap.CommonTo} ${uiLabelMap.ProductShipmentItemSeqId}:</b> ${itemIssuance.shipmentId}:${itemIssuance.shipmentItemSeqId}
                          <b>${uiLabelMap.ProductQuantity}:</b> ${itemIssuance.quantity}
                          <b>${uiLabelMap.CommonDate}: </b> ${itemIssuance.issuedDateTime}
                        </#list>
                      </@td>
                    </@tr>
                  </#list>
                </@table>
              </div>
              <#if picklistBinInfo.productStore.managedByLot?? && picklistBinInfo.productStore.managedByLot == "Y">
                <div style="margin-left: 30px;">
                  <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                    <@thead>
                    <@tr class="header-row">                   
                          <@th>${uiLabelMap.ProductOrderId}</@th>
                          <@th>${uiLabelMap.ProductOrderShipGroupId}</@th>
                          <@th>${uiLabelMap.ProductOrderItem}</@th>
                          <@th>${uiLabelMap.ProductProduct}</@th>
                          <@th>${uiLabelMap.ProductInventoryItem}</@th>
                          <@th>${uiLabelMap.ProductLotId}</@th>
                          <@th>${uiLabelMap.ProductQuantity}</@th>
                          <@th>&nbsp;</@th>
                      </@tr>
                      </@thead>
                      <#list picklistBinInfo.picklistItemInfoList! as picklistItemInfo>
                        <#assign picklistItem = picklistItemInfo.picklistItem>
                        <#assign inventoryItemAndLocation = picklistItemInfo.inventoryItemAndLocation>
                        <#if !picklistItemInfo.product.lotIdFilledIn?has_content || picklistItemInfo.product.lotIdFilledIn != "Forbidden">
                          <form name="editPicklistItem_${picklist.picklistId}_${picklistItem.orderId}_${picklistItemInfo_index}" method="post" action="<@ofbizUrl>editPicklistItem</@ofbizUrl>">
                            <@tr valign="middle">
                              <@td>${picklistItem.orderId}</@td>
                              <@td>${picklistItem.shipGroupSeqId}</@td>
                              <@td>${picklistItem.orderItemSeqId}</@td>
                              <@td>${picklistItemInfo.orderItem.productId}<#if picklistItemInfo.orderItem.productId != inventoryItemAndLocation.productId>&nbsp;[${inventoryItemAndLocation.productId}]</#if></@td>
                              <@td>${inventoryItemAndLocation.inventoryItemId}</@td>
                              <@td><input type="text" name="lotId" <#if inventoryItemAndLocation.lotId?has_content>value="${inventoryItemAndLocation.lotId}"</#if> /></@td>
                              <@td><input type="text" name="quantity" value="${picklistItem.quantity}" /></@td>
                              <@td>
                                <input type="hidden" name="picklistBinId" value="${picklistItemInfo.picklistItem.picklistBinId}"/>
                                <input type="hidden" name="orderId" value="${picklistItemInfo.picklistItem.orderId}"/>
                                <input type="hidden" name="orderItemSeqId" value="${picklistItemInfo.picklistItem.orderItemSeqId}"/>
                                <input type="hidden" name="shipGroupSeqId" value="${picklistItemInfo.picklistItem.shipGroupSeqId}"/>
                                <input type="hidden" name="inventoryItemId" value="${picklistItemInfo.picklistItem.inventoryItemId}"/>
                                <input type="hidden" name="facilityId" value="${facilityId!}"/>
                                <input type="hidden" name="productId" value="${picklistItemInfo.orderItem.productId}" />
                                <#if inventoryItemAndLocation.lotId?has_content>
                                  <input type="hidden" name="oldLotId" value="${inventoryItemAndLocation.lotId}" />
                                </#if>
                                <a href="javascript:document.editPicklistItem_${picklist.picklistId}_${picklistItem.orderId}_${picklistItemInfo_index}.submit()" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>
                              </@td>
                            </@tr>
                          </form>
                        </#if>
                      </#list>
                  </@table>
                </div>
              </#if>
            </#if>
          </#if>
        </#list>
        <#if picklistInfo_has_next>
          <hr />
        </#if>
      </#list>
    </@paginate>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.ProductNoPicksStarted}.</@commonMsg>
  </#if>
</@section>
