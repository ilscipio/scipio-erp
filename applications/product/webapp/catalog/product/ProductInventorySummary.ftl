<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if parameters.showAllFacilities??>
    <@menuitem type="link" href="EditProductInventoryItems?productId=${productId}" text=uiLabelMap.ProductShowProductFacilities class="+${styles.action_run_sys!} ${styles.action_find!}" />
  <#else>
    <@menuitem type="link" href="EditProductInventoryItems?productId=${productId}&amp;showAllFacilities=Y" text=uiLabelMap.ProductShowAllFacilities class="+${styles.action_run_sys!} ${styles.action_find!}" />
  </#if>
  </@menu>
</#macro>
<@section title=uiLabelMap.ProductInventorySummary menuContent=menuContent>
  <#if product??>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductFacility}</@th>
                <@th>${uiLabelMap.ProductAtp}</@th>
                <@th>${uiLabelMap.ProductQoh}</@th>
                <#if isMarketingPackage == "true">
                <@th>${uiLabelMap.ProductMarketingPackageATP}</@th>
                <@th>${uiLabelMap.ProductMarketingPackageQOH}</@th>
                </#if>
                <@th>${uiLabelMap.ProductIncomingShipments}</@th>
                <@th>${uiLabelMap.ProductIncomingProductionRuns}</@th>
                <@th>${uiLabelMap.ProductOutgoingProductionRuns}</@th>
            </@tr>
            </@thead>
            <#list quantitySummaryByFacility.values() as quantitySummary>
                <#if quantitySummary.facilityId??>
                    <#assign facilityId = quantitySummary.facilityId>
                    <#assign facility = delegator.findOne("Facility", {"facilityId":facilityId}, false)>
                    <#assign manufacturingInQuantitySummary = manufacturingInQuantitySummaryByFacility.get(facilityId)!>
                    <#assign manufacturingOutQuantitySummary = manufacturingOutQuantitySummaryByFacility.get(facilityId)!>
                    <#assign totalQuantityOnHand = quantitySummary.totalQuantityOnHand!>
                    <#assign totalAvailableToPromise = quantitySummary.totalAvailableToPromise!>
                    <#assign mktgPkgATP = quantitySummary.mktgPkgATP!>
                    <#assign mktgPkgQOH = quantitySummary.mktgPkgQOH!>
                    <#assign incomingShipmentAndItemList = quantitySummary.incomingShipmentAndItemList!>
                    <#assign incomingProductionRunList = manufacturingInQuantitySummary.incomingProductionRunList!>
                    <#assign incomingQuantityTotal = manufacturingInQuantitySummary.estimatedQuantityTotal!>
                    <#assign outgoingProductionRunList = manufacturingOutQuantitySummary.outgoingProductionRunList!>
                    <#assign outgoingQuantityTotal = manufacturingOutQuantitySummary.estimatedQuantityTotal!>
                    <@tr valign="middle">
                        <@td>${(facility.facilityName)!} [${facilityId?default("[No Facility]")}]
                        <a href="<@ofbizInterWebappUrl>/facility/control/ReceiveInventory?facilityId=${facilityId}&amp;productId=${productId}&amp;externLoginKey=${externalLoginKey}</@ofbizInterWebappUrl>" class="${styles.link_nav!} ${styles.action_receive!}">${uiLabelMap.ProductInventoryReceive}</a></@td>
                        <@td><#if totalAvailableToPromise??>${totalAvailableToPromise}<#else>&nbsp;</#if></@td>
                        <@td><#if totalQuantityOnHand??>${totalQuantityOnHand}<#else>&nbsp;</#if></@td>
                        <#if isMarketingPackage == "true">
                        <@td><#if mktgPkgATP??>${mktgPkgATP}<#else>&nbsp;</#if></@td>
                        <@td><#if mktgPkgQOH??>${mktgPkgQOH}<#else>&nbsp;</#if></@td>
                        </#if>
                        <@td>
                            <#if incomingShipmentAndItemList?has_content>
                                <#list incomingShipmentAndItemList as incomingShipmentAndItem>
                                    <div>${incomingShipmentAndItem.shipmentId}:${incomingShipmentAndItem.shipmentItemSeqId}-${(incomingShipmentAndItem.estimatedArrivalDate.toString())!}-<#if incomingShipmentAndItem.quantity??>${incomingShipmentAndItem.quantity?string.number}<#else>[${uiLabelMap.ProductQuantityNotSet}]</#if></div>
                                </#list>
                            <#else>
                                <div>&nbsp;</div>
                            </#if>
                        </@td>
                        <@td>
                            <#if incomingProductionRunList?has_content>
                                <#list incomingProductionRunList as incomingProductionRun>
                                    <div>${incomingProductionRun.workEffortId}-${(incomingProductionRun.estimatedCompletionDate.toString())!}-<#if incomingProductionRun.estimatedQuantity??>${incomingProductionRun.estimatedQuantity?string.number}<#else>[${uiLabelMap.ProductQuantityNotSet}]</#if></div>
                                </#list>
                                <div><b>${uiLabelMap.CommonTotal}:&nbsp;${incomingQuantityTotal!}</b></div>
                            <#else>
                                <div>&nbsp;</div>
                            </#if>
                        </@td>
                        <@td>
                            <#if outgoingProductionRunList?has_content>
                                <#list outgoingProductionRunList as outgoingProductionRun>
                                    <div>${outgoingProductionRun.workEffortParentId!""}:${outgoingProductionRun.workEffortId}-${(outgoingProductionRun.estimatedStartDate.toString())!}-<#if outgoingProductionRun.estimatedQuantity??>${outgoingProductionRun.estimatedQuantity?string.number}<#else>[${uiLabelMap.ProductQuantityNotSet}]</#if></div>
                                </#list>
                                <div><b>${uiLabelMap.CommonTotal}:&nbsp;${outgoingQuantityTotal!}</b></div>
                            <#else>
                                <div>&nbsp;</div>
                            </#if>
                        </@td>
                    </@tr>

                </#if>
            </#list>
        </@table>
    </div>
  <#else>
    <@commonMsg type="error">${uiLabelMap.ProductProductNotFound} ${productId!}!</@commonMsg>
  </#if>
</@section>
