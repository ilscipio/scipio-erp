<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://workeffort/webapp/workeffort/common/common.ftl">

<#-- SCIPIO: modified to show workEffortIds only if workEffortName missing -->
<#assign workEffortLinkLabel = workEffort.workEffortName!"">
<#if !workEffortLinkLabel?has_content>
  <#assign workEffortLinkLabel = workEffort.workEffortId!"">
</#if>
<#assign workEffortStatusLabel = "">
<#if workEffort.currentStatusId?has_content>
  <#assign workEffortStatusLabel = (workEffort.getRelatedOne("CurrentStatusItem", false).get("description", locale))!"">
</#if>

<#assign calEventVerbose = calEventVerbose!true>
    
<#if workEffort.workEffortTypeId == "PROD_ORDER_HEADER">
  <a href="<@ofbizInterWebappUrl>/manufacturing/control/ShowProductionRun?productionRunId=${workEffort.workEffortId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!} event">
    ${workEffortLinkLabel}
    <#--${workEffort.workEffortId}-->
  </a>
  <#--&nbsp;${workEffort.workEffortName!"Undefined"}-->
<#if calEventVerbose>
  <#if workEffortStatusLabel?has_content> <span class="cal-entry-status">[${workEffortStatusLabel}]</span></#if>
  <#if workOrderItemFulfillments?has_content>
    <#list workOrderItemFulfillments as workOrderItemFulfillment>
      <br/>${uiLabelMap.OrderOrderId}: <a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${workOrderItemFulfillment.orderId}</@ofbizInterWebappUrl>" class="event">${workOrderItemFulfillment.orderId} / ${workOrderItemFulfillment.orderItemSeqId}</a>
      <#assign orderItemAndShipGroupAssocs = delegator.findByAnd("OrderHeaderItemAndShipGroup", {"orderId", workOrderItemFulfillment.orderId, "orderItemSeqId", workOrderItemFulfillment.orderItemSeqId}, null, false)!/>
      <#list orderItemAndShipGroupAssocs as orderItemAndShipGroupAssoc>
        <#if orderItemAndShipGroupAssoc.shipByDate?has_content>
          ${uiLabelMap.OrderShipBeforeDate}: ${orderItemAndShipGroupAssoc.shipByDate}
        </#if>
      </#list>
    </#list>
  </#if>
</#if>
<#elseif workEffort.workEffortTypeId == "PROD_ORDER_TASK">
  <a href="<@ofbizInterWebappUrl>/manufacturing/control/ShowProductionRun?productionRunId=${workEffort.workEffortParentId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_desc!} event">
    ${workEffortLinkLabel}
    <#--${workEffort.workEffortParentId} / ${workEffort.workEffortId}-->
  </a>
  <#--&nbsp;${workEffort.workEffortName!"Undefined"}-->
<#if calEventVerbose>
  <#if workEffortStatusLabel?has_content> <span class="cal-entry-status">[${workEffortStatusLabel}]</span></#if>
  <#if workEffort.reservPersons??>&nbsp;Persons: ${workEffort.reservPersons}</#if>
  <#if parentWorkOrderItemFulfillments?has_content>
    <#list parentWorkOrderItemFulfillments as parentWorkOrderItemFulfillment>
      <br/>${uiLabelMap.OrderOrderId}: <a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${parentWorkOrderItemFulfillment.orderId}</@ofbizInterWebappUrl>" class="event">${parentWorkOrderItemFulfillment.orderId} / ${parentWorkOrderItemFulfillment.orderItemSeqId}</a>
      <#assign orderItemAndShipGroupAssocs = delegator.findByAnd("OrderHeaderItemAndShipGroup", {"orderId", parentWorkOrderItemFulfillment.orderId, "orderItemSeqId", parentWorkOrderItemFulfillment.orderItemSeqId}, null, false)!/>
      <#list orderItemAndShipGroupAssocs as orderItemAndShipGroupAssoc>
        <#if orderItemAndShipGroupAssoc.shipByDate?has_content>
          ${uiLabelMap.OrderShipBeforeDate}: ${orderItemAndShipGroupAssoc.shipByDate}
        </#if>
      </#list>
    </#list>
  </#if>
</#if>
<#else>
  <#-- Allow containing screens to specify the URL for editing an event -->
  <#if !editCalEventUrl??>
    <#assign editCalEventUrl = parameters._LAST_VIEW_NAME_>
  </#if>
  <a href="<@ofbizUrl>${editCalEventUrl}?form=edit&amp;parentTypeId=${parentTypeId!}&amp;period=${periodType!}&amp;start=${parameters.start!}&amp;workEffortId=${workEffort.workEffortId}${addlParam!}${urlParam!}</@ofbizUrl>" class="event">
    ${workEffortLinkLabel}
    <#--${workEffort.workEffortId}-->
  </a>
  <#--&nbsp;${workEffort.workEffortName!""}-->
<#if calEventVerbose>
  <#if workEffortStatusLabel?has_content> <span class="cal-entry-status">[${workEffortStatusLabel}]</span></#if>
</#if>
</#if>
