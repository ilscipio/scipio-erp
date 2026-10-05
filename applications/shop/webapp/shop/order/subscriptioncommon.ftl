<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#--
SCIPIO: Local subscriptions template common defs
-->

<#macro subscriptionLinkContent productSubscriptionResource subscriptionResource orderItem index>
  <#if !subscriptionResource?has_content>
      <#local subscriptionResource = productSubscriptionResource.getRelatedOne("SubscriptionResource", true)>   
  </#if> 
  <#assign subscriptionModalLabel>
    <#if orderItem.subscriptionId?has_content>[${orderItem.subscriptionId!}] - </#if> ${raw(subscriptionResource.description)}
  </#assign>   
  <@modal id="row_orderitem_subscription_${index}_${orderItem.orderItemSeqId}" label="${subscriptionModalLabel}">
    <@section title="${raw(subscriptionResource.description)}: ${raw(orderItem.itemDescription!)}">
        <@table>
            <@tr>
                <@td>Max Time</@td>
                <#local maxLifeTimeUom = productSubscriptionResource.getRelatedOne("MaxLifeTimeUom", true)!>
                <@td>${productSubscriptionResource.maxLifeTime!} <#if maxLifeTimeUom?has_content>${maxLifeTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Available Time</@td>
                <#local availableTimeUom = productSubscriptionResource.getRelatedOne("AvailableTimeUom", true)!>
                <@td>${productSubscriptionResource.availableTime!} <#if availableTimeUom?has_content>${availableTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Use Count Limit</@td>                
                <@td>${productSubscriptionResource.useCountLimit!}</@td>
            </@tr>
            <@tr>
                <@td>Use Time</@td>
                <#local useTimeUom = productSubscriptionResource.getRelatedOne("UseTimeUom", true)!>
                <@td>${productSubscriptionResource.useTime!} <#if useTimeUom?has_content>${useTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Automatic Extend</@td>
                <@td>${productSubscriptionResource.automaticExtend!}</@td>
            </@tr>
            <@tr>
                <@td>Cancel Automatic Extended Time</@td>
                <#local cancelTimeUom = productSubscriptionResource.getRelatedOne("CancelTimeUom", true)!>
                <@td>${productSubscriptionResource.canclAutmExtTime!} <#if cancelTimeUom?has_content>${cancelTimeUom.description!}</#if></@td>
            </@tr>
            <@tr>
                <@td>Period On Expiry</@td>
                <#local gracePeriodUom = productSubscriptionResource.getRelatedOne("GracePeriodUom", true)!>
                <@td>${productSubscriptionResource.gracePeriodOnExpiry!} <#if gracePeriodUom?has_content>${gracePeriodUom.description!}</#if></@td>
            </@tr>
        </@table>               
    </@section>
  </@modal>
</#macro>