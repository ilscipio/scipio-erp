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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#assign delegator = requestAttributes.delegator>
<#if communicationEvent.partyIdFrom??>
    <#assign fromName = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, communicationEvent.partyIdFrom, true)>
</#if>
<#if communicationEvent.partyIdTo??>
    <#assign toName = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, communicationEvent.partyIdTo, true)>
</#if>

<#-- TODO: this was turned into menu below, may need something more to achieve look... extra menu class/type...
        this code was BEFORE or LEFT of title, not after
        <div class="boxlink">
            <#if ((communicationEvent.partyIdFrom!) != (userLogin.partyId)!)>
              <a href="<@pageUrl>newmessage?communicationEventId=${communicationEvent.communicationEventId}</@pageUrl>" class="submenutext">${uiLabelMap.PartyReply}</a>
            </#if>
            <a href="<@pageUrl>messagelist</@pageUrl>" class="submenutextright">${uiLabelMap.EcommerceViewList}</a>
        </div>
-->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if ((communicationEvent.partyIdFrom!) != (userLogin.partyId)!)>
          <@menuitem type="link" href=makePageUrl("newmessage?communicationEventId=${communicationEvent.communicationEventId}") text=uiLabelMap.PartyReply />
        </#if>
        <@menuitem type="link" href=makePageUrl("messagelist") text=uiLabelMap.EcommerceViewList />
    </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceReadMessage menuContent=menuContent menuLayoutTitle="inline-title">
    <@table type="fields">
      <@tr><@td>&nbsp;</@td></@tr>
      <@tr>
          <@td>${uiLabelMap.CommonFrom}</@td>
          <@td>${fromName!}</@td>
      </@tr>
      <@tr>
          <@td>${uiLabelMap.CommonTo}</@td>
          <@td>${toName!}</@td>
      </@tr>
      <@tr>
          <@td>${uiLabelMap.CommonDate}</@td>
          <@td>${communicationEvent.entryDate}</@td>
      </@tr>
      <@tr>
          <@td>${uiLabelMap.EcommerceSubject}</@td>
          <@td>&nbsp;${(communicationEvent.subject)!("[${uiLabelMap.EcommerceNoSubject}]")}</@td>
      </@tr>
      <@tr><@td>&nbsp;</@td></@tr>
      <@tr>
        <@td>&nbsp;</@td>
        
        <@td>
            <#-- SCIPIO: NOTE: 2016-10-20: this content markup is subject to serious security concerns.
                Strict filter is used, and whether any markup is allowed at all is dependent on
                and centralized in the escapeVal call. -->
            <#if (communicationEvent.content)??>
              ${escapeVal(communicationEvent.content, 'htmlmarkup', {"allow":"external"})}
            <#else>
              ${uiLabelMap.EcommerceEmptyBody}
            </#if>
        </@td>
      </@tr>
    </@table>
</@section>
