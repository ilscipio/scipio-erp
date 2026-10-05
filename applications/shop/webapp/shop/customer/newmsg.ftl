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

<#-- TODO: this was turned into menu below, may need something more to achieve look... extra menu class/type...
        this code was BEFORE or LEFT of title, not after
        <div class="boxlink">
            <#if showMessageLinks?default("false")?upper_case == "TRUE">
                <a href="<@pageUrl>messagelist</@pageUrl>" class="submenutextright">${uiLabelMap.EcommerceViewList}</a>
            </#if>
        </div>
-->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if ((showMessageLinks!"FALSE")?upper_case) == "TRUE">
          <@menuitem type="link" href=makePageUrl("messagelist") text=uiLabelMap.EcommerceViewList />
        </#if>
    </@menu>
</#macro>
<@section title=(pageHeader!) menuContent=menuContent menuLayoutTitle="inline-title">
      <form name="contactus" method="post" action="<@pageUrl>${submitRequest}</@pageUrl>">
        <input type="hidden" name="partyIdFrom" value="${userLogin.partyId}"/>
        <input type="hidden" name="contactMechTypeId" value="WEB_ADDRESS"/>
        <input type="hidden" name="communicationEventTypeId" value="WEB_SITE_COMMUNICATI"/>
        <#if productStore?has_content>
          <input type="hidden" name="partyIdTo" value="${productStore.payToPartyId!}"/>
        </#if>
        <input type="hidden" name="note" value="${UtilHttp.getFullRequestUrl(request)}"/>
        <#if message?has_content>
          <input type="hidden" name="parentCommEventId" value="${communicationEvent.communicationEventId}"/>
          <#if (communicationEvent.origCommEventId?? && communicationEvent.origCommEventId?length > 0)>
            <#assign orgComm = communicationEvent.origCommEventId>
          <#else>
            <#assign orgComm = communicationEvent.communicationEventId>
          </#if>
          <input type="hidden" name="origCommEventId" value="${orgComm}"/>
        </#if>
        <@table type="fields">
          <@tr>
            <@td colspan="2">&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.CommonFrom}</@td>
            <@td>&nbsp;${sessionAttributes.autoName!} [${userLogin.partyId}] (${uiLabelMap.CommonNotYou}?&nbsp;<a href="<@pageUrl>autoLogout</@pageUrl>" class="${styles.link_nav!} ${styles.action_login!}">${uiLabelMap.CommonClickHere}</a>)</@td>
          </@tr>
          <#if partyIdTo?has_content>
            <#assign partyToName = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, partyIdTo, true)>
            <input type="hidden" name="partyIdTo" value="${partyIdTo}"/>
            <@tr>
              <@td colspan="2">&nbsp;</@td>
            </@tr>
            <@tr>
              <@td>${uiLabelMap.CommonTo}</@td>
              <@td>&nbsp;${partyToName}</@td>
            </@tr>
          </#if>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
          </@tr>
          <#assign defaultSubject = (communicationEvent.subject)?default("")>
          <#if (defaultSubject?length == 0)>
            <#assign replyPrefix = "RE: ">
            <#if parentEvent?has_content>
              <#if !parentEvent.subject?default("")?upper_case?starts_with(replyPrefix)>
                <#assign defaultSubject = replyPrefix>
              </#if>
              <#assign defaultSubject = defaultSubject + parentEvent.subject?default("")>
            </#if>
          </#if>
          <@tr>
            <@td>${uiLabelMap.EcommerceSubject}</@td>
            <@td><input type="input" name="subject" size="20" value="${defaultSubject}"/></@td>
          </@tr>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.CommonMessage}</@td>
            <@td>&nbsp;</@td>
          </@tr>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
            <@td colspan="2">
              <textarea name="content" cols="40" rows="5"></textarea>
            </@td>
          </@tr>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
          </@tr>
          <@tfoot>
            <@tr>
              <@td colspan="2">&nbsp;</@td>
              <@td><input type="submit" class="${styles.link_run_sys!} ${styles.action_send!}" value="${uiLabelMap.CommonSend}"/></@td>
            </@tr>
          </@tfoot>
        </@table>
      </form>
</@section>
