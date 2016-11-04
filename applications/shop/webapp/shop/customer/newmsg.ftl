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
<#include "customercommon.ftl">

<#-- TODO: this was turned into menu below, may need something more to achieve look... extra menu class/type...
        this code was BEFORE or LEFT of title, not after
        <div class="boxlink">
            <#if showMessageLinks?default("false")?upper_case == "TRUE">
                <a href="<@ofbizUrl>messagelist</@ofbizUrl>" class="submenutextright">${uiLabelMap.EcommerceViewList}</a>
            </#if>
        </div>
-->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if ((showMessageLinks!"FALSE")?upper_case) == "TRUE">
          <@menuitem type="link" href=makeOfbizUrl("messagelist") text=uiLabelMap.EcommerceViewList />
        </#if>
    </@menu>
</#macro>
<@section title=(pageHeader!) menuContent=menuContent menuLayoutTitle="inline-title">
      <form name="contactus" method="post" action="<@ofbizUrl>${submitRequest}</@ofbizUrl>">
        <input type="hidden" name="partyIdFrom" value="${userLogin.partyId}"/>
        <input type="hidden" name="contactMechTypeId" value="WEB_ADDRESS"/>
        <input type="hidden" name="communicationEventTypeId" value="WEB_SITE_COMMUNICATI"/>
        <#if productStore?has_content>
          <input type="hidden" name="partyIdTo" value="${productStore.payToPartyId!}"/>
        </#if>
        <input type="hidden" name="note" value="${Static["org.ofbiz.base.util.UtilHttp"].getFullRequestUrl(request)}"/>
        <#if message?has_content>
          <input type="hidden" name="parentCommEventId" value="${communicationEvent.communicationEventId}"/>
          <#if (communicationEvent.origCommEventId?? && communicationEvent.origCommEventId?length > 0)>
            <#assign orgComm = communicationEvent.origCommEventId>
          <#else>
            <#assign orgComm = communicationEvent.communicationEventId>
          </#if>
          <input type="hidden" name="origCommEventId" value="${orgComm}"/>
        </#if>
        <@table type="fields"> <#-- orig: width="100%" border="0" cellspacing="0" cellpadding="0" class="boxbottom" -->
          <@tr>
            <@td colspan="2">&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.CommonFrom}</@td>
            <@td>&nbsp;${sessionAttributes.autoName!} [${userLogin.partyId}] (${uiLabelMap.CommonNotYou}?&nbsp;<a href="<@ofbizUrl>autoLogout</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_login!}">${uiLabelMap.CommonClickHere}</a>)</@td>
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
