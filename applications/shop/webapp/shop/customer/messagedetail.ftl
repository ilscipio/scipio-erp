<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
              <a href="<@ofbizUrl>newmessage?communicationEventId=${communicationEvent.communicationEventId}</@ofbizUrl>" class="submenutext">${uiLabelMap.PartyReply}</a>
            </#if>
            <a href="<@ofbizUrl>messagelist</@ofbizUrl>" class="submenutextright">${uiLabelMap.EcommerceViewList}</a>
        </div>
-->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if ((communicationEvent.partyIdFrom!) != (userLogin.partyId)!)>
          <@menuitem type="link" href=makeOfbizUrl("newmessage?communicationEventId=${communicationEvent.communicationEventId}") text=uiLabelMap.PartyReply />
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("messagelist") text=uiLabelMap.EcommerceViewList />
    </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceReadMessage menuContent=menuContent menuLayoutTitle="inline-title">
    <@table type="fields"> <#-- orig: width="100%" border="0" cellpadding="1" -->
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
