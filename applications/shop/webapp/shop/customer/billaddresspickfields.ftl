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

<#-- Cato: Based on editcreditcard.ftl -->

<#if useScripts && useNewAddr && newAddrInline && 
    newAddrContentId?has_content && pickFieldClass?has_content && 
    newAddrFieldId?has_content>
<@script>

<@initItemSelectionWithNewFormScript itemFieldClass=pickFieldClass 
    newItems=[{"fieldId":newAddrFieldId, "contentId":newAddrContentId}] />

</@script>
</#if>

<@fields type="default" ignoreParentField=true>
    <#-- Removed because is confusing, can add but would have to come back here with all data populated as before...
    <a href="<@ofbizUrl>editcontactmech</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">
      [Create New Address]</a>&nbsp;&nbsp;
    -->

  <#assign hasCurrent = curPostalAddress?has_content />

  <#if parameters["${fieldNamePrefix}contactMechId"]??>
    <#assign selectedContactMechId = parameters["${fieldNamePrefix}contactMechId"]>
  <#elseif hasCurrent>
    <#assign selectedContactMechId = curContactMechId>
  <#elseif (bapfFallbacks.contactMechId)??>
    <#assign selectedContactMechId = bapfFallbacks.contactMechId>
  <#else>
    <#assign selectedContactMechId = "">
  </#if>

  <@addressList>
  <#if hasCurrent>

    <#macro addrContent args={}>
        <p><b>${uiLabelMap.PartyUseCurrentAddress}:</b></p>
        <#list curPartyContactMechPurposes as curPartyContactMechPurpose>
          <#assign curContactMechPurposeType = curPartyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true) />
          <div>
            ${curContactMechPurposeType.get("description",locale)!}
            <#if curPartyContactMechPurpose.thruDate??>
              ((${uiLabelMap.CommonExpire}: ${curPartyContactMechPurpose.thruDate.toString()})
            </#if>
          </div>
        </#list>
        <div>
          <@formattedAddress address=curPostalAddress emphasis=false />
        </div>
      <#if showVerbose>
        <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(curPartyContactMech.fromDate.toString())!})</div>
        <#if curPartyContactMech.thruDate??><div>${uiLabelMap.CommonDelete}:&nbsp;${curPartyContactMech.thruDate.toString()}</div></#if>
      </#if>
    </#macro>
    <@addressEntry>
    <@commonInvField type="radio" name="${fieldNamePrefix}contactMechId" value=curContactMechId 
        labelContent=addrContent checked=(selectedContactMechId == curContactMechId) class="+${pickFieldClass}"  />
    </@addressEntry>
  <#else>
    <#-- 
    ${uiLabelMap.PartyBillingAddressNotSelected}
    -->
  </#if>
   <#-- is confusing
   ${uiLabelMap.EcommerceMessage3}
   -->
  <#list postalAddressInfos as postalAddressInfo>
    <#assign contactMech = postalAddressInfo.contactMech />
    <#assign partyContactMechPurposes = postalAddressInfo.partyContactMechPurposes />
    <#assign postalAddress = postalAddressInfo.postalAddress />
    <#assign partyContactMech = postalAddressInfo.partyContactMech />
    <#macro addrContent args={}>
        <#list partyContactMechPurposes as partyContactMechPurpose>
          <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true) />
          <div>
            ${contactMechPurposeType.get("description",locale)!}
            <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate})</#if>
          </div>
        </#list>
        <div>
          <@formattedAddress address=postalAddress emphasis=false />
        </div>
      <#if showVerbose>
        <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(partyContactMech.fromDate.toString())!})</div>
        <#if partyContactMech.thruDate??><div>${uiLabelMap.CommonDelete}:&nbsp;${partyContactMech.thruDate.toString()}</div></#if>
      </#if>
    </#macro>
    <@addressEntry>
    <@commonInvField type="radio" name="${fieldNamePrefix}contactMechId" value=contactMech.contactMechId 
        labelContent=addrContent checked=(selectedContactMechId == contactMech.contactMechId) class="+${pickFieldClass}" />
    </@addressEntry>
  </#list>
  </@addressList>
    <#if !postalAddressInfos?has_content && !curContactMech?? && !useNewAddr><#-- Cato: Don't show if also showing "new" option -->
      <@commonMsg type="info">${uiLabelMap.PartyNoContactInformation}.</@commonMsg>
    </#if>
  <#if useNewAddr>
    <#macro addrContent args={}>
        <#--${uiLabelMap.PartyCreateNewBillingAddress}-->
        <#if newAddrInline>
          <div<#if newAddrContentId?has_content> id="${newAddrContentId}"</#if><#rt/>
            <#lt><#if (selectedContactMechId != "_NEW_")> style="display:none;"</#if> class="new-item-selection-content">
            <@fields type="default" ignoreParentField=true>
              <@render resource="component://shop/widget/CustomerScreens.xml#postalAddressFields" 
                  ctxVars={
                    "pafFieldNamePrefix":newAddrFieldNamePrefix,
                    "pafUseScripts":useScripts,
                    "pafFallbacks":(bapfNewAddrFallbacks!{})
                    }/>
            </@fields>
          </div>       
        </#if>
    </#macro>
    <@addressList>
      <@addressEntry>
    <@commonInvField type="radio" name="${fieldNamePrefix}contactMechId" value="_NEW_" checked=(selectedContactMechId == "_NEW_") 
        class="+${pickFieldClass}" id=(newAddrFieldId!) label=uiLabelMap.PartyCreateNewBillingAddress/><#--labelContent=addrContent -->
      </@addressEntry>
    </@addressList>

    <@addrContent />
  </#if>
</@fields>

