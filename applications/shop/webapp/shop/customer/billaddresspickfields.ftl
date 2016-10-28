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

<#-- SCIPIO: Based on editcreditcard.ftl -->

<#if !bapfParams??>
  <#assign bapfParams = parameters>
</#if>

<#-- NOTE: You can override the look of the new address (postal code, country, etc.) fields 
    specifically using this map, which is args to @fields (see checkoutpayment.ftl for example)
    This is a map that itself may contain fieldArgs map 
    This is where the Scipio macro arg map support unleashes its power! -->
<#assign newAddressFieldsWrapperArgs = bapfNewAddressFieldsWrapperArgs!{
    "type":"default", "ignoreParentField":true,
    "fieldArgs": {
        "gridArgs": {
        }
    }
}>


<#if useScripts && useNewAddr && newAddrInline && 
    newAddrContentId?has_content && pickFieldClass?has_content && 
    newAddrFieldId?has_content>
  <@script>

  <@initItemSelectionWithContentFormScript itemFieldClass=pickFieldClass 
      contentItems=[{"fieldId":newAddrFieldId, "contentId":newAddrContentId}] />

  </@script>
</#if>

<@fields type="default" ignoreParentField=true>
    <#-- Removed because is confusing, can add but would have to come back here with all data populated as before...
    <a href="<@ofbizUrl>editcontactmech</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">
      [Create New Address]</a>&nbsp;&nbsp;
    -->

  <#assign hasCurrent = curPostalAddress?has_content />

  <#if bapfParams["${fieldNamePrefix}contactMechId"]??>
    <#assign selectedContactMechId = bapfParams["${fieldNamePrefix}contactMechId"]>
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
        <@formattedAddress usePanel=true address=curPostalAddress emphasis=false updateLink=(useUpdate?string(updateLink!,"")) 
            title="${uiLabelMap.PartyUseCurrentAddress}:">
          <#if showVerbose>
            <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(curPartyContactMech.fromDate.toString())!})</div>
            <#if curPartyContactMech.thruDate??><div>${uiLabelMap.CommonDelete}:&nbsp;${curPartyContactMech.thruDate.toString()}</div></#if>
          </#if>
        </@formattedAddress>
    </#macro>
    <@addressEntry>
    <@checkAddressInvField type="radio" name="${fieldNamePrefix}contactMechId" value=curContactMechId 
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
        <div>
          <@formattedAddress usePanel=true address=postalAddress emphasis=false 
            updateLink=(useUpdate?string(rawString(updateLink!)?replace('_CONTACT_MECH_ID_', contactMech.contactMechId),"")) 
            partyContactMechPurposes=partyContactMechPurposes>
            <#if showVerbose>
              <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(partyContactMech.fromDate.toString())!})</div>
              <#if partyContactMech.thruDate??><div>${uiLabelMap.CommonDelete}:&nbsp;${partyContactMech.thruDate.toString()}</div></#if>
            </#if>
          </@formattedAddress>
        </div>
    </#macro>
    <@addressEntry>
      <@checkAddressInvField type="radio" name="${fieldNamePrefix}contactMechId" value=contactMech.contactMechId 
          labelContent=addrContent checked=(selectedContactMechId == contactMech.contactMechId) class="+${pickFieldClass}" />
    </@addressEntry>
  </#list>
  <#if useNewAddr>
    <#macro addrContent args={}>
        <#--${uiLabelMap.PartyCreateNewBillingAddress}-->
        <#if newAddrInline>
          <div<#if newAddrContentId?has_content> id="${newAddrContentId}"</#if><#rt/>
            <#lt><#if (selectedContactMechId != "_NEW_")> style="display:none;"</#if> class="new-item-selection-content">
            <@fields args=newAddressFieldsWrapperArgs>
              <@render resource="component://shop/widget/CustomerScreens.xml#postalAddressFields" 
                  ctxVars={
                    "pafFieldNamePrefix":newAddrFieldNamePrefix,
                    "pafFieldIdPrefix":newAddrFieldIdPrefix,
                    "pafUseScripts":useScripts,
                    "pafFallbacks":(bapfNewAddrFallbacks!{}),
                    "pafParams":bapfParams
                    }/>
            </@fields>
          </div>       
        </#if>
    </#macro>
    <@addressEntry ownLine=true>
      <#assign labelContent><span class="new-address-radio-label">${uiLabelMap.PartyCreateNewBillingAddress}</span></#assign>
      <@checkAddressInvField type="radio" name="${fieldNamePrefix}contactMechId" value="_NEW_" checked=(selectedContactMechId == "_NEW_") 
          class="+${pickFieldClass}" id=(newAddrFieldId!) labelContent=labelContent/><#--label=uiLabelMap.PartyCreateNewBillingAddress labelContent=addrContent -->
    </@addressEntry>
  </#if>
  </@addressList>
    
  <#if useNewAddr>
    <@addrContent />
  </#if>

    <#if !postalAddressInfos?has_content && !curContactMech?? && !useNewAddr><#-- SCIPIO: Don't show if also showing "new" option -->
      <@commonMsg type="info">${uiLabelMap.PartyNoContactInformation}.</@commonMsg>
    </#if>
</@fields>

