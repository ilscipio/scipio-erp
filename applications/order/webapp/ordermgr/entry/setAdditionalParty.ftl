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

<#-- ==================== Party Selection dialog box ========================= -->
<@section>
  <@row>
    <@cell columns=6 last=true>

  <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
    <input type="hidden" name="finalizeReqAdditionalParty" value="false"/>
    <input type="hidden" name="finalizeMode" value="addpty"/>
  </form>
  <form method="post" action="<@ofbizUrl>setAdditionalParty</@ofbizUrl>" name="quickAddPartyForm">

  <#assign defaultFieldGridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":false})>
  <#macro partyCheckRow label id>
    <@row>
      <@cell class=addClassArg(defaultFieldGridStyles.labelArea, "${styles.text_right!}")>
        <#nested>
      </@cell>
      <@cell class=defaultFieldGridStyles.widgetArea>
        <label for="${id}">${label}</label>
      </@cell>
    </@row>
  </#macro>

    <@section>
      <@heading>1) ${uiLabelMap.OrderSelectPartyToOrder}</@heading>

      <@partyCheckRow label=uiLabelMap.CommonPerson id="additionalPartyType_Person">
        <input type="radio" id="additionalPartyType_Person" name="additionalPartyType" value="Person" onclick="<#if additionalPartyType??>javascript:document.quickAddPartyForm.additionalPartyId.value='';</#if>document.quickAddPartyForm.submit()"<#if (additionalPartyType?? && additionalPartyType == "Person")> checked="checked"</#if> />
      </@partyCheckRow>

      <@partyCheckRow label=uiLabelMap.CommonGroup id="additionalPartyType_Group">
        <input type="radio" id="additionalPartyType_Group" name="additionalPartyType" value="Group" onclick="<#if additionalPartyType??>javascript:document.quickAddPartyForm.additionalPartyId.value='';</#if>document.quickAddPartyForm.submit()"<#if additionalPartyType?? && additionalPartyType == "Group"> checked="checked"</#if> />
      </@partyCheckRow>

      <@partyCheckRow label=uiLabelMap.OrderPartyDontWish id="additionalPartyType_None">
        <input type="radio" id="additionalPartyType_None" name="additionalPartyType" value="None" onclick="<#if additionalPartyType??>javascript:document.quickAddPartyForm.additionalPartyId.value='';</#if>document.quickAddPartyForm.submit()"<#if !additionalPartyType??> checked="checked"</#if> />
      </@partyCheckRow>
    </@section>

  <#if additionalPartyType?? && additionalPartyType != "" && additionalPartyType != "None">
    <#if additionalPartyType == "Person">
      <#assign lookupPartyView="LookupPerson">
    <#else>
      <#assign lookupPartyView="LookupPartyGroup">
    </#if>

    <@section>
      <@heading>2) ${uiLabelMap.PartyFindParty}</@heading>
  
      <@field type="lookup" label=uiLabelMap.CommonIdentifier value=(additionalPartyId!) formName="quickAddPartyForm" name="additionalPartyId" id="additionalPartyId" fieldFormName=lookupPartyView/>
      <@field type="submit" submitType="link" href="javascript:document.quickAddPartyForm.submit()" class="+${styles.link_run_session!} ${styles.action_update!}" text=uiLabelMap.CommonApply />
    </@section>

  </form>

    <#if roles?has_content>
  
    <@section>
      <@heading>3) ${uiLabelMap.OrderPartySelectRoleForParty}</@heading>
      
        <form method="post" action="<@ofbizUrl>addAdditionalParty</@ofbizUrl>" name="addAdditionalPartyForm" id="addAdditionalPartyForm">
          <@fields type="default-nolabelarea">
          <input type="hidden" name="additionalPartyId" value="${additionalPartyId}" />
          <@field type="select" label=uiLabelMap.CommonRole name="additionalRoleTypeId" id="additionalRoleTypeId" size="5" multiple=true>
              <#list roles as role>
              <option value="${role.roleTypeId}">${role.get("description",locale)}</option>
              </#list>
          </@field>
          <@field type="submit" class="+${styles.link_run_session!} ${styles.action_add!}" text=uiLabelMap.CommonAdd/>
          </@fields>
        </form>
    </@section>

    </#if> <#-- roles?has_content -->
  <#else>
  </form>
  </#if> <#-- additionalPartyType?has_content -->

    </@cell>
  </@row>
</@section>
