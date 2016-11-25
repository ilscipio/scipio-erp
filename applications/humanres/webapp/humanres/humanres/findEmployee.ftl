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

<#assign extInfo = parameters.extInfo!"N">

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
<#-- SCIPIO: show at bottom 
    <@menuitem type="link" href="javascript:document.lookupparty.submit();" text=uiLabelMap.PartyLookupParty class="+${styles.action_run_sys!} ${styles.action_find!}" />->
-->
    <@menuitem type="link" href=makeOfbizUrl("NewEmployee") text=uiLabelMap.HumanResNewEmployee class="+${styles.action_nav!} ${styles.action_add!}"/>
    <#if findEmplQueryRan>
       <#if (parameters.hideFields!"N") == "Y">
         <@menuitem type="link" href=makeOfbizUrl("findEmployees?hideFields=N&doFindQuery=Y${rawString(paramList)}") text=uiLabelMap.CommonShowLookupFields class="+${styles.action_run_sys!} ${styles.action_show!}"/>
       <#else>
         <@menuitem type="link" href=makeOfbizUrl("findEmployees?hideFields=Y&doFindQuery=Y${rawString(paramList)}") text=uiLabelMap.CommonHideFields class="+${styles.action_run_sys!} ${styles.action_hide!}"/>
       </#if>
    </#if>
  </@menu>
</#macro>
<@section id="findEmployee" menuContent=menuContent>
    <#if (parameters.hideFields!"N") != "Y">
      <#-- NOTE: this form is setup to allow a search by partial partyId or userLoginId; to change it to go directly to
          the viewprofile page when these are entered add the follow attribute to the form element:

           onsubmit="javascript:lookupparty('<@ofbizUrl>viewprofile</@ofbizUrl>');"
       -->
        <form method="post" name="lookupparty" action="<@ofbizUrl>findEmployees</@ofbizUrl>" class="basic-form">
            <input type="hidden" name="doFindQuery" value="Y"/><#-- SCIPIO: extra control in addition to POST -->
            <input type="hidden" name="lookupFlag" value="Y"/>
            <input type="hidden" name="hideFields" value="Y"/>
                <@field type="generic" label=uiLabelMap.PartyContactInformation>
                    <@field type="radio" name="extInfo" value="N" onClick="javascript:refreshInfo();" checked=(extInfo == "N") label=uiLabelMap.CommonNone/>
                    <@field type="radio" name="extInfo" value="P" onClick="javascript:refreshInfo();" checked=(extInfo == "P") label=uiLabelMap.PartyPostal/>
                    <@field type="radio" name="extInfo" value="T" onClick="javascript:refreshInfo();" checked=(extInfo == "T") label=uiLabelMap.PartyTelecom/>
                    <@field type="radio" name="extInfo" value="O" onClick="javascript:refreshInfo();" checked=(extInfo == "O") label=uiLabelMap.CommonOther/>
                </@field>
                <@field type="input" label=uiLabelMap.PartyLastName name="lastName" value=(parameters.lastName!)/>
                <@field type="input" label=uiLabelMap.PartyFirstName name="firstName" value=(parameters.firstName!)/>
                <@field type="lookup" label=uiLabelMap.PartyPartyId value=(requestParameters.partyId!) formName="lookupparty" name="partyId" id="partyId" fieldFormName="LookupPerson"/>
                <@field type="input" label=uiLabelMap.PartyUserLogin name="userLoginId" value=(parameters.userLoginId!)/>
                <input type="hidden" name="groupName" value="${parameters.groupName!}"/>
                <input type="hidden" name="roleTypeId" value="EMPLOYEE"/>
                
            <#if extInfo == "P">
                <hr />
                <@field type="input" label=uiLabelMap.CommonAddress1 name="address1" value=(parameters.address1!)/>
                <@field type="input" label=uiLabelMap.CommonAddress2 name="address2" value=(parameters.address2!)/>
                <@field type="input" label=uiLabelMap.CommonCity name="city" value=(parameters.city!)/>
                <@field type="select" label=uiLabelMap.CommonStateProvince name="stateProvinceGeoId">
                  <#if currentStateGeo?has_content>
                    <option value="${currentStateGeo.geoId}">${currentStateGeo.geoName!currentStateGeo.geoId}</option>
                    <option value="${currentStateGeo.geoId}">---</option>
                  </#if>
                    <option value="ANY">${uiLabelMap.CommonAnyStateProvince}</option>
                    <@render resource="component://common/widget/CommonScreens.xml#states" ctxVars={"statesPreselect":false}/>
                </@field>
                <@field type="input" label=uiLabelMap.PartyPostalCode name="postalCode" value=(parameters.postalCode!)/>
            </#if>
            <#if extInfo == "T">
                <hr />
                <@field type="input" label=uiLabelMap.CommonCountryCode name="countryCode" value=(parameters.countryCode!)/>
                <@field type="input" label=uiLabelMap.PartyAreaCode name="areaCode" value=(parameters.areaCode!)/>
                <@field type="input" label=uiLabelMap.PartyContactNumber name="contactNumber" value=(parameters.contactNumber!)/>
            </#if>
            <#if extInfo == "O">
                <hr />
                <@field type="input" label=uiLabelMap.PartyContactInformation name="infoString" value=(parameters.infoString!)/>
            </#if>
                <#--<hr />-->
                <@field type="submitarea">
                    <@field type="submit" text=uiLabelMap.PartyLookupParty onClick="javascript:document.lookupparty.submit();" class="+${styles.link_run_sys!} ${styles.action_find!}"/>
                    <@field type="submit" submitType="link" href=makeOfbizUrl("findEmployees?roleTypeId=EMPLOYEE&hideFields=Y&lookupFlag=Y&doFindQuery=Y") class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonShowAllRecords />
                </@field>
        </form>
    </#if>
</@section>

  <#if (parameters.hideFields!"N") != "Y">
    <@script>
      jQuery(document).ready(function() {
        document.lookupparty.partyId.focus();
      });
    </@script>
  </#if>
    
  <#if findEmplQueryRan>
    <@section id="findEmployeeResults" title=uiLabelMap.PartyPartiesFound>
    <#if lookupErrorMessage??>
        <@commonMsg type="error">${lookupErrorMessage}</@commonMsg>
    </#if>
    <#if partyList?has_content>
      <#assign paramStr = addParamsToStr(rawString(paramList!""), {"hideFields": parameters.hideFields!"N"}, "&", false)>
      <@paginate mode="content" url=makeOfbizUrl("findEmployees") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=partyListSize!0>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.PartyName}</@th>
                <@th>${uiLabelMap.PartyPartyId}</@th>
                <#if extInfo?default("") == "P" >
                    <@th>${uiLabelMap.PartyCity}</@th>
                </#if>
                <#if extInfo?default("") == "P">
                    <@th>${uiLabelMap.PartyPostalCode}</@th>
                </#if>
                <#if extInfo?default("") == "T">
                    <@th>${uiLabelMap.PartyAreaCode}</@th>
                </#if>
            </@tr>
            </@thead>
            <#list partyList as partyRow>
            <#assign partyType = partyRow.getRelatedOne("PartyType", false)!>
            <@tr valign="middle">
                <@td><#if partyRow.getModelEntity().isField("lastName") && partyRow.lastName?has_content>
                        <a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${partyRow.lastName}<#if partyRow.firstName?has_content>, ${partyRow.firstName}</#if></a>
                    <#elseif partyRow.getModelEntity().isField("groupName") && partyRow.groupName?has_content>
                        <a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${partyRow.groupName}</a>
                    <#else>
                        <#assign partyName = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(partyRow, true)>
                        <#if partyName?has_content>
                            <a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${partyName}</a>
                        <#else>
                            (${uiLabelMap.PartyNoNameFound})
                        </#if>
                    </#if>
                </@td>
                <@td><a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${partyRow.partyId}</a></@td>
                <#if extInfo?default("") == "T">
                    <@td>${partyRow.areaCode!}</@td>
                </#if>
                <#if extInfo?default("") == "P" >
                    <@td>${partyRow.city!}, ${partyRow.stateProvinceGeoId!}</@td>
                </#if>
                <#if extInfo?default("") == "P">
                    <@td>${partyRow.postalCode!}</@td>
                </#if>
            </@tr>
            </#list>
        </@table>
      </@paginate>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoPartiesFound}</@commonMsg>
    </#if>
    </@section>
  </#if>
