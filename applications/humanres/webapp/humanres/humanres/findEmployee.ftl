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

<#assign extInfo = parameters.extInfo?default("N")>

<#assign menuHtml>
  <@menu type="section" inlineItems=true>
  <#if parameters.hideFields?default("N") == "Y">
    <@menuitem type="link" href=makeOfbizUrl("findEmployees?hideFields=N${paramList}" text="${uiLabelMap.CommonShowLookupFields}") />
  <#else>
    <#if partyList??><@menuitem type="link" href=makeOfbizUrl("findEmployees?hideFields=Y${paramList}" text="${uiLabelMap.CommonHideFields}") /></#if>
    <@menuitem type="link" href="javascript:document.lookupparty.submit();" text="${uiLabelMap.PartyLookupParty}" />
  </#if>
  </@menu>
</#assign>
<@section id="findEmployee" title="${uiLabelMap.CommonFind} ${uiLabelMap.HumanResEmployee}" menuHtml=menuHtml>
    <#if parameters.hideFields?default("N") != "Y">
      <#-- NOTE: this form is setup to allow a search by partial partyId or userLoginId; to change it to go directly to
          the viewprofile page when these are entered add the follow attribute to the form element:

           onsubmit="javascript:lookupparty('<@ofbizUrl>viewprofile</@ofbizUrl>');"
       -->
        <form method="post" name="lookupparty" action="<@ofbizUrl>findEmployees</@ofbizUrl>" class="basic-form">
            <input type="hidden" name="lookupFlag" value="Y"/>
            <input type="hidden" name="hideFields" value="Y"/>
                <@field type="generic" label="${uiLabelMap.PartyContactInformation}">
                    <input type="radio" name="extInfo" value="N" onclick="javascript:refreshInfo();" <#if extInfo == "N">checked="checked"</#if>/>${uiLabelMap.CommonNone}&nbsp;
                        <input type="radio" name="extInfo" value="P" onclick="javascript:refreshInfo();" <#if extInfo == "P">checked="checked"</#if>/>${uiLabelMap.PartyPostal}&nbsp;
                        <input type="radio" name="extInfo" value="T" onclick="javascript:refreshInfo();" <#if extInfo == "T">checked="checked"</#if>/>${uiLabelMap.PartyTelecom}&nbsp;
                        <input type="radio" name="extInfo" value="O" onclick="javascript:refreshInfo();" <#if extInfo == "O">checked="checked"</#if>/>${uiLabelMap.CommonOther}&nbsp;
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyPartyId}">
                    <@htmlTemplate.lookupField value='${requestParameters.partyId!}' formName="lookupparty" name="partyId" id="partyId" fieldFormName="LookupPerson"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyUserLogin}">
                    <input type="text" name="userLoginId" value="${parameters.userLoginId!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyLastName}">
                    <input type="text" name="lastName" value="${parameters.lastName!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyFirstName}">
                    <input type="text" name="firstName" value="${parameters.firstName!}"/>
                </@field>
                <@field type="generic"><input type="hidden" name="groupName" value="${parameters.groupName!}"/></@field>
                <@field type="generic"><input type="hidden" name="roleTypeId" value="EMPLOYEE"/></@field>
            <#if extInfo == "P">
                <hr />
                <@field type="generic" label="${uiLabelMap.CommonAddress1}">
                    <input type="text" name="address1" value="${parameters.address1!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.CommonAddress2}">
                    <input type="text" name="address2" value="${parameters.address2!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.CommonCity}">
                    <input type="text" name="city" value="${parameters.city!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.CommonStateProvince}">
                    <select name="stateProvinceGeoId">
                        <#if currentStateGeo?has_content>
                            <option value="${currentStateGeo.geoId}">${currentStateGeo.geoName?default(currentStateGeo.geoId)}</option>
                            <option value="${currentStateGeo.geoId}">---</option>
                        </#if>
                            <option value="ANY">${uiLabelMap.CommonAnyStateProvince}</option>
                            ${screens.render("component://common/widget/CommonScreens.xml#states")}
                        </select>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyPostalCode}">
                    <input type="text" name="postalCode" value="${parameters.postalCode!}"/>
                </@field>
            </#if>
            <#if extInfo == "T">
                <hr />
                <@field type="generic" label="${uiLabelMap.CommonCountryCode}">
                    <input type="text" name="countryCode" value="${parameters.countryCode!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyAreaCode}">
                    <input type="text" name="areaCode" value="${parameters.areaCode!}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.PartyContactNumber}">
                    <input type="text" name="contactNumber" value="${parameters.contactNumber!}"/>
                </@field>
            </#if>
            <#if extInfo == "O">
                <hr />
                <@field type="generic" label="${uiLabelMap.PartyContactInformation}">
                    <input type="text" name="infoString" value="${parameters.infoString!}"/>
                </@field>
            </#if>
                <hr />
                <@field type="submitarea">
                    <input type="submit" value="${uiLabelMap.PartyLookupParty}" onclick="javascript:document.lookupparty.submit();"/>
                        <a href="<@ofbizUrl>findEmployees?roleTypeId=EMPLOYEE&amp;hideFields=Y&amp;lookupFlag=Y</@ofbizUrl>" class="smallSubmit ${styles.button_default!}">${uiLabelMap.CommonShowAllRecords}</a>
                </@field>
        </form>
    </#if>
</@section>

  <#if parameters.hideFields?default("N") != "Y">
    <script language="JavaScript" type="text/javascript">
    <!--//
      document.lookupparty.partyId.focus();
    //-->
    </script>
  </#if>
    
  <#if partyList??>
    <#assign menuHtml>
      <@menu type="section" inlineItems=true>
      <#if (partyListSize > 0)>
        <@menuitem type="link" href=makeOfbizUrl("findEmployees?VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex+1}&amp;hideFields=${parameters.hideFields?default('N')}${paramList}" text="${uiLabelMap.CommonNext}" contentClass="+nav-next") disabled=(!(partyListSize > highIndex)) />
        <@menuitem type="text" text="${lowIndex} - ${highIndex} ${uiLabelMap.CommonOf} ${partyListSize}" />
        <@menuitem type="link" href=makeOfbizUrl("findEmployees?VIEW_SIZE=${viewSize}&amp;VIEW_INDEX=${viewIndex-1}&amp;hideFields=${parameters.hideFields?default('N')}${paramList}" text="${uiLabelMap.CommonPrevious}" contentClass="+nav-previous") disabled=(!(viewIndex > 0)) />
      </#if>
      </@menu>
    </#assign>
    <@section id="findEmployeeResults" title="${uiLabelMap.PartyPartiesFound}" menuHtml=menuHtml>
    <#if partyList?has_content>
        <@table type="data-list" autoAltRows=true class="basic-table" cellspacing="0">
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.PartyPartyId}</@th>
                <@th>${uiLabelMap.PartyUserLogin}</@th>
                <@th>${uiLabelMap.PartyName}</@th>
                <#if extInfo?default("") == "P" >
                    <@th>${uiLabelMap.PartyCity}</@th>
                </#if>
                <#if extInfo?default("") == "P">
                    <@th>${uiLabelMap.PartyPostalCode}</@th>
                </#if>
                <#if extInfo?default("") == "T">
                    <@th>${uiLabelMap.PartyAreaCode}</@th>
                </#if>
                <@th>${uiLabelMap.PartyType}</@th>
                <@th>&nbsp;</@th>
            </@tr>
            </@thead>
            <#list partyList as partyRow>
            <#assign partyType = partyRow.getRelatedOne("PartyType", false)!>
            <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.button_default!}">${partyRow.partyId}</a></@td>
                <@td><#if partyRow.containsKey("userLoginId")>
                        ${partyRow.userLoginId?default("N/A")}
                    <#else>
                    <#assign userLogins = partyRow.getRelated("UserLogin", null, null, false)>
                    <#if (userLogins.size() > 0)>
                        <#if (userLogins.size() > 1)>
                            (${uiLabelMap.CommonMany})
                        <#else>
                        <#assign userLogin = userLogins.get(0)>
                            ${userLogin.userLoginId}
                        </#if>
                        <#else>
                            (${uiLabelMap.CommonNone})
                        </#if>
                    </#if>
                </@td>
                <@td><#if partyRow.getModelEntity().isField("lastName") && lastName?has_content>
                        ${partyRow.lastName}<#if partyRow.firstName?has_content>, ${partyRow.firstName}</#if>
                    <#elseif partyRow.getModelEntity().isField("groupName") && partyRow.groupName?has_content>
                        ${partyRow.groupName}
                    <#else>
                    <#assign partyName = Static["org.ofbiz.party.party.PartyHelper"].getPartyName(partyRow, true)>
                    <#if partyName?has_content>
                        ${partyName}
                    <#else>
                        (${uiLabelMap.PartyNoNameFound})
                    </#if>
                    </#if>
                </@td>
                <#if extInfo?default("") == "T">
                    <@td>${partyRow.areaCode!}</@td>
                </#if>
                <#if extInfo?default("") == "P" >
                    <@td>${partyRow.city!}, ${partyRow.stateProvinceGeoId!}</@td>
                </#if>
                <#if extInfo?default("") == "P">
                    <@td>${partyRow.postalCode!}</@td>
                </#if>
                <@td><#if partyType.description??>${partyType.get("description", locale)}<#else>???</#if></@td>
                <@td class="button-col align-float">
                    <a href="<@ofbizUrl>EmployeeProfile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDetails}</a>
                </@td>
            </@tr>
            </#list>
        </@table>
    <#else>
        <@resultMsg>${uiLabelMap.PartyNoPartiesFound}</@resultMsg>
    </#if>
    <#if lookupErrorMessage??>
        <@alert type="error">${lookupErrorMessage}</@alert>
    </#if>
    </@section>
  </#if>
<!-- end findEmployees.ftl -->
