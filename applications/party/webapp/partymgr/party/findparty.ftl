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
<#assign extInfo = parameters.extInfo!("N")>
<#assign inventoryItemId = parameters.inventoryItemId!"">
<#assign serialNumber = parameters.serialNumber!"">
<#assign softIdentifier = parameters.softIdentifier!"">
<#assign sortField = parameters.sortField!/>
<#-- Only allow the search fields to be hidden when we have some results -->
<#if partyList?has_content>
  <#assign hideFields = parameters.hideFields!"N">
<#else>
  <#assign hideFields = "N">
</#if>
<#if (parameters.firstName?has_content || parameters.lastName?has_content)>
  <#assign createUrl = "editperson?create_new=Y&amp;lastName=${parameters.lastName!}&amp;firstName=${parameters.firstName!}"/>
<#elseif (parameters.groupName?has_content)>
  <#assign createUrl = "editpartygroup?create_new=Y&amp;groupName=${parameters.groupName!}"/>
<#else>
  <#assign createUrl = "createnew"/>
</#if>

<#assign menuHtml>
      <li><a href="<@ofbizUrl>${createUrl}</@ofbizUrl>" class="${styles.button_default!} create">${uiLabelMap.CommonCreateNew}</a></li>
<#if partyList?has_content>    
  <#if hideFields == "Y">
      <li class="collapsed"><a href="<@ofbizUrl>findparty?hideFields=N&amp;sortField=${sortField!}${paramList}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonShowLookupFields}</a></li>
  <#else>
      <li class="expanded"><a href="<@ofbizUrl>findparty?hideFields=Y&amp;sortField=${sortField!}${paramList}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonHideFields}</a></li>
  </#if>
</#if>  
</#assign>
<@section menuHtml=menuHtml>
  
    <div id="findPartyParameters" <#if hideFields != "N"> style="display:none" </#if> >

      <#-- NOTE: this form is setup to allow a search by partial partyId or userLoginId; to change it to go directly to
          the viewprofile page when these are entered add the follow attribute to the form element:

           onsubmit="javascript:lookupParty('<@ofbizUrl>viewprofile</@ofbizUrl>');"
       -->
      <form method="post" name="lookupparty" action="<@ofbizUrl>findparty</@ofbizUrl>" class="basic-form">
        <input type="hidden" name="lookupFlag" value="Y"/>
        <input type="hidden" name="hideFields" value="Y"/>
        
        
    <@row>
      <@cell columns=9>        
        
        <#assign radioItems = [
          { "key": "N", "description": "${uiLabelMap.CommonNone}", "event": "onclick", "action": "javascript:refreshInfo();" },
          { "key": "P", "description": "${uiLabelMap.PartyPostal}", "event": "onclick", "action": "javascript:refreshInfo();" },
          { "key": "T", "description": "${uiLabelMap.PartyTelecom}", "event": "onclick", "action": "javascript:refreshInfo();" },
          { "key": "O", "description": "${uiLabelMap.CommonOther}", "event": "onclick", "action": "javascript:refreshInfo();" }
        ]>
        <@field type="radio" label="${uiLabelMap.PartyContactInformation}" name="extInfo" items=radioItems currentValue="${extInfo!}"/>
        
        <@field type="input" label="${uiLabelMap.PartyPartyId}" name="partyId" value="${parameters.partyId!}"/>
        <@field type="input" label="${uiLabelMap.PartyUserLogin}" name="userLoginId" value="${parameters.userLoginId!}"/>
        <@field type="input" label="${uiLabelMap.PartyLastName}" name="lastName" value="${parameters.lastName!}"/>
        <@field type="input" label="${uiLabelMap.PartyFirstName}" name="firstName" value="${parameters.firstName!}"/>
        <@field type="input" label="${uiLabelMap.PartyPartyGroupName}" name="groupName" value="${parameters.groupName!}"/>
        <@field type="select" label="${uiLabelMap.PartyRoleType}" name="roleTypeId" currentValue="${(currentRole.roleTypeId)!}">
          <#if currentRole?has_content>
            <option value="${currentRole.roleTypeId}">${currentRole.get("description",locale)}</option>
            <option value="${currentRole.roleTypeId}">---</option>
          </#if>
            <option value="ANY">${uiLabelMap.CommonAnyRoleType}</option>
          <#list roleTypes as roleType>
            <option value="${roleType.roleTypeId}">${roleType.get("description",locale)}</option>
          </#list>        
        </@field>
        <@field type="select" label="${uiLabelMap.PartyType}" name="partyTypeId" currentValue="${(currentPartyType.partyTypeId)!}">
          <#if currentPartyType?has_content>
            <option value="${currentPartyType.partyTypeId}">${currentPartyType.get("description",locale)}</option>
            <option value="${currentPartyType.partyTypeId}">---</option>
          </#if>
            <option value="ANY">${uiLabelMap.CommonAny}</option>
          <#list partyTypes as partyType>
            <option value="${partyType.partyTypeId}">${partyType.get("description",locale)}</option>
          </#list>    
        </@field>
        <@field type="input" label="${uiLabelMap.ProductInventoryItemId}" name="inventoryItemId" value="${parameters.inventoryItemId!}"/>
        <@field type="input" label="${uiLabelMap.ProductSerialNumber}" name="serialNumber" value="${parameters.serialNumber!}"/>
        <@field type="input" label="${uiLabelMap.ProductSoftIdentifier}" name="softIdentifier" value="${parameters.softIdentifier!}"/>
        
    <#if extInfo == "P">
        <hr />
        <@field type="input" label="${uiLabelMap.CommonAddress1}" name="address1" value="${parameters.address1!}"/>
        <@field type="input" label="${uiLabelMap.CommonAddress2}" name="address2" value="${parameters.address2!}"/>
        <@field type="input" label="${uiLabelMap.CommonCity}" name="city" value="${parameters.city!}"/>
        <@field type="select" label="${uiLabelMap.CommonStateProvince}" name="stateProvinceGeoId" currentValue="${(currentStateGeo.geoId)!}">
          <#if currentStateGeo?has_content>
            <option value="${currentStateGeo.geoId}">${currentStateGeo.geoName!(currentStateGeo.geoId)}</option>
            <option value="${currentStateGeo.geoId}">---</option>
          </#if>
            <option value="ANY">${uiLabelMap.CommonAnyStateProvince}</option>
            ${screens.render("component://common/widget/CommonScreens.xml#states")}     
        </@field>
        <@field type="input" label="${uiLabelMap.PartyPostalCode}" name="postalCode" value="${parameters.postalCode!}"/>
        
    </#if>
    <#if extInfo == "T">
        <hr />
        <@field type="input" label="${uiLabelMap.CommonCountryCode}" name="countryCode" value="${parameters.countryCode!}"/>
        <@field type="input" label="${uiLabelMap.PartyAreaCode}" name="areaCode" value="${parameters.areaCode!}"/>
        <@field type="input" label="${uiLabelMap.PartyContactNumber}" name="contactNumber" value="${parameters.contactNumber!}"/>
    </#if>
    <#if extInfo == "O">
        <hr />
        <@field type="input" label="${uiLabelMap.PartyContactInformation}" name="infoString" value="${parameters.infoString!}"/>
    </#if>

        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonFind}" onclick="javascript:document.lookupparty.submit();"/>
        </@field>
      </form>
    </div>
    <script language="JavaScript" type="text/javascript">
        $(document).ready(function() {
            document.lookupparty.partyId.focus();
        });
    </script>
    
    </@cell>
  </@row>
</@section>

<#if (searchPerformed!false)==true>

  <@section title="${uiLabelMap.CommonSearchResults}" id="findPartyResults">
    <@row>
      <@cell>
      
  <#if lookupErrorMessage?has_content>
    <@alert type="alert">
        ${lookupErrorMessage}
    </@alert>
  </#if>      
      
  <#if partyList?has_content>
    <#-- Pagination -->
    <#include "component://common/webcommon/includes/htmlTemplate.ftl"/>
    <#assign commonUrl = "findparty?hideFields=" + hideFields + paramList + "&amp;sortField=" + (sortField!) + "&amp;"/>
    <#assign viewIndexFirst = 0/>
    <#assign viewIndexPrevious = viewIndex - 1/>
    <#assign viewIndexNext = viewIndex + 1/>
    <#assign viewIndexLast = Static["org.ofbiz.base.util.UtilMisc"].getViewLastIndex(partyListSize, viewSize) />
    <#assign messageMap = Static["org.ofbiz.base.util.UtilMisc"].toMap("lowCount", lowIndex, "highCount", highIndex, "total", partyListSize)/>
    <#assign commonDisplaying = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)/>
    <#macro paginateParties>
      <@nextPrev commonUrl=commonUrl ajaxEnabled=false javaScriptEnabled=false paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=partyListSize viewSize=viewSize ajaxFirstUrl="" firstUrl="" paginateFirstLabel="" paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying=commonDisplaying paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" />
    </#macro>
    <#assign paginated = true>
    
    <#if paginated>
      <@paginateParties />
    </#if>
    
    <table class="basic-table hover-bar" cellspacing="0">
     <thead>
      <tr class="header-row-2">
        <th>${uiLabelMap.PartyPartyId}</th>
        <th>${uiLabelMap.PartyUserLogin}</th>
        <th>${uiLabelMap.PartyName}</th>
    <#if (extInfo!"") == "P" >
        <th>${uiLabelMap.PartyCity}</th>
    </#if>
    <#if (extInfo!"") == "P">
        <th>${uiLabelMap.PartyPostalCode}</th>
    </#if>
    <#if (extInfo!"") == "T">
        <th>${uiLabelMap.PartyAreaCode}</th>
    </#if>
    <#if (inventoryItemId!"") != "">
        <th>${uiLabelMap.ProductInventoryItemId}</th>
    </#if>
    <#if (serialNumber!"") != "">
        <th>${uiLabelMap.ProductSerialNumber}</th>
    </#if>
    <#if (softIdentifier!"") != "">
        <th>${uiLabelMap.ProductSoftIdentifier}</th>
    </#if>
        <th>${uiLabelMap.PartyRelatedCompany}</th>
        <th>${uiLabelMap.PartyType}</th>
        <th>${uiLabelMap.PartyMainRole}</th>
        <th>
            <a href="<@ofbizUrl>findparty</@ofbizUrl>?<#if sortField?has_content><#if sortField == "createdDate">sortField=-createdDate<#elseif sortField == "-createdDate">sortField=createdDate<#else>sortField=createdDate</#if><#else>sortField=createdDate</#if>${paramList!}&VIEW_SIZE=${viewSize!}&VIEW_INDEX=${viewIndex!}" 
                <#if sortField?has_content><#if sortField == "createdDate">class="sort-order-desc"<#elseif sortField == "-createdDate">class="sort-order-asc"<#else>class="sort-order"</#if><#else>class="sort-order"</#if>>${uiLabelMap.FormFieldTitle_createdDate}
            </a>
        </th>
        <th>
            <a href="<@ofbizUrl>findparty</@ofbizUrl>?<#if sortField?has_content><#if sortField == "lastModifiedDate">sortField=-lastModifiedDate<#elseif sortField == "-lastModifiedDate">sortField=lastModifiedDate<#else>sortField=lastModifiedDate</#if><#else>sortField=lastModifiedDate</#if>${paramList!}&VIEW_SIZE=${viewSize!}&VIEW_INDEX=${viewIndex!}" 
                <#if sortField?has_content><#if sortField == "lastModifiedDate">class="sort-order-desc"<#elseif sortField == "-lastModifiedDate">class="sort-order-asc"<#else>class="sort-order"</#if><#else>class="sort-order"</#if>>${uiLabelMap.FormFieldTitle_lastModifiedDate}
            </a>
        </th>
        <th>&nbsp;</th>
      </tr>
    </thead>
    <#assign alt_row = false>
    <#assign rowCount = 0>
    <#list partyList as partyRow>
      <#assign partyType = partyRow.getRelatedOne("PartyType", false)!>
      <tr<@dataRowClassStr alt=alt_row />>
        <td><a href="<@ofbizUrl>viewprofile?partyId=${partyRow.partyId}</@ofbizUrl>">${partyRow.partyId}</a></td>
        <td>
      <#if partyRow.containsKey("userLoginId")>
          ${partyRow.userLoginId!"N/A"}
      <#else>
        <#assign userLogins = partyRow.getRelated("UserLogin", null, null, false)!>
        <#if userLogins?has_content && (userLogins.size() > 0)>
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
        </td>
        <td>
      <#if partyRow.getModelEntity().isField("lastName") && lastName?has_content>
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
        </td>
      <#if (extInfo!"") == "T">
        <td>${partyRow.areaCode!}</td>
      </#if>
      <#if (extInfo!"") == "P" >
        <td>${partyRow.city!}, ${partyRow.stateProvinceGeoId!}</td>
      </#if>
      <#if (extInfo!"") == "P">
        <td>${partyRow.postalCode!}</td>
      </#if>
      <#if (inventoryItemId!"") != "">
        <td>${partyRow.inventoryItemId!}</td>
      </#if>
      <#if (serialNumber!"") != "">
        <td>${partyRow.serialNumber!}</td>
      </#if>
      <#if (softIdentifier!"") != "">
        <td>${partyRow.softIdentifier!}</td>
      </#if>
      <#if partyType??>
        <td>
        <#if partyType.partyTypeId?has_content && partyType.partyTypeId=="PERSON">
          <#assign partyRelateCom = delegator.findByAnd("PartyRelationship", {"partyIdTo", partyRow.partyId,"roleTypeIdFrom","ACCOUNT","roleTypeIdTo","CONTACT"}, null, false)>
          <#if partyRelateCom?has_content>
            <#list partyRelateCom as partyRelationship>
              <#if partyRelationship.partyIdFrom?has_content>
                <#assign companyName=Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, partyRelationship.partyIdFrom, true)>
          ${companyName!}
              </#if>
            </#list>
          </#if>
        </#if>
        </td>
        <td><#if partyType.description??>${partyType.get("description", locale)}<#else>???</#if></td>
      <#else>
        <td></td><td></td>
      </#if>
        <td>
      <#assign mainRole = dispatcher.runSync("getPartyMainRole", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", partyRow.partyId, "userLogin", userLogin))/>
              ${mainRole.description!}
        </td>
        <#assign partyDate = delegator.findOne("Party", {"partyId":partyRow.partyId}, true)/>
        <td>${partyDate.createdDate!}</td>
        <td>${partyDate.lastModifiedDate!}</td>
        <td class="button-col">
          <ul class="button-group">
          <li><a href="<@ofbizUrl>viewprofile?partyId=${partyRow.partyId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDetails}</a></li>
      <#if security.hasEntityPermission("ORDERMGR", "_VIEW", session)>
          <li><form name="searchorders_o_${rowCount}" method="post" action="/ordermgr/control/searchorders">
            <input type="hidden" name="lookupFlag" value="Y" />
            <input type="hidden" name="hideFields" value="Y" />
            <input type="hidden" name="partyId" value="${partyRow.partyId}" />
            <input type="hidden" name="viewIndex" value="1" />
            <input type="hidden" name="viewSize" value="20" />
            <a href="javascript:document.searchorders_o_${rowCount}.submit()" class="${styles.button_default!}">${uiLabelMap.OrderOrders}</a>
          </form></li>
          <li><a href="/ordermgr/control/FindQuote?partyId=${partyRow.partyId + externalKeyParam}" class="${styles.button_default!}">${uiLabelMap.OrderOrderQuotes}</a></li>
      </#if>
      <#if security.hasEntityPermission("ORDERMGR", "_CREATE", session)>
          <li><a href="/ordermgr/control/checkinits?partyId=${partyRow.partyId + externalKeyParam}" class="${styles.button_default!}">${uiLabelMap.OrderNewOrder}</a></li>
          <li><a href="/ordermgr/control/EditQuote?partyId=${partyRow.partyId + externalKeyParam}" class="${styles.button_default!}">${uiLabelMap.OrderNewQuote}</a></li>
      </#if>
          </ul>
        </td>
      </tr>
      <#assign rowCount = rowCount + 1>
      <#-- toggle the row color -->
      <#assign alt_row = !alt_row>
    </#list>
    </table>
    
    <#if paginated>
      <@paginateParties />
    </#if>
    
  <#else>
    <p>${uiLabelMap.PartyNoPartiesFound}</p>
  </#if>
  
      </@cell>
    </@row>
  </@section>
</#if>


