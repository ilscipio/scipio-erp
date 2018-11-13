<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>
  <@section>
    <form name="checkoutsetupform" method="post" action="<@ofbizUrl>createCustomer</@ofbizUrl>">
      <input type="hidden" name="finalizeMode" value="cust" />
      <input type="hidden" name="finalizeReqNewShipAddress" value="true" />

      <@field type="input" label=uiLabelMap.CommonTitle name="personalTitle" value=(requestParameters.personalTitle!) size="10" maxlength="30" />
      <@field type="input" required=true label=uiLabelMap.PartyFirstName name="firstName" value=(requestParameters.firstName!) size="30" maxlength="30"/>
      <@field type="input" label=uiLabelMap.PartyMiddleInitial name="middleName" value=(requestParameters.middleName!) size="4" maxlength="4"/>
      <@field type="input" required=true label=uiLabelMap.PartyLastName name="lastName" value=(requestParameters.lastName!) size="30" maxlength="30"/>
      <@field type="input" label=uiLabelMap.PartySuffix name="suffix" value=(requestParameters.suffix!) size="10" maxlength="30"/>
 
      <@field type="generic" label=uiLabelMap.PartyHomePhone>
          <@field type="input" inline=true name="homeCountryCode" value=(requestParameters.homeCountryCode!) size="4" maxlength="10"/>
          -&nbsp;<@field type="input" inline=true required=true name="homeAreaCode" value=(requestParameters.homeAreaCode!) size="4" maxlength="10"/>
          -&nbsp;<@field type="input" inline=true required=true name="homeContactNumber" value=(requestParameters.homeContactNumber!) size="15" maxlength="15"/>
          &nbsp;ext&nbsp;<@field type="input" inline=true name="homeExt" value=(requestParameters.homeExt!) size="6" maxlength="10"/>

          <@field type="select" name="homeSol" label=uiLabelMap.OrderAllowSolicitation>
            <#if (((requestParameters.homeSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
            <#if (((requestParameters.homeSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
            <option></option>
            <option value="Y">${uiLabelMap.CommonY}</option>
            <option value="N">${uiLabelMap.CommonN}</option>
          </@field>
      </@field>
      <@field type="generic" label=uiLabelMap.PartyBusinessPhone>
          <@field type="input" inline=true name="workCountryCode" value=(requestParameters.CUSTOMER_WORK_COUNTRY!) size="4" maxlength="10"/>
          -&nbsp;<@field type="input" inline=true name="workAreaCode" value=(requestParameters.CUSTOMER_WORK_AREA!) size="4" maxlength="10"/>
          -&nbsp;<@field type="input" inline=true name="workContactNumber" value=(requestParameters.CUSTOMER_WORK_CONTACT!) size="15" maxlength="15"/>
          &nbsp;ext&nbsp;<@field type="input" inline=true name="workExt" value=(requestParameters.CUSTOMER_WORK_EXT!) size="6" maxlength="10"/>

          <@field type="select" name="workSol" label=uiLabelMap.OrderAllowSolicitation>
            <#if (((requestParameters.workSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
            <#if (((requestParameters.workSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
            <option></option>
            <option value="Y">${uiLabelMap.CommonY}</option>
            <option value="N">${uiLabelMap.CommonN}</option>
          </@field>
      </@field>

      <@field type="generic" label=uiLabelMap.PartyEmailAddress>
          <@field type="input"  name="emailAddress" value="" size="60" maxlength="255" />
  
          <@field type="select" name="emailSol" label=uiLabelMap.OrderAllowSolicitation>
            <#if (((requestParameters.emailSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
            <#if (((requestParameters.emailSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
            <option></option>
            <option value="Y">${uiLabelMap.CommonY}</option>
            <option value="N">${uiLabelMap.CommonN}</option>
          </@field>
      </@field>
   
      <@field type="input" label=uiLabelMap.CommonUsername name="userLoginId" value=(requestParameters.USERNAME!) size="20" maxlength="250"/>
    </form>
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
