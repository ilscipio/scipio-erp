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

    <#-- reference number -->
    <#if txType?default("") == "PRDS_PAY_CREDIT" || txType?default("") == "PRDS_PAY_CAPTURE" || 
         txType?default("") == "PRDS_PAY_RELEASE" || txType?default("") == "PRDS_PAY_REFUND">
        ${setRequestAttribute("validTx", "true")}
        <#assign validTx = true>
        <hr />
        <@field type="input" size="30" maxlength="60" name="referenceNum" label="${uiLabelMap.AccountingReferenceNumber}" required=true tooltip="${uiLabelMap.CommonRequired}"/>
        <@field type="text" size="20" maxlength="20" name="orderPaymentPreferenceId" label="${uiLabelMap.FormFieldTitle_orderPaymentPreferenceId}" required=true tooltip="${uiLabelMap.CommonRequired}" />      
    </#if>
    <#-- manual credit card information -->
    <#if txType?default("") == "PRDS_PAY_RELEASE">      
      ${setRequestAttribute("validTx", "true")}
      <@script>
        document.manualTxForm.action = "<@ofbizUrl>processReleaseTransaction</@ofbizUrl>";
      </@script>      
    </#if>
    <#if txType?default("") == "PRDS_PAY_REFUND">      
      ${setRequestAttribute("validTx", "true")}
      <@script>
        document.manualTxForm.action = "<@ofbizUrl>processRefundTransaction</@ofbizUrl>";
      </@script>    
    </#if>
    <#if txType?default("") == "PRDS_PAY_CREDIT" || txType?default("") == "PRDS_PAY_AUTH">     
        ${setRequestAttribute("validTx", "true")}
        <@script>
            document.manualTxForm.action = "<@ofbizUrl>processManualCcTx</@ofbizUrl>";
        </@script>     
        <hr/>      
        <@field type="input" size="30" maxlength="60" name="firstName" value="${(person.firstName)!}" label="${uiLabelMap.PartyFirstName}" tooltip="${uiLabelMap.CommonRequired}" required=true />        
        <@field type="input" size="30" maxlength="60" name="lastName" value="${(person.lastName)!}" label="${uiLabelMap.PartyLastName}" tooltip="${uiLabelMap.CommonRequired}" required=true />   
        <@field type="input" size="30" maxlength="60" name="infoString" value="" label="${uiLabelMap.PartyEmailAddress}" tooltip="${uiLabelMap.CommonRequired}" required=true />       
        <hr/>
        ${screens.render("component://accounting/widget/CommonScreens.xml#creditCardFields")}      
        <hr/>      
        <#-- first / last name -->
        
        <#assign disabled = false />
        <#if requestParameters.useShipAddr??><#assign disabled = true /></#if>
        <@field type="input" size="30" maxlength="30" name="firstName" value="${(person.firstName)!}" disabled=disabled label="${uiLabelMap.PartyFirstName}" required=true tooltip="${uiLabelMap.CommonRequired}"/>
        <@field type="input" size="30" maxlength="30" name="lastName" value="${(person.lastName)!}" disabled=disabled label="${uiLabelMap.PartyLastName}" required=true tooltip="${uiLabelMap.CommonRequired}"/>         
      
        <#-- credit card address -->     
        <@field type="input" size="30" maxlength="30" name="address1" value="${(postalFields.address1)!}" label="${uiLabelMap.AccountingBillToAddress1}" disabled=disabled required=true tooltip="${uiLabelMap.CommonRequired}" />
        <@field type="input" size="30" maxlength="30" name="address2" value="${(postalFields.address2)!}" label="${uiLabelMap.AccountingBillToAddress2}" disabled=disabled required=true tooltip="${uiLabelMap.CommonRequired}" />
        <@field type="input" size="30" maxlength="30" name="city" value="${(postalFields.city)!}" label="${uiLabelMap.CommonCity}" disabled=disabled required=true tooltip="${uiLabelMap.CommonRequired}" />
        <@field type="select" name="stateProvinceGeoId" label="${uiLabelMap.CommonStateProvince}" disabled=disabled required=true>
            <#if (postalFields.stateProvinceGeoId)??>
              <option>${postalFields.stateProvinceGeoId}</option>
              <option value="${postalFields.stateProvinceGeoId}">---</option>
            <#else>
              <option value="">${uiLabelMap.CommonNone} ${uiLabelMap.CommonState}</option>
            </#if>
            ${screens.render("component://common/widget/CommonScreens.xml#states")}
        </@field>
        <@field type="input" size="12" maxlength="10" name="postalCode" value="${(postalFields.postalCode)!}" label="${uiLabelMap.CommonZipPostalCode}" disabled=disabled required=true tooltip="${uiLabelMap.CommonRequired}"/>
        <@field type="select" name="countryGeoId" disabled=disabled label="${uiLabelMap.CommonCountry}" required=true tooltip="${uiLabelMap.CommonRequired}">
            <#if postalFields.countryGeoId?has_content>
              <option>${postalFields.countryGeoId}</option>
              <option value="${postalFields.countryGeoId}">---</option>
            </#if>
            ${screens.render("component://common/widget/CommonScreens.xml#countries")}
        </@field>        
    </#if>