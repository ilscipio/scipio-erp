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
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#macro fieldErrors fieldName>
  <#if errorMessageList?has_content>
    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldName, true, errorMessageList)>
    <ul>
      <#list fieldMessages as errorMsg>
        <li class="errorMessage">${errorMsg}</li>
      </#list>
    </ul>
  </#if>
</#macro>
<#-- SCIPIO: invoker usable with @field pre/PostWidgetContent -->
<#macro fieldErrorsInvoker args={}>
  <@fieldErrors fieldName=args.fieldName />
</#macro>
<#macro fieldErrorsMulti fieldName1 fieldName2 fieldName3 fieldName4>
  <#if errorMessageList?has_content>
    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldName1, fieldName2, fieldName3, fieldName4, true, errorMessageList)>
    <ul>
      <#list fieldMessages as errorMsg>
        <li class="errorMessage">${errorMsg}</li>
      </#list>
    </ul>
  </#if>
</#macro>
<#-- SCIPIO: invoker usable with @field pre/PostWidgetContent -->
<#macro fieldErrorsMultiInvoker args={}>
  <@fieldErrorsMulti fieldName1=args.fieldName1 fieldName2=args.fieldName2 fieldName3=args.fieldName3 fieldName4=args.fieldName4 />
</#macro>

<@section title=uiLabelMap.PartyBasicInformation>
  <form name="${parameters.formNameValue}" id="quickAnonProcessCustomer" method="post" action="<@ofbizUrl>quickAnonProcessCustomerSettings</@ofbizUrl>">
    <input type="hidden" name="partyId" value="${parameters.partyId!}"/>
    <input type="hidden" name="shippingContactMechId" value="${parameters.shippingContactMechId!}"/>
    <input type="hidden" name="billingContactMechId" value="${parameters.billingContactMechId!}"/>
    <input type="hidden" name="shippingContactMechPurposeTypeId" value="${parameters.shippingContactMechPurposeTypeId!}"/>
    <input type="hidden" name="billingContactMechPurposeTypeId" value="${parameters.billingContactMechPurposeTypeId!}"/>

    <@row>
      <@cell columns=6 last=true>
        <@section title=uiLabelMap.PartyNameAndConactInfo>

          <@field type="input" label=uiLabelMap.PartyFirstName required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"firstName"}
              name="firstName" id="firstName" value=(parameters.firstName!) size="30" maxlength="30"/>

          <@field type="input" label=uiLabelMap.PartyMiddleInitial
              name="middleName" value=(parameters.middleName!) size="4" maxlength="4"/>
          <@field type="input" label=uiLabelMap.PartyLastName required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"lastName"}
              name="lastName" value=(parameters.lastName!) size="30" maxlength="30"/>
          <@field type="display">
              [${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyExtension}]
          </@field>
          <@field type="generic" label=uiLabelMap.PartyHomePhone required=true preWidgetContent=fieldErrorsMultiInvoker prePostContentArgs={"fieldName1":"homeCountryCode", "fieldName2":"homeAreaCode", "fieldName3":"homeContactNumber", "fieldName4":"homeExt"}>
                <input type="hidden" name="homePhoneContactMechId" value="${parameters.homePhoneContactMechId!}"/>
                <@field type="input" inline=true required=true name="homeCountryCode" value=(parameters.homeCountryCode!) size="4" maxlength="10"/>
                -&nbsp;<@field type="input" inline=true required=true name="homeAreaCode" value=(parameters.homeAreaCode!) size="4" maxlength="10"/>
                -&nbsp;<@field type="input" inline=true required=true name="homeContactNumber" value=(parameters.homeContactNumber!) size="15" maxlength="15"/>
                -&nbsp;<@field type="input" inline=true name="homeExt" value=(parameters.homeExt!) size="6" maxlength="10"/>
          </@field>
          <@field type="generic" label=uiLabelMap.PartyBusinessPhone>
              <input type="hidden" name="workPhoneContactMechId" value="${parameters.workPhoneContactMechId!}"/>
              <@field type="input" inline=true name="workCountryCode" value=(parameters.workCountryCode!) size="4" maxlength="10"/>
              -&nbsp;<@field type="input" inline=true name="workAreaCode" value=(parameters.workAreaCode!) size="4" maxlength="10"/>
              -&nbsp;<@field type="input" inline=true name="workContactNumber" value=(parameters.workContactNumber!) size="15" maxlength="15"/>
              -&nbsp;<@field type="input" inline=true name="workExt" value=(parameters.workExt!) size="6" maxlength="10"/>
          </@field>
          <input type="hidden" name="emailContactMechId" value="${parameters.emailContactMechId!}"/>
          <@field type="input" label=uiLabelMap.PartyEmailAddress required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"emailAddress"}
              class="+validate-email" name="emailAddress" value=(parameters.emailAddress!) size="40" maxlength="255"/>
        </@section>
      </@cell>
    </@row>

    <hr />
    
    <@row>
      <@cell columns=6>
        <@section title=uiLabelMap.OrderShippingAddress>
     
              <@field type="input" label=uiLabelMap.PartyToName preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToName"}
                  name="shipToName" id="shipToName" value=(parameters.shipToName!) size="30" maxlength="30"/>
              <@field type="input" label=uiLabelMap.PartyAttentionName preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToAttnName"}
                  id="shipToAttnName" name="shipToAttnName" value=(parameters.shipToAttnName!) size="30" maxlength="30"/>
              <@field type="input" label=uiLabelMap.PartyAddressLine1 required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToAddress1"}
                  size="30" maxlength="30" id="shipToAddress1" name="shipToAddress1" value=(parameters.shipToAddress1!) />
              <@field type="input" label=uiLabelMap.PartyAddressLine2
                  size="30" maxlength="30" id="shipToAddress2" name="shipToAddress2" value=(parameters.shipToAddress2!) />
              <@field type="input" label=uiLabelMap.PartyCity required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToCity"}
                  size="30" maxlength="30" id="shipToCity" name="shipToCity" value=(parameters.shipToCity!) />
              <@field type="select" label=uiLabelMap.PartyState required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToStateProvinceGeoId"}
                  name="shipToStateProvinceGeoId" id="shipToStateProvinceGeoId">
                <#if (parameters.shipToStateProvinceGeoId)??>
                   <option>${parameters.shipToStateProvinceGeoId}</option>
                   <option value="${parameters.shipToStateProvinceGeoId}">---</option>
                <#else>
                   <option value="">${uiLabelMap.PartyNoState}</option>
                </#if>
                   <#--<@render resource="component://common/widget/CommonScreens.xml#states" />-->
              </@field>
              <@field type="input" label=uiLabelMap.PartyZipCode required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToPostalCode"}
                  size="12" maxlength="10" id="shipToPostalCode" name="shipToPostalCode" value=(parameters.shipToPostalCode!) />
              <@field type="select" label=uiLabelMap.CommonCountry required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"shipToCountryGeoId"}
                  name="shipToCountryGeoId" id="shipToCountryGeoId">
                <#if (parameters.shipToCountryGeoId)??>
                   <option>${parameters.shipToCountryGeoId}</option>
                   <option value="${parameters.shipToCountryGeoId}">---</option>
                </#if>
                   <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!((parameters.shipToCountryGeoId)??)}/>
              </@field>
        </@section>
      </@cell>
      <@cell columns=6>
        <@section title=uiLabelMap.PartyBillingAddress>
              <@field type="checkbox" id="useShippingPostalAddressForBilling" name="useShippingPostalAddressForBilling" value="Y" label=uiLabelMap.FacilityBillingAddressSameShipping/>
      
              <@field type="input" label=uiLabelMap.PartyToName preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToName"}
                  id="billToName" name="billToName" value=(parameters.billToName!) size="30" maxlength="30"/>
              <@field type="input" label=uiLabelMap.PartyAttentionName preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToAttnName"}
                  id="billToAttnName" name="billToAttnName" value=(parameters.billToAttnName!) size="30" maxlength="30"/>
              <@field type="input" label=uiLabelMap.PartyAddressLine1 required=true
                  id="billToAddress1" size="30" maxlength="30" name="billToAddress1" value=(parameters.billToAddress1!) />
              <@field type="input" label=uiLabelMap.PartyAddressLine2
                  id="billToAddress2" size="30" maxlength="30" name="billToAddress2" value=(parameters.billToAddress2!) />
              <@field type="input" label=uiLabelMap.PartyCity required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToCity"}
                  id="billToCity" size="30" maxlength="30" name="billToCity" value=(parameters.billToCity!) />
              <@field type="select" label=uiLabelMap.PartyState required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToStateProvinceGeoId"}
                  name="billToStateProvinceGeoId" id="billToStateProvinceGeoId">
                <#if (parameters.billToStateProvinceGeoId)??>
                   <option>${parameters.billToStateProvinceGeoId}</option>
                   <option value="${parameters.billToStateProvinceGeoId}">---</option>
                <#else>
                   <option value="">${uiLabelMap.PartyNoState}</option>
                </#if>
                   <#--<@render resource="component://common/widget/CommonScreens.xml#states" />-->
              </@field>
              <@field type="input" label=uiLabelMap.PartyZipCode required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToPostalCode"}
                  size="12" maxlength="10" id="billToPostalCode" name="billToPostalCode" value=(parameters.billToPostalCode!) />
              <@field type="select" label=uiLabelMap.CommonCountry required=true preWidgetContent=fieldErrorsInvoker prePostContentArgs={"fieldName":"billToCountryGeoId"}
                  name="billToCountryGeoId" id="billToCountryGeoId">
                <#if (parameters.billToCountryGeoId)??>
                   <option>${parameters.billToCountryGeoId}</option>
                   <option value="${parameters.billToCountryGeoId}">---</option>
                </#if>
                   <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!((parameters.billToCountryGeoId)??)}/>
              </@field>
        </@section>
      </@cell>
    </@row>

    <@field type="submit" class="${styles.link_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue/>

  </form>
</@section>
