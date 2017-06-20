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

<#assign cart = sessionAttributes.shoppingCart!>

<@section>
  <div id="shippingFormServerError" class="errorMessage"></div>
  <form id="editShippingContact" method="post" action="<@ofbizUrl>processShipSettings</@ofbizUrl>" name="${parameters.formNameValue}">

      <input type="hidden" name="shippingContactMechId" value="${parameters.shippingContactMechId!}"/>
      <input type="hidden" name="partyId" value="${cart.getPartyId()!"_NA_"}"/>
      
      <@field type="input" id="address1" name="address1" required=true value=(address1!) label=uiLabelMap.PartyAddressLine1/>
      <@field type="input" id="address2" name="address2" value=(address2!) label=uiLabelMap.PartyAddressLine2/>
      <@field type="input" id="city" name="city" required=true value=(city!) label=uiLabelMap.CommonCity/>
      <@field type="input" id="postalCode" name="postalCode" required=true value=(postalCode!) size="12" maxlength="10" label=uiLabelMap.PartyZipCode/>

      <@field type="select" name="countryGeoId" id="countryGeoId" required=true label=uiLabelMap.CommonCountry>
        <#if countryGeoId??>
          <option value="${countryGeoId!}">${countryProvinceGeo!(countryGeoId!)}</option>
        </#if>
        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!(countryGeoId??)}/>
      </@field>
   
      <@field type="select" id="stateProvinceGeoId" name="stateProvinceGeoId" label=uiLabelMap.CommonState required=true>
        <#if stateProvinceGeoId?has_content>
          <option value="${stateProvinceGeoId!}">${stateProvinceGeo!(stateProvinceGeoId!)}</option>
        <#else>
          <option value="_NA_">${uiLabelMap.PartyNoState}</option>
        </#if>
        <#--<@render resource="component://common/widget/CommonScreens.xml#states" />-->
      </@field>
    <#--
      <@field type="submit" class="${styles.link_run_session!} ${styles.action_update!}" value=(uiLabelMap.CommonContinue)/>
    -->
  </form>
</@section>

<@checkoutActionsMenu directLinks=true />

