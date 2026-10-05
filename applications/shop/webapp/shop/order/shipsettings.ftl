<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: Must use context or accessor
<#assign cart = sessionAttributes.shoppingCart!>-->
<#assign cart = getShoppingCart()!>

<@section>
  <div id="shippingFormServerError" class="errorMessage"></div>
  <form id="editShippingContact" method="post" action="<@pageUrl>processShipSettings</@pageUrl>" name="${parameters.formNameValue}">

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

