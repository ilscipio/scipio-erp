<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<div id="serverError_${contactMech.contactMechId}" class="errorMessage"></div>
<#assign postalAddress = delegator.findOne("PostalAddress", {"contactMechId":contactMech.contactMechId}, true) />

<form id="editPostalAddress_${contactMech.contactMechId}" method="post" action="">
  <fieldset>
    <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}" />
    <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
    <input type="hidden" name="productStoreId" value="${productStoreId!}" />
      <div>
        <label for="address1_${contactMech.contactMechId}">${uiLabelMap.PartyAddressLine1}*</label>
        <input type="text" class="required" name="address1" id="address1_${contactMech.contactMechId}" value="${postalAddress.address1!}" maxlength="30" />
        <span id="advice-required-address1_${contactMech.contactMechId}" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
      </div>
      <div>
        <label for="additionalAddress2_${contactMech.contactMechId}">${uiLabelMap.PartyAddressLine2}</label>
        <input type="text" name="address2" id="additionalAddress2_${contactMech.contactMechId}" value="${postalAddress.address2!}" maxlength="30" />
      </div>
      <div>
        <label for="city_${contactMech.contactMechId}">${uiLabelMap.PartyCity}*</label>
        <input type="text" class="required" name="city" id="city_${contactMech.contactMechId}" value="${postalAddress.city!}" maxlength="30" />
        <span id="advice-required-city_${contactMech.contactMechId}" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
      </div>
      <div>
        <label for="postalCode_${contactMech.contactMechId}">${uiLabelMap.PartyZipCode}*</label>
        <input type="text" class="required" name="postalCode" id="postalCode_${contactMech.contactMechId}" value="${postalAddress.postalCode!}" maxlength="10" />
        <span id="advice-required-postalCode_${contactMech.contactMechId}" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
      </div>
      <div>
        <label for="countryGeoId_${contactMech.contactMechId}">${uiLabelMap.CommonCountry}*</label>
        <select name="countryGeoId" id="countryGeoId_${contactMech.contactMechId}" class="required">
          <#if postalAddress.countryGeoId??>
            <#assign geo = delegator.findOne("Geo", {"geoId":postalAddress.countryGeoId}, true) />
            <option value="${postalAddress.countryGeoId}">${geo.geoName!(postalAddress.countryGeoId)}</option>
          </#if>
          <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!postalAddress.countryGeoId??} />
        </select>
        <span id="advice-required-countryGeoId_${contactMech.contactMechId}" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
      </div>
      <div id="states_${contactMech.contactMechId}">
        <label for="stateProvinceGeoId_${contactMech.contactMechId}">${uiLabelMap.PartyState}*</label>
        <select name="stateProvinceGeoId" id="stateProvinceGeoId_${contactMech.contactMechId}">
          <#if postalAddress.stateProvinceGeoId??>
            <#assign geo = delegator.findOne("Geo", {"geoId":postalAddress.stateProvinceGeoId}, true) />
            <option value="${postalAddress.stateProvinceGeoId}">${geo.geoName!(postalAddress.stateProvinceGeoId)}</option>
          <#else>
            <option value="_NA_">${uiLabelMap.PartyNoState}</option>
          </#if>
        </select>
        <span id="advice-required-stateProvinceGeoId_${contactMech.contactMechId}" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
      </div>
      <div class="inline">
        <label for="setBillingPurposeForPostalAddress">${uiLabelMap.EcommerceMyDefaultBillingAddress}</label>
        <input type="checkbox" name="setBillingPurpose" id="setBillingPurposeForPostalAddress" value="Y" <#if setBillingPurpose??>checked="checked"</#if> />
      </div>
      <div class="inline">
        <label for="setShippingPurposeForPostalAddress">${uiLabelMap.EcommerceMyDefaultShippingAddress}</label>
        <input type="checkbox" name="setShippingPurpose" id="setShippingPurposeForPostalAddress" value="Y" <#if setShippingPurpose??>checked="checked"</#if> />
      </div>
        <#--
      <div>
        <a name="submitEditPostalAddress_${contactMech.contactMechId}" id="submitEditPostalAddress_${contactMech.contactMechId}" class="${styles.link_run_sys!} ${styles.action_update!}" onclick="updatePartyPostalAddress('submitEditPostalAddress_${contactMech.contactMechId}')">${uiLabelMap.CommonSubmit}</a>
        <a href="javascript:void(0);" class="popup_closebox ${styles.link_nav_cancel!}">${uiLabelMap.CommonClose}</a>
      </div>
        -->
  </fieldset>
</form>