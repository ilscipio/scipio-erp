<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<div id="shipToServerError" class="errorMessage"></div>
<form id="editShipToPostalAddress" method="post" action="">
  <fieldset>
    <input type="hidden" name="setShippingPurpose" value="Y" />
    <input type="hidden" name="contactMechId" value="${shipToContactMechId!}" />
    <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
    <input type="hidden" name="productStoreId" value="${productStoreId!}" />
    <div>
      <label for="shipToAddress1">${uiLabelMap.PartyAddressLine1}*</label>
      <input type="text" class="required" name="address1" id="shipToAddress1" value="${shipToAddress1!}" maxlength="30" />
      <span id="advice-required-shipToAddress1" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
    </div>
    <div>
      <label for="shipToAddress2">${uiLabelMap.PartyAddressLine2}</label>
      <input type="text" name="address2" id="shipToAddress2" value="${shipToAddress2!}" maxlength="30" />
    </div>
    <div>
      <label for="shipToCity">${uiLabelMap.PartyCity}*</label>
      <input type="text" class="required" name="city" id="shipToCity" value="${shipToCity!}" maxlength="30" />
      <span id="advice-required-shipToCity" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
    </div>
    <div>
      <label for="shipToPostalCode">${uiLabelMap.PartyZipCode}*</label>
      <input type="text" class="required" name="postalCode" id="shipToPostalCode" value="${shipToPostalCode!}" maxlength="10" />
      <span id="advice-required-shipToPostalCode" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
    </div>
    <div>
      <label for="shipToCountryGeoId">${uiLabelMap.CommonCountry}*</label>
      <select name="countryGeoId" id="shipToCountryGeoId" class="required">
        <#if shipToCountryGeoId??>
          <option value="${shipToCountryGeoId!}">${shipToCountryProvinceGeo!(shipToCountryGeoId!)}</option>
        </#if>
        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"countriesPreselect":!shipToCountryGeoId??}/>
      </select>
      <span id="advice-required-shipToCountryGeoId" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
    </div>
    <div id="shipToStates">
      <label for="shipToStateProvinceGeoId">${uiLabelMap.PartyState}*<span id="advice-required-shipToStateProvinceGeoId" style="display: none" class="errorMessage">(${uiLabelMap.CommonRequired})</span></label>
      <select name="stateProvinceGeoId" id="shipToStateProvinceGeoId">
        <#if shipToStateProvinceGeoId?has_content>
          <option value="${shipToStateProvinceGeoId!}">${shipToStateProvinceGeo!(shipToStateProvinceGeoId!)}</option>
        <#else>
          <option value="_NA_">${uiLabelMap.PartyNoState}</option>
        </#if>
      </select>
    </div>
    <#if shipToTelecomNumber?has_content>
      <div>
        <label>${uiLabelMap.PartyPhoneNumber}*</label>
        <span id="advice-required-shipToCountryCode" style="display:none" class="errorMessage"></span>
        <span id="advice-required-shipToAreaCode" style="display:none" class="errorMessage"></span>
        <span id="advice-required-shipToContactNumber" style="display:none" class="errorMessage"></span>
        <span id="shipToPhoneRequired" style="display: none;" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
        <input type="hidden" name="phoneContactMechId" value="${shipToTelecomNumber.contactMechId!}" />
        <input type="text" name="countryCode" id="shipToCountryCode" class="required" value="${shipToTelecomNumber.countryCode!}" size="3" maxlength="3" />
        - <input type="text" name="areaCode" id="shipToAreaCode" class="required" value="${shipToTelecomNumber.areaCode!}" size="3" maxlength="3" />
        - <input type="text" name="contactNumber" id="shipToContactNumber" class="required" value="${contactNumber?default("${shipToTelecomNumber.contactNumber!}")}" size="6" maxlength="7" />
        - <input type="text" name="extension" value="${extension?default("${shipToExtension!}")}" size="3" maxlength="3" />
      </div>
    </#if>
    <div class="inline">
      <label for="setBillingPurposeForShipping">${uiLabelMap.EcommerceMyDefaultBillingAddress}</label>
      <input type="checkbox" name="setBillingPurpose" id="setBillingPurposeForShipping" value="Y" <#if setBillingPurpose??>checked="checked"</#if> />
    </div>
    <#--
    <div>
      <a name="submitEditShipToPostalAddress" id="submitEditShipToPostalAddress" class="${styles.link_run_sys!} ${styles.action_update!}" onclick="updatePartyShipToPostalAddress('submitEditShipToPostalAddress')">${uiLabelMap.CommonSubmit}</a>
      <a class="popup_closebox ${styles.link_nav_cancel!}">${uiLabelMap.CommonClose}</a>
    </div>
    -->
  </fieldset>
</form>