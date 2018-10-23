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

<@section relHeadingLevel=+1>
  <#assign shoppingCart = sessionAttributes.shoppingCart! />

    <#if shoppingCart?has_content && (shoppingCart.size() > 0)>
    
      <#-- SCIPIO: NOTE: Not all divs below need to be @sections, only those that had "screenlet" -->
    
      <div id="checkoutPanel">

<#-- ========================================================================================================================== -->

        <@section id="cartPanel">
          <@render resource="component://shop/widget/CartScreens.xml#UpdateCart" />
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="shippingPanel" title="${rawLabel('EcommerceStep')} 2: ${rawLabel('FacilityShipping')}">
          <div id="shippingSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openShippingPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <@panel id="shippingCompleted">
              <@row>
                <@cell columns=6>
                  <@heading>${uiLabelMap.OrderShipTo}</@heading>
                  <ul class="text-line-list">
                    <li id="completedShipToAttn"></li>
                    <li id="completedShippingContactNumber"></li>
                    <li id="completedEmailAddress"></li>
                  </ul>
                </@cell>
                <@cell columns=6>
                  <@heading>${uiLabelMap.EcommerceLocation}</@heading>
                  <ul class="text-line-list">
                    <li id="completedShipToAddress1"></li>
                    <li id="completedShipToAddress2"></li>
                    <li id="completedShipToGeo"></li>
                  </ul>
                </@cell>
              </@row>
            </@panel>
          </div>

<#-- ============================================================= -->
          <div id="editShippingPanel" style="display: none;">
            <form id="shippingForm" action="<@ofbizUrl>createUpdateShippingAddress</@ofbizUrl>" method="post">
                <fieldset>
                  <input type="hidden" id="shipToContactMechId" name="shipToContactMechId" value="${shipToContactMechId!}" />
                  <input type="hidden" id="billToContactMechIdInShipingForm" name="billToContactMechId" value="${billToContactMechId!}" />
                  <input type="hidden" id="shipToPartyId" name="partyId" value="${partyId!}" />
                  <input type="hidden" id="shipToPhoneContactMechId" name="shipToPhoneContactMechId" value="${(shipToTelecomNumber.contactMechId)!}" />
                  <input type="hidden" id="emailContactMechId" name="emailContactMechId" value="${emailContactMechId!}" />
                  <input type="hidden" name="shipToName" value="${shipToName!}" />
                  <input type="hidden" name="shipToAttnName" value="${shipToAttnName!}" />
                  <#if userHasAccount>
                    <input type="hidden" name="keepAddressBook" value="Y" />
                    <input type="hidden" name="setDefaultShipping" value="Y" />
                    <input type="hidden" name="userLoginId" id="userLoginId" value="${userLogin.userLoginId!}" />
                    <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
                    <input type="hidden" name="productStoreId" value="${productStoreId!}" />
                  <#else>
                    <input type="hidden" name="keepAddressBook" value="N" />
                  </#if>
                  <@alert type="error" containerId="shippingFormServerError_container" containerStyle="display:none;">
                    <div id="shippingFormServerError" class="errorMessage"></div>
                  </@alert>
                
                  <@field type="input" label=uiLabelMap.PartyFirstName id="firstName" name="firstName" required=true value=(firstName!) />
                  <@field type="input" label=uiLabelMap.PartyLastName id="lastName" name="lastName" required=true value=(lastName!) />
                  <@field type="generic" label=uiLabelMap.PartyContactNumber required=true>
                    <#if shipToTelecomNumber?has_content>
                      <@field type="input" inline=true name="shipToCountryCode" required=true id="shipToCountryCode" value=(shipToTelecomNumber.countryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                      <@field type="input" inline=true name="shipToAreaCode" required=true id="shipToAreaCode" value=(shipToTelecomNumber.areaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                      <@field type="input" inline=true name="shipToContactNumber" required=true id="shipToContactNumber" value=(shipToTelecomNumber.contactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                      <@field type="input" inline=true name="shipToExtension" id="shipToExtension" value=(shipToExtension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                    <#elseif phoneContactMechId?has_content><#-- SCIPIO: fallback to primary phone -->
                      <@field type="input" inline=true name="shipToCountryCode" required=true id="shipToCountryCode" value=(countryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                      <@field type="input" inline=true name="shipToAreaCode" required=true id="shipToAreaCode" value=(areaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                      <@field type="input" inline=true name="shipToContactNumber" required=true id="shipToContactNumber" value=(contactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                      <@field type="input" inline=true name="shipToExtension" id="shipToExtension" value=(extension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                    <#else>
                      <@field type="input" inline=true name="shipToCountryCode" required=true id="shipToCountryCode" value=(parameters.shipToCountryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                      <@field type="input" inline=true name="shipToAreaCode" required=true id="shipToAreaCode" value=(parameters.shipToAreaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                      <@field type="input" inline=true name="shipToContactNumber" required=true id="shipToContactNumber" value=(parameters.shipToContactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                      <@field type="input" inline=true name="shipToExtension" id="shipToExtension" value=(parameters.shipToExtension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                    </#if>
                  </@field>
                  <@field type="input" id="emailAddress" name="emailAddress" required=true class="+validate-email" maxlength="255" size="40" value=(emailAddress!) label=uiLabelMap.PartyEmailAddress/>
                  <@field type="input" id="shipToAddress1" name="shipToAddress1" required=true value=(shipToAddress1!) maxlength="255" size="40" label=uiLabelMap.PartyAddressLine1/>
                  <@field type="input" id="shipToAddress2" name="shipToAddress2" value=(shipToAddress2!) maxlength="255" size="40" label=uiLabelMap.PartyAddressLine2/>
                  <@field type="input" id="shipToCity" name="shipToCity" required=true value=(shipToCity!) maxlength="255" size="40" label=uiLabelMap.CommonCity/>
                  <@field type="input" id="shipToPostalCode" name="shipToPostalCode" required=true value=(shipToPostalCode!) size="12" maxlength="10" label=uiLabelMap.PartyZipCode/>

                  <@field type="select" name="shipToCountryGeoId" id="shipToCountryGeoId" required=true label=uiLabelMap.CommonCountry>
                    <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"currentCountryGeoId":shipToCountryGeoId!""} />
                  </@field>
                  <div id="shipToStates">
                    <@field type="select" id="shipToStateProvinceGeoId" required=true name="shipToStateProvinceGeoId" label=uiLabelMap.CommonState>
                      <#if shipToStateProvinceGeoId?has_content>
                        <option value="${shipToStateProvinceGeoId!}">${shipToStateProvinceGeo!(shipToStateProvinceGeoId!)}</option>
                      <#else>
                        <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                      </#if>
                      <#--
                      State prefill removed for now as it list too many entries
                      <@render resource="component://common/widget/CommonScreens.xml#states" />-->
                    </@field>
                  </div>
                </fieldset>
                <#--<fieldset>-->
                  <@field type="submitarea">
                    <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePartyAndShippingContact" text="${rawLabel('EcommerceContinueToStep')} 3"/>
                    <@field type="submit" submitType="link" class="${styles.link_run_session!}" style="display: none;" href="javascript:void(0);" id="processingShippingOptions" text="${rawLabel('EcommercePleaseWait')}..."/>
                  </@field>
                <#--</fieldset>-->
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="shippingOptionPanel" title="${rawLabel('EcommerceStep')} 3: ${rawLabel('PageTitleShippingOptions')}">
          <div id="shippingOptionSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openShippingOptionPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <@panel class="+completed" id="shippingOptionCompleted">
              <@heading>${uiLabelMap.OrderShippingMethod}</@heading>
              <ul class="text-line-list">
                <li id="selectedShipmentOption"></li>
              </ul>
            </@panel>
          </div>

<#-- ============================================================= -->
          <div id="editShippingOptionPanel" style="display: none;">
            <form id="shippingOptionForm" action="<@ofbizUrl></@ofbizUrl>" method="post">
              <fieldset>
                <@alert type="error" containerId="shippingOptionFormServerError_container" containerStyle="display:none;">
                  <div id="shippingOptionFormServerError" class="errorMessage"></div>
                </@alert>
                <@field type="select" id="shipMethod" required=true name="shipMethod" required=true label=uiLabelMap.OrderSelectShippingMethod>
                  <option value=""></option>
                </@field>
              </fieldset>
              <#--<fieldset>-->
                <@field type="submitarea">
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="saveShippingMethod" text="${rawLabel('EcommerceContinueToStep')} 4"/>
                  <@field type="submit" submitType="link" style="display:none" class="${styles.link_run_session!}" style="display: none;" href="javascript:void(0);" id="processingBilling" text="${rawLabel('EcommercePleaseWait')}..."/>
                </@field>
              <#--</fieldset>-->
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="billingPanel" title="${rawLabel('EcommerceStep')} 4: ${rawLabel('AccountingBilling')}">
          <div id="billingSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openBillingPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <@panel class="+completed" id="billingCompleted">
              <@row>
                <@cell columns=6>
                  <@heading>${uiLabelMap.OrderBillUpTo}</@heading>
                  <ul class="text-line-list">
                    <li id="completedBillToAttn"></li>
                    <li id="completedBillToPhoneNumber"></li>
                    <li id="paymentMethod"></li>
                    <li id="completedCCNumber"></li>
                    <li id="completedExpiryDate"></li>
                  </ul>
                </@cell>
                <@cell columns=6>
                  <@heading>${uiLabelMap.EcommerceLocation}</@heading>
                  <ul class="text-line-list">
                    <li id="completedBillToAddress1"></li>
                    <li id="completedBillToAddress2"></li>
                    <li id="completedBillToGeo"></li>
                  </ul>
                </@cell>
              </@row>
            </@panel>
          </div>

<#-- ============================================================= -->

          <div id="editBillingPanel" style="display: none;">
            <form id="billingForm" class="theform" action="<@ofbizUrl></@ofbizUrl>" method="post">
              <fieldset class="col">
                <input type="hidden" id="billToContactMechId" name="billToContactMechId" value="${billToContactMechId!}" />
                <input type="hidden" id="shipToContactMechIdInBillingForm" name="shipToContactMechId" value="${shipToContactMechId!}" />
                <input type="hidden" id="paymentMethodId" name="paymentMethodId" value="${paymentMethodId!}" />
                <input type="hidden" id="paymentMethodTypeId" name="paymentMethodTypeId" value="${paymentMethodTypeId?default("CREDIT_CARD")}" />
                <input type="hidden" id="billToPartyId" name="partyId" value="${parameters.partyId!}" />
                <input type="hidden" name="expireDate" value="${expireDate!}" />
                <input type="hidden" id="billToPhoneContactMechId" name="billToPhoneContactMechId" value="${(billToTelecomNumber.contactMechId)!}" />
                <input type="hidden" name="billToName" value="${billToName!}" />
                <input type="hidden" name="billToAttnName" value="${billToAttnName!}" />
                <#if userHasAccount>
                  <input type="hidden" name="keepAddressBook" value="Y" />
                  <input type="hidden" name="setDefaultBilling" value="Y" />
                  <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
                  <input type="hidden" name="productStoreId" value="${productStoreId!}" />
                <#else>
                  <input type="hidden" name="keepAddressBook" value="N" />
                </#if>
                <@alert type="error" containerId="billingFormServerError_container" containerStyle="display:none;">
                  <div id="billingFormServerError" class="errorMessage"></div>
                </@alert>
                
                <#-- SCIPIO: default is now Y if billToContactMechId was not set on page load. Only case we don't is if there was an initial billing contact mech different from ship contact mech.
                    old: checked=((useShippingAddressForBilling!"")=="Y") 
                    NOTE: The values will be loaded by Javascript, which we can get away with and need to do anyway because the user may have changed
                        the ship address since this page was loaded, so no point in doing statically 
                    NOTE: The JS re-implements this logic its own way -->
                <#assign useShipAddrForBillingBool = (((useShippingAddressForBilling!"")=="Y") || !billToContactMechId?has_content)>

                <@field type="checkbox" id="useShippingAddressForBilling" name="useShippingAddressForBilling" type="checkbox" value="Y" checked=useShipAddrForBillingBool label=uiLabelMap.FacilityBillingAddressSameShipping />
                
                <@field type="input" id="firstNameOnCard" name="firstNameOnCard" required=true value=(firstNameOnCard!) label=uiLabelMap.PartyFirstName/>
                <@field type="input" id="lastNameOnCard" name="lastNameOnCard" required=true value=(lastNameOnCard!) label=uiLabelMap.PartyLastName/>
                <@field type="generic" label=uiLabelMap.PartyContactNumber required=true>  
                  <#if billToTelecomNumber?has_content>
                    <@field type="input" inline=true name="billToCountryCode" required=true id="billToCountryCode" value=(billToTelecomNumber.countryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                    <@field type="input" inline=true name="billToAreaCode" required=true id="billToAreaCode" value=(billToTelecomNumber.areaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                    <@field type="input" inline=true name="billToContactNumber" required=true id="billToContactNumber" value=(billToTelecomNumber.contactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                    <@field type="input" inline=true name="billToExtension" id="billToExtension" value=(billToExtension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                  <#elseif phoneContactMechId?has_content><#-- SCIPIO: fallback on primary phone -->
                    <@field type="input" inline=true name="billToCountryCode" required=true id="billToCountryCode" value=(countryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                    <@field type="input" inline=true name="billToAreaCode" required=true id="billToAreaCode" value=(areaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                    <@field type="input" inline=true name="billToContactNumber" required=true id="billToContactNumber" value=(contactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                    <@field type="input" inline=true name="billToExtension" id="billToExtension" value=(extension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                  <#else>
                    <@field type="input" inline=true name="billToCountryCode" required=true id="billToCountryCode" value=(parameters.billToCountryCode!) size="5" maxlength="10" label=uiLabelMap.CommonCountry/> -
                    <@field type="input" inline=true name="billToAreaCode" required=true id="billToAreaCode" value=(parameters.billToAreaCode!) size="5" maxlength="10" label=uiLabelMap.PartyAreaCode/> -
                    <@field type="input" inline=true name="billToContactNumber" required=true id="billToContactNumber" value=(parameters.billToContactNumber!) size="10" maxlength="15" label=uiLabelMap.PartyContactNumber/> -
                    <@field type="input" inline=true name="billToExtension" id="billToExtension" value=(parameters.billToExtension!) size="5" maxlength="10" label=uiLabelMap.PartyExtension/>
                  </#if>
                </@field>
                <@field type="select" name="cardType" id="cardType" required=true label=uiLabelMap.AccountingCardType>
                    <#if cardType?has_content>
                      <option label="${cardType!}" value="${cardType!}">${cardType!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#cctypes" />
                </@field>

                <@field type="input" id="cardNumber" name="cardNumber" required=true class="+creditcard" value=(cardNumber!) size="30" maxlength="16" label=uiLabelMap.AccountingCardNumber/>
                <@field type="input" id="billToCardSecurityCode" name="billToCardSecurityCode" size="4" maxlength="4" value="" label="CVV2"/>
                <@field type="select" id="expMonth" name="expMonth" required=true label=uiLabelMap.CommonMonth>
                    <#if expMonth?has_content>
                      <option label="${expMonth!}" value="${expMonth!}">${expMonth!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccmonths" />
                </@field>
                <@field type="select" id="expYear" name="expYear" required=true label=uiLabelMap.CommonYear>
                    <#if expYear?has_content>
                      <option value="${expYear!}">${expYear!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccyears" />
                </@field>
              </fieldset>
              <fieldset class="col">  
                <div id="billingAddress"<#if useShipAddrForBillingBool> style="display:none"</#if>>
                  <@field type="input" id="billToAddress1" name="billToAddress1" required=true size="30" value=(billToAddress1!) label=uiLabelMap.PartyAddressLine1/>
                  <@field type="input" id="billToAddress2" name="billToAddress2" value=(billToAddress2!) size="30" label=uiLabelMap.PartyAddressLine2/>
                  <@field type="input" id="billToCity" name="billToCity" required=true value=(billToCity!) label=uiLabelMap.CommonCity/>
                  <@field type="input" id="billToPostalCode" name="billToPostalCode" required=true value=(billToPostalCode!) size="12" maxlength="10" label=uiLabelMap.PartyZipCode/>
                  <@field type="select" name="billToCountryGeoId" required=true id="billToCountryGeoId" label=uiLabelMap.CommonCountry>
                    <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"currentCountryGeoId":billToCountryGeoId!""} />
                  </@field>
                  <@field type="select" id="billToStateProvinceGeoId" name="billToStateProvinceGeoId" required=true label=uiLabelMap.CommonState>
                    <#if billToStateProvinceGeoId?has_content>
                      <option value="${billToStateProvinceGeoId!}">${billToStateProvinceGeo!(billToStateProvinceGeoId!)}</option>
                    <#else>
                      <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                    </#if>
                  </@field>
                </div>
              </fieldset>
              <#--<fieldset>-->
                <@field type="submitarea">
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePaymentAndBillingContact" text="${rawLabel('EcommerceContinueToStep')} 5"/>
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!}" style="display: none;" id="processingOrderSubmitPanel" text="${rawLabel('EcommercePleaseWait')}..."/>
                </@field>
              <#--</fieldset>-->
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section title="${rawLabel('EcommerceStep')} 5: ${rawLabel('OrderSubmitOrder')}">
          <div id="orderSubmitPanel" style="display: none;">
            <form id="orderSubmitForm" action="<@ofbizUrl>onePageProcessOrder</@ofbizUrl>" method="post">
                <#--<fieldset>-->
                  <@field type="submitarea">
                    <@field type="submit" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" id="processOrderButton" name="processOrderButton" text=uiLabelMap.OrderSubmitOrder />
                    <@field type="submit" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" style="display: none;" id="processingOrderButton" name="processingOrderButton" text=uiLabelMap.OrderSubmittingOrder />
                  </@field>
                <#--</fieldset>-->
            </form>
          </div>
        </@section>
      </div>
    </#if>

<#-- ========================================================================================================================== -->
    
    <#assign sectionStyle><#if shoppingCart?has_content && (shoppingCart.size() > 0)>display: none;</#if></#assign>
    <@section id="emptyCartCheckoutPanel" style=sectionStyle>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 1: ${uiLabelMap.PageTitleShoppingCart}</@heading>
        <p>You currently have no items in your cart. Click <a href="<@ofbizUrl>main</@ofbizUrl>">here</a> to view our products.</p>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 2: ${uiLabelMap.FacilityShipping}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 3: ${uiLabelMap.PageTitleShippingOptions}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 4: ${uiLabelMap.AccountingBilling}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 5: ${uiLabelMap.OrderSubmitOrder}</@heading>
    </@section>
    
</@section>
