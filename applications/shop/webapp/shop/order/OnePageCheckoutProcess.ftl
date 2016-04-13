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

<@section relHeadingLevel=+1>
  <#assign shoppingCart = sessionAttributes.shoppingCart! />

    <#if shoppingCart?has_content && (shoppingCart.size() > 0)>
    
      <#-- Cato: NOTE: Not all divs below need to be @sections, only those that had "screenlet" -->
    
      <div id="checkoutPanel">

<#-- ========================================================================================================================== -->

        <@section id="cartPanel">
          <@render resource="component://shop/widget/CartScreens.xml#UpdateCart" />
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="shippingPanel" title="${uiLabelMap.EcommerceStep} 2: ${uiLabelMap.FacilityShipping}">
          <div id="shippingSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openShippingPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <div id="shippingCompleted">
              <ul>
                <li>
                  <@heading>${uiLabelMap.OrderShipTo}</@heading>
                  <ul>
                    <li id="completedShipToAttn"></li>
                    <li id="completedShippingContactNumber"></li>
                    <li id="completedEmailAddress"></li>
                  </ul>
                </li>
                <li>
                  <@heading>${uiLabelMap.EcommerceLocation}</@heading>
                  <ul>
                    <li id="completedShipToAddress1"></li>
                    <li id="completedShipToAddress2"></li>
                    <li id="completedShipToGeo"></li>
                  </ul>
                </li>
              </ul>
            </div>
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
                  <#if userLogin??>
                    <input type="hidden" name="keepAddressBook" value="Y" />
                    <input type="hidden" name="setDefaultShipping" value="Y" />
                    <input type="hidden" name="userLoginId" id="userLoginId" value="${userLogin.userLoginId!}" />
                    <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
                    <input type="hidden" name="productStoreId" value="${productStoreId!}" />
                  <#else>
                    <input type="hidden" name="keepAddressBook" value="N" />
                  </#if>
                  <div id="shippingFormServerError" class="errorMessage"></div>
                  
                  <@field type="input" label=uiLabelMap.PartyFirstName id="firstName" name="firstName" required=true value=(firstName!) />
                  <@field type="input" label=uiLabelMap.PartyLastName id="lastName" name="lastName" required=true value=(lastName!) />
                  <@field type="generic" label=uiLabelMap.PartyContactNumber required=true>
                    <#if shipToTelecomNumber?has_content>
                      <@field type="input" inline=true name="shipToCountryCode" required=true id="shipToCountryCode" value=(shipToTelecomNumber.countryCode!) size="5" maxlength="10" label="${uiLabelMap.CommonCountry}"/> -
                      <@field type="input" inline=true name="shipToAreaCode" required=true id="shipToAreaCode" value=(shipToTelecomNumber.areaCode!) size="5" maxlength="10" label="${uiLabelMap.PartyAreaCode}"/> -
                      <@field type="input" inline=true name="shipToContactNumber" required=true id="shipToContactNumber" value=(shipToTelecomNumber.contactNumber!) size="10" maxlength="15" label="${uiLabelMap.PartyContactNumber}"/> -
                      <@field type="input" inline=true name="shipToExtension" id="shipToExtension" value=(shipToExtension!) size="5" maxlength="10" label="${uiLabelMap.PartyExtension}"/>
                    <#else>
                      <@field type="input" inline=true name="shipToCountryCode" required=true id="shipToCountryCode" value=(parameters.shipToCountryCode!) size="5" maxlength="10" label="${uiLabelMap.CommonCountry}"/> -
                      <@field type="input" inline=true name="shipToAreaCode" required=true id="shipToAreaCode" value=(parameters.shipToAreaCode!) size="5" maxlength="10" label="${uiLabelMap.PartyAreaCode}"/> -
                      <@field type="input" inline=true name="shipToContactNumber" required=true id="shipToContactNumber" value=(parameters.shipToContactNumber!) size="10" maxlength="15" label="${uiLabelMap.PartyContactNumber}"/> -
                      <@field type="input" inline=true name="shipToExtension" id="shipToExtension" value=(parameters.shipToExtension!) size="5" maxlength="10" label="${uiLabelMap.PartyExtension}"/>
                    </#if>
                  </@field>
                  <@field type="input" id="emailAddress" name="emailAddress" required=true class="+validate-email" maxlength="255" size="40" value=(emailAddress!) label="${uiLabelMap.PartyEmailAddress}"/>
                  <@field type="input" id="shipToAddress1" name="shipToAddress1" required=true value=(shipToAddress1!) maxlength="255" size="40" label="${uiLabelMap.PartyAddressLine1}"/>
                  <@field type="input" id="shipToAddress2" name="shipToAddress2" value=(shipToAddress2!) maxlength="255" size="40" label="${uiLabelMap.PartyAddressLine2}"/>
                  <@field type="input" id="shipToCity" name="shipToCity" required=true value=(shipToCity!) maxlength="255" size="40" label="${uiLabelMap.CommonCity}"/>
                  <@field type="input" id="shipToPostalCode" name="shipToPostalCode" required=true value=(shipToPostalCode!) size="12" maxlength="10" label="${uiLabelMap.PartyZipCode}"/>

                  <@field type="select" name="shipToCountryGeoId" id="shipToCountryGeoId" required=true label="${uiLabelMap.CommonCountry}">
                    <#if shipToCountryGeoId??>
                      <option value="${shipToCountryGeoId!}">${shipToCountryProvinceGeo?default(shipToCountryGeoId!)}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#countries" />
                  </@field>
                  <div id="shipToStates">
                    <@field type="select" id="shipToStateProvinceGeoId" required=true name="shipToStateProvinceGeoId" label="${uiLabelMap.CommonState}">
                      <#if shipToStateProvinceGeoId?has_content>
                        <option value="${shipToStateProvinceGeoId!}">${shipToStateProvinceGeo?default(shipToStateProvinceGeoId!)}</option>
                      <#else>
                        <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                      </#if>
                      <@render resource="component://common/widget/CommonScreens.xml#states" />
                    </@field>
                  </div>
                </fieldset>
                <fieldset>
                  <@field type="submitarea">
                    <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePartyAndShippingContact" text="${uiLabelMap.EcommerceContinueToStep} 3"/>
                    <@field type="submit" submitType="link" class="${styles.link_run_session!}" style="display: none;" href="javascript:void(0);" id="processingShippingOptions" text="${uiLabelMap.EcommercePleaseWait}..."/>
                  </@field>
                </fieldset>
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="shippingOptionPanel" title="${uiLabelMap.EcommerceStep} 3: ${uiLabelMap.PageTitleShippingOptions}">
          <div id="shippingOptionSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openShippingOptionPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <div class="completed" id="shippingOptionCompleted">
              ${uiLabelMap.CommonMethod}
              <ul>
                <li id="selectedShipmentOption"></li>
              </ul>
            </div>
          </div>

<#-- ============================================================= -->
          <div id="editShippingOptionPanel" style="display: none;">
            <form id="shippingOptionForm" action="<@ofbizUrl></@ofbizUrl>" method="post">
              <fieldset>
                <div id="shippingOptionFormServerError" class="errorMessage"></div>
                <@field type="select" id="shipMethod" required=true name="shipMethod" required=true label="${uiLabelMap.OrderSelectShippingMethod}">
                  <option value=""></option>
                </@field>
              </fieldset>
              <fieldset>
                <@field type="submitarea">
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="saveShippingMethod" text="${uiLabelMap.EcommerceContinueToStep} 4"/>
                  <@field type="submit" submitType="link" style="display:none" class="${styles.link_run_session!}" style="display: none;" href="javascript:void(0);" id="processingBilling" text="${uiLabelMap.EcommercePleaseWait}..."/>
                </@field>
              </fieldset>
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="billingPanel" title="${uiLabelMap.EcommerceStep} 4: ${uiLabelMap.AccountingBilling}">
          <div id="billingSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openBillingPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <div class="completed" id="billingCompleted">
              <ul>
                <li>
                  <@heading>${uiLabelMap.OrderBillUpTo}</@heading>
                  <ul>
                    <li id="completedBillToAttn"></li>
                    <li id="completedBillToPhoneNumber"></li>
                    <li id="paymentMethod"></li>
                    <li id="completedCCNumber"></li>
                    <li id="completedExpiryDate"></li>
                  </ul>
                </li>
                <li>
                  <@heading>${uiLabelMap.EcommerceLocation}</@heading>
                  <ul>
                    <li id="completedBillToAddress1"></li>
                    <li id="completedBillToAddress2"></li>
                    <li id="completedBillToGeo"></li>
                  </ul>
                </li>
              </ul>
            </div>
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
                <#if userLogin??>
                  <input type="hidden" name="keepAddressBook" value="Y" />
                  <input type="hidden" name="setDefaultBilling" value="Y" />
                  <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
                  <input type="hidden" name="productStoreId" value="${productStoreId!}" />
                <#else>
                  <input type="hidden" name="keepAddressBook" value="N" />
                </#if>
                <div id="billingFormServerError" class="errorMessage"></div>
                
                <@field type="input" id="firstNameOnCard" name="firstNameOnCard" required=true value=(firstNameOnCard!) label="${uiLabelMap.PartyFirstName}"/>
                <@field type="input" id="lastNameOnCard" name="lastNameOnCard" required=true value=(lastNameOnCard!) label="${uiLabelMap.PartyLastName}"/>
                <@field type="generic" label=uiLabelMap.PartyContactNumber required=true>  
                  <#if billToTelecomNumber?has_content>
                    <@field type="input" inline=true name="billToCountryCode" required=true id="billToCountryCode" value=(billToTelecomNumber.countryCode!) size="5" maxlength="10" label="${uiLabelMap.CommonCountry}"/> -
                    <@field type="input" inline=true name="billToAreaCode" required=true id="billToAreaCode" value=(billToTelecomNumber.areaCode!) size="5" maxlength="10" label="${uiLabelMap.PartyAreaCode}"/> -
                    <@field type="input" inline=true name="billToContactNumber" required=true id="billToContactNumber" value=(billToTelecomNumber.contactNumber!) size="10" maxlength="15" label="${uiLabelMap.PartyContactNumber}"/> -
                    <@field type="input" inline=true name="billToExtension" id="billToExtension" value=(billToExtension!) size="5" maxlength="10" label="${uiLabelMap.PartyExtension}"/>
                  <#else>
                    <@field type="input" inline=true name="billToCountryCode" required=true id="billToCountryCode" value=(parameters.billToCountryCode!) size="5" maxlength="10" label="${uiLabelMap.CommonCountry}"/> -
                    <@field type="input" inline=true name="billToAreaCode" required=true id="billToAreaCode" value=(parameters.billToAreaCode!) size="5" maxlength="10" label="${uiLabelMap.PartyAreaCode}"/> -
                    <@field type="input" inline=true name="billToContactNumber" required=true id="billToContactNumber" value=(parameters.billToContactNumber!) size="10" maxlength="15" label="${uiLabelMap.PartyContactNumber}"/> -
                    <@field type="input" inline=true name="billToExtension" id="billToExtension" value=(parameters.billToExtension!) size="5" maxlength="10" label="${uiLabelMap.PartyExtension}"/>
                  </#if>
                </@field>
                <@field type="select" name="cardType" id="cardType" required=true label="${uiLabelMap.AccountingCardType}">
                    <#if cardType?has_content>
                      <option label="${cardType!}" value="${cardType!}">${cardType!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#cctypes" />
                </@field>

                <@field type="input" id="cardNumber" name="cardNumber" required=true class="+creditcard" value=(cardNumber!) size="30" maxlength="16" label="${uiLabelMap.AccountingCardNumber}"/>
                <@field type="input" id="billToCardSecurityCode" name="billToCardSecurityCode" size="4" maxlength="4" value="" label="CVV2"/>
                <@field type="select" id="expMonth" name="expMonth" required=true label="${uiLabelMap.CommonMonth}">
                    <#if expMonth?has_content>
                      <option label="${expMonth!}" value="${expMonth!}">${expMonth!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccmonths" />
                </@field>
                <@field type="select" id="expYear" name="expYear" required=true label="${uiLabelMap.CommonYear}">
                    <#if expYear?has_content>
                      <option value="${expYear!}">${expYear!}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#ccyears" />
                </@field>
              </fieldset>
              <fieldset class="col">
                <@field type="checkbox" id="useShippingAddressForBilling" name="useShippingAddressForBilling" type="checkbox" value="Y" checked=(useShippingAddressForBilling?has_content && (useShippingAddressForBilling!"")=="Y") label=uiLabelMap.FacilityBillingAddressSameShipping />
  
                <div id="billingAddress"<#if useShippingAddressForBilling?has_content && (useShippingAddressForBilling!"")=="Y"> style="display:none"</#if>>
                  <@field type="input" id="billToAddress1" name="billToAddress1" required=true size="30" value=(billToAddress1!) label="${uiLabelMap.PartyAddressLine1}"/>
                  <@field type="input" id="billToAddress2" name="billToAddress2" value=(billToAddress2!) size="30" label="${uiLabelMap.PartyAddressLine2}"/>
                  <@field type="input" id="billToCity" name="billToCity" required=true value=(billToCity!) label="${uiLabelMap.CommonCity}"/>
                  <@field type="input" id="billToPostalCode" name="billToPostalCode" required=true value=(billToPostalCode!) size="12" maxlength="10" label="${uiLabelMap.PartyZipCode}"/>
                  <@field type="select" name="billToCountryGeoId" required=true id="billToCountryGeoId" label="${uiLabelMap.CommonCountry}">
                    <#if billToCountryGeoId??>
                      <option value="${billToCountryGeoId!}">${billToCountryProvinceGeo!(billToCountryGeoId!)}</option>
                    </#if>
                    <@render resource="component://common/widget/CommonScreens.xml#countries" />
                  </@field>
                  <@field type="select" id="billToStateProvinceGeoId" name="billToStateProvinceGeoId" required=true label="${uiLabelMap.CommonState}">
                    <#if billToStateProvinceGeoId?has_content>
                      <option value="${billToStateProvinceGeoId!}">${billToStateProvinceGeo?default(billToStateProvinceGeoId!)}</option>
                    <#else>
                      <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                    </#if>
                  </@field>
                </div>
              </fieldset>
              <fieldset>
                <@field type="submitarea">
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePaymentAndBillingContact" text="${uiLabelMap.EcommerceContinueToStep} 5"/>
                  <@field type="submit" submitType="link" href="javascript:void(0);" class="${styles.link_run_session!}" style="display: none;" id="processingOrderSubmitPanel" text="${uiLabelMap.EcommercePleaseWait}..."/>
                </@field>
              </fieldset>
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section title="${uiLabelMap.EcommerceStep} 5: ${uiLabelMap.OrderSubmitOrder}">
          <div id="orderSubmitPanel" style="display: none;">
            <form id="orderSubmitForm" action="<@ofbizUrl>onePageProcessOrder</@ofbizUrl>" method="post">
                <fieldset>
                  <@field type="submitarea">
                    <@field type="submit" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" id="processOrderButton" name="processOrderButton" text=uiLabelMap.OrderSubmitOrder />
                    <@field type="submit" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" style="display: none;" id="processingOrderButton" name="processingOrderButton" text=uiLabelMap.OrderSubmittingOrder />
                  </@field>
                </fieldset>
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
