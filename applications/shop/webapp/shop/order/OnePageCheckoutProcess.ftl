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
          ${screens.render("component://shop/widget/CartScreens.xml#UpdateCart")}
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
                  <div>
                      <span>
                        <label for="firstName">${uiLabelMap.PartyFirstName}*
                          <span id="advice-required-firstName" style="display: none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                        </label>
                        <input id="firstName" name="firstName" class="required" type="text" value="${firstName!}" />
                      </span>
                      <span>
                        <label for="lastName">${uiLabelMap.PartyLastName}*
                          <span id="advice-required-lastName" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                        </label>
                        <input id="lastName" name="lastName" class="required" type="text" value="${lastName!}" />
                      </span>
                  </div>
                  <div>
                  <#if shipToTelecomNumber?has_content>
                      <span>
                          <label for="shipToCountryCode">${uiLabelMap.CommonCountry}*
                              <span id="advice-required-shipToCountryCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToCountryCode" class="required" id="shipToCountryCode" value="${shipToTelecomNumber.countryCode!}" size="5" maxlength="10" /> -
                      </span>
                      <span>
                          <label for="shipToAreaCode">${uiLabelMap.PartyAreaCode}*
                              <span id="advice-required-shipToAreaCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToAreaCode" class="required" id="shipToAreaCode" value="${shipToTelecomNumber.areaCode!}" size="5" maxlength="10" /> -
                      </span>
                      <span>
                          <label for="shipToContactNumber">${uiLabelMap.PartyContactNumber}*
                              <span id="advice-required-shipToContactNumber" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToContactNumber" class="required" id="shipToContactNumber" value="${shipToTelecomNumber.contactNumber!}" size="10" maxlength="15" /> -
                      </span>
                      <span>
                          <label for="shipToExtension">${uiLabelMap.PartyExtension}</label>
                          <input type="text" name="shipToExtension" id="shipToExtension" value="${shipToExtension!}" size="5" maxlength="10" />
                      </span>
                  <#else>
                      <span>
                          <label for="shipToCountryCode">${uiLabelMap.CommonCountry}*
                              <span id="advice-required-shipToCountryCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToCountryCode" class="required" id="shipToCountryCode" value="${parameters.shipToCountryCode!}" size="5" maxlength="10" /> -
                      </span>
                      <span>
                          <label for="shipToAreaCode">${uiLabelMap.PartyAreaCode}*
                              <span id="advice-required-shipToAreaCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToAreaCode" class="required" id="shipToAreaCode" value="${parameters.shipToAreaCode!}" size="5" maxlength="10" /> -
                      </span>
                      <span>
                          <label for="shipToContactNumber">${uiLabelMap.PartyContactNumber}*
                              <span id="advice-required-shipToContactNumber" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input type="text" name="shipToContactNumber" class="required" id="shipToContactNumber" value="${parameters.shipToContactNumber!}" size="10" maxlength="15" /> -
                      </span>
                      <span>
                          <label for="shipToExtension">${uiLabelMap.PartyExtension}</label>
                          <input type="text" name="shipToExtension" id="shipToExtension" value="${parameters.shipToExtension!}" size="5" maxlength="10" />
                      </span>
                  </#if>
                  </div>
                  <div>
                      <span>
                          <label for="emailAddress">${uiLabelMap.PartyEmailAddress}*
                            <span id="advice-required-emailAddress" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                          </label>
                          <input id="emailAddress" name="emailAddress" class="required validate-email" maxlength="255" size="40" type="text" value="${emailAddress!}" />
                      </span>
                  </div>
                    <div>
                        <span>
                            <label for="shipToAddress1">${uiLabelMap.PartyAddressLine1}*
                                <span id="advice-required-shipToAddress1" class="custom-advice errorMessage" style="display:none"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <input id="shipToAddress1" name="shipToAddress1" class="required" type="text" value="${shipToAddress1!}" maxlength="255" size="40" />
                        </span>
                    </div>
                    <div>
                        <span>
                          <label for="shipToAddress2">${uiLabelMap.PartyAddressLine2}</label>
                          <input id="shipToAddress2" name="shipToAddress2" type="text" value="${shipToAddress2!}" maxlength="255" size="40" />
                        </span>
                    </div>
                    <div>
                        <span>
                            <label for="shipToCity">${uiLabelMap.CommonCity}*
                                <span id="advice-required-shipToCity" class="custom-advice errorMessage" style="display:none"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <input id="shipToCity" name="shipToCity" class="required" type="text" value="${shipToCity!}" maxlength="255" size="40" />
                        </span>
                    </div>
                    <div>
                        <span>
                            <label for="shipToPostalCode">${uiLabelMap.PartyZipCode}*
                                <span id="advice-required-shipToPostalCode" class="custom-advice errorMessage" style="display:none"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <input id="shipToPostalCode" name="shipToPostalCode" class="required" type="text" value="${shipToPostalCode!}" size="12" maxlength="10" />
                        </span>
                    </div>
                    <div>
                        <span>
                            <label for="shipToCountryGeoId">${uiLabelMap.CommonCountry}*
                                <span id="advice-required-shipToCountryGeo" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <select name="shipToCountryGeoId" id="shipToCountryGeoId">
                              <#if shipToCountryGeoId??>
                                <option value="${shipToCountryGeoId!}">${shipToCountryProvinceGeo?default(shipToCountryGeoId!)}</option>
                              </#if>
                              ${screens.render("component://common/widget/CommonScreens.xml#countries")}
                            </select>
                        </span>
                    </div>
                    <div id="shipToStates">
                        <span>
                            <label for="shipToStateProvinceGeoId">${uiLabelMap.CommonState}*
                                <span id="advice-required-shipToStateProvinceGeoId" style="display:none" class="errorMessage">(${uiLabelMap.CommonRequired})</span>
                            </label>
                            <select id="shipToStateProvinceGeoId" name="shipToStateProvinceGeoId">
                              <#if shipToStateProvinceGeoId?has_content>
                                <option value="${shipToStateProvinceGeoId!}">${shipToStateProvinceGeo?default(shipToStateProvinceGeoId!)}</option>
                              <#else>
                                <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                              </#if>
                              ${screens.render("component://common/widget/CommonScreens.xml#states")}
                            </select>
                        </span>
                    </div>
                  </fieldset>
                  <fieldset>
                    <a href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePartyAndShippingContact">${uiLabelMap.EcommerceContinueToStep} 3</a>
                    <a style="display:none" class="${styles.link_run_session!}" href="javascript:void(0);" id="processingShippingOptions">${uiLabelMap.EcommercePleaseWait}....</a>
                  </fieldset>
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section id="shippingOptionPanel" title="${uiLabelMap.EcommerceStep} 3: ${uiLabelMap.PageTitleShippingOptions}">
          <div id="shippingOptionSummaryPanel" style="display: none;">
            <a href="javascript:void(0);" id="openShippingOptionPanel" class="${styles.link_run_local!} ${styles.action_show!}">${uiLabelMap.EcommerceClickHereToEdit}</a>
            <div class="completed" id="shippingOptionCompleted">
              <ul>
                <li>${uiLabelMap.CommonMethod}</li>
                <li id="selectedShipmentOption"></li>
              </ul>
            </div>
          </div>

<#-- ============================================================= -->
          <div id="editShippingOptionPanel" style="display: none;">
            <form id="shippingOptionForm" action="<@ofbizUrl></@ofbizUrl>" method="post">
              <fieldset>
                  <div id="shippingOptionFormServerError" class="errorMessage"></div>
                  <div>
                      <label for="shipMethod">${uiLabelMap.OrderSelectShippingMethod}*
                          <span id="advice-required-shipping_method" class="custom-advice" style="display:none"> (${uiLabelMap.CommonRequired})</span>
                      </label>
                      <select id="shipMethod" name="shipMethod" class="required">
                          <option value=""></option>
                      </select>
                  </div>
              </fieldset>
              <fieldset>
                <a href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="saveShippingMethod">${uiLabelMap.EcommerceContinueToStep} 4</a>
                <a style="display:none" class="${styles.link_run_session!}" href="javascript:void(0);" id="processingBilling">${uiLabelMap.EcommercePleaseWait}....</a>
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
                        <div>
                            <span>
                                <label for="firstNameOnCard">${uiLabelMap.PartyFirstName}*
                                    <span id="advice-required-firstNameOnCard" style="display: none;" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input id="firstNameOnCard" name="firstNameOnCard" class="required" type="text" value="${firstNameOnCard!}" />
                            </span>
                            <span>
                                <label for="lastNameOnCard">${uiLabelMap.PartyLastName}*
                                    <span id="advice-required-lastNameOnCard" style="display: none;" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input id="lastNameOnCard" name="lastNameOnCard" class="required" type="text" value="${lastNameOnCard!}" />
                            </span>
                        </div>
                        <div>  
                          <#if billToTelecomNumber?has_content>
                            <span>
                                <label for="billToCountryCode">${uiLabelMap.CommonCountry}*
                                    <span id="advice-required-billToCountryCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToCountryCode" class="required" id="billToCountryCode" value="${billToTelecomNumber.countryCode!}" size="5" maxlength="10" /> -
                            </span>
                            <span>
                                <label for="billToAreaCode">${uiLabelMap.PartyAreaCode}*
                                    <span id="advice-required-billToAreaCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToAreaCode" class="required" id="billToAreaCode" value="${billToTelecomNumber.areaCode!}" size="5" maxlength="10" /> -
                            </span>
                            <span>
                                <label for="billToContactNumber">${uiLabelMap.PartyContactNumber}*
                                    <span id="advice-required-billToContactNumber" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToContactNumber" class="required" id="billToContactNumber" value="${billToTelecomNumber.contactNumber!}" size="10" maxlength="15" /> -
                            </span>
                            <span>
                                <label for="billToExtension">${uiLabelMap.PartyExtension}</label>
                                <input type="text" name="billToExtension" id="billToExtension" value="${billToExtension!}" size="5" maxlength="10" />
                            </span>
                          <#else>
                            <span>
                                <label for="billToCountryCode">${uiLabelMap.CommonCountry}*
                                    <span id="advice-required-billToCountryCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToCountryCode" class="required" id="billToCountryCode" value="${parameters.billToCountryCode!}" size="5" maxlength="10" /> -
                            </span>
                            <span>
                                <label for="billToAreaCode">${uiLabelMap.PartyAreaCode}*
                                    <span id="advice-required-billToAreaCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToAreaCode" class="required" id="billToAreaCode" value="${parameters.billToAreaCode!}" size="5" maxlength="10" /> -
                            </span>
                            <span>
                                <label for="billToContactNumber">${uiLabelMap.PartyContactNumber}*
                                    <span id="advice-required-billToContactNumber" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input type="text" name="billToContactNumber" class="required" id="billToContactNumber" value="${parameters.billToContactNumber!}" size="10" maxlength="15" /> -
                            </span>
                            <span>
                                <label for="billToExtension">${uiLabelMap.PartyExtension}</label>
                                <input type="text" name="billToExtension" id="billToExtension" value="${parameters.billToExtension!}" size="5" maxlength="10" />
                            </span>
                          </#if>
                        </div>
                        <div>
                          <span>
                                <label for="cardType">${uiLabelMap.AccountingCardType}*<span id="advice-required-cardType" style="display: none;" class="errorMessage"> (${uiLabelMap.CommonRequired})</span></label>
                                <select name="cardType" id="cardType">
                                  <#if cardType?has_content>
                                    <option label="${cardType!}" value="${cardType!}">${cardType!}</option>
                                  </#if>
                                  ${screens.render("component://common/widget/CommonScreens.xml#cctypes")}
                                </select>
                          </span>
                        </div>
                        <div>
                            <span>
                                <label for="cardNumber">${uiLabelMap.AccountingCardNumber}*
                                    <span id="advice-required-cardNumber" style="display: none;" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                                </label>
                                <input id="cardNumber" name="cardNumber" class="required creditcard" type="text" value="${cardNumber!}" size="30" maxlength="16" />
                            </span>
                            <span>
                                <label for="billToCardSecurityCode">CVV2</label>
                                <input id="billToCardSecurityCode" name="billToCardSecurityCode" size="4" type="text" maxlength="4" value="" />
                            </span>
                        </div>
                        <div>
                          <span>
                            <label for="expMonth">${uiLabelMap.CommonMonth}:*
                                <span id="advice-required-expMonth" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <select id="expMonth" name="expMonth" class="required">
                              <#if expMonth?has_content>
                                <option label="${expMonth!}" value="${expMonth!}">${expMonth!}</option>
                              </#if>
                              ${screens.render("component://common/widget/CommonScreens.xml#ccmonths")}
                            </select>
                          </span>
                          <span>
                            <label for="expYear">${uiLabelMap.CommonYear}:*
                                <span id="advice-required-expYear" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                            </label>
                            <select id="expYear" name="expYear" class="required">
                              <#if expYear?has_content>
                                <option value="${expYear!}">${expYear!}</option>
                              </#if>
                              ${screens.render("component://common/widget/CommonScreens.xml#ccyears")}
                            </select>
                          </span>
                        </div>
                    </fieldset>
                    <fieldset class="col">
                        <div>
                                <input class="checkbox" id="useShippingAddressForBilling" name="useShippingAddressForBilling" type="checkbox" value="Y" <#if useShippingAddressForBilling?has_content && useShippingAddressForBilling?default("")=="Y">checked="checked"</#if> /><label for="useShippingAddressForBilling">${uiLabelMap.FacilityBillingAddressSameShipping}</label>
                        </div>
                        <div id="billingAddress" <#if useShippingAddressForBilling?has_content && useShippingAddressForBilling?default("")=="Y">style="display:none"</#if>>
                          <div>
                              <label for="billToAddress1">${uiLabelMap.PartyAddressLine1}*
                                <span id="advice-required-billToAddress1" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                              </label>
                              <input id="billToAddress1" name="billToAddress1" class="required" size="30" type="text" value="${billToAddress1!}" />
                          </div>
                          <div>
                              <label for="billToAddress2">${uiLabelMap.PartyAddressLine2}</label>
                              <input id="billToAddress2" name="billToAddress2" type="text" value="${billToAddress2!}" size="30" />
                          </div>
                          <div>
                              <label for="billToCity">${uiLabelMap.CommonCity}*
                                <span id="advice-required-billToCity" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                              </label>
                              <input id="billToCity" name="billToCity" class="required" type="text" value="${billToCity!}" />
                          </div>
                          <div>
                              <label for="billToPostalCode">${uiLabelMap.PartyZipCode}*
                                <span id="advice-required-billToPostalCode" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                              </label>
                              <input id="billToPostalCode" name="billToPostalCode" class="required" type="text" value="${billToPostalCode!}" size="12" maxlength="10" />
                          </div>
                          <div>
                              <label for="billToCountryGeoId">${uiLabelMap.CommonCountry}*
                                <span id="advice-required-billToCountryGeoId" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                              </label>
                              <select name="billToCountryGeoId" id="billToCountryGeoId">
                                <#if billToCountryGeoId??>
                                  <option value="${billToCountryGeoId!}">${billToCountryProvinceGeo?default(billToCountryGeoId!)}</option>
                                </#if>
                                ${screens.render("component://common/widget/CommonScreens.xml#countries")}
                              </select>
                          </div>
                          <div>
                              <label for="billToStateProvinceGeoId">${uiLabelMap.CommonState}*
                                <span id="advice-required-billToStateProvinceGeoId" style="display:none" class="errorMessage"> (${uiLabelMap.CommonRequired})</span>
                              </label>
                              <select id="billToStateProvinceGeoId" name="billToStateProvinceGeoId">
                                <#if billToStateProvinceGeoId?has_content>
                                  <option value="${billToStateProvinceGeoId!}">${billToStateProvinceGeo?default(billToStateProvinceGeoId!)}</option>
                                <#else>
                                  <option value="_NA_">${uiLabelMap.PartyNoState}</option>
                                </#if>
                              </select>
                          </div>
                        </div>
                    </fieldset>
                    <br style="clear:both;"/>
                    <fieldset>
                      <a href="javascript:void(0);" class="${styles.link_run_session!} ${styles.action_continue!}" id="savePaymentAndBillingContact">${uiLabelMap.EcommerceContinueToStep} 5</a>
                      <a href="javascript:void(0);" class="${styles.link_run_session!}" style="display: none;" id="processingOrderSubmitPanel">${uiLabelMap.EcommercePleaseWait}....</a>
                    </fieldset>
            </form>
          </div>
        </@section>

<#-- ========================================================================================================================== -->
        <@section title="${uiLabelMap.EcommerceStep} 5: ${uiLabelMap.OrderSubmitOrder}">
          <div id="orderSubmitPanel" style="display: none;">
            <form id="orderSubmitForm" action="<@ofbizUrl>onePageProcessOrder</@ofbizUrl>" method="post">
                <fieldset>
                    <input type="${styles.link_run_sys!} ${styles.action_complete!}" id="processOrderButton" name="processOrderButton" value="${uiLabelMap.OrderSubmitOrder}" />
                    <input type="${styles.link_run_sys!} ${styles.action_complete!}" style="display: none;" id="processingOrderButton" name="processingOrderButton" value="${uiLabelMap.OrderSubmittingOrder}" />
                </fieldset>
            </form>
          </div>
        </@section>
      </div>
    </#if>

<#-- ========================================================================================================================== -->
    
    <#assign style><#if shoppingCart?has_content && (shoppingCart.size() > 0)>display: none;</#if></#assign>
    <@section id="emptyCartCheckoutPanel" style=style>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 1: ${uiLabelMap.PageTitleShoppingCart}</@heading>
        <span>You currently have no items in your cart. Click <a href="<@ofbizUrl>main</@ofbizUrl>">here</a> to view our products.</span>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 2: ${uiLabelMap.FacilityShipping}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 3: ${uiLabelMap.PageTitleShippingOptions}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 4: ${uiLabelMap.AccountingBilling}</@heading>
        <@heading relLevel=+1>${uiLabelMap.EcommerceStep} 5: ${uiLabelMap.OrderSubmitOrder}</@heading>
    </@section>
    
</@section>
