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

<#-- gift card fields -->
  <input type="hidden" name="addGiftCard" value="Y"/>
  <#assign giftCard = giftCard!>
  <#if (paymentMethodTypeId!) != "GIFT_CARD">
    <hr />
  </#if>
  <@heading>${uiLabelMap.AccountingGiftCardInformation}</@heading>
  <@field type="input" label=uiLabelMap.AccountingGiftCardNumber required=true size="20" maxlength="60" name="giftCardNumber" value=(giftCard.cardNumber!)/>
  <@field type="input" label=uiLabelMap.AccountingPINNumber required=true size="10" maxlength="60" name="giftCardPin" value=(giftCard.pinNumber!)/>
  <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(giftCard.description!)/>
  <#if (paymentMethodTypeId!) != "GIFT_CARD">
    <@field type="input" label=uiLabelMap.AccountingAmountToUse required=true size="5" maxlength="10" name="giftCardAmount" value=(giftCard.pinNumber!)/>
  </#if>
