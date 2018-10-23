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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@heading>${uiLabelMap.AccountingGiftCardBalance}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardNumber}</p>

<@table type="fields"> <#-- orig: align="center" -->
  <#if requestAttributes.processResult??>
    <@tr>
      <@td colspan="2">
        <div align="center">
          ${uiLabelMap.AccountingCurrentBalance}
        </div>
      </@td>
    </@tr>
    <@tr>
      <@td colspan="2">
        <div class="graybox">
          <#if ((requestAttributes.balance!0) > 0)>
            ${requestAttributes.balance}
          <#else>
            ${uiLabelMap.AccountingCurrentBalanceProblem}
          </#if>
        </div>
      </@td>
    </@tr>
    <@tr><@td colspan="2">&nbsp;</@td></@tr>
  </#if>
  <form method="post" action="<@ofbizUrl>querygcbalance</@ofbizUrl>">
    <input type="hidden" name="currency" value="USD" />
    <input type="hidden" name="paymentConfig" value="${paymentProperties!"payment.properties"}" />
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="cardNumber" size="20" value="${(requestParameters.cardNumber)!}" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="pin" size="15" value="${(requestParameters.pin)!}" /></@td>
    </@tr>
    <@tr><@td colspan="2">&nbsp;</@td></@tr>
    <@tr>
      <@td colspan="2" align="center"><input type="submit" class="${styles.link_run_sys!} ${styles.action_verify!}" value="${uiLabelMap.EcommerceCheckBalance}" /></@td>
    </@tr>
  </form>
</@table>
<br />
