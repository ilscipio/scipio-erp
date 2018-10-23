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

<@heading>${uiLabelMap.AccountingGiftCardLink}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardLink}.</p>

<form name="gclink" method="post" action="<@ofbizUrl>linkgiftcard</@ofbizUrl>">
  <input type="hidden" name="paymentConfig" value="${paymentProperties!"payment.properties"}" />
  <#if userLogin?has_content>
    <input type="hidden" name="partyId" value="${userLogin.partyId}" />
  </#if>
  <@table type="fields"> <#-- orig: align="center" -->
    <@tr>
      <@td colspan="2" align="center">
        <div class="tableheadtext">${uiLabelMap.AccountingPhysicalCard}</div>
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="physicalCard" size="20" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="physicalPin" size="20" /></@td>
    </@tr>
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td colspan="2" align="center">
        <div class="tableheadtext">${uiLabelMap.AccountingVirtualCard}</div>
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingCardNumber}</@td>
      <@td><input type="text" name="virtualCard" size="20" /></@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.AccountingPINNumber}</@td>
      <@td><input type="text" name="virtualPin" size="20" /></@td>
    </@tr>
    <@tr>
      <@td colspan="2">&nbsp;</@td>
    </@tr>
    <@tr>
      <@td colspan="2" align="center"><input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.EcommerceLinkCards}" /></@td>
    </@tr>
  </@table>
</form>
<br />
