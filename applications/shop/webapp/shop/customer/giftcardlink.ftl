<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@heading>${uiLabelMap.AccountingGiftCardLink}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardLink}.</p>

<form name="gclink" method="post" action="<@ofbizUrl>linkgiftcard</@ofbizUrl>">
  <input type="hidden" name="paymentConfig" value="${paymentProperties!"payment.properties"}" />
  <#if userLogin?has_content>
    <input type="hidden" name="partyId" value="${userLogin.partyId}" />
  </#if>
  <@table type="fields">
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
