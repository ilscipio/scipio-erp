<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@heading>${uiLabelMap.AccountingGiftCardBalance}</@heading>

<p>${uiLabelMap.AccountingEnterGiftCardNumber}</p>

<@table type="fields">
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
