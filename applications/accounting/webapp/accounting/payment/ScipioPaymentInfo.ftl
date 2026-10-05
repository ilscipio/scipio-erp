<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- SCIPIO -->
<@section title=uiLabelMap.CommonOverview>
  <#assign paymentType = payment.getRelatedOne("PaymentType", false)/>
  <#assign currentStatus = payment.getRelatedOne("StatusItem", false)/>
  <@table type="fields">
      <@tr>
          <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
          <@td colspan="3">
            ${currentStatus.get('description',locale)}
          </@td>
        </@tr>

      <#if payment.effectiveDate?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingEffectiveDate}
          </@td>
          <@td colspan="3"><@formattedDateTime date=payment.effectiveDate /></@td>
        </@tr>
      </#if>

      <#if payment.amount?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.CommonAmount}</@td>
          <@td colspan="3"><@ofbizCurrency isoCode=payment.currencyUomId amount=(payment.amount!)/>
                <#if payment.actualCurrencyAmount?has_content && payment.actualCurrencyUomId != payment.currencyUomId>(<@ofbizCurrency isoCode=(payment.actualCurrencyUomId!) amount=(payment.actualCurrencyAmount!)/>)</#if>
            </@td>
        </@tr>
      </#if>

      <#if payment.paymentMethodId?has_content>
        <#assign paymentMethod = payment.getRelatedOne("PaymentMethod", false)/>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingPaymentMethod}
          </@td>
          <@td colspan="3">${paymentMethod.get('description',locale)!paymentMethod.paymentMethodTypeId!}</@td>
        </@tr>
      </#if>

      <#if paymentType?has_content>
        <@tr>
          <@td class="${styles.grid_large!}2">${uiLabelMap.CommonType}
          </@td>
          <@td colspan="3">${paymentType.get('description',locale)!}</@td>
        </@tr>
      </#if>
      
    
    <#if payment.partyIdFrom?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonFrom}</@td>
            <@td colspan="3"><a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${payment.partyIdFrom!}</@serverUrl>">${partyNameResultFrom.fullName}</a></@td>
        </@tr>
    </#if>

    <#if payment.partyIdTo?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonTo}</@td>
            <@td colspan="3"><a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${payment.partyIdTo!}</@serverUrl>">${partyNameResultTo.fullName}</a></@td>
        </@tr>
    </#if>

    <#if payment.comments?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.CommonComments}</@td>
            <@td colspan="3">${payment.comments!}</@td>
        </@tr>
    </#if>

    <#if payment.paymentRefNum?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.FormFieldTitle_referenceNum}</@td>
            <@td colspan="3">${payment.paymentRefNum!}</@td>
        </@tr>
    </#if>

    <#if payment.paymentGatewayResponseId?has_content>
        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.AccountingGatewayResponse}</@td>
            <@td colspan="3"><a href="<@pageUrl>ViewGatewayResponse?paymentGatewayResponseId=${payment.paymentGatewayResponseId!}</@pageUrl>">${payment.paymentGatewayResponseId!}</a></@td>
        </@tr>
    </#if>

  </@table>
</@section>