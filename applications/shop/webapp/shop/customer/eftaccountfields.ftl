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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#if !eftParams??>
  <#assign eftParams = parameters>
</#if>

<#-- SCIPIO: EFT account fields, originally from editeftaccount.ftl 
  for pre-fill, requires:
    eftAccountData 
    paymentMethodData
-->

<@field type="input" label=uiLabelMap.AccountingNameOnAccount required=true size="30" maxlength="60" name="${fieldNamePrefix}nameOnAccount" value=(eftParams["${fieldNamePrefix}nameOnAccount"]!(eftAccountData.nameOnAccount)!(eafFallbacks.nameOnAccount)!) />
<@field type="input" label=uiLabelMap.AccountingCompanyNameOnAccount size="30" maxlength="60" name="${fieldNamePrefix}companyNameOnAccount" value=(eftParams["${fieldNamePrefix}companyNameOnAccount"]!(eftAccountData.companyNameOnAccount)!(eafFallbacks.companyNameOnAccount)!) />
<@field type="input" label=uiLabelMap.AccountingBankName required=true size="30" maxlength="60" name="${fieldNamePrefix}bankName" value=(eftParams["${fieldNamePrefix}bankName"]!(eftAccountData.bankName)!(eafFallbacks.bankName)!) />
<@field type="input" label=uiLabelMap.AccountingRoutingNumber required=true size="10" maxlength="30" name="${fieldNamePrefix}routingNumber" value=(eftParams["${fieldNamePrefix}routingNumber"]!(eftAccountData.routingNumber)!(eafFallbacks.routingNumber)!) />
<@field type="select" label=uiLabelMap.AccountingAccountType required=true name="${fieldNamePrefix}accountType">
  <#assign selectedAccountType = (eftParams["${fieldNamePrefix}accountType"]!(eftAccountData.accountType)!(eafFallbacks.accountType)!)>
  <#-- SCIPIO: NOTE: These type names are very loosely defined, so we must accept others... 
      FIXME: Server-side may not validate these currently -->
  <#if selectedAccountType?has_content && !["Checking", "Savings"]?seq_contains(selectedAccountType)>
    <option value="${selectedAccountType}">${selectedAccountType!}</option>
    <option></option>
  </#if>
  <option<#if selectedAccountType == "Checking"> value="Checking"</#if>>${uiLabelMap.CommonChecking}</option>
  <option<#if selectedAccountType == "Savings"> value="Savings"</#if>>${uiLabelMap.CommonSavings}</option>
</@field>
<@field type="input" label=uiLabelMap.AccountingAccountNumber required=true size="20" maxlength="40" name="${fieldNamePrefix}accountNumber" value=(eftParams["${fieldNamePrefix}accountNumber"]!(eftAccountData.accountNumber)!(eafFallbacks.accountNumber)!) />
<@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="${fieldNamePrefix}description" value=(eftParams["${fieldNamePrefix}description"]!(paymentMethodData.description)!(eafFallbacks.description)!) containerClass="+${styles.field_extra!}" />


   

