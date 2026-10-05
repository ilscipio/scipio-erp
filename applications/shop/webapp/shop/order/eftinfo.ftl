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

<#-- eft fields -->
<#if !eftAccount?has_content><#assign eftAccount = requestParameters></#if>

<hr />

<input type="hidden" name="paymentMethodId" value="${parameters.paymentMethodId!}"/>

<@heading>${uiLabelMap.AccountingEFTAccountInformation}</@heading>

<@field type="input" label=uiLabelMap.AccountingNameOnAccount required=true size="30" maxlength="60" name="nameOnAccount" value=(eftAccount.nameOnAccount!)/>
<@field type="input" label=uiLabelMap.AccountingCompanyNameOnAccount size="30" maxlength="60" name="companyNameOnAccount" value=(eftAccount.companyNameOnAccount!)/>
<@field type="input" label=uiLabelMap.AccountingBankName required=true size="30" maxlength="60" name="bankName" value=(eftAccount.bankName!)/>
<@field type="input" label=uiLabelMap.AccountingRoutingNumber required=true size="10" maxlength="30" name="routingNumber" value=(eftAccount.routingNumber!)/>

<@field type="select" name="accountType" label=uiLabelMap.AccountingAccountType required=true>
  <option>${eftAccount.accountType!}</option>
  <option></option>
  <option>Checking</option>
  <option>Savings</option>
</@field>

<@field type="input" label=uiLabelMap.AccountingAccountNumber required=true size="20" maxlength="40" name="accountNumber" value=(eftAccount.accountNumber!)/>

<@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(eftAccount.description!)/>

