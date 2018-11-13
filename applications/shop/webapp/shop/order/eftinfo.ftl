<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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

