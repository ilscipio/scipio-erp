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

