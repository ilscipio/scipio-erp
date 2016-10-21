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

<#if glAcctgTrialBalanceList?has_content>
    <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>
            <@menuitem type="link" href=makeOfbizUrl("GlAccountTrialBalanceReportPdf.pdf?organizationPartyId=${organizationPartyId}&timePeriod=${parameters.timePeriod}&isPosted=${parameters.isPosted}&glAccountId=${parameters.glAccountId}") text=uiLabelMap.AccountingInvoicePDF target="_BLANK" class="+${styles.action_run_sys!} ${styles.action_export!}" />
        </@menu>
    </#macro>
    <@section menuContent=menuContent>
        <form name="glAccountTrialBalanceReport" id="glAccountTrialBalanceReport">
            <@heading>${uiLabelMap.AccountingSubsidiaryLedger}</@heading>
            <@heading>${uiLabelMap.FormFieldTitle_companyName} : ${(currentOrganization.groupName)!}</@heading>
            <@heading>${uiLabelMap.AccountingTimePeriod} : <#if currentTimePeriod?has_content>${(currentTimePeriod.fromDate)!} ${uiLabelMap.CommonTo} ${(currentTimePeriod.thruDate)!}</#if></@heading>
            <@heading>${uiLabelMap.AccountingGlAccountNameAndGlAccountCode} : ${(glAccount.accountCode)!} - ${(glAccount.accountName)!}</@heading>
            <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" responsive=true scrollable=true> <#-- orig: class="" --> <#-- orig: cellspacing="" --> <#-- orig: border=2 -->
                <@thead>
                    <@tr>
                        <@th align="left"><b>${uiLabelMap.FormFieldTitle_transactionDate}</b></@th>
                        <@th align="left"><b>${uiLabelMap.AccountingAccountTransactionId}</b></@th>
                        <@th align="left"><b>${uiLabelMap.CommonDescription}</b></@th>
                        <@th align="left"><b>${uiLabelMap.AccountingTypeOfTheCurrency}</b></@th>
                        <@th align="left"><b>${uiLabelMap.AccountingOriginalCurrency}</b></@th>
                        <@th align="right"><b>${uiLabelMap.AccountingDebitAmount}</b></@th>
                        <@th align="right"><b>${uiLabelMap.AccountingCreditAmount}</b></@th>
                        <@th align="right"><b>${uiLabelMap.AccountingDebitOrCreditOfBalance}</b></@th>
                        <@th align="right"><b>${uiLabelMap.AccountingBalanceOfTheAccount}</b></@th>
                    </@tr>
                    <@tr class="header-row">
                        <@th colspan=2></@th>
                        <@th colspan=3 align="center"><b>${uiLabelMap.AccountingTheBalanceOfLastYear}</b></@th>
                        <@th colspan=2></@th>
                        <@th align="right"><b><#if (isDebitAccount)>${uiLabelMap.AccountingDebitFlag}<#else>${uiLabelMap.AccountingCreditFlag}</#if></b></@th>
                        <@th align="right">${(openingBalance)!}</@th>
                    </@tr>
                </@thead>
                <#list glAcctgTrialBalanceList as glAcctgTrialBalance>                    
                    <#assign acctgTransAndEntries = glAcctgTrialBalance.acctgTransAndEntries/>
                    <#if acctgTransAndEntries?has_content>
                        <#list acctgTransAndEntries as acctgTransAndEntry>
                            <@tr>
                                <@td align="left">${(acctgTransAndEntry.transactionDate)!}</@td>
                                <@td align="left">${(acctgTransAndEntry.acctgTransId)!}</@td>
                                <@td align="left">${(acctgTransAndEntry.transDescription)!}</@td>
                                <@td align="left">${(acctgTransAndEntry.currencyUomId)!}</@td>
                                <@td align="left">${(acctgTransAndEntry.origCurrencyUomId)!}</@td>
                                <@td class="align-center"><#if ((acctgTransAndEntry.debitCreditFlag)!) == "D">${(acctgTransAndEntry.amount)!}<#else>0</#if></@td>
                                <@td class="align-right"><#if ((acctgTransAndEntry.debitCreditFlag)!) == "C">${(acctgTransAndEntry.amount)!}<#else>0</#if></@td>
                                <@td class="align-right"></@td>
                                <@td class="align-right"></@td>
                            </@tr>
                        </#list>
                        <@tfoot>
                            <@tr class="header-row">
                                <@td colspan=2></@td>
                                <@td colspan=3 align="center"><b>${uiLabelMap.AccountingTotalOfTheCurrentMonth}</b></@td>
                                <@td class="amount" colspan=1><b>${(glAcctgTrialBalance.debitTotal)!}</b></@td>
                                <@td class="amount" colspan=1><b>${(glAcctgTrialBalance.creditTotal)!}</b></@td>
                                <@td class="align-center" colspan=1><b><#if (isDebitAccount)>${uiLabelMap.AccountingDebitFlag}<#else>${uiLabelMap.AccountingCreditFlag}</#if></b></@td>
                                <@td class="amount" colspan=1><b>${(glAcctgTrialBalance.balance)!}</b></@td>
                            </@tr>
                            <@tr class="header-row">
                                <@td colspan=2></@td>
                                <@td align="center" colspan=3><b>${uiLabelMap.AccountingTotalOfYearToDate}</b></@td>
                                <@td class="amount"><b>${glAcctgTrialBalance.totalOfYearToDateDebit}</b></@td>
                                <@td class="amount"><b>${glAcctgTrialBalance.totalOfYearToDateCredit}</b></@td>
                                <@td class="align-center"><b><#if (isDebitAccount)>${uiLabelMap.AccountingDebitFlag}<#else>${uiLabelMap.AccountingCreditFlag}</#if></b></@td>
                                <@td class="amount"><b>${(glAcctgTrialBalance.balanceOfTheAcctgForYear)!}</b></@td>
                            </@tr>
                        </@tfoot>
                    </#if>
                </#list>
            </@table>
        </form>
    </@section>
<#else>
    <@commonMsg type="result-norecord" />
</#if>

