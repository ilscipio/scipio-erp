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
<#if glAcctBalancesByCostCenter?has_content && glAccountCategories?has_content>
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.CommonId}</@th>
                <@th>${uiLabelMap.CommonCode}</@th>
                <@th>${uiLabelMap.CommonName}</@th>
                <@th class="align-right">${uiLabelMap.FormFieldTitle_postedBalance} - (${currencyUomId})</@th>
                <#list glAccountCategories as glAccountCategory>
                    <@th class="align-right">${glAccountCategory.description!} - (${currencyUomId})</@th>
                </#list>
            </@tr>
        </@thead>
        <#list glAcctBalancesByCostCenter as glAcctBalanceByCostCenter>
            <@tr>
                <@td>${glAcctBalanceByCostCenter.glAccountId!}</@td>
                <@td>${glAcctBalanceByCostCenter.accountCode!}</@td>
                <@td>${glAcctBalanceByCostCenter.accountName!}</@td>
                <@td class="amount">${glAcctBalanceByCostCenter.balance!}</@td>
                <#list glAccountCategories as glAccountCategory>
                    <@td class="amount">${(glAcctBalanceByCostCenter[glAccountCategory.glAccountCategoryId!]!)}</@td>
                </#list>
            </@tr>
        </#list>
    </@table>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
