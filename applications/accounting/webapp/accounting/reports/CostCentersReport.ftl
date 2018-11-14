<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
