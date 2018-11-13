<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>
    <#if glAcctBalancesByCostCenter?has_content && glAccountCategories?has_content>
        <fo:table border="1pt solid" border-width=".1mm" width="19cm">
            <fo:table-header>
                <fo:table-cell border="1pt solid" border-width=".1mm">
                    <fo:block text-align="center">${uiLabelMap.FormFieldTitle_glAccountId}</fo:block>
                </fo:table-cell>
                <fo:table-cell border="1pt solid" border-width=".1mm">
                    <fo:block text-align="center">${uiLabelMap.FormFieldTitle_accountCode}</fo:block>
                </fo:table-cell>
                <fo:table-cell border="1pt solid" border-width=".1mm">
                    <fo:block text-align="center">${uiLabelMap.FormFieldTitle_accountName}</fo:block>
                </fo:table-cell>
                <fo:table-cell border="1pt solid" border-width=".1mm">
                    <fo:block text-align="center">${uiLabelMap.FormFieldTitle_postedBalance} - (${currencyUomId!})</fo:block>
                </fo:table-cell>
                <#list glAccountCategories as glAccountCategory>
                    <fo:table-cell border="1pt solid" border-width=".1mm">
                        <fo:block text-align="center">${glAccountCategory.description!} - (${currencyUomId!})</fo:block>
                    </fo:table-cell>
                </#list>
            </fo:table-header>
            <fo:table-body>
                <#list glAcctBalancesByCostCenter as glAcctBalanceByCostCenter>
                    <#if glAcctBalanceByCostCenter?has_content>
                        <fo:table-row>
                            <fo:table-cell border="1pt solid" border-width=".1mm">
                                <fo:block text-align="center">${glAcctBalanceByCostCenter.glAccountId!}</fo:block>
                            </fo:table-cell>
                            <fo:table-cell border="1pt solid" border-width=".1mm">
                                <fo:block text-align="center">${glAcctBalanceByCostCenter.accountCode!}</fo:block>
                            </fo:table-cell>
                            <fo:table-cell border="1pt solid" border-width=".1mm">
                                <fo:block text-align="center">${glAcctBalanceByCostCenter.accountName!}</fo:block>
                            </fo:table-cell>
                            <fo:table-cell border="1pt solid" border-width=".1mm">
                                <fo:block text-align="center">${glAcctBalanceByCostCenter.balance!!}</fo:block>
                            </fo:table-cell>
                            <#list glAccountCategories as glAccountCategory>
                                <fo:table-cell border="1pt solid" border-width=".1mm">
                                    <fo:block text-align="center">${(glAcctBalanceByCostCenter[glAccountCategory.glAccountCategoryId!]!)}</fo:block>
                                </fo:table-cell>
                            </#list>
                        </fo:table-row>
                    </#if>
                </#list>
            </fo:table-body>
        </fo:table>
    <#else>
        <fo:block text-align="center">${uiLabelMap.CommonNoRecordFound}</fo:block>
    </#if>
</#escape>
