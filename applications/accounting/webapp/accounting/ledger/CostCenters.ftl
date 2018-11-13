<!--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if glAcctgAndAmountPercentageList?has_content && glAccountCategories?has_content>
    <form id="costCenters" method="post" action="<@ofbizUrl>createUpdateCostCenter</@ofbizUrl>">
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" -->
            <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.FormFieldTitle_organizationPartyId}</@th>
                    <@th>${uiLabelMap.FormFieldTitle_glAccountId}</@th>
                    <@th>${uiLabelMap.FormFieldTitle_accountCode}</@th>
                    <@th>${uiLabelMap.FormFieldTitle_accountName}</@th>
                    <#list glAccountCategories as glAccountCategory>
                        <@th>${glAccountCategory.description!}</@th>
                    </#list>
                </@tr>
            </@thead>
            <#list glAcctgAndAmountPercentageList as glAcctgAndAmountPercentage>
                <@tr id="row_${glAcctgAndAmountPercentage.glAccountId}">
                    <@td>${glAcctgAndAmountPercentage.organizationPartyId}</@td>
                    <@td>
                        <input type="hidden" id="glAccountId_${glAcctgAndAmountPercentage.glAccountId}" name="glAccountId_o_${glAcctgAndAmountPercentage_index}" value="${glAcctgAndAmountPercentage.glAccountId!}"/>
                        <input name="_rowSubmit_o_${glAcctgAndAmountPercentage_index}" type="hidden" value="Y"/>          
                        ${glAcctgAndAmountPercentage.glAccountId}
                    </@td>
                    <@td>${glAcctgAndAmountPercentage.accountCode!}</@td>
                    <@td>${glAcctgAndAmountPercentage.accountName!}</@td>                      
                    <#list glAccountCategories as glAccountCategory>
                        <@td>
                            <#if (glAcctgAndAmountPercentage[glAccountCategory.glAccountCategoryId!])??>
                                <@field type="input" id="cc_${glAcctgAndAmountPercentage.glAccountId}_${glAccountCategory.glAccountCategoryId}" size="5" name="amp_${glAccountCategory.glAccountCategoryId!}_o_${glAcctgAndAmountPercentage_index}" value=((glAcctgAndAmountPercentage[glAccountCategory.glAccountCategoryId!])!)/>%
                            <#else>
                                <@field type="input" id="cc_${glAcctgAndAmountPercentage.glAccountId}_${glAccountCategory.glAccountCategoryId}" size="5" name="amp_${glAccountCategory.glAccountCategoryId!}_o_${glAcctgAndAmountPercentage_index}" value=""/>%
                            </#if>
                        </@td>
                    </#list>
                </@tr>
            </#list>
        </@table>
        <@field type="submit" text=uiLabelMap.CommonSubmit class="+${styles.link_run_sys!} ${styles.action_update!}" />
    </form>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
